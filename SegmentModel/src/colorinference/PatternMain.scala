package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 11/2/12
 * Time: 12:32 AM
 * To change this template use File | Settings | File Templates.
 */


import cc.factorie.{SamplingMaximizer, VariableSettingsSampler}
import collection.mutable.Seq
import scala.collection.mutable
import scala.collection.mutable._
import java.io.File
import cc.factorie._
import la.Tensor1
import la.DenseTensor1
import scala.util.Random


// This does not use labels
class TemplateModelTraining
{
    type Examples = ArrayBuffer[HistogramRegressor.RegressionExample]
    case class UnaryProperty(name:String, extractor:UnarySegmentTemplate.ColorPropertyExtractor)
    {
        val examples = new Examples
    }
    case class BinaryProperty(name:String, extractor:BinarySegmentTemplate.ColorPropertyExtractor)
    {
        val examples = new Examples
    }

    /* Unary color properties */
    val unary = new ArrayBuffer[UnaryProperty]()
    unary += UnaryProperty("Lightness", (c:Color) => { Tensor1(c.copyIfNeededTo(LABColorSpace)(0)) })
    unary += UnaryProperty("Saturation", (c:Color) => { Tensor1(c.copyIfNeededTo(HSVColorSpace)(1)) })

    /* Binary color properties */
    //TODO: the assumption for the binary properties thus far is that they're symmetric (no directionality between the variables), which is probably ok
    val binary = new ArrayBuffer[BinaryProperty]()
    binary += BinaryProperty("Contrast", (c1:Color, c2:Color) => { Tensor1(Color.contrast(c1, c2)) })
    binary += BinaryProperty("Hue Complementarity", (c1:Color, c2:Color) => { Tensor1(Color.hueComplementarity(c1, c2)) })
    binary += BinaryProperty("Relative Saturation", (c1:Color, c2:Color) => { Tensor1(Color.relativeSaturation(c1, c2)) })

    def this(trainingMeshes:Array[SegmentMesh])
    {
        this()

        // TODO: Training meshes with more segments generate more samples. Eliminate this bias!
        //repeating the examples according to the lcm doesn't work...as the lcm turns out to be big, and we run out of heap space
        //so we'll weight each example according to 1/numSegments or 1/numAdjacencies. Scale by 2, so we don't run into rounding errors (when Weka checks that weights add up to >=1)

        for (mesh <- trainingMeshes)
        {
            val unaryWeight = 2.0/mesh.segments.length

            // Unary stuff
            for (seg <- mesh.segments)
            {
                val fvec = getUnarySegmentRegressionFeatures(seg)
                for (prop <- unary) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg.group.color.observedColor), fvec, unaryWeight) }
            }

            //val numAdj = {for(seg1<-mesh.segments; seg2<-seg1.adjacencies if seg1.index<seg2.index) yield 1}.length //I don't know why this doesn't work..

            var checkAdj = 0
            for (seg1<-mesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
                checkAdj+=1

            val binaryWeight =  2.0/checkAdj

            //println("numAdj " + numAdj)
            //println("checkAdj " + checkAdj)

            // Binary stuff
            for (seg1 <- mesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
            {
                val fvec = getBinarySegmentRegressionFeatures(seg1, seg2)
                for (prop <- binary) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg1.group.color.observedColor,seg2.group.color.observedColor), fvec, binaryWeight) }
            }
        }
    }

    def concatVectors[T<:Tensor1](vecs:Seq[T]) : Tensor1 =
    {
        var totaldims = 0
        for (v <- vecs) totaldims += v.length
        val vec = new DenseTensor1(totaldims)
        var finalIndex = 0
        for (v <- vecs; vi <- 0 until v.length)
        {
            vec(finalIndex) = v(vi)
            finalIndex += 1
        }
        vec
    }

    def getUnarySegmentRegressionFeatures(seg:Segment) : Tensor1 =
    {
        val featureList = seg.features.filter(f => f.name != "Label").map(f => f.values)
        concatVectors(featureList)
    }

    def getBinarySegmentRegressionFeatures(seg1:Segment, seg2:Segment) : Tensor1 =
    {
        val fvec1 = getUnarySegmentRegressionFeatures(seg1)
        val fvec2 = getUnarySegmentRegressionFeatures(seg2)

        // Sort by distance from the origin in feature space
        if (fvec1.twoNormSquared < fvec2.twoNormSquared)
            concatVectors(Array(fvec1, fvec2))
        else
            concatVectors(Array(fvec2, fvec1))

    }

  //only matters if we want to explore directional binary factors
  //i.e. smaller region to larger region has a decrease in saturation
    def getOrderedSegments(seg1:Segment, seg2:Segment): (Segment, Segment) =
      {
        val fvec1 = getUnarySegmentRegressionFeatures(seg1)
        val fvec2 = getUnarySegmentRegressionFeatures(seg2)

        // Sort by distance from the origin in feature space
        if (fvec1.twoNormSquared < fvec2.twoNormSquared)
          (seg1, seg2)
        else
          (seg2, seg1)
    }

    // This currently uses discrete variables and weights fixed at 1
    // TODO: Try continuous variables and weight learning
    def buildTemplateModel(targetMesh:SegmentMesh, bins:Int = 20) : Model =
    {
        println("Building template model...")

        // Train regressors
        println("Training regressors...")
        val unaryRegressors = for (prop <- unary) yield
            HistogramRegressor.LogisticRegression(prop.examples, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)
        println("Training binary regressors...")
        val binaryRegressors = for (prop <- binary) yield
            HistogramRegressor.LogisticRegression(prop.examples, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)

        // Predict histograms for every segment of the target mesh
        println("Predicting histograms for each segment & segment-pair...")
        val unaryHistMaps = for (prop <- unary) yield new UnarySegmentTemplate.InputData()
        val binaryHistMaps = for (prop <- binary) yield new BinarySegmentTemplate.InputData()
        // Unary stuff
        for (seg <- targetMesh.segments)
        {
            val f = getUnarySegmentRegressionFeatures(seg)
            for (i <- 0 until unary.length)
                unaryHistMaps(i)(seg) = unaryRegressors(i).predictHistogram(f)
        }
        // Binary stuff
        for (seg1 <- targetMesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
        {
            val f = getBinarySegmentRegressionFeatures(seg1, seg2)
            for (i <- 0 until binary.length)
            {
                val hist = binaryRegressors(i).predictHistogram(f)
                binaryHistMaps(i)((seg1, seg2)) = hist
                binaryHistMaps(i)((seg2, seg1)) = hist
            }
        }

        // Actually assemble the template model
        println("Assembling final model...")
        val model = new TemplateModel()
        for (i <- 0 until unary.length)
        {
            val template = new DiscreteUnarySegmentTemplate(unary(i).name, unary(i).extractor, unaryHistMaps(i))
            template.setWeight(1.0)
            model += template
        }
        for (i <- 0 until binary.length)
        {
            val template = new DiscreteBinarySegmentTemplate(binary(i).name, binary(i).extractor, binaryHistMaps(i))
            template.setWeight(1.0)
            model += template
        }

        model
    }


    def buildUnrolledTemplateModel(targetMesh:SegmentMesh, bins:Int=20): Model =
    {
        println("Building non-template model...")
        val tempModel = buildTemplateModel(targetMesh, bins)
        val itemModel = tempModel.itemizedModel(for (group <- targetMesh.groups) yield group.color.asInstanceOf[DiscreteColorVariable])
        itemModel
    }


  def buildManualItemizedModel(targetMesh:SegmentMesh, bins:Int=20): Model =
    {
      println("Building non-template model...")

      // Train regressors
      println("Training regressors...")
      val unaryRegressors = for (prop <- unary) yield
        HistogramRegressor.LogisticRegression(prop.examples, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)
      val binaryRegressors = for (prop <- binary) yield
        HistogramRegressor.LogisticRegression(prop.examples, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)

      // Predict histograms for every segment of the target mesh
      println("Predicting histograms for each segment & segment-pair...")
      val unaryHistMaps = for (prop <- unary) yield new UnarySegmentTemplate.InputData()
      val binaryHistMaps = for (prop <- binary) yield new BinarySegmentTemplate.InputData()
      // Unary stuff
      for (seg <- targetMesh.segments)
      {
        val f = getUnarySegmentRegressionFeatures(seg)
        for (i <- 0 until unary.length)
          unaryHistMaps(i)(seg) = unaryRegressors(i).predictHistogram(f)
      }
      // Binary stuff
      for (seg1 <- targetMesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
      {
        val f = getBinarySegmentRegressionFeatures(seg1, seg2)
        for (i <- 0 until binary.length)
        {
          val hist = binaryRegressors(i).predictHistogram(f)
          binaryHistMaps(i)((seg1, seg2)) = hist
          binaryHistMaps(i)((seg2, seg1)) = hist
        }
      }

     //return a model with factors
      val model = new ItemizedModel()

      //for each pair of adjacent regions in the target mesh, add a factor for the contrast, smaller label first
      for (seg <- targetMesh.segments)
      {
        //add unary factors
        //TODO: there may also be an imbalance w.r.t color groups with more segments getting more weight for their preferred color, which might be ok, but also means groups with few but large segments might get shafted
        //maybe there's some way to weight the unary factors appropriately? Or maybe just weight based on relative size (withiin the factor scoring function)?
        for (i <- 0 until unary.length)
            model += new UnaryPriorFactor(seg.group.color.asInstanceOf[DiscreteColorVariable], unaryHistMaps(i)(seg), unary(i).extractor )

        for (n <- seg.adjacencies if seg.index < n.index)
        {
          for (i<-0 until binary.length)
            model += new BinaryPriorFactor(seg.group.color.asInstanceOf[DiscreteColorVariable], n.group.color.asInstanceOf[DiscreteColorVariable], binaryHistMaps(i)((seg,n)), binary(i).extractor)
        }
      }

      model
    }
}

//Note: A lower score is better
object PatternMain {
  //Just a place to test loading and evaluating patterns/images. Subject to much change
  val inputDir = "../PatternColorizer/out/mesh"
  val outputDir = "../PatternColorizer/out/specs"

  var meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]()
  var files:Array[File] = null
  val numBins = 10     //the maximum number of bins
  val random = new Random()
  val numIterations = 100

  def main(args:Array[String])
  {
      // Verify that outputDir exists
      val outputDirTestFile = new File(outputDir)
      if (!outputDirTestFile.exists)
          outputDirTestFile.mkdir

    //load all files
    files = new File(inputDir).listFiles.filter(_.getName.endsWith(".txt"))

    if (files.length == 0)
      println("No files found in the input directory!")

    for (f <- files)
    {
      meshes.append(new SegmentMesh(DiscreteColorVariable, f.getAbsolutePath))
    }

    var avgTScore:Double = 0
    var randTScore:Double = 0
    var tcount = 0
//    //test the model by training and testing on the same mesh
//    for (idx<-meshes.indices if idx<10)
//    {
//      println("Testing model on mesh " + files(idx).getName )
//      val (score, rand) = TrainTestModel(meshes(idx), Array[SegmentMesh]{meshes(idx)})
//      avgTScore += score
//      randTScore += rand
//      tcount +=1
//    }
    avgTScore /= tcount
    randTScore /= tcount

    //for now, just try using the original palette
    var avgScore:Double = 0
    var count = 0
    var randScore:Double = 0

    for (idx <- meshes.indices if idx < 50)
    {
      println("Testing mesh " + idx)
      val segmesh = meshes(idx)
      count += 1
      //get all the other meshes
      val trainingMeshes:Array[SegmentMesh] = {for (tidx<-meshes.indices if tidx != idx) yield meshes(tidx)}.toArray

      // Evaluate assignments
      val (score, rscore) = TrainTestModel(segmesh, trainingMeshes)
      avgScore += score
      randScore += rscore

      // Output the result
      segmesh.saveColorAssignments(outputDir+"/"+files(idx).getName)

    }

    println("Hold-one-out results")
    println("Average Score: " + (avgScore/count))
    println("Average Random Score: " + (randScore/count))

    println("\nWhen training and testing on the same mesh..")
    println("Average Score: " + avgTScore)
    println("Average Random Score: " + randTScore)

  }

  def RandomAssignment(segmesh:SegmentMesh, palette:ColorPalette) : Seq[Color] =
  {
     segmesh.groups.map(g => palette(random.nextInt(palette.length)))
  }


  def TrainTestModel(segmesh:SegmentMesh, trainingMeshes:Array[SegmentMesh]):(Double,Double) =
  {
      // set the variable domain
      val palette = ColorPalette(segmesh)
      DiscreteColorVariable.initDomain(palette)

    val trainer = new TemplateModelTraining(trainingMeshes)

    val model = trainer.buildTemplateModel(segmesh, numBins)
    //val model = trainer.buildManualItemizedModel(segmesh, numBins)

    // Do inference
    println("Performing inference")
    val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
    val optimizer = new SamplingMaximizer(sampler)
    optimizer.maximize(for (group <- segmesh.groups) yield group.color.asInstanceOf[DiscreteColorVariable], numIterations)

    // Evaluate assignments
    val score = segmesh.scoreAssignment()
    println("Score: "+score)

    //Evaluate random assignment (3 trials)
    var rscore = 0.0
    for (t <- 0 until 3)
    {
      val assign = RandomAssignment(segmesh, palette)
      rscore += segmesh.scoreAssignment(assign)
    }
    rscore /= 3.0
    println("Random score: " + rscore)

    (score,rscore)

  }

}
