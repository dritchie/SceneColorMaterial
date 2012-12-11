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
    type UnaryProperty = (UnarySegmentTemplate.ColorPropertyExtractor, Examples)
    type BinaryProperty = (BinarySegmentTemplate.ColorPropertyExtractor, Examples)

    def addUnaryProperty(extractor:UnarySegmentTemplate.ColorPropertyExtractor)
    {
        val tup = (extractor, new Examples())
        unary += tup
    }

    def addBinaryProperty(extractor:BinarySegmentTemplate.ColorPropertyExtractor)
    {
        val tup = (extractor, new Examples())
        binary += tup
    }

    // Unary (TODO: Add more!)
    val unary = new ArrayBuffer[UnaryProperty]()
    // Lightness
    addUnaryProperty((c:Color) => { Tensor1(c.copyIfNeededTo(LABColorSpace)(0)) })
    // Saturation
    addUnaryProperty((c:Color) => { Tensor1(c.copyIfNeededTo(HSVColorSpace)(1)) })

    // Binary (TODO: Add more!)
    //TODO: the assumption for the binary properties thus far is that they're symmetric (no directionality between the variables), which is probably ok
    val binary = new ArrayBuffer[BinaryProperty]()
    // Contrast
    addBinaryProperty((c1:Color, c2:Color) => { Tensor1(Color.contrast(c1, c2)) })

    def this(trainingMeshes:Array[SegmentMesh])
    {
        this()

        // TODO: Training meshes with more segments generate more samples. Eliminate this bias!
        for (mesh <- trainingMeshes)
        {
            // Unary stuff
            for (seg <- mesh.segments)
            {
                val fvec = getUnarySegmentRegressionFeatures(seg)
                for (prop <- unary) { prop._2 += HistogramRegressor.RegressionExample(prop._1(seg.group.color.observedColor), fvec) }
            }

            // Binary stuff
            for (seg1 <- mesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
            {
                val fvec = getBinarySegmentRegressionFeatures(seg1, seg2)
                for (prop <- binary) { prop._2 += HistogramRegressor.RegressionExample(prop._1(seg1.group.color.observedColor,seg2.group.color.observedColor), fvec) }
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
            HistogramRegressor.LogisticRegression(prop._2, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)
        val binaryRegressors = for (prop <- binary) yield
            HistogramRegressor.LogisticRegression(prop._2, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)

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
            val template = new DiscreteUnarySegmentTemplate(unary(i)._1, unaryHistMaps(i))
            template.setWeight(1.0)
            model += template
        }
        for (i <- 0 until binary.length)
        {
            val template = new DiscreteBinarySegmentTemplate(binary(i)._1, binaryHistMaps(i))
            template.setWeight(1.0)
            model += template
        }

        model
    }



  def buildModel(targetMesh:SegmentMesh, bins:Int=20): Model =
    {
      println("Building non-template model...")

      // Train regressors
      println("Training regressors...")
      val unaryRegressors = for (prop <- unary) yield
        HistogramRegressor.LogisticRegression(prop._2, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)
      val binaryRegressors = for (prop <- binary) yield
        HistogramRegressor.LogisticRegression(prop._2, MathUtils.euclideanDistance, new KMeansVectorQuantizer(bins), WekaMultiClassHistogramRegressor)

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
          model += new UnaryPriorFactor(seg.group.color.asInstanceOf[DiscreteColorVariable], unaryHistMaps(i)(seg), unary(i)._1 )

        for (n <- seg.adjacencies if seg.index < n.index)
        {
          for (i<-0 until binary.length)
            model += new BinaryPriorFactor(seg.group.color.asInstanceOf[DiscreteColorVariable], n.group.color.asInstanceOf[DiscreteColorVariable], binaryHistMaps(i)((seg,n)), binary(i)._1)
        }
      }

      model
    }
}


object PatternMain {
  //Just a place to test loading and evaluating patterns/images. Subject to much change
  val inputDir = "../PatternColorizer/out/mesh"
  val outputDir = "../PatternColorizer/out/specs"

  var meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]()
  var files:Array[File] = null
  val numBins = 10
  val random = new Random()
  val ignoreLabels = false

  def main(args:Array[String])
  {
    //load all files
    files = new File(inputDir).listFiles.filter(_.getName.endsWith(".txt"))

    if (files.length == 0)
      println("No files found in the input directory!")

    for (f <- files)
    {
      meshes.append(new SegmentMesh(DiscreteColorVariable, f.getAbsolutePath))
    }

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


      //test the template model
      val trainer = new TemplateModelTraining(trainingMeshes)
      //val model = trainer.buildTemplateModel(segmesh,5)
      val model = trainer.buildModel(segmesh, 5)

      val palette = ColorPalette(segmesh)
      DiscreteColorVariable.initDomain(palette)

      // Do inference
      println("Performing inference")
      val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
      val optimizer = new SamplingMaximizer(sampler)
      optimizer.maximize(for (group <- segmesh.groups) yield group.color.asInstanceOf[DiscreteColorVariable], 1000)

      // Output the result
      segmesh.saveColorAssignments(outputDir+"/"+files(idx).getName)

      // Evaluate assignments
      val score = segmesh.scoreAssignment()
      println(files(idx).getName+" Score: "+score)
      avgScore += score

      //Evaluate random assignment (3 trials)
      var rscore = 0.0
      for (t <- 0 until 3)
      {
        val assign = RandomAssignment(segmesh, palette)
        rscore += segmesh.scoreAssignment(assign)
      }
      rscore /= 3.0

      println("Rand Score: " + rscore)

      randScore += rscore


    }


    println("Average Score: " + (avgScore/count))
    println("Average Random Score: " + (randScore/count))

  }

  def RandomAssignment(segmesh:SegmentMesh, palette:ColorPalette) : Seq[Color] =
  {
     segmesh.groups.map(g => palette(random.nextInt(palette.length)))
  }

}
