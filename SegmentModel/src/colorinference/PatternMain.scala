package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 11/2/12
 * Time: 12:32 AM
 * To change this template use File | Settings | File Templates.
 */


import cc.factorie.{SamplingMaximizer, VariableSettingsSampler}
import scala.collection.mutable._
import java.io.File
import cc.factorie._
import la.Tensor1
import scala.util.Random
import java.io.FileWriter


object ModelTraining
{
    val namingModel = new ColorNamingModel("../c3_data.json")

    /* Color properties */

    // Unary
    def colorfulness(c:Color) = Tensor1(c.colorfulness)
    def lightness(c:Color) = Tensor1(c.copyIfNeededTo(LABColorSpace)(0))
    def nameSaliency(c:Color) = Tensor1(namingModel.saliency(c))
//    def saturation(c:Color) = Tensor1(c.copyIfNeededTo(HSVColorSpace)(1))

    // Binary
    def perceptualDifference(c1:Color, c2:Color) = Tensor1(Color.perceptualDifference(c1, c2))
    def chromaDifference(c1:Color, c2:Color) = Tensor1(Color.chromaDifference(c1, c2))
    def relativeColorfulness(c1:Color, c2:Color) = Tensor1(Color.relativeColorfulness(c1, c2))
    def relativeLightness(c1:Color, c2:Color) = Tensor1(Color.relativeLightness(c1, c2))
    def nameSimilarity(c1:Color, c2:Color) = Tensor1(namingModel.cosineSimilarity(c1, c2))
//    def luminanceContrast(c1:Color, c2:Color) = Tensor1(Color.luminanceContrast(c1, c2))
//    def hueComplementarity(c1:Color, c2:Color) = Tensor1(Color.hueAngle(c1, c2))
//    def relativeSaturation(c1:Color, c2:Color) = Tensor1(Color.relativeSaturation(c1, c2))


    /* Quantizers */
    val uniformQuant10 = new UniformVectorQuantizer(Array(10))

    def apply(trainingMeshes:Array[SegmentMesh]) : ColorInferenceModel =
    {
        val training = new ModelTraining
        training.train(trainingMeshes)
    }
}

// This does not use labels
class ModelTraining
{
    type Examples = ArrayBuffer[HistogramRegressor.RegressionExample]
    case class UnarySegmentProperty(name:String, extractor:UnarySegmentTemplate.ColorPropertyExtractor, quant:VectorQuantizer)
    {
        val examples = new Examples
    }
    case class BinarySegmentProperty(name:String, extractor:BinarySegmentTemplate.ColorPropertyExtractor, quant:VectorQuantizer)
    {
        val examples = new Examples
    }
    case class ColorGroupProperty(name:String, extractor:ColorGroupTemplate.ColorPropertyExtractor, quant:VectorQuantizer)
    {
        val examples = new Examples
    }

    /* Unary segment properties */
    val unarySegProps = new ArrayBuffer[UnarySegmentProperty]()
    unarySegProps += UnarySegmentProperty("Lightness", ModelTraining.lightness, ModelTraining.uniformQuant10)
    unarySegProps += UnarySegmentProperty("Colorfulness", ModelTraining.colorfulness, ModelTraining.uniformQuant10)
    unarySegProps += UnarySegmentProperty("Name Saliency", ModelTraining.nameSaliency, ModelTraining.uniformQuant10)
//    unarySegProps += UnarySegmentProperty("Saturation", ModelTraining.saturation, ModelTraining.uniformQuant10)

    /* Binary segment properties */
    // The assumption for the binary properties thus far is that they're symmetric (no directionality between the variables), which is probably ok
    val binarySegProps = new ArrayBuffer[BinarySegmentProperty]()
    binarySegProps += BinarySegmentProperty("Perceptual Difference", ModelTraining.perceptualDifference, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Chroma Difference", ModelTraining.chromaDifference, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Relative Colorfulness", ModelTraining.relativeColorfulness, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Relative Lightness", ModelTraining.relativeLightness, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Name Similarity", ModelTraining.nameSimilarity, ModelTraining.uniformQuant10)
//    binarySegProps += BinarySegmentProperty("Luminance Contrast", ModelTraining.luminanceContrast, ModelTraining.uniformQuant10)
//    binarySegProps += BinarySegmentProperty("Hue Complementarity", ModelTraining.hueComplementarity, ModelTraining.uniformQuant10)
//    binarySegProps += BinarySegmentProperty("Relative Saturation", ModelTraining.relativeSaturation, ModelTraining.uniformQuant10)

    /* Color group properties */
    val groupProps = new ArrayBuffer[ColorGroupProperty]()
    groupProps += ColorGroupProperty("Lightness", ModelTraining.lightness, ModelTraining.uniformQuant10)
    groupProps += ColorGroupProperty("Colorfulness", ModelTraining.colorfulness, ModelTraining.uniformQuant10)
    groupProps += ColorGroupProperty("Name Saliency", ModelTraining.nameSaliency, ModelTraining.uniformQuant10)

    def train(trainingMeshes:Array[SegmentMesh]) : ColorInferenceModel =
    {
        /** Extract training data points from meshes **/

        // Training meshes with more segments generate more samples. Here we eliminate that bias
        // repeating the examples according to the lcm doesn't work...as the lcm turns out to be big, and we run out of heap space
        // so we'll weight each example according to 1/numSegments or 1/numAdjacencies. Scale by 2, so we don't run into rounding errors (when Weka checks that weights add up to >=1)
        for (mesh <- trainingMeshes)
        {
            val unaryWeight = 2.0/mesh.segments.length

            // Unary segment properties
            for (seg <- mesh.segments)
            {
                val fvec = Segment.getUnaryRegressionFeatures(seg)
                for (prop <- unarySegProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg.group.color.observedColor), fvec, unaryWeight) }
            }

            var checkAdj = 0
            for (seg1<-mesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
                checkAdj+=1

            val binaryWeight =  2.0/checkAdj

            // Binary segment properties
            for (seg1 <- mesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
            {
                val fvec = Segment.getBinaryRegressionFeatures(seg1, seg2)
                for (prop <- binarySegProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg1.group.color.observedColor,seg2.group.color.observedColor), fvec, binaryWeight) }
            }

            // Group properties
            // TODO: Should these training examples be weighted like the ones above? I think it's probably unnecessary.
            for (group <- mesh.groups)
            {
                val fvec = SegmentGroup.getRegressionFeatures(group)
                for (prop <- groupProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(group.color.observedColor), fvec)}
            }
        }

        /** Construct model **/
        val model = new ColorInferenceModel
        for (i <- 0 until unarySegProps.length)
        {
            val template = new DiscreteUnarySegmentTemplate(unarySegProps(i))
            template.setWeight(1.0)
            model += template
        }
        for (i <- 0 until binarySegProps.length)
        {
            val template = new DiscreteBinarySegmentTemplate(binarySegProps(i))
            template.setWeight(1.0)
            model += template
        }
        for (i <- 0 until groupProps.length)
        {
            val template = new DiscreteColorGroupTemplate(groupProps(i))
            template.setWeight(1.0)
            model += template
        }

        /** Train weights of the model **/
        // TODO: Remove the 'setWeight' calls above and actually do parameter estimation

        model
    }
}

//Note: A lower score is better
object PatternMain {
  //Just a place to test loading and evaluating patterns/images. Subject to much change
  val inputDir = "../PatternColorizer/out/mesh"
  val outputDir = "../PatternColorizer/out/specs"
  val histDir = "../PatternColorizer/out/hist"

  val visDir = "../PatternColorizer/out/vis"

  var meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]()
  var files:Array[File] = null
  val random = new Random()
  val numIterations = 100

  def main(args:Array[String])
  {
    // Verify that outputDir exists
    val outputDirTestFile = new File(outputDir)
    if (!outputDirTestFile.exists)
        outputDirTestFile.mkdir

    // Verify that visDir exists
    val visDirTestFile = new File(visDir)
    if (!visDirTestFile.exists)
      visDirTestFile.mkdir

    // Verify that histDir exists
    val histDirTestFile = new File(histDir)
    if (!histDirTestFile.exists)
      histDirTestFile.mkdir

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

    //list of patterns to consider
    val patterns = Array(
      296605,
      244833,
      231386,
      447439,
      499194,
      506633,
      789577,
      304986,
      243893,
      220077,
      500393,
      508162,
      515691,
      798455)

    for (idx <- meshes.indices
         if (patterns.contains(files(idx).getName().replace(".txt","").toInt)))
    {

      val trainingMeshes:Array[SegmentMesh] = {for (tidx<-meshes.indices if tidx != idx) yield meshes(tidx)}.toArray
      val hfilename = histDir + "/"+files(idx).getName()
      val vfilename = visDir + "/"+files(idx).getName()

      val palette = ColorPalette(meshes(idx))
      DiscreteColorVariable.initDomain(palette)

      val model = ModelTraining(trainingMeshes)
      model.conditionOn(meshes(idx))


      val patternId = files(idx).getName().replace(".txt","").toInt
      OutputHistograms(meshes(idx), model, hfilename, patternId)
      OutputAllPermutations(meshes(idx), model, palette, vfilename)

    }
    //test the model by training and testing on the same mesh, plus a few other meshes
    /*for (idx<-meshes.indices if idx<5)
    {
      println("Testing model on mesh " + files(idx).getName )
      val trainingMeshes:ArrayBuffer[SegmentMesh] = ArrayBuffer[SegmentMesh]{meshes(idx)}

      //pick 4 more random meshes (different)
      val pool = ArrayBuffer[Int]()
      pool ++= (0 until meshes.indices.length)
      pool -= idx
      while (trainingMeshes.length < 3)
      {
        val todrop = pool(random.nextInt(pool.length))
        trainingMeshes += meshes(todrop)
        pool -= todrop
      }

      val (score, rand) = TrainTestModel(meshes(idx), trainingMeshes.toArray)
      avgTScore += score
      randTScore += rand
      tcount +=1
    }
    avgTScore /= tcount
    randTScore /= tcount

    //for now, just try using the original palette
    var avgScore:Double = 0
    var count = 0
    var randScore:Double = 0

    for (idx <- meshes.indices if idx < 10)
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
    println("Average Random Score: " + randTScore)*/

  }

  def RandomAssignment(segmesh:SegmentMesh, palette:ColorPalette) : Seq[Color] =
  {
     segmesh.groups.map(g => palette(random.nextInt(palette.length)))
  }

  def SetRandomPermutation(mesh:SegmentMesh)
  {
    // set the variable domain
    val palette = ColorPalette(mesh)
    DiscreteColorVariable.initDomain(palette)

    val numVals = DiscreteColorVariable.domain.size
    val allPerms = (0 until numVals).toList.permutations.toList
    val randP = allPerms(random.nextInt(allPerms.length))

    for (i <- mesh.groups.indices)
    {
      mesh.groups(i).color.setColor(DiscreteColorVariable.domain.category(randP(i)))
    }

  }

  def SetRandomCombination(mesh:SegmentMesh)
  {
    // set the variable domain
    val palette = ColorPalette(mesh)
    DiscreteColorVariable.initDomain(palette)

    val assignment = mesh.groups.map(g => random.nextInt(palette.length))

    for (i <- mesh.groups.indices)
    {
      mesh.groups(i).color.setColor(DiscreteColorVariable.domain.category(assignment(i)))
    }
  }

  /** Visualization output methods **/
  def OutputHistograms(mesh:SegmentMesh, model:ColorInferenceModel, filename:String, patternId:Int)
  {
    //output the histograms in a csv format
    //TODO: output group marginals
    //TODO: current format may not work for histograms greater than 1D
    //TODO: add more summary items?
    val out = new FileWriter(filename)
    out.write("\"pattern\",\"factortype\",\"property\",\"ids\",\"bin\",\"value\",\"smoothed\"\n")

    val summary:ArrayBuffer[SummaryItem] = model.getSummary

    for (s <- summary)
    {
      val name = s.ttype
      val prop = s.propname
      val ids = s.ids
      val hist = s.hist

      val centroids = hist.getCentroids
      val bins = hist.getBins
      var idx = 0
      for (c <- centroids)
      {
        out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\","+c.mkString("-")+","+hist.evaluateAt(c)+",\"true\"\n")

        out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\","+c.mkString("-")+","+bins(idx)++",\"false\"\n")
        idx += 1
      }
    }
    out.close()

  }


  def OutputAllPermutations(mesh:SegmentMesh, model:ColorInferenceModel, palette:ColorPalette, filename:String)
  {
    //output all the permutations in order of score, indicate which one is the original
    val numVals = DiscreteColorVariable.domain.size
    val vars = mesh.groups.map(g => g.color)
    val allPerms = (0 until numVals).toList.permutations.toList

    //store the permutation index and the score in a list
    var results = ArrayBuffer[(Int, Double)]()
    val itemizedModel = model.itemizedModel(vars)

    //add the results
    var idx = 0
    for (p <- allPerms)
    {
      val assignment = new HashMapAssignment(vars)
      for (i <- mesh.groups.indices)
      {
        assignment.update(mesh.groups(i).color.asInstanceOf[DiscreteColorVariable], DiscreteColorVariable.domain(p(i)))
      }
      for (f <- itemizedModel.factors)
      {
        f.variables.foreach{ e => e match {
          case(v:UnarySegmentTemplate.DatumVariable) => assignment.update(v, v.value)
          case(b:BinarySegmentTemplate.DatumVariable) => assignment.update(b, b.value)
          case _ => null
        }}
      }

      val currScore = model.assignmentScore(vars, assignment)

      //store the permutation index and the score into the results list
      results += ((idx, currScore))

      idx += 1
    }

    results = results.sortBy(t => -1*t._2)



    //write the file
    //Start with Score number isOrig
    //then color assignments
    val out = new FileWriter(filename)
    out.write("Count " + allPerms.length +"\n")
    for (r <- results)
    {
      val p = allPerms(r._1)
      val score = r._2

      //check if it is the original
      var orig = true
      for (i <- mesh.groups.indices)
      {
        if (palette(p(i)).distance(mesh.groups(i).color.observedColor) > 0)
          orig = false
      }

      out.write("Score " + score + " " + orig+"\n")
      for (i <- mesh.groups.indices)
      {
        out.write(palette(p(i)).componentString + "\n")
      }
    }
    out.close()

  }

  def TrainTestModel(segmesh:SegmentMesh, trainingMeshes:Array[SegmentMesh]):(Double,Double) =
  {
    // set the variable domain
    val palette = ColorPalette(segmesh)
    DiscreteColorVariable.initDomain(palette)

      // Convert colors to LAB space, since most of our factors use LAB features
      for (color <- palette) color.convertTo(LABColorSpace)

    val model = ModelTraining(trainingMeshes)
    model.conditionOn(segmesh)

    // Do inference
    println("Performing inference")
    /*val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
    val optimizer = new SamplingMaximizer(sampler)
    optimizer.maximize(for (group <- segmesh.groups) yield group.color.asInstanceOf[DiscreteColorVariable], numIterations)*/

    //ExhaustiveSearch.allCombinations(segmesh, model)

    ExhaustiveSearch.allPermutations(segmesh, model)

      // Convert colors back to RGB space before we do any comparisons to ground truth, etc.
      for (color <- palette) color.convertTo(RGBColorSpace)

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
//TODO: incorporate this more nicely with  Factorie?
//There are some ugly type casts here...hopefully some way to fix
object ExhaustiveSearch
{

  def allCombinations(mesh:SegmentMesh, model:Model)
  {
    println("Starting exhaustive search through all combos")

    val numVals = DiscreteColorVariable.domain.size
    val vars = mesh.groups.map(g => g.color)
    val allCombs = MathUtils.comb[Int](vars.size, (0 until numVals).toList)
    var iters = 0

    for (c <- allCombs; p <- c.permutations)
      iters += 1

    println("Number of combinations: "+iters)

    var bestScore = Double.NegativeInfinity
    var currIter = 0
    val itemizedModel = model.itemizedModel(vars)
    for (c <- allCombs; p<-c.permutations)
    {
      if (currIter % 500 == 0) println("Current iteration ..." + currIter)
      currIter += 1
      //create the new assignment
      //TODO: learn DiffLists
      val assignment = new HashMapAssignment(vars)
      for (i <- mesh.groups.indices)
      {
        assignment.update(mesh.groups(i).color.asInstanceOf[DiscreteColorVariable], DiscreteColorVariable.domain(p(i)))
      }
      //TODO: this is ugly
      for (f <- itemizedModel.factors)
      {
         f.variables.foreach{ e => e match {
           case(v:UnarySegmentTemplate.DatumVariable) => assignment.update(v, v.value)
           case(b:BinarySegmentTemplate.DatumVariable) => assignment.update(b, b.value)
           case (g:ColorGroupTemplate.DatumVariable) => assignment.update(g, g.value)
           case _ => null
         }}
      }


      val currScore = model.assignmentScore(vars, assignment)
      if (currScore > bestScore)
      {
          //set the assignment
          for (i <- mesh.groups.indices)
          {
            mesh.groups(i).color.setColor(DiscreteColorVariable.domain.category(p(i)))
          }
          bestScore = currScore
      }

    }

  }


  def allPermutations(mesh:SegmentMesh, model:Model)
  {

    println("Starting exhaustive search through all permutation")
     val numVals = DiscreteColorVariable.domain.size
     val vars = mesh.groups.map(g => g.color)
      assert(numVals==vars.size, "allPermutations: Number of variables is not equal to domain!")

     val allPerms = (0 until numVals).toList.permutations.toList

     println("Number of permutations " + allPerms.length)
     var currIter = 0
     var bestScore = Double.NegativeInfinity

    val itemizedModel = model.itemizedModel(vars)

    for (p <- allPerms)
    {
      if (currIter % 10 == 0) println("Current iteration ..." + currIter)
      currIter += 1
      //create the new assignment
      val assignment = new HashMapAssignment(vars)
      for (i <- mesh.groups.indices)
      {
        assignment.update(mesh.groups(i).color.asInstanceOf[DiscreteColorVariable], DiscreteColorVariable.domain(p(i)))
      }

      for (f <- itemizedModel.factors)
      {
        f.variables.foreach{ e => e match {
          case(v:UnarySegmentTemplate.DatumVariable) => assignment.update(v, v.value)
          case(b:BinarySegmentTemplate.DatumVariable) => assignment.update(b, b.value)
          case (g:ColorGroupTemplate.DatumVariable) => assignment.update(g, g.value)
          case _ => null
        }}
      }


      val currScore = model.assignmentScore(vars, assignment)
      if (currScore > bestScore)
      {
        //set the assignment
        for (i <- mesh.groups.indices)
        {
          mesh.groups(i).color.setColor(DiscreteColorVariable.domain.category(p(i)))
        }
        bestScore = currScore

      }

    }

  }

}
