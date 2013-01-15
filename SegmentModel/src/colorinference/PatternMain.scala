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
import javax.imageio.stream.FileImageOutputStream


object PatternMain {
  //Just a place to test loading and evaluating patterns/images. Subject to much change
  val inputDir = "../PatternColorizer/out/mesh"
  val outputDir = "../PatternColorizer/out/specs"
  val histDir = "../PatternColorizer/out/hist"
  val visDir = "../PatternColorizer/out/vis"

  var meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]()
  var patterns:Array[PatternItem] = null
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

    //load all files TODO: to make this manageable, let's try out one subfolders for now
    patterns = PatternIO.getPatterns(inputDir).filter(p=>(p.directory == "davidgav")).toArray// || p.directory=="cameo")).toArray

    if (patterns.length == 0)
      println("No files found in the input directory!")

      // Setup model training parameters (we'll use Discrete color variables in this test)
      val params = new ModelTrainingParams
      {
          type VariableType = DiscreteColorVariable
          val colorVarParams = DiscreteColorVariableParams
          includeUnaryTerms = true
          //trainerType = TrainerType.SampleRank

          //regression = HistogramRegressor.KNN
          regression = HistogramRegressor.LogisticRegression
            saveRegressorsIfPossible = true
            saveWeightsIfPossible = true
            loadRegressorsIfPossible = true
            loadWeightsIfPossible = true
      }

    for (p <- patterns)
    {
      meshes.append(new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath))
    }

    //list of patterns to consider
    val pids = Array(
        2991717,
        2840695,
        2733843,
        2740524,
        2733830,
        2439705,
        2439487,
        2441582,
        2991721)


    //train a model on all patterns, except the ones being considered (train/test set)
    val testingMeshes = {for (idx<-meshes.indices if (pids.contains(patterns(idx).name.replace(".txt","").toInt))) yield meshes(idx)}
    val trainingMeshes = {for (m<-meshes if (!testingMeshes.contains(m))) yield m}

    println("Training on " + trainingMeshes.length + " meshes")
    val model = ModelTraining(trainingMeshes, params)

    val (totalScore, randomScore) = testingMeshes.foldLeft[(Double,Double)]((0.0,0.0))((curSum, mesh) =>
    {
      val (score, rscore) = TestModel(mesh, model)
      (curSum._1+score, curSum._2+rscore)
    } )

    //higher score is better
    println("Average score " + totalScore/testingMeshes.length)
    println("Average random score " + randomScore/testingMeshes.length)


    //OutputVisualizations(pids, model,"knn")


    OutputVisualizations(pids, model, "")

  }


    /////////////////////////////////////////////////////////////////////////////////////////////
    // NOTE: None of the stuff below currently takes the type of ColorVariable we're using     //
    // into account. This stuff is only for use when you know you're using Discrete variables. //
    /////////////////////////////////////////////////////////////////////////////////////////////


 /** Getting and setting random assignments **/
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
  def OutputVisualizations(pids:Array[Int], model:ColorInferenceModel, label:String)
  {
    val hallfilename = histDir + "/allhist"+ label+".txt"
    val cfilename = histDir + "/coeff"+label+".txt"
    val file = new File(hallfilename)
    file.delete()

    var count=0
    for (idx <- meshes.indices
         if (pids.contains(patterns(idx).name.replace(".txt","").toInt)))
    {
      println("Testing mesh " + patterns(idx).name)
      val vfilename = PatternIO.ensureAndGetFileName(patterns(idx), visDir, ".txt")//visDir + "/"+patterns(idx).name

      //val palette = ColorPalette(meshes(idx))

        val palette = new ColorPalette

        //Brick: 199,70,41 99,149,234 100,30,20 145,136,129 30,32,45
        //palette += Color.RGBColor(199.0 / 255.0, 70.0 / 255.0, 41.0 / 255.0)
        //palette += Color.RGBColor(99.0 / 255.0, 149.0 / 255.0, 234.0 / 255.0)
        //palette += Color.RGBColor(100.0 / 255.0, 30.0 / 255.0, 20.0 / 255.0)
        //palette += Color.RGBColor(145.0 / 255.0, 136.0 / 255.0, 129.0 / 255.0)
        //palette += Color.RGBColor(30.0 / 255.0, 32.0 / 255.0, 45.0 / 255.0)

        //Sunset: 243,37,0 253,253,245 253,237,27 139,44,0 59,7,9
        palette += Color.RGBColor(243.0 / 255.0, 37.0 / 255.0, 0.0 / 255.0)
        palette += Color.RGBColor(253.0 / 255.0, 253.0 / 255.0, 245.0 / 255.0)
        palette += Color.RGBColor(253.0 / 255.0, 237.0 / 255.0, 27.0 / 255.0)
        palette += Color.RGBColor(139.0 / 255.0, 44.0 / 255.0, 0.0 / 255.0)
        palette += Color.RGBColor(59.0 / 255.0, 7.0 / 255.0, 9.0 / 255.0)

      DiscreteColorVariable.initDomain(palette)

      model.conditionOn(meshes(idx))


      val patternId = patterns(idx).name.replace(".txt","").toInt

      VisualizationIO.OutputHistograms(model, hallfilename, patternId, true, count==0)
      VisualizationIO.OutputAllPermutations(meshes(idx), model, palette, vfilename)
      count+=1

    }

    //coefficients are shared across all test patterns
    VisualizationIO.OutputCoefficients(model, cfilename)
  }



  /** Testing the model **/
  def TestModel(segmesh:SegmentMesh, model:ColorInferenceModel):(Double,Double) =
  {
    // set the variable domain
    val palette = ColorPalette(segmesh)
    DiscreteColorVariable.initDomain(palette)

      // Convert colors to LAB space, since most of our factors use LAB features
      for (color <- palette) color.convertTo(LABColorSpace)

    model.conditionOn(segmesh)

    // Do inference
    println("Performing inference")
    /*val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
    val optimizer = new SamplingMaximizer(sampler)
    optimizer.maximize(for (group <- segmesh.groups) yield group.color.asInstanceOf[DiscreteColorVariable], numIterations)*/

    //ExhaustiveInference.allCombinations(segmesh, model)

    ExhaustiveInference.allPermutations(segmesh, model)



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

    // Convert colors back to RGB space before outputting...
    for (color <- palette) color.convertTo(RGBColorSpace)

    (score,rscore)

  }

}
