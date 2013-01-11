package colorinference

import java.io.{FileWriter, File}
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 1/10/13
 * Time: 2:19 PM
 * To change this template use File | Settings | File Templates.
 */
object TurkMain {
  val outDir = "turkImages"
  val meshDir = "../PatternColorizer/out/mesh"
  val logDir = "../PatternColorizer/out/turkLog"

  //TODO: sync up with standard parameters
  // Maximization parameters
  val numParallelChains = 5
  val iterations = 2000
  val cciterations = 20000 //use different number of iterations for color compatibility model?
  val initTemp = 1.0
  val finalTemp = 0.01
  val rounds = 40
  val lambda = 0.5

  // Parallel tempering parameters
  val chainTemps = Array(1.0, 0.5, 0.2, 0.05, 0.01)
  val itersBetweenSwaps = 50

  val numSamplesToOutput = 4

  val params = new ModelTrainingParams
  {
    type VariableType = ContinuousColorVariable
    val colorVarParams = ContinuousColorVariableParams
    includeColorCompatibilityTerm = true
    saveRegressorsIfPossible = true
    saveWeightsIfPossible = true
    loadRegressorsIfPossible = true
    loadWeightsIfPossible = true
    modelSaveDirectory = "savedModel-turkModel-2"

    initialLearningRate = 0.2
    numWeightTuningIterations = 20
    enforceMinimumWeight = true
    minWeight = 0.0

    includeColorChoiceTerms = true
    colorChoiceType = ModelTraining.ColorChoiceType.NamesConditional
    includeUnaryTerms = true
    includeGroupTerms = true
    includeBinaryTerms = true

    weightGroups = true
    crossValidateHistogramParams = false
  }

  case class TurkScoredValue(values:IndexedSeq[Color], score:Double, ccScore:Double)

  def main(args:Array[String])
  {
    //get a large pool of different templated patterns
    //PatternIO.getRandomUniquePatterns(50, "../Colourlovers/turk")
     new File(outDir).mkdir()
    new File(logDir).mkdir()



    //TODO: get list of pids from figures
    val pids = Array(
      786134,
      977295,
      1198080,
      1537398,
      1564727,
      1588575,
      1612867,
      1735168,
      1775244,
      1879321,
      1894975,
      2019255,
      2531192,
      2539244,
      2737168
    )

    //TODO: determine n, or re-use trained model from elsewhere
    val trainingSet = PatternIO.getTopNArtistPatterns(meshDir, 2, pids)
    val testSet = PatternIO.getPatterns(meshDir).filter(p => pids.contains(p.pid))

    val trainingMeshes = trainingSet.map(p => new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)).toArray
    val testMeshes = testSet.map(p => new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)).toArray

    println("Training on " + trainingMeshes.length + " meshes")
    val model = ModelTraining(trainingMeshes, params)

    //color compatibility model onlu
    val ccmodel  = new ColorInferenceModel
    ccmodel += new ColorCompatibilityTemplate

    outputRandomPatterns(model, ccmodel, testMeshes)
    outputCCPatterns(model, ccmodel, testMeshes)
    outputModelPatterns(model, ccmodel, testMeshes)

  }

  //assuming the mesh has a filename that ends in the pid.txt
  def getPid(mesh:SegmentMesh):Int =
  {
    var last = mesh.name.split('\\').last
    last.replace(".txt","").toInt
  }

  //just dump all pattern descriptions into one file
  def outputRandomPatterns(model:ColorInferenceModel, ccmodel:ColorInferenceModel, meshes:Seq[SegmentMesh])
  {
    println("Generating random patterns...")
    val pidToOutput = new mutable.HashMap[Int,Array[TurkScoredValue]] ()
    //output random patterns
    for (idx <- meshes.indices)
    {
      val mesh = meshes(idx)
      val samples = (for (i <- 0 until numSamplesToOutput) yield
      {
        mesh.randomizeVariableAssignments()
        val variables = mesh.variablesAs[ContinuousColorVariable]
        model.conditionOn(mesh)
        val score = model.currentScore(variables)

        ccmodel.conditionOn(mesh)
        val ccscore = math.exp(ccmodel.currentScore(variables))*5.0

        TurkScoredValue(variables.map(_.getColor), score, ccscore)
      }).sortWith(_.score > _.score).toArray
      pidToOutput(getPid(mesh)) = samples
    }

    savePatterns("randomPatterns.txt","r",pidToOutput)
  }

  def outputModelPatterns(model:ColorInferenceModel, ccmodel:ColorInferenceModel, meshes:Seq[SegmentMesh])
  {
    val pidToOutput = new mutable.HashMap[Int,Array[TurkScoredValue]] ()
    println("Generating CC patterns...")

    for (mesh <- meshes)
    {
      ccmodel.conditionOn(mesh)
      model.conditionOn(mesh)

      val variables = mesh.variablesAs[ContinuousColorVariable]

      //sample from the trained model
      val samplerGenerator = () => { new ContinuousColorSampler(model) with MemorizingSampler[ContinuousColorVariable] }
      val maximizer = new ParallelTemperingMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, chainTemps)

      maximizer.maximize(mesh, iterations, itersBetweenSwaps)

      // Convert sampled values to LAB first, since our similarity metric operates in LAB space
      maximizer.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

      val metric = genMMRSimilarityMetric(mesh.variablesAs[ContinuousColorVariable])
      val mmr = new MMR(maximizer.samples, metric)
      val rankedSamples = mmr.getRankedSamples(numSamplesToOutput, lambda)

      pidToOutput(getPid(mesh)) = rankedSamples.map(s => new TurkScoredValue(s.values, s.score, math.exp(ccmodel.currentScore(variables))*5.0)).toArray
    }
    savePatterns("modelPatterns.txt","m",pidToOutput)
  }

  def outputCCPatterns(model:ColorInferenceModel, ccmodel:ColorInferenceModel, meshes:mutable.Seq[SegmentMesh])
  {
    val pidToOutput = new mutable.HashMap[Int,Array[TurkScoredValue]] ()
    println("Generating CC patterns...")

    for (mesh <- meshes)
    {
      ccmodel.conditionOn(mesh)
      model.conditionOn(mesh)

      val variables = mesh.variablesAs[ContinuousColorVariable]

      //sample from the cc model
      val samplerGenerator = () => { new ContinuousColorSampler(ccmodel) with MemorizingSampler[ContinuousColorVariable] }
      val maximizer = new ParallelTemperingMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, chainTemps)

      maximizer.maximize(mesh, cciterations, itersBetweenSwaps)

      // Convert sampled values to LAB first, since our similarity metric operates in LAB space
      maximizer.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

      val metric = genMMRSimilarityMetric(mesh.variablesAs[ContinuousColorVariable])
      val mmr = new MMR(maximizer.samples, metric)
      val rankedSamples = mmr.getRankedSamples(numSamplesToOutput, lambda)

      pidToOutput(getPid(mesh)) = rankedSamples.map(s => new TurkScoredValue(s.values, model.currentScore(variables), math.exp(s.score)*5.0)).toArray
    }
    savePatterns("ccPatterns.txt","c",pidToOutput)
  }

  def outputArtistPatterns(indir:String)
  {
    //TODO:Need to produce artist pattern info from Colourlovers
  }

  def savePatterns(filename:String, label:String, samples:mutable.HashMap[Int, Array[TurkScoredValue]])
  {
    var file = new File(logDir, filename)
    var writer = new FileWriter(file)

    //headers are
    //pid, imagename, modelscore, paletteScore, colors separated by ^, fields separated by |
    var pids = samples.keys
    for (pid <- pids)
    {
      var pidSamples = samples(pid)

      for (idx <- pidSamples.indices)
      {
        //Record
        writer.write("%d|%s|%f|%s\n".format(pid, pid+"_"+label+idx, pidSamples(idx).score, pidSamples(idx).values.map(c => c.copyIfNeededTo(RGBColorSpace).componentString).mkString("^")))
        writer.flush()
      }

    }
    writer.close()
  }

  private val maxLABdist = math.sqrt(100*100 + 200*200 + 200*200)
  def genMMRSimilarityMetric(vars:Seq[ContinuousColorVariable]) : (Seq[Color], Seq[Color]) => Double =
  {
    val maxDist = vars.map(_.group.size * maxLABdist).sum

    def metric(vals1:Seq[Color], vals2:Seq[Color]) : Double =
    {
      // Size-weighted Delta E, normalized by maximum possible difference
      var dist = 0.0
      for (i <- 0 until vars.length)
        dist += (vars(i).group.size * Color.perceptualDifference(vals1(i), vals2(i)))
      1.0 - (dist / maxDist)
    }

    metric
  }


}
