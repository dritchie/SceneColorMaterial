package colorinference

import java.io.{FileWriter, File}
import collection.mutable
import io.Source
import java.awt
import collection.mutable.ArrayBuffer

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
  val studyFile = "../mturk/data/pilot-all.csv"
  val turkDir = "../mturk/data"

  //TODO: sync up with standard parameters
  // Maximization parameters
  val numParallelChains = 5
  val iterations = 2000
  val cciterations = 2000//20000 //use different number of iterations for color compatibility model? TODO: in interest of time, currently setting this lower
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
    saveRegressorsIfPossible = false
    saveWeightsIfPossible = false
    loadRegressorsIfPossible = false
    loadWeightsIfPossible = false
    modelSaveDirectory = "savedModel-turkModel-final"

    initialLearningRate = 0.2
    numWeightTuningIterations = 20
    enforceMinimumWeight = true
    minWeight = 0.0

    includeColorChoiceTerms = true
    colorChoiceType = ModelTraining.ColorChoiceType.NamesConditional
    includeUnaryTerms = true
    includeGroupTerms = true
    includeBinaryTerms = true


    //includeUnaryTerms = false
    //includeGroupTerms = false
    //includeBinaryTerms = false
    //includeColorCompatibilityTerm = false


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
    new File(turkDir).mkdir()



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
      1894975,
      2019255,
      2531192,
      2539244,
      2737168,
      2644367,
        535217,
        591717,
        798455,
        2439705,
        2476359,
        2707928,
        2730347,
        2840695,
        2991717,
        2991721,
        2263667
    )

    //TODO: determine n, or re-use trained model from elsewhere
   /* val trainingSet = PatternIO.getTopNArtistPatterns(meshDir, 12, pids)
    val testSet = PatternIO.getPatterns(meshDir).filter(p => pids.contains(p.pid))

    val trainingMeshes = trainingSet.map(p => new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)).toArray
    val testMeshes = testSet.map(p => new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)).toArray

    println("Training on " + trainingMeshes.length + " meshes")
    val model = ModelTraining(trainingMeshes, params)

    //color compatibility model onlu
    val ccmodel  = new ColorInferenceModel
    ccmodel += new ColorCompatibilityTemplate  */

    ReformatStudyFile(studyFile)

    /*outputRandomPatterns(model, ccmodel, testMeshes)
    outputCCPatterns(model, ccmodel, testMeshes)
    outputModelPatterns(model, ccmodel, testMeshes)
    outputArtistPatterns() */

    //model.conditionOn(testMeshes(0))
    //InspectNameCentroids(model)

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

  def outputArtistPatterns()
  {
    //TODO:Actually compute model scores for these, for now just output the colors

    val artistFile = "../ColourLovers/turkPatterns.csv"
    val source = Source.fromFile(artistFile)
    val lineIterator = source.getLines()
    val pidToOutput = new mutable.HashMap[Int, ArrayBuffer[TurkScoredValue]] ()
    while (lineIterator.hasNext)
    {
      val line = lineIterator.next()
      val tokens = line.split('|')
      val pid:Int = tokens(0).toInt
      val name = pid+"_a"+tokens(1)
      val colorHex = tokens(3).split('^')

      val colors = for (cstr <- colorHex) yield
        {
          val ac = awt.Color.decode("0x"+cstr)
          val rgb = new Array[Float](3)
          ac.getRGBColorComponents(rgb)
          Color.RGBColor(rgb(0), rgb(1), rgb(2))
        }
      if (!pidToOutput.contains(pid))
        pidToOutput(pid) = new ArrayBuffer[TurkScoredValue]()
      pidToOutput(pid) += new TurkScoredValue(colors, 0, 0)
    }
    source.close()
    savePatterns("artistPatterns.txt","a",pidToOutput.map((kv=>(kv._1, kv._2.toArray))))
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
        writer.write("%d|%s|%f|%s|%f\n".format(pid, pid+"_"+label+idx, pidSamples(idx).score, pidSamples(idx).values.map(c => c.copyIfNeededTo(RGBColorSpace).componentString).mkString("^"), pidSamples(idx).ccScore))
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


 case class TurkAnswerItem(pattern:Int, best:Set[String], worst:Set[String])
 case class SuggestionTotals(id:String, var best:Int, var worst:Int, var total:Int)

  def ReformatStudyFile(filename:String)
  {
    //Count number of artist, model, cc, and random are chosen for best and for worst
    val methodToBest = new mutable.HashMap[String, Int]()
    val methodToWorst = new mutable.HashMap[String, Int]()

    val workerToAnswers = new mutable.HashMap[String, mutable.HashMap[Int, TurkAnswerItem]]()
    val suggestionToStats = new mutable.HashMap[String, SuggestionTotals]()
    val pidToShown = new mutable.HashMap[Int, Int]()

    var total = 0

    val source = Source.fromFile(filename)
    val lineIterator = source.getLines()
    //skip the headers
    lineIterator.next()

    val methods = Array("a","m","c","r")

    val flaggedWorkers = new mutable.HashSet[String]()
    flaggedWorkers.add("danielr")

    while (lineIterator.hasNext)
    {
       //don't count replications
      val line = lineIterator.next().replace("\"","")
      val tokens = line.split(',')
      val isRep = tokens(9).contains("1")

      val worker = tokens(0)
      val pid = tokens(1).toInt
      val best = tokens(2).split('^').toSet
      val worst = tokens(3).split('^').toSet


      if (!workerToAnswers.contains(worker))
        workerToAnswers(worker) = new mutable.HashMap[Int, TurkAnswerItem]()
      if (!workerToAnswers(worker).contains(pid))
        workerToAnswers(worker)(pid) = new TurkAnswerItem(pid, best, worst)
      else
      {
        //check for consistency
        //if the worker does not have at least one answer in common in both sets, then not consistent enough...
         val answer = workerToAnswers(worker)(pid)
         val bestIntersect = answer.best.intersect(best).size
         val worstIntersect = answer.worst.intersect(worst).size
         if (bestIntersect < 1 || worstIntersect < 1)
         {
           println("Flagged worker: "+worker)
           flaggedWorkers.add(worker)
         }

      }

    }

    val aggregateWriter = new FileWriter(new File(turkDir, "aggregate.csv"))
    val writer = new FileWriter(new File(turkDir,"perPattern.csv"))
    val htmlWriter = new FileWriter(new File(turkDir, "index.html"))

    writer.write("pid,worker,method,numBest,numWorst,total\n")
    aggregateWriter.write("method,numBest,numWorst,total\n")

    for (worker <- workerToAnswers.keys if !flaggedWorkers.contains(worker))
    {
      val pidToAnswers = workerToAnswers(worker)
      if (pidToAnswers.keys.size != 5)
        println("Worker %s only did %d patterns".format(worker, pidToAnswers.keys.size))
      for (pid <- pidToAnswers.keys)
      {
        total = total+1
        if (!pidToShown.contains(pid))
          pidToShown(pid) = 0
        pidToShown(pid)+=1

        val best = pidToAnswers(pid).best
        val worst = pidToAnswers(pid).worst

        for (m <- methods)
        {
          if (!methodToBest.contains(m))
            methodToBest(m) = 0

          best.foreach(b => {
            if (!suggestionToStats.contains(b))
              suggestionToStats(b) = new SuggestionTotals(b,0,0,0)
            suggestionToStats(b).best += 1
          })


          val curBest = best.count(_.contains(m))

          methodToBest(m) += curBest

          if (!methodToWorst.contains(m))
            methodToWorst(m) = 0

          worst.foreach(w => {
            if (!suggestionToStats.contains(w))
              suggestionToStats(w) = new SuggestionTotals(w,0,0,0)
            suggestionToStats(w).worst += 1
          })

          val curWorst = worst.count(_.contains(m))

          methodToWorst(m) += curWorst

          writer.write("%d,%s,%s,%d,%d,%d\n".format(pid, worker, m, curBest, curWorst, 4))
          writer.flush()

        }
      }
    }


    //ok, now go through the loaded answers, and output files
    //for proportion test and ANOVA

    for (m <- methods)
    {
      println("Method %s Best: %d Worst: %d Total: %d".format(m,methodToBest(m), methodToWorst(m), total*4))
      aggregateWriter.write("%s,%d,%d,%d\n".format(m, methodToBest(m), methodToWorst(m), total*4))
      aggregateWriter.flush()
    }

    //write out the html file




    writer.close()
    aggregateWriter.close()
    htmlWriter.close()

    source.close()


    //



  }


  def InspectNameCentroids(model:ColorInferenceModel)
  {
    val summary = model.summary
    val terms = ModelTraining.namingModel.getAllTerms
    var break = false
    for (s <- summary.histograms if (!break))
    {
      if (s.propname.contains("ColorNames"))
      {
        //print out the centroids in a readable format, for each centroid, print out the top 5 terms plus counts
        val centroids = s.hist.getCentroids
        for (c <- centroids)
        {
          val topK = c.top(5)
          val cTerms = topK.map(i=>(terms(i.index) + ":" + i.score))
          println(cTerms.mkString(", "))
          break = true
        }
      }
    }
  }


}
