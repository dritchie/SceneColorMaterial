package colorinference

import collection.immutable.HashMap
import collection.mutable.ArrayBuffer
import scala.Array
import java.io.{File, FileWriter}
import cc.factorie.{Family, SamplingMaximizer, HashMapAssignment}

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 12/27/12
 * Time: 3:04 PM
 * To change this template use File | Settings | File Templates.
 */
//testing if the model can capture different styles

object StyleTestMain {
  case class StyleItem( model:ColorInferenceModel,  trainPatterns:Seq[PatternItem],  testPatterns:Seq[PatternItem],  meshes:Seq[SegmentMesh])
  val imageDir:String = "../Colourlovers/styles"
  val meshDir:String = "../PatternColorizer/out/mesh"
  val outDir:String = "../PatternColorizer/out/styles"
  val histDir:String = "../PatternColorizer/out/hist"
  var styleToModel = new HashMap[String, StyleItem] //map from the style name to the model
  var meshToStyleInfo = new HashMap[String, PatternItem]
  var styleToPatterns = new HashMap[String, ArrayBuffer[PatternItem]]
  val savedModelsDir = "style_colorunary_groupWeighted"

  /** filtering options **/
  val filterWhenTesting = true
  val adjK = -1
  val segK = -1
  val ignoreNoise = true

  //inference params

  // Maximization parameters
  val numParallelChains = 5
  val iterations = 2000
  val initTemp = 1.0
  val finalTemp = 0.01
  val rounds = 40

  // Parallel tempering parameters
  val chainTemps = Array(1.0, 0.5, 0.2, 0.05, 0.01)
  val itersBetweenSwaps = 50

  val n = 3 //number of test patterns per style

  val lambdas = Array(1.0, 0.5)

  case class Setting(includeColorChoiceTerms:Boolean, colorChoiceType:ModelTraining.ColorChoiceType.Value, directoryLabel:String)
  val settings = Array(
    //new Setting(false, ModelTraining.ColorChoiceType.LABMarginal, ""),
    //new Setting(true, ModelTraining.ColorChoiceType.LABMarginal, "_labMarginal"),
    //new Setting(true, ModelTraining.ColorChoiceType.LABConditional,"_labConditional"),
    //new Setting(true, ModelTraining.ColorChoiceType.NamesMarginal,"_namesMarginal")
    new Setting(true, ModelTraining.ColorChoiceType.NamesConditional, "_namesConditional")
    )


  //default model training params
  val params = new ModelTrainingParams
  {
    type VariableType = ContinuousColorVariable
    val colorVarParams = ContinuousColorVariableParams
    includeColorCompatibilityTerm = true
    saveRegressorsIfPossible = true
    saveWeightsIfPossible = true
    loadRegressorsIfPossible = false
    loadWeightsIfPossible = false
    modelSaveDirectory = "" //this will be changed to whatever the style folder is named

    initialLearningRate = 0.2
    numWeightTuningIterations = 20
    enforceMinimumWeight = true
    minWeight = 0.0

    includeColorChoiceTerms = true
    colorChoiceType = ModelTraining.ColorChoiceType.NamesMarginal
    includeUnaryTerms = true
    includeGroupTerms = true
    includeBinaryTerms = true

    weightGroups = true //I think setting this to true produces more expected results for some styles (i.e. blue and pink), and is a bit more consistent
  }

  def main(args:Array[String])
  {
    // Verify that outDir exists
    val visDirTestFile = new File(outDir)
    if (!visDirTestFile.exists)
      visDirTestFile.mkdir

     new File(savedModelsDir).mkdir

     val styleinf = PatternIO.getPatterns(imageDir)
     val validPids = styleinf.map(_.name.replace(".png","").toInt).toSet

     //create maps from the mesh info to the style info
     meshToStyleInfo = new HashMap[String, PatternItem]

     val patterns = PatternIO.getPatterns(meshDir).filter(p => validPids.contains(p.name.replace(".txt","").toInt))
     patterns.foreach( p => {
       //find the style info
       val style = styleinf.find(s => s.name.replace(".png","") == p.name.replace(".txt","")).get
       meshToStyleInfo += p.fullpath -> style
      }
     )

    //get the unique style folders, and split the patterns into each folder
    styleToPatterns = new HashMap[String, ArrayBuffer[PatternItem]]
    for (pattern <- patterns)
    {
      val styleinfo = meshToStyleInfo(pattern.fullpath)
      if (!styleToPatterns.contains(styleinfo.directory))
        styleToPatterns += (styleinfo.directory) -> new ArrayBuffer[PatternItem]
      styleToPatterns(styleinfo.directory) += pattern
    }

    for (setting <- settings)
    {
      if (setting.includeColorChoiceTerms)
        println("Training a model using " + setting.directoryLabel)
      else
        println("Training a model ignoring color choice terms")

      TrainModels(setting)

      //OutputModelHistograms(setting:Setting)

      GeneratePatterns(setting)

      //test 2 is to see if for each model, the model rates the test patterns in that style more highly than patterns in other styles
      //output file
      //print out style model, ScoreWithinStyle, ScoreStyleA, ScoreStyleB
      CompareModelsOnObservedPatterns()


      //See what happens when we filter out noise and/or only consider the topK segments
      /*for (s <- styleToPatterns.keys)
      {
        val trainPatterns = styleToPatterns(s).take(styleToPatterns(s).length-n)
        val testPatterns = {for (p<-styleToPatterns(s)if (!trainPatterns.contains(p))) yield p}.map(_.fullpath)
        val meshes = styleToPatterns(s).map(p=> {
          new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)
        })

        var testMeshes = meshes.filter(m => testPatterns.contains(m.name))
        for (mesh <- testMeshes)
        {
          println("\nMesh "+mesh.name)
          InspectMeshStats(mesh, -1, -1, false)
          InspectMeshStats(mesh, -1, -1, true)
          InspectMeshStats(mesh, 5, 5, true)
        }
      }*/

      //print out the name centroids to see if they make sense
      for (s <- styleToModel.keys)
      {
        val model = styleToModel(s).model
        println("\nModel %s color name centroids".format(s))
        InspectNameCentroids(model)
      }


    }



  }

  def OutputModelHistograms(setting:Setting)
  {
    for (s <- styleToModel.keys)
    {
      val testPatterns = styleToModel(s).testPatterns.map(_.fullpath)
      var testMeshes = styleToModel(s).meshes.filter(m => testPatterns.contains(m.name))
      val trainPatterns= styleToModel(s).trainPatterns.map(_.fullpath)
      val trainMeshes = for (m<-styleToModel(s).meshes if trainPatterns.contains(m.name)) yield m

      //output histograms for test meshes
      //OutputAllHistograms(trainMeshes, styleToModel(s).model, s)
      OutputAllHistograms(testMeshes, styleToModel(s).model, s+setting.directoryLabel)
    }
  }




  def TrainModels(setting:Setting)
  {
    for (s <- styleToPatterns.keys)
    {
      //change the save folder
      params.modelSaveDirectory = savedModelsDir+"/"+s+setting.directoryLabel
      params.includeColorChoiceTerms = setting.includeColorChoiceTerms
      params.colorChoiceType = setting.colorChoiceType

      //train the model
      val trainPatterns = styleToPatterns(s).take(styleToPatterns(s).length-n)
      val testPatterns = {for (p<-styleToPatterns(s)if (!trainPatterns.contains(p))) yield p}
      val meshes = styleToPatterns(s).map(p=> {
        new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)
      })

      val trainMeshes = for (m<-meshes if trainPatterns.map(_.fullpath).contains(m.name)) yield m
      val model = ModelTraining(trainMeshes, params)

      styleToModel += s -> new StyleItem(model, trainPatterns, testPatterns, meshes)

    }
  }


  def GeneratePatterns(setting:Setting)
  {
    //for each test pattern, generate the top N colorings from each of the style models
    //visualization columns will look like:
    //original pattern + style name, style A, style B, style C
    //output file will look like:
    //filename: patternid.txt
    //headers: original style name, style name, style colors
    for (s <- styleToModel.keys if s=="blue" || s=="pink") //restrict it for now, to get some results faster
    {
      println("Test pattern from style " + s)
      val testPatterns = styleToModel(s).testPatterns.map(_.fullpath)
      var testMeshes = styleToModel(s).meshes.filter(m => testPatterns.contains(m.name)).take(1)

      val trainPatterns= styleToModel(s).trainPatterns.map(_.fullpath)
      val trainMeshes = {for (m<-styleToModel(s).meshes if trainPatterns.contains(m.name)) yield m}.take(5)

      for (mesh<-testMeshes)
      {
        //filter the mesh, if specified
        if (filterWhenTesting)
          mesh.filter(segK, adjK, ignoreNoise)

        var lambdaToFile = new HashMap[Double, FileWriter]()
        var doneAlready = true
        for (l <- lambdas)
        {
          var newOutDir = outDir+setting.directoryLabel+"_"+l
          new File(newOutDir).mkdir()
          newOutDir += "/styles"
          val fname = PatternIO.ensureAndGetFileName(meshToStyleInfo(mesh.name), newOutDir, ".txt")

          //see if it exists
          val test = new File(fname)
          if (!test.exists())
          {
            doneAlready = false
            val file = new FileWriter(fname)
            lambdaToFile += l -> file
          }
        }

        if (doneAlready)
          println("Skipping/Done with mesh: "+ mesh.name)
        else
        {
          for (s2 <- styleToModel.keys)
          {
            println("Sampling from model " + s2)

            val item = styleToModel(s2)
            item.model.conditionOn(mesh)

            params.colorVarParams.initDomain(mesh)

            /*val sampler = new ContinuousColorSampler(item.model) with MemorizingSampler[ContinuousColorVariable]
            val maximizer = new SamplingMaximizer(sampler)
            maximizer.maximize(Array(mesh.variablesAs[ContinuousColorVariable]), iterations, initTemp, finalTemp, rounds)
            // Convert sampled values to LAB first, since our similarity metric operates in LAB space
            sampler.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace))) */

            val model = item.model
            model.conditionOn(mesh)

            //val samplerGenerator = () => new ContinuousColorSampler(model) with MemorizingSampler[ContinuousColorVariable]
            //val maximizer = new ParallelTemperingMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, chainTemps)

            val samplerGenerator = () =>
            {
              new ContinuousColorSampler(model) with MemorizingSampler[ContinuousColorVariable]
            }
            val maximizer = new ParallelTemperingMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, chainTemps)
            maximizer.maximize(mesh, iterations, itersBetweenSwaps)


            // Convert sampled values to LAB first, since our similarity metric operates in LAB space
            maximizer.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

            val metric = genMMRSimilarityMetric(mesh.variablesAs[ContinuousColorVariable])
            val mmr = new MMR(maximizer.samples, metric)
            //val mmr = new MMR(sampler.samples, metric)

            //print out the original score
            val originalScore = getObservedAssignmentScore(model, mesh)
            val (minscore, maxscore) = mmr.getScoreRange
            val meanscore = maximizer.mixingScores(maximizer.mixingScores.length-1).meanF

            if (s == s2)
            {
              println("\nComparing %s On a %s pattern".format(s2, s))
              println("nObserved pattern score: " + originalScore)
              println("Min %f, Mean %f, Max %f".format(minscore, meanscore, maxscore))
            }

            for (l <- lambdas if lambdaToFile.contains(l))
            {
              val rankedSamples = mmr.getRankedSamples(n, l)
              val file = lambdaToFile(l)

              //output the results, original style name, style name, style colors separated by ^
              for (r<-rankedSamples)
                file.write("%s,%s,%s,%f\n".format(s, s2, r.values.map(_.copyIfNeededTo(RGBColorSpace).componentString).mkString("^"), r.score))
              file.flush()
            }
          }

          lambdaToFile.values.foreach(f => f.close())
        }
      }
    }
  }


  def CompareModelsOnObservedPatterns()
  {
    val test = true    //use test meshes or train meshes?

    for (s <- styleToModel.keys)
    {
      println("\nInspecting style model %s".format(s))
      val item = styleToModel(s)
      val testPatterns = item.testPatterns.map(_.fullpath)
      var testMeshes = item.meshes.filter(m => testPatterns.contains(m.name))

      val trainPatterns= styleToModel(s).trainPatterns.map(_.fullpath)
      val trainMeshes = for (m<-styleToModel(s).meshes if trainPatterns.contains(m.name)) yield m

      val inMeshes = {if (test) testMeshes else trainMeshes}

      val sumWithin = inMeshes.foldLeft(0.0)((sofar, mesh) => {
        val curScore = getObservedAssignmentScore(item.model, mesh)
        sofar+curScore
      })
      println("Within style %s, avgscore %f".format(s, sumWithin/inMeshes.length))

      for (s2 <- styleToModel.keys if s!=s2)
      {
        val item2 = styleToModel(s2)

        val testPatterns = item2.testPatterns.map(_.fullpath)
        var testMeshes = item2.meshes.filter(m => testPatterns.contains(m.name))

        val trainPatterns= styleToModel(s).trainPatterns.map(_.fullpath)
        val trainMeshes = for (m<-styleToModel(s).meshes if trainPatterns.contains(m.name)) yield m

        val outMeshes = {if (test) testMeshes else trainMeshes}

        val sumOut = outMeshes.foldLeft(0.0)((sofar, mesh) => {
          val curScore = getObservedAssignmentScore(item.model, mesh)
          sofar+curScore
        })
        println("Style %s, avgscore %f".format(s2, sumOut/outMeshes.length))

      }

    }
  }

  def getObservedAssignmentScore(model:ColorInferenceModel, mesh:SegmentMesh):Double =
  {
    /*val vars = mesh.groups.map(_.color)
    val assignment = new HashMapAssignment(vars)
    for (g <- mesh.groups)
    {
      assignment.update(g.color, g.color.observedColor.asInstanceOf[ColorVariable#Value])
    }
    model.conditionOn(mesh)
    val itemizedModel = model.itemizedModel(vars)
    for (f <- itemizedModel.factors)
    {
      f.variables.foreach{ e => e match {
        case(v:UnarySegmentTemplate.DatumVariable) => assignment.update(v, v.value)
        case(b:BinarySegmentTemplate.DatumVariable) => assignment.update(b, b.value)
        case (g:ColorGroupTemplate.DatumVariable) => assignment.update(g, g.value)
        case _ => null
      }}
    }
     model.assignmentScore(vars, assignment)*/
    //set the color vars to their observed color

    mesh.setVariableValuesToObserved()
    model.conditionOn(mesh)
    val score = model.itemizedModel(mesh.groups.map(_.color)).currentScore

    //revert
    mesh.randomizeVariableAssignments()

    score

  }




  /**These settings copied over from MMRTest**/
  // This is the maximum possible distance between two colors in LAB space
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

  def OutputAllHistograms(meshes:Seq[SegmentMesh], model:ColorInferenceModel, label:String)
  {
    val hallfilename = histDir + "/allhist"+ label+".txt"
    val cfilename = histDir + "/coeff"+label+".txt"
    val file = new File(hallfilename)
    file.delete()

    var count=0
    for (mesh <- meshes)
    {
      val info = meshToStyleInfo(mesh.name)

      println("Testing mesh " + info.name)

      model.conditionOn(mesh)


      val patternId = info.name.replace(".png","").toInt

      VisualizationIO.OutputHistograms(model, hallfilename, patternId, true, count==0)
      count+=1

    }
  }

  def InspectMeshStats(mesh:SegmentMesh, segK:Int = -1, adjK:Int = -1, ignoreNoise:Boolean)
  {
    //print out number of relevant segments, and percentage of original segments weights
    println("Filtering segK = %d, adjK = %d, ignoreNoise %b".format(segK, adjK, ignoreNoise))
    mesh.filter(segK, adjK, ignoreNoise)
    val segPercent = mesh.segments.map(_.size).sum
    println("Number of segments: %d, percent size of original: %f".format(mesh.segments.length, segPercent ))

    //print out number of adjacencies considered, and percentage of adjacencies weights
    var adjPercent =  0.0
    var count = 0
    for (seg1<-mesh.segments; adj<-seg1.adjacencies.values if seg1.index < adj.neighbor.index)
    {
      adjPercent += (adj.strength + adj.neighbor.adjacencies(seg1).strength)
      count+=1
    }
    println("Number of adjacencies %d, percent size of original: %f".format(count, adjPercent))
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
