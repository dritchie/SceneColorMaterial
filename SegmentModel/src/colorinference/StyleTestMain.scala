package colorinference

import collection.immutable.HashMap
import collection.mutable.ArrayBuffer
import scala.Array
import java.io.{File, FileWriter}
import cc.factorie.{SamplingMaximizer, HashMapAssignment}
import colorinference.ColorPalette

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
  val imageDir:String = "../../Colourlovers/styles"
  val meshDir:String = "../PatternColorizer/out/mesh"
  val outDir:String = "../PatternColorizer/out/styles"
  val histDir:String = "../PatternColorizer/out/hist"
  var styleToModel = new HashMap[String, StyleItem] //map from the style name to the model
  var meshToStyleInfo = new HashMap[String, PatternItem]

  val n = 3 //number of test patterns per style

  def main(args:Array[String])
  {
    // Verify that outDir exists
    val visDirTestFile = new File(outDir)
    if (!visDirTestFile.exists)
      visDirTestFile.mkdir

    val params = new ModelTrainingParams
    {
      type VariableType = ContinuousColorVariable
      val colorVarParams = ContinuousColorVariableParams
      includeColorCompatibilityTerm = true
      saveRegressorsIfPossible = true
      saveWeightsIfPossible = true
      loadRegressorsIfPossible = true
      loadWeightsIfPossible = true
      modelSaveDirectory = "" //this will be changed to whatever the style folder is named


      initialLearningRate = 0.2
      numWeightTuningIterations = 10
      enforceMinimumWeight = true
      minWeight = 0.0

      includeColorChoiceTerms = true
    }

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
    var styleToPatterns = new HashMap[String, ArrayBuffer[PatternItem]]
    for (pattern <- patterns)
    {
      val styleinfo = meshToStyleInfo(pattern.fullpath)
      if (!styleToPatterns.contains(styleinfo.directory))
        styleToPatterns += (styleinfo.directory) -> new ArrayBuffer[PatternItem]
      styleToPatterns(styleinfo.directory) += pattern
    }
    for (s <- styleToPatterns.keys)
    {
      //change the save folder
      params.modelSaveDirectory = s+"groupconditional"

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

    //for each test pattern, generate the top N colorings from each of the style models
    //visualization columns will look like:
    //original pattern + style name, style A, style B, style C
    //output file will look like:
    //filename: patternid.txt
    //headers: original style name, style name, style colors
    for (s <- styleToModel.keys)
    {
      val testPatterns = styleToModel(s).testPatterns.map(_.fullpath)
      var testMeshes = styleToModel(s).meshes.filter(m => testPatterns.contains(m.name))

      //output histograms for test meshes
      OutputAllHistograms(testMeshes, styleToModel(s).model, s)

      for (mesh<-testMeshes)
      {
        val fname = PatternIO.ensureAndGetFileName(meshToStyleInfo(mesh.name), outDir, ".txt")
        val file = new FileWriter(fname)


        for (s2 <- styleToModel.keys)
        {
          val item = styleToModel(s2)
          //mesh.randomizeVariableAssignments
          //mesh.setVariableValuesToObserved
          item.model.conditionOn(mesh)

          params.colorVarParams.initDomain(mesh)

          val lambda = 0.25
          //settings copied from MMRTest
          val iterations = 2000
          val initTemp = 1.0
          val finalTemp = 0.01
          val rounds = 40
          val numParallelChains = 5

          val sampler = new ContinuousColorSampler(item.model) with MemorizingSampler[ContinuousColorVariable]
          val maximizer = new SamplingMaximizer(sampler)

          //val samplerGenerator = () => new ContinuousColorSampler(item.model) with MemorizingSampler[ContinuousColorVariable]
          //val maximizer = new ParallelMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, numParallelChains)
          //maximizer.maximize(mesh, iterations, initTemp, finalTemp, rounds)
          maximizer.maximize(Array(mesh.variablesAs[ContinuousColorVariable]), iterations, initTemp, finalTemp, rounds)
          // Convert sampled values to LAB first, since our similarity metric operates in LAB space
          //maximizer.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))
          sampler.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

          val metric = genMMRSimilarityMetric(mesh.variablesAs[ContinuousColorVariable])
          //val mmr = new MMR(maximizer.samples, metric)
          val mmr = new MMR(sampler.samples, metric)

          val rankedSamples = mmr.getRankedSamples(n, lambda)

          //output the results, original style name, style name, style colors separated by ^
          for (r<-rankedSamples)
            file.write("%s,%s,%s,%f\n".format(s, s2, r.values.map(_.copyIfNeededTo(RGBColorSpace).componentString).mkString("^"), r.score))

          file.flush()
        }

        file.close()
      }
    }


    //test 2 is to see if for each model, the model rates the test patterns in that style more highly than patterns in other styles
    //output file
    //print out style model, ScoreWithinStyle, ScoreStyleA, ScoreStyleB
    for (s <- styleToModel.keys)
    {
      println("Inspecting style model %s".format(s))
      val item = styleToModel(s)
      val testPatterns = item.testPatterns.map(_.fullpath)
      var testMeshes = item.meshes.filter(m => testPatterns.contains(m.name))

      println(testMeshes.length)
      val sumWithin = testMeshes.foldLeft(0.0)((sofar, mesh) => {
        val curScore = getObservedAssignmentScore(item.model, mesh)
        sofar+curScore
      })
      println("Within style %s, avgscore %f".format(s, sumWithin/testMeshes.length))

      for (s2 <- styleToModel.keys if s!=s2)
      {
        val item2 = styleToModel(s2)
        val sumOut = testMeshes.foldLeft(0.0)((sofar, mesh) => {
          val curScore = getObservedAssignmentScore(item2.model, mesh)
          sofar+curScore
        })
        println("Style %s, avgscore %f".format(s2, sumOut/testMeshes.length))

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
    model.itemizedModel(mesh.groups.map(_.color)).currentScore

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

      OutputHistograms(model, hallfilename, patternId, true, count==0)
      count+=1

    }
  }

  def OutputHistograms(model:ColorInferenceModel, filename:String, patternId:Int, append:Boolean, headers:Boolean)
  {
    //output the histograms in a csv format
    val out = new FileWriter(filename, append)
    if (headers)
      out.write("\"pattern\",\"factortype\",\"property\",\"ids\",\"bin\",\"value\",\"smoothed\"\n")

    val summary = model.summary

    for (s <- summary.histograms)
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




}
