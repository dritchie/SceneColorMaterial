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

    //load all files TODO: to make this manageable, let's try out two subfolders for now
    patterns = PatternIO.getPatterns(inputDir).filter(p=>(p.directory == "sugar!" || p.directory=="cameo")).toArray

    if (patterns.length == 0)
      println("No files found in the input directory!")

      // Setup model training parameters (we'll use Discrete color variables in this test)
      val params = new ModelTrainingParams
      {
          type VariableType = DiscreteColorVariable
          val colorVarParams = DiscreteColorVariableParams
          regression = HistogramRegressor.LogisticRegression
      }

    for (p <- patterns)
    {
      meshes.append(new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath))
    }

    //list of patterns to consider
    val pids = Array(
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


    //train a model on all patterns, except the ones being considered (train/test set)
    val testingMeshes = {for (idx<-meshes.indices if (pids.contains(patterns(idx).name.replace(".txt","").toInt))) yield meshes(idx)}
    val trainingMeshes = {for (m<-meshes if (!testingMeshes.contains(m))) yield m}

    println("Training on " + trainingMeshes.length + " meshes")
    val model = ModelTraining(trainingMeshes, params)

    /*val (totalScore, randomScore) = testingMeshes.foldLeft[(Double,Double)]((0.0,0.0))((curSum, mesh) =>
    {
      val (score, rscore) = TestModel(mesh, model)
      (curSum._1+score, curSum._2+rscore)
    } )

    //higher score is better
    println("Average score " + totalScore/testingMeshes.length)
    println("Average random score " + randomScore/testingMeshes.length)*/


    OutputVisualizations(pids, model, "allhist.txt")

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
  def OutputVisualizations(pids:Array[Int], model:ColorInferenceModel, histfilename:String)
  {
      // TODO: This isn't possible anymore, given the new treatment of ModelTrainingParams.
      // TODO: Actually, I don't think this *ever* had any effect. The histogram regressors have already
      // TODO:   been trained, so changing the regression type doesn't do anything, right?
//    //change the regression type
//    ModelParams.regression = regression

    val hallfilename = histDir + "/"+ histfilename
    val file = new File(hallfilename)
    file.delete()

    var count=0
    for (idx <- meshes.indices
         if (pids.contains(patterns(idx).name.replace(".txt","").toInt)))
    {
      println("Testing mesh " + patterns(idx).name)
      val vfilename = PatternIO.ensureAndGetFileName(patterns(idx), visDir, ".txt")//visDir + "/"+patterns(idx).name

      val palette = ColorPalette(meshes(idx))
      DiscreteColorVariable.initDomain(palette)

      model.conditionOn(meshes(idx))


      val patternId = patterns(idx).name.replace(".txt","").toInt

      OutputHistograms(meshes(idx), model, hallfilename, patternId, true, count==0)
      OutputAllPermutations(meshes(idx), model, palette, vfilename)
      count+=1

    }
  }

  def OutputHistograms(mesh:SegmentMesh, model:ColorInferenceModel, filename:String, patternId:Int, append:Boolean, headers:Boolean)
  {
    //output the histograms in a csv format
    //TODO: output group marginals
    //TODO: current format may not work for histograms greater than 1D
    //TODO: add more summary items?
    val out = new FileWriter(filename, append)
    if (headers)
      out.write("\"pattern\",\"factortype\",\"property\",\"ids\",\"bin\",\"value\",\"smoothed\"\n")

    val summary = model.summary

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
        out.write(palette(p(i)).copyIfNeededTo(RGBColorSpace).componentString + "\n")
      }
    }
    out.close()

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
