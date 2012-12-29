package colorinference

import cc.factorie.{HashMapAssignment, Factor, Model}
import java.awt
import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter
import java.io.File
import java.lang.Runtime

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 12/27/12
 * Time: 11:57 AM
 * To change this template use File | Settings | File Templates.
 */

object DebugRuntime
{
  val runtime = Runtime.getRuntime
  val mb = 1024*1024
  def printStats = { println("Memory " + runtime.totalMemory/mb + " out of " + runtime.maxMemory/mb + " Freespace " + runtime.freeMemory/mb )}
}
object PaletteFindingMain {
  //find patterns with 5-color palettes that are low-scoring, or have a high variance scores depending on order

  case class PaletteStats(artist:String, patternid:String, minScore:Double, maxScore:Double, avgScore:Double)
  var paletteScores:Seq[PaletteStats] = null
  var patterns:Array[PatternItem] = null
  val inputDir:String = "../PatternColorizer/out/mesh"
  val visDir:String = "../PatternColorizer/out/vis"
  val meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]

  def main(args:Array[String])
  {
    val filename:String = "paletteScores.csv"
    val testFile = new File(filename)
    if (!testFile.exists)
    {
      println("Creating color vars...")
      val colorvars = for (i <- 0 until 5) yield
      {
        val cvar = new ContinuousColorVariable(null, null)
        cvar.setColor(Color.RGBColor(math.random, math.random, math.random))
        cvar
      }

      // Create a color compatibility factor over those variables
      println("Creating compatibility factor...")
      val family = new ColorCompatibilityFamily
      val factor = new family.Factor
      factor.vars ++= colorvars

      // Define an itemized model that just contains this one factor
      println("Creating itemized model...")
      val model = new ItemizedColorInferenceModel(factor.asInstanceOf[Factor])


      outputPatternPaletteScores(colorvars, model, filename)
    }

    //read in the file
    paletteScores = loadPaletteScores(filename)

    DebugRuntime.printStats

    //Find the palettes with the minimum min score, the minimum average score, and the greatest difference between min and max scores
    //min score
    println("Palettes with min score")
    val minscore = paletteScores.sortWith((a,b)=>a.minScore < b.minScore)
    for (i <- 0 until 5)
      println("%s %s %f".format(minscore(i).artist, minscore(i).patternid, minscore(i).minScore))

    //min average score
    println("Palettes with min average score")
    val minavg = paletteScores.sortWith((a,b)=>a.avgScore < b.avgScore)
    for (i <- 0 until 5)
      println("%s %s %f".format(minavg(i).artist, minavg(i).patternid, minavg(i).avgScore))

    //greatest max-min
    println("Palettes with greatest range in score")
    val maxrange = paletteScores.sortWith((a,b)=>(a.maxScore-a.minScore) > (b.maxScore-b.minScore))
    for (i <- 0 until 5)
      println("%s %s %f".format(maxrange(i).artist, maxrange(i).patternid, maxrange(i).maxScore-maxrange(i).minScore))

    //combine them all into one list
    val ptoconsider = Seq.concat(minscore.take(5), minavg.take(5), maxrange.take(5)).toSet

    val artists = ptoconsider.map(_.artist)
    val pids = ptoconsider.map(_.patternid.toInt)

    // Verify that visDir exists
    val visDirTestFile = new File(visDir)
    if (!visDirTestFile.exists)
      visDirTestFile.mkdir

    //load all files TODO: for now, lonly loading relevant artists' folders (who have a pattern within pids)
    patterns = PatternIO.getPatterns(inputDir).filter(p=>(artists.contains(p.directory))).toArray

    if (patterns.length == 0)
      println("No files found in the input directory!")

    // Setup model training parameters (we'll use Discrete color variables in this test)
    val params = new ModelTrainingParams
    {
      type VariableType = DiscreteColorVariable
      val colorVarParams = DiscreteColorVariableParams
      includeUnaryTerms = true
      regression = HistogramRegressor.LogisticRegression
      modelSaveDirectory = "paletteTestModel"
      saveRegressorsIfPossible = true
      saveWeightsIfPossible = true
      loadRegressorsIfPossible = true
      loadWeightsIfPossible = true
    }

    for (p <- patterns)
    {
      meshes.append(new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath))
    }

    //train a model on all patterns, except the ones being considered (train/test set)
    val testingMeshes = {for (idx<-meshes.indices if (pids.contains(patterns(idx).name.replace(".txt","").toInt))) yield meshes(idx)}
    val trainingMeshes = {for (m<-meshes if (!testingMeshes.contains(m))) yield m}

    println("Training on " + trainingMeshes.length + " meshes")
    val pmodel = ModelTraining(trainingMeshes, params)

    for (idx <- meshes.indices if testingMeshes.contains(meshes(idx)))
    {
      println("Testing mesh " + patterns(idx).name)
      val vfilename = PatternIO.ensureAndGetFileName(patterns(idx), visDir, ".txt")

      val palette = ColorPalette(meshes(idx))
      DiscreteColorVariable.initDomain(palette)

      pmodel.conditionOn(meshes(idx))

      OutputAllPermutations(meshes(idx), pmodel, palette, vfilename)
    }


  }

  def outputPatternPaletteScores(cvars:IndexedSeq[ContinuousColorVariable], m:Model, filename:String)
  {
    println("Computing scores of all permutations of ColourLovers pattern palettes...")

    val datafile = "../../Colourlovers/patterns.csv"
    val lines = io.Source.fromFile(datafile).getLines().map(_.split(',')).toSeq

    // Extract data
    println("Extracting data...")
    var i = 0


    val file = new FileWriter("paletteScores.csv")
    file.write("artist,id,minscore,maxscore,avgscore\n")
    file.flush()

    for (line <- lines)
    {
      // Fields are: artist, id, url, palette
      // Palette is a list of space separated 24bit hex RGB colors
      if (line.length == 4)
      {
        val artist = line(0)
        val id = line(1)
        val palette = line(3).split(' ').distinct

        // We can only really evaluate the model on 5 color palettes
        if (palette.length == 5)
        {
          val colors = for (cstr <- palette) yield
          {
            val ac = awt.Color.decode("0x"+cstr)
            val rgb = new Array[Float](3)
            ac.getRGBColorComponents(rgb)
            Color.RGBColor(rgb(0), rgb(1), rgb(2))
          }

          // Generate and score all permutations of the palette
          val datumlist = (for (cperm <- colors.permutations) yield
          {
            for (i <- 0 until cperm.length) cvars(i).setColor(cperm(i))
            val score = math.exp(m.currentScore(cvars))*5
            score
          }).toSeq

          // Get the optimal score
          val maxs = datumlist.reduce((a,b) => {if (a > b) a else b})

          // Get the minimum score
          val mins = datumlist.reduce((a,b) => {if (a > b) b else a})

          // Get the average score
          val avgs = datumlist.sum/datumlist.length


          file.write("%s,%s,%s,%f,%f\n".format(artist,id,mins, maxs, avgs))
          file.flush()
        }
      }
      i += 1
      println("Done %d/%d".format(i, lines.length))
    }


    println("DONE")
    file.close()
  }


  def loadPaletteScores(filename:String):Seq[PaletteStats] =
  {
    val paletteList = new ArrayBuffer[PaletteStats]
    val lines = io.Source.fromFile(filename).getLines().map(_.split(',')).toSeq

    for (idx <- 1 until lines.length)
    {
      val line = lines(idx)
      paletteList += new PaletteStats(line(0), line(1), line(2).toDouble, line(3).toDouble, line(4).toDouble)
    }


    paletteList

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



}
