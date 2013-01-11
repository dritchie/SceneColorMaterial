package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 12/24/12
 * Time: 1:30 PM
 * To change this template use File | Settings | File Templates.
 */


import collection.mutable.ArrayBuffer
import collection.mutable.HashSet
import collection.mutable.HashMap
import cc.factorie.la.Tensor1
import cc.factorie.la.DenseTensor1
import io.Source
import java.io.FileWriter
import collection.mutable
import util.Random
import java.io.{File,FileInputStream,FileOutputStream}


class PatternItem(val fullpath:String, val directory:String, val name:String, val pid:Int)

/**Handles reading and output of patterns**/
object PatternIO {

  private val colorLoversDir = "../Colourlovers"
  private val artistsFile = new File(colorLoversDir, "artsts.csv")
  private val templatesFile = new File(colorLoversDir, "templates.csv")
  private val imageDir = "../Colourlovers/data"

  //get the list of sorted artists
  private val artists = Source.fromFile(artistsFile).getLines().map(_.split(',')).toList(0)

  //build a map from pid to template id
  private val pidToTemplateId = populateTemplateIds()

  //build a map from "valid" pids (that don't have multiple same colors), to artists
  //we currently can't recolor those pids properly using Colourlovers, though it is possible
  //to, if we check for that case
  //this just makes the pattern image easier to find
  private val pidToArtist = populateArtistsForTurk()

  private def populateTemplateIds():mutable.HashMap[Int,Int] =
  {
    val pToT= new mutable.HashMap[Int,Int]()
    val source = Source.fromFile(templatesFile)
    val lineIterator = source.getLines()
    while (lineIterator.hasNext)
    {
      val line = lineIterator.next()
      val tokens = line.split(',')
      val pid:Int = tokens(1).toInt
      val tid:Int = {if (tokens(4)=="none") -1 else tokens(4).toInt} //we'll treat all patterns without a template as the same...
      pToT(pid) = tid
    }
    source.close()
    pToT
  }

  private def populateArtistsForTurk():mutable.HashMap[Int,String] =
  {
    val pToA = new mutable.HashMap[Int,String]()

    val source = Source.fromFile(templatesFile)
    val lineIterator = source.getLines()
    while (lineIterator.hasNext)
    {
      val line = lineIterator.next()
      val tokens = line.split(',')
      val colors = tokens(3).split(" ")
      if (colors.distinct.length == colors.length)
      {
        val pid:Int = tokens(1).toInt
        val artist:String = tokens(0)
        pToA(pid) = artist
      }
    }
    source.close()
    pToA
  }


  def getPatterns(basedir:String):Seq[PatternItem] =
  {
     val patterns = new ArrayBuffer[PatternItem]
     val folders =  (new File(basedir)).listFiles.filter(_.isDirectory).map(_.getAbsolutePath)
     for (d <- folders)
     {
       val files = new File(d).listFiles().filter(_.isFile)
       val folderfiles = files.map(f => new PatternItem(f.getAbsolutePath, f.getParentFile.getName, f.getName, f.getName.replace(".txt","").replace(".png","").toInt))  //handling .png and .txt extensions for now
       patterns ++= folderfiles
     }
    patterns
  }

  def ensureAndGetFileName(info:PatternItem, outDir:String, extension:String):String =
  {
      val outDirFile = new File(outDir)
      outDirFile.mkdir()

      val suboutDir = new File(outDir, info.directory)
      suboutDir.mkdir()

      val filename = new File(suboutDir, info.name.replace(".txt","").replace(".png","")+extension) //just handle the .png and .txt filename extensions for now...
      filename.getAbsolutePath
  }

  //sometimes a pid is not in the templates file...not sure why that is. Might have been due to a failed download attempt
  def getTemplateId(pid:Int):Int =
  {
    if (pidToTemplateId.contains(pid))
      pidToTemplateId(pid)
    else
      -1
  }

  //get the top n artists
  def getTopNArtists(n:Int):Seq[String] =
  {
    artists.take(n)
  }

  /** Filtering methods **/

  //returns a subset of the training set that has no common templates with the given pids
  def filterTrainingSet(trainingSet:Seq[PatternItem], pids:Seq[Int]): Seq[PatternItem] =
  {
    val tids = pids.map(p => getTemplateId(p)).toSet
    trainingSet.filter(p => !tids.contains(getTemplateId(p.pid)) && getTemplateId(p.pid)>=0)    //to be safe, we'll also just filter out patterns with no template
  }

  //convenience method for getting the patterns of the top n artists, without the templates of test pids
  def getTopNArtistPatterns(meshDir:String, n:Int, pids:Seq[Int]):Seq[PatternItem] =
  {
    val topArtists = artists.take(n)
    val unfilteredTrainingSet = getPatterns(meshDir).filter(p => topArtists.contains(p.directory))
    filterTrainingSet(unfilteredTrainingSet, pids)
  }


  /** MTurk methods **/

  //pick a random larger pool of patterns (that do not share the same templateid)
  //and also "respect" the template (i.e. they do not color two color groups the same color)
  //We'll have to manually remove those that are semantically strong..i.e. have people
  //copy the images to the copyDir directory
  def getRandomUniquePatterns(n:Int, copyDir:String)
  {

     new File(copyDir).mkdir()

     val tidsSoFar = HashSet[Int](-1) //let's not use any patterns that don't have templates
     val patterns = HashSet[Int]()
     var candidates = pidToArtist.keys.toList
     candidates = candidates.filter(c => !tidsSoFar.contains(pidToTemplateId(c)))

     val r = new Random()
     while (patterns.size < n)
     {
       val cand = candidates(r.nextInt(candidates.size))
       patterns.add(cand)
       val tid = pidToTemplateId(cand)
       tidsSoFar.add(tid)
       candidates = candidates.filter(c => tid != pidToTemplateId(c))
     }

    //print out the pids and artist
    for (p<-patterns)
      println(p + " " + pidToArtist(p))

    //form the filenames
    var idx = 0
    patterns.foreach (p => {
      val src = new File(imageDir+"/"+pidToArtist(p), p+".png")
      val dest = new File(copyDir, p+".png")
      idx+=1
      new FileOutputStream(dest) getChannel() transferFrom(
        new FileInputStream(src) getChannel, 0, Long.MaxValue )
    }
    )
  }



}
