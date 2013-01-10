package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 12/24/12
 * Time: 1:30 PM
 * To change this template use File | Settings | File Templates.
 */


import collection.mutable.ArrayBuffer
import java.io.File
import collection.mutable.HashSet
import collection.mutable.HashMap
import cc.factorie.la.Tensor1
import cc.factorie.la.DenseTensor1
import io.Source
import java.io.FileWriter
import collection.mutable


class PatternItem(val fullpath:String, val directory:String, val name:String, val pid:Int)

/**Handles reading and output of patterns**/
object PatternIO {

  private val colorLoversDir = "../Colourlovers"
  private val artistsFile = new File(colorLoversDir, "artsts.csv")
  private val templatesFile = new File(colorLoversDir, "templates.csv")

  //get the list of sorted artists
  private val artists = Source.fromFile(artistsFile).getLines().map(_.split(',')).toList(0)

  //build a map from pid to template id
  private val pidToTemplateId = populateTemplateIds()

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

  //returns a subset of the training set that has no common templates with the given pids
  def filterTrainingSet(trainingSet:Seq[PatternItem], pids:Seq[Int]): Seq[PatternItem] =
  {
    val tids = pids.map(p => getTemplateId(p)).toSet
    trainingSet.filter(p => !tids.contains(getTemplateId(p.pid)))
  }

  //convenience method for getting the patterns of the top n artists, without the templates of test pids
  def getTopNArtistPatterns(meshDir:String, n:Int, pids:Seq[Int]):Seq[PatternItem] =
  {
    val topArtists = artists.take(n)
    val unfilteredTrainingSet = getPatterns(meshDir).filter(p => topArtists.contains(p.directory))
    filterTrainingSet(unfilteredTrainingSet, pids)
  }



}
