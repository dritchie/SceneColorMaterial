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


class PatternItem(val fullpath:String, val directory:String, val name:String)

/**Handles reading and output of patterns**/
object PatternIO {
  def getPatterns(basedir:String):Seq[PatternItem] =
  {
     val patterns = new ArrayBuffer[PatternItem]
     val folders =  (new File(basedir)).listFiles.filter(_.isDirectory).map(_.getAbsolutePath)
     for (d <- folders)
     {
       val files = new File(d).listFiles().filter(_.isFile)
       val folderfiles = files.map(f => new PatternItem(f.getAbsolutePath, f.getParentFile.getName, f.getName))
       patterns ++= folderfiles
     }
    patterns
  }

  def ensureAndGetFileName(info:PatternItem, outDir:String, extension:String):String =
  {
      val suboutDir = new File(outDir, info.directory)
      suboutDir.mkdir()

      val filename = new File(suboutDir, info.name.replace(".txt","").replace(".png","")+extension) //just handle the .png and .txt filename extensions for now...
      filename.getAbsolutePath()
  }



}
