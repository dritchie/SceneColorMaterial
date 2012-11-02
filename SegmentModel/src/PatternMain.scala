/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 11/2/12
 * Time: 12:32 AM
 * To change this template use File | Settings | File Templates.
 */


import cc.factorie.{SamplingMaximizer, VariableSettingsSampler}
import collection.mutable.ArrayBuffer
import java.io.File

object PatternMain {
  //Just a place to test loading and evaluating patterns/images. Subject to much change
  val inputDir = "../PatternColorizer/out/mesh"
  val outputDir = "../PatternColorizer/out/specs"

  var meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]()
  var files:Array[File] = null

  def main(args:Array[String])
  {
    //load all files
    files = new File(inputDir).listFiles.filter(_.getName.endsWith(".txt"))

    if (files.length == 0)
      println("No files found in the input directory!")

    for (f <- files)
    {
      meshes.append(new SegmentMesh(f.getAbsolutePath))
    }

    //for now, just try the contrast model on the images, using the original palette
    //probably highly overfitting..
    var avgScore:Double = 0

    for (idx <- meshes.indices)
    {
      val segmesh = meshes(idx)

      val model = new MaintainObservedContrastModel(segmesh)
      val palette = new ColorPalette(segmesh)
      DiscreteColorVariable.initDomain(palette.colors)

      // Do inference
      val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
      val optimizer = new SamplingMaximizer(sampler)
      optimizer.maximize(for (group <- segmesh.groups) yield group.color, 100)

      // Output the result
      segmesh.saveColorAssignments(outputDir+"/"+files(idx).getName)

      //Evaluate assignments
      val score = segmesh.scoreAssignment()
      println(files(idx).getName+" Score: "+score)
      avgScore += score

    }
    println("Average Score: " + (avgScore/meshes.length))

  }

}
