/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/17/12
 * Time: 3:54 PM
 * To change this template use File | Settings | File Templates.
 */


object Main
{
    def main(args: Array[String])
    {
//        // Test loading up a segment mesh
//        val filename = "../SceneViewer/SegmentationOutput/modelDescription.txt"
//        val segmesh = new SegmentMesh(filename)
//        println("Done with all the things")


      //Testing color conversions
      val rgb = Color.RGBColor(1,0,0)
      val lab = new Color(rgb)
      lab.convertTo(LABColorSpace)

      println("rgb red to lab " + lab.toString())
      println("Converting red from rgb to lab and back " + (LABColorSpace.toRGB _ ).tupled(LABColorSpace.fromRGB(1,0,0)))
      println("Converting red from rgb to hsv and back " + (HSVColorSpace.toRGB _).tupled(HSVColorSpace.fromRGB(1,0,0)))



    }
}
