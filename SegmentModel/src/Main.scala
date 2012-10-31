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
        // Test loading up a segment mesh
        val filename = "../SceneViewer/SegmentationOutput/modelDescription.txt"
        val segmesh = new SegmentMesh(filename)
        println("Done with all the things")



      //Testing color conversions
      var rgb:RGBColor = new RGBColor(255,0,0)
      var lab:LABColor = new LABColor(0,0,0)
      lab.initFrom(rgb)

      var hsv:HSVColor = new HSVColor(0,0,0)


      println("rgb red to lab " + lab.toString())
      println("Converting red from rgb to lab and back " + (lab.toRGB _ ).tupled(lab.fromRGB(255,0,0)))
      println("Converting red from rgb to hsv and back " + (hsv.toRGB _).tupled(hsv.fromRGB(255,0,0)))



    }
}
