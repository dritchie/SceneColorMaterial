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
    }
}
