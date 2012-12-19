package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/18/12
 * Time: 11:29 PM
 * To change this template use File | Settings | File Templates.
 */

// Testing against http://vis.stanford.edu/color-names/analyzer/
object ColorNamingTestMain
{
    val namingModel = new ColorNamingModel("../c3_data.json")

    def main(args:Array[String])
    {
        val blue = Color.RGBColor(31/255.0, 119/255.0, 180/255.0)
        val lightblue = Color.RGBColor(174/255.0, 199/255.0, 232/255.0)
        val red = Color.RGBColor(214/255.0, 39/255.0, 40/255.0)

        val blueLAB = blue.copyIfNeededTo(LABColorSpace)

        // Test saliency
        // (Linear remap as done in the c3 paper)
        val sal_blue = normalizedSaliency(blue)
        val sal_lightblue = normalizedSaliency(lightblue)
        val sal_red = normalizedSaliency(red)

        // Test color name distance
        val dist_blue_lightblue = 1.0 - namingModel.cosineSimilarity(blue, lightblue)
        val dist_blue_red = 1.0 - namingModel.cosineSimilarity(blue, red)

        println("Done")
    }

    def linearRemap(x:Double, srcmin:Double, srcmax:Double, dstmin:Double, dstmax:Double) =
    {
        val t = (x - srcmin) / (srcmax - srcmin)
        (1-t)*dstmin + t*dstmax
    }

    def normalizedSaliency(color:Color) =
    {
        linearRemap(namingModel.saliency(color), -4.5, 0, 0, 1)
    }
}
