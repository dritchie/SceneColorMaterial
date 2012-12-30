package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/24/12
 * Time: 10:38 AM
 * To change this template use File | Settings | File Templates.
 */

object ColorSpaceConversionCacheTest
{
    def main(args:Array[String])
    {
        // Generate 10 random colors
        val colors = for (i <- 0 until 10) yield Color.RGBColor(math.random, math.random, math.random)

        // Convert them all to LAB (10 cache misses, should fill up the cache)
        for (c <- colors) c.convertTo(LABColorSpace)

        // Convert them all back (should cause all 10 to hit the cache)
        for (c <- colors) c.convertTo(RGBColorSpace)

        // Make a couple of new random colors
        val colors2 = for (i <- 0 until 2) yield Color.RGBColor(math.random, math.random, math.random)

        // Convert them to LAB (this should create 4 mappings, evict 4, and have 2 misses)
        for (c <- colors2) c.convertTo(LABColorSpace)

        //Color.ColorSpaceConversionCache.report()
    }
}
