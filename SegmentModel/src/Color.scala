/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 1:23 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.DenseTensor1

abstract class Color(c1:Double, c2:Double, c3:Double)
{
    private val components = new DenseTensor1(3)
    components.update(0, c1)
    components.update(1, c2)
    components.update(2, c3)

    // Methods to be provided by a mixed-in color space
    def colorSpace() : String
    def sameColorSpace(color:Color) : Boolean
    def newColor(c1:Double, c2:Double, c3:Double) : Color
    def toRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double)
    def fromRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double)

    // Color space conversion constructor
    def this(c:Color)
    {
        // Initialize to a dummy value (since we're forced to call this constructor right away)
        this(0, 0, 0)
        // Do the conversion
        val comptuple = (fromRGB _).tupled(c.toRGB(c.components(0), c.components(1), c.components(2)))
        components(0) = comptuple._1
        components(1) = comptuple._2
        components(2) = comptuple._3
    }

    // Call this before performing any operation between this and another color
    // to make sure that the colors are in the same color space
    def ensureColorSpaceCompatibility(c:Color)
    {
        if (!sameColorSpace(c))
            throw new Error("Attempting operation on two colors from different color spaces!")
    }

    // Operations (scaling, adding, distance computation, etc.) go here
    // (Make sure to call 'ensureColorSpaceCompatibility' before doing any of these)
}


// Color spaces and associated convenience classes

trait ColorSpace
trait RGBColorSpace extends ColorSpace
{
    def colorSpace() : String = "RGB"
    def sameColorSpace(color:Color) : Boolean = color.isInstanceOf[RGBColorSpace]
    def newColor(c1:Double, c2:Double, c3:Double) : Color = new RGBColor(c1,c2,c3)
    def toRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double) = (c1, c2, c3)
    def fromRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double) = (c1, c2, c3)
}
class RGBColor(c1:Double, c2:Double, c3:Double) extends Color(c1,c2,c3) with RGBColorSpace

trait HSVColorSpace extends ColorSpace
{
    def colorSpace() : String = "HSV"
    def sameColorSpace(color:Color) : Boolean = color.isInstanceOf[HSVColorSpace]
    def newColor(c1:Double, c2:Double, c3:Double) : Color = new Color(c1,c2,c3) with HSVColorSpace
    def toRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double) =
    {
        throw new Error("Not yet implemented")
    }
    def fromRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double) =
    {
        throw new Error("Not yet implemented")
    }
}
class HSVColor(c1:Double, c2:Double, c3:Double) extends Color(c1,c2,c3) with HSVColorSpace

trait LABColorSpace extends ColorSpace
{
    def colorSpace() : String = "LAB"
    def sameColorSpace(color:Color) : Boolean = color.isInstanceOf[LABColorSpace]
    def newColor(c1:Double, c2:Double, c3:Double) : Color = new Color(c1,c2,c3) with LABColorSpace
    def toRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double) =
    {
        throw new Error("Not yet implemented")
    }
    def fromRGB(c1:Double, c2:Double, c3:Double) : (Double, Double, Double) =
    {
        throw new Error("Not yet implemented")
    }
}
class LABColor(c1:Double, c2:Double, c3:Double) extends Color(c1,c2,c3) with LABColorSpace
