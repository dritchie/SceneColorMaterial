import java.awt.MultipleGradientPaint.ColorSpaceType

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/9/12
 * Time: 11:59 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class Color(val c1:Float, val c2:Float, val c3:Float)
{
    // Methods to be provided by a mixed-in color space
    def colorSpace() : String
    def sameColorSpace(color:Color) : Boolean
    def newColor(c1:Float, c2:Float, c3:Float) : Color
    def toRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float)
    def fromRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float)

    // Color space conversion constructor
    def this(c:Color) = this((fromRGB _).tupled(c.toRGB(c.c1, c.c2, c.c3)))

    // Call this before performing any operation between this and another color
    // to make sure that the colors are in the same color space
    def ensureColorSpaceCompatibility(c:Color)
    {
        if (!sameColorSpace(c))
            throw new Error("Attempting operation on two colors from different color spaces!")
    }
}

// Color spaces and associated convenience classes
trait ColorSpace
trait RGBColorSpace extends ColorSpace
{
    def colorSpace() : String = "RGB"
    def sameColorSpace(color:Color) : Boolean = color.isInstanceOf[RGBColorSpace]
    def newColor(c1:Float, c2:Float, c3:Float) : Color = new RGBColor(c1,c2,c3)
    def toRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float) = (c1, c2, c3)
    def fromRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float) = (c1, c2, c3)
}
class RGBColor(c1:Float, c2:Float, c3:Float) extends Color(c1,c2,c3) with RGBColorSpace
trait HSVColorSpace extends ColorSpace
{
    def colorSpace() : String = "HSV"
    def sameColorSpace(color:Color) : Boolean = color.isInstanceOf[HSVColorSpace]
    def newColor(c1:Float, c2:Float, c3:Float) : Color = new Color(c1,c2,c3) with HSVColorSpace
    def toRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float) = throw new Error("Not yet implemented")
    def fromRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float) = throw new Error("Not yet implemented")
}
class HSVColor(c1:Float, c2:Float, c3:Float) extends Color(c1,c2,c3) with HSVColorSpace
trait LABColorSpace extends ColorSpace
{
    def colorSpace() : String = "LAB"
    def sameColorSpace(color:Color) : Boolean = color.isInstanceOf[LABColorSpace]
    def newColor(c1:Float, c2:Float, c3:Float) : Color = new Color(c1,c2,c3) with LABColorSpace
    def toRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float) = throw new Error("Not yet implemented")
    def fromRGB(c1:Float, c2:Float, c3:Float) : (Float, Float, Float) = throw new Error("Not yet implemented")
}
class LABColor(c1:Float, c2:Float, c3:Float) extends Color(c1,c2,c3) with LABColorSpace


class ColorPalette
{
    // For the time being, we assume a five-color palette
    var colors:Array[Color] = new Array(5)
}
