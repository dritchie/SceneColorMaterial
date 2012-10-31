/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 1:23 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.DenseTensor1

class Color(c1: Double, c2: Double, c3: Double, private var colorspace:ColorSpace)
{
    val components = new DenseTensor1(3)
    components.update(0, c1)
    components.update(1, c2)
    components.update(2, c3)

    def this(c:Color) = this(c.components(0), c.components(1), c.components(2), c.colorspace)

    override def toString(): String = colorspace + "(" + components(0) + "," + components(1) + "," + components(2) + ")"

    def sameColorSpace(c:Color) = colorspace == c.colorspace

    // Convert to a different color space
    def convertTo(cspace:ColorSpace)
    {
        if (cspace != colorspace)
        {
            val newcomps = (cspace.fromRGB _).tupled(colorspace.toRGB(components(0), components(1), components(2)))
            components.update(0, newcomps._1)
            components.update(1, newcomps._2)
            components.update(2, newcomps._3)
            colorspace = cspace
        }
    }

    // Call this before performing any operation between this and another color
    // to make sure that the colors are in the same color space
    protected def ensureColorSpaceCompatibility(c: Color)
    {
        if (!sameColorSpace(c))
            throw new Error("Attempting operation on two colors from different color spaces!")
    }

    // Operations (scaling, adding, distance computation, etc.) go here
    // (Make sure to call 'ensureColorSpaceCompatibility' before doing any of these)
}

object Color
{
    def RGBColor(c1:Double, c2:Double, c3:Double) = new Color(c1, c2, c3, RGBColorSpace)
    def HSVColor(c1:Double, c2:Double, c3:Double) = new Color(c1, c2, c3, HSVColorSpace)
    def LABColor(c1:Double, c2:Double, c3:Double) = new Color(c1, c2, c3, LABColorSpace)
}


// Color spaces

trait ColorSpace
{
    def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double)
    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double)
}

object RGBColorSpace extends ColorSpace
{
    override def toString(): String = "RGB"
    def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) = (c1, c2, c3)
    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) = (c1, c2, c3)
}


//conversions assume that RGB is in the range 0-255 and HSV has hue in degrees with S and V between 0 and 1
object HSVColorSpace extends ColorSpace
{
    override def toString(): String = "HSV"

    def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
    {
        val hue = c1
        val saturation = c2
        val value = c3 * 255

        val hi: Int = math.floor(hue / 60).toInt % 6
        val f = hue / 60 - math.floor(hue / 60)
        val v = value.toInt
        val p = (value * (1 - saturation)).toInt
        val q = (value * (1 - f * saturation)).toInt
        val t = (value * (1 - (1 - f) * saturation)).toInt

        if (hi == 0)
            return (v, t, p)
        else if (hi == 1)
            return (q, v, p)
        else if (hi == 2)
            return (p, v, t)
        else if (hi == 3)
            return (p, q, v)
        else if (hi == 4)
            return (t, p, v)
        else
            return (v, p, q)
    }

    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
    {
        val r = c1
        val g = c2
        val b = c3
        val max = math.max(r, math.max(g, b))
        val min = math.min(r, math.min(g, b))
        val chroma = (max - min).toDouble

        var huep: Double = 0
        if (chroma == 0)
            huep = 0
        else if (max == r)
            huep = (g - b) / chroma % 6
        else if (max == g)
            huep = (b - r) / chroma + 2
        else
            huep = (r - g) / chroma + 4

        var hue = 60 * huep
        if (hue < 0)
            hue = hue + 360

        val saturation: Double = {
            if (max == 0) 0.0 else 1.0 - (1.0 * min / max)
        }
        val value: Double = max / 255.0

        return (hue, saturation, value)
    }
}


//conversions assume RGB is in the range 0 to 255
object LABColorSpace extends ColorSpace
{
    override def toString(): String = "LAB"

    def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
    {
        val gamma = 2.2
        val e = 216 / 24389.0
        val k = 24389 / 27.0

        val XR = 0.95047
        val YR = 1.0
        val ZR = 1.08883

        val cieL = c1
        val cieA = c2
        val cieB = c3

        val fy = (cieL + 16) / 116.0
        val fx = (cieA / 500.0) + fy
        val fz = fy - cieB / 200.0

        val M = Array(Array(3.2404542, -1.5371385, -0.4985314),
            Array(-0.9692660, 1.8760108, 0.0415560),
            Array(0.0556434, -0.2040259, 1.0572252))
        var xR = math.pow(fx, 3.0)
        var zR = math.pow(fz, 3.0)

        xR = {
            if (xR > e) xR else (116 * fx - 16) / k
        }
        val yR = {
            if (cieL > (k * e)) math.pow((cieL + 16) / 116.0, 3.0) else cieL / k
        }
        zR = {
            if (zR > e) zR else (116 * fz - 16) / k
        }

        val x = xR * XR
        val y = yR * YR
        val z = zR * ZR

        val r = M(0)(0) * x + M(0)(1) * y + M(0)(2) * z
        val g = M(1)(0) * x + M(1)(1) * y + M(1)(2) * z
        val b = M(2)(0) * x + M(2)(1) * y + M(2)(2) * z

        val clamp = (value: Double) => math.min(math.max(value, 0.0), 1.0)

        val red = math.round(255 * (math.pow(clamp(r), 1.0 / gamma)))
        val green = math.round(255 * math.pow(clamp(g), 1.0 / gamma))
        val blue = math.round(255 * math.pow(clamp(b), 1.0 / gamma))

        return (red, green, blue)
    }

    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
    {
        val gamma = 2.2
        val red = math.pow(c1 / 255.0, gamma)
        val green = math.pow(c2 / 255.0, gamma)
        val blue = math.pow(c3 / 255.0, gamma)

        //sRGB to xyz using the D65 illuminant
        //transformation from http://www.brucelindbloom.com
        val M = Array(
            Array(0.4124564, 0.3575761, 0.1804375),
            Array(0.2126729, 0.7151522, 0.0721750),
            Array(0.0193339, 0.1191920, 0.9503041))

        val x = M(0)(0) * red + M(0)(1) * green + M(0)(2) * blue
        val y = M(1)(0) * red + M(1)(1) * green + M(1)(2) * blue
        val z = M(2)(0) * red + M(2)(1) * green + M(2)(2) * blue

        val XR = 0.95047
        val YR = 1.00000
        val ZR = 1.08883

        val e = 216 / 24389.0
        val k = 24389 / 27.0

        val xR = x / XR
        val yR = y / YR
        val zR = z / ZR

        val fx = {
            if (xR > e) math.pow(xR, 1 / 3.0) else (k * xR + 16) / 116.0
        }
        val fy = (if (yR > e) math.pow(yR, 1 / 3.0) else (k * yR + 16) / 116.0)
        val fz = (if (zR > e) math.pow(zR, 1 / 3.0) else (k * zR + 16) / 116.0)

        val cieL = 116 * fy - 16
        val cieA = 500 * (fx - fy)
        val cieB = 200 * (fy - fz)

        return (cieL, cieA, cieB)
    }

}
