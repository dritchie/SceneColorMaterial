package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 1:23 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.Tensor1
import cc.factorie.la.DenseTensor1

class Color(c1: Double, c2: Double, c3: Double, private var colorspace:ColorSpace) extends DenseTensor1(3)
{
    update(0, c1)
    update(1, c2)
    update(2, c3)

    def this(comps:Tensor1, cspace:ColorSpace) = this(comps(0), comps(1), comps(2), cspace)
    def this(c:Color) = this(c, c.colorspace)
    def this(cspace:ColorSpace) = this(0.0, 0.0, 0.0, cspace)

    override def toString : String = colorspace + "(" + this(0) + "," + this(1) + "," + this(2) + ")"
    def componentString : String = "" + this(0) + " " + this(1) + " " + this(2)

    def colorSpace = colorspace
    def isIn(cspace:ColorSpace) = colorspace == cspace
    def sameColorSpace(c:Color) = colorspace == c.colorspace

    // Convert to a different color space
    def convertTo(cspace:ColorSpace)
    {
        if (cspace != colorspace)
        {
            val newcomps = (cspace.fromRGB _).tupled(colorspace.toRGB(this(0), this(1), this(2)))
            update(0, newcomps._1)
            update(1, newcomps._2)
            update(2, newcomps._3)
            colorspace = cspace
        }
    }

    // Create a copy in a different color space
    def copyTo(cspace:ColorSpace) : Color =
    {
        val c = new Color(this)
        c.convertTo(cspace)
        c
    }

    // Convert to a different colorspace, but if the color is already in that
    // colorspace, just return a reference to the color unchanged
    def copyIfNeededTo(cspace:ColorSpace) : Color =
    {
        if (isIn(cspace))
            this
        else
            copyTo(cspace)
    }

    def luminance : Double =
    {
        val c = copyIfNeededTo(RGBColorSpace)
        0.212*c(0) + 0.7152*c(1) + 0.0722*c(2)
    }

    // See http://en.wikipedia.org/wiki/Colorfulness
    def colorfulness : Double =
    {
        val c = copyIfNeededTo(LABColorSpace)
        val chromaSq = c(1)*c(1) + c(2)*c(2)
        val lightness = c(0)

        // Colors with zero lightness require special case
        if (lightness == 0) 0

        // This is one estimate of colorfulness
        // math.sqrt(chromaSq) / lightness

        // This one is supposedly a little bit more in line with
        // human perception (percentage of total color stimulus that
        // is due to chroma)
        math.sqrt(chromaSq) / math.sqrt(chromaSq + lightness*lightness)
    }

    def distance(color:Color): Double =
    {
        val c = color.copyIfNeededTo(this.colorspace)
        colorspace.distance(this, c)
    }
}

object Color
{
    def RGBColor(c1:Double, c2:Double, c3:Double) = new Color(c1, c2, c3, RGBColorSpace)
    def HSVColor(c1:Double, c2:Double, c3:Double) = new Color(c1, c2, c3, HSVColorSpace)
    def LABColor(c1:Double, c2:Double, c3:Double) = new Color(c1, c2, c3, LABColorSpace)

    // For the time being, this is just Delta E.
    // TODO: Replace this with something more state-of-the-art e.g. CIEDE2000?
    // TODO: Or, threshold distance once it gets very big (as the measure is no longer meaningful for huge differences)?
    // See http://en.wikipedia.org/wiki/Color_difference
    def perceptualDifference(col1:Color, col2:Color) : Double =
    {
        val c1 = col1.copyIfNeededTo(LABColorSpace)
        val c2 = col2.copyIfNeededTo(LABColorSpace)
        (c1 - c2).twoNorm
    }

    // More perceptually accurate than hue angle in HSV space
    def chromaDifference(col1:Color, col2:Color) : Double =
    {
        val c1 = col1.copyIfNeededTo(LABColorSpace)
        val c2 = col2.copyIfNeededTo(LABColorSpace)

        // Percentage of total difference that is due to chroma
        val ldiff = c2(0) - c1(0)
        val adiff = c2(1) - c1(1)
        val bdiff = c2(2) - c1(2)
        val chromaDiffSq = adiff*adiff + bdiff*bdiff
        chromaDiffSq / (chromaDiffSq + ldiff*ldiff)
    }

    // A more perceptually-relevant contrast measure
    // (Note that this does not take into account the Helmholtz-Kohlrausch effect)
    // http://en.wikipedia.org/wiki/Helmholtz%E2%80%93Kohlrausch_effect
    def relativeLightness(col1:Color, col2:Color) : Double =
    {
        val c1 = col1.copyIfNeededTo(LABColorSpace)
        val c2 = col2.copyIfNeededTo(LABColorSpace)
        math.abs(c1(0) - c2(0))
    }

    // Better than relative saturation in HSV space
    def relativeColorfulness(col1:Color, col2:Color) : Double =
    {
        math.abs(col1.colorfulness - col2.colorfulness)
    }

    // Super simple contrast measure using luminance difference / average luminance
    def luminanceContrast(col1:Color, col2:Color) : Double =
    {
        val l1 = col1.luminance
        val l2 = col2.luminance
        //math.abs(l1 - l2) / (0.5 * (l1 + l2) + 1e-10)

        //normalize by max luminance diff, which is 1
        math.abs(l1-l2)
    }

    def hueAngle(col1:Color, col2:Color) : Double =
    {
        val hue1 = col1.copyIfNeededTo(HSVColorSpace)(0)
        val hue2 = col2.copyIfNeededTo(HSVColorSpace)(0)
        val hueabsdiff = math.abs(hue1 - hue2)
        math.min(hueabsdiff, 360 - hueabsdiff)
    }

    def relativeSaturation(col1:Color, col2:Color) : Double =
    {
        val sat1 = col1.copyIfNeededTo(HSVColorSpace)(1)
        val sat2 = col2.copyIfNeededTo(HSVColorSpace)(1)
       // math.abs(sat1 - sat2) / (0.5 * (sat1 + sat2) + 1e-10)

        //normalize by max posible saturation diff, which is 1
        math.abs(sat1-sat2)
    }

}


// Color spaces

trait ColorSpace
{
    def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double)
    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double)

    // Default is Euclidean distance, but other color spaces (like HSV) might want something else
    def distance:MathUtils.DistanceMetric = MathUtils.euclideanDistance
}

object RGBColorSpace extends ColorSpace
{
    override def toString: String = "RGB"
    def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) = (c1, c2, c3)
    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) = (c1, c2, c3)
}


//conversions assume that RGB is in the range 0-1 and HSV has hue in degrees with S and V between 0 and 1
object HSVColorSpace extends ColorSpace
{
    override def toString: String = "HSV"

    def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
    {
        val hue = c1
        val saturation = c2
        val value = c3

        val hi: Int = math.floor(hue / 60).toInt % 6
        val f = hue / 60 - math.floor(hue / 60)
        val v = value
        val p = (value * (1 - saturation))
        val q = (value * (1 - f * saturation))
        val t = (value * (1 - (1 - f) * saturation))

        if (hi == 0)
            (v, t, p)
        else if (hi == 1)
            (q, v, p)
        else if (hi == 2)
            (p, v, t)
        else if (hi == 3)
            (p, q, v)
        else if (hi == 4)
            (t, p, v)
        else
            (v, p, q)
    }

    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
    {
        val r = 255*c1
        val g = 255*c2
        val b = 255*c3
        val max = math.max(r, math.max(g, b))
        val min = math.min(r, math.min(g, b))
        val chroma = (max - min)

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

        (hue, saturation, value)
    }
}


//conversions assume RGB is in the range 0 to 1
object LABColorSpace extends ColorSpace
{
    override def toString: String = "LAB"

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

        val red = math.pow(clamp(r), 1.0 / gamma)
        val green = math.pow(clamp(g), 1.0 / gamma)
        val blue = math.pow(clamp(b), 1.0 / gamma)

        (red, green, blue)
    }

    def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
    {
        val gamma = 2.2
        val red = math.pow(c1, gamma)
        val green = math.pow(c2, gamma)
        val blue = math.pow(c3, gamma)

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

        (cieL, cieA, cieB)
    }

}
