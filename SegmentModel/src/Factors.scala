/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/9/12
 * Time: 1:36 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import la.DenseTensor1

class TargetSaturationFactor(v:DiscreteColorVariable, private val target:Double, private val bandwidth:Double) extends Factor1(v)
{
    def score(v:DiscreteColorVariable#Value) =
    {
        val saturation = v.category.copyIfNeededTo(HSVColorSpace)(1)
        MathUtils.logGaussianKernel(saturation, target, bandwidth)
    }
}

class TargetValueFactor(v:DiscreteColorVariable, private val target:Double, private val bandwidth:Double) extends Factor1(v)
{
    def score(v:DiscreteColorVariable#Value) =
    {
        val value = v.category.copyIfNeededTo(HSVColorSpace)(2)
        MathUtils.logGaussianKernel(value, target, bandwidth)
    }
}

class TargetContrastFactor(v1:DiscreteColorVariable, v2:DiscreteColorVariable, private val target:Double, private val bandwidth:Double) extends Factor2(v1, v2)
{
    def score(val1:DiscreteColorVariable#Value, val2:DiscreteColorVariable#Value) =
    {
        val contrast = Color.contrast(val1.category, val2.category)
        MathUtils.logGaussianKernel(contrast, target, bandwidth)
    }
}

class TargetComplementarityFactor(v1:DiscreteColorVariable, v2:DiscreteColorVariable, private val target:Double, private val bandwidth:Double) extends Factor2(v1,v2)
{
    def score(val1:DiscreteColorVariable#Value, val2:DiscreteColorVariable#Value) =
    {
        val hue1 = val1.category.copyIfNeededTo(HSVColorSpace)(0)
        val hue2 = val2.category.copyIfNeededTo(HSVColorSpace)(0)
        val complementarity = math.abs(hue1 - hue2)
        MathUtils.logGaussianKernel(complementarity, target, bandwidth)
    }
}

// Assumes the histogram was calculated from LAB colors
class InterpolatedHistogramFactor(v:DiscreteColorVariable, private val hist:VectorHistogram) extends Factor1(v)
{
    def score(v:DiscreteColorVariable#Value) =
    {
    }

    private def estimateBandwidth(vec:DenseTensor1) : Double =
    {

    }
}