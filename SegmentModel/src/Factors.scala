/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/9/12
 * Time: 1:36 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

/**
 * Factor that enforces that the contrast between two colors should be similar to the observed contrast between
 * their owner groups
 */
class PairwiseMaintainObservedContrastFactor(v1:DiscreteColorVariable, v2:DiscreteColorVariable) extends Factor2(v1,v2)
{
    private val sigma = 0.2     // I just made this up
    private val targetContrast = Color.contrast(v1.observedColor, v2.observedColor)

    def score(val1:DiscreteColorVariable#Value, val2:DiscreteColorVariable#Value) =
    {
        val contrast = Color.contrast(val1.category, val2.category)
        // This is intended to be a gaussian, but we don't exponentiate it because factorie operates in log space
        -math.abs(contrast - targetContrast) / sigma
    }
}

class TargetSaturationFactor(v:DiscreteColorVariable, private val target:Double, private val bandwidth:Double) extends Factor1(v)
{
    def score(v:DiscreteColorVariable#Value) =
    {
        val saturation = v.category.copyTo(HSVColorSpace)(1)
        MathUtils.gaussianKernel(saturation, target, bandwidth)
    }
}

class TargetValueFactor(v:DiscreteColorVariable, private val target:Double, private val bandwidth:Double) extends Factor1(v)
{
    def score(v:DiscreteColorVariable#Value) =
    {
        val value = v.category.copyTo(HSVColorSpace)(2)
        MathUtils.gaussianKernel(value, target, bandwidth)
    }
}

class TargetContrastFactor(v1:DiscreteColorVariable, v2:DiscreteColorVariable, private val target:Double, private val bandwidth:Double) extends Factor2(v1, v2)
{
    def score(val1:DiscreteColorVariable#Value, val2:DiscreteColorVariable#Value) =
    {
        val contrast = Color.contrast(val1.category, val2.category)
        MathUtils.gaussianKernel(contrast, target, bandwidth)
    }
}