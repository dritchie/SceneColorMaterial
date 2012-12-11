package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/9/12
 * Time: 1:36 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import la.Tensor1

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

// Based on [Wang et al. 2010] "Data-Driven Image Color Theme Enhancement"
class ColorHistogramPriorFactor(v:DiscreteColorVariable, private val hist:ColorHistogram) extends Factor1(v)
{
    // TODO: Consider caching scores to avoid unncessary recomputation (perhaps better done at the Template level?)

    def score(v:DiscreteColorVariable#Value) =
    {
        val c = v.category.copyIfNeededTo(hist.colorspace)
        math.log(hist.evaluateAt(c))
    }
}

class BinaryPriorFactor(v1:DiscreteColorVariable, v2:DiscreteColorVariable, private val hist:VectorHistogram, val func:(Color,Color)=>Tensor1) extends Factor2(v1,v2)
{
  def score(val1:DiscreteColorVariable#Value, val2:DiscreteColorVariable#Value) =
  {
    val c = func(val1.category, val2.category)
    math.log(hist.evaluateAt(c))
  }

}

class UnaryPriorFactor(v1:DiscreteColorVariable, private val hist:VectorHistogram, val func:(Color=>Tensor1)) extends Factor1(v1)
{
  def score(val1:DiscreteColorVariable#Value) =
  {
    val c = func(val1.category)
    math.log(hist.evaluateAt(c))
  }
}