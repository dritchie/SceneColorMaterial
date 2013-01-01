package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:43 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

trait Copyable[T]
{
    def copy:T
}

trait ColorVariable extends Variable with Copyable[ColorVariable]
{
    // Set this to true in order to condition on this variable as observed
    var fixed = false

    def group:SegmentGroup
    def observedColor:Color
    def setColor(color:Color)
    def getColor:Color
}

trait ColorVariableGenerator
{
    def apply(group:SegmentGroup, observedColor:Color = null) : ColorVariable
}

/*
 * Discrete color variables are used when we know exactly the set of colors that could possibly be used for assignments
 * (i.e. when we have a restricted palette)
 */

class DiscreteColorVariable(val group:SegmentGroup, val observedColor:Color = null) extends CategoricalVariable[Color]
    with ColorVariable with LabeledVar
{
    def domain = DiscreteColorVariable.domain
    def setColor(color:Color)
    {
        set(domain.index(color))(null)
    }
    def getColor:Color = value.category

    def copy:DiscreteColorVariable = new DiscreteColorVariable(this.group, this.observedColor)

    def targetValue = domain.value(observedColor)//observedColor.asInstanceOf[DiscreteColorVariable#Value]
    override def valueIsTarget = value.category.distance(observedColor) == 0
}

object DiscreteColorVariable extends ColorVariableGenerator
{
    var domain : CategoricalDomain[Color] = null

    def initDomain(colors:Seq[Color])
    {
        domain = new CategoricalDomain(colors)
    }

    def apply(group:SegmentGroup, observedColor:Color = null) : DiscreteColorVariable = new DiscreteColorVariable(group, observedColor)
}


/*
 * Continuous color variables are used in the more general case where we don't have a predetermined set of colors
 * to draw from and we allow our assignments to range over the entire color space
 */

class ContinuousColorVariable(val group:SegmentGroup, val observedColor:Color = null) extends MutableTensorVar[Color]
    with ColorVariable
{
    // Initialize value to random
    set(Color.RGBColor(math.random, math.random, math.random))(null)

    def domain = TensorDomain
    def setColor(color:Color)
    {
        set(color)(null)
    }
    def getColor = value

    def copy:ContinuousColorVariable = new ContinuousColorVariable(this.group, this.observedColor)
}

object ContinuousColorVariable extends ColorVariableGenerator
{
    def apply(group:SegmentGroup, observedColor:Color = null) : ContinuousColorVariable = new ContinuousColorVariable(group, observedColor)
}
