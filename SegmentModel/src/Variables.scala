/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:43 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

trait ColorVariable
{
    def observedColor:Color
    def setColor(color:Color)
    def getColor:Color
}

trait ColorVariableGenerator[V <: ColorVariable]
{
    def apply(group:SegmentGroup[V], observedColor:Color = null) : V
}

/*
 * Discrete color variables are used when we know exactly the set of colors that could possibly be used for assignments
 * (i.e. when we have a restricted palette)
 */

class DiscreteColorVariable(val group:SegmentGroup[DiscreteColorVariable], val observedColor:Color = null) extends CategoricalVariable[Color] with ColorVariable
{
    def domain = DiscreteColorVariable.domain
    def setColor(color:Color)
    {
        set(domain.index(color))(null)
    }
    def getColor:Color = value.category
}

object DiscreteColorVariable extends ColorVariableGenerator[DiscreteColorVariable]
{
    var domain : CategoricalDomain[Color] = null

    def initDomain(colors:Seq[Color])
    {
        domain = new CategoricalDomain(colors)
    }

    def apply(group:SegmentGroup[DiscreteColorVariable], observedColor:Color = null) : DiscreteColorVariable = new DiscreteColorVariable(group, observedColor)
}


/*
 * Continuous color variables are used in the more general case where we don't have a predetermined set of colors
 * to draw from and we allow our assignments to range over the entire color space
 */

class ContinuousColorVariable(val group:SegmentGroup[ContinuousColorVariable], val observedColor:Color = null) extends MutableTensorVar[Color] with ColorVariable
{
    def domain = TensorDomain
    def setColor(color:Color)
    {
        set(color)(null)
    }
    def getColor = value
}

object ContinuousColorVariable extends ColorVariableGenerator[ContinuousColorVariable]
{
    def apply(group:SegmentGroup[ContinuousColorVariable], observedColor:Color = null) : ContinuousColorVariable = new ContinuousColorVariable(group, observedColor)
}
