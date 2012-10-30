/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:43 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

class DiscreteColorVariable(val group:SegmentGroup) extends CategoricalVariable[Color]
{
    def this(group:SegmentGroup, c:Color)
    {
        this(group)
        observedColor = c
    }

    var observedColor:Color = null
    def domain = DiscreteColorVariable.domain
}

object DiscreteColorVariable
{
    var domain : CategoricalDomain[Color] = null

    def initDomain(colors:Seq[Color])
    {
        domain = new CategoricalDomain(colors)
    }
}
