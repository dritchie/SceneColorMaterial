/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/9/12
 * Time: 11:51 AM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._


class DiscreteColorVariable extends CategoricalVariable[Color]
{
    // TODO: Constructor
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
