/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 9/20/12
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

class MaterialVariable(val ownerObject:FurnitureObject) extends CategoricalVariable[String]
{
    var observed = false
    def this(ownerObject:FurnitureObject, initialVal:String) = { this(ownerObject); _set(domain.index(initialVal)); observed = true }
    def domain = MaterialVariable.domain
}

object MaterialVariable
{
    var domain : CategoricalDomain[String] = null

    def initDomain(materials:Seq[String])
    {
        domain = new CategoricalDomain(materials)
    }
}
