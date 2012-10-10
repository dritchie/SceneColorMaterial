/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/9/12
 * Time: 11:51 AM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._


class DiscreteColorVariable(val ownerObject:SceneObject) extends CategoricalVariable[Color]
{
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

///// This next thing (ObjectCategory) isn't *really* a variable, since it's always fixed,
///// but it's nice to take advantage of the structure CategoricalVariable gives us

class ObjectCategory(categoryName:String) extends CategoricalVariable[String](categoryName)
{
    def domain = ObjectCategory.domain
}

object ObjectCategory
{
    var domain : CategoricalDomain[String] = null

    def initDomain(categories:Seq[String])
    {
        domain = new CategoricalDomain(categories)
    }
}
