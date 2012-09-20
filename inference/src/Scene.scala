/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 9/20/12
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

class Scene(var objects: Seq[FurnitureObject])
{
    objects.foreach(obj => obj.scene = this)

    def objectsWithCategory(category:String) = objects.filter(obj => obj.category == category)

    def observedMaterials = objects.map(obj => obj.material).filter(mat => mat.isInstanceOf[ObservedVariable])
    def unobservedMaterials = objects.map(obj => obj.material).filter(mat => !mat.isInstanceOf[ObservedVariable])
    def allMaterials = objects.map(obj => obj.material)

    def print() { objects.foreach(obj => obj.print()) }
}

class FurnitureObject(val category: String, mat:String = null)
{
    var scene:Scene = null
    var material: MaterialVariable = null
    if (mat != null)
        material = new MaterialVariable(this, mat) with ObservedVariable
    else
        material = new MaterialVariable(this)

    def print() { println("FurnitureObject: (Category = %s, Material = %s)".format(category, material.value.category)) }
}
