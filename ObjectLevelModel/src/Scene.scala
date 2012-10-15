import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/9/12
 * Time: 12:07 PM
 * To change this template use File | Settings | File Templates.
 */

class SceneObject(categoryName:String)
{
    val category:ObjectCategory = new ObjectCategory(categoryName)
    var color:DiscreteColorVariable = new DiscreteColorVariable(this)
    var scene:Scene = null
}

class Scene
{
    private var objects:ArrayBuffer[SceneObject] = new ArrayBuffer[SceneObject]
    var colorPalette:ColorPalette = null

    def addObject(obj:SceneObject)
    {
        obj.scene = this
        objects += obj
    }

    def removeObject(obj:SceneObject)
    {
        obj.scene = null
        objects -= obj
    }

    def objectsOfCategory(catIndex:Int) =
    {
        objects.filter(obj => obj.category.intValue == catIndex)
    }

    def objectsOfCategory(catName:String) =
    {
        objects.filter(obj => obj.category.categoryValue == catName)
    }
}
