import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/9/12
 * Time: 12:07 PM
 * To change this template use File | Settings | File Templates.
 */

class SceneObject(val category:String)
{
    var color:DiscreteColorVariable = new DiscreteColorVariable
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
}
