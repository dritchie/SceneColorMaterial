/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:39 PM
 * To change this template use File | Settings | File Templates.
 */

class ColorPalette
{
    def this(filename:String)
    {
        this()
        // TODO: Make this load colors from some file (possibly an Adobe .ase file?)
    }

  //get the palette of the segment mesh
    def this(segmesh:SegmentMesh)
    {
      this()
      colors = {for (group<-segmesh.groups if (group.color.observedColor!=null))
                    yield group.color.observedColor}.toArray
    }

    var colors : Array[Color] = null
}
