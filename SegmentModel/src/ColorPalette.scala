/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:39 PM
 * To change this template use File | Settings | File Templates.
 */

import collection.mutable.ArrayBuffer

class ColorPalette extends ArrayBuffer[Color]

object ColorPalette
{
    def apply(filename:String) =
    {
        // TODO: Make this load colors from some file (possibly an Adobe .ase file?)
        new ColorPalette
    }

    def apply(segmesh:SegmentMesh) =
    {
        val colors = {for (group<-segmesh.groups if (group.color.observedColor!=null))
            yield group.color.observedColor}.toArray
        val palette = new ColorPalette
        palette ++= colors
        palette
    }
}

