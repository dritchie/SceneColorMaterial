/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/17/12
 * Time: 3:54 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

object Main
{
    def main(args: Array[String])
    {
        testContrastModel()
    }

    def testColorConversions()
    {
        //Testing color conversions
        val rgb = Color.RGBColor(1, 0, 0)
        val lab = rgb.copyTo(LABColorSpace)

        println("rgb red to lab " + lab.toString())
        println("Converting red from rgb to lab and back " + (LABColorSpace.toRGB _).tupled(LABColorSpace.fromRGB(1, 0, 0)))
        println("Converting red from rgb to hsv and back " + (HSVColorSpace.toRGB _).tupled(HSVColorSpace.fromRGB(1, 0, 0)))
    }

    def testContrastModel()
    {
        val filename = "../SceneViewer/SegmentationOutput/segDescription.txt"
        val segmesh = new SegmentMesh(filename)
        val model = new MaintainObservedContrastModel(segmesh)

        // Define a color palette, use those (and only those) colors as our
        // discrete color domain
        // (These colors come from ashley furniture image 11803U2)
        val palette = new ColorPalette
        palette.colors = Array(Color.RGBColor(63.0/255.0, 40.0/255.0, 22.0/255.0),
                               Color.RGBColor(111.0/255.0, 159.0/255.0, 161.0/255.0),
                               Color.RGBColor(184.0/255.0, 171.0/255.0, 119.0/255.0),
                               Color.RGBColor(243.0/255.0, 236.0/255.0, 218.0/255.0),
                               Color.RGBColor(118.0/255.0, 103.0/255.0, 84.0/255.0))
        DiscreteColorVariable.initDomain(palette.colors)

        // Do inference
        val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
        val optimizer = new SamplingMaximizer(sampler)
        optimizer.maximize(for (group <- segmesh.groups) yield group.color, 100)

        // Output the result
        segmesh.saveColorAssignments("../SceneViewer/SegmentationOutput/colorAssignments.txt")
    }
}
