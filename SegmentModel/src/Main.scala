/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/17/12
 * Time: 3:54 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import cc.factorie.la._
import java.awt
import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object Main
{
    def main(args: Array[String])
    {
        //testContrastModel()
        testVectorHistogram()
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
        val filename = "../SceneToolbox/Output/segDescription.txt"
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
        segmesh.saveColorAssignments("../SceneToolbox/Output/colorAssignments.txt")
    }

    def testVectorHistogram()
    {
        /** Lay down some 2D gaussians, then use VectorHistogram to reconstruct **/

        // 3 different isotropic Gaussians
        val weights = Array(1.0/3.0, 1.0/3.0, 1.0/3.0)
        val means = Array(new DenseTensor1(Array(0.25, 0.25)),
                          new DenseTensor1(Array(0.5, 0.75)),
                          new DenseTensor1(Array(0.75, 0.4)))
        val stddevs = Array(0.1, 0.1, 0.1)

        // Rasterize to density map
        println("Generating ground truth density map...")
        val imdim = 500
        var densityMap = Array.fill(imdim, imdim)(0.0)
        for (y <- 0 until imdim; x <- 0 until imdim)
        {
            val fy = y.toDouble / imdim
            val fx = x.toDouble / imdim
            var sum = 0.0
            for (i <- 0 until weights.length)
            {
                val arg = MathUtils.euclideanDistance(Tensor1(fx, fy), means(i))
                sum += weights(i)*MathUtils.gaussianDistribution(arg, stddevs(i))
            }
            densityMap(x)(y) = sum
        }
        // Normalize density map
        var max = 0.0
        for (y <- 0 until imdim; x <- 0 until imdim) max = math.max(max, densityMap(x)(y))
        for (y <- 0 until imdim; x <- 0 until imdim) densityMap(x)(y) /= max

        // Convert density map to image; save to disk
        var img = new BufferedImage(imdim, imdim, BufferedImage.TYPE_INT_ARGB)
        for (y <- 0 until imdim; x <- 0 until imdim)
        {
            val density = densityMap(x)(y).toFloat
            val color = new awt.Color(density, density, density)
            img.setRGB(x, y, color.getRGB)
        }
        ImageIO.write(img, "png", new File("trueDensity.png"))

        // Sample from this distribution a bunch, then build VectorHistogram from samples
        println("Sampling from ground truth distribution...")
        val numsamples = 50000
        val samples = for (i <- 0 until numsamples) yield
        {
            val which = MathUtils.multinomialRandom(weights)
            val radius = stddevs(which)*MathUtils.gaussianRandom()
            val angle = MathUtils.randBetween(0.0, 2.0*math.Pi)
            (means(which) + MathUtils.polarToRectangular(angle, radius)).asInstanceOf[Tensor1]
        }
        println("Training vector histogram from samples...")
        val hist = VectorHistogram.trainKMeans(samples, 20, MathUtils.euclideanDistance)

        // Convert to density map
        println("Generating estimated density map...")
        densityMap = Array.fill(imdim, imdim)(0.0)
        for (y <- 0 until imdim; x <- 0 until imdim)
        {
            val fy = y.toDouble / imdim
            val fx = x.toDouble / imdim
            val d = hist.evaluateAt(Tensor1(fx, fy))
            assert(d == d, {"Histogram evaluated to NaN"})
            densityMap(x)(y) = d
        }
        // Normalize density map
        max = 0.0
        for (y <- 0 until imdim; x <- 0 until imdim) max = math.max(max, densityMap(x)(y))
        for (y <- 0 until imdim; x <- 0 until imdim) densityMap(x)(y) /= max

        // Convert density map to image; save to disk
        img = new BufferedImage(imdim, imdim, BufferedImage.TYPE_INT_ARGB)
        for (y <- 0 until imdim; x <- 0 until imdim)
        {
            val density = densityMap(x)(y).toFloat
            val color = new awt.Color(density, density, density)
            img.setRGB(x, y, color.getRGB)
        }
        ImageIO.write(img, "png", new File("estimatedDensity.png"))

        println("DONE")
    }
}
