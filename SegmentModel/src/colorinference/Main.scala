package colorinference

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
import collection.mutable.ArrayBuffer

object Main
{
    def main(args: Array[String])
    {
        //testContrastModel()
        //testVectorHistogram()
        testHistogramRegression()
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
        class MaintainObservedContrastModel(segmesh:SegmentMesh) extends ItemizedModel
        {
            // For each group, add a factor for each adjacent group
            // (Deduplicate by only adding the (low, high) pair)
            for (group1 <- segmesh.groups)
            {
                for (group2 <- group1.adjacencies)
                {
                    if (group1.index < group2.index)
                        this += new PairwiseMaintainObservedContrastFactor(group1.color.asInstanceOf[DiscreteColorVariable],
                                                                           group2.color.asInstanceOf[DiscreteColorVariable])
                }
            }
        }

        val filename = "../SceneToolbox/Output/segDescription.txt"
        val segmesh = new SegmentMesh(DiscreteColorVariable, filename)
        val model = new MaintainObservedContrastModel(segmesh)

        // Define a color palette, use those (and only those) colors as our
        // discrete color domain
        // (These colors come from ashley furniture image 11803U2)
        val palette = new ColorPalette
        palette ++= Array(Color.RGBColor(63.0/255.0, 40.0/255.0, 22.0/255.0),
                               Color.RGBColor(111.0/255.0, 159.0/255.0, 161.0/255.0),
                               Color.RGBColor(184.0/255.0, 171.0/255.0, 119.0/255.0),
                               Color.RGBColor(243.0/255.0, 236.0/255.0, 218.0/255.0),
                               Color.RGBColor(118.0/255.0, 103.0/255.0, 84.0/255.0))
        DiscreteColorVariable.initDomain(palette)

        // Do inference
        val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
        val optimizer = new SamplingMaximizer(sampler)
        optimizer.maximize(for (group <- segmesh.groups) yield group.color.asInstanceOf[DiscreteColorVariable], 100)

        // Output the result
        segmesh.saveColorAssignments("../SceneToolbox/Output/colorAssignments.txt")
    }

    type DensityMap = Array[Array[Double]]

    // Assumes that each gaussian in the mixture is contained in (0,1) along each dimension
    // Just isotropic gaussians, to make things easy
    def gmmToDensityMap(weights:Array[Double], means:Array[DenseTensor1], stddevs:Array[Double], resolution:Int) : DensityMap =
    {
        val densityMap = Array.fill(resolution, resolution)(0.0)
        for (y <- 0 until resolution; x <- 0 until resolution)
        {
            val fy = y.toDouble / resolution
            val fx = x.toDouble / resolution
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
        for (y <- 0 until resolution; x <- 0 until resolution) max = math.max(max, densityMap(x)(y))
        for (y <- 0 until resolution; x <- 0 until resolution) densityMap(x)(y) /= max
        densityMap
    }

    def histogramToDensityMap(hist:VectorHistogram, resolution:Int) : DensityMap =
    {
        val densityMap = Array.fill(resolution, resolution)(0.0)
        for (y <- 0 until resolution; x <- 0 until resolution)
        {
            val fy = y.toDouble / resolution
            val fx = x.toDouble / resolution
            val d = hist.evaluateAt(Tensor1(fx, fy))
            assert(d == d, {"Histogram evaluated to NaN"})
            densityMap(x)(y) = d
        }
        // Normalize density map
        var max = 0.0
        for (y <- 0 until resolution; x <- 0 until resolution) max = math.max(max, densityMap(x)(y))
        for (y <- 0 until resolution; x <- 0 until resolution) densityMap(x)(y) /= max
        densityMap
    }

    // Saves to PNG format
    def saveDensityMapToImage(densityMap:DensityMap, imfilename:String)
    {
        val imdim = densityMap.length
        val img = new BufferedImage(imdim, imdim, BufferedImage.TYPE_INT_ARGB)
        for (y <- 0 until imdim; x <- 0 until imdim)
        {
            val density = densityMap(x)(y).toFloat
            val color = new awt.Color(density, density, density)
            img.setRGB(x, y, color.getRGB)
        }
        ImageIO.write(img, "png", new File(imfilename))
    }

    // Just isotropic gaussians, to make things easy
    def sampleFromGMM(weights:Array[Double], means:Array[DenseTensor1], stddevs:Array[Double], numsamples:Int) : IndexedSeq[Tensor1] =
    {
        for (i <- 0 until numsamples) yield
        {
            val which = MathUtils.multinomialRandom(weights)
            val radius = stddevs(which)*MathUtils.gaussianRandom()
            val angle = MathUtils.randBetween(0.0, 2.0*math.Pi)
            (means(which) + MathUtils.polarToRectangular(angle, radius)).asInstanceOf[Tensor1]
        }
    }

    def testHistogramRegression()
    {
        /* Have features drawn from two distinct classes (two gaussians)
         * For each feature class, generate a distinct target distribution (using two gaussians?)
         * Try to regress histograms for these distributions based on the features
         */

        val numClasses = 2

        // Feature classes
        val feature_means = Array(Tensor1(0.1, 0.1), Tensor1(5.9, 5.9))
        val feature_stddevs = Array(0.1, 0.1)

        // Target distributions
        val target_weights = Array(
            Array(0.5, 0.5),    // Class 1
            Array(0.5, 0.5)     // Class 2
        )
        val target_means = Array(
            Array(Tensor1(0.5, 0.25), Tensor1(0.5, 0.75)),    // Class 1
            Array(Tensor1(0.25, 0.5), Tensor1(0.75, 0.5))     // Class 2
        )
        val target_stddevs  = Array(
            Array(0.1, 0.1),        // Class 1
            Array(0.1, 0.1)         // Class 2
        )

        // Generate density maps for ground truth distributions for each feature class;
        // save these to images
        val imdim = 500
        for (i <- 0 until numClasses)
        {
            println("Generating ground truth density for class " + i + "...")
            val densityMap = gmmToDensityMap(target_weights(i), target_means(i), target_stddevs(i), imdim)
            saveDensityMapToImage(densityMap, "trueDensity_class" + i + ".png")
        }

        // Sample a bunch from each feature class, associating a sampled target value with each sampled feature vector
        //val numSamplesPerClass = 20000
        val numSamplesPerClass = 2000
        val samples = new ArrayBuffer[HistogramRegressor.RegressionExample]
        println("Generating training samples...")
        for (i <- 0 until numClasses)
        {
            val featureVecs = sampleFromGMM(Array(1.0), Array(feature_means(i)), Array(feature_stddevs(i)), numSamplesPerClass)
            val targetVecs = sampleFromGMM(target_weights(i), target_means(i), target_stddevs(i), numSamplesPerClass)
            for (j <- 0 until numSamplesPerClass)
                samples += HistogramRegressor.RegressionExample(targetVecs(j), featureVecs(j))
        }

        // Train a HistogramRegressor
        println("Training HistogramRegressor...")
        val hr = HistogramRegressor.LogisticRegression(MathUtils.euclideanDistance, 1.0, WekaMultiClassHistogramRegressor)
        hr.train(samples, new KMeansVectorQuantizer(20))

        // Predict histograms for the means of each feature class, convert to densitymaps and save images
        for (i <- 0 until numClasses)
        {
            println("Predicting density for features drawn from class " + i + "...")
            val hist = hr.predictHistogram(feature_means(i))
            val densityMap = histogramToDensityMap(hist, imdim)
            saveDensityMapToImage(densityMap, "predictedDensity_class" + i + ".png")
        }
    }

    def testVectorHistogram()
    {
        /** Lay down some 2D gaussians, then use VectorHistogram to reconstruct **/

        // 3 different isotropic Gaussians
        val weights = Array(1.0/3.0, 1.0/3.0, 1.0/3.0)
        val means = Array(Tensor1(0.25, 0.25),
                          Tensor1(0.5, 0.75),
                          Tensor1(0.75, 0.4))
        val stddevs = Array(0.1, 0.1, 0.1)

        // Rasterize to density map
        println("Generating ground truth density map...")
        val imdim = 500
        var densityMap = gmmToDensityMap(weights, means, stddevs, imdim)

        // Convert density map to image; save to disk
        saveDensityMapToImage(densityMap, "trueDensity.png")

        // Sample from this distribution a bunch, then build VectorHistogram from samples
        println("Sampling from ground truth distribution...")
        val numsamples = 50000
        val samples = sampleFromGMM(weights, means, stddevs, numsamples)
        println("Training vector histogram from samples...")
        val hist = VectorHistogram(samples, MathUtils.euclideanDistance, 1.0, new KMeansVectorQuantizer(20))
        //val hist = VectorHistogram(samples, MathUtils.euclideanDistance, new UniformVectorQuantizer(Array(10, 10)))

        // Convert to density map
        println("Generating estimated density map...")
        densityMap = histogramToDensityMap(hist, imdim)

        // Convert density map to image; save to disk
        saveDensityMapToImage(densityMap, "estimatedDensity.png")

        println("DONE")
    }
}
