/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/11/12
 * Time: 10:42 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.Tensor1
import collection.mutable.ArrayBuffer
import util.Random

class ColorHistogram(centroids:IndexedSeq[Tensor1], bins:IndexedSeq[Double], metric:MathUtils.DistanceMetric, val colorspace:ColorSpace) extends VectorHistogram(centroids, bins, metric)
{
}

object ColorHistogram
{
    def apply(data:IndexedSeq[Color], numBins:Int)
    {
        assert(data.length > 0, {println("ColorHistogram: cannot construct from 0 data points")})

        val colorspace = data(0).colorSpace

        // Ensure that all colors are in the same color space
        assert(data.foldLeft(true)((sofar, curr) => { sofar && curr.isIn(colorspace) }),
        {println("ColorHistogram: input colors not all in same color space!")})

        VectorHistogram.trainKMeans(for (c <- data) yield c.components, numBins, colorspace.distance)
    }
}

class VectorHistogram(val centroids:IndexedSeq[Tensor1], val bins:IndexedSeq[Double], val metric:MathUtils.DistanceMetric)
{
    assert(bins.length > VectorHistogram.numBandwidthEstimationNeighbors, {println("VectorHistogram: Not enough bins to do bandwidth estimation")})

    private val bandwidths = estimateBandwidths()

    /** Methods **/

    def evaluateAt(point:Tensor1) : Double =
    {
        var sum = 0.0
        for (i <- 0 until bins.length)
        {
            val centroid = centroids(i)
            val freq = bins(i)
            val d2 = metric(point, centroid)
            val sigma = bandwidths(i)
            sum += freq * MathUtils.gaussianDistribution(d2, sigma)
        }
        sum / bins.length
    }

    private def estimateBandwidths() =
    {
        val n = VectorHistogram.numBandwidthEstimationNeighbors
        val invN = 1.0/n
        for (i <- 0 until bins.length) yield
        {
            val otherIndices = (0 until bins.length).filter((index) => index != i)
            var dists = for (j <- otherIndices) yield metric(centroids(i), centroids(j))
            dists = dists.sorted
            var sum = 0.0
            for (j <- 0 until n) sum += dists(j)
            invN*sum
        }
    }
}

object VectorHistogram
{
    private val numBandwidthEstimationNeighbors = 3

    // Construct a vector histogram from data using KMeans
    def trainKMeans(data:Seq[Tensor1], numBins:Int, metric:MathUtils.DistanceMetric) : VectorHistogram =
    {
        assert(data.length > 0, {println("VectorHistogram: cannot construct from 0 data points")})

        val bins = Array.fill[Double](numBins)(0.0)
        val (centroids, assignments) = vectorQuantization(data, numBins, metric)

        // Record bin frequencies
        for (i <- 0 until data.length) bins(assignments(i)) += 1

        // Normalize the histogram
        for (i <- 0 until bins.length)
            bins(i) /= data.length

        new VectorHistogram(centroids, bins, metric)
    }

//    // Construct a vector histogram from data using uniform binning
//    def trainUniform(data:Seq[Tensor1], binsPerDim:IndexedSeq[Int], metric:MathUtils.DistanceMetric) : VectorHistogram =
//    {
//        assert(data.length > 0, {println("VectorHistogram: cannot construct from 0 data points")})
//
//        val numbins = binsPerDim.reduceLeft((a,b) => a * b)
//        val bins = Array.fill[Double](numbins)(0.0)
//        val centroids = Array.fill[Tensor1](numbins)(Tensor1(0.0, 0.0))
//        // TODO: Verify that centroids is full of *different* Tensor1's
//
//        // TODO: Finish this
//    }

    // Returns a tuple of the quantization vectors and the assignments of each sample to its closest quantization vector
    // (Implemenation-wise, this just does kmeans)
    def vectorQuantization(samples:Seq[Tensor1], numSymbols:Int, metric:MathUtils.DistanceMetric) : (IndexedSeq[Tensor1], IndexedSeq[Int]) =
    {
        // Choose the initial quantization vectors randomly from the input samples
        val pool = new ArrayBuffer[Int]()
        pool ++= (0 until samples.length)
        val r = new Random()
        val quantVecs = for (i <- 0 until numSymbols) yield
        {
            val rindex = r.nextInt(pool.length)
            val todrop = pool(rindex)
            pool -= todrop
            samples(todrop).copy
        }

        val assignments = Array.fill[Int](samples.length)(-1)
        val counts = new Array[Int](quantVecs.length)
        var converged = false
        while (!converged)
        {
            // Reset counts
            for (i <- 0 until counts.length) counts(i) = 0

            // Assign points to closest quantization vector
            var changed = false
            for (i <- 0 until samples.length)
            {
                val closestIndex = MathUtils.closestVectorBruteForce(samples(i), quantVecs, metric)
                changed |= (closestIndex != assignments(i))
                assignments(i) = closestIndex
                counts(closestIndex) += 1
            }
            if (!changed) converged = true

            // Update quant vectors to be average of assigned vectors
            if (changed)
            {
                for (i <- 0 until quantVecs.length) quantVecs(i) := 0.0
                for (i <- 0 until samples.length) quantVecs(assignments(i)) += samples(i)
                for (i <- 0 until quantVecs.length) if (counts(i) > 0) quantVecs(i) /= counts(i)
            }
        }

        (quantVecs, assignments)
    }
}
