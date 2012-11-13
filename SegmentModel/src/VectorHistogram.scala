/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/11/12
 * Time: 10:42 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.DenseTensor1

class ColorHistogram(data:Seq[Color], numBins:Int)
{
    assert(data.length > 0, {println("ColorHistogram: cannot construct from 0 data points")})

    val colorspace = data(0).colorSpace
    val bins = new Array[Double](numBins)
    val centroids = new Array[Color](numBins)
    constructorHelper(data, numBins)

    def constructorHelper(data:Seq[Color], numBins:Int)
    {
        // Ensure that all colors are in the same color space
        assert(data.foldLeft(true)((sofar, curr) => { sofar && curr.isIn(colorspace) }),
               {println("ColorHistogram: input colors not all in same color space!")})

        // Convert from a vector histogram
        val hist = new VectorHistogram(for (color <- data) yield color.components, numBins)
        Array.copy(hist.bins, 0, bins, 0, hist.bins.length)
        for (i <- 0 until hist.centroids.length)
        {
            val c = hist.centroids(i)
            centroids(i) = new Color(c(0), c(1), c(2), colorspace)
        }
    }
}

class VectorHistogram(data:Seq[DenseTensor1], numBins:Int)
{
    val bins = Array.fill[Double](numBins)(0.0)
    val centroids = VectorHistogram.vectorQuantization(data, numBins)

    // Assign each data point to the closet centroid; record frequencies
    // (I assume that numBins will be small, so we're just using brute-force
    // closest-centroid finding)
    for (point <- data)
    {
        val bestIndex = MathUtils.closestVectorBruteForce(point, centroids)
        bins(bestIndex) += 1
    }

    // Normalize the histogram
    for (i <- 0 until bins.length)
        bins(i) /= data.length
}

object VectorHistogram
{
    // This just does kmeans
    def vectorQuantization(samples:Seq[DenseTensor1], numSymbols:Int) : Seq[DenseTensor1] =
    {
        // Find the minimum values for vector components
        val mins = samples.reduceLeft(
        (minsofar, curr) =>
        {
            for (i <- 0 until minsofar.length) minsofar(i) = math.min(minsofar(i), curr(i))
            minsofar
        })
        val maxs = samples.reduceLeft(
        (maxsofar, curr) =>
        {
            for (i <- 0 until maxsofar.length) maxsofar(i) = math.max(maxsofar(i), curr(i))
            maxsofar
        })

        // Initialize the quantization vectors randomly
        // TODO: Consider stratified randomization?
        val quantVecs = for (i <- 0 until numSymbols) yield MathUtils.randomVector(mins, maxs)

        val assignments = new Array[Int](samples.length)
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
                val closestIndex = MathUtils.closestVectorBruteForce(samples(i), quantVecs)
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
                for (i <- 0 until quantVecs.length) quantVecs(i) /= counts(i)
            }
        }

        quantVecs
    }
}
