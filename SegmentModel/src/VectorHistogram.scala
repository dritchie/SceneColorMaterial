/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/11/12
 * Time: 10:42 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.Tensor1

class ColorHistogram(centroids:IndexedSeq[Tensor1], bins:IndexedSeq[Double], metric:MathUtils.DistanceMetric, val colorspace:ColorSpace) extends VectorHistogram(centroids, bins, metric)
{
}

object ColorHistogram
{
    def apply(data:Seq[Color], numBins:Int)
    {
        assert(data.length > 0, {println("ColorHistogram: cannot construct from 0 data points")})

        val colorspace = data(0).colorSpace

        // Ensure that all colors are in the same color space
        assert(data.foldLeft(true)((sofar, curr) => { sofar && curr.isIn(colorspace) }),
        {println("ColorHistogram: input colors not all in same color space!")})

        VectorHistogram(for (c <- data) yield c.components, numBins, colorspace.distance)
    }
}

class VectorHistogram(val centroids:IndexedSeq[Tensor1], val bins:IndexedSeq[Double], val metric:MathUtils.DistanceMetric)
{
    assert(bins.length >= 3, {println("VectorHistogram: need at least 3 bins to do bandwidth estimation")})

    /** Methods **/

    def evaluateAt(point:Tensor1) : Double =
    {
        val sigma = estimateBandwidth(point)

        var sum = 0.0
        for (i <- 0 until bins.length)
        {
            val centroid = centroids(i)
            val freq = bins(i)
            val d2 = metric(point, centroid)
            sum += freq * MathUtils.gaussianDistribution(d2, sigma)
        }

        math.log(sum)
    }

    private def estimateBandwidth(point:Tensor1) : Double =
    {
        // Sort bins by their distance to c
        var dists = for (c <- centroids) yield metric(c, point)
        dists = dists.sorted

        // The average distance of the top 3
        0.3333 * (dists(0) + dists(1) + dists(3))
    }
}

object VectorHistogram
{
    // Construct a vector histogram from data.
    // I think this is kind of a hack, but apparently this is a common design pattern in Scala
    //  to get around the 'must call default constructor as the first line of an alternative constructor'
    //  issue.
    def apply(data:Seq[Tensor1], numBins:Int, metric:MathUtils.DistanceMetric)
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

    // Returns a tuple of the quantization vectors and the assignments of each sample to its closest quantization vector
    // (Implemenation-wise, this just does kmeans)
    def vectorQuantization(samples:Seq[Tensor1], numSymbols:Int, metric:MathUtils.DistanceMetric) : (IndexedSeq[Tensor1], IndexedSeq[Int]) =
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
                for (i <- 0 until quantVecs.length) quantVecs(i) /= counts(i)
            }
        }

        (quantVecs, assignments)
    }
}
