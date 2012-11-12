/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/11/12
 * Time: 10:42 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.DenseTensor1
import collection.mutable.ArrayBuffer

class VectorHistogram(data:Seq[DenseTensor1], numBins:Int)
{
    val bins = new Array[Double](numBins)
    val centroids = VectorHistogram.vectorQuantization(data, numBins)

    // Assign each data point to the closet centroid; record frequencies
    // (I assume that numBins will be small, so we're just using brute-force
    // closest-centroid finding)
    val centroidIndices = 0 until centroids.length
    for (point <- data)
    {
        val closest = centroidIndices.reduceLeft[(Int, Double)](
        (bestSoFar, currIndex) =>
        {
            val dist = (point - centroids(currIndex)).twoNormSquared
            if (dist < bestSoFar._2)
                (currIndex, dist)
            else
                bestSoFar
        })
        bins(closest._1) += 1
    }

    // Normalize the histogram
    for (i <- 0 until bins.length)
        bins(i) /= data.length
}

object VectorHistogram
{
    def vectorQuantization(samples:Seq[DenseTensor1], numGroups:Int) : Seq[DenseTensor1] =
    {
        // TODO: Fill in
        new ArrayBuffer[DenseTensor1](numGroups)
    }
}
