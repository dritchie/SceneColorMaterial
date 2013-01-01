package colorinference

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


class VectorHistogram(private val metric:MathUtils.DistanceMetric, private val bandwidthScale:Double)
{
    private var centroids:IndexedSeq[Tensor1] = new Array[Tensor1](0)
    private var bins:IndexedSeq[Double] = new Array[Double](0)
    private var bandwidths:IndexedSeq[Double] = new Array[Double](0)

    def train(data:Seq[Tensor1], quantizer:VectorQuantizer)
    {
        assert(data.length > 0, {println("VectorHistogram: cannot train on 0 data points")})

        // Invoke quantizer
        val (_centroids, _assignments) = quantizer(data, metric)

        // Record bin frequencies
        val _bins = Array.fill(_centroids.length)(0.0)
        for (i <- 0 until data.length) _bins(_assignments(i)) += 1

        // Normalize the histogram
        for (i <- 0 until _bins.length)
            _bins(i) /= data.length

        // Set data
        setData(_centroids, _bins)
    }

    def setData(centroids:IndexedSeq[Tensor1], bins:IndexedSeq[Double])
    {
        assert(centroids.length == bins.length, {println("VectorHistogram.setData - centroids, bins have mismatched dimensions")})
        assert(centroids.length > VectorHistogram.numBandwidthEstimationNeighbors, {println("VectorHistogram.setData - Too few bins for bandwidth estimation")})

        this.centroids = centroids
        this.bins = bins
        estimateBandwidths()
    }

    def getCentroids:IndexedSeq[Tensor1] =
    {
      centroids
    }

    def getBins:IndexedSeq[Double] =
    {
      bins
    }

    def getBin(value:Tensor1):Int =
    {
      MathUtils.closestVectorBruteForce(value, centroids, metric)
    }

    def evaluateAt(point:Tensor1) : Double =
    {
        var sum = 0.0
        for (i <- 0 until bins.length)
        {
            val centroid = centroids(i)
            val freq = bins(i)
            val d = metric(point, centroid)
            val sigma = bandwidths(i)
            val gauss = MathUtils.gaussianDistribution(d, sigma)
            val totalterm = freq * gauss
            sum += totalterm
        }
        //sum / bins.length     // This is a linear superposition of PDFs, so it is already a normalized PDF
        sum
    }

    def evaluateBinAt(bin:Int, point:Tensor1) : Double =
    {
        val centroid = centroids(bin)
        val d = metric(point, centroid)
        val sigma = bandwidths(bin)
        val value = MathUtils.gaussianDistribution(d, sigma)
        value
    }

    private def estimateBandwidths()
    {
        val n = VectorHistogram.numBandwidthEstimationNeighbors
        val scale = bandwidthScale*(1.0/n)
        bandwidths = for (i <- 0 until bins.length) yield
        {
            val otherIndices = (0 until bins.length).filter((index) => index != i)
            var dists = for (j <- otherIndices) yield metric(centroids(i), centroids(j))
            dists = dists.sorted
            var sum = 0.0
            for (j <- 0 until n) sum += dists(j)
            scale*sum
        }
    }
}

object VectorHistogram
{
    val numBandwidthEstimationNeighbors = 3

    def apply(data:Seq[Tensor1], metric:MathUtils.DistanceMetric, bandwidthScale:Double, quantizer:VectorQuantizer) : VectorHistogram =
    {
        assert(data.length > 0, {println("VectorHistogram: cannot construct from 0 data points")})

        val hist = new VectorHistogram(metric, bandwidthScale)
        hist.train(data, quantizer)
        hist
    }
}

trait VectorQuantizer
{
    def apply(samples:Seq[Tensor1], metric:MathUtils.DistanceMetric, weights:Seq[Double]=null) : (IndexedSeq[Tensor1], IndexedSeq[Int])

    protected def sanityCheckResult(centroids:IndexedSeq[Tensor1])
    {
        // Make sure there are no NaNs
        for (v <- centroids; i <- 0 until v.length) assert(v(i) == v(i), {println("VectorQuantizer: NaNs in final quantization vectors")})

        // Make sure there are no co-located centroids
        for (i <- 0 until centroids.length-1; j <- i+1 until centroids.length)
            assert((centroids(i) - centroids(j)).twoNormSquared != 0.0, {println("VectorQuantizer: Detected co-located final quantization vectors")})
    }
}

//this doesn't take into account weights
class UniformVectorQuantizer(private val binsPerDim:IndexedSeq[Int]) extends VectorQuantizer
{
    def apply(samples:Seq[Tensor1], metric:MathUtils.DistanceMetric, weights:Seq[Double]=null) : (IndexedSeq[Tensor1], IndexedSeq[Int]) =
    {
        assert(samples.length > 0, {println("UniformVectorQuantizer: cannot operate on 0 data points")})

        // Find min/max value for each dimension
        val minvec = samples.reduceLeft((sofar, curr) => {
            val newsofar = sofar.copy
            for (i <- 0 until curr.length) newsofar(i) = math.min(sofar(i), curr(i))
            newsofar
        })
        val maxvec = samples.reduceLeft((sofar, curr) => {
            val newsofar = sofar.copy
            for (i <- 0 until curr.length) newsofar(i) = math.max(sofar(i), curr(i))
            newsofar
        })

        // Compute discretization along each axis
        val slices = for (i <- 0 until binsPerDim.length) yield
        {
            val min = minvec(i)
            val max = maxvec(i)
            val numbins = binsPerDim(i)
            val binwidth = (max - min) / numbins
            val halfwidth = 0.5*binwidth
            for (i <- 0 until numbins) yield min + i*binwidth + halfwidth
        }

        // Take the 'outer product' of slices to produce the final centroids
        def outerProdFrom(index:Int) : IndexedSeq[IndexedSeq[Double]] =
        {
            if (index == binsPerDim.length-1)
                slices(index).map((sliceval) => IndexedSeq(sliceval))
            else
            {
                val op = outerProdFrom(index+1)
                slices(index).flatMap((sliceval) => op.map((seq) => seq.+:(sliceval)))
            }
        }
        val centroids = outerProdFrom(0).map((seq) => Tensor1(seq:_*))

        // Find closest assignments
        val assignments = new Array[Int](samples.length)
        for (i <- 0 until samples.length)
        {
            assignments(i) = MathUtils.closestVectorBruteForce(samples(i), centroids, metric)
        }

        sanityCheckResult(centroids)

        (centroids, assignments)
    }
}

//takes into account weights, if provided
class KMeansVectorQuantizer(private val numClusters:Int) extends VectorQuantizer
{
    def apply(samples:Seq[Tensor1], metric:MathUtils.DistanceMetric, weights:Seq[Double]=null) : (IndexedSeq[Tensor1], IndexedSeq[Int]) =
    {
        assert(samples.length > 0, {println("KMeansVectorQuantizer: cannot operate on 0 data points")})

        // We can only deal with as many clusters as we have (unique) samples
        // TODO: Make this look at the number of *unique* samples (based on vector values, not pointer equality)

        //check number of unique samples
        val uniqSamples = samples.foldLeft(ArrayBuffer[Tensor1]())((sofar,elem)=>{
          if (sofar.exists(s => (s-elem).twoNorm==0))
            sofar
          else {
            sofar += elem
            sofar
          }
        })

        val actualClusters = math.min(numClusters, uniqSamples.length)

        // Choose the initial quantization vectors randomly from the input samples
        val pool = new ArrayBuffer[Int]()
        pool ++= (0 until uniqSamples.length)
        val r = new Random()
        val quantVecs = for (i <- 0 until actualClusters) yield
        {
            val rindex = r.nextInt(pool.length)
            val todrop = pool(rindex)
            pool -= todrop
            uniqSamples(todrop).copy
        }

        val assignments = Array.fill[Int](samples.length)(-1)
        val counts = new Array[Double](quantVecs.length)
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
                counts(closestIndex) += {if (weights==null) 1.0 else weights(i)}
            }
            if (!changed) converged = true

            // Update quant vectors to be the weighted average of assigned vectors
            if (changed)
            {
                for (i <- 0 until quantVecs.length) quantVecs(i) := 0.0
                for (i <- 0 until samples.length) quantVecs(assignments(i)) += (samples(i) * {if (weights==null) 1.0 else weights(i)} )
                for (i <- 0 until quantVecs.length) if (counts(i) > 0) quantVecs(i) /= counts(i)
            }
        }

        sanityCheckResult(quantVecs)

        (quantVecs, assignments)
    }
}
