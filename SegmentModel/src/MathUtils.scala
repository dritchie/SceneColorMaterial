/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/9/12
 * Time: 1:45 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.{DenseTensor1, Tensor1}

object MathUtils
{
    /** Gaussians **/

    def logGaussianKernel(arg:Double, sigma:Double) : Double = -(arg*arg) / (2*sigma*sigma)

    def gaussianKernel(arg:Double, sigma:Double) : Double = math.exp(logGaussianKernel(arg, sigma))

    def logGaussianDistribution(arg:Double, sigma:Double) : Double =
    {
        val coeff = 1.0 / (sigma * math.sqrt(2*math.Pi))
        math.log(coeff) + logGaussianKernel(arg, sigma)
    }

    def gaussianDistribution(arg:Double, sigma:Double) : Double =
    {
        val coeff = 1.0 / (sigma * math.sqrt(2*math.Pi))
        coeff * gaussianKernel(arg, sigma)
    }

    def logGaussianKernel(x:Double, mu:Double, sigma:Double) : Double =
    {
        val xminusmu = x - mu
        logGaussianKernel(xminusmu, sigma)
    }

    def gaussianKernel(x:Double, mu:Double, sigma:Double) : Double = math.exp(logGaussianKernel(x, mu, sigma))

    def logGaussianDistribution(x:Double, mu:Double, sigma:Double) : Double =
    {
        val xminusmu = x - mu
        logGaussianDistribution(xminusmu, sigma)
    }

    def gaussianDistribution(x:Double, mu:Double, sigma:Double) : Double =
    {
        val xminusmu = x - mu
        gaussianDistribution(xminusmu, sigma)
    }


    /** Random numbers **/

    def randBetween(min:Double, max:Double) : Double = min + math.random*max

    def randomVector(mins:Tensor1, maxs:Tensor1) =
    {
        val rvec = mins.copy
        for (i <- 0 until rvec.length) rvec(i) = randBetween(mins(i), maxs(i))
        rvec
    }

    def gaussianRandom() : Double =
    {
        // Box-Muller method
        val minval = Double.MinPositiveValue
        val maxval = 1.0 - minval
        val u = randBetween(minval, maxval)
        val v = randBetween(minval, maxval)
        math.sqrt(-2.0 * math.log(u)) * math.cos(2.0*math.Pi*v)
    }

    def gaussianRandom(mu:Double, sigma:Double) : Double = mu + sigma*gaussianRandom()

    def multinomialRandom(params:IndexedSeq[Double]) : Int =
    {
        val x = math.random
        var probAccum = Double.MinPositiveValue
        for (result <- 0 until params.length)
        {
            probAccum += params(result)
            if (x <= probAccum) return result
        }
        params.length - 1
    }


    /** Polar and spherical coordinates **/

    def polarToRectangular(angle:Double, radius:Double) : DenseTensor1 =
    {
        new DenseTensor1(Array(radius*math.cos(angle), radius*math.sin(angle)))
    }


    /** Distance and nearest neighbors **/

    type DistanceMetric = (Tensor1, Tensor1) => Double

    def euclideanDistance(a:Tensor1, b:Tensor1):Double = (a - b).twoNorm

    // Returns the index of the closest vector
    def closestVectorBruteForce(queryVec:Tensor1, pool:Seq[Tensor1], metric:DistanceMetric) : Int =
    {
        var bestDist = Double.MaxValue
        var bestIndex = -1
        for (index <- 0 until pool.length)
        {
            val dist = metric(queryVec, pool(index))
            if (dist < bestDist)
            {
                bestDist = dist
                bestIndex = index
            }
        }
        bestIndex
    }


    /** Misc **/

    def clamp(value:Double, min:Double, max:Double) : Double =
    {
        if (value < min) min else if (value > max) max else value
    }
}
