/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/9/12
 * Time: 1:45 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.Tensor1

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

    def randBetween(min:Double, max:Double) = min + math.random*max

    def randomVector(mins:Tensor1, maxs:Tensor1) =
    {
        val rvec = mins.copy
        for (i <- 0 until rvec.length) rvec(i) = randBetween(mins(i), maxs(i))
        rvec
    }


    /** Misc **/

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
}
