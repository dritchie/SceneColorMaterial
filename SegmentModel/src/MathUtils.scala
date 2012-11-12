/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/9/12
 * Time: 1:45 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.DenseTensor1

object MathUtils
{
    def logGaussianKernel(x:Double, mu:Double, sigma:Double) =
    {
        val xminusmu = x - mu
        (-xminusmu*xminusmu) / (2*sigma*sigma)
    }

    def logGaussianDistribution(x:Double, mu:Double, sigma:Double) =
    {
        val coeff = 1.0 / (sigma * math.sqrt(2*math.Pi))
        coeff * logGaussianKernel(x, mu, sigma)
    }

    def randBetween(min:Double, max:Double) = min + math.random*max

    def randomVector(mins:DenseTensor1, maxs:DenseTensor1) =
    {
        val rvec = new DenseTensor1(mins.length)
        for (i <- 0 until rvec.length) rvec(i) = randBetween(mins(i), maxs(i))
        rvec
    }

    // Returns the index of the closest vector
    def closestVectorBruteForce(queryVec:DenseTensor1, pool:Seq[DenseTensor1]) : Int =
    {
        var bestDist = Double.MaxValue
        var bestIndex = -1
        for (index <- 0 until pool.length)
        {
            val dist = (queryVec - pool(index)).twoNormSquared
            if (dist < bestDist)
            {
                bestDist = dist
                bestIndex = index
            }
        }
        bestIndex
    }
}
