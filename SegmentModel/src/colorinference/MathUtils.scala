package colorinference

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
    /** Constants **/
    val epsilon = 0.00001

    /** Gaussians and related distributions **/

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

    def gaussianDistributionIsotropic(x:Tensor1, mu:Tensor1, sigma:Double) : Double =
    {
        // This is correct and (probably) more efficient than the commented-out thing below
        val diff = (x-mu).twoNormSquared
        val coeff = 1.0 / (math.pow(sigma, mu.length) * math.pow(2*math.Pi, mu.length/2))
        coeff * math.exp(-diff/(2*sigma*sigma))

//        var prod = 1.0
//        for (i <- 0 until x.length)
//            prod *= gaussianDistribution(x(i), mu(i), sigma)
//        prod
    }

    // The following two functions cribbed from Scalala
    // https://github.com/scalala/Scalala/blob/master/src/main/scala/scalala/library/Numerics.scala
    private val erf_a = 0.147
    def erf(x: Double) =
    {
        val x2 = x*x
        val inner = math.exp(-1*x2*(4/math.Pi + erf_a*x2) /
            (1.0 + erf_a*x2))
        math.signum(x)*math.sqrt(1.0 - inner)
    }
    def erfi(x: Double) =
    {
        val x2 = x*x
        val i1 = 2.0/(math.Pi*erf_a) + math.log(1-x2)/2.0
        val i2 = math.log(1-x2)/erf_a
        math.signum(x) * math.sqrt( math.sqrt(i1*i1 - i2) - i1 )
    }

    def cumulativeNormalDistribution(x:Double) : Double =
    {
        0.5 * (1 + erf(x / math.sqrt(2.0)))
    }

    def inverseCumulativeNormalDistribution(p:Double) : Double =
    {
        math.sqrt(2.0) * erfi(2*p - 1)
    }

    // http://en.wikipedia.org/wiki/Truncated_normal_distribution
    def gaussianDistributionTruncated(x:Double, mu:Double, sigma:Double, lo:Double, hi:Double) : Double =
    {
        if (x >= lo && x <= hi)
        {
            val numer = logGaussianDistribution(x, mu, sigma)
            val denom = cumulativeNormalDistribution((hi-mu)/sigma) - cumulativeNormalDistribution((lo-mu)/sigma)
            numer / denom
        }
        else 0.0
    }

    def gaussianDistributionIsotropicTruncated(x:Tensor1, mu:Tensor1, sigma:Double, los:Tensor1, his:Tensor1) : Double =
    {
        var prod = 1.0
        for (i <- 0 until x.length)
        {
            if (x(i) < los(i) || x(i) > his(i)) return 0.0
            prod *= gaussianDistributionTruncated(x(i), mu(i), sigma, los(i), his(i))
        }
        prod
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

    // http://en.wikipedia.org/wiki/Truncated_normal_distribution
    // TODO: Try the rejection approach of http://link.springer.com/article/10.1007%2FBF00143942
    def gaussianRandomTruncated(lo:Double, hi:Double) : Double =
    {
        inverseCumulativeNormalDistribution(
            cumulativeNormalDistribution(lo) +
                math.random * (cumulativeNormalDistribution(hi) -cumulativeNormalDistribution(lo)))
    }

    def gaussianRandomTruncated(mu:Double, sigma:Double, lo:Double, hi:Double) : Double =
    {
        val rawresult = gaussianRandomTruncated((lo - mu)/sigma, (hi-mu)/sigma)
        sigma*rawresult + mu
    }

    def gaussianRandomIsotropic(dim:Int) : Tensor1 =
    {
        val result = new DenseTensor1(dim)
        for (i <- 0 until dim) result(i) = gaussianRandom()
        result
    }

    def gaussianRandomIsotropic(mu:Tensor1, sigma:Double) : Tensor1 =
    {
        val result = gaussianRandomIsotropic(mu.length)
        result *= sigma
        result += mu
        result
    }

    def gaussianRandomIsotropicTruncated(dim:Int, los:Tensor1, his:Tensor1) : Tensor1 =
    {
        val result = new DenseTensor1(dim)
        for (i <- 0 until dim) result(i) = gaussianRandomTruncated(los(i), his(i))
        result
    }

    def gaussianRandomIsotropicTruncated(mu:Tensor1, sigma:Double, los:Tensor1, his:Tensor1) : Tensor1 =
    {
        val result = gaussianRandomIsotropicTruncated(mu.length, ((los - mu)/sigma).asInstanceOf[Tensor1],
                                                                 ((his - mu)/sigma).asInstanceOf[Tensor1])
        result *= sigma
        result += mu
        result
    }

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

    def safeLog(value:Double) : Double =
    {
        math.log(value + 1e-10)
    }

    def concatVectors[T<:Tensor1](vecs:T*) : Tensor1 = concatVectors(vecs.toIterable)

    def concatVectors[T<:Tensor1](vecs:collection.Iterable[T]) : Tensor1 =
    {
        var totaldims = 0
        for (v <- vecs) totaldims += v.length
        val vec = new DenseTensor1(totaldims)
        var finalIndex = 0
        for (v <- vecs; vi <- 0 until v.length)
        {
            vec(finalIndex) = v(vi)
            finalIndex += 1
        }
        vec
    }

  /** Greatest common denomimator and least common multiple **/
  def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
  def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)


  /** All Combinations. from: http://stackoverflow.com/questions/1070859/listing-combinations-with-repetitions-in-scala **/
  def mycomb[T](n: Int, l: List[T]): List[List[T]] =
    n match {
      case 0 => List(List())
      case _ => for(el <- l;
                    sl <- mycomb(n-1, l dropWhile { _ != el } ))
      yield el :: sl
    }

  def comb[T](n: Int, l: List[T]): List[List[T]] = mycomb(n, l.distinct)

}
