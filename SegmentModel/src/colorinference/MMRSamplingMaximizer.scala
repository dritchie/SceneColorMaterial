package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/26/12
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import collection.mutable.ArrayBuffer

/*
 * Has the same sampling/annealing behavior as SamplingMaximizer, but it also builds up a ranked list of all the samples
 * ever accepted by the underlying MCMC chain. The ranking is according to a variant of Maximum Marginal Relevance.
 * See http://dl.acm.org/citation.cfm?id=291025
 * It is possible to call 'getRankedSamples' multiple times with different lambdas after a single run of the maximizer.
 *  - 'varsOfInterest' are the variables whose state we wish to track/do MMR on. Currently, these must all be of the same type.
 *  - 'simMetric' is a similarity metric between variable values. It should return larger numbers for more similar sets of values
 *  - 'lambda' controls the tradeoff between score and diversity. Lambda = 1 means only look at score, Lambda = 0 means
 *    only look at diversity
 */
object MMRSamplingMaximizer
{
    case class SampleRecord[ValType](values:Seq[ValType], score:Double) { var normalizedScore = Double.NaN}
}
class MMRSamplingMaximizer[C, V<:Variable](override val sampler:ProposalSampler[C], val varsOfInterest:Seq[V],
                                 val simMetric:(Seq[V],Seq[V#Value],Seq[V#Value]) => Double, var lambda:Double)
    extends SamplingMaximizer[C](sampler)
{
    type SampleRecord = MMRSamplingMaximizer.SampleRecord[V#Value]

    private val sampleRecords = new ArrayBuffer[SampleRecord]
    private var currScore = Double.NegativeInfinity
    private var maxScore = Double.NegativeInfinity
    private var minsim = Double.NaN
    private var maxsim = Double.NaN

    // Call this after 'maximize' has finished running.
    def getRankedSamples(numToRetrieve:Int) : IndexedSeq[SampleRecord] =
    {
        println("Retrieving %d ranked samples...".format(numToRetrieve))

        val minScore = sampleRecords.map(_.score).min
        val maxScore = sampleRecords.map(_.score).max
        val scoreRange = maxScore - minScore

        // Normalize scores
        if (sampleRecords(0).normalizedScore.isNaN)
        {
            println("Normalizing scores...")
            sampleRecords.foreach(s => s.normalizedScore = (s.score - minScore)/scoreRange)
        }

        // Normalize similarities
        if (minsim.isNaN || maxsim.isNaN)
            computeSimilarityNormalization()
        val simRange = maxsim - minsim

        // Greedily build up the result list
        println("Building ranked list...")
        val remainingSamples = new ArrayBuffer[SampleRecord]; remainingSamples ++= sampleRecords
        val chosenSamples = new ArrayBuffer[SampleRecord]
        for (resultIndex <- 0 until numToRetrieve)
        {
            var bestScore = 0.0
            var bestCandidate:SampleRecord = null.asInstanceOf[SampleRecord]
            // Consider all samples as the next possible candidate for insertion
            for (sample <- remainingSamples)
            {
                // Diversity part of the score is based on the maximum similarity between this
                // sample and any of the samples we've already chosen
                val maxSim = if (chosenSamples.length == 0) 0 else chosenSamples.map(s => (simMetric(varsOfInterest, s.values, sample.values) - minsim)/simRange).max
                val score = lambda*sample.normalizedScore - (1-lambda)*maxSim
                if (score > bestScore)
                {
                    bestScore = score
                    bestCandidate = sample
                }
            }
            // Add the best candidate to the set of chosen samples, remove it
            // from the list of samples under consideration
            chosenSamples += bestCandidate
            remainingSamples -= bestCandidate
        }

        // Return the results
        chosenSamples
    }

    private def computeSimilarityNormalization()
    {
        // TODO: This is probably really slow, but I can't think of a great way around it.
        // TODO: Also, if we're doing all this, it'd be nice to save the similarity matrix
        // TODO: (but that's too big to fit in memory, in general)
        minsim = 0.0
        maxsim = 1.0
        if (lambda < 1.0)
        {
            minsim = Double.PositiveInfinity
            maxsim = Double.NegativeInfinity
            println("Normalizing similarities...")
            for (i <- 0 until sampleRecords.length-1; j <- i+1 until sampleRecords.length)
            {
                val sim = simMetric(varsOfInterest, sampleRecords(i).values, sampleRecords(j).values)
                minsim = math.min(sim, minsim)
                maxsim = math.max(sim, maxsim)
            }
        }
    }

    // Overrides of SamplingMaximizer methods that we need in order to correctly keep track of stuff
    override def maximize(varying:Iterable[C], iterations:Int = 50, initialTemperature: Double = 1.0, finalTemperature: Double = 0.01, rounds:Int = 5): Iterable[Variable] =
    {
        sampleRecords.clear()
        minsim = Double.NaN
        maxsim = Double.NaN
        maxScore = sampler.model.currentScore(varsOfInterest)
        super.maximize(varying, iterations, initialTemperature, finalTemperature, rounds)
    }
    override def maximize(varying:Iterable[C], iterations:Int): Iterable[Variable] =
    {
        currScore = maxScore

        // Called after the proposed change is actually made
        def proposalHook(p:Proposal)
        {
            currScore += p.modelScore
            maxScore = math.max(currScore, maxScore)
            val curValues = varsOfInterest.map(_.value.asInstanceOf[V#Value])
            sampleRecords += MMRSamplingMaximizer.SampleRecord(curValues, currScore)
        }

        sampler.proposalHooks += proposalHook
        val retval = super.maximize(varying, iterations)
        sampler.proposalHooks -= proposalHook
        retval
    }
}
