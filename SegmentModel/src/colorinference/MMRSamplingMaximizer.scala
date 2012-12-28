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
import collection.mutable

/*
 * Has the same sampling/annealing behavior as SamplingMaximizer, but it also builds up a ranked list of all the samples
 * ever accepted by the underlying MCMC chain. The ranking is according to a variant of Maximum Marginal Relevance.
 * See http://dl.acm.org/citation.cfm?id=291025
 * It is possible to call 'getRankedSamples' multiple times with different lambdas after a single run of the maximizer.
 *  - 'varsOfInterest' are the variables whose state we wish to track/do MMR on. Currently, these must all be of the same type.
 *  - 'simMetric' is a similarity metric between variable values. It should return 1 for identical arguments, 0 for totally dissimilar
 *    arguments, and values between 0 and 1 for arguments with partial similarity.
 *  - 'lambda' controls the tradeoff between score and diversity. Lambda = 1 means only look at score, Lambda = 0 means
 *    only look at diversity
 */
object MMRSamplingMaximizer
{
    case class SampleRecord[ValType](values:Seq[ValType], score:Double) { var normalizedScore = Double.NaN}
}
class MMRSamplingMaximizer[C, V<:Variable](override val sampler:ProposalSampler[C], val varsOfInterest:Seq[V],
                                 val simMetric:(Seq[V#Value],Seq[V#Value]) => Double, var lambda:Double)
    extends SamplingMaximizer[C](sampler)
{
    type SampleRecord = MMRSamplingMaximizer.SampleRecord[V#Value]

    private var lastAcceptedSample:SampleRecord = null.asInstanceOf[SampleRecord]
    private var sampleRecords = new ArrayBuffer[SampleRecord]
    private var currScore = Double.NegativeInfinity
    private var maxScore = Double.NegativeInfinity
    private var minsim = Double.NaN
    private var maxsim = Double.NaN

    // Call this after 'maximize' has finished running.
    def getRawSamples : IndexedSeq[SampleRecord] = sampleRecords

    // Call this after 'maximize' has finished running.
    def getRankedSamples(numToRetrieve:Int) : IndexedSeq[SampleRecord] =
    {
        println("Retrieving %d ranked samples...".format(numToRetrieve))

        val minScore = sampleRecords.map(_.score).min
        val maxScore = sampleRecords.map(_.score).max
        val scoreRange = maxScore - minScore

        // Normalize & sorting scores
        if (sampleRecords(0).normalizedScore.isNaN)
        {
            println("Normalizing scores...")
            sampleRecords.foreach(s => s.normalizedScore = (s.score - minScore)/scoreRange)
            sampleRecords = sampleRecords.sortWith(_.normalizedScore > _.normalizedScore)
        }

        // Normalize similarities
        if (minsim.isNaN || maxsim.isNaN)
            computeSimNormalization()

        // Greedily build up the result list
        println("Building ranked list...")
        val chosenSamples = new mutable.HashSet[SampleRecord]
        val resultList = new ArrayBuffer[SampleRecord]
        for (resultIndex <- 0 until numToRetrieve)
        {
            var bestScore = Double.NegativeInfinity
            var bestCandidate:SampleRecord = null.asInstanceOf[SampleRecord]
            // Consider all samples as the next possible candidate for insertion
            def findBestCandidate()
            {
                for (sample <- sampleRecords if (!chosenSamples.contains(sample)))
                {
                    // Since the samples are sorted by normalized score, we might be able
                    // to cut off the search early.
                    val bestPossibleScore = lambda*sample.normalizedScore
                    if (bestPossibleScore < bestScore) return

                    // Diversity part of the score is based on the maximum similarity between this
                    // sample and any of the samples we've already chosen
                    // TODO: Consider caching similarity metric computations?
                    val maxSim = if (chosenSamples.size == 0) 0 else chosenSamples.map(s => simMetric(s.values, sample.values)).max
                    val score = bestPossibleScore - (1-lambda)*maxSim
                    if (score > bestScore)
                    {
                        bestScore = score
                        bestCandidate = sample
                    }
                }
            }
            findBestCandidate()
            // Add the best candidate to the set of chosen samples
            resultList += bestCandidate
            chosenSamples += bestCandidate
        }

        // Return the results
        resultList
    }

    private def computeSimNormalization()
    {
        println("Normalizing similarities...")
        minsim = 0.0
        maxsim = 1.0
        if (lambda < 1.0)
        {
            minsim = Double.PositiveInfinity
            maxsim = Double.NegativeInfinity
            for (i <- 0 until sampleRecords.length-1; j <- 0 until sampleRecords.length)
            {
                val sim = simMetric(sampleRecords(i).values, sampleRecords(j).values)
                minsim = math.min(sim, minsim)
                maxsim = math.max(sim, maxsim)
            }
        }
    }

    // Overrides of SamplingMaximizer methods that we need in order to correctly keep track of stuff
    override def maximize(varying:Iterable[C], iterations:Int = 50, initialTemperature: Double = 1.0, finalTemperature: Double = 0.01, rounds:Int = 5): Iterable[Variable] =
    {
        lastAcceptedSample = null
        sampleRecords.clear()
        maxScore = sampler.model.currentScore(varsOfInterest)
        minsim = Double.NaN
        maxsim = Double.NaN
        super.maximize(varying, iterations, initialTemperature, finalTemperature, rounds)
    }
    override def maximize(varying:Iterable[C], iterations:Int): Iterable[Variable] =
    {
        currScore = maxScore

        // Called after the proposed change is actually made
        def trackSamples(p:Proposal)
        {
            currScore += p.modelScore
            maxScore = math.max(currScore, maxScore)
            val curValues = varsOfInterest.map(_.value.asInstanceOf[V#Value])
            val sample = MMRSamplingMaximizer.SampleRecord(curValues, currScore)
            if (!sample.equals(lastAcceptedSample))
            {
                sampleRecords += sample
                lastAcceptedSample = sample
            }
        }

        val trackingHook: Proposal=>Unit = trackSamples _
        sampler.proposalHooks += trackingHook
        val retval = super.maximize(varying, iterations)
        sampler.proposalHooks -= trackingHook
        retval
    }
}
