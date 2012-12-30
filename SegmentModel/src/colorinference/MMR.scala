package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/29/12
 * Time: 2:49 PM
 * To change this template use File | Settings | File Templates.
 */

import collection.mutable
import collection.mutable.ArrayBuffer


class MMR[ValType](private var samples:Seq[ScoredValue[ValType]], private val simMetric:(ValType,ValType) => Double)
{
    type SampleRecord = ScoredValue[ValType]

    private var minsim = Double.PositiveInfinity
    private var maxsim = Double.NegativeInfinity

    normalizeAndSortScores()
    computeSimilarityNormalization()

    private def normalizeAndSortScores()
    {
        println("[MMR] Normalizing and sorting scores...")
        val minscore = samples.map(_.score).min
        val maxscore = samples.map(_.score).max
        val scorerange = maxscore - minscore
        samples.foreach(s => s.normalizedScore = (s.score - minscore) / scorerange)
        samples = samples.sortWith(_.normalizedScore > _.normalizedScore)
    }

    private def computeSimilarityNormalization()
    {
        println("[MMR] Normalizing similarities...")
        for (i <- 0 until samples.length-1; j <- i+1 until samples.length)
        {
            val sim = simMetric(samples(i).values, samples(j).values)
            minsim = math.min(minsim, sim)
            maxsim = math.max(maxsim, sim)
        }
    }

    def getRankedSamples(numToRetrieve:Int, lambda:Double) : IndexedSeq[SampleRecord] =
    {
        println("[MMR] Retrieving %d ranked samples using lambda = %g...".format(numToRetrieve, lambda))

        // Greedily build up the result list
        val chosenSamples = new mutable.HashSet[SampleRecord]
        val resultList = new ArrayBuffer[SampleRecord]
        for (resultIndex <- 0 until numToRetrieve)
        {
            var bestScore = Double.NegativeInfinity
            var bestCandidate:SampleRecord = null.asInstanceOf[SampleRecord]
            // Consider all samples as the next possible candidate for insertion
            def findBestCandidate()
            {
                for (sample <- samples if (!chosenSamples.contains(sample)))
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
}
