package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/29/12
 * Time: 3:03 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import collection.mutable.ArrayBuffer

case class ScoredValue[ValType](values:ValType, score:Double) { var normalizedScore = Double.NaN}

trait MemorizingSampler[V<:Variable] extends MHSampler[IndexedSeq[V]]
{
    type SampleRecord = ScoredValue[IndexedSeq[V#Value]]
    val samples = new ArrayBuffer[SampleRecord]
    private var variablesOfInterest:IndexedSeq[V] = null
    private var currScore = Double.NaN

    // We use this to keep track of what the current variables of interest
    // are, so that we can track their values
    abstract override def proposals(context:IndexedSeq[V]) : Seq[Proposal] =
    {
        variablesOfInterest = context
        super.proposals(context)
    }

    // Get the current values of the variables of interest
    private def currValuesOfInterest() : IndexedSeq[V#Value] =
    {
        variablesOfInterest.map(_.value.asInstanceOf[V#Value])
    }

    def resetSamplingMemory()
    {
        samples.clear()
        variablesOfInterest = null
        currScore = Double.NaN
    }

    // A 'proposalsHook' that makes sure we are keeping track of the current score
    // correctly. The current score could change under us at any moment, so we can't
    // assume it's always the socre of the last sample we've seen
    private val ensureCorrectCurrScore : (Seq[cc.factorie.Proposal] => Unit) = (proposals:Seq[cc.factorie.Proposal]) =>
    {
        // If we have no samples, or if the current value of our variables of interest
        // does not match the last value we saw, we assume the variable state has been reset
        // and we need to compute the score fresh.
        if (samples.length == 0 || !currValuesOfInterest().equals(samples.last.values))
        {
            currScore = model.currentScore(variablesOfInterest)
        }
    }
    proposalsHooks += ensureCorrectCurrScore

    // A 'proposalHook' that takes the proposed (and accepted) change and adds
    // a new sample accordingly. We only add if this sample is not a duplicate of the
    // previous one (these are typically 'failed' MH proposals, and they're redundant)
    private val addNewSample : (cc.factorie.Proposal => Unit) = (proposal:cc.factorie.Proposal) =>
    {
        currScore += proposal.modelScore
        val curVals = currValuesOfInterest()
        if (samples.length == 0 || !curVals.equals(samples.last.values))
        {
            samples += ScoredValue(curVals, currScore)
        }
    }
    proposalHooks += addNewSample
}
