package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/29/12
 * Time: 5:25 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import collection.mutable.ArrayBuffer
import actors.Actor
import collection.mutable
import java.util.concurrent.CountDownLatch

/*
 * This is not, strictly speaking, *true* parallel tempering, since it doesn't evaluate whether to make a swap using the
 * MH acceptance ratio. Rather, it just *always* makes a swap. There's much less inter-thread communication this way, so
 * it's a lot easier to implement.
 */
class ParallelTemperingMAPInferencer[V<:Variable, S<:(VariableStructure with Copyable[S])](private val baseSamplerGenerator:(S) => MemorizingSampler[V],
                                                                                           private val chainTemps:IndexedSeq[Double])
{
    type SampleRecord = ScoredValue[IndexedSeq[V#Value]]
    val samples = new ArrayBuffer[SampleRecord]

    def maximize(varying:S, iterations:Int, iterationsBetweenSwaps:Int)
    {
        samples.clear()

        // Set up the chains and the coordinator
        val chains = for (t <- chainTemps) yield new SamplingChain(varying.copy, iterations, iterationsBetweenSwaps, t)
        val latch = new CountDownLatch(1)
        val coordinator = new ChainCoordinator(chains, latch)
        chains.foreach(_.coordinator = coordinator)

        // Run them
        coordinator.start()
        chains.foreach(_.start())
        latch.await()

        // Aggregate samples
        for (c <- chains) samples ++= c.sampler.samples
    }

    // Messages
    case object RoundComplete
    case object Resume
    case class AssumeThisTemp(temp:Double)
    case object ChainFinished

    // Actors
    class SamplingChain(val varyingCopy:S, val iterations:Int, val itersBetweenSwaps:Int, temp:Double) extends Actor
    {
        var coordinator:ChainCoordinator = null     // This must be set before running this actor!
        val sampler = baseSamplerGenerator(varyingCopy)
        sampler.temperature = temp

        def act()
        {
            val varsOfInterest = varyingCopy.variablesAs[V]
            val numRounds = iterations / itersBetweenSwaps
            for (i <- 0 until numRounds)
            {
                sampler.process(varsOfInterest, itersBetweenSwaps)
                coordinator ! RoundComplete
                var waiting = true
                while (waiting) receive
                {
                    case Resume => waiting = false
                    case AssumeThisTemp(newtemp) => { sampler.temperature = newtemp; waiting = false }
                }
            }
            coordinator ! ChainFinished
            exit()
        }
    }
    class ChainCoordinator(chains:IndexedSeq[SamplingChain], val latch:CountDownLatch) extends Actor
    {
        class ChainState(chain:SamplingChain)
        {
            var temp = chain.sampler.temperature
            var roundsDone = 0
            var isFinished = false
        }
        private val chainStates = new mutable.HashMap[SamplingChain, ChainState]
        chainStates ++= (for (chain <- chains) yield (chain, new ChainState(chain)))
        var currRound = 0

        def act()
        {
            while (true)
            {
                receive
                {
                    case RoundComplete =>
                    {
                        val chain = sender.asInstanceOf[SamplingChain]
                        val state = chainStates(chain)
                        state.roundsDone += 1

                        // Once all chains have finished this round, we can decide what to do
                        val minRoundDone = chainStates.values.map(_.roundsDone).min
                        if (minRoundDone > currRound)
                        {
                            currRound += 1

                            // Pick two chains at random to swap temperatures
                            // (and update the coordinator's internal state)
                            val cs = collection.mutable.ArrayBuffer(chainStates.keys.toSeq:_*)
                            val rc1 = cs(random.nextInt(cs.length))
                            cs -= rc1
                            val rc2 = cs(random.nextInt(cs.length))
                            val temp1 = chainStates(rc1).temp
                            val temp2 = chainStates(rc2).temp
                            chainStates(rc2).temp = temp1
                            chainStates(rc1).temp = temp2
                            rc1 ! AssumeThisTemp(temp2)
                            rc2 ! AssumeThisTemp(temp1)

                            // Notify the rest of the chains to procede as normal
                            for (c <- chainStates.keys if c != rc1 && c != rc2) c ! Resume
                        }
                    }
                    case ChainFinished =>
                    {
                        chainStates(sender.asInstanceOf[SamplingChain]).isFinished = true
                        val allFinished = chainStates.values.map(_.isFinished).reduce(_ && _)
                        if (allFinished)
                        {
                            latch.countDown()
                            exit()
                        }
                    }
                }
            }
        }
    }
}
