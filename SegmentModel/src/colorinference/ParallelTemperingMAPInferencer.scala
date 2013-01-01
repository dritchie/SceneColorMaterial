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
    var mixingScores = new ArrayBuffer[MixingRecord]

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

        //print out the convergence history
        mixingScores = coordinator.mixingRecords

        //for (i<-mixingScores.indices) println("Iteration %d, mixing score %f, mean samples score %f".format(i, mixingScores(i).mixingScore, mixingScores(i).meanF))
    }

    case class MixingRecord(val mixingScore:Double, val meanF:Double)

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
        var f:Double = -1
        var W:Double = Double.PositiveInfinity

        private def computeStats(n:Int)
        {
           val samples = sampler.lastSamples(n)
           assert(samples.length == n && n>1, "SamplingChain: There are not enough samples in memory! Or itersBetweenSwaps <= 1")
           val scores = samples.map(s => (s.score))

           f = scores.sum/n

           W = scores.map(s => Math.pow(s-f, 2)).sum/(n-1)
        }


        def act()
        {
            val varsOfInterest = varyingCopy.variablesAs[V]
            val numRounds = iterations / itersBetweenSwaps
            for (i <- 0 until numRounds)
            {
                sampler.process(varsOfInterest, itersBetweenSwaps)

                //TODO: I'm also not sure if this is technically correct here, because from p.523 of PGM book, it seems
                //like there should be a burn-in time and then only samples after that time should be considered
                //if the chain restarts anew (without some acceptance prob), then does the time before count as burn-in time?
                computeStats(itersBetweenSwaps)

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
            var f = -1.0
            var W = Double.PositiveInfinity
        }
        private val chainStates = new mutable.HashMap[SamplingChain, ChainState]
        chainStates ++= (for (chain <- chains) yield (chain, new ChainState(chain)))
        var currRound = 0
        var meanF = -1.0
        var mixingScore = Double.PositiveInfinity
        var mixingRecords = new ArrayBuffer[MixingRecord]

        //PGM p. 523, the R score
        private def computeStats()
        {
          //compute the mean score
          meanF = chainStates.values.map(_.f).sum/chains.length

          //compute the variance between the scores across chains and within chains
          val M = chains(0).itersBetweenSwaps.toDouble //assume these are all the same
          var W = chainStates.values.map(_.W).sum/chains.length
          val BoverM = chainStates.values.map(s => Math.pow(s.f-meanF,2)).sum/(chains.length-1)
          val V = (M-1)/M * W + BoverM

          //guard against 0 W, this is very very unlikely
          if (W == 0.0) W = 0.00001

          mixingScore = Math.sqrt(V/W)

          //record the history, so we can analyze it later
          mixingRecords += new MixingRecord(mixingScore, meanF)

        }


        def act()
        {
            while (true)
            {
                receive
                {
                    case RoundComplete =>
                    {
                        val chain:SamplingChain = sender.asInstanceOf[SamplingChain]
                        val state = chainStates(chain)
                        state.roundsDone += 1
                        state.f = chain.f
                        state.W = chain.W

                        // Once all chains have finished this round, we can decide what to do
                        val minRoundDone = chainStates.values.map(_.roundsDone).min
                        if (minRoundDone > currRound)
                        {
                            currRound += 1

                            //compute convergence stats
                            computeStats()

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
