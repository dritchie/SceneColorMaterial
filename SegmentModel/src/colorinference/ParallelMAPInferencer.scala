package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/29/12
 * Time: 4:22 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import collection.mutable.ArrayBuffer
import actors.Futures._

class ParallelMAPInferencer[V<:Variable, S<:(VariableStructure with Copyable[S])](private val baseSamplerGenerator:() => MemorizingSampler[V], private val numChains:Int)
{
    type SampleRecord = ScoredValue[IndexedSeq[V#Value]]
    val samples = new ArrayBuffer[SampleRecord]

    def maximize(varying:S, iterations:Int = 50, initialTemperature: Double = 1.0, finalTemperature: Double = 0.01, rounds:Int = 5)
    {
        samples.clear()

        // Allocate samplers
        val samplers = for (i <- 0 until numChains) yield baseSamplerGenerator()

        // Set up the asynchronous computations
        val futures = for (i <- 0 until numChains) yield future
        {
            val maximizer = new SamplingMaximizer(samplers(i))
            val varyingCopy = varying.copy
            val varsToSample = varyingCopy.variablesAs[V]
            maximizer.maximize(Array(varsToSample), iterations, initialTemperature, finalTemperature, rounds)
        }

        // Run them all to completion
        futures.foreach(_())

        // Aggregate their samples
        for (sampler <- samplers) samples ++= sampler.samples

//        val sampler = baseSamplerGenerator()
//        val maximizer = new SamplingMaximizer(sampler)
//        val varyingCopy = varying.copy
//        val varsToSample = varyingCopy.variablesAs[V]
//        maximizer.maximize(Array(varsToSample), iterations, initialTemperature, finalTemperature, rounds)
//        samples ++= sampler.samples
    }
}
