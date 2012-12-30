package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/26/12
 * Time: 6:00 PM
 * To change this template use File | Settings | File Templates.
 */

import java.io.{FileWriter, File}
import cc.factorie.SamplingMaximizer

object MMRTest
{
    val inputDir = "../PatternColorizer/out/mesh"
    val randVisDir = "../PatternColorizer/out/vis_rand"
    val mmrVisDir = "../PatternColorizer/out/vis_mmr"
    val lambdas = Array(0.25, 0.5, 0.75)
    val numSamplesToOutput = 40
    val numParallelChains = 1
    val iterations = 2000
    val initTemp = 1.0
    val finalTemp = 0.01
    val rounds = 40

    def main(args:Array[String])
    {
        // Verify that visDirs exists
//        var visDirTestFile = new File(randVisDir)
//        if (!visDirTestFile.exists)
//            visDirTestFile.mkdir
        val visDirTestFile = new File(mmrVisDir)
        if (!visDirTestFile.exists)
            visDirTestFile.mkdir

        val testingArtist = "sugar!"
        val trainingArtist = "davidgav"
        val patterns = PatternIO.getPatterns(inputDir).filter(p=>(p.directory == testingArtist || p.directory == trainingArtist)).toArray

        if (patterns.length == 0)
            println("No files found in the input directory!")

        // Setup model training parameters (we'll use Discrete color variables in this test)
        val params = new ModelTrainingParams
        {
            type VariableType = ContinuousColorVariable
            val colorVarParams = ContinuousColorVariableParams

            modelSaveDirectory = "savedModel"
            doWeightTuning = true
            saveRegressorsIfPossible = true
            saveWeightsIfPossible = true
            loadRegressorsIfPossible = true
            loadWeightsIfPossible = true

            includeColorCompatibilityTerm = true

            initialLearningRate = 0.2
            numWeightTuningIterations = 20
            //cdK = 100

            enforceMinimumWeight = true
            minWeight = 0.0
        }

        val meshes = for (p <- patterns) yield new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)

        // These are the ids of the patterns we will test on
        val pids = Array(
            296605,
            244833,
            231386,
            447439,
            499194,
            506633,
            789577,
            304986,
            243893,
            220077,
            500393,
            508162,
            515691,
            798455)


        val testingMeshes = {for (idx<-meshes.indices if (pids.contains(patterns(idx).name.replace(".txt","").toInt))) yield meshes(idx)}
        val trainingMeshes = for (idx <- meshes.indices if (patterns(idx).directory == trainingArtist)) yield meshes(idx)
        //val trainingMeshes = testingMeshes

        println("Training on " + trainingMeshes.length + " meshes")
        val model = ModelTraining(trainingMeshes, params)

        for (i <- meshes.indices if (pids.contains(patterns(i).name.replace(".txt","").toInt)))
        {
            val pattern = patterns(i)
            println("Generating patterns for mesh %s...".format(pattern.name))
            val mesh = meshes(i)
            //outputRandomPatterns(mesh, pattern, model)
            outputOptimizedPatterns(mesh, pattern, model)
        }
    }

    def outputRandomPatterns(mesh:SegmentMesh, pattern:PatternItem, model:ColorInferenceModel)
    {
        println("Generating random patterns...")
        val samples = (for (i <- 0 until numSamplesToOutput) yield
        {
            mesh.randomizeVariableAssignments()
            val variables = mesh.variablesAs[ContinuousColorVariable]
            model.conditionOn(mesh)
            val score = model.currentScore(variables)
            ScoredValue(variables.map(_.getColor), score)
        }).sortWith(_.score > _.score)

        savePatterns(samples, randVisDir, pattern)
    }

    def outputOptimizedPatterns(mesh:SegmentMesh, pattern:PatternItem, model:ColorInferenceModel)
    {
        println("Generating MMR-optimized patterns...")
//        val sampler = new ContinuousColorSampler(model) with MemorizingSampler[ContinuousColorVariable]
//        val maximizer = new SamplingMaximizer(sampler)
        val samplerGenerator = () => new ContinuousColorSampler(model) with MemorizingSampler[ContinuousColorVariable]
        val maximizer = new ParallelMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, numParallelChains)
        model.conditionOn(mesh)
        maximizer.maximize(mesh, iterations, initTemp, finalTemp, rounds)
        //maximizer.maximize(Array(mesh.variablesAs[ContinuousColorVariable]), iterations, initTemp, finalTemp, rounds)

        // Convert sampled values to LAB first, since our similarity metric operates in LAB space
        maximizer.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))
        //sampler.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

        // Spit out a bunch of different versions for different lambdas
        val metric = genMMRSimilarityMetric(mesh.variablesAs[ContinuousColorVariable])
        val mmr = new MMR(maximizer.samples, metric)
        //val mmr = new MMR(sampler.samples, metric)
        for (lambda <- lambdas)
        {
            val dirName = "%s/%1.2f".format(mmrVisDir, lambda)

            // Ensure directory exits
            val dir = new File(dirName)
            if (!dir.exists)
                dir.mkdir

            val rankedSamples = mmr.getRankedSamples(numSamplesToOutput, lambda)
            savePatterns(rankedSamples, dirName, pattern)
        }
    }

    def savePatterns(rankedPatterns:IndexedSeq[ScoredValue[IndexedSeq[Color]]], dir:String, pattern:PatternItem)
    {
        val filename = PatternIO.ensureAndGetFileName(pattern, dir, ".txt")
        val out = new FileWriter(filename)
        out.write("Count " + rankedPatterns.length +"\n")
        for (rp <- rankedPatterns)
        {
            out.write("Score " + rp.score + " false" + "\n")
            for (c <- rp.values)
            {
                out.write(c.copyIfNeededTo(RGBColorSpace).componentString + "\n")
            }
        }
        out.close()
    }

    // This is the maximum possible distance between two colors in LAB space
    private val maxLABdist = math.sqrt(100*100 + 200*200 + 200*200)
    def genMMRSimilarityMetric(vars:Seq[ContinuousColorVariable]) : (Seq[Color], Seq[Color]) => Double =
    {
        val maxDist = vars.map(_.group.size * maxLABdist).sum

        def metric(vals1:Seq[Color], vals2:Seq[Color]) : Double =
        {
            // Size-weighted Delta E, normalized by maximum possible difference
            var dist = 0.0
            for (i <- 0 until vars.length)
                dist += (vars(i).group.size * Color.perceptualDifference(vals1(i), vals2(i)))
            1.0 - (dist / maxDist)
        }

        metric
    }
}
