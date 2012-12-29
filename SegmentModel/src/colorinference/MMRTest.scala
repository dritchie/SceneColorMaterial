package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/26/12
 * Time: 6:00 PM
 * To change this template use File | Settings | File Templates.
 */

import java.io.{FileWriter, File}

object MMRTest
{
    val inputDir = "../PatternColorizer/out/mesh"
    val randVisDir = "../PatternColorizer/out/vis_rand"
    val mmrVisDir = "../PatternColorizer/out/vis_mmr"
    val numSamplesToOutput = 40
    val mmrLambda = 0.75

    def main(args:Array[String])
    {
        // Verify that visDirs exists
//        var visDirTestFile = new File(randVisDir)
//        if (!visDirTestFile.exists)
//            visDirTestFile.mkdir
        val visDirTestFile = new File(mmrVisDir)
        if (!visDirTestFile.exists)
            visDirTestFile.mkdir

        //load all files TODO: to make this manageable, let's try out one subfolders for now
        val patterns = PatternIO.getPatterns(inputDir).filter(p=>(p.directory == "sugar!")).toArray// || p.directory=="cameo")).toArray

        if (patterns.length == 0)
            println("No files found in the input directory!")

        // Setup model training parameters (we'll use Discrete color variables in this test)
        val params = new ModelTrainingParams
        {
            type VariableType = ContinuousColorVariable
            val colorVarParams = ContinuousColorVariableParams

            modelSaveDirectory = "savedModel_withCompat"
            doWeightTuning = true
//            saveRegressorsIfPossible = true
            saveWeightsIfPossible = true
            loadRegressorsIfPossible = true
            loadWeightsIfPossible = false
            includeColorCompatibilityTerm = true

            initialLearningRate = 0.5
            //cdK = 5
        }

        val meshes = for (p <- patterns) yield new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)

        //list of patterns to consider
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


        //train a model on all patterns, except the ones being considered (train/test set)
        val testingMeshes = {for (idx<-meshes.indices if (pids.contains(patterns(idx).name.replace(".txt","").toInt))) yield meshes(idx)}
        val trainingMeshes = {for (m<-meshes if (!testingMeshes.contains(m))) yield m}

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
            MMRSamplingMaximizer.SampleRecord(variables.map(_.getColor), score)
        }).sortWith(_.score > _.score)

        savePatterns(samples, randVisDir, pattern)
    }

    def outputOptimizedPatterns(mesh:SegmentMesh, pattern:PatternItem, model:ColorInferenceModel)
    {
        println("Generating MMR-optimized patterns...")
        val sampler = new ContinuousColorSampler(model)
        val varsOfInterest = mesh.variablesAs[ContinuousColorVariable]
        val metric = genMMRSimilarityMetric(varsOfInterest)
        val maximizer = new MMRSamplingMaximizer[IndexedSeq[ContinuousColorVariable], ContinuousColorVariable](sampler, varsOfInterest, metric, mmrLambda)
        val iterations = 2000
        val initTemp = 1.0
        val finalTemp = 0.01
        val rounds = 40
        model.conditionOn(mesh)
        maximizer.maximize(Array(mesh.variablesAs[ContinuousColorVariable]), iterations, initTemp, finalTemp, rounds)

        // Convert sampled values to LAB first, since our similarity metric operates in LAB space
        maximizer.getRawSamples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

        // Spit out a bunch of different versions for different lambdas
        val lambdas = Array(1.0, 0.75, 0.5, 0.25, 0.0)
        for (lambda <- lambdas)
        {
            val dirName = "%s/%1.2f".format(mmrVisDir, lambda)

            // Ensure directory exits
            val dir = new File(dirName)
            if (!dir.exists)
                dir.mkdir

            maximizer.lambda = lambda
            val rankedSamples = maximizer.getRankedSamples(numSamplesToOutput)
            savePatterns(rankedSamples, dirName, pattern)
        }
    }

    def savePatterns(rankedPatterns:Seq[MMRSamplingMaximizer.SampleRecord[Color]], dir:String, pattern:PatternItem)
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
