package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/26/12
 * Time: 6:00 PM
 * To change this template use File | Settings | File Templates.
 */

import java.io.{FileWriter, File}
import collection.mutable.ArrayBuffer

object UserColorContraintTest
{
    var params:ModelTrainingParams = null

    val inputDir = "../PatternColorizer/out/mesh"
    val randVisDir = "../PatternColorizer/out/vis_random_offset"
    val mmrVisDir = "../PatternColorizer/out/vis_constraint"
    val lambdas = Array(0.45, 0.5, 0.55)
    val numSamplesToOutput = 8

    // Maximization parameters
    val numParallelChains = 5
    val iterations = 2000
    val initTemp = 1.0
    val finalTemp = 0.01
    val rounds = 40

    val deviationBandwidth = 30.0
    val constraintFactorWeight = 3.0

    // Parallel tempering parameters
    val chainTemps = Array(1.0, 0.5, 0.2, 0.05, 0.01)
    val itersBetweenSwaps = 50

    def main(args:Array[String])
    {
        val visDirTestFile = new File(mmrVisDir)
        if (!visDirTestFile.exists)
            visDirTestFile.mkdir

        val visDirRandomFile = new File(randVisDir)
        if (!visDirRandomFile.exists)
            visDirRandomFile.mkdir

        val trainingArtists = Set("sugar!", "davidgav", "earlgrey", "sandradumit")
        //val trainingArtists = Set("sugar!")
        val patterns = PatternIO.getPatterns(inputDir).filter(p=>(trainingArtists.contains(p.directory))).toArray

        if (patterns.length == 0)
            println("No files found in the input directory!")

        // Setup model training parameters
        params = new ModelTrainingParams
        {
            type VariableType = ContinuousColorVariable
            val colorVarParams = ContinuousColorVariableParams

            modelSaveDirectory = "savedModel"
            doWeightTuning = false
            saveRegressorsIfPossible = true
            saveWeightsIfPossible = true
            loadRegressorsIfPossible = true
            loadWeightsIfPossible = true

            includeColorCompatibilityTerm = false
//            includeUnaryTerms = false
//            includeBinaryTerms = false
//            includeGroupTerms = false

            crossValidateHistogramParams = false
            saveCrossValidationLog = false

            initialLearningRate = 0.2
            numWeightTuningIterations = 20
            //cdK = 100

            enforceMinimumWeight = true
            minWeight = 0.0
        }

        val meshes = for (p <- patterns) yield new SegmentMesh(params.colorVarParams.variableGenerator, p.fullpath)

        // These are the ids of the patterns we will test on
        val pids = Array(
            789577,
            515691,
            1760884,
            1468857,
            1897867,
            2007304)
//            515691)


        val testingMeshes = {for (idx<-meshes.indices if (pids.contains(patterns(idx).name.replace(".txt","").toInt))) yield meshes(idx)}
        val trainingMeshes = for (mesh <- meshes if !testingMeshes.contains(mesh)) yield mesh

        println("Training on " + trainingMeshes.length + " meshes")
        val model = ModelTraining(trainingMeshes, params)

        for (i <- meshes.indices if (pids.contains(patterns(i).name.replace(".txt","").toInt)))
        {
            val pattern = patterns(i)
            println("Generating patterns for mesh %s...".format(pattern.name))
            val mesh = meshes(i)
            outputRandomPatterns(mesh, pattern)
            //outputOptimizedPatterns(mesh, pattern, model)
        }
        println("Done generating patterns")
    }

    def outputRandomPatterns(mesh:SegmentMesh, pattern:PatternItem)
    {
        println("Generating random patterns...")

        val simpleModel = new ColorInferenceModel
        simpleModel += new UserColorConstraintGroupTemplate("userLABConstraint", ModelTraining.labColor, deviationBandwidth)
        simpleModel.conditionOn(mesh)

        val samplerGenerator = () => { new ContinuousColorSampler(simpleModel) with MemorizingSampler[ContinuousColorVariable] }
        val maximizer = new ParallelTemperingMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, chainTemps)

        maximizer.maximize(mesh, iterations, itersBetweenSwaps)

        // Convert sampled values to LAB first, since our similarity metric operates in LAB space
        maximizer.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

        // Spit out a bunch of different versions for different lambdas
        val metric = genMMRSimilarityMetric(mesh.variablesAs[ContinuousColorVariable])
        val mmr = new MMR(maximizer.samples, metric)
        for (lambda <- lambdas)
        {
            val dirName = "%s/%1.2f".format(randVisDir, lambda)

            // Ensure directory exits
            val dir = new File(dirName)
            if (!dir.exists)
                dir.mkdir

            val rankedSamples = mmr.getRankedSamples(numSamplesToOutput, lambda)
            savePatterns(mesh, rankedSamples, dirName, pattern)
        }
    }

    def outputOptimizedPatterns(mesh:SegmentMesh, pattern:PatternItem, model:ColorInferenceModel)
    {
        println("Generating MMR-optimized patterns...")

//        // TEST: Fix one of the colors for conditional inference.
//        mesh.groups(0).color.setColor(Color.RGBColor(0.28, 0.03, 0.23))
//        mesh.groups(0).color.fixed = true
        val constraintTemplate = new UserColorConstraintGroupTemplate("userLABConstraint", ModelTraining.labColor, deviationBandwidth)
        constraintTemplate.setWeight(model.trainableWeights.sum * constraintFactorWeight)
        model += constraintTemplate
        model.conditionOn(mesh)

        val samplerGenerator = () => { new ContinuousColorSampler(model) with MemorizingSampler[ContinuousColorVariable] }
        val maximizer = new ParallelTemperingMAPInferencer[ContinuousColorVariable, SegmentMesh](samplerGenerator, chainTemps)

        maximizer.maximize(mesh, iterations, itersBetweenSwaps)

        // Convert sampled values to LAB first, since our similarity metric operates in LAB space
        maximizer.samples.foreach(_.values.foreach(_.convertTo(LABColorSpace)))

        // Spit out a bunch of different versions for different lambdas
        val metric = genMMRSimilarityMetric(mesh.variablesAs[ContinuousColorVariable])
        val mmr = new MMR(maximizer.samples, metric)
        for (lambda <- lambdas)
        {
            val dirName = "%s/%1.2f".format(mmrVisDir, lambda)

            // Ensure directory exits
            val dir = new File(dirName)
            if (!dir.exists)
                dir.mkdir

            val rankedSamples = mmr.getRankedSamples(numSamplesToOutput, lambda)
            savePatterns(mesh, rankedSamples, dirName, pattern)
        }
    }

    def savePatterns(mesh:SegmentMesh, rankedPatterns:IndexedSeq[ScoredValue[IndexedSeq[Color]]], dir:String, pattern:PatternItem)
    {
        // Need to be able to match up values with variables
        val vars = mesh.variablesAs[ContinuousColorVariable]

        val filename = PatternIO.ensureAndGetFileName(pattern, dir, ".txt")
        val out = new FileWriter(filename)
        out.write("Count " + rankedPatterns.length +"\n")
        for (rp <- rankedPatterns)
        {
            out.write("Score " + rp.score + " false" + "\n")
            for (g <- mesh.groups)
            {
                val indexInValList = vars.indexOf(g.color)
                var color:Color = null
                if (indexInValList == -1)
                    color = g.color.getColor
                else
                    color = rp.values(indexInValList)
                out.write(color.copyIfNeededTo(RGBColorSpace).componentString + "\n")
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
