package colorinference

import cc.factorie.la._
import collection.mutable.{HashMap, ArrayBuffer}
import cc.factorie._
import cc.factorie.optimize._
import collection.{mutable, Set}
import util.DoubleAccumulator
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/18/12
 * Time: 11:26 AM
 * To change this template use File | Settings | File Templates.
 */

/* Various parameters for defining/learning the model */

abstract class ModelTrainingParams
{
    var regression:HistogramRegressor.RegressionFunction = HistogramRegressor.LogisticRegression

    //include the segment factors?
    var includeUnaryTerms = true
    var includeBinaryTerms = true
    var includeGroupTerms = true

    // This is only possible if we're doing continuous variables, though
    var includeColorCompatibilityTerm = false

    // Training histogram regressors
    var fixedBandwidthScale = 1.0
    var fixedNumBins = 7
    var crossValidateHistogramParams = false
    var saveCrossValidationLog = false
    import HistogramRegressor.CrossValidationRanges
    var cvRanges = CrossValidationRanges(VectorHistogram.numBandwidthEstimationNeighbors+1 until 15,
                                             Array(4.0, 2.0, 1.0, 1/2.0, 1/4.0, 1/8.0, 1/16.0, 1/32.0, 1/64.0, 1/128.0, 1/256.0))

    /** filtering options**/
    var filterWhenTraining = false //filter meshes when training the model? If we want to filter when testing, we have to enforce that ourselves
    var ignoreNoise = false
    var segK  = -1 //only consider the top segK segments per group, -1 has no effect
    var adjK = -1 //only consider the top adjK adjacencies per segment, -1 has no effect

    //threshold at which to cap the distance
    //var perceptualDistThresh = Double.PositiveInfinity

    // Saving and loading model components so iterating on experiments doesn't
    // take forever
    var modelSaveDirectory = "savedModel"   // Change to whatever
    var saveWeightsIfPossible = false
    var loadWeightsIfPossible = false
    var saveRegressorsIfPossible = false
    var loadRegressorsIfPossible = false


    /* BEGIN weight tuning */

    var doWeightTuning = true

    object TrainerType extends Enumeration
    {
        type TrainerType = Value
        val SampleRank, MaximumLikelihood, ContrastiveDivergence = Value
    }
    var trainerType = TrainerType.ContrastiveDivergence

    var cdK = 1
    var normalizeWeights = false

    // Old stuff
    var numWeightTuningIterations = 10
    var enforceMinimumWeight = false
    var minWeight = 0.0
    var initialLearningRate = 1.0

    // New stuff
    var weightTuningMiniBatchSize = 10
    var weightTuningValidationSetSize = 0.25
    var weightTuningBatchesBetweenOverfitCheck = 4
    var weightTuningWeightUpdateMagnitude = 10e-3
    var weightTuningOverfitThreshold = 0.025

    /* END weight tuning */


    // MH Sampling
    var minRadius:Double = 0.01
    var maxRadius:Double = 0.33
    var minSwapProb:Double = 0.05
    var maxSwapProb:Double = 0.5

    var includeColorChoiceTerms = false
    var colorChoiceType = ModelTraining.ColorChoiceType.LABConditional

    //weigh the groups or not?
    var weightGroups = false

    // Which variable type are we using?
    type VariableType <: ColorVariable
    def colorVarParams:ColorVariableParams[VariableType]
}

/* Different variable types require different model building/training operations */
trait ColorVariableParams[V<:ColorVariable]
{
    type SamplingContextType = IndexedSeq[V]
    def variableGenerator:ColorVariableGenerator
    def newUnarySegmentTemplate(prop:ModelTraining#UnarySegmentProperty, loadFrom:String = ""):UnarySegmentTemplate[V]
    def newBinarySegmentTemplate(prop:ModelTraining#BinarySegmentProperty, loadFrom:String = ""):BinarySegmentTemplate[V]
    def newGroupTemplate(prop:ModelTraining#ColorGroupProperty, loadFrom:String = "", wBySize:Boolean=false):ColorGroupTemplate[V]
    def newInferenceSampler(model:Model, objective:Model, params:ModelTrainingParams):MHSampler[SamplingContextType]
    def newTrainingSampler(model:Model, params:ModelTrainingParams):KStepContrastiveDivergence[SamplingContextType]
    def initDomain(mesh:SegmentMesh)
    def supportsMaxLikelihood:Boolean
    def supportsColorCompatibility:Boolean
}
object DiscreteColorVariableParams extends ColorVariableParams[DiscreteColorVariable]
{
    def variableGenerator = DiscreteColorVariable
    def newUnarySegmentTemplate(prop:ModelTraining#UnarySegmentProperty, loadFrom:String = "") = new DiscreteUnarySegmentTemplate(prop,loadFrom)
    def newBinarySegmentTemplate(prop:ModelTraining#BinarySegmentProperty, loadFrom:String = "") = new DiscreteBinarySegmentTemplate(prop,loadFrom)
    def newGroupTemplate(prop:ModelTraining#ColorGroupProperty, loadFrom:String = "", wBySize:Boolean=false) = new DiscreteColorGroupTemplate(prop,loadFrom, wBySize)
    def newInferenceSampler(model:Model, objective:Model, params:ModelTrainingParams) = new DiscreteColorSampler(model, objective)
    def newTrainingSampler(model:Model, params:ModelTrainingParams) = new DiscreteColorTrainingSampler(model, params.cdK)
    def initDomain(mesh:SegmentMesh)
    {
        val palette = ColorPalette(mesh)
        DiscreteColorVariable.initDomain(palette)
        for (color <- palette) color.convertTo(LABColorSpace)   // Since most features are in LAB
    }
    def supportsMaxLikelihood:Boolean = true
    def supportsColorCompatibility:Boolean = false
}
object ContinuousColorVariableParams extends ColorVariableParams[ContinuousColorVariable]
{
    def variableGenerator = ContinuousColorVariable
    def newUnarySegmentTemplate(prop:ModelTraining#UnarySegmentProperty, loadFrom:String = "") = new ContinuousUnarySegmentTemplate(prop,loadFrom)
    def newBinarySegmentTemplate(prop:ModelTraining#BinarySegmentProperty, loadFrom:String = "") = new ContinuousBinarySegmentTemplate(prop,loadFrom)
    def newGroupTemplate(prop:ModelTraining#ColorGroupProperty, loadFrom:String = "", wBySize:Boolean=false) = new ContinuousColorGroupTemplate(prop,loadFrom, wBySize)
    def newInferenceSampler(model:Model, objective:Model, params:ModelTrainingParams) =
        new ContinuousColorSampler(model, objective, params.minRadius, params.maxRadius,
            params.minSwapProb, params.maxSwapProb, null)
    def newTrainingSampler(model:Model, params:ModelTrainingParams) =
        new ContinuousColorTrainingSampler(model, params.minRadius, params.maxRadius,
            params.minSwapProb, params.maxSwapProb, null, params.cdK)
    def initDomain(mesh:SegmentMesh) { }
    def supportsMaxLikelihood:Boolean = false
    def supportsColorCompatibility:Boolean = true
}

object ModelTraining
{
    val namingModel = new ColorNamingModel("../c3_data.json")

    /* enums */
    object ColorChoiceType extends Enumeration
    {
      type CholorChoiceType = Value
      val LABMarginal, NamesMarginal, LABConditional, NamesConditional = Value
    }

    /* Color properties */

    // Unary
    def colorfulness(c:Color) = Tensor1(c.colorfulness)
    def lightness(c:Color) = Tensor1(c.copyIfNeededTo(LABColorSpace)(0))
    def nameSaliency(c:Color) = Tensor1(namingModel.saliency(c))
    def labColor(c:Color) = {c.copyIfNeededTo(LABColorSpace)}
    def colorNames(c:Color) = {namingModel.getTermCountsVector(c)}

    // Binary
    def perceptualDifference(c1:Color, c2:Color) = Tensor1(Color.perceptualDifference(c1, c2))
    def chromaDifference(c1:Color, c2:Color) = Tensor1(Color.chromaDifference(c1, c2))
    def relativeColorfulness(c1:Color, c2:Color) = Tensor1(Color.relativeColorfulness(c1, c2))
    def relativeLightness(c1:Color, c2:Color) = Tensor1(Color.relativeLightness(c1, c2))
    def nameSimilarity(c1:Color, c2:Color) = Tensor1(namingModel.cosineSimilarity(c1, c2))

    def apply(trainingMeshes:IndexedSeq[SegmentMesh], params:ModelTrainingParams) : ColorInferenceModel =
    {
        val training = new ModelTraining(params)
        training.train(trainingMeshes)
    }
}

class ModelTraining(val params:ModelTrainingParams)
{
    type Examples = ArrayBuffer[HistogramRegressor.RegressionExample]
    case class UnarySegmentProperty(name:String, extractor:UnarySegmentTemplate.ColorPropertyExtractor,
                                    ranges:HistogramRegressor.CrossValidationRanges,
                                    metric:MathUtils.DistanceMetric = MathUtils.euclideanDistance)
    {
        val regression = params.regression
        val examples = new Examples
        var quantLevel = params.fixedNumBins
        var bandScale = params.fixedBandwidthScale
        val crossValidate = params.crossValidateHistogramParams
        val saveValidationLog = params.saveCrossValidationLog
    }
    case class BinarySegmentProperty(name:String, extractor:BinarySegmentTemplate.ColorPropertyExtractor,
                                     ranges:HistogramRegressor.CrossValidationRanges,
                                     metric:MathUtils.DistanceMetric = MathUtils.euclideanDistance)
    {
        val regression = params.regression
        val examples = new Examples
        var quantLevel = params.fixedNumBins
        var bandScale = params.fixedBandwidthScale
        val crossValidate = params.crossValidateHistogramParams
        val saveValidationLog = params.saveCrossValidationLog
    }
    case class ColorGroupProperty(name:String, extractor:ColorGroupTemplate.ColorPropertyExtractor,
                                  ranges:HistogramRegressor.CrossValidationRanges,
                                  isMarginal:Boolean = false, metric:MathUtils.DistanceMetric = MathUtils.euclideanDistance)
    {
        val regression = params.regression
        val examples = new Examples
        var quantLevel = params.fixedNumBins
        var bandScale = params.fixedBandwidthScale
        val crossValidate = params.crossValidateHistogramParams
        val saveValidationLog = params.saveCrossValidationLog
    }

    /* Unary segment properties */
    val unarySegProps = new ArrayBuffer[UnarySegmentProperty]()
    if (params.includeUnaryTerms)
    {
      unarySegProps += UnarySegmentProperty("Lightness", ModelTraining.lightness, params.cvRanges)
      unarySegProps += UnarySegmentProperty("Colorfulness", ModelTraining.colorfulness, params.cvRanges)
      unarySegProps += UnarySegmentProperty("Name Saliency", ModelTraining.nameSaliency, params.cvRanges)
    }

    /* Binary segment properties */
    // The assumption for the binary properties thus far is that they're symmetric (no directionality between the variables), which is probably ok
    val binarySegProps = new ArrayBuffer[BinarySegmentProperty]()
    if (params.includeBinaryTerms)
    {
      binarySegProps += BinarySegmentProperty("Perceptual Difference", ModelTraining.perceptualDifference, params.cvRanges)
      binarySegProps += BinarySegmentProperty("Chroma Difference", ModelTraining.chromaDifference, params.cvRanges)
      binarySegProps += BinarySegmentProperty("Relative Colorfulness", ModelTraining.relativeColorfulness, params.cvRanges)
      binarySegProps += BinarySegmentProperty("Relative Lightness", ModelTraining.relativeLightness, params.cvRanges)
      binarySegProps += BinarySegmentProperty("Name Similarity", ModelTraining.nameSimilarity, params.cvRanges)
    }

    /* Color group properties */
    val groupProps = new ArrayBuffer[ColorGroupProperty]()
    if (params.includeGroupTerms)
    {
      groupProps += ColorGroupProperty("Lightness", ModelTraining.lightness, params.cvRanges)
      groupProps += ColorGroupProperty("Colorfulness", ModelTraining.colorfulness, params.cvRanges)
      groupProps += ColorGroupProperty("Name Saliency", ModelTraining.nameSaliency, params.cvRanges)
    }

    val groupMarginalProps = new ArrayBuffer[ColorGroupProperty]() //color group properties that don't have any features

    //we could add color choice properties for unary and binary factors too, but that might be overfitting
    if (params.includeColorChoiceTerms)
    {
      //lets try adding segment properties too.., maybe some of the terms would help inform better assignment. Also, we might as well be symmetric/consistent
      params.colorChoiceType match
      {
        case ModelTraining.ColorChoiceType.LABMarginal =>
          groupMarginalProps += ColorGroupProperty("LABColor Marginal", ModelTraining.labColor, params.cvRanges, true)
        case ModelTraining.ColorChoiceType.LABConditional =>
          groupProps += ColorGroupProperty("LABColor Conditional", ModelTraining.labColor, params.cvRanges, false)
          unarySegProps += UnarySegmentProperty("LABColor Conditional", ModelTraining.labColor, params.cvRanges, MathUtils.cosineDistance)
        case ModelTraining.ColorChoiceType.NamesMarginal =>
          groupMarginalProps += ColorGroupProperty("ColorNames Marginal", ModelTraining.colorNames, params.cvRanges, true, MathUtils.cosineDistance)
        case ModelTraining.ColorChoiceType.NamesConditional =>
          groupProps += ColorGroupProperty("ColorNames Conditional", ModelTraining.colorNames, params.cvRanges, false, MathUtils.cosineDistance)
          unarySegProps += UnarySegmentProperty("ColorNames Conditional", ModelTraining.colorNames, params.cvRanges, MathUtils.cosineDistance)

        case _ => throw new Error("No valid trainer type!")
      }
    }

    def train(trainingMeshes:IndexedSeq[SegmentMesh]) : ColorInferenceModel =
    {
        // Shuffle meshes to eliminate potential bias
        val shuffledTrainingMeshes = trainingMeshes.shuffle.toIndexedSeq

        /** Extract training data points from meshes **/

        // Training meshes with more segments generate more samples. Here we eliminate that bias
        // repeating the examples according to the lcm doesn't work...as the lcm turns out to be big, and we run out of heap space
        // so we'll weight each example according to 1/numSegments or 1/numAdjacencies. Scale by 2, so we don't run into rounding errors (when Weka checks that weights add up to >=1)
        for (mesh <- shuffledTrainingMeshes)
        {
            //filter the mesh, if specified
            if (params.filterWhenTraining)
              mesh.filter(params.segK, params.adjK, params.ignoreNoise)

            val unaryWeight = 2.0/mesh.segments.length

            // Unary segment properties
            for (seg <- mesh.segments.shuffle)
            {
                val fvec = Segment.getUnaryRegressionFeatures(seg)
                for (prop <- unarySegProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg.group.color.observedColor), fvec._1, fvec._2, unaryWeight) }
            }

            var checkAdj = 0
            for (seg1<-mesh.segments; seg2 <- seg1.adjacencies.values.map(a=>a.neighbor) if seg1.index < seg2.index)
                checkAdj+=1

            val binaryWeight =  2.0/checkAdj

            // Binary segment properties
            for (seg1 <- mesh.segments.shuffle; adj <- seg1.adjacencies.values.shuffle if seg1.index < adj.neighbor.index)
            {
                val seg2 = adj.neighbor
                val fvec = Segment.getBinaryRegressionFeatures(seg1, adj)
                for (prop <- binarySegProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg1.group.color.observedColor,seg2.group.color.observedColor), fvec._1, fvec._2, binaryWeight) }
            }

            // Group properties
            val groupWeight = if (params.weightGroups) 2.0/mesh.groups.length else 1.0
            for (group <- mesh.groups.shuffle)
            {
                val fvec = SegmentGroup.getRegressionFeatures(group)
                for (prop <- groupProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(group.color.observedColor), fvec._1, fvec._2, groupWeight)}

                //weight marginals by their relative group size
                for (prop <- groupMarginalProps) {prop.examples += HistogramRegressor.RegressionExample(prop.extractor(group.color.observedColor), Tensor1(1), Array("constant"), groupWeight)}//, group.size )}   //for marginals, the only predictor is a constant...
            }

        }

        /** Construct model **/
        val model = new ColorInferenceModel
        println("Training Unary Segment Templates...")
        val loadDir = if (params.loadRegressorsIfPossible) params.modelSaveDirectory else ""
        for (i <- 0 until unarySegProps.length)
        {
            val template = params.colorVarParams.newUnarySegmentTemplate(unarySegProps(i), loadDir)
            model += template
        }
        DebugRuntime.printStats
        println("Training Binary Segment Templates...")
        for (i <- 0 until binarySegProps.length)
        {
            val template = params.colorVarParams.newBinarySegmentTemplate(binarySegProps(i), loadDir)
            model += template
        }
        DebugRuntime.printStats
        println("Training Group Templates...")
        for (i <- 0 until groupProps.length)
        {
            val template = params.colorVarParams.newGroupTemplate(groupProps(i), loadDir, params.weightGroups)
            model += template
        }

        println("Training Group Marginal Templates...")
        for (i <- 0 until groupMarginalProps.length)
        {
          val template = params.colorVarParams.newGroupTemplate(groupMarginalProps(i), loadDir, params.weightGroups)
            model += template
        }
        // Include the color compatibility term?
        if (params.includeColorCompatibilityTerm && params.colorVarParams.supportsColorCompatibility)
        {
            println("Adding color compatibility factor...")
            model += new ColorCompatibilityTemplate
        }

        // Save stuff?
        if (params.saveRegressorsIfPossible || params.saveWeightsIfPossible)
        {
            // Ensure directory exists
            val dir = new File(params.modelSaveDirectory)
            if (!dir.exists)
                dir.mkdir
        }

        if (params.saveRegressorsIfPossible)
        {
            println("Saving regressors...")
            model.regressionBasedComps.foreach(_.saveRegressorIfPossible(params.modelSaveDirectory))
        }

        // Load weights?
        var allLoaded = true
        if (params.loadWeightsIfPossible)
        {
            println("Attempting to load weights...")
            for (t <- model.trainables) allLoaded &= t.loadWeight(params.modelSaveDirectory)
        }
        if (!allLoaded) println("Not all weights successfully loaded")

        if (params.loadWeightsIfPossible && allLoaded && params.doWeightTuning)
            println("Skipping weight tuning because all weights were successfully loaded")
        else if (params.doWeightTuning)
        {
            /** Train weights of the model **/
            println("Tuning weights...")
            params.trainerType match
            {
                case params.TrainerType.SampleRank =>
                    TuneWeightsSampleRank(model, shuffledTrainingMeshes, params.numWeightTuningIterations)
                case params.TrainerType.MaximumLikelihood if params.colorVarParams.supportsMaxLikelihood =>
                    TuneWeightsMaxLikelihood(model, shuffledTrainingMeshes, params.numWeightTuningIterations)
                case params.TrainerType.ContrastiveDivergence =>
                    TuneWeightsContrastiveDivergence(model, shuffledTrainingMeshes, params.numWeightTuningIterations, params.cdK)
                case _ => throw new Error("No valid trainer type!")
            }
        }

        // print the weights
        println("Weights:")
        for (t <- model.trainables)
        {
            println(t.name + " : " + t.getWeight)
        }
        println()

        if (params.saveWeightsIfPossible)
        {
            println("Saving weights...")
            model.trainables.foreach(_.saveWeight(params.modelSaveDirectory))
        }

        model
    }


    def TuneWeightsSampleRank(model:ColorInferenceModel, trainingMeshes:IndexedSeq[SegmentMesh], iterations:Int)
    {
      //TODO: For sample rank, may need to add more constraints. i.e. constraining the original color of one (or more) of the color groups

      println("Tuning weights Sample Rank...")
      val objective = new AssignmentScoreTemplate()
      val trainer = new SampleRank(params.colorVarParams.newInferenceSampler(model, objective, params), new StepwiseGradientAscent)

//      model.conditionOnAll(trainingMeshes)
//      objective.conditionOnAll(trainingMeshes)

      var prevWeights = model.getWeights
      for (i <- 0 until iterations)
      {
        var avgAccuracy = 0.0
        for (mesh <- trainingMeshes)
        {
          //set the pattern domain
          params.colorVarParams.initDomain(mesh)

          model.conditionOn(mesh)
          objective.conditionOn(mesh)

          // Set the initial state of the mesh's color variables to be the observed colors
          mesh.setVariableValuesToObserved()

          //process the variables and learn the weights
          val vars = mesh.groups.map(g => g.color.asInstanceOf[params.VariableType])

          trainer.process(vars, 1)

          //print the accuracy
          //println("Iteration "+i+" Training Accuracy: " + objective.accuracy(vars))

          avgAccuracy += objective.accuracy(vars)
          print(".")
        }

        //print change in weights
        val curWeights = model.getWeights
        println("\nWeights delta: " + (curWeights-prevWeights).twoNorm)
        prevWeights = curWeights

        println("Iteration " + i+ " Overall Training Accuracy: " + avgAccuracy/trainingMeshes.length)
      }
    }

  //TODO: This is broken right now...
    def TuneWeightsMaxLikelihood(model:ColorInferenceModel, trainingMeshes:IndexedSeq[SegmentMesh], iterations:Int)
    {
      println("Tuning weights Max Likelihood...")

      val trainer = new BatchTrainer(new StepwiseGradientAscent, model)

      //model.conditionOnAll(trainingMeshes)

      var prevWeights = model.getWeights
      for (i <- 0 until iterations)
      {
        var avgLikelihood = 0.0    //well, this might not be comparable across meshes...
        for (mesh <- trainingMeshes)
        {
          params.colorVarParams.initDomain(mesh)

            model.conditionOn(mesh)

          //in this case, there's only one example...the observed original assignment
          //TODO: ideally, we might want this to use Loopy BP, but there seems to be a problem with our color variables having a third refvariable and BP requiring it to be a DiscreteTensorVar
          //TODO: alternatively (and probably easier), we can calculate log Z and the marginal statistics (whatever those are) by exhaustive inference

          //val examples = Array(new MaxLikelihoodExample(mesh.groups.map(g=>g.color.asInstanceOf[DiscreteColorVariable]), InferByBPLoopy))
          val examples = Array(new ModifiedMaxLikelihoodExample(mesh.groups.map(g=>g.color.asInstanceOf[DiscreteColorVariable]), mesh))

          trainer.processAll(examples)

          val likelihood = trainer.valueAccumulator.value
          avgLikelihood += likelihood

          print(".")
        }

        //print change in weights
        val curWeights = model.getWeights
        println("\nWeights delta: " + (curWeights-prevWeights).twoNorm)
        prevWeights = curWeights

        println("Iteration "+i+" Avg. Likelihood " + avgLikelihood/trainingMeshes.length)
      }

    }

    def TuneWeightsContrastiveDivergence(model:ColorInferenceModel, trainingMeshes:IndexedSeq[SegmentMesh], iterations:Int, cdK:Int)
    {
        println("Tuning weights by Contrastive Divergence...")

        // Just so randomness is a bit more random, for testing
        setRandomSeed(compat.Platform.currentTime)

        val trainer = params.colorVarParams.newTrainingSampler(model, params)

        if (params.normalizeWeights)
            model.normalizeWeights()

        // Split the data into training and validation, so we can monitor overfitting.
        val shuffledMeshes = trainingMeshes.shuffle
        val splitPoint = ((1.0 - params.weightTuningValidationSetSize)*shuffledMeshes.length).toInt
        val trainingMeshList = shuffledMeshes.slice(0, splitPoint)
        val validationMeshList = shuffledMeshes.slice(splitPoint+1, shuffledMeshes.length)
        val trainingRepresentativeMeshList = trainingMeshList.take(validationMeshList.length)

        // This only works with discrete variables
        def logZ(meshes:Seq[SegmentMesh]) : Double =
        {
            meshes.map(mesh =>
            {
                params.colorVarParams.initDomain(mesh)
                model.conditionOn(mesh)
                mesh.setVariableValuesToObserved()
                ExhaustiveInference.logZAllPermutations(mesh, model)
            }).sum
        }

        // Checking likelihoods every so often
        val batchesBetweenLikelihoodChecks = 200
        var batchCount = 0

        def overfitCheck() : Double =
        {
            val avgTrainingScore = trainingRepresentativeMeshList.map(mesh =>
            {
                params.colorVarParams.initDomain(mesh)
                model.conditionOn(mesh)
                mesh.setVariableValuesToObserved()
                model.currentScore(mesh.variablesAs[params.VariableType])
            }).sum / trainingRepresentativeMeshList.length

            val avgValidationScore = validationMeshList.map(mesh =>
            {
                params.colorVarParams.initDomain(mesh)
                model.conditionOn(mesh)
                mesh.setVariableValuesToObserved()
                model.currentScore(mesh.variablesAs[params.VariableType])
            }).sum / validationMeshList.length

            if (trainingMeshes.head.groups.head.color.isInstanceOf[DiscreteColorVariable]
                && (batchCount % batchesBetweenLikelihoodChecks == 0))
            {
                val trainingLogZ = logZ(trainingRepresentativeMeshList)
                val validationLogZ = logZ(validationMeshList)
                val trainingLL = avgTrainingScore*trainingRepresentativeMeshList.length - trainingLogZ
                val validationLL = avgValidationScore*validationMeshList.length - validationLogZ
                println("Training log likelihood: %g | Validation log likelihood: %g".format(trainingLL, validationLL))
            }
            val likelihoodRatio = math.exp(avgTrainingScore - avgValidationScore)
            println("Average training score: %g | Average validation score: %g | Likelihood ratio: %g".format(avgTrainingScore, avgValidationScore, likelihoodRatio))
            likelihoodRatio
        }

        // Maintain a buffer of the weight vectors from the last few batches
        var weightBuffer = new ArrayBuffer[Tensor1]

        // Get the initial overfitting bias (this should be close to 1.0)
        // We will run training until the overfitting bias gets larger than this initial bias
        // by a fixed threshold
        val initialOverfitBias = overfitCheck()

        // Iterate over the whole training set multiple times
        val trainMeshes = trainingMeshList.shuffle
        var batchesSinceLastOverfitCheck = 0
        var nextMesh = 0
        while (true)
        {
            // Process the training data in small 'mini-batches'
            val meshesPerBatch = params.weightTuningMiniBatchSize
            val batchWeight = 1.0 / meshesPerBatch

            // We only update the weights after processing an entire mini-batch, and we update
            // them by the average gradient over the whole mini-batch.
            println("Mini-batch %d".format(batchCount+1))
            val weightGradient = Tensor1(Array.fill(model.trainables.length)(0.0):_*)
            //for (m <- b*meshesPerBatch until (b+1)*meshesPerBatch if m < trainMeshes.length)
            for (m <- 0 until meshesPerBatch)
            {
                println("Processing mesh %d/%d".format(nextMesh, trainMeshes.length))
                val mesh = trainMeshes(nextMesh)
                nextMesh = (nextMesh + 1) % trainMeshes.length

                params.colorVarParams.initDomain(mesh)

                model.conditionOn(mesh)

                trainer.reset()
                // Set the initial state of the mesh's color variables to be the observed colors
                mesh.setVariableValuesToObserved()

                // Run the MCMC sampling chain for k steps, which will invoke the CD parameter update
                val oldweights = model.getWeights
                trainer.process(mesh.variablesAs[params.VariableType], cdK)
                weightGradient += ((model.getWeights - oldweights) * batchWeight)
                model.setWeights(oldweights)
            }

            // The learning rate is set so that the weight update is approximately
            // params.weightTuningWeightUpdateMagnitude times the size of the weights
            // This is a rule of thumb to enforce that we don't make updates too big.
            val updateSize = params.weightTuningWeightUpdateMagnitude * model.getWeights.twoNorm
            weightGradient.twoNormalize(); weightGradient *= updateSize

            // Update the weights and print out what they've become
            val newweights = model.getWeights
            newweights += weightGradient
            model.setWeights(newweights)

            if (params.normalizeWeights)
                model.normalizeWeights()

            println("Weights delta: %g".format((newweights - model.getWeights).twoNorm))
            println("Weights:")
            for (t <- model.trainables)
            {
                println(t.name + " : " + t.getWeight)
            }
            println()

            // Update the weight buffer (evicting if necessary)
            if (weightBuffer.length == params.weightTuningBatchesBetweenOverfitCheck)
                weightBuffer = weightBuffer.drop(1)
            weightBuffer += newweights

            batchCount += 1
            batchesSinceLastOverfitCheck += 1
            if (batchesSinceLastOverfitCheck == params.weightTuningBatchesBetweenOverfitCheck)
            {
                batchesSinceLastOverfitCheck = 0

                // Compare the average training set score to the average validation set score
                // If the training score is significantly higher than the validation score, we
                // are probably starting to overfit.
                println("Checking for overfitting...")
                val overfitBias = overfitCheck()
                if (overfitBias - initialOverfitBias > params.weightTuningOverfitThreshold)
                {
                    // If we are overfitting, then average out the weights from the last few batches
                    // and quit learning
                    println("FAILED; stopping training")
                    val finalweights = Tensor1(Array.fill(model.trainables.length)(0.0):_*)
                    for (w <- weightBuffer) finalweights += w
                    finalweights /= weightBuffer.length
                    model.setWeights(finalweights)
                    return
                }

                println("PASSED; continuing training")
            }
        }
    }
}


//TODO: check if this is correct
object InferByBPLoopy extends InferByBP {
  override def infer(variables:Iterable[Variable], model:Model, summary:Summary[Marginal] = null): Option[BPSummary] = variables match {
    case variables:Iterable[DiscreteVar] if (variables.forall(_.isInstanceOf[DiscreteVar])) => Some(apply(variables.toSet, model))
  }
  def apply(varying:Set[DiscreteVar], model:Model): BPSummary = {
    val summary = BPSummary(varying, model)
  //TODO: figure out how many iterations
    BP.inferLoopy(summary)
    summary
  }

}


class ModifiedMaxLikelihoodExample(labels:Iterable[DiscreteColorVariable], mesh:SegmentMesh) extends Example[Model] {
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator): Unit = {
    if (labels.size == 0) return

    //get logZ, by enumerating all permutations (TODO: or should we enumerate combinations)
    val logZ = ExhaustiveInference.logZAllPermutations(mesh, model)

    if (value != null)
      value.accumulate(model.assignmentScore(labels, TargetAssignment) - logZ)

    if (gradient != null) {
      model.factorsOfFamilyClass[DotFamily](labels, classOf[DotFamily]).foreach(factor => {
        gradient.accumulate(factor.family, factor.assignmentStatistics(TargetAssignment))
        //gradient.accumulate(factor.family, summary.marginalTensorStatistics(factor), -1.0)     // TODO: need to calculate the marginal tensor statistics....
      })
    }
  }
}

/** Template that scores how close a variable's color is to the original.**/
object AssignmentScoreTemplate
{
    type DatumVariable = RefVariable[ColorVariable]
    type Data = HashMap[ColorVariable, DatumVariable]
}
class AssignmentScoreTemplate extends Template2[ColorVariable, AssignmentScoreTemplate.DatumVariable]
{
    import AssignmentScoreTemplate._
    protected val data = new Data

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
        data.clear()
        for (mesh<-meshes; g<-mesh.groups)
            data(g.color) = new DatumVariable(g.color)
    }

    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (g <- mesh.groups)
        {
            data(g.color) = new DatumVariable(g.color)
        }
    }

    override def score(val1:ColorVariable#Value, val2:DatumVariable#Value):Double=
    {
        -1.0*(Color.perceptualDifference(val2.getColor, val2.observedColor)/100.0)*val2.group.size
    }

    def unroll1(v1:ColorVariable) =
    {
        Factor(v1.group.color, data(v1.group.color))
    }

    def unroll2(v2:DatumVariable) =
    {
        throw new Error("Cannot unroll target variable!")
    }

    def accuracy(context: Iterable[ColorVariable]): Double = context.map(currentScore(_)).sum// / context.size

}


