package colorinference

import cc.factorie.la._
import collection.mutable.{HashMap, ArrayBuffer}
import cc.factorie._
import cc.factorie.optimize._
import collection.{mutable, Set}
import util.DoubleAccumulator

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
    type RegressionFunction = (Seq[HistogramRegressor.RegressionExample], MathUtils.DistanceMetric, VectorQuantizer, WekaHistogramRegressor) => HistogramRegressor
    var regression:RegressionFunction = HistogramRegressor.LogisticRegression

    //include the segment factors?
    var includeUnaryTerms = false

    // This is only possible if we're doing continuous variables, though
    var includeColorCompatibilityTerm = false

    //ignore the noise segments?
    //var filterNoise = false

    //threshold at which to cap the distance
    //var perceptualDistThresh = Double.PositiveInfinity

    // Which trainer to use?
    object TrainerType extends Enumeration
    {
        type TrainerType = Value
        val SampleRank, MaximumLikelihood, ContrastiveDivergence = Value
    }
    var trainerType = TrainerType.ContrastiveDivergence

    var numWeightTuningIterations = 10

    // MH Sampling / Contrastive divergence params
    var cdK = 1
    var initialLearningRate = 1.0
    var minRadius:Double = 0.01
    var maxRadius:Double = 0.33
    var minSwapProb:Double = 0.05
    var maxSwapProb:Double = 0.5

    // Which variable type are we using?
    type VariableType <: ColorVariable
    def colorVarParams:ColorVariableParams[VariableType]
}

/* Different variable types require different model building/training operations */
trait ColorVariableParams[V<:ColorVariable]
{
    type SamplingContextType = IndexedSeq[V]
    def variableGenerator:ColorVariableGenerator
    def newUnarySegmentTemplate(prop:ModelTraining#UnarySegmentProperty):UnarySegmentTemplate[V]
    def newBinarySegmentTemplate(prop:ModelTraining#BinarySegmentProperty):BinarySegmentTemplate[V]
    def newGroupTemplate(prop:ModelTraining#ColorGroupProperty):ColorGroupTemplate[V]
    def newInferenceSampler(model:Model, objective:Model, params:ModelTrainingParams):MHSampler[SamplingContextType]
    def newTrainingSampler(model:Model, params:ModelTrainingParams):KStepContrastiveDivergence[SamplingContextType]
    def initDomain(mesh:SegmentMesh)
    def supportsMaxLikelihood:Boolean
    def supportsColorCompatibility:Boolean
}
object DiscreteColorVariableParams extends ColorVariableParams[DiscreteColorVariable]
{
    def variableGenerator = DiscreteColorVariable
    def newUnarySegmentTemplate(prop:ModelTraining#UnarySegmentProperty) = new DiscreteUnarySegmentTemplate(prop)
    def newBinarySegmentTemplate(prop:ModelTraining#BinarySegmentProperty) = new DiscreteBinarySegmentTemplate(prop)
    def newGroupTemplate(prop:ModelTraining#ColorGroupProperty) = new DiscreteColorGroupTemplate(prop)
    def newInferenceSampler(model:Model, objective:Model, params:ModelTrainingParams) = new DiscreteColorSampler(model, objective)
    def newTrainingSampler(model:Model, params:ModelTrainingParams) = new DiscreteColorTrainingSampler(model, params.cdK)
    def initDomain(mesh:SegmentMesh)
    {
        val palette = ColorPalette(mesh);
        DiscreteColorVariable.initDomain(palette)
        for (color <- palette) color.convertTo(LABColorSpace)   // Since most features are in LAB
    }
    def supportsMaxLikelihood:Boolean = true
    def supportsColorCompatibility:Boolean = false
}
object ContinuousColorVariableParams extends ColorVariableParams[ContinuousColorVariable]
{
    def variableGenerator = ContinuousColorVariable
    def newUnarySegmentTemplate(prop:ModelTraining#UnarySegmentProperty) = new ContinuousUnarySegmentTemplate(prop)
    def newBinarySegmentTemplate(prop:ModelTraining#BinarySegmentProperty) = new ContinuousBinarySegmentTemplate(prop)
    def newGroupTemplate(prop:ModelTraining#ColorGroupProperty) = new ContinuousColorGroupTemplate(prop)
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

    /* Color properties */

    // Unary
    def colorfulness(c:Color) = Tensor1(c.colorfulness)
    def lightness(c:Color) = Tensor1(c.copyIfNeededTo(LABColorSpace)(0))
    def nameSaliency(c:Color) = Tensor1(namingModel.saliency(c))

    // Binary
    def perceptualDifference(c1:Color, c2:Color) = Tensor1(Color.perceptualDifference(c1, c2))
    def chromaDifference(c1:Color, c2:Color) = Tensor1(Color.chromaDifference(c1, c2))
    def relativeColorfulness(c1:Color, c2:Color) = Tensor1(Color.relativeColorfulness(c1, c2))
    def relativeLightness(c1:Color, c2:Color) = Tensor1(Color.relativeLightness(c1, c2))
    def nameSimilarity(c1:Color, c2:Color) = Tensor1(namingModel.cosineSimilarity(c1, c2))


    /* Quantizers */
    val uniformQuant10 = new UniformVectorQuantizer(Array(10))

    def apply(trainingMeshes:IndexedSeq[SegmentMesh], params:ModelTrainingParams) : ColorInferenceModel =
    {
        val training = new ModelTraining(params)
        training.train(trainingMeshes)
    }
}

class ModelTraining(val params:ModelTrainingParams)
{
    type Examples = ArrayBuffer[HistogramRegressor.RegressionExample]
    case class UnarySegmentProperty(name:String, extractor:UnarySegmentTemplate.ColorPropertyExtractor, quant:VectorQuantizer)
    {
        val regression = params.regression
        val examples = new Examples
    }
    case class BinarySegmentProperty(name:String, extractor:BinarySegmentTemplate.ColorPropertyExtractor, quant:VectorQuantizer)
    {
        val regression = params.regression
        val examples = new Examples
    }
    case class ColorGroupProperty(name:String, extractor:ColorGroupTemplate.ColorPropertyExtractor, quant:VectorQuantizer)
    {
        val regression = params.regression
        val examples = new Examples
    }

    /* Unary segment properties */
    val unarySegProps = new ArrayBuffer[UnarySegmentProperty]()
    if (params.includeUnaryTerms)
    {
      unarySegProps += UnarySegmentProperty("Lightness", ModelTraining.lightness, ModelTraining.uniformQuant10)
      unarySegProps += UnarySegmentProperty("Colorfulness", ModelTraining.colorfulness, ModelTraining.uniformQuant10)
      unarySegProps += UnarySegmentProperty("Name Saliency", ModelTraining.nameSaliency, ModelTraining.uniformQuant10)
    }

    /* Binary segment properties */
    // The assumption for the binary properties thus far is that they're symmetric (no directionality between the variables), which is probably ok
    val binarySegProps = new ArrayBuffer[BinarySegmentProperty]()
    binarySegProps += BinarySegmentProperty("Perceptual Difference", ModelTraining.perceptualDifference, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Chroma Difference", ModelTraining.chromaDifference, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Relative Colorfulness", ModelTraining.relativeColorfulness, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Relative Lightness", ModelTraining.relativeLightness, ModelTraining.uniformQuant10)
    binarySegProps += BinarySegmentProperty("Name Similarity", ModelTraining.nameSimilarity, ModelTraining.uniformQuant10)

    /* Color group properties */
    val groupProps = new ArrayBuffer[ColorGroupProperty]()
    groupProps += ColorGroupProperty("Lightness", ModelTraining.lightness, ModelTraining.uniformQuant10)
    groupProps += ColorGroupProperty("Colorfulness", ModelTraining.colorfulness, ModelTraining.uniformQuant10)
    groupProps += ColorGroupProperty("Name Saliency", ModelTraining.nameSaliency, ModelTraining.uniformQuant10)

    def train(trainingMeshes:IndexedSeq[SegmentMesh]) : ColorInferenceModel =
    {
        /** Extract training data points from meshes **/

        // Training meshes with more segments generate more samples. Here we eliminate that bias
        // repeating the examples according to the lcm doesn't work...as the lcm turns out to be big, and we run out of heap space
        // so we'll weight each example according to 1/numSegments or 1/numAdjacencies. Scale by 2, so we don't run into rounding errors (when Weka checks that weights add up to >=1)
        for (mesh <- trainingMeshes)
        {
            val unaryWeight = 2.0/mesh.segments.length

            // Unary segment properties
            for (seg <- mesh.segments)
            {
                val fvec = Segment.getUnaryRegressionFeatures(seg)
                for (prop <- unarySegProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg.group.color.observedColor), fvec, unaryWeight) }
            }

            var checkAdj = 0
            for (seg1<-mesh.segments; seg2 <- seg1.adjacencies.map(a=>a.neighbor) if seg1.index < seg2.index)
                checkAdj+=1

            val binaryWeight =  2.0/checkAdj

            // Binary segment properties
            for (seg1 <- mesh.segments; adj <- seg1.adjacencies if seg1.index < adj.neighbor.index)
            {
                val seg2 = adj.neighbor
                val fvec = Segment.getBinaryRegressionFeatures(seg1, adj)
                for (prop <- binarySegProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(seg1.group.color.observedColor,seg2.group.color.observedColor), fvec, binaryWeight) }
            }

            // Group properties
            // TODO: Should these training examples be weighted like the ones above? I think it's probably unnecessary.
            for (group <- mesh.groups)
            {
                val fvec = SegmentGroup.getRegressionFeatures(group)
                for (prop <- groupProps) { prop.examples += HistogramRegressor.RegressionExample(prop.extractor(group.color.observedColor), fvec)}
            }
        }

        /** Construct model **/
        println("Training Unary Segment Templates...")
        val spatialModel = new TemplateColorInferenceModel
        for (i <- 0 until unarySegProps.length)
        {
            val template = params.colorVarParams.newUnarySegmentTemplate(unarySegProps(i))
            spatialModel += template
        }
        println("Training Binary Segment Templates...")
        for (i <- 0 until binarySegProps.length)
        {
            val template = params.colorVarParams.newBinarySegmentTemplate(binarySegProps(i))
            spatialModel += template
        }
        println("Training Group Templates...")
        for (i <- 0 until groupProps.length)
        {
            val template = params.colorVarParams.newGroupTemplate(groupProps(i))
            spatialModel += template
        }
        val model = new CombinedColorInferenceModel(spatialModel)
        // Include the color compatibility term?
        if (params.includeColorCompatibilityTerm && params.colorVarParams.supportsColorCompatibility)
        {
            val cfam = new ColorCompatibilityFamily
            val cfac = new cfam.Factor
            val cmodel = new ItemizedColorInferenceModel(cfac.asInstanceOf[Factor])
            model += cmodel
        }

        /** Train weights of the model **/
        println("Learning Weights...")
        params.trainerType match
        {
            case params.TrainerType.SampleRank =>
                TuneWeightsSampleRank(model, trainingMeshes, params.numWeightTuningIterations)
            case params.TrainerType.MaximumLikelihood if params.colorVarParams.supportsMaxLikelihood =>
                TuneWeightsMaxLikelihood(model, trainingMeshes, params.numWeightTuningIterations)
            case params.TrainerType.ContrastiveDivergence =>
                TuneWeightsContrastiveDivergence(model, trainingMeshes, params.numWeightTuningIterations, params.cdK)
            case _ => throw new Error("No valid trainer type!")
        }

        // print the weights
        println("Weights:")
        for (t <- model.trainables)
        {
            println(t.name + " : " + t.getWeight)
        }
        println()

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

      var prevWeights = model.trainableWeights
      for (i <- 0 until iterations)
      {
        var avgAccuracy = 0.0
        for (mesh <- trainingMeshes)
        {
          //set the pattern domain
          params.colorVarParams.initDomain(mesh)

          model.conditionOn(mesh)
          objective.conditionOn(mesh)

          //process the variables and learn the weights
          val vars = mesh.groups.map(g => g.color.asInstanceOf[params.VariableType])

          trainer.process(vars, 1)

          //print the accuracy
          //println("Iteration "+i+" Training Accuracy: " + objective.accuracy(vars))

          avgAccuracy += objective.accuracy(vars)
          print(".")
        }

        //print change in weights
        val curWeights = model.trainableWeights
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

      var prevWeights = model.trainableWeights
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
        val curWeights = model.trainableWeights
        println("\nWeights delta: " + (curWeights-prevWeights).twoNorm)
        prevWeights = curWeights

        println("Iteration "+i+" Avg. Likelihood " + avgLikelihood/trainingMeshes.length)
      }

    }

    def TuneWeightsContrastiveDivergence(model:ColorInferenceModel, trainingMeshes:IndexedSeq[SegmentMesh], iterations:Int, cdK:Int)
    {
        println("Tuning weights by Contrastive Divergence...")

        val trainer = params.colorVarParams.newTrainingSampler(model, params)
        //model.conditionOnAll(trainingMeshes)
        var prevWeights = model.trainableWeights

//        // Initial average likelihood
//        var initLL = 0.0
//        for (mesh <- trainingMeshes)
//        {
//            params.colorVarParams.initDomain(mesh)
//            model.conditionOn(mesh)
//            mesh.setVariableValuesToObserved()
//            initLL += (model.currentScore(mesh.variablesAs[params.VariableType]) - ExhaustiveInference.logZAllPermutations(mesh, model))
//        }
//        println("Initial Log likelihood: " + initLL)

        // Iterate over the whole training set multiple times
        for (i <- 0 until iterations)
        {
            // Lower the learning rate as the iterations go on.
            val t = i/(iterations.toDouble)
            trainer.learningRate = (1-t)*params.initialLearningRate

            println("Iteration %d/%d".format(i+1, iterations))
            for (m <- 0 until trainingMeshes.length)
            {
                val mesh = trainingMeshes(m)
                println("Processing mesh %d/%d".format(m+1, trainingMeshes.length))

                params.colorVarParams.initDomain(mesh)

                model.conditionOn(mesh)

                trainer.reset()
                // Set the initial state of the mesh's color variables to be the observed colors
                mesh.setVariableValuesToObserved()
                // Run the MCMC sampling chain for k steps, which will invoke the CD parameter update
                trainer.process(mesh.variablesAs[params.VariableType], cdK)
            }

//            var ll = 0.0
//            for (mesh <- trainingMeshes)
//            {
//                params.colorVarParams.initDomain(mesh)
//                model.conditionOn(mesh)
//                mesh.setVariableValuesToObserved()
//                ll += (model.currentScore(mesh.variablesAs[params.VariableType]) - ExhaustiveInference.logZAllPermutations(mesh, model))
//            }
//            println("Iteration "+(i+1)+" Log Likelihood " + ll)

            val curWeights = model.trainableWeights
            println("\nWeights delta: " + (curWeights-prevWeights).twoNorm)
            prevWeights = curWeights
        }

//        var finalLL = 0.0
//        for (mesh <- trainingMeshes)
//        {
//            params.colorVarParams.initDomain(mesh)
//            model.conditionOn(mesh)
//            mesh.setVariableValuesToObserved()
//            finalLL += (model.currentScore(mesh.variablesAs[params.VariableType]) - ExhaustiveInference.logZAllPermutations(mesh, model))
//        }
//        println("Final Log Likelihood " + finalLL)
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


