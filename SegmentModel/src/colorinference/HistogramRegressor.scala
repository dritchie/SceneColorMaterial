package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/15/12
 * Time: 11:22 AM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.Tensor1
import jnisvmlight._
import weka.classifiers.`lazy`.{LWL, IBk}
import weka.classifiers.Classifier
import weka.classifiers.functions.supportVector.RBFKernel
import weka.classifiers.functions.{SMO, Logistic}
import weka.core._
import neighboursearch.KDTree

object HistogramRegressor
{
    case class RegressionExample(target:Tensor1, features:Tensor1, weight:Double=1) {}

    def KNN(examples:Seq[HistogramRegressor.RegressionExample], metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer, generator:WekaHistogramRegressor) =
    {
        val classifier = new IBk()
        classifier.setDistanceWeighting(new SelectedTag(IBk.WEIGHT_INVERSE, IBk.TAGS_WEIGHTING))
        classifier.setKNN(100)
        classifier.setMeanSquared(true)
        classifier.setNearestNeighbourSearchAlgorithm(new KDTree())

        generator(classifier, examples, metric, quantizer)
    }

    def LogisticRegression(examples:Seq[HistogramRegressor.RegressionExample], metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer, generator:WekaHistogramRegressor) =
    {
        val classifier = new Logistic()

        generator(classifier, examples, metric, quantizer)
    }

    def LWLR(examples:Seq[HistogramRegressor.RegressionExample], metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer, generator:WekaHistogramRegressor) =
    {
        val classifier = new Logistic()
        val enhancer = new LWL()
        enhancer.setClassifier(classifier)
        enhancer.setNearestNeighbourSearchAlgorithm(new KDTree())
        enhancer.setKNN(100)

        generator(enhancer, examples, metric, quantizer)
    }

    def SVM(examples:Seq[HistogramRegressor.RegressionExample], metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer, generator:WekaHistogramRegressor) =
    {
        val classifier = new SMO()
        classifier.setNumFolds(10)
        classifier.setFilterType(new SelectedTag(SMO.FILTER_STANDARDIZE, SMO.TAGS_FILTER))
        val kernel = new RBFKernel()
        classifier.setKernel(kernel)

        generator(classifier, examples, metric, quantizer)
    }
}

abstract class HistogramRegressor(examples:Seq[HistogramRegressor.RegressionExample], private val metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer)
{
    protected val centroids:IndexedSeq[Tensor1] = getCentroids(examples, quantizer)

    private def getCentroids(examples:Seq[HistogramRegressor.RegressionExample], quantizer:VectorQuantizer) : IndexedSeq[Tensor1] =
    {
        assert(examples.length > 0, {println("HistogramRegressor: cannot train with 0 training examples")})
        println("Quantizing...")
        (quantizer(for (ex <- examples) yield ex.target, metric, for(ex <- examples) yield ex.weight))._1
    }

    protected def fillBins(featureVec:Tensor1, bins:Array[Double])

    def predictHistogram(featureVec:Tensor1) : VectorHistogram =
    {
        val bins = new Array[Double](centroids.length)
        fillBins(featureVec, bins)

        // Normalize bins
        var totalmass = 0.0
        for (i <- 0 until bins.length)
            totalmass += bins(i)
        if (totalmass > 0.0)
            for (i <- 0 until bins.length) bins(i) /= totalmass

        // Construct histogram
        val hist = new VectorHistogram(metric)
        hist.setData(centroids, bins)
        hist
    }
}

/*
There are two posssible implmentations for Weka-based regressors:
 (1) Train a multi-class classifier where each bin is a class, then ask for the distribution over classes
 (2) Train one regressor for each bin
I don't know which one will work better, so it's probably best to try both and see.
 */

trait WekaHistogramRegressor
{
    def apply(classifier:Classifier, examples:Seq[HistogramRegressor.RegressionExample],
                          metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer) : HistogramRegressor
}

object WekaMultiClassHistogramRegressor extends WekaHistogramRegressor
{
    def apply(classifier:Classifier, examples:Seq[HistogramRegressor.RegressionExample],
                          metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer) = new WekaMultiClassHistogramRegressor(classifier, examples, metric, quantizer)
}

class WekaMultiClassHistogramRegressor(private val classifier:Classifier, examples:Seq[HistogramRegressor.RegressionExample], metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer)
    extends HistogramRegressor(examples, metric, quantizer)
{
    private val attributePrototype = buildAttributePrototype(examples)

    private val dummyDataset = new Instances("dummySet", attributePrototype, 0)
    dummyDataset.setClassIndex(attributePrototype.size - 1)

    train(examples)

    private def buildAttributePrototype(examples:Seq[HistogramRegressor.RegressionExample]) : FastVector =
    {
        val featureVec = examples(0).features
        val attribProto = new FastVector(featureVec.length+1)

        // Add feature attributes
        for (i <- 0 until featureVec.length)
            attribProto.addElement(new Attribute("numericAttrib"+i))

        // Add the class attribute
        val classValues = new FastVector(centroids.length)
        for (i <- 0 until centroids.length)
            classValues.addElement(i.toString)
        attribProto.addElement(new Attribute("class", classValues))

        attribProto
    }

    // If trueBin = -1, then this is an 'unlabeled' instance
    private def convertToWekaInstance(featureVec:Tensor1, weight:Double=1, trueBin:Int = -1) : Instance =
    {
        val instance = new Instance(attributePrototype.size)

        instance.setWeight(weight)

        // Set feature values
        for (i <- 0 until featureVec.length)
            instance.setValue(i, featureVec(i))

        // Set class value
        if (trueBin >= 0)
            instance.setValue(attributePrototype.size - 1, trueBin)

        instance
    }

    private def train(examples:Seq[HistogramRegressor.RegressionExample])
    {
        println("Training multi-class histogram regressor...")

        // Associate training examples with their closest bin
        val assignments = new Array[Int](examples.length)
        for (i <- 0 until examples.length)
            assignments(i) = MathUtils.closestVectorBruteForce(examples(i).target, centroids, metric)

        // Init Weka training set
        val trainingSet = new Instances("trainingSet", attributePrototype, examples.length)
        // Don't forget to tell it where in the attribute vector the class is!
        trainingSet.setClassIndex(attributePrototype.size - 1)

        // Populate training set
        for (i <- 0 until examples.length)
        {
            val instance = convertToWekaInstance(examples(i).features, examples(i).weight, assignments(i))
            trainingSet.add(instance)
        }

        // Train the model
        classifier.buildClassifier(trainingSet)
    }

    protected def fillBins(featureVec:Tensor1, bins:Array[Double])
    {
        // Convert feature vector to Weka format
        val instance = convertToWekaInstance(featureVec)

        // We have to associate the instance with a dataset in order for it
        // to know which attributes it has and which is the class attribute
        instance.setDataset(dummyDataset)

        // Ask classifier for distribution over classes
        val distrib = classifier.distributionForInstance(instance)
        Array.copy(distrib, 0, bins, 0, bins.length)
    }
}

object WekaBinByBinHistogramRegressor extends WekaHistogramRegressor
{
    def apply(classifier:Classifier, examples:Seq[HistogramRegressor.RegressionExample],
              metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer) = new WekaBinByBinHistogramRegressor(classifier, examples, metric, quantizer)
}

class WekaBinByBinHistogramRegressor(classifier:Classifier, examples:Seq[HistogramRegressor.RegressionExample], metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer)
    extends HistogramRegressor(examples, metric, quantizer)
{
    private val regressors = Classifier.makeCopies(classifier, centroids.length)

    private val attributePrototype = buildAttributePrototype(examples)

    private val dummyDataset = new Instances("dummySet", attributePrototype, 0)
    dummyDataset.setClassIndex(attributePrototype.size - 1)

    train(examples)

    private def buildAttributePrototype(examples:Seq[HistogramRegressor.RegressionExample]) : FastVector =
    {
        val featureVec = examples(0).features
        val attribProto = new FastVector(featureVec.length+1)

        // Add feature attributes
        for (i <- 0 until featureVec.length)
            attribProto.addElement(new Attribute("numericAttrib"+i))

        // Add the class attribute
        attribProto.addElement(new Attribute("class"))

        attribProto
    }

    // The last three parameters are only provided during training
    private def convertToWekaInstance(featureVec:Tensor1, weight:Double=1, trainingHist:VectorHistogram = null, bin:Int = -1, targetVec:Tensor1 = null) : Instance =
    {
        val instance = new Instance(attributePrototype.size)

        instance.setWeight(weight)

        // Set feature values
        for (i <- 0 until featureVec.length)
            instance.setValue(i, featureVec(i))

        // Set class value
        if (trainingHist != null)
            instance.setValue(attributePrototype.size - 1, trainingHist.evaluateBinAt(bin, targetVec))

        instance
    }

    private def train(examples:Seq[HistogramRegressor.RegressionExample])
    {
        println("Training bin-by-bin histogram regressor...")

        // Build a 'training histogram' so we can evaluate bin probabilities
        val trainingHist = new VectorHistogram(metric)
        trainingHist.setData(centroids, for (i <- 0 until centroids.length) yield 0.0)

        // Train each regressor
        for (b <- 0 until centroids.length)
        {
            println("Training bin " + b + "...")

            // Init Weka training set
            val trainingSet = new Instances("trainingSet", attributePrototype, examples.length)
            // Don't forget to tell it where in the attribute vector the class is!
            trainingSet.setClassIndex(attributePrototype.size - 1)
            // Populate training set
            for (i <- 0 until examples.length)
            {
                val instance = convertToWekaInstance(examples(i).features, examples(i).weight, trainingHist, b, examples(i).target)
                trainingSet.add(instance)
            }

            // Train the model
            regressors(b).buildClassifier(trainingSet)
        }
    }

    protected def fillBins(featureVec:Tensor1, bins:Array[Double])
    {
        // Convert feature vector to Weka format
        val instance = convertToWekaInstance(featureVec)

        // We have to associate the instance with a dataset in order for it
        // to know which attributes it has and which is the class attribute
        instance.setDataset(dummyDataset)

        // Evaluate each regressor at this instance
        for (b <- 0 until centroids.length)
            bins(b) = regressors(b).classifyInstance(instance)
    }
}




/*
Keeping this around for historical purposes
 */

class SVMLightHistogramRegressor(examples:Seq[HistogramRegressor.RegressionExample], metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer)
    extends HistogramRegressor(examples, metric, quantizer)
{
    private var regressors:Array[SVMLightModel] = null
    private var featureIndices:Array[Int] = null

    train(examples)

    private def train(examples:Seq[HistogramRegressor.RegressionExample])
    {
        val numBins = centroids.length
        featureIndices = (0 until examples(0).features.length).toArray

        // Set up the JNI interface to svmlight
        val trainer = new SVMLightInterface()
        //SVMLightInterface.SORT_INPUT_VECTORS = false    // Dimensions will already be sorted

        // Set up necessary training parameters
        val params = new TrainingParameters()
        //TODO: Changing this to Regression seems to make things very slow. Might be a parameter or training data issue?
        params.getLearningParameters.`type` = LearnParam.REGRESSION.toLong
        //params.getLearningParameters.svm_c = 1.0
        // TODO: Consider non-default (linear) kernels, such as RBF kernel?
        //params.getLearningParameters.verbosity = 1

        // Train 'em up!
        val trainingHist = new VectorHistogram(metric)
        trainingHist.setData(centroids, for (i <- 0 until centroids.length) yield 0.0)
        regressors = new Array[SVMLightModel](numBins)
        for (i <- 0 until numBins)
        {
            println("HistogramRegressor: Training bin " + i)
            // Fill in training data in the format expected by svmlight
            println("Formatting training data...")
            val trainingData = new Array[LabeledFeatureVector](examples.length)
            for (j <- 0 until examples.length)
            {
                val ex = examples(j)
                //val target = if (assignments(j) == i) 1.0 else 0.0
                val target = trainingHist.evaluateBinAt(i, ex.target)
                val dims = (0 until ex.features.length).map(x => x+1).toArray  //feature numbers have to be >=1
            val features = ex.features.toArray
                trainingData(j) = new LabeledFeatureVector(target, dims, features)
            }
            // TODO: Consider normalizing/standardizing the data so that linear kernel turns into cosine/mahalanobis distance?

            // Finally, train the SVM regressor
            println("Training SVM regressor...")
            regressors(i) = trainer.trainModel(trainingData, params)
        }
    }

    protected def fillBins(featureVec:Tensor1, bins:Array[Double])
    {
        val svmlightfeatures = new FeatureVector(featureIndices, featureVec.toArray)
        for (i <- 0 until bins.length)
            bins(i) = MathUtils.clamp(regressors(i).classify(svmlightfeatures), 0.0, 1.0)
    }
}
