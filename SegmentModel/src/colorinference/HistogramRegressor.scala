package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/15/12
 * Time: 11:22 AM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.{DenseTensor1, Tensor1}
import weka.classifiers.`lazy`.{LWL, IBk}
import weka.classifiers.Classifier
import weka.classifiers.functions.supportVector.RBFKernel
import weka.classifiers.functions.{SMO, Logistic}
import weka.core._
import neighboursearch.KDTree
import java.io.{FileNotFoundException, FileWriter}
import io.Source

object HistogramRegressor
{
    type RegressionFunction = (MathUtils.DistanceMetric, Double, WekaHistogramRegressor) => HistogramRegressor
    case class RegressionExample(target:Tensor1, features:Tensor1, featureNames:Array[String]=null, weight:Double=1) {}
    case class CrossValidationRanges(numBins:Seq[Int], bandScale:Seq[Double])

    def KNN(metric:MathUtils.DistanceMetric, bandwidthScale:Double, generator:WekaHistogramRegressor) =
    {
        val classifier = new IBk()
        classifier.setDistanceWeighting(new SelectedTag(IBk.WEIGHT_INVERSE, IBk.TAGS_WEIGHTING))
        classifier.setKNN(100)
        classifier.setMeanSquared(true)
        classifier.setNearestNeighbourSearchAlgorithm(new KDTree())

        generator(classifier, metric, bandwidthScale)
    }

    def LogisticRegression(metric:MathUtils.DistanceMetric, bandwidthScale:Double, generator:WekaHistogramRegressor) =
    {
        val classifier = new Logistic()

        generator(classifier, metric, bandwidthScale)
    }

    def LWLR(metric:MathUtils.DistanceMetric, bandwidthScale:Double, generator:WekaHistogramRegressor) =
    {
        val classifier = new Logistic()
        val enhancer = new LWL()
        enhancer.setClassifier(classifier)
        enhancer.setNearestNeighbourSearchAlgorithm(new KDTree())
        enhancer.setKNN(100)

        generator(enhancer, metric, bandwidthScale)
    }

    def SVM(metric:MathUtils.DistanceMetric, bandwidthScale:Double, generator:WekaHistogramRegressor) =
    {
        val classifier = new SMO()
        classifier.setNumFolds(10)
        classifier.setFilterType(new SelectedTag(SMO.FILTER_STANDARDIZE, SMO.TAGS_FILTER))
        val kernel = new RBFKernel()
        classifier.setKernel(kernel)

        generator(classifier, metric, bandwidthScale)
    }
}

abstract class HistogramRegressor(private val metric:MathUtils.DistanceMetric, var bandwidthScale:Double)
{
    protected var centroids:IndexedSeq[Tensor1] = null
    protected var featureNames:Seq[String] = null

    private def computeCentroids(examples:Seq[HistogramRegressor.RegressionExample], quantizer:VectorQuantizer) : IndexedSeq[Tensor1] =
    {
        assert(examples.length > 0, {println("HistogramRegressor: cannot train with 0 training examples")})
        //println("Quantizing...")
        (quantizer(for (ex <- examples) yield ex.target, metric, for(ex <- examples) yield ex.weight))._1
    }

    protected def fillBins(featureVec:Tensor1, bins:Array[Double])

    def load(examples:Seq[HistogramRegressor.RegressionExample], loadFrom:String) : Boolean =
    {
        featureNames = examples(0).featureNames
        loadBandwidthScale(loadFrom) &&
        loadCentroids(loadFrom) &&
        loadRegressor(examples, loadFrom)
    }

    def train(examples:Seq[HistogramRegressor.RegressionExample], quantizer:VectorQuantizer, loadFrom:String = "")
    {
        featureNames = examples(0).featureNames
        centroids = computeCentroids(examples, quantizer)
        trainRegressor(examples)
    }
    def trainRegressor(examples:Seq[HistogramRegressor.RegressionExample])

    def avgLogLikelihood(examples:Seq[HistogramRegressor.RegressionExample]) : Double =
    {
        val ll = examples.map(ex =>
        {
            val hist = predictHistogram(ex.features)
            val p = hist.evaluateAt(ex.target)
            MathUtils.safeLog(p)
        }).reduce(_ + _)

        ll / examples.length
    }

    def save(fileBaseName:String)
    {
        saveBandwithScale(fileBaseName)
        saveCentroids(fileBaseName)
        saveRegressor(fileBaseName)
    }

    protected def saveBandwithScale(fileBaseName:String)
    {
        val fw = new FileWriter(fileBaseName + ".bandscale")
        fw.write("%f\n".format(bandwidthScale))
        fw.close()
    }
    protected def loadBandwidthScale(fileBaseName:String) : Boolean =
    {
        print("Attemping to load bandwidth scale...")
        try
        {
            bandwidthScale = Source.fromFile(fileBaseName + ".bandscale").getLines().next().toDouble
        } catch { case e:FileNotFoundException => println("FAILED"); return false }
        println("SUCCESS")
        true
    }

    protected def saveCentroids(fileBaseName:String)
    {
        val fw = new FileWriter(fileBaseName + ".centroids")
        for (c <- centroids)
        {
            for (i <- 0 until c.length)
                fw.write("%f ".format(c(i)))
            fw.write("\n")
        }
        fw.close()
    }
    protected def loadCentroids(fileBaseName:String) : Boolean =
    {
        print("Attemping to load centroids...")
        try
        {
            centroids = Source.fromFile(fileBaseName + ".centroids").getLines().map(l => Tensor1(l.split(' ').map(_.toDouble):_*)).toArray[DenseTensor1]
        } catch { case e:FileNotFoundException => println("FAILED"); return false }
        println("SUCCESS")
        true
    }
    protected def saveRegressor(fileBaseName:String) {}
    protected def loadRegressor(examples:Seq[HistogramRegressor.RegressionExample], fileBaseName:String) : Boolean =  { false }

    def predictHistogram(featureVec:Tensor1) : VectorHistogram =
    {
        val bins = new Array[Double](centroids.length)
        fillBins(featureVec, bins)

        // Normalize bins (also do a sanity check)
        var totalmass = 0.0
        for (b <- bins)
        {
            assert(b == b, {println("HistogramRegressor - NaNs in predicted bins")})
            totalmass += b
        }
        if (totalmass > 0.0)
            for (i <- 0 until bins.length) bins(i) /= totalmass

        // Construct histogram
        val hist = new VectorHistogram(metric, bandwidthScale)
        hist.setData(centroids, bins)
        hist
    }

    def getCentroids:Seq[Tensor1] = centroids

    //assume feature names for all examples is the same, plus the extra class feature
    def getFeatureNames:Seq[String] = featureNames

    def getCoefficients:Array[Array[Double]]

}

/*
There are two posssible implmentations for Weka-based regressors:
 (1) Train a multi-class classifier where each bin is a class, then ask for the distribution over classes
 (2) Train one regressor for each bin
I don't know which one will work better, so it's probably best to try both and see.
 */

trait WekaHistogramRegressor
{
    def apply(classifier:Classifier, metric:MathUtils.DistanceMetric, bandwidthScale:Double) : HistogramRegressor
}

object WekaMultiClassHistogramRegressor extends WekaHistogramRegressor
{
    def apply(classifier:Classifier, metric:MathUtils.DistanceMetric, bandwidthScale:Double) =
        new WekaMultiClassHistogramRegressor(classifier, metric, bandwidthScale)
}

class WekaMultiClassHistogramRegressor(private var classifier:Classifier, metric:MathUtils.DistanceMetric, bandScale:Double)
    extends HistogramRegressor(metric, bandScale)
{
    private var attributePrototype:FastVector = null
    private var dummyDataset:Instances = null

    def copy =
    {
        val thiscopy = new WekaMultiClassHistogramRegressor(this.classifier, this.metric, this.bandwidthScale)
        thiscopy.copyFrom(this)
        thiscopy
    }

    def copyFrom(other:WekaMultiClassHistogramRegressor)
    {
        // Assumes 'metric' is the same
        this.centroids = other.centroids
        this.featureNames = other.featureNames
        this.attributePrototype = other.attributePrototype
        this.dummyDataset = other.dummyDataset
        this.bandwidthScale = other.bandwidthScale
        this.classifier = Classifier.makeCopy(other.classifier)
    }

    def getCoefficients:Array[Array[Double]] =
    {
      //get coefficients if applicable , second dimension is classes/centroids, first dimension is features
      classifier match {
        case logistic:Logistic => logistic.coefficients()
        case _ => {println("WekaMultiHistogramRegressor: getCoefficients hasn't been implemented for this classifier"); Array()} //otherwise, it doesn't apply...
      }
    }

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

    override def saveRegressor(fileBaseName:String)
    {
        // Save the classifier
        weka.core.SerializationHelper.write(fileBaseName+".regressor", classifier)
    }

    override def loadRegressor(examples:Seq[HistogramRegressor.RegressionExample], fileBaseName:String) : Boolean =
    {
        attributePrototype = buildAttributePrototype(examples)
        dummyDataset = new Instances("dummySet", attributePrototype, 0)
        dummyDataset.setClassIndex(attributePrototype.size - 1)

        print("Attempting to load regressor...")
        try
        {
            // Load classifier
            classifier = weka.core.SerializationHelper.read(fileBaseName+".regressor").asInstanceOf[Classifier]
        } catch { case e:Exception => println("FAILED"); return false }
        println("SUCCESS")
        true
    }

    def trainRegressor(examples:Seq[HistogramRegressor.RegressionExample])
    {
        //println("Training multi-class histogram regressor...")

        attributePrototype = buildAttributePrototype(examples)
        dummyDataset = new Instances("dummySet", attributePrototype, 0)
        dummyDataset.setClassIndex(attributePrototype.size - 1)

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

        classifier.buildClassifier(trainingSet)
    }

    def classificationAccuracy(examples:Seq[HistogramRegressor.RegressionExample]) : Double =
    {
        val assignments = new Array[Int](examples.length)
        for (i <- 0 until examples.length)
            assignments(i) = MathUtils.closestVectorBruteForce(examples(i).target, centroids, metric)

        var numCorrectlyPredicted = 0
        for (i <- 0 until examples.length)
        {
            val instance = convertToWekaInstance(examples(i).features)
            instance.setDataset(dummyDataset)
            val predictedBin = classifier.classifyInstance(instance).toInt
            if (predictedBin == assignments(i)) numCorrectlyPredicted += 1
        }
        val accuracy = numCorrectlyPredicted.toDouble / examples.length
        accuracy
    }

    // Not all misclassifications are equally bad. This measure takes this into account
    // Here, we look at the metric distance between the predicted bin centroid and the target value
    def avgResidualSumOfSquaredError(examples:Seq[HistogramRegressor.RegressionExample]) : Double =
    {
        var error = 0.0
        for (i <- 0 until examples.length)
        {
            val instance = convertToWekaInstance(examples(i).features)
            instance.setDataset(dummyDataset)
            val predictedBin = classifier.classifyInstance(instance).toInt
            val predictedCentroid = centroids(predictedBin)
            val err = metric(predictedCentroid, examples(i).target)
            error += err*err
        }
        error / examples.length
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
    def apply(classifier:Classifier, metric:MathUtils.DistanceMetric, bandwidthScale:Double) =
        new WekaBinByBinHistogramRegressor(classifier, metric, bandwidthScale)
}

class WekaBinByBinHistogramRegressor(classifier:Classifier, metric:MathUtils.DistanceMetric, bandwidthScale:Double)
    extends HistogramRegressor(metric, bandwidthScale)
{
    private val regressors = Classifier.makeCopies(classifier, centroids.length)
    private var attributePrototype:FastVector = null
    private var dummyDataset:Instances = null

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

    def trainRegressor(examples:Seq[HistogramRegressor.RegressionExample])
    {
        //println("Training bin-by-bin histogram regressor...")

        attributePrototype = buildAttributePrototype(examples)
        dummyDataset = new Instances("dummySet", attributePrototype, 0)
        dummyDataset.setClassIndex(attributePrototype.size - 1)

        // Build a 'training histogram' so we can evaluate bin probabilities
        val trainingHist = new VectorHistogram(metric, bandwidthScale)
        trainingHist.setData(centroids, for (i <- 0 until centroids.length) yield 0.0)

        // Train each regressor
        for (b <- 0 until centroids.length)
        {
            //println("Training bin " + b + "...")

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
        {
            bins(b) = regressors(b).classifyInstance(instance)
        }
    }

    def getCoefficients:Array[Array[Double]] =
    {
      throw new Error("getCoefficients not implemented in WekaBinByBinHistogramRegressor yet")
    }

}