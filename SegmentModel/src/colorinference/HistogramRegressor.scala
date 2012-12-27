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

// A subclass of weka's logistic regression that allows us to manually set the model parameters.
// This lets us save/load models
class LogisticWithSettableParams extends Logistic
{
    // The onus is on the caller to make sure that the dimensions of coeffs are correct
    def setCoefficients(coeffs:Array[Array[Double]])
    {
        this.m_Par = coeffs
    }

    def setClassIndex(index:Int)
    {
        this.m_ClassIndex = index
    }

    def setNumPredictors(num:Int)
    {
        this.m_NumPredictors = num
    }
}

object HistogramRegressor
{
    case class RegressionExample(target:Tensor1, features:Tensor1, featureNames:Array[String]=null, weight:Double=1) {}

    def KNN(metric:MathUtils.DistanceMetric, generator:WekaHistogramRegressor) =
    {
        val classifier = new IBk()
        classifier.setDistanceWeighting(new SelectedTag(IBk.WEIGHT_INVERSE, IBk.TAGS_WEIGHTING))
        classifier.setKNN(100)
        classifier.setMeanSquared(true)
        classifier.setNearestNeighbourSearchAlgorithm(new KDTree())

        generator(classifier, metric)
    }

    def LogisticRegression(metric:MathUtils.DistanceMetric, generator:WekaHistogramRegressor) =
    {
        val classifier = new LogisticWithSettableParams()

        generator(classifier, metric)
    }

    def LWLR(metric:MathUtils.DistanceMetric, generator:WekaHistogramRegressor) =
    {
        val classifier = new Logistic()
        val enhancer = new LWL()
        enhancer.setClassifier(classifier)
        enhancer.setNearestNeighbourSearchAlgorithm(new KDTree())
        enhancer.setKNN(100)

        generator(enhancer, metric)
    }

    def SVM(metric:MathUtils.DistanceMetric, generator:WekaHistogramRegressor) =
    {
        val classifier = new SMO()
        classifier.setNumFolds(10)
        classifier.setFilterType(new SelectedTag(SMO.FILTER_STANDARDIZE, SMO.TAGS_FILTER))
        val kernel = new RBFKernel()
        classifier.setKernel(kernel)

        generator(classifier, metric)
    }
}

abstract class HistogramRegressor(private val metric:MathUtils.DistanceMetric)
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

    def train(examples:Seq[HistogramRegressor.RegressionExample], quantizer:VectorQuantizer, loadFrom:String = "")
    {
        featureNames = examples(0).featureNames
        if (loadFrom.isEmpty || !loadCentroids(loadFrom))
            centroids = computeCentroids(examples, quantizer)
        trainRegressor(examples, loadFrom)
    }
    def trainRegressor(examples:Seq[HistogramRegressor.RegressionExample], loadFrom:String = "")

    def saveCentroids(fileBaseName:String)
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
    def loadCentroids(fileBaseName:String) : Boolean =
    {
        print("Attemping to load centroids...")
        try
        {
            centroids = Source.fromFile(fileBaseName + ".centroids").getLines().map(l => Tensor1(l.split(' ').map(_.toDouble):_*)).toArray[DenseTensor1]
        } catch { case e:FileNotFoundException => println("FAILED"); return false }
        println("SUCCESS")
        true
    }
    def saveRegressor(fileBaseName:String) {}
    def loadRegressor(fileBaseName:String) : Boolean =  { false }

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
        val hist = new VectorHistogram(metric)
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
    def apply(classifier:Classifier, metric:MathUtils.DistanceMetric) : HistogramRegressor
}

object WekaMultiClassHistogramRegressor extends WekaHistogramRegressor
{
    def apply(classifier:Classifier, metric:MathUtils.DistanceMetric) = new WekaMultiClassHistogramRegressor(classifier, metric)
}

class WekaMultiClassHistogramRegressor(private val classifier:Classifier, metric:MathUtils.DistanceMetric)
    extends HistogramRegressor(metric)
{
    private var attributePrototype:FastVector = null
    private var dummyDataset:Instances = null

    override def saveRegressor(fileBaseName:String)
    {
        // Save logistic coefficients
        val c = classifier.asInstanceOf[LogisticWithSettableParams]
        val fw = new FileWriter(fileBaseName + ".coeffs")
        for (i <- 0 until c.coefficients.length)
        {
            for (j <- 0 until c.coefficients()(i).length)
            {
                fw.write("%f ".format(c.coefficients()(i)(j)))
            }
            fw.write("\n")
        }
        fw.close()
    }

    override def loadRegressor(fileBaseName:String) : Boolean =
    {
        print("Attempting to load regressor...")
        try
        {
            // Load logistic coefficients
            val coeffs = Source.fromFile(fileBaseName + ".coeffs").getLines().map(_.split(' ').map(_.toDouble)).toArray
            classifier.asInstanceOf[LogisticWithSettableParams].setCoefficients(coeffs)
        } catch { case e:FileNotFoundException => println("FAILED"); return false }
        println("SUCCESS")
        true
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

    def trainRegressor(examples:Seq[HistogramRegressor.RegressionExample], loadFrom:String = "")
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
        // If we're loading, then we just stick one instance in there (to make Weka happy)
        val loading = !loadFrom.isEmpty && loadRegressor(loadFrom)
        val numInsts = if (loading) 1 else examples.length
        for (i <- 0 until numInsts)
        {
            val instance = convertToWekaInstance(examples(i).features, examples(i).weight, assignments(i))
            trainingSet.add(instance)
        }

        // Train the model
        if (loading)
        {
            val c = classifier.asInstanceOf[LogisticWithSettableParams]
            val savedCoeffs = c.coefficients
            c.buildClassifier(trainingSet)
            c.setCoefficients(savedCoeffs)
            // We also have to set the following two members, since Weka will filter out all of our
            // attributes as 'useless' if there's just one training example
            c.setClassIndex(trainingSet.classIndex)
            c.setNumPredictors(trainingSet.numAttributes - 1)

        }
        else classifier.buildClassifier(trainingSet)
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
    def apply(classifier:Classifier, metric:MathUtils.DistanceMetric) = new WekaBinByBinHistogramRegressor(classifier, metric)
}

class WekaBinByBinHistogramRegressor(classifier:Classifier, metric:MathUtils.DistanceMetric)
    extends HistogramRegressor(metric)
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

    def trainRegressor(examples:Seq[HistogramRegressor.RegressionExample], loadFrom:String = "")
    {
        //println("Training bin-by-bin histogram regressor...")

        attributePrototype = buildAttributePrototype(examples)
        dummyDataset = new Instances("dummySet", attributePrototype, 0)
        dummyDataset.setClassIndex(attributePrototype.size - 1)

        // Build a 'training histogram' so we can evaluate bin probabilities
        val trainingHist = new VectorHistogram(metric)
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
            bins(b) = regressors(b).classifyInstance(instance)
    }

    def getCoefficients:Array[Array[Double]] =
    {
      throw new Error("getCoefficients not implemented in WekaBinByBinHistogramRegressor yet")
    }

}