/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/15/12
 * Time: 11:22 AM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie.la.Tensor1
import jnisvmlight._

class ConditionalHistogramRegressor(examples:Seq[ConditionalHistogramRegressor.RegressionExample], private val metric:MathUtils.DistanceMetric, quantizer:VectorQuantizer)
{
    private var centroids:IndexedSeq[Tensor1] = null
    private var regressors:Array[SVMLightModel] = null
    private var featureIndices:Array[Int] = null

    train(examples, quantizer)

    private def train(examples:Seq[ConditionalHistogramRegressor.RegressionExample], quantizer:VectorQuantizer)
    {
        assert(examples.length > 0, {println("ConditionalHistogramRegressor: cannot train with 0 training examples")})
        featureIndices = (0 until examples(0).features.length).toArray

        // Invoke the quantizer
        val (centroids, assignments) = quantizer(for (ex <- examples) yield ex.target, metric)
        this.centroids = centroids
        val numBins = centroids.length

        // Set up the JNI interface to svmlight
        val trainer = new SVMLightInterface()
        SVMLightInterface.SORT_INPUT_VECTORS = false    // Dimensions will already be sorted

        // Set up necessary training parameters
        val params = new TrainingParameters()
        params.getLearningParameters.`type` = LearnParam.REGRESSION.toLong
        // TODO: Consider non-default kernels, such as RBF kernel?

        // Train 'em up!
        regressors = new Array[SVMLightModel](numBins)
        for (i <- 0 until numBins)
        {
            // Fill in training data in the format expected by svmlight
            val trainingData = new Array[LabeledFeatureVector](examples.length)
            for (j <- 0 until examples.length)
            {
                val ex = examples(j)
                val target = if (assignments(j) == i) 1.0 else 0.0
                val dims = (0 until ex.features.length).toArray
                val features = ex.features.toArray
                trainingData(i) = new LabeledFeatureVector(target, dims, features)
            }
            // TODO: Consider normalizing/standardizing the data so that linear kernel turns into cosine/mahalanobis distance?

            // Finally, train the SVM regressor
            regressors(i) = trainer.trainModel(trainingData, params)
        }
    }

    def predictHistogram(featureVec:Tensor1) : VectorHistogram =
    {
        val svmlightfeatures = new FeatureVector(featureIndices, featureVec.toArray)
        val bins = new Array[Double](centroids.length)
        var totalmass = 0.0
        for (i <- 0 until bins.length)
        {
            val prediction = regressors(i).classify(svmlightfeatures)
            bins(i) = prediction
            totalmass += prediction
        }

        // Normalize bins
        for (i <- 0 until bins.length) bins(i) /= totalmass

        val hist = new VectorHistogram(metric)
        hist.setData(centroids, bins)
        hist
    }
}

object ConditionalHistogramRegressor
{
    case class RegressionExample(target:Tensor1, features:Tensor1) {}
}
