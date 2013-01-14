package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import cc.factorie.la.Tensor
import cc.factorie.la.Tensor1
import cc.factorie.la.DenseTensor1
import collection.mutable.{ArrayBuffer, HashMap}
import scala.Array
import matlabcontrol._
import collection.mutable
import java.io.{File, FileNotFoundException, FileWriter}
import io.Source
import java.awt.image.BufferedImage
import java.awt

object ColorInferenceModel
{
    // Everything from top-to-bottom is conditional
    trait Conditional
    {
        def conditionOn(mesh:SegmentMesh)
        def conditionOnAll(meshes:Seq[SegmentMesh])
    }

    // Most things are named
    trait Named
    {
        def name:String
    }

    // All model components that wish to use a trainable log-linear weight
    // must mix-in this trait
    trait Trainable extends DotFamily with Named
    {
        lazy val weights = Tensor1(1.0)
        def setWeight(w:Double) { weights.update(0, w) }
        def getWeight = weights(0)

        def saveWeight(dir:String)
        {
            val fw = new FileWriter("%s/%s.weights".format(dir, name))
            fw.write("%f\n".format(getWeight))
            fw.close()
        }

        def loadWeight(dir:String) : Boolean =
        {
            try { setWeight(Source.fromFile("%s/%s.weights".format(dir,name)).getLines().toIndexedSeq(0).toDouble) }
            catch { case e:FileNotFoundException => return false }
            println("Loaded weight")
            true
        }
    }

    trait RegressionBased extends Named
    {
        protected def regressor:HistogramRegressor

        protected def trainRegressor(examples:Seq[HistogramRegressor.RegressionExample], crossValidate:Boolean,
                                     saveValidationLog:Boolean, cvRanges:HistogramRegressor.CrossValidationRanges,
                                     numBins:Int, bandScale:Double, loadFrom:String = "")
        {
            if (loadFrom.isEmpty || !regressor.load(examples, "%s/%s".format(loadFrom, name)))
            {
                var bins = numBins
                if (crossValidate)
                {
                    println("Cross-validating...")

                    var log:FileWriter = null
                    if (saveValidationLog)
                    {
                        val dir = new File("crossValidationLog")
                        if (!dir.exists)
                            dir.mkdir

                        log = new FileWriter("%s/%s.crossValidation.csv".format(dir, name))
                        // Write headers
                        log.write("numBins,bandScale,likelihood,classificationAccuracy,predictionError\n")
                    }

                    // Split examples into training and test
                    // We assume that any randomization of examples has already been done.
                    val splitPoint = (0.75*examples.length).toInt
                    val trainData = examples.slice(0, splitPoint)
                    val testData = examples.slice(splitPoint+1, examples.length)

                    // Find the number of bins that gives the best likelihood after averaging
                    // over 'all possible' (read: many) bandwidth scales

                    //Find the number of bins and the bandwidth scale that gives the best likelihood
                    //on cross validation
                    var bestLL = Double.NegativeInfinity
                    var bestNumBins = -1
                    var bestBandScale = bandScale
                    for (bins <- cvRanges.numBins)
                    {
                        println("Trying numBins = %d".format(bins))
                        regressor.train(trainData, new KMeansVectorQuantizer(bins))
                        var avgLL = 0.0
                        for (scale <- cvRanges.bandScale)
                        {
                            regressor.bandwithScale = scale
                            val ll = regressor.avgLogLikelihood(testData)
                            avgLL += ll
                            if (saveValidationLog)
                            {
                                val acc = regressor.asInstanceOf[WekaMultiClassHistogramRegressor].classificationAccuracy(testData)
                                val err = regressor.asInstanceOf[WekaMultiClassHistogramRegressor].avgPredictionError(testData)
                                log.write("%d,%g,%g,%g,%g\n".format(bins, scale, ll, acc, err))
                                log.flush()
                            }
                            if (ll>bestLL)
                            {
                              bestLL = ll
                              bestNumBins = bins
                              bestBandScale = scale
                            }

                        }
                        avgLL /= cvRanges.bandScale.length
                       /* if (avgLL > bestLL)
                        {
                            bestLL = avgLL
                            bestNumBins = bins
                        } */
                    }

                    println("Best numBins = %d".format(bestNumBins))
                    bins = bestNumBins
                    // Reset bandwidth scale to original value
                    //regressor.bandwithScale = bandScale
                    regressor.bandwithScale = bestBandScale

                    if (saveValidationLog)
                        log.close()
                }
                // Final training
                println("Training...")
                regressor.train(examples, new KMeansVectorQuantizer(bins))
            }
        }

        def saveRegressorIfPossible(dir:String)
        {
            val basename = "%s/%s".format(dir, name)
            regressor.save(basename)
        }
    }

    // Implement this trait so that we can spit out & analyze model contents
    class SummaryItem(val ttype:String, val propname:String, val ids:Array[String], val hist:VectorHistogram, val origValue:Tensor1)
    class CoefficientsItem(val ttype:String, val propname:String, val classes:Seq[Tensor1], val featureNames:Seq[String], val coefficients:Array[Array[Double]])
    class Summary
    {
      var histograms:mutable.IndexedSeq[SummaryItem] = new ArrayBuffer[SummaryItem]
      var coefficients:mutable.IndexedSeq[CoefficientsItem] = new ArrayBuffer[CoefficientsItem]

      def ++= (other:Summary)
      {
        histograms ++= other.histograms
        coefficients ++= other.coefficients
      }

    }
    trait Summarizable
    {
        def summary:Summary
    }
}

class ColorInferenceModel extends TemplateModel
    with ColorInferenceModel.Conditional with ColorInferenceModel.Summarizable
{
    import ColorInferenceModel._

    def trainables:Seq[Trainable] = families.collect{case t:Trainable => t}
    def regressionBasedComps:Seq[RegressionBased] = families.collect{case rb:RegressionBased => rb}

    def getWeights: Tensor1 =
    {
        Tensor1((for (t <- trainables) yield t.getWeight):_*)
    }

    def setWeights(weights:Tensor1)
    {
        val ts = trainables
        for (i <- 0 until ts.length)
            ts(i).setWeight(weights(i))
    }

    def enforceMinimumWeight(minWeight:Double)
    {
        trainables.foreach(t => t.setWeight(math.max(minWeight, t.getWeight)))
    }

    def normalizeWeights()
    {
        // First, all weights have to be non-negative
        enforceMinimumWeight(0.0)
        val weightsum = trainables.map(_.getWeight).sum
        trainables.foreach(t => t.setWeight(t.getWeight / weightsum))
    }

    def randomizeWeights()
    {
        for (t <- trainables) t.setWeight(math.random)
    }

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
        for (c <- this.templates.collect{case c:Conditional => c})
            c.conditionOnAll(meshes)
    }


    def conditionOn(mesh:SegmentMesh)
    {
        for (c <- this.templates.collect{case c:Conditional => c})
            c.conditionOn(mesh)
    }

    def summary:Summary =
    {
        var summary = new Summary
        for (s <- this.templates.collect{case s:Summarizable => s})
            summary ++= s.summary
        summary
    }
}


object UnarySegmentTemplate
{
    type ColorPropertyExtractor = Color => Tensor1
    case class Datum(seg:Segment, hist:VectorHistogram)
    type DatumVariable = RefVariable[Datum]
    protected type Data = HashMap[Int, DatumVariable]
}

trait UnarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, UnarySegmentTemplate.DatumVariable]
    with ColorInferenceModel.Conditional with ColorInferenceModel.Trainable with ColorInferenceModel.Summarizable
    with ColorInferenceModel.RegressionBased
{
    import ColorInferenceModel._
    import UnarySegmentTemplate._

    protected def colorPropExtractor:ColorPropertyExtractor
    protected val data = new Data
    def propName:String
    def name = "Unary "+propName

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
      data.clear()
      for (mesh<-meshes; seg <- mesh.segments)
      {
        val f = Segment.getUnaryRegressionFeatures(seg)._1
        data.put(seg.index, new DatumVariable(Datum(seg, regressor.predictHistogram(f))))
      }
    }


    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (seg <- mesh.segments)
        {
            val f = Segment.getUnaryRegressionFeatures(seg)._1
            data.put(seg.index, new DatumVariable(Datum(seg, regressor.predictHistogram(f))))
        }
    }

    def summary:Summary =
    {
      val s = new Summary
      val items = data.keys.map(si => new SummaryItem("unarysegment", propName, Array("s"+si), data(si).value.hist, colorPropExtractor(data(si).value.seg.group.color.observedColor)) )
      s.histograms = items.toArray[SummaryItem]

      s.coefficients = Array(new CoefficientsItem("unarysegment", propName, regressor.getCentroids, regressor.getFeatureNames, regressor.getCoefficients))
      s
    }

    protected def createRegressor(property:ModelTraining#UnarySegmentProperty) : HistogramRegressor =
    {
        println("Training " + name + "...")
        property.regression(MathUtils.euclideanDistance, property.bandScale, WekaMultiClassHistogramRegressor)
    }

    protected def computeStatistics(color:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color)
        var density = datum.hist.evaluateAt(props)
        density = MathUtils.safeLog(density)
        // Weight by relative size so that groups with tons of little segments don't get
        // unfairly emphasized
        density *= datum.seg.size
        Tensor1(density)
    }

    def unroll1(v1:ColorVar) =
    {
        // Yield a new factor for every segment in the color group associated with this color
        // Pass the DataVariable associated with the segment into the factor as well, so that
        //  it's available when it comes time to compute scores
        for (seg <- v1.group.members)
            yield Factor(v1, data(seg.index))
    }

    // This will never be called, since the DataVariable never changes

    def unroll2(v2:DatumVariable) =
    {
        Nil
    }
}

class DiscreteUnarySegmentTemplate(property:ModelTraining#UnarySegmentProperty, loadFrom:String = "")
    extends DotTemplate2[DiscreteColorVariable, UnarySegmentTemplate.DatumVariable] with UnarySegmentTemplate[DiscreteColorVariable]
{
    import UnarySegmentTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = createRegressor(property)
    trainRegressor(property.examples, property.crossValidate, property.saveValidationLog, property.ranges, property.quantLevel, property.bandScale, loadFrom)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2)
    }
}

class ContinuousUnarySegmentTemplate(property:ModelTraining#UnarySegmentProperty, loadFrom:String = "")
    extends DotTemplate2[ContinuousColorVariable, UnarySegmentTemplate.DatumVariable] with UnarySegmentTemplate[ContinuousColorVariable]
{
    import UnarySegmentTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = createRegressor(property)
    trainRegressor(property.examples, property.crossValidate, property.saveValidationLog, property.ranges, property.quantLevel, property.bandScale, loadFrom)

    override def statistics(v1:ContinuousColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1, v2)
    }
}



object BinarySegmentTemplate
{
    type ColorPropertyExtractor = (Color, Color) => Tensor1
    case class Datum(seg1:Segment, seg2:Segment, hist:VectorHistogram)
    type DatumVariable = RefVariable[Datum]
    protected type Data = HashMap[(Int,Int), DatumVariable]
}

trait BinarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate3[ColorVar, ColorVar, BinarySegmentTemplate.DatumVariable]
    with ColorInferenceModel.Conditional with ColorInferenceModel.Trainable with ColorInferenceModel.Summarizable
    with ColorInferenceModel.RegressionBased
{
    import ColorInferenceModel._
    import BinarySegmentTemplate._

    def propName:String
    def name = "Binary " + propName
    protected def colorPropExtractor:ColorPropertyExtractor
    protected val data = new Data

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
      data.clear()
      for (mesh<-meshes; seg1 <- mesh.segments; adj <- seg1.adjacencies.values if seg1.index < adj.neighbor.index)
      {
        val seg2 = adj.neighbor
        val f = Segment.getBinaryRegressionFeatures(seg1, adj)._1
        data.put((seg1.index, seg2.index), new DatumVariable(Datum(seg1, seg2, regressor.predictHistogram(f))))
      }
    }

    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (seg1 <- mesh.segments; adj <- seg1.adjacencies.values if seg1.index < adj.neighbor.index)
        {
            val seg2 = adj.neighbor
            val f = Segment.getBinaryRegressionFeatures(seg1, adj)._1
            data.put((seg1.index, seg2.index), new DatumVariable(Datum(seg1, seg2, regressor.predictHistogram(f))))
        }
    }

  def summary:Summary =
  {
    val s = new Summary
    val items = data.keys.map(k => new SummaryItem("binarysegment", propName, Array("s"+k._1, "s"+k._2), data(k).value.hist, colorPropExtractor(data(k).value.seg1.group.color.observedColor,data(k).value.seg2.group.color.observedColor)))
    s.histograms = items.toArray[SummaryItem]

    s.coefficients = Array(new CoefficientsItem("binarysegment", propName, regressor.getCentroids, regressor.getFeatureNames, regressor.getCoefficients))

    s
  }


  protected def createRegressor(property:ModelTraining#BinarySegmentProperty) : HistogramRegressor =
    {
        println("Training " + name + "...")
        property.regression(MathUtils.euclideanDistance, property.bandScale, WekaMultiClassHistogramRegressor)
    }

    protected def computeStatistics(color1:Color, color2:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color1, color2)
        var density = datum.hist.evaluateAt(props)
        density = MathUtils.safeLog(density)

        // Again, weight by size. This formula should make the total weight sum to 1
        //val sizew  = (datum.seg1.size / datum.seg1.adjacencies.size) + (datum.seg2.size / datum.seg2.adjacencies.size)


        //weight based on adjacency strength seg1->seg2 (which is the number of pixels or seg2 surrounding seg1 divided by
        //the adjacency strengths of everything, including redundant edges like seg2->seg1)
        // since the adjacency strength is still not necessarily symmetric, due to the
        //pixelized nature of the region, add the adjacency strengths of seg2->seg1 and seg1->seg2
        val sizew = (datum.seg1.adjacencies(datum.seg2).strength)+(datum.seg2.adjacencies(datum.seg1).strength)

        density *= sizew
        Tensor1(density)
    }

    def unroll1(v1:ColorVar) =
    {
        // Find all neighbors of v1, yield a factor for each segment pair
        for (seg1 <- v1.group.members; seg2 <- seg1.adjacencies.keys if seg1.index < seg2.index)
            yield Factor(v1, seg2.group.color.asInstanceOf[ColorVar], data((seg1.index,seg2.index)))
    }

    def unroll2(v2:ColorVar) =
    {
        // Symmetric w.r.t to unroll1
        for (seg2 <- v2.group.members; seg1 <- seg2.adjacencies.keys if seg1.index < seg2.index)
            yield Factor(seg1.group.color.asInstanceOf[ColorVar], v2, data((seg1.index,seg2.index)))
    }

    // This will never be called, since the DataVariable never changes
    def unroll3(v3:DatumVariable) =
    {
        Nil
    }
}

class DiscreteBinarySegmentTemplate(property:ModelTraining#BinarySegmentProperty, loadFrom:String = "")
    extends DotTemplate3[DiscreteColorVariable, DiscreteColorVariable, BinarySegmentTemplate.DatumVariable] with BinarySegmentTemplate[DiscreteColorVariable]
{
    import BinarySegmentTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = createRegressor(property)
    trainRegressor(property.examples, property.crossValidate, property.saveValidationLog, property.ranges, property.quantLevel, property.bandScale, loadFrom)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DiscreteColorVariable#Value, v3:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2.category, v3)
    }
}

class ContinuousBinarySegmentTemplate(property:ModelTraining#BinarySegmentProperty, loadFrom:String = "")
    extends DotTemplate3[ContinuousColorVariable, ContinuousColorVariable, BinarySegmentTemplate.DatumVariable] with BinarySegmentTemplate[ContinuousColorVariable]
{
    import BinarySegmentTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = createRegressor(property)
    trainRegressor(property.examples, property.crossValidate, property.saveValidationLog, property.ranges, property.quantLevel, property.bandScale, loadFrom)

    override def statistics(v1:ContinuousColorVariable#Value, v2:ContinuousColorVariable#Value, v3:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1, v2, v3)
    }
}



object ColorGroupTemplate
{
    type ColorPropertyExtractor = Color => Tensor1
    case class Datum(group:SegmentGroup, hist:VectorHistogram)
    type DatumVariable = RefVariable[Datum]
    protected type Data = HashMap[Int, DatumVariable]
}

trait ColorGroupTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, ColorGroupTemplate.DatumVariable]
    with ColorInferenceModel.Conditional with ColorInferenceModel.Trainable with ColorInferenceModel.Summarizable
    with ColorInferenceModel.RegressionBased
{
    import ColorInferenceModel._
    import ColorGroupTemplate._

    def propName:String
    def name = "Group " + propName
    def isMarginal:Boolean
    def weightBySize:Boolean
    protected def colorPropExtractor:ColorPropertyExtractor
    protected val data = new Data

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
      data.clear()
      for (mesh<-meshes; group <- mesh.groups)
      {
        val f = {if (isMarginal) Tensor1(1.0) else SegmentGroup.getRegressionFeatures(group)._1}
        data.put(group.index, new DatumVariable(Datum(group, regressor.predictHistogram(f))))
      }
    }


    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (group <- mesh.groups)
        {
            val f = {if (isMarginal) Tensor1(1.0) else SegmentGroup.getRegressionFeatures(group)._1}
            data.put(group.index, new DatumVariable(Datum(group, regressor.predictHistogram(f))))
        }
    }

    def summary:Summary =
    {
      val s = new Summary
      val items = data.keys.map(gi => new SummaryItem("unarygroup", propName, Array("g"+gi), data(gi).value.hist, colorPropExtractor(data(gi).value.group.color.observedColor)))
      s.histograms = items.toArray[SummaryItem]

      s.coefficients = Array(new CoefficientsItem("unarygroup", propName, regressor.getCentroids, regressor.getFeatureNames, regressor.getCoefficients))
      s
    }

    protected def createRegressor(property:ModelTraining#ColorGroupProperty) : HistogramRegressor =
    {
        println("Training " + name + "...")
        property.regression(MathUtils.euclideanDistance, property.bandScale, WekaMultiClassHistogramRegressor)
    }

    protected def computeStatistics(color:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color)
        var density = datum.hist.evaluateAt(props)
        density = MathUtils.safeLog(density)
        // TODO: Some form of size weighting? I don't think it's needed...
        //TODO: might as well be consistent?  This just prefers scores of large groups over small groups. If we want to weight groups equally, the segment size should be normalized by the group size
      //when weighting unary terms
        if (weightBySize)
          density *= datum.group.size
        Tensor1(density)
    }

    def unroll1(v1:ColorVar) =
    {
        Factor(v1, data(v1.group.index))
    }

    // This will never be called, since the DataVariable never changes
    def unroll2(v2:DatumVariable) =
    {
        Nil
    }
}

class DiscreteColorGroupTemplate(property:ModelTraining#ColorGroupProperty, loadFrom:String = "",wBySize:Boolean=false)
    extends DotTemplate2[DiscreteColorVariable, ColorGroupTemplate.DatumVariable] with ColorGroupTemplate[DiscreteColorVariable]
{
    import ColorGroupTemplate._

    val propName = property.name
    val isMarginal = property.isMarginal
    val weightBySize = wBySize
    protected val colorPropExtractor = property.extractor
    protected val regressor = createRegressor(property)
    trainRegressor(property.examples, property.crossValidate, property.saveValidationLog, property.ranges, property.quantLevel, property.bandScale, loadFrom)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2)
    }
}

class ContinuousColorGroupTemplate(property:ModelTraining#ColorGroupProperty, loadFrom:String = "", wBySize:Boolean=false)
    extends DotTemplate2[ContinuousColorVariable, ColorGroupTemplate.DatumVariable] with ColorGroupTemplate[ContinuousColorVariable]
{
    import ColorGroupTemplate._

    val propName = property.name
    val isMarginal = property.isMarginal
    val weightBySize = wBySize
    protected val colorPropExtractor = property.extractor
    protected val regressor = createRegressor(property)
    trainRegressor(property.examples, property.crossValidate, property.saveValidationLog, property.ranges, property.quantLevel, property.bandScale, loadFrom)

    override def statistics(v1:ContinuousColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1, v2)
    }
}

class UserColorConstraintGroupTemplate(name: String, extractor:ColorGroupTemplate.ColorPropertyExtractor, bandwidth: Double, metric:MathUtils.DistanceMetric = MathUtils.euclideanDistance, loadFrom:String = "", wBySize:Boolean=false)
    extends DotTemplate2[ContinuousColorVariable, ColorGroupTemplate.DatumVariable] with ColorGroupTemplate[ContinuousColorVariable]
{
    import ColorGroupTemplate._

    val propName = name
    val isMarginal = true
    val weightBySize = wBySize
    protected val colorPropExtractor = extractor
    protected val regressor = null // TODO: see if this is necessary

    override def statistics(v1:ContinuousColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        Tensor1(MathUtils.logGaussianKernel(metric(colorPropExtractor(v2.group.color.observedColor), colorPropExtractor(v1)), 0, bandwidth))
    }

    override def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (group <- mesh.groups)
        {
            data.put(group.index, new DatumVariable(Datum(group, null)))
        }
    }

    override def conditionOnAll(meshes:Seq[SegmentMesh])
    {
        data.clear()
        for (mesh<-meshes; group <- mesh.groups)
        {
            data.put(group.index, new DatumVariable(Datum(group, null)))
        }
    }
}

object ColorCompatibilityTemplate
{
    private val proxyFactory = new MatlabProxyFactory(new MatlabProxyFactoryOptions.Builder().setUsePreviouslyControlledSession(true).build())
    private val thread2proxy = new java.util.concurrent.ConcurrentHashMap[Long, MatlabProxyScalaWrapper]
    case class ProxyRecord(proxy:MatlabProxyScalaWrapper, var ownerId:Long) { var isInUse = true }
    private val proxyPool = new mutable.ArrayBuffer[ProxyRecord]()

    // Should only be called from a synchronized block
    private def releaseProxiesFromDeadThreads()
    {
        val numThreadsUpperBoundEstimate = Thread.activeCount() + Runtime.getRuntime.availableProcessors
        val threads = new Array[Thread](numThreadsUpperBoundEstimate+1)
        val numThreadsRetrieved = Thread.enumerate(threads)
        assert(numThreadsRetrieved <= numThreadsUpperBoundEstimate, {println("ColorCompatibilityTemplate: Couldn't retrieve all active threads")})

        // First, mark proxy records as no longer in use
        val deadIds = new mutable.HashSet[Long]
        for (record <- proxyPool if record.isInUse && threads.find(t => t != null && t.getId == record.ownerId) == None)
        {
            deadIds += record.ownerId
            record.isInUse = false
            record.ownerId = -1
        }

        // Then, remove useless entries from the hash table
        for (id <- deadIds) thread2proxy.remove(id)
    }

    def getMatlabProxy : MatlabProxyScalaWrapper =
    {
        val id = Thread.currentThread.getId

        // First, check if we already have a proxy associated with this thread
        var proxy = thread2proxy.get(id)
        if (proxy == null)
        {
            // If we don't, then first try to reclaim an unused one
            this.synchronized
            {
                releaseProxiesFromDeadThreads()
                proxy = proxyPool.find(!_.isInUse) match
                {
                    case Some(proxyRecord) =>
                    {
                        // Assume ownership of this proxy
                        proxyRecord.isInUse = true
                        proxyRecord.ownerId = id
                        thread2proxy.put(id, proxyRecord.proxy)
                        println("Thread %d re-using available MATLAB proxy".format(id))
                        proxyRecord.proxy
                    }
                    case _ => null
                }
            }
            if (proxy == null)
            {
                // If all else fails, create a new proxy and set it up
                val newproxy = proxyFactory.getProxy
                val matlabProxy = new MatlabProxyScalaWrapper(newproxy)
                matlabProxy.eval("cd ../odonovan")
                matlabProxy.eval("setup_rating_env")
                this.synchronized { proxyPool += ProxyRecord(matlabProxy, id) }
                thread2proxy.put(id, matlabProxy)
                println("Thread %d creating new MATLAB proxy".format(id))
                proxy = matlabProxy
            }
        }
        proxy
    }
}

class ColorCompatibilityTemplate extends DotTemplateN[ContinuousColorVariable] with ColorInferenceModel.Trainable
{
    var skipTopN = 0

    def name = "ColorCompatibility"

    private def colorsToArray(colors:Seq[Color]) : Array[Double] =
    {
        MathUtils.concatVectors(colors.map(_.copyIfNeededTo(RGBColorSpace))).toArray
    }

    override def statistics(values:Seq[Color]): Tensor =
    {
        assert(values.length <= 5, {println("ColorCompatibilityTemplate.statistics - values list has more than 5 elements")})

        // If we have less than 5 colors, then (partially) cycle the available colors(?)
        // These will be the colors of the n largest color groups, since we sorted the variables
        // NOTE: This is only guaranteed to be true if this method was invoked via 'currentStatistics'
        //  If it was invoked via e.g. 'assignmentStatistics,' then the order of the variables could get scrambled
        val c = new Array[Color](5)
        val n = values.length
        for (i <- 0 until 5) c.update(i, values(i % n))

        val retval = ColorCompatibilityTemplate.getMatlabProxy.returningFeval("getRating", 1, colorsToArray(c))
        val rating = retval(0).asInstanceOf[Array[Double]](0)
        val normalizedRating = rating / 5.0
        Tensor1(MathUtils.safeLog(normalizedRating))
    }

    private def sizedOrderedVars(mesh:SegmentMesh) =
    {
        // (At most) the top five biggest groups in the mesh
        mesh.groups.sortWith(_.size > _.size).map(_.color.asInstanceOf[ContinuousColorVariable]).slice(skipTopN,skipTopN+5)
    }

    def unroll(v:ContinuousColorVariable) : Iterable[Factor] =
    {
        val orderedVars = sizedOrderedVars(v.group.owner)
        if (orderedVars.contains(v))
            new Factor(orderedVars:_*)
        else
            Nil
    }
}



class ImageHistogramTemplate(private val img:BufferedImage, private val quantization:Int, areaWeighted:Boolean = true)
    extends DotTemplate1[ContinuousColorVariable] with ColorInferenceModel.Trainable
{
    def name = "TargetImage"
    private val hist = buildHistogram()

    private def buildHistogram() : VectorHistogram =
    {
        // Extract all the pixels
        println("TargetImageTemplate - extracting all pixels in LAB space...")
        val pixels = for (y <- 0 until img.getHeight; x <- 0 until img.getWidth) yield img.getRGB(x, y)

        // Keep track of counts for unique pixels
        val pixelCounts = new mutable.HashMap[Int, Int]
        for (rgb <- pixels)
        {
            pixelCounts.get(rgb) match
            {
                case Some(count) => pixelCounts.update(rgb, count+1)
                case None => pixelCounts.put(rgb, 1)
            }
        }
        // Sum up the total counts
        val totalCount = pixelCounts.values.reduce(_+_)

        // Convert unique pixels into LAB colors, and turn the counts into weights
        val colors = new ArrayBuffer[Color]
        val weights = new ArrayBuffer[Double]
        for ((rgb,count) <- pixelCounts)
        {
            val c = new java.awt.Color(rgb)
            val labc = Color.RGBColor(c.getRed/255.0, c.getGreen/255.0, c.getBlue/255.0).copyTo(LABColorSpace)
            colors += labc
            weights += count.toDouble / totalCount
        }

        // Quantize the weighted colors
        val quantizer = new KMeansVectorQuantizer(quantization)
        val (centroids, assignments) = quantizer.apply(colors, MathUtils.euclideanDistance, weights)
        val bins = Array.fill(centroids.length)(0.0)
        if (areaWeighted)
        {
            for (i <- 0 until colors.length) bins(assignments(i)) += weights(i)
        }
        else
        {
            for (i <- 0 until colors.length) bins(assignments(i)) += (1.0/centroids.length)
        }

        // Set up the histogram
        val hist = new VectorHistogram(MathUtils.euclideanDistance, 1.0)
        //val hist = new MassiveVectorHistogram(MathUtils.euclideanDistance, 1.0)
        hist.setData(centroids, bins)
        hist
    }

    override def statistics(v1:Color) : Tensor =
    {
        Tensor1(MathUtils.safeLog(hist.evaluateAt(v1.copyIfNeededTo(LABColorSpace))))
    }

    override def unroll1(v1:ContinuousColorVariable) : Iterable[Factor] =
    {
        // This factor applies to every color variable
        Factor(v1)
    }
}


class BackgroundDissimilarityTemplate(private val beFarFrom:Color) extends DotTemplate1[ContinuousColorVariable]
    with ColorInferenceModel.Trainable
{
    def name = "BackgroundDissimilarity"

    override def statistics(v1:Color) : Tensor =
    {
        // Stay away from that color!
        val dist = Color.perceptualDifference(v1, beFarFrom)
        Tensor1(dist)
    }

    override def unroll1(v1:ContinuousColorVariable) : Iterable[Factor] =
    {
        // We assume the biggest color group is the background
        val sortedVars = v1.group.owner.groups.sortWith(_.size > _.size).map(_.color)
        if (sortedVars(0) == v1)
            Factor(v1)
        else
            Nil
    }
}


// Assumes that all meshes this will ever be used on have at least as many color groups as the number
// of required colors
class RequiredColorsTemplate(private val colors:Seq[Color]) extends DotTemplateN[ContinuousColorVariable]
    with ColorInferenceModel.Trainable
{
    def name = "RequiredColors"

    override def statistics(values:Seq[Color]): Tensor =
    {
        // Do a greedy bipartite matching: Find the closest variable value to each required color,
        // sum up the distances
        var distsum = 0.0
        val colorIsAlreadyUsed = Array.fill(values.length)(false)
        for (reqColor <- colors)
        {
            var bestIndex = -1
            var bestDist = Double.PositiveInfinity
            for (i <- values.indices)
            {
                if (!colorIsAlreadyUsed(i))
                {
                    val dist = Color.perceptualDifference(values(i), reqColor)
                    if (dist < bestDist)
                    {
                        bestDist = dist
                        bestIndex = i
                    }
                }
            }
            distsum += bestDist
        }

        // Return the negative distance as the score
        // TODO: Make this into a log-Gaussian instead?
        Tensor1(-distsum)
    }

    private def sizedOrderedVars(mesh:SegmentMesh) =
    {
        // Get all color variables, sorted by size
        mesh.groups.sortWith(_.size > _.size).map(_.color.asInstanceOf[ContinuousColorVariable])
    }

    def unroll(v:ContinuousColorVariable) : Iterable[Factor] =
    {
        val orderedVars = sizedOrderedVars(v.group.owner)

        // This term applies to every variable
        new Factor(orderedVars:_*)
    }
}


// I'm assuming the image doesn't have too many colors (i.e. it came from a minimal web page), so this
/// template doesn't use a Kd tree to find the closest color.
class ImagePaletteTemplate(img:BufferedImage) extends DotTemplate1[ContinuousColorVariable]
    with ColorInferenceModel.Trainable
{
    def name = "UnorderedPalette"

    private val colors = extractColorsFromImage(img)

    private def extractColorsFromImage(img:BufferedImage) : Seq[Color] =
    {
        val colors = (for (y <- 0 until img.getHeight; x <- 0 until img.getWidth) yield img.getRGB(x, y)).distinct
        val mycolors = colors.map(rgb =>
        {
            val c = new java.awt.Color(rgb)
            Color.RGBColor(c.getRed/255.0, c.getGreen/255.0, c.getBlue/255.0)
        })
        mycolors
    }

    override def statistics(v1:Color) : Tensor =
    {
        // Find the closest palette color, and penalize by distance to that color
        var closestColor:Color = null
        var closestDist = Double.PositiveInfinity
        for (c <- colors)
        {
            val dist = Color.perceptualDifference(c, v1)
            if (dist < closestDist)
            {
                closestColor = c
                closestDist = dist
            }
        }

        Tensor1(-closestDist)
    }

    override def unroll1(v1:ContinuousColorVariable) : Iterable[Factor] =
    {
        // This factor applies to every color variable
        Factor(v1)
    }
}



object TargetImageTemplate
{
    type DatumVariable = RefVariable[Color]
}
class TargetImageTemplate(img:BufferedImage, mesh:SegmentMesh, private val bandwidth:Double) extends DotTemplate2[ContinuousColorVariable, TargetImageTemplate.DatumVariable]
    with ColorInferenceModel.Trainable
{
    import TargetImageTemplate._

    def name = "TargetImage"

    private val data = buildData(img, mesh)

    private def buildData(img:BufferedImage, mesh:SegmentMesh) : mutable.Map[Int, DatumVariable] =
    {
        // Build up map of segment index -> target color based on the image
        val thedata = new mutable.HashMap[Int, DatumVariable]
        val w = img.getWidth
        val h = img.getHeight
        for (seg <- mesh.segments)
        {
            val relp = seg.features("OneInteriorPoint")
            val p = Tensor1(w*(0.5 + relp(0)), h*(0.5 + relp(1)))
            val rgb = img.getRGB(p(0).toInt, p(1).toInt)
            val c = new awt.Color(rgb)
            val myc = Color.RGBColor(c.getRed/255.0, c.getGreen/255.0, c.getBlue/255.0)
            thedata.put(seg.index, new DatumVariable(myc.copyTo(LABColorSpace)))
        }
        thedata
    }

    override def statistics(variableColor:Color, targetColor:Color) : Tensor =
    {
        Tensor1(MathUtils.logGaussianKernel(Color.perceptualDifference(variableColor, targetColor), bandwidth))
    }

    def unroll1(v1:ContinuousColorVariable) : Iterable[Factor] =
    {
        for (seg <- v1.group.members) yield Factor(v1, data(seg.index))
    }

    def unroll2(v2:DatumVariable) : Iterable[Factor] =
    {
        Nil
    }
}

