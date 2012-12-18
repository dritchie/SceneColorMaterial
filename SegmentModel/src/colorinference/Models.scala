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

class SummaryItem(val ttype:String, val propname:String, val ids:Array[String], val hist:VectorHistogram)


/** All the templates we define will have this trait **/
trait ColorInferenceModelComponent
{
    lazy val weights = new DenseTensor1(1)
    def setWeight(w:Double) { weights.update(0, w) }

    def conditionOn(mesh:SegmentMesh)
}

trait ColorInferenceHistogramTemplate extends ColorInferenceModelComponent
{
    def summarize():Array[SummaryItem]
}

/** This is the top-level model that we use for everything.
  * Templates are added to this thing.
   */
class ColorInferenceModel extends TemplateModel
{
    type ModelSummary = ArrayBuffer[SummaryItem]
    def conditionOn(mesh:SegmentMesh)
    {
        for (t <- this.templates)
            t.asInstanceOf[ColorInferenceModelComponent].conditionOn(mesh)
    }

    def getSummary:ModelSummary =
    {
      var summary = new ModelSummary
      for (t <- this.templates)
        summary ++= t.asInstanceOf[ColorInferenceHistogramTemplate].summarize()
      summary
    }

}

object UnarySegmentTemplate
{
    type ColorPropertyExtractor = Color => Tensor1
    case class Datum(seg:Segment, hist:VectorHistogram)
    type DatumVariable = RefVariable[Datum]
    protected type Data = HashMap[Segment, DatumVariable]
}

trait UnarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, UnarySegmentTemplate.DatumVariable] with ColorInferenceHistogramTemplate
{
    import UnarySegmentTemplate._

    protected def colorPropExtractor:ColorPropertyExtractor
    protected def regressor:HistogramRegressor
    protected val data = new Data
    def propName:String

    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (seg <- mesh.segments)
        {
            val f = Segment.getUnaryRegressionFeatures(seg)
            data(seg) = new DatumVariable(Datum(seg, regressor.predictHistogram(f)))
        }
    }

    def summarize():Array[SummaryItem] =
    {
      val items = data.keys.map(s => new SummaryItem("unarysegment", propName, Array("s"+s.index), data(s).value.hist))

      items.toArray
    }

    protected def trainRegressor(property:ModelTraining#UnarySegmentProperty) : HistogramRegressor =
    {
        //HistogramRegressor.LogisticRegression(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)
        HistogramRegressor.KNN(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)
    }

    protected def computeStatistics(color:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color)
        val density = datum.hist.evaluateAt(props)
        var logDensity = MathUtils.safeLog(density)
        // Weight by relative size so that groups with tons of little segments don't get
        // unfairly emphasized
        logDensity *= datum.seg.size
        Tensor1(logDensity)
    }

    def unroll1(v1:ColorVar) =
    {
        // Yield a new factor for every segment in the color group associated with this color
        // Pass the DataVariable associated with the segment into the factor as well, so that
        //  it's available when it comes time to compute scores
        for (seg <- v1.group.members)
            yield Factor(v1, data(seg))
    }

    // This will never be called, since the DataVariable never changes

    def unroll2(v2:DatumVariable) =
    {
        Nil
    }
}

class DiscreteUnarySegmentTemplate(property:ModelTraining#UnarySegmentProperty)
    extends DotTemplate2[DiscreteColorVariable, UnarySegmentTemplate.DatumVariable] with UnarySegmentTemplate[DiscreteColorVariable]
{
    import UnarySegmentTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = trainRegressor(property)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2)
    }
}

class ContinuousUnarySegmentTemplate(property:ModelTraining#UnarySegmentProperty)
    extends DotTemplate2[ContinuousColorVariable, UnarySegmentTemplate.DatumVariable] with UnarySegmentTemplate[ContinuousColorVariable]
{
    import UnarySegmentTemplate._

    protected val colorPropExtractor = property.extractor
    protected val regressor = trainRegressor(property)
    val propName = property.name

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
    protected type Data = HashMap[(Segment,Segment), DatumVariable]
}

trait BinarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate3[ColorVar, ColorVar, BinarySegmentTemplate.DatumVariable] with ColorInferenceHistogramTemplate
{
    import BinarySegmentTemplate._

    def propName:String
    protected def colorPropExtractor:ColorPropertyExtractor
    protected def regressor:HistogramRegressor
    protected val data = new Data


    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (seg1 <- mesh.segments; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
        {
            val f = Segment.getBinaryRegressionFeatures(seg1, seg2)
            data((seg1, seg2)) = new DatumVariable(Datum(seg1, seg2, regressor.predictHistogram(f)))
        }
    }

  def summarize():Array[SummaryItem] =
  {
    val items = data.keys.map(s => new SummaryItem("binarysegment", propName, Array("s"+s._1.index, "s"+s._2.index), data(s).value.hist))

    items.toArray
  }


  protected def trainRegressor(property:ModelTraining#BinarySegmentProperty) : HistogramRegressor =
    {
        //HistogramRegressor.LogisticRegression(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)

        HistogramRegressor.KNN(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)
    }

    protected def computeStatistics(color1:Color, color2:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color1, color2)
        val density = datum.hist.evaluateAt(props)
        var logDensity = MathUtils.safeLog(density)
        // Again, weight by size. This formula should make the total weight sum to 1
        val sizew  = (datum.seg1.size / datum.seg1.adjacencies.size) + (datum.seg2.size / datum.seg2.adjacencies.size)
        logDensity *= sizew
        Tensor1(logDensity)
    }

    def unroll1(v1:ColorVar) =
    {
        // Find all neighbors of v1, yield a factor for each segment pair
        for (seg1 <- v1.group.members; seg2 <- seg1.adjacencies if seg1.index < seg2.index)
            yield Factor(v1, seg2.group.color.asInstanceOf[ColorVar], data((seg1,seg2)))
    }

    def unroll2(v2:ColorVar) =
    {
        // Symmetric w.r.t to unroll1
        for (seg2 <- v2.group.members; seg1 <- seg2.adjacencies if seg1.index < seg2.index)
            yield Factor(seg1.group.color.asInstanceOf[ColorVar], v2, data((seg1,seg2)))
    }

    // This will never be called, since the DataVariable never changes
    def unroll3(v3:DatumVariable) =
    {
        Nil
    }
}

class DiscreteBinarySegmentTemplate(property:ModelTraining#BinarySegmentProperty)
    extends DotTemplate3[DiscreteColorVariable, DiscreteColorVariable, BinarySegmentTemplate.DatumVariable] with BinarySegmentTemplate[DiscreteColorVariable]
{
    import BinarySegmentTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = trainRegressor(property)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DiscreteColorVariable#Value, v3:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2.category, v3)
    }
}

class ContinuousBinarySegmentTemplate(property:ModelTraining#BinarySegmentProperty)
    extends DotTemplate3[ContinuousColorVariable, ContinuousColorVariable, BinarySegmentTemplate.DatumVariable] with BinarySegmentTemplate[ContinuousColorVariable]
{
    import BinarySegmentTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = trainRegressor(property)

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
    protected type Data = HashMap[SegmentGroup, DatumVariable]
}

trait ColorGroupTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, ColorGroupTemplate.DatumVariable] with ColorInferenceHistogramTemplate
{
    import ColorGroupTemplate._

    def propName:String
    protected def colorPropExtractor:ColorPropertyExtractor
    protected def regressor:HistogramRegressor
    protected val data = new Data

    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (group <- mesh.groups)
        {
            val f = SegmentGroup.getRegressionFeatures(group)
            data(group) = new DatumVariable(Datum(group, regressor.predictHistogram(f)))
        }
    }

    def summarize():Array[SummaryItem] =
    {
      val items = data.keys.map(g => new SummaryItem("unarygroup", propName, Array("g"+g.index), data(g).value.hist))

      items.toArray
    }

    protected def trainRegressor(property:ModelTraining#ColorGroupProperty) : HistogramRegressor =
    {
        //HistogramRegressor.LogisticRegression(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)

      HistogramRegressor.KNN(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)
    }

    protected def computeStatistics(color:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color)
        val density = datum.hist.evaluateAt(props)
        val logDensity = MathUtils.safeLog(density)
        // TODO: Some form of size weighting? I don't think it's needed...
        Tensor1(logDensity)
    }

    def unroll1(v1:ColorVar) =
    {
        Factor(v1, data(v1.group))
    }

    // This will never be called, since the DataVariable never changes
    def unroll2(v2:DatumVariable) =
    {
        Nil
    }
}

class DiscreteColorGroupTemplate(property:ModelTraining#ColorGroupProperty)
    extends DotTemplate2[DiscreteColorVariable, ColorGroupTemplate.DatumVariable] with ColorGroupTemplate[DiscreteColorVariable]
{
    import ColorGroupTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = trainRegressor(property)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2)
    }
}

class ContinuousColorGroupTemplate(property:ModelTraining#ColorGroupProperty)
    extends DotTemplate2[ContinuousColorVariable, ColorGroupTemplate.DatumVariable] with ColorGroupTemplate[ContinuousColorVariable]
{
    import ColorGroupTemplate._

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = trainRegressor(property)

    override def statistics(v1:ContinuousColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1, v2)
    }
}


/*
Base class of all factors that touch an arbitrary number of the same type of variable
 */
abstract class FactorN[V<:Variable](varlist:V*) extends Factor
{
    val vars = ArrayBuffer(varlist:_*)

    def variables: Seq[Variable] = vars
    def numVariables: Int = vars.length
    def variable(index: Int): Variable = vars(index)

    def score(values:Seq[V#Value]): Double
    def statistics(values:Seq[V#Value]): StatisticsType = values.asInstanceOf[StatisticsType]

    def currentScore: Double = score(for (v <- vars) yield v.value.asInstanceOf[V#Value])
    override def currentStatistics: StatisticsType = statistics(for (v <- vars) yield v.value.asInstanceOf[V#Value])

    def currentAssignment = new HashMapAssignment(vars)
    def assignmentScore(a:Assignment) = score(for (v <- a.variables.toSeq) yield a(v).asInstanceOf[V#Value])
    override final def assignmentStatistics(a:Assignment): StatisticsType =
        statistics(for (v <- a.variables.toSeq) yield a(v).asInstanceOf[V#Value])

    def valuesIterator: ValuesIterator = new ValuesIterator
    {
        def factor: FactorN[V] = FactorN.this
        def hasNext = false
        def next() = null.asInstanceOf[Assignment]  // Not sure if this will work...
        def score: Double = Double.NaN
        def valuesTensor: Tensor = null
    }
}

abstract class TensorFactorN[V<:Variable](varlist:V*) extends FactorN[V](varlist:_*)
{
    type StatisticsType = Tensor
    override def statistics(values:Seq[V#Value]): Tensor
    final def score(values:Seq[V#Value]): Double = statisticsScore(statistics(values))
    def scoreAndStatistics(values:Seq[V#Value]): (Double, Tensor) = {
        val tensor = statistics(values)
        (statisticsScore(tensor), tensor)
    }
    def statisticsScore(t:Tensor): Double
}

abstract class DotFactorN[V<:Variable](varlist:V*) extends TensorFactorN[V](varlist:_*)
{
    def weights: Tensor
    def statisticsScore(t:Tensor): Double = weights dot t
}


object ColorCompatibilityFactor
{
    var matlabProxy:MatlabProxyScalaWrapper = null

    def ensureMatlabConnection()
    {
        if (matlabProxy == null)
            setupMatlabConnection()
    }

    private def setupMatlabConnection()
    {
        // Open the connection to matlab
        val options = new MatlabProxyFactoryOptions.Builder().setUsePreviouslyControlledSession(true).build()
        val factory = new MatlabProxyFactory(options)
        val proxy = factory.getProxy
        matlabProxy = new MatlabProxyScalaWrapper(proxy)

        // Set up the workspace for processing color rating queries
        matlabProxy.eval("cd ../odonovan")
        matlabProxy.eval("setup_rating_env")
    }

    // Will we ever actually call this, or will we just let the program terminate? Is there a problem
    // if we do that?
    def closeMatlabConnection()
    {
        matlabProxy.proxy.disconnect()
    }
}

// This factor only makes sense in the context of continuous color variables, so there aren't
// two (discrete, continuous) versions of it.
class ColorCompatibilityFactor extends DotFactorN[ContinuousColorVariable] with ColorInferenceModelComponent
{
    def conditionOn(mesh:SegmentMesh)
    {
        // This factor touches the 5 largest color groups
        vars.clear()
        val sortedGroups = mesh.groups.sortWith((g1, g2) => g1.size > g2.size)
        for (i <- 0 until 5)
            vars += sortedGroups(i).color.asInstanceOf[ContinuousColorVariable]
    }

    private def colorsToArray(c1:Color, c2:Color, c3:Color, c4:Color, c5:Color) : Array[Double] =
    {
        MathUtils.concatVectors(c1, c2, c3, c4, c5).toArray
    }

    def statistics(c1:Color, c2:Color, c3:Color, c4:Color, c5:Color) : Tensor1 =
    {
        // TODO: Permutations?
        val retval = ColorCompatibilityFactor.matlabProxy.returningFeval("getRating", 1, colorsToArray(c1,c2,c3,c4,c5))
        val rating = retval(0).asInstanceOf[Array[Double]](0)
        val normalizedRating = rating / 5.0
        Tensor1(MathUtils.safeLog(normalizedRating))
    }

    override def statistics(values:Seq[ContinuousColorVariable#Value]): Tensor =
    {
        assert(values.length == 5, {println("ColorCompatibilityFactor.statistics - values list does not have 5 elements")})
        statistics(values(0), values(1), values(2), values(3), values(4))
    }
}


