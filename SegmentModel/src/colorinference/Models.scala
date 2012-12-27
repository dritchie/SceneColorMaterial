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
    }

    // VectorHistogram-based components must implement this trait so
    // that we can spit out & analyze their contents
    class SummaryItem(val ttype:String, val propname:String, val ids:Array[String], val hist:VectorHistogram)
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

trait ColorInferenceModel extends Model
    with ColorInferenceModel.Conditional with ColorInferenceModel.Summarizable
{
    import ColorInferenceModel._
    def trainables:Seq[Trainable] = families.collect{case t:Trainable => t}
    def trainableWeights:Tensor1 = MathUtils.concatVectors(trainables.map(_.weights))

    def normalizeWeights()
    {
        var totalWeight = 0.0
        for (t <- trainables) totalWeight += t.getWeight
        if (totalWeight > 0.0)
            for (t <- trainables) t.setWeight(t.getWeight / totalWeight)
    }

    def randomizeWeights()
    {
        for (t <- trainables) t.setWeight(math.random)
    }
}

class CombinedColorInferenceModel(theSubModels:Model*) extends CombinedModel(theSubModels:_*) with ColorInferenceModel
{
    import ColorInferenceModel._

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
        for (c <- this.subModels.collect{case c:Conditional => c})
            c.conditionOnAll(meshes)
    }


    def conditionOn(mesh:SegmentMesh)
    {
        for (c <- this.subModels.collect{case c:Conditional => c})
            c.conditionOn(mesh)
    }

    def summary:Summary =
    {
        var summary = new Summary
        for (s <- this.subModels.collect{case s:Summarizable => s})
            summary ++= s.summary
        summary
    }
}

class TemplateColorInferenceModel(theSubModels:ModelAsTemplate*) extends TemplateModel(theSubModels:_*) with ColorInferenceModel
{
    import ColorInferenceModel._

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

class ItemizedColorInferenceModel(initialFactors:Factor*) extends ItemizedModel(initialFactors:_*) with ColorInferenceModel
{
    import ColorInferenceModel._

    // Some of this model's factors might come from families, so we must provide
    // a way to retrieve all of those families (particularly important for training)
    override def families: Seq[cc.factorie.Family] =
    {
        this.factors.collect{case f:cc.factorie.Family#Factor => f.family}.toSeq.distinct
    }

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
        for (c <- this.factors.collect{case c:Conditional => c})
            c.conditionOnAll(meshes)
        for (c <- this.families.collect{case c:Conditional => c})
            c.conditionOnAll(meshes)
    }

    def conditionOn(mesh:SegmentMesh)
    {
        for (c <- this.factors.collect{case c:Conditional => c})
            c.conditionOn(mesh)
        for (c <- this.families.collect{case c:Conditional => c})
            c.conditionOn(mesh)
    }

    def summary:Summary =
    {
        var summary = new Summary//new ArrayBuffer[SummaryItem]
        for (s <- this.factors.collect{case s:Summarizable => s})
            summary ++= s.summary
        for (s <- this.families.collect{case s:Summarizable => s})
            summary ++= s.summary
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

trait UnarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, UnarySegmentTemplate.DatumVariable]
    with ColorInferenceModel.Conditional with ColorInferenceModel.Trainable with ColorInferenceModel.Summarizable
{
    import ColorInferenceModel._
    import UnarySegmentTemplate._

    protected def colorPropExtractor:ColorPropertyExtractor
    protected def regressor:HistogramRegressor
    protected val data = new Data
    def propName:String
    def name = propName

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
      data.clear()
      for (mesh<-meshes; seg <- mesh.segments)
      {
        val f = Segment.getUnaryRegressionFeatures(seg)._1
        data(seg) = new DatumVariable(Datum(seg, regressor.predictHistogram(f)))
      }
    }


    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (seg <- mesh.segments)
        {
            val f = Segment.getUnaryRegressionFeatures(seg)._1
            data(seg) = new DatumVariable(Datum(seg, regressor.predictHistogram(f)))
        }
    }

    def summary:Summary =
    {
      val s = new Summary
      val items = data.keys.map(s => new SummaryItem("unarysegment", propName, Array("s"+s.index), data(s).value.hist))
      s.histograms = items.toArray[SummaryItem]

      s.coefficients = Array(new CoefficientsItem("unarysegment", propName, regressor.getCentroids, regressor.getFeatureNames, regressor.getCoefficients))
      s
    }

    protected def trainRegressor(property:ModelTraining#UnarySegmentProperty) : HistogramRegressor =
    {
        println("Training " + name + "...")
        property.regression(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)
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

    val propName = property.name
    protected val colorPropExtractor = property.extractor
    protected val regressor = trainRegressor(property)

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

trait BinarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate3[ColorVar, ColorVar, BinarySegmentTemplate.DatumVariable]
    with ColorInferenceModel.Conditional with ColorInferenceModel.Trainable with ColorInferenceModel.Summarizable
{
    import ColorInferenceModel._
    import BinarySegmentTemplate._

    def propName:String
    def name = propName
    protected def colorPropExtractor:ColorPropertyExtractor
    protected def regressor:HistogramRegressor
    protected val data = new Data

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
      data.clear()
      for (mesh<-meshes; seg1 <- mesh.segments; adj <- seg1.adjacencies if seg1.index < adj.neighbor.index)
      {
        val seg2 = adj.neighbor
        val f = Segment.getBinaryRegressionFeatures(seg1, adj)._1
        data((seg1, seg2)) = new DatumVariable(Datum(seg1, seg2, regressor.predictHistogram(f)))
      }
    }

    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (seg1 <- mesh.segments; adj <- seg1.adjacencies if seg1.index < adj.neighbor.index)
        {
            val seg2 = adj.neighbor
            val f = Segment.getBinaryRegressionFeatures(seg1, adj)._1
            data((seg1, seg2)) = new DatumVariable(Datum(seg1, seg2, regressor.predictHistogram(f)))
        }
    }

  def summary:Summary =
  {
    val s = new Summary
    val items = data.keys.map(s => new SummaryItem("binarysegment", propName, Array("s"+s._1.index, "s"+s._2.index), data(s).value.hist))
    s.histograms = items.toArray[SummaryItem]

    s.coefficients = Array(new CoefficientsItem("binarysegment", propName, regressor.getCentroids, regressor.getFeatureNames, regressor.getCoefficients))

    s
  }


  protected def trainRegressor(property:ModelTraining#BinarySegmentProperty) : HistogramRegressor =
    {
        println("Training " + name + "...")
        property.regression(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)
    }

    protected def computeStatistics(color1:Color, color2:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color1, color2)
        val density = datum.hist.evaluateAt(props)
        var logDensity = MathUtils.safeLog(density)
        // Again, weight by size. This formula should make the total weight sum to 1
        //val sizew  = (datum.seg1.size / datum.seg1.adjacencies.size) + (datum.seg2.size / datum.seg2.adjacencies.size)


        //weight based on adjacency strength seg1->seg2 (which is the number of pixels or seg2 surrounding seg1 divided by
        //the adjacency strengths of everything, including redundant edges like seg2->seg1)
        // since the adjacency strength is still not necessarily symmetric, due to the
        //pixelized nature of the region, add the adjacency strengths of seg2->seg1 and seg1->seg2
        val sizew = (datum.seg1.adjacencies.find(a=>a.neighbor==datum.seg2).get.strength)+(datum.seg2.adjacencies.find(a=>a.neighbor==datum.seg1).get.strength)
        logDensity *= sizew
        Tensor1(logDensity)
    }

    def unroll1(v1:ColorVar) =
    {
        // Find all neighbors of v1, yield a factor for each segment pair
        for (seg1 <- v1.group.members; seg2 <- seg1.adjacencies.map(_.neighbor) if seg1.index < seg2.index)
            yield Factor(v1, seg2.group.color.asInstanceOf[ColorVar], data((seg1,seg2)))
    }

    def unroll2(v2:ColorVar) =
    {
        // Symmetric w.r.t to unroll1
        for (seg2 <- v2.group.members; seg1 <- seg2.adjacencies.map(_.neighbor) if seg1.index < seg2.index)
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

trait ColorGroupTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, ColorGroupTemplate.DatumVariable]
    with ColorInferenceModel.Conditional with ColorInferenceModel.Trainable with ColorInferenceModel.Summarizable
{
    import ColorInferenceModel._
    import ColorGroupTemplate._

    def propName:String
    def name = propName
    protected def colorPropExtractor:ColorPropertyExtractor
    protected def regressor:HistogramRegressor
    protected val data = new Data

    def conditionOnAll(meshes:Seq[SegmentMesh])
    {
      data.clear()
      for (mesh<-meshes; group <- mesh.groups)
      {
        val f = SegmentGroup.getRegressionFeatures(group)._1
        data(group) = new DatumVariable(Datum(group, regressor.predictHistogram(f)))
      }
    }


    def conditionOn(mesh:SegmentMesh)
    {
        data.clear()
        for (group <- mesh.groups)
        {
            val f = SegmentGroup.getRegressionFeatures(group)._1
            data(group) = new DatumVariable(Datum(group, regressor.predictHistogram(f)))
        }
    }

    def summary:Summary =
    {
      val s = new Summary
      val items = data.keys.map(g => new SummaryItem("unarygroup", propName, Array("g"+g.index), data(g).value.hist))
      s.histograms = items.toArray[SummaryItem]

      s.coefficients = Array(new CoefficientsItem("unarygroup", propName, regressor.getCentroids, regressor.getFeatureNames, regressor.getCoefficients))
      s
    }

    protected def trainRegressor(property:ModelTraining#ColorGroupProperty) : HistogramRegressor =
    {
        println("Training " + name + "...")
        property.regression(property.examples, MathUtils.euclideanDistance, property.quant, WekaMultiClassHistogramRegressor)
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
    override def statisticsScore(t:Tensor): Double = weights dot t
}


/*
A family of factors that touch an arbitrary number of the same type of variable
 */
trait FamilyN[V<:Variable] extends Family
{
    type FactorType = Factor

    // Stupid thing from Family that we have to define
    type NeighborType1 = V

    class Factor(varlist:V*) extends FactorN[V](varlist:_*) with super.Factor
    {
        // Another stupid thing from Family#Factor that we need to define
        def _1:NeighborType1 = null.asInstanceOf[NeighborType1]

        // Ignore the red squigglies below--this actually compiles just fine.
        override def equalityPrerequisite: AnyRef = FamilyN.this
        override def score(values:Seq[V#Value]): Double = FamilyN.this.score(values)
        override def statistics(values:Seq[V#Value]): StatisticsType = FamilyN.this.statistics(values)
        def scoreAndStatistics(values:Seq[V#Value]): (Double,StatisticsType) = FamilyN.this.scoreAndStatistics(values)
    }

    /* Methods that the inner Factor class links to */
    def score(values:Seq[V#Value]): Double
    def statistics(values:Seq[V#Value]): StatisticsType = values.asInstanceOf[StatisticsType]
    def scoreAndStatistics(values:Seq[V#Value]): (Double,StatisticsType) = (score(values), statistics(values))
}

trait TensorFamilyN[V<:Variable] extends FamilyN[V] with TensorFamily
{
    override def statistics(values:Seq[V#Value]): Tensor
}

trait DotFamilyN[V<:Variable] extends FamilyN[V] with DotFamily
{
    def score(values:Seq[V#Value]): Double = statisticsScore(statistics(values))
}


object ColorCompatibilityFamily
{
    var matlabProxy:MatlabProxyScalaWrapper = null

    def ensureMatlabConnection()
    {
        if (matlabProxy == null)
            setupMatlabConnection()
    }

    private def setupMatlabConnection()
    {
        println("Setting up MATLAB connection...")

        // Open the connection to matlab
        val options = new MatlabProxyFactoryOptions.Builder().setUsePreviouslyControlledSession(true).build()
        val factory = new MatlabProxyFactory(options)
        val proxy = factory.getProxy
        matlabProxy = new MatlabProxyScalaWrapper(proxy)

        // Set up the workspace for processing color rating queries
        matlabProxy.eval("cd ../odonovan")
        matlabProxy.eval("setup_rating_env")

        println("MATLAB connection set up.")
    }

    // Will we ever actually call this, or will we just let the program terminate? Is there a problem
    // if we do that?
    def closeMatlabConnection()
    {
        matlabProxy.proxy.disconnect()
    }
}

class ColorCompatibilityFamily extends DotFamilyN[ContinuousColorVariable]
    with ColorInferenceModel.Trainable
{
    import ColorInferenceModel._

    def name = "ColorCompatibility"

    // Again, ignore the squiggly--this builds fine.
    final class Factor(varlist:ContinuousColorVariable*) extends super.Factor(varlist:_*) with Conditional
    {
        override def conditionOnAll(meshes:Seq[SegmentMesh])
        {
            //this method doesn't really make sense here...(at least, not yet)
            throw new Error("ColorCompatibilityFactor: conditionOnAll doesn't really make sense here")
        }

        override def conditionOn(mesh:SegmentMesh)
        {
            // This factor touches the 5 largest color groups
            vars.clear()
            val sortedGroups = mesh.groups.sortWith((g1, g2) => g1.size > g2.size)
            for (i <- 0 until 5)
                vars += sortedGroups(i).color.asInstanceOf[ContinuousColorVariable]
        }
    }

    private def colorsToArray(c1:Color, c2:Color, c3:Color, c4:Color, c5:Color) : Array[Double] =
    {
        MathUtils.concatVectors(c1.copyIfNeededTo(RGBColorSpace),
                                c2.copyIfNeededTo(RGBColorSpace),
                                c3.copyIfNeededTo(RGBColorSpace),
                                c4.copyIfNeededTo(RGBColorSpace),
                                c5.copyIfNeededTo(RGBColorSpace)).toArray
    }

    def statistics(c1:Color, c2:Color, c3:Color, c4:Color, c5:Color) : Tensor1 =
    {
        ColorCompatibilityFamily.ensureMatlabConnection()

        // TODO: Permutations?
        val retval = ColorCompatibilityFamily.matlabProxy.returningFeval("getRating", 1, colorsToArray(c1,c2,c3,c4,c5))
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


