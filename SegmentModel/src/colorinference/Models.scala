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
import collection.mutable.HashMap

object ModelConfigOptions
{
    val doSizeWeighting = true
}

object UnarySegmentTemplate
{
    type ColorPropertyExtractor = Color => Tensor1
    case class Datum(seg:Segment, hist:VectorHistogram)
    type InputData = Seq[Datum]
    type DatumVariable = RefVariable[Datum]
    protected type InternalData = HashMap[Segment, DatumVariable]
}

trait UnarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, UnarySegmentTemplate.DatumVariable]
{
    import UnarySegmentTemplate._

    lazy val weights = new DenseTensor1(1)

    protected def colorPropExtractor:ColorPropertyExtractor
    protected def data:InternalData

    def setWeight(w:Double) { weights.update(0, w) }

    protected def genInternalData(input:InputData) =
    {
        val output = new InternalData()
        for (datum <- input)
            output(datum.seg) = new DatumVariable(datum)
        output
    }

    protected def computeStatistics(color:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color)
        val density = datum.hist.evaluateAt(props)
        var logDensity = MathUtils.safeLog(density)
        // Weight by relative size so that groups with tons of little segments don't get
        // unfairly emphasized
        if (ModelConfigOptions.doSizeWeighting)
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

class DiscreteUnarySegmentTemplate(val name:String, protected val colorPropExtractor:UnarySegmentTemplate.ColorPropertyExtractor,
                                   inputData:UnarySegmentTemplate.InputData)
    extends DotTemplate2[DiscreteColorVariable, UnarySegmentTemplate.DatumVariable] with UnarySegmentTemplate[DiscreteColorVariable]
{
    import UnarySegmentTemplate._

    protected val data = genInternalData(inputData)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2)
    }
}

class ContinuousUnarySegmentTemplate(val name:String, protected val colorPropExtractor:UnarySegmentTemplate.ColorPropertyExtractor,
                                     inputData:UnarySegmentTemplate.InputData)
    extends DotTemplate2[ContinuousColorVariable, UnarySegmentTemplate.DatumVariable] with UnarySegmentTemplate[ContinuousColorVariable]
{
    import UnarySegmentTemplate._

    protected val data = genInternalData(inputData)

    override def statistics(v1:ContinuousColorVariable#Value, v2:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1, v2)
    }
}



object BinarySegmentTemplate
{
    type ColorPropertyExtractor = (Color, Color) => Tensor1
    case class Datum(seg1:Segment, seg2:Segment, hist:VectorHistogram)
    type InputData = Seq[Datum]
    type DatumVariable = RefVariable[Datum]
    protected type InternalData = HashMap[(Segment,Segment), DatumVariable]
}

trait BinarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate3[ColorVar, ColorVar, BinarySegmentTemplate.DatumVariable]
{
    import BinarySegmentTemplate._

    lazy val weights = new DenseTensor1(1)

    protected def colorPropExtractor:ColorPropertyExtractor
    protected def data:InternalData

    def setWeight(w:Double) { weights.update(0, w) }

    protected def genInternalData(input:InputData) : InternalData =
    {
        val output = new InternalData()
        for (datum <- input)
            output((datum.seg1, datum.seg2)) = new DatumVariable(datum)
        output
    }

    protected def computeStatistics(color1:Color, color2:Color, datum:Datum) : Tensor1  =
    {
        val props = colorPropExtractor(color1, color2)
        val density = datum.hist.evaluateAt(props)
        var logDensity = MathUtils.safeLog(density)
        // Again, weight by size. This formula should make the total weight sum to 1
        if (ModelConfigOptions.doSizeWeighting)
        {
            val sizew  = (datum.seg1.size / datum.seg1.adjacencies.size) + (datum.seg2.size / datum.seg2.adjacencies.size)
            logDensity *= sizew
        }
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

class DiscreteBinarySegmentTemplate(val name:String, protected val colorPropExtractor:BinarySegmentTemplate.ColorPropertyExtractor,
                                   inputData:BinarySegmentTemplate.InputData)
    extends DotTemplate3[DiscreteColorVariable, DiscreteColorVariable, BinarySegmentTemplate.DatumVariable] with BinarySegmentTemplate[DiscreteColorVariable]
{
    import BinarySegmentTemplate._

    protected val data = genInternalData(inputData)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DiscreteColorVariable#Value, v3:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1.category, v2.category, v3)
    }
}

class ContinuousBinarySegmentTemplate(val name:String, protected val colorPropExtractor:BinarySegmentTemplate.ColorPropertyExtractor,
                                     inputData:BinarySegmentTemplate.InputData)
    extends DotTemplate3[ContinuousColorVariable, ContinuousColorVariable, BinarySegmentTemplate.DatumVariable] with BinarySegmentTemplate[ContinuousColorVariable]
{
    import BinarySegmentTemplate._

    protected val data = genInternalData(inputData)

    override def statistics(v1:ContinuousColorVariable#Value, v2:ContinuousColorVariable#Value, v3:DatumVariable#Value) : Tensor =
    {
        computeStatistics(v1, v2, v3)
    }
}

