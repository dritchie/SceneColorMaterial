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

/*
 TODO: IntelliJ refused to compile this file when it had a top-level type declaration (DataVariable), so I stuck everything in an object to make it happy. Long term, we should probably put everything in a package.
  */
object Templates
{
    // Sticking data in a variable is a factorie design pattern for making that data available
    // to factors for computing suff stats / scores
    type SegmentTemplateDataVariable = RefVariable[VectorHistogram]

    trait UnarySegmentTemplateTypes
    {
        type ColorPropertyExtractor = Color => Tensor1
        type InputData = HashMap[Segment, VectorHistogram]
        protected type InternalData = HashMap[Segment, SegmentTemplateDataVariable]
    }

    trait UnarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate2[ColorVar, SegmentTemplateDataVariable] with UnarySegmentTemplateTypes
    {
        lazy val weights = new DenseTensor1(1)

        protected def colorPropExtractor:ColorPropertyExtractor
        protected def data:InternalData

        protected def genInternalData(input:InputData) : InternalData =
        {
            val output = new InternalData()
            for ((k, v) <- input)
            {
                val tup = (k, new SegmentTemplateDataVariable(v))
                output += tup
            }
            output
        }

        protected def computeStatistics(color:Color, hist:VectorHistogram) : Tensor1  =
        {
            val props = colorPropExtractor(color)
            val density = hist.evaluateAt(props)
            Tensor1(math.log(density))
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
        def unroll2(v2:SegmentTemplateDataVariable) =
        {
            Nil
        }
    }

    class DiscreteUnarySegmentTemplate(protected val colorPropExtractor:UnarySegmentTemplateTypes#ColorPropertyExtractor,
                                       inputData:UnarySegmentTemplateTypes#InputData)
        extends DotTemplate2[DiscreteColorVariable, SegmentTemplateDataVariable] with UnarySegmentTemplate[DiscreteColorVariable]
    {
        protected val data = genInternalData(inputData)

        override def statistics(v1:DiscreteColorVariable#Value, v2:SegmentTemplateDataVariable#Value) : Tensor =
        {
            computeStatistics(v1.category, v2)
        }
    }

    class ContinuousUnarySegmentTemplate(protected val colorPropExtractor:UnarySegmentTemplateTypes#ColorPropertyExtractor,
                                         inputData:UnarySegmentTemplateTypes#InputData)
        extends DotTemplate2[ContinuousColorVariable, SegmentTemplateDataVariable] with UnarySegmentTemplate[ContinuousColorVariable]
    {
        protected val data = genInternalData(inputData)

        override def statistics(v1:ContinuousColorVariable#Value, v2:SegmentTemplateDataVariable#Value) : Tensor =
        {
            computeStatistics(v1, v2)
        }
    }



    trait BinarySegmentTemplateTypes
    {
        type ColorPropertyExtractor = (Color, Color) => Tensor1
        type InputData = HashMap[(Segment ,Segment), VectorHistogram]
        protected type InternalData = HashMap[(Segment ,Segment), SegmentTemplateDataVariable]
    }

    trait BinarySegmentTemplate[ColorVar<:ColorVariable] extends DotTemplate3[ColorVar, ColorVar, SegmentTemplateDataVariable] with BinarySegmentTemplateTypes
    {
        lazy val weights = new DenseTensor1(1)

        protected def colorPropExtractor:ColorPropertyExtractor
        protected def data:InternalData

        protected def genInternalData(input:InputData) : InternalData =
        {
            val output = new InternalData()
            for ((k, v) <- input)
            {
                val tup = (k, new SegmentTemplateDataVariable(v))
                output += tup
            }
            output
        }

        protected def computeStatistics(color1:Color, color2:Color, hist:VectorHistogram) : Tensor1  =
        {
            val props = colorPropExtractor(color1, color2)
            val density = hist.evaluateAt(props)
            Tensor1(math.log(density))
        }

        def unroll1(v1:ColorVar) =
        {
            // Find all neighbors of v1, yield a factor for each segment pair
            for (seg1 <- v1.group.members; seg2 <- seg1.adjacencies)
                yield Factor(v1, seg2.group.color.asInstanceOf[ColorVar], data((seg1,seg2)))
        }

        def unroll2(v2:ColorVar) =
        {
            // Symmetric w.r.t to unroll1
            for (seg2 <- v2.group.members; seg1 <- seg2.adjacencies)
                yield Factor(seg1.group.color.asInstanceOf[ColorVar], v2, data((seg1,seg2)))
        }

        // This will never be called, since the DataVariable never changes
        def unroll3(v3:SegmentTemplateDataVariable) =
        {
            Nil
        }
    }

    class DiscreteBinarySegmentTemplate(protected val colorPropExtractor:BinarySegmentTemplateTypes#ColorPropertyExtractor,
                                       inputData:BinarySegmentTemplateTypes#InputData)
        extends DotTemplate3[DiscreteColorVariable, DiscreteColorVariable, SegmentTemplateDataVariable] with BinarySegmentTemplate[DiscreteColorVariable]
    {
        protected val data = genInternalData(inputData)

        override def statistics(v1:DiscreteColorVariable#Value, v2:DiscreteColorVariable#Value, v3:SegmentTemplateDataVariable#Value) : Tensor =
        {
            computeStatistics(v1.category, v2.category, v3)
        }
    }

    class ContinuousBinarySegmentTemplate(protected val colorPropExtractor:BinarySegmentTemplateTypes#ColorPropertyExtractor,
                                         inputData:BinarySegmentTemplateTypes#InputData)
        extends DotTemplate3[ContinuousColorVariable, ContinuousColorVariable, SegmentTemplateDataVariable] with BinarySegmentTemplate[ContinuousColorVariable]
    {
        protected val data = genInternalData(inputData)

        override def statistics(v1:ContinuousColorVariable#Value, v2:ContinuousColorVariable#Value, v3:SegmentTemplateDataVariable#Value) : Tensor =
        {
            computeStatistics(v1, v2, v3)
        }
    }
}

