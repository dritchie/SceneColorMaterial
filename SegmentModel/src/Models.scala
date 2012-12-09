/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import cc.factorie.la.Tensor1

type UnaryColorPropertyExtractor = Color => Tensor1
type BinaryColorPropertyExtractor = (Color, Color) => Tensor1

trait UnarySegmentTemplate
{
    def colorPropExtractor:UnaryColorPropertyExtractor
    def computeStatistics(color:Color, hist:VectorHistogram) : Tensor1  =
    {
        val props = colorPropExtractor(color)
        val density = hist.evaluateAt(props)
        Tensor1(math.log(density))
    }
}

trait BinarySegmentTemplate
{
    def colorPropExtractor:BinaryColorPropertyExtractor
    def computeScore(color1:Color, color2:Color, hist:VectorHistogram) : Tensor1 =
    {
        val props = colorPropExtractor(color1, color2)
        val density = hist.evaluateAt(props)
        Tensor1(math.log(density))
    }
}

// A factorie-friendly way to pass VectorHistograms to the score/statistcs functions
// in factor templates
type VecHistVariable = RefVariable[VectorHistogram]


//class DiscreteUnarySegmentTemplate(val colorPropExtractor:UnaryColorPropertyExtractor)
//    extends DotTemplate2[DiscreteColorVariable, VecHistVariable] with UnarySegmentTemplate
//{
//}