/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/9/12
 * Time: 11:51 AM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import la.{Tensor2, DenseTensor1, SparseBinaryTensor1}

// Log-linear-ization of a multinomial unary factor over the different colors
class ColorPriorTemplate(val catName:String) extends DotTemplate1[DiscreteColorVariable]
{
    lazy val weights = new DenseTensor1(DiscreteColorVariable.domain.length)
    private val catIndex = ObjectCategory.domain.index(catName)

    override def statistics(v1:DiscreteColorVariable#Value) =
    {
        val features = new SparseBinaryTensor1(DiscreteColorVariable.domain.length)
        features := 0
        //val currAssignmentIndex = DiscreteColorVariable.domain.index(v1.category)
        val currAssignmentIndex = v1.intValue
        features.update(currAssignmentIndex, 1)
        features
    }

    override def unroll1(v:DiscreteColorVariable) =
    {
        if (v.ownerObject.category.intValue == catIndex)
            Factor(v)
        else
            Nil
    }
}

// Log-linear factor modeling the correlational relationship between two colors
class ColorCorrelationTemplate(val catName1:String, val catName2:String, private val categoryConnectivityMatrix:Tensor2) extends DotTemplate2[DiscreteColorVariable, DiscreteColorVariable]
{
    // TODO: How many features do we need, exactly?
    lazy val weights = new DenseTensor1(4)

    private val catIndex1 = ObjectCategory.domain.index(catName1)
    private val catIndex2 = ObjectCategory.domain.index(catName2)

    override def statistics(v1:DiscreteColorVariable#Value, v2:DiscreteColorVariable#Value) =
    {
        val features = new DenseTensor1(4)
        // TODO: Fill in features!
        features
    }

    override def unroll1(v1:DiscreteColorVariable) =
    {
        if (v1.ownerObject.category.intValue == catIndex1)
        {
            val cat2objs = v1.ownerObject.scene.objectsOfCategory(catIndex2)
            for (obj <- cat2objs; if obj != v1.ownerObject) yield Factor(v1, obj.color)
        }
        else Nil
    }

    override def unroll2(v2:DiscreteColorVariable) =
    {
        // Symmetric if the two categories are equal
        if (catIndex1 == catIndex2)
            Nil
        else if (v2.ownerObject.category.intValue == catIndex2)
        {
            val cat1objs = v2.ownerObject.scene.objectsOfCategory(catIndex1)
            for (obj <- cat1objs; if obj != v2.ownerObject) yield Factor(obj.color, v2)
        }
        else Nil
    }
}

// Log-linear factor modeling how closely a color conforms to a color palette
class ColorPaletteMatchingTemplate(val catName:String, private val colorPalette:ColorPalette) extends DotTemplate1[DiscreteColorVariable]
{
    lazy val weights = new DenseTensor1(1)
    private val catIndex = ObjectCategory.domain.index(catName)

    override def statistics(v1:DiscreteColorVariable#Value) =
    {
        val features = new DenseTensor1(1)
        // TODO: Fill in features!
        features
    }

    override def unroll1(v:DiscreteColorVariable) =
    {
        if (v.ownerObject.category.intValue == catIndex)
            Factor(v)
        else
            Nil
    }
}