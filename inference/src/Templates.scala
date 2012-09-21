/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 9/20/12
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

import collection.mutable.ArrayBuffer
import cc.factorie._

abstract class PairwiseMaterialTemplate(val cat1:String, val cat2:String) extends Template2[MaterialVariable, MaterialVariable]

class PairwiseMultinomialMaterialTemplate(cat1: String, cat2: String, val probTable: ArrayBuffer[ArrayBuffer[Double]]) extends PairwiseMaterialTemplate(cat1,cat2) with Statistics2[String, String] {
    def score(s: Stat) = probTable(MaterialVariable.domain.index(s._1))(MaterialVariable.domain.index(s._2))

    def statistics(values: ValuesType) = Stat(values._1.category, values._2.category)

    def unroll1(mv1: MaterialVariable) = {
        if (mv1.ownerObject.category == cat1) {
            val cat2objs = mv1.ownerObject.scene.objectsWithCategory(cat2)
            for (obj <- cat2objs; if obj != mv1.ownerObject) yield Factor(mv1, obj.material)
        }
        else Nil
    }

    def unroll2(mv2: MaterialVariable) = {
        // Symmetric if the two categories are equal
        if (cat1 == cat2)
            Nil
        else if (mv2.ownerObject.category == cat2) {
            val cat1objs = mv2.ownerObject.scene.objectsWithCategory(cat1)
            for (obj <- cat1objs; if obj != mv2.ownerObject) yield Factor(obj.material, mv2)
        }
        else Nil
    }

    def printTable() {
        println("Nonzero entries for (%s,%s) template:".format(cat1, cat2))
        for (i <- 0 until MaterialVariable.domain.length; j <- 0 until MaterialVariable.domain.length) {
            if (probTable(i)(j) > PairwiseMultinomialMaterialTemplate.logZero) {
                println("(%s,%s) = %g".format(MaterialVariable.domain.dimensionName(i), MaterialVariable.domain.dimensionName(j), math.exp(probTable(i)(j))))
            }
        }
        println("")
    }
}

object PairwiseMultinomialMaterialTemplate
{
    /* Constants */
    private val laplaceAlpha = 0.01         // For Laplace smoothing
    private val logZero = -100000           // For avoiding NaNs during inference

    def constructAllFromCSVData(items:Seq[Data.CSVFurnitureItem], scene:Scene) =
    {
        val catsInScene = (for (obj <- scene.objects) yield obj.category).sorted
        for (c1 <- catsInScene; c2 <- catsInScene; if c1 <= c2 && Data.categoriesCoOccur(items, c1, c2)) yield constructOneFromCSVData(items, c1, c2)
    }

    private def constructOneFromCSVData(items:Seq[Data.CSVFurnitureItem], cat1:String, cat2:String) =
    {
        val cat1items = items.filter(item => item.furnitureType == cat1)
        val cat2items = items.filter(item => item.furnitureType == cat2)
        val occurrencePairs = for (i1 <- cat1items; i2 <- cat2items; if i1 != i2 && i1.collectionId == i2.collectionId) yield (i1, i2)

        val numMaterials = MaterialVariable.domain.length
        var probTable = ArrayBuffer.fill(numMaterials, numMaterials)(laplaceAlpha)

        occurrencePairs.foreach(pair => {
            val i1 = MaterialVariable.domain.index(pair._1.material)
            val i2 = MaterialVariable.domain.index(pair._2.material)
            probTable(i1)(i2) += 1.0
        })

        // Normalize
        val NplusAlphaD = occurrencePairs.length + laplaceAlpha * (numMaterials * numMaterials)
        probTable = probTable.map(subtable => subtable.map(entry => entry / NplusAlphaD))

        // Inference operates in log space, so we need to log every table entry
        probTable = probTable.map(subtable => subtable.map(entry => if (entry != 0.0) math.log(entry) else logZero))

        new PairwiseMultinomialMaterialTemplate(cat1, cat2, probTable)
    }
}
