/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 9/20/12
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

import io.Source
import collection.immutable.HashSet

object Data {

    case class CSVFurnitureItem(csvline: String) {
        val tokens = csvline.split(",")
        val collectionId = tokens(0)
        val id = tokens(1)
        val name = tokens(2)
        val collectionName = tokens(3)
        val subordinateFurnitureType = tokens(4)
        val detailsUrl = tokens(5)
        val material = tokens(6)
        val furnitureType = tokens(7)
        val roomType = tokens(8)
        val color = tokens(9)
    }

    def loadCSVData(filename: String): Seq[CSVFurnitureItem] = {
        Source.fromFile(filename).getLines().map((l: String) => CSVFurnitureItem(l)).toSeq.drop(1)
    }

    def getPossibleMaterials(items: Seq[CSVFurnitureItem]): Seq[String] = {
        (HashSet() ++ items.map(item => item.material)).toList
    }

    def categoriesCoOccur(items:Seq[CSVFurnitureItem], cat1:String, cat2:String) =
    {
        val cat1items = items.filter(item => item.furnitureType == cat1)
        val cat2items = items.filter(item => item.furnitureType == cat2)
        val occurrencePairs = for (i1 <- cat1items; i2 <- cat2items; if i1 != i2 && i1.collectionId == i2.collectionId) yield (i1, i2)
        occurrencePairs.length > 0
    }
}
