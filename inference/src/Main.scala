/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 9/18/12
 * Time: 10:22 AM
 * To change this template use File | Settings | File Templates.
 */

import scala.io.Source
import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import cc.factorie._

object Main
{

    // Some constants
    val logZero = -100000
    val laplaceAlpha = 0.01

    var items: Seq[FurnitureItem] = Nil
    var materials: Seq[String] = Nil
    var categories: Seq[String] = Nil

    case class FurnitureItem(csvline: String)
    {
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

    def loadData(filename: String): Seq[FurnitureItem] =
    {
        Source.fromFile(filename).getLines().map((l: String) => FurnitureItem(l)).toSeq.drop(1)
    }

    def getPossibleMaterials(items: Seq[FurnitureItem]): Seq[String] =
    {
        (HashSet() ++ items.map(item => item.material)).toList
    }

    def getPossibleObjectCategories(items: Seq[FurnitureItem]): Seq[String] =
    {
        (HashSet() ++ items.map(item => item.furnitureType)).toList
    }

    class Scene(var objects: Seq[FurnitureObject])
    {
        objects.foreach(obj => obj.scene = this)

        def objectsWithCategory(category:String) : Seq[FurnitureObject] =
        {
            objects.filter(obj => obj.category == category)
        }

        def observedMaterials = objects.map(obj => obj.material).filter(mat => mat.observed)
        def unobservedMaterials = objects.map(obj => obj.material).filter(mat => !mat.observed)
        def allMaterials = objects.map(obj => obj.material)

        def print() { objects.foreach(obj => obj.print()) }
    }

    class FurnitureObject(val category: String, mat:String = null)
    {
        var scene:Scene = null
        var material: MaterialVariable = null
        if (mat != null)
            material = new MaterialVariable(this, mat)
        else
            material = new MaterialVariable(this)

        def print() { println("FurnitureObject: (Category = %s, Material = %s)".format(category, material.value.category)) }
    }

    object MaterialDomain extends CategoricalDomain(materials)
    class MaterialVariable(val ownerObject:FurnitureObject) extends CategoricalVariable[String]
    {
        var observed = false
        def this(ownerObject:FurnitureObject, initialVal:String) = { this(ownerObject); _set(domain.index(initialVal)); observed = true }
        def domain = MaterialDomain
    }

    class PairwiseMaterialTemplate(val cat1:String, val cat2:String, val probTable: ArrayBuffer[ArrayBuffer[Double]]) extends Template2[MaterialVariable, MaterialVariable] with Statistics2[String, String]
    {
        def score(s: Stat) = probTable(materials.indexOf(s._1))(materials.indexOf(s._2))

        def statistics(values: ValuesType) = Stat(values._1.category, values._2.category)

        def unroll1(mv1: MaterialVariable) = {
            if (mv1.ownerObject.category == cat1)
            {
                val cat2objs = mv1.ownerObject.scene.objectsWithCategory(cat2)
                for (obj <- cat2objs; if obj != mv1.ownerObject) yield Factor(mv1, obj.material)
            }
            else Nil
        }

        def unroll2(mv2: MaterialVariable) = {
            // Symmetric if the two categories are equal
            if (cat1 == cat2)
                Nil
            else if (mv2.ownerObject.category == cat2)
            {
                val cat1objs = mv2.ownerObject.scene.objectsWithCategory(cat1)
                for (obj <- cat1objs; if obj != mv2.ownerObject) yield Factor(obj.material, mv2)
            }
            else Nil
        }

        def printTable()
        {
            println("Nonzero entries for (%s,%s) template:".format(cat1, cat2))
            for (i <- (0 until materials.length); j <- (0 until materials.length))
            {
                if (probTable(i)(j) > logZero)
                {
                    println("(%s,%s) = %g".format(materials(i), materials(j), math.exp(probTable(i)(j))))
                }
            }
            println("")
        }
    }

    // 'alpha' is for Laplace smoothing
    def constructPairwiseTemplate(cat1: String, cat2: String, alpha:Double = 0.0) =
    {
        val cat1items = items.filter(item => item.furnitureType == cat1)
        val cat2items = items.filter(item => item.furnitureType == cat2)
        val occurrencePairs = for (i1 <- cat1items; i2 <- cat2items; if i1 != i2 && i1.collectionId == i2.collectionId) yield (i1, i2)

        val numMaterials = materials.length
        var probTable = ArrayBuffer.fill(numMaterials, numMaterials)(alpha)

        occurrencePairs.foreach(pair => {
            val i1 = materials.indexOf(pair._1.material)
            val i2 = materials.indexOf(pair._2.material)
            probTable(i1)(i2) += 1.0
        })

        // Normalize
        val NplusAlphaD = occurrencePairs.length + alpha*(numMaterials*numMaterials)
        probTable = probTable.map(subtable => subtable.map(entry => entry / NplusAlphaD))

        // Inference operates in log space, so we need to log every table entry
        probTable = probTable.map(subtable => subtable.map(entry => if (entry != 0.0) math.log(entry) else logZero))

        new PairwiseMaterialTemplate(cat1, cat2, probTable)
    }

    def categoriesCoOccur(cat1:String, cat2:String) =
    {
        val cat1items = items.filter(item => item.furnitureType == cat1)
        val cat2items = items.filter(item => item.furnitureType == cat2)
        val occurrencePairs = for (i1 <- cat1items; i2 <- cat2items; if i1 != i2 && i1.collectionId == i2.collectionId) yield (i1, i2)
        occurrencePairs.length > 0
    }

    def main(args: Array[String])
    {
        items = loadData("../data/livingRoomItems.csv")
        materials = getPossibleMaterials(items)
        println(materials)
        categories = getPossibleObjectCategories(items).sorted

        // Build up some scene here
        val scene  = new Scene(List(new FurnitureObject("Couch"),
                                    new FurnitureObject("Couch"),
                                    new FurnitureObject("Chair"),
                                    new FurnitureObject("Table"),
                                    new FurnitureObject("Rug"),
                                    new FurnitureObject("Lamp")))

        val catsInScene = (for (obj <- scene.objects) yield obj.category).sorted

        val templates = for (c1 <- catsInScene; c2 <- catsInScene; if c1 <= c2 && categoriesCoOccur(c1, c2)) yield constructPairwiseTemplate(c1, c2, laplaceAlpha)

//        templates.foreach(t => if (t.cat2 == "Lamp") t.printTable())
//        templates.foreach(t => if (t.cat1 == "Chair" && t.cat2 == "Couch") t.printTable())

        val model = new TemplateModel(templates:_*)
        val summary = new DiscreteSummary1[MaterialVariable](scene.unobservedMaterials)
        val sampler = new VariableSettingsSampler[MaterialVariable](model)
        {
//            override def postProcessHook(mv:MaterialVariable, d:DiffList): Unit =
//            {
//                scene.objects.foreach(obj => print("%s: %s, ".format(obj.category, obj.material.value.category)))
//                println(" (%g)".format(model.score(scene.unobservedMaterials)))
//            }
//
//            override def proposalsHook(proposals:Seq[Proposal]): Unit =
//            {
//                val whichVar = proposals.filter(_.diff.length > 0).map(_.diff(0).variable).toSeq(0).asInstanceOf[MaterialVariable]
//                print("(%s) ".format(whichVar.ownerObject.category))
//                val curScore = model.score(scene.unobservedMaterials)
//                proposals.foreach(prop => {
//                    var propVal:String = null
//                    if (prop.diff.length > 0)
//                        propVal = materials(prop.diff(0).asInstanceOf[DiscreteVariable#DiscreteVariableDiff].newValue)
//                    else
//                        propVal = whichVar.value.category
//                    print("%s=%g, ".format(propVal, prop.modelScore+curScore))
//                })
//                println("")
//            }
        }

//        sampler.temperature = 5.0
//        val inferencer = new SamplingInferencer(sampler, summary)
//        inferencer.process(scene.unobservedMaterials)
//        scene.unobservedMaterials.foreach(mat => {
//            val marginal = summary.marginal(mat)
//            println("Marginal for material of %s:".format(mat.ownerObject.category))
//            println(marginal.proportions)
//            println("")
//        })

        val maximizer = new SamplingMaximizer[MaterialVariable](sampler)
        maximizer.maximize(scene.unobservedMaterials, iterations=500, rounds=5, initialTemperature=10, finalTemperature=0.1)

        scene.print()

        println("Final score: %g".format(model.score(scene.allMaterials)))
    }
}
