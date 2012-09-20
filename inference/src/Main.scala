/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 9/18/12
 * Time: 10:22 AM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

object Main
{
    def main(args: Array[String])
    {
        val items = Data.loadCSVData("../data/livingRoomItems.csv")
        
        val materials = Data.getPossibleMaterials(items)
        MaterialVariable.initDomain(materials)
        println(materials)

        // Build up some scene here
        val scene  = new Scene(List(new FurnitureObject("Couch"),
                                    new FurnitureObject("Couch"),
                                    new FurnitureObject("Chair"),
                                    new FurnitureObject("Table"),
                                    new FurnitureObject("Rug"),
                                    new FurnitureObject("Lamp")))

        val templates = PairwiseMaterialTemplate.constructAllFromCSVData(items, scene)

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
