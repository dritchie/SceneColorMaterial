/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 11/2/12
 * Time: 12:32 AM
 * To change this template use File | Settings | File Templates.
 */


import cc.factorie.{SamplingMaximizer, VariableSettingsSampler}
import collection.mutable
import collection.mutable._
import java.io.File
import cc.factorie._
import la.Tensor1

class TrainingSamples() {
   val contrasts = Map[(Int,Int), ArrayBuffer[ConditionalHistogramRegressor.RegressionExample]]()//ArrayBuffer[(Double, Double, Double)]]()
   val contrastRegressor = mutable.Map[(Int,Int), ConditionalHistogramRegressor]()
}


/*class ContrastDistFactor(v1:DiscreteColorVariable, v2:DiscreteColorVariable, private val cbandwidth:Double, private val sbandwidth:Double) extends Factor2(v1, v2)
{

}*/

object PatternMain {
  //Just a place to test loading and evaluating patterns/images. Subject to much change
  val inputDir = "../PatternColorizer/out/mesh"
  val outputDir = "../PatternColorizer/out/specs"

  var meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]()
  var files:Array[File] = null

  def main(args:Array[String])
  {
    //load all files
    files = new File(inputDir).listFiles.filter(_.getName.endsWith(".txt"))

    if (files.length == 0)
      println("No files found in the input directory!")

    for (f <- files)
    {
      meshes.append(new SegmentMesh(f.getAbsolutePath))
    }

    //for now, just try the contrast model on the images, using the original palette
    //probably highly overfitting..
    var avgScore:Double = 0

    for (idx <- meshes.indices)
    {
      println("Testing mesh " + idx)
      val segmesh = meshes(idx)

      //get all the other meshes
      val trainingMeshes:Array[SegmentMesh] = {for (tidx<-meshes.indices if tidx != idx) yield meshes(tidx)}.toArray
      val samples = getSamples(trainingMeshes)


      val model = buildModel(segmesh, samples)//MaintainObservedContrastModel(segmesh)
      val palette = new ColorPalette(segmesh)
      DiscreteColorVariable.initDomain(palette.colors)

      // Do inference
      val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
      val optimizer = new SamplingMaximizer(sampler)
      optimizer.maximize(for (group <- segmesh.groups) yield group.color, 100)

      // Output the result
      segmesh.saveColorAssignments(outputDir+"/"+files(idx).getName)

      //Evaluate assignments
      val score = segmesh.scoreAssignment()
      println(files(idx).getName+" Score: "+score)
      avgScore += score

    }
    println("Average Score: " + (avgScore/meshes.length))

  }



  def getLabels = (seg:Segment) => {
    val list = seg.features.filter(f => f.name == "Label").map(f => f.values(0))
    list(0).toInt
  }
  def getSizes = (seg:Segment) => {
    val list = seg.features.filter(f => f.name == "RelativeSize").map(f => f.values(0))
    list(0)
  }

  def getSamples(trainingMeshes:Array[SegmentMesh]):TrainingSamples =
  {
    val samples = new TrainingSamples()      //(Label, Label) => (Size, Size, Contrast)

    //initialize the map keys
    /*for (s <- 0 until 2)
    {
       for (b <- s until 2)
       {
          samples.contrasts += ((s,b) -> new ArrayBuffer[ConditionalHistogramRegressor.RegressionExample]())
       }
    }*/

    for (mesh <- trainingMeshes)
    {
      for (seg <- mesh.segments)
      {
        val label = getLabels(seg)
        val size = getSizes(seg)

        //get labels and contrast for adjacent elements of a higher index, so we don't double count
        //list of tuples (label1, label2, contrast)
        val neighbors = seg.adjacencies.filter(n => n.index > seg.index)
        val vals = neighbors.map(n => {
          val nlabel = getLabels(n)
          (nlabel, getSizes(n), Color.contrast(n.group.color.observedColor, seg.group.color.observedColor) )
        })


        //add samples to the dictionary. Tuple, order smaller label first
        for ((n,s,c) <- vals)
        {

          if (!samples.contrasts.contains((Math.min(n, label), Math.max(n, label))))
            samples.contrasts += ((Math.min(n, label),Math.max(n, label)) -> new ArrayBuffer[ConditionalHistogramRegressor.RegressionExample]())

           if (n < label)
             samples.contrasts((n,label)) += ConditionalHistogramRegressor.RegressionExample(Tensor1(c), Tensor1(s,size)) //((s, size, c))
          else
             samples.contrasts((label,n)) += ConditionalHistogramRegressor.RegressionExample(Tensor1(c), Tensor1(size, s))//((size, s, c))

        }
      }
    }

    //train the regressors
    println("Start training regressors")
    for ((l1, l2) <- samples.contrasts.keys)
    {
      println("step " + l1 + " " + l2)
      val seq = samples.contrasts((l1,l2)).toSeq
      println("seq length " + seq.length)
      samples.contrastRegressor += ((l1,l2) -> new ConditionalHistogramRegressor(seq, MathUtils.euclideanDistance, new KMeansVectorQuantizer(5)))
    }
    println("Done training regressors")


    samples

  }

  def buildModel(targetMesh:SegmentMesh, samples:TrainingSamples): ItemizedModel =
  {
    //return a model with factors
    val model = new ItemizedModel()

    //for each pair of adjacent regions in the target mesh, add a factor for the contrast, smaller label first
    for (seg <- targetMesh.segments)
    {
      val size = getSizes(seg)
      val label = getLabels(seg)

      val neighbors = seg.adjacencies.filter(n=> n.index > seg.index)
      val vals = neighbors.map(n=> {
        val nlabel = getLabels(n)
        (nlabel, getSizes(n), n.group)
      })

      //create the factors
      for ((n,s,g)<-vals)
      {
        if (n < label)
        {
          val distribution = samples.contrastRegressor((n, label)).predictHistogram(Tensor1(s, size))
          model += new ContrastPriorFactor2(g.color, seg.group.color, distribution)

        } else
        {
          val distribution = samples.contrastRegressor((n, label)).predictHistogram(Tensor1(size, s))
          model += new ContrastPriorFactor2(seg.group.color, g.color, distribution)
        }
      }

      //contrast for now, TODO: color, saturation, hue?

    }

    model
  }




}
