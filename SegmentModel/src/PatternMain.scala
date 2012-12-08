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
import scala.util.Random

class TrainingSamples() {
  //binary features
   val contrasts = Map[(Int,Int), ArrayBuffer[HistogramRegressor.RegressionExample]]()//ArrayBuffer[(Double, Double, Double)]]()
   val contrastRegressor = mutable.Map[(Int,Int), HistogramRegressor]()

  //unary features
   val lightness = Map[Int, ArrayBuffer[HistogramRegressor.RegressionExample]]()
   val lightnessRegressor = mutable.Map[Int, HistogramRegressor]()
   val saturation = Map[Int, ArrayBuffer[HistogramRegressor.RegressionExample]]()
   val saturationRegressor = mutable.Map[Int, HistogramRegressor]()

   //testing the vector histogram
   val lightnessVH = Map[Int, VectorHistogram]()
   val saturationVH = Map[Int, VectorHistogram]()
   val contrastVH = Map[(Int,Int), VectorHistogram]()
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
  val numBins = 10
  val random = new Random()

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

    //for now, just try using the original palette
    var avgScore:Double = 0
    var count = 0
    var randScore:Double = 0

    for (idx <- meshes.indices if idx < 50)
    {
      println("Testing mesh " + idx)
      val segmesh = meshes(idx)
      count += 1
      //get all the other meshes
      val trainingMeshes:Array[SegmentMesh] = {for (tidx<-meshes.indices if tidx != idx) yield meshes(tidx)}.toArray
      val samples = getSamples(trainingMeshes)


      val model = buildModelVH(segmesh, samples)//buildModel(segmesh, samples) //MaintainObservedContrastModel(segmesh)
      val palette = new ColorPalette(segmesh)
      DiscreteColorVariable.initDomain(palette.colors)

      // Do inference
      val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
      val optimizer = new SamplingMaximizer(sampler)
      optimizer.maximize(for (group <- segmesh.groups) yield group.color, 100)

      // Output the result
      segmesh.saveColorAssignments(outputDir+"/"+files(idx).getName)

      // Evaluate assignments
      val score = segmesh.scoreAssignment()
      println(files(idx).getName+" Score: "+score)
      avgScore += score

      //Evaluate random assignment (3 trials)
      var rscore = 0.0
      for (t <- 0 until 3)
      {
        val assign = RandomAssignment(segmesh, palette)
        rscore += segmesh.scoreAssignment(assign)
      }
      rscore /= 3.0

      println("Rand Score: " + rscore)

      randScore += rscore


    }


    println("Average Score: " + (avgScore/count))
    println("Average Random Score: " + (randScore/count))

  }

  def RandomAssignment(segmesh:SegmentMesh, palette:ColorPalette):Seq[Color] =
  {
     segmesh.groups.map(g => palette.colors(random.nextInt(palette.colors.length)))
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
    val samples = new TrainingSamples()

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
            samples.contrasts += ((Math.min(n, label),Math.max(n, label)) -> new ArrayBuffer[HistogramRegressor.RegressionExample]())

           if (n < label)
             samples.contrasts((n,label)) += HistogramRegressor.RegressionExample(Tensor1(c), Tensor1(s,size)) //((s, size, c))
          else
             samples.contrasts((label,n)) += HistogramRegressor.RegressionExample(Tensor1(c), Tensor1(size, s))//((size, s, c))

        }

        //get unary features, use consistent color space?
        val lab = seg.group.color.observedColor.copyTo(LABColorSpace)
        val lightness = lab(0)
        val hsv = seg.group.color.observedColor.copyTo(HSVColorSpace)
        val saturation = hsv(1)

        if (!samples.lightness.contains(label))
          samples.lightness += label -> new ArrayBuffer[HistogramRegressor.RegressionExample]
        if (!samples.saturation.contains(label))
          samples.saturation += label -> new ArrayBuffer[HistogramRegressor.RegressionExample]

        samples.lightness(label) += HistogramRegressor.RegressionExample(Tensor1(lightness), Tensor1(size))
        samples.saturation(label) += HistogramRegressor.RegressionExample(Tensor1(saturation), Tensor1(size))

      }
    }




    samples

  }

  def buildModel(targetMesh:SegmentMesh, samples:TrainingSamples): ItemizedModel =
  {

    //train the regressors
    println("Start training regressors")
    for ((l1, l2) <- samples.contrasts.keys)
    {
      println("step " + l1 + " " + l2)
      val seq = samples.contrasts((l1,l2)).toSeq
      println("seq length " + seq.length)
      samples.contrastRegressor += ((l1,l2) -> new SVMLightHistogramRegressor(seq, MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins)))

    }
    for (l <- samples.lightness.keys)
    {
      samples.lightnessRegressor += l -> new SVMLightHistogramRegressor(samples.lightness(l).toSeq, MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins))
      samples.saturationRegressor += l -> new SVMLightHistogramRegressor(samples.saturation(l).toSeq, MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins))
    }

    println("Done training regressors")




    //return a model with factors
    val model = new ItemizedModel()

    //for each pair of adjacent regions in the target mesh, add a factor for the contrast, smaller label first
    for (seg <- targetMesh.segments)
    {
      val size = getSizes(seg)
      val label = getLabels(seg)

      val getLightness = (color:Color) => (color.copyTo(LABColorSpace)(0))
      val getSaturation = (color:Color) => (color.copyTo(HSVColorSpace)(1))


      //for each individual region in the target mesh, add a unary factor
      model += new FeaturePriorFactor(seg.group.color, samples.lightnessRegressor(label).predictHistogram(Tensor1(size)), getLightness)
      model += new FeaturePriorFactor(seg.group.color, samples.saturationRegressor(label).predictHistogram(Tensor1(size)), getSaturation)

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
          val distribution = samples.contrastRegressor((label, n)).predictHistogram(Tensor1(size, s))
          model += new ContrastPriorFactor2(seg.group.color, g.color, distribution)
        }
      }

      //contrast for now, TODO: color, saturation, hue?

    }

    model
  }

  //test building the model using the vector histogram (no features)
  def buildModelVH(targetMesh:SegmentMesh, samples:TrainingSamples): ItemizedModel =
  {
    println("Building model")


    val stripFeatures = (list:ArrayBuffer[HistogramRegressor.RegressionExample]) => list.map(e => e.target).toSeq


    //precompute the Vector Histograms
    for (label <- samples.lightness.keys)
    {
        samples.lightnessVH += label -> VectorHistogram(stripFeatures(samples.lightness(label)) , MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins))
        samples.saturationVH += label -> VectorHistogram(stripFeatures(samples.saturation(label)) , MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins))
    }

    for ((l1,l2) <- samples.contrasts.keys)
    {
      samples.contrastVH += (l1,l2) -> VectorHistogram(stripFeatures(samples.contrasts((l1,l2))), MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins))
    }




    //return a model with factors
    val model = new ItemizedModel()

    //for each pair of adjacent regions in the target mesh, add a factor for the contrast, smaller label first
    for (seg <- targetMesh.segments)
    {
      val size = getSizes(seg)
      val label = getLabels(seg)

      val getLightness = (color:Color) => (color.copyTo(LABColorSpace)(0))
      val getSaturation = (color:Color) => (color.copyTo(HSVColorSpace)(1))


      //for each individual region in the target mesh, add a unary factor
      model += new FeaturePriorFactor(seg.group.color, samples.lightnessVH(label), getLightness)
      model += new FeaturePriorFactor(seg.group.color, samples.saturationVH(label), getSaturation)

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
          val distribution = samples.contrastVH((n,label))
          model += new ContrastPriorFactor2(g.color, seg.group.color, distribution)

        } else
        {
          val distribution = samples.contrastVH((label, n))
          model += new ContrastPriorFactor2(seg.group.color, g.color, distribution)
        }
      }

      //contrast for now, TODO: color, saturation, hue?

    }
    println("Done building model")
    model
  }




}
