package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 11/2/12
 * Time: 12:32 AM
 * To change this template use File | Settings | File Templates.
 */


import cc.factorie.{SamplingMaximizer, VariableSettingsSampler}
import collection.mutable.Seq
import scala.collection.mutable
import scala.collection.mutable._
import java.io.File
import cc.factorie._
import la.Tensor1
import scala.util.Random

object UnaryProperties extends Enumeration {
  type Property = Value
  val Lightness, Saturation = Value
}

object BinaryProperties extends Enumeration {
   type Property = Value
   val Contrast = Value
}


class TrainingSamples() {
  //typedefs for clarity
  type RegressionExamples = ArrayBuffer[HistogramRegressor.RegressionExample]
  type LabeledRegressionExamples = mutable.Map[(Int, Int), RegressionExamples]

  //labeled samples
  val labeledBinarySamples = mutable.Map[(BinaryProperties.Property), LabeledRegressionExamples]
  val binarySamples = mutable.Map[(BinaryProperties.Property), RegressionExamples]

  //non-labeled samples
  val labeledUnarySamples = mutable.Map[UnaryProperties.Property, LabeledRegressionExamples]
  val unarySamples = mutable.Map[UnaryProperties.Property, RegressionExamples]

  //labeled regressors
  val labeledRegressors = mutable.Map[Int, HistogramRegressor]()

  //non-labeled regressors
  val regressors = mutable.Map[Int, HistogramRegressor]()

}


object PatternMain {
  //Just a place to test loading and evaluating patterns/images. Subject to much change
  val inputDir = "../PatternColorizer/out/mesh"
  val outputDir = "../PatternColorizer/out/specs"

  var meshes:ArrayBuffer[SegmentMesh] = new ArrayBuffer[SegmentMesh]()
  var files:Array[File] = null
  val numBins = 10
  val random = new Random()
  val ignoreLabels = false

  def main(args:Array[String])
  {
    //load all files
    files = new File(inputDir).listFiles.filter(_.getName.endsWith(".txt"))

    if (files.length == 0)
      println("No files found in the input directory!")

    for (f <- files)
    {
      meshes.append(new SegmentMesh(DiscreteColorVariable, f.getAbsolutePath))
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


      val model = buildModel(segmesh, samples, HistogramRegressor.KNN)//buildModel(segmesh, samples) //MaintainObservedContrastModel(segmesh)
      val palette = ColorPalette(segmesh)
      DiscreteColorVariable.initDomain(palette)

      // Do inference
      val sampler = new VariableSettingsSampler[DiscreteColorVariable](model)
      val optimizer = new SamplingMaximizer(sampler)
      optimizer.maximize(for (group <- segmesh.groups) yield group.color.asInstanceOf[DiscreteColorVariable], 1000)

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

  def RandomAssignment(segmesh:SegmentMesh, palette:ColorPalette) : Seq[Color] =
  {
     segmesh.groups.map(g => palette(random.nextInt(palette.length)))
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

  def buildModel(targetMesh:SegmentMesh, samples:TrainingSamples, regress:(Seq[HistogramRegressor.RegressionExample], MathUtils.DistanceMetric, VectorQuantizer, WekaHistogramRegressor)=>(HistogramRegressor)): ItemizedModel =
  {
    //train the regressors
    println("Start training regressors")

    if (!ignoreLabels)
    {
      for ((l1, l2) <- samples.contrasts.keys)
      {
        println("step " + l1 + " " + l2)
        var seq = samples.contrasts((l1,l2))
        println("seq length " + seq.length)
        samples.contrastRegressor += ((l1,l2) -> regress(seq, MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins), WekaMultiClassHistogramRegressor))

      }
      for (l <- samples.lightness.keys)
      {
        samples.lightnessRegressor += l -> regress(samples.lightness(l), MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins), WekaMultiClassHistogramRegressor)
        samples.saturationRegressor += l -> regress(samples.saturation(l), MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins), WekaMultiClassHistogramRegressor)
      }
    } else {

      val allContrasts = {for(k<-samples.contrasts.keys; v<-samples.contrasts(k)) yield v}.toArray
      val allLightness = {for (k<-samples.lightness.keys; v<-samples.lightness(k)) yield v}.toArray
      val allSaturation = {for (k<-samples.saturation.keys; v<-samples.saturation(k)) yield v}.toArray

      val contrastRegressor = regress(allContrasts, MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins), WekaMultiClassHistogramRegressor)
      val lightnessRegressor = regress(allLightness, MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins), WekaMultiClassHistogramRegressor)
      val saturationRegressor = regress(allSaturation, MathUtils.euclideanDistance, new KMeansVectorQuantizer(numBins), WekaMultiClassHistogramRegressor)

      for ((l1, l2) <- samples.contrasts.keys)
      {
        samples.contrastRegressor += ((l1,l2) -> contrastRegressor)
      }
      for (l <- samples.lightness.keys)
      {
        samples.lightnessRegressor += l -> lightnessRegressor
        samples.saturationRegressor += l -> saturationRegressor
      }


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
      model += new FeaturePriorFactor(seg.group.color.asInstanceOf[DiscreteColorVariable], samples.lightnessRegressor(label).predictHistogram(Tensor1(size)), getLightness)
      model += new FeaturePriorFactor(seg.group.color.asInstanceOf[DiscreteColorVariable], samples.saturationRegressor(label).predictHistogram(Tensor1(size)), getSaturation)

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
          model += new ContrastPriorFactor2(g.color.asInstanceOf[DiscreteColorVariable], seg.group.color.asInstanceOf[DiscreteColorVariable], distribution)

        } else
        {
          val distribution = samples.contrastRegressor((label, n)).predictHistogram(Tensor1(size, s))
          model += new ContrastPriorFactor2(seg.group.color.asInstanceOf[DiscreteColorVariable], g.color.asInstanceOf[DiscreteColorVariable], distribution)
        }
      }

      //contrast for now, TODO: color, saturation, hue?

    }

    model
  }



}
