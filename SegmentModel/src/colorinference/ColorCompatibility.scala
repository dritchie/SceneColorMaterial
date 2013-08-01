package colorinference

import breeze.linalg._
import breeze.math._
import io.Source
import cc.factorie.la.Tensor1

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 7/27/13
 * Time: 10:24 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * Port of O'Donovan et al's color compatibility model to Scala
 */

//CHSV color space
//conversions assume that RGB is in the range 0-1 and HSV has hue in degrees with S and V between 0 and 1
object CHSVColorSpace extends ColorSpace
{
  override def toString: String = "CHSV"
  val hueRemap = PiecewisePolynomial.load("odonovan/hueRemap.txt")

  def toRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
  {
 //TODO: invert this
    throw new Exception("CHSV toRGB: Not implemented yet")
  }

  def fromRGB(c1: Double, c2: Double, c3: Double): (Double, Double, Double) =
  {
     val (h,s,v) = HSVColorSpace.fromRGB(c1, c2, c3)
     val remap =  hueRemap.evalAt(h/360.0)
     (s*math.cos(360*remap.toRadians), s* -1 *math.sin(360*remap.toRadians), v)
  }

  def clamp(components:Tensor1)
  {
    components(0) = components(0) % 360
    components(1) = MathUtils.clamp(components(1), 0, 1)
    components(2) = MathUtils.clamp(components(2), 0, 1)
  }
}

object PiecewisePolynomial
{
  def load(filename:String):PiecewisePolynomial =
  {
    //first line are the breaks
    //next lines are the coefficients
    val source = Source.fromFile(filename)
    val lines = source.getLines().toArray[String]
    val order = lines(0).toInt
    val breaks = lines(1).split(',').map(s => s.toDouble)
    val coefficients = new DenseMatrix[Double](breaks.length-1, order+1)

    val numLines = breaks.length-1
    for (i<-0 until numLines)
    {
      val data:Array[Double] = lines(i+2).split(',').map(s => s.toDouble)
      coefficients(i,::) := DenseVector(data).t
    }


    new PiecewisePolynomial(breaks, coefficients, order)
  }
}

class PiecewisePolynomial(val breaks:Array[Double], val coefficients:DenseMatrix[Double], val order:Int) {

  def evalAt(x:Double):Double =
  {
     //find the breaks and associated coefficients to use
     var idx = java.util.Arrays.binarySearch(breaks,x)

     if (idx < 0)
       idx = -1*(idx+1)

     if (breaks(idx) > x)
       idx-=1

     var result:Double = 0.0
     val delta = x-breaks(idx)
     for (i<-0 to order)
       result += coefficients(idx,i) * math.pow(delta,order-i)

    result
  }
}

object ColorCompatibilityModel {
  val weightsFile:String = "odonovan/compatWeights.txt"
  val hjFile:String = "odonovan/hueJoint.txt"
  val haFile:String = "odonovan/hueAdj.txt"
  val hpFile:String = "odonovan/hueProb.txt"
  val weights:DenseVector[Double] = loadVector(weightsFile)
  val hueJoint:DenseMatrix[Double] = loadMatrix(hjFile)
  val hueAdj:DenseMatrix[Double] = loadMatrix(haFile)
  val hueProb:DenseVector[Double] = loadVector(hpFile)

  private def loadMatrix(file:String):DenseMatrix[Double] =
  {
    val source = Source.fromFile(file)
    val lines = source.getLines().toArray[String]
    val matrix = DenseMatrix.zeros[Double](lines.length, lines(0).split(',').length)
    for (i <- lines.indices)
      matrix(i,::) := DenseVector(lines(i).split(',').map(s => s.toDouble)).t

    matrix
  }

  private def loadVector(file:String):DenseVector[Double] =
  {
    val source = Source.fromFile(file)
    val lines = source.getLines().toArray[String]

    var w = DenseVector.zeros[Double](lines.length)
    for (i <- lines.indices)
      w(i) = lines(i).toDouble

    w
  }

  def getBasicStats(x:DenseVector[Double]):DenseVector[Double] =
  {
     if (x.length == 0)
      return DenseVector[Double](0,0,0,0,0,0,0,0)

     //mean, std, min, max, meanlog, stdlog, minlog, maxlog
     val logx:DenseVector[Double] = x.map(e => math.log(e+0.000001))

     val check = (d:Double)=> {if (d.isNaN || (d.isInfinity)) 0.0 else d}

     DenseVector[Double](check(mean(x)), check(stddev(x)), check(x.min), check(max(x)), check(mean(logx)), check(stddev(logx)), check(logx.min), check(max(logx)))

  }

  def pca2(matrix:DenseMatrix[Double]):(DenseMatrix[Double],DenseMatrix[Double],DenseVector[Double]) =
  {
    val Y  = matrix :/ math.sqrt(matrix.rows-1)

    val (u,s,pt) = svd(Y)
    //calculate the variances
    val v = s :* s

    //project the original data
    val signals = pt * matrix.t

    //seems like the principal components are returned transposed as compared to matlab
    (signals, pt.t, v)
  }

  def getPlaneFeatures(matrixOrig:DenseMatrix[Double]):DenseVector[Double] =
  {
    val matrix = matrixOrig.copy
    val means = sum(matrix, Axis._0) :/ (matrix.rows.toDouble)

    //subtract the means
    for (i <- 0 until matrix.rows)
      matrix(i,::) :-= means

    val (signals,coeff,roots) = pca2(matrix)
    val normal = coeff(::,2)
    if (normal(0) < 0)
      normal :*= -1.0

    val pctExplained:DenseVector[Double] = {if (sum(roots)==0) DenseVector.zeros[Double](3) else roots :/ sum(roots)}
    val temp:DenseVector[Double] = matrix*normal
    val error:DenseVector[Double] = temp.map(e=>math.abs(e))
    error:^=2.0
    val sse = sum(error)

    DenseVector.vertcat[Double](normal, pctExplained, DenseVector[Double](sse))

  }

  def circVMPdf(alpha:DenseVector[Double], thetahat:Double):DenseVector[Double] =
  {
    //kappa set to 2*pi
    //besseli(0, 2*pi) = 87.1085
    val kappa = 2*math.Pi
    val besseli = 87.1085
    val C = 1.0/(2.0*math.Pi*besseli)
    val vcos = (v:DenseVector[Double]) => v.map(e => math.cos(e))
    val vexp = (v:DenseVector[Double]) => v.map(e => math.exp(e))
    val p = vexp(vcos(alpha-thetahat)*kappa) * C
    p
  }

  implicit def IdxSeq2DenseVector(seq:IndexedSeq[Double]):DenseVector[Double] = DenseVector[Double](seq.toArray)

  def getHueProbs(palette:Array[Array[Double]], satThresh:Double=0.2):DenseVector[Double] =
  {
    val validHues = palette.filter(c => c(1) >= satThresh).map(c => (math.round(c(0)*359)+1).toInt)
    val hueJointList = DenseVector.zeros[Double]((1 to validHues.length).sum)
    var idx = 0
    for (i<-validHues.indices)
    {
      for (j<-i until validHues.length)
      {
        hueJointList(idx) = hueJoint(validHues(i)-1, validHues(j)-1)
        idx += 1
      }
    }

    val hueAdjList = (1 until validHues.length).map(i => hueAdj(validHues(i-1)-1, validHues(i)-1))
    val hueProbList = validHues.map(h => hueProb(h-1))

    val entropy = {

      if (validHues.length > 0)
      {
        val alpha = linspace(0, 2*math.Pi, 361).slice(0, 360)
        val pMix:DenseVector[Double] = validHues.foldLeft(DenseVector.ones[Double](alpha.length)*0.001)((b,a) => b + circVMPdf(alpha, a*2*math.Pi))
        val normPMix:DenseVector[Double] = pMix/pMix.sum
        val logPMix:DenseVector[Double] = normPMix.map(e=>math.log(e))
        val entropy = -1*sum(normPMix :* logPMix )
        entropy

      } else
        5.9 //set the entropy high
    }

    //concatenate all the features
    val hpf = getBasicStats(DenseVector(hueProbList.toArray[Double]))
    val hjf = getBasicStats(hueJointList)
    val haf = getBasicStats(DenseVector(hueAdjList.toArray[Double]))

    DenseVector.vertcat(hpf, hjf, haf, DenseVector[Double](entropy))

  }

  def getAllFeatures(palette:Array[Color]):DenseVector[Double] =
  {
    val colorspaces = Array(CHSVColorSpace, LABColorSpace, HSVColorSpace, RGBColorSpace)
    var allFeatures = DenseVector[Double]()
    for (cspace <- colorspaces)
    {
      //if it's LAB space, the original O'Donovan code does not gamma correct
      //so make up for that
      val convertedPalette =
      {
        val gamma = 2.2
        if (cspace != LABColorSpace)
          palette.map(c => c.copyIfNeededTo(cspace).toArray).toArray
        else
          palette.map(c => {
            val temp = new Color(c)
            temp(0) = math.pow(temp(0), 1.0/gamma)
            temp(1) = math.pow(temp(1), 1.0/gamma)
            temp(2) = math.pow(temp(2), 1.0/gamma)
            temp.copyTo(LABColorSpace).toArray
          }).toArray
      }

      //normalize
      if (cspace == HSVColorSpace)
        for (c<-convertedPalette.indices)
          convertedPalette(c)(0) = convertedPalette(c)(0)/360.0 //normalize between 0 and 1
      else if (cspace == LABColorSpace)
        for (c<-convertedPalette.indices)
        {
          convertedPalette(c)(0) = convertedPalette(c)(0)/100.0
          convertedPalette(c)(1) = convertedPalette(c)(1)/128.0
          convertedPalette(c)(2) = convertedPalette(c)(2)/128.0
        }

      //color coordinates
      val coords = DenseVector(convertedPalette.map(c => c.toArray).flatten)

      //sorted colors by 3rd dimension
      val sortedCoords = DenseVector(convertedPalette.sortBy(c => c(2)).map(c => c.toArray).flatten)

      //get pairwise channel differences  (by channel)
      val pairwiseDiffs = (palette:Array[Array[Double]], channel:Int) => {for (i<-1 until palette.length) yield palette(i)(channel) - palette(i-1)(channel)}
      val diffs1 = {
        if (cspace == HSVColorSpace) {
          (1 until convertedPalette.length).map(i => {val hdiff:Double= math.abs(convertedPalette(i)(0)-convertedPalette(i-1)(0)); math.min(hdiff, 1.0-hdiff)})
        } else
          pairwiseDiffs(convertedPalette,0)
      }

      val diffs2 = pairwiseDiffs(convertedPalette,1)
      val diffs3 = pairwiseDiffs(convertedPalette,2)

      //pairwise sorted channel differences (sorted within each channel)
      val sorteddiffs1 = diffs1.sortBy(e => -1*e)
      val sorteddiffs2 = diffs2.sortBy(e => -1*e)
      val sorteddiffs3 = diffs3.sortBy(e => -1*e)

      //mean, stddev, median, max, min of each channel
      //populate the matrix
      var matrix:DenseMatrix[Double] = new DenseMatrix[Double](convertedPalette.length, 3)

      for (i <- convertedPalette.indices)
        matrix(i,::) := DenseVector(convertedPalette(i)(0), convertedPalette(i)(1), convertedPalette(i)(2)).t

      val medianFnc = (v:DenseVector[Double]) => {val sorted = v.toArray.sortBy(e=>e); sorted((v.length/2.0).toInt)}

      val means = (0 until matrix.cols).map(a => mean(matrix(::,a)))
      val stddevs = (0 until matrix.cols).map(a => stddev(matrix(::,a)))
      val medians = (0 until matrix.cols).map(a => medianFnc(matrix(::, a)))
      val maxs = (0 until matrix.cols).map(a => max(matrix(::,a)))

      //breeze min() at this snapshot is broken...
      val mins = (0 until matrix.cols).map(a => matrix(::,a).min)


      //max-min difference for each channel
      val maxMin = (0 until matrix.cols).map(a => maxs(a)-mins(a))

      //plane features (if not hsv)
      //get hue probability features (if hsv)
      val extraFeatures = {if (cspace != HSVColorSpace) getPlaneFeatures(matrix) else getHueProbs(convertedPalette) }

      val newFeatures = DenseVector.vertcat(coords, sortedCoords, diffs1, diffs2, diffs3,
        sorteddiffs1, sorteddiffs2, sorteddiffs3,means,
        stddevs, medians, maxs, mins, maxMin, extraFeatures)

      allFeatures = DenseVector.vertcat(allFeatures, newFeatures)

    }
    allFeatures
  }

  def getRating(palette:Array[Color]):Double =
  {
    //calculate the features
    //chsv, lab, hsv, rgb
    val allFeatures = getAllFeatures(palette)

    //now compute the score
    val score = weights.t*DenseVector.vertcat(allFeatures, DenseVector[Double](1.0))
    score(0)

  }




}

object ColorCompatibilityTest
{
  def main(args:Array[String])
  {

    val pp = CHSVColorSpace.hueRemap.evalAt(0.3380)
    println(pp)

    val iRGB = Array[Color](Color.RGBColor(0.1176, 0.5373, 0.1294),
                            Color.RGBColor(0.0863, 0.2745, 0.6824),
                            Color.RGBColor(0.9176, 0.7922, 0.0314),
                            Color.RGBColor(0.6902, 0.1098, 0.0314),
                            Color.RGBColor(0.0863, 0.2745, 0.6824))

    val rating = ColorCompatibilityModel.getRating(iRGB)

    println(rating)
    //Should be: 2.5729


    //compare individual features
    val loadList = (file:String) => {
        val source = Source.fromFile(file)
        val lines = source.getLines().toArray[String]

        var w = Array.ofDim[String](lines.length)
        for (i <- lines.indices)
          w(i) = lines(i)
        w
    }

    val featureNames = loadList("odonovan/featureNames.txt")
    val trueFeatures = loadList("odonovan/testFeatures.txt").map(s => s.toDouble)
    val myFeatures = ColorCompatibilityModel.getAllFeatures(iRGB)
    val epsilon = 0.01
    for (f <- trueFeatures.indices)
    {
      if (math.abs(trueFeatures(f)-myFeatures(f))>epsilon)
        println(featureNames(f) + " expected: " + trueFeatures(f) + ", got: " + myFeatures(f))
    }



  }
}
