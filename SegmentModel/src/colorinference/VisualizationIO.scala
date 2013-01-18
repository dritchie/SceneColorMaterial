package colorinference

import java.io.FileWriter
import cc.factorie.HashMapAssignment
import collection.mutable.ArrayBuffer
import cc.factorie.la.{DenseTensor1, Tensor, Tensor1}
import colorinference.ColorInferenceModel.Named

/**
 * Created with IntelliJ IDEA.
 * User: sharon
 * Date: 12/30/12
 * Time: 5:10 PM
 * To change this template use File | Settings | File Templates.
 */
object VisualizationIO {
   //methods that output visualization-related files
   def OutputHistograms(model:ColorInferenceModel, filename:String, patternId:Int, append:Boolean, headers:Boolean)
   {
     //output the histograms in a csv format, indicate which bin is the original bin
     val out = new FileWriter(filename, append)
     if (headers)
       out.write("\"pattern\",\"factortype\",\"property\",\"ids\",\"bin\",\"value\",\"smoothed\",\"origBin\"\n")

     val summary = model.summary

     for (s <- summary.histograms)
     {
       val name = s.ttype
       val prop = s.propname
       val ids = s.ids
       val hist = s.hist

       val centroids = hist.getCentroids
       val bins = hist.getBins
       val origBin = hist.getBin(s.origValue)
       var idx = 0
       var smoothedCentroids = centroids


       var numDim = centroids(0).dim1
       if (numDim == 1)
       {
         var quantizer = new UniformVectorQuantizer(for (i<-0 until numDim) yield 50)
         //val t = quantizer.apply(centroids, MathUtils.euclideanDistance)
         //hardcode to between 0 and 1
         val t = quantizer.apply(Array(Tensor1(0), Tensor1(1)), MathUtils.euclideanDistance)

         smoothedCentroids = t._1
       }
       if (numDim <= 3)
       {
         for (c <- centroids)
         {
           val isOrigBin = (centroids(origBin) == c)
           //out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c.mkString("-")+"\","+hist.evaluateAt(c)+",\"true\",\""+isOrigBin +"\"\n")
           out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c.mkString("-")+"\","+bins(idx)++",\"false\",\""+isOrigBin +"\"\n")
           idx += 1
         }

         for (c<- smoothedCentroids)
         {
           val isOrigBin = (centroids(origBin) == c)       //TODO: this doesn't work for smoothed histograms anymore, unless the quantizer exactly hits an original bin
           out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c.mkString("-")+"\","+hist.evaluateAt(c)+",\"true\",\""+isOrigBin +"\"\n")
         }
       } else {
         for (c <- centroids.indices)
         {
           val isOrigBin = (centroids(origBin) == centroids(c))
           //out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c.mkString("-")+"\","+hist.evaluateAt(c)+",\"true\",\""+isOrigBin +"\"\n")
           out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c+"\","+bins(idx)++",\"false\",\""+isOrigBin +"\"\n")
           idx += 1
         }

         for (c<- smoothedCentroids.indices)
         {
           val isOrigBin = (centroids(origBin) == smoothedCentroids(c))       //TODO: this doesn't work for smoothed histograms anymore, unless the quantizer exactly hits an original bin
           out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c+"\","+hist.evaluateAt(smoothedCentroids(c))+",\"true\",\""+isOrigBin +"\"\n")
         }
       }

     }
     out.close()

   }

  case class PermutationScore(colors:Seq[Color], score:Double)
  def OutputAllPermutations(mesh:SegmentMesh, model:ColorInferenceModel, palette:ColorPalette, filename:String):Seq[PermutationScore] =
  {
    //output all the permutations in order of score, indicate which one is the original
    val numVals = DiscreteColorVariable.domain.size
    val vars = mesh.groups.map(g => g.color)
    val allPerms = (0 until numVals).toList.permutations.toList

    //store the permutation index and the score in a list
    var results = ArrayBuffer[(Int, Double)]()
    val itemizedModel = model.itemizedModel(vars)

    //add the results
    var idx = 0
    for (p <- allPerms)
    {
      val assignment = new HashMapAssignment(vars)
      for (i <- mesh.groups.indices)
      {
        assignment.update(mesh.groups(i).color.asInstanceOf[DiscreteColorVariable], DiscreteColorVariable.domain(p(i)))
      }
      for (f <- itemizedModel.factors)
      {
        f.variables.foreach{ e => e match {
          case(v:UnarySegmentTemplate.DatumVariable) => assignment.update(v, v.value)
          case(b:BinarySegmentTemplate.DatumVariable) => assignment.update(b, b.value)
          case(g:ColorGroupTemplate.DatumVariable) => assignment.update(g, g.value)
          case _ => null
        }}
      }

      val currScore = model.assignmentScore(vars, assignment)

      //store the permutation index and the score into the results list
      results += ((idx, currScore))

      idx += 1
    }

    results = results.sortBy(t => -1*t._2)



    //write the file
    //Start with Score number isOrig
    //then color assignments
    val out = new FileWriter(filename)
    out.write("Count " + allPerms.length +"\n")
    for (r <- results)
    {
      val p = allPerms(r._1)
      val score = r._2

      //check if it is the original
      var orig = true
      for (i <- mesh.groups.indices)
      {
        if (palette(p(i)).distance(mesh.groups(i).color.observedColor) > 0)
          orig = false
      }

      out.write("Score " + score + " " + orig+"\n")
      for (i <- mesh.groups.indices)
      {
        out.write(palette(p(i)).copyIfNeededTo(RGBColorSpace).componentString + "\n")
      }
    }
    out.close()

    results.map(r => {
      val colors = allPerms(r._1).map(c => palette(c))
      new PermutationScore(colors, r._2)
    })

  }

  //assuming the mesh has a filename that ends in the pid.txt
  def getPid(mesh:SegmentMesh):Int =
  {
    var last = mesh.name.split('\\').last
    last.replace(".txt","").toInt
  }

  case class PermutationStats(pid:Int, artistRank:Int, artistL:Double, minL:Double, maxL:Double, avgL:Double, stdL:Double)
  def OutputPermutationStats(mesh:SegmentMesh, model:ColorInferenceModel, palette:ColorPalette):PermutationStats =
  {
    //output all the permutations in order of score, indicate which one is the original
    val numVals = DiscreteColorVariable.domain.size
    val vars = mesh.groups.map(g => g.color)
    val allPerms = (0 until numVals).toList.permutations.toList

    //store the permutation index and the score in a list
    var results = ArrayBuffer[(Int, Double)]()
    val itemizedModel = model.itemizedModel(vars)

    //add the results
    var idx = 0
    var minL = 1.0
    var maxL = 0.0
    var avgL = 0.0
    var stdL = 0.0
    var artL = 0.0
    var artRank = 0
    var Z = math.exp(ExhaustiveInference.logZAllPermutations(mesh, model))
    for (p <- allPerms)
    {
      val assignment = new HashMapAssignment(vars)
      for (i <- mesh.groups.indices)
      {
        assignment.update(mesh.groups(i).color.asInstanceOf[DiscreteColorVariable], DiscreteColorVariable.domain(p(i)))
      }

      for (f <- itemizedModel.factors)
      {
        f.variables.foreach{ e => e match {
          case(v:UnarySegmentTemplate.DatumVariable) => assignment.update(v, v.value)
          case(b:BinarySegmentTemplate.DatumVariable) => assignment.update(b, b.value)
          case(g:ColorGroupTemplate.DatumVariable) => assignment.update(g, g.value)
          case _ => null
        }}
      }

      val currScore = model.assignmentScore(vars, assignment)
      avgL += math.exp(currScore)
      minL = math.min(minL, math.exp(currScore))
      maxL = math.max(maxL, math.exp(currScore))

      //store the permutation index and the score into the results list
      results += ((idx, currScore))

      idx += 1
    }

    results = results.sortBy(t => -1*t._2)

    for (i <-results.indices)
    {
      val r = results(i)
      val p = allPerms(r._1)
      val score = r._2

      //check if it is the original
      var orig = true
      for (i <- mesh.groups.indices)
      {
        if (palette(p(i)).distance(mesh.groups(i).color.observedColor) > 0)
          orig = false
      }
      if (orig)
      {
        artRank = i+1
        artL = math.exp(score)
      }
    }



    avgL /= allPerms.length


    stdL = (for (r<-results) yield math.pow(math.exp(r._2)-avgL,2.0)).sum
    stdL /= allPerms.length
    stdL = math.sqrt(stdL)

    //normalize by the partition function
    stdL /= Z
    avgL /= Z
    minL /= Z
    maxL /= Z
    artL /= Z

    val stats = new PermutationStats(getPid(mesh), artRank, artL, minL, maxL, avgL, stdL)

    println("Pattern %d".format(getPid(mesh)))
    println("ArtistRank: %d, AL: %f, minL %f, maxL %f, avgL %f, stdL %f".format(stats.artistRank, artL, minL, maxL, avgL, stdL))

    stats
  }



  def OutputCoefficients(model:ColorInferenceModel, filename:String)
  {
    //output the coefficients of the regression if applicable
    val out = new FileWriter(filename)
    val summary = model.summary
    out.write("\"factortype\",\"property\",\"bin\",\"feature\",\"coefficient\"\n")
    for (s<- summary.coefficients)
    {
      //output the factortype, property name, bin, feature, and coefficient
      val coeff = s.coefficients
      val numFeatures = coeff.length

      if (coeff.length>0)
      {
        val numBins = coeff(0).length

        for (b <- 0 until numBins; f <- 0 until numFeatures)
        {
          val fn = {if (s.featureNames != null && f<s.featureNames.length) s.featureNames(f) else "noname"}
          val bn = s.classes(b)

          out.write("\"%s\",\"%s\",\"%s\",\"%s\",%f\n".format(s.ttype, s.propname, bn.mkString("-"),fn,coeff(f)(b)))
        }
      }
    }

    out.close()
  }

  //get weighted term scores
  case class TermScores(names:Seq[String], weightedScores:DenseTensor1)
  {
    def + (other:TermScores):TermScores = new TermScores(names, new DenseTensor1(weightedScores + other.weightedScores) )

    def - (other:TermScores):TermScores = new TermScores(names, new DenseTensor1(weightedScores - other.weightedScores ))

    def / (number:Double):TermScores = new TermScores(names, new DenseTensor1(weightedScores/number))

    def print()
    {
      for (i <- names.indices)
      {
        println("%s: %f".format(names(i), weightedScores(i)))
      }
    }

  }

  def GetTermScores(model:ColorInferenceModel, mesh:SegmentMesh, colors:Seq[Color]):TermScores =
  {
    //save the original colors
    model.conditionOn(mesh)
    val orig = mesh.groups.map(_.color.getColor)
    val variables = mesh.groups.map(_.color)

    //set the color
    for (i<-colors.indices)
      mesh.groups(i).color.setColor(colors(i))

    val scores = {for (t <- model.templates) yield
    {
      //print out the term scores
      val score = t.itemizedModel(variables).currentScore
      val weight = t.weightsTensor.toArray(0) //assume these are single-valued tensors

      score*weight

    } }

    val names = {for (t<-model.templates) yield t match {
      case n:Named => n.name
      case _ => ""
    }}

    new TermScores(names, new DenseTensor1(scores.toArray))

  }


}
