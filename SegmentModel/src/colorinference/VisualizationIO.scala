package colorinference

import java.io.FileWriter
import cc.factorie.HashMapAssignment
import collection.mutable.ArrayBuffer

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
       for (c <- centroids)
       {
         val isOrigBin = (centroids(origBin) == c)

         out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c.mkString("-")+"\","+hist.evaluateAt(c)+",\"true\",\""+isOrigBin +"\"\n")

         out.write("\""+patternId+"\",\""+name +"\",\""+prop+"\",\""+ ids.mkString("-")+"\",\""+c.mkString("-")+"\","+bins(idx)++",\"false\",\""+isOrigBin +"\"\n")
         idx += 1
       }
     }
     out.close()

   }


  def OutputAllPermutations(mesh:SegmentMesh, model:ColorInferenceModel, palette:ColorPalette, filename:String)
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

          out.write("\"%s\",\"%s\",\"%s\",\"%s\",%f".format(s.ttype, s.propname, bn.mkString("-"),fn,coeff(f)(b)))
        }
      }
    }

    out.close()
  }

}
