package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 11:24 AM
 * To change this template use File | Settings | File Templates.
 */

import collection.mutable.ArrayBuffer
import collection.mutable.HashSet
import collection.mutable.HashMap
import cc.factorie.la.Tensor1
import io.Source
import java.io.FileWriter
import collection.mutable


class Segment(val index:Int, val owner:SegmentMesh)
{
    val features = new mutable.HashMap[String, Tensor1]
    val adjacencies = new HashSet[Segment]
    var group : SegmentGroup = null

    // Quick-access to features that are accessed frequently
    // in inference inner loop
    var size:Double = 0.0

    def extractQuickAccessFeatures()
    {
        size = features("RelativeSize")(0)
    }
}

class SegmentGroup(val index:Int, val owner:SegmentMesh)
{
    var color:ColorVariable = null
    val members = new ArrayBuffer[Segment]
    val adjacencies = new HashSet[SegmentGroup]
//    val features = new mutable.HashMap[String, Tensor1]
//
//    // Extracts features from its member segments
//    def extractFeatures()
//    {
//        //
//    }
}

class SegmentMesh(private val gen:ColorVariableGenerator)
{
    /** Data members **/
    val segments = new ArrayBuffer[Segment]
    val groups = new ArrayBuffer[SegmentGroup]

    /** Constructor **/
    def this(gen:ColorVariableGenerator, filename:String)
    {
        this(gen)

        val adjacencies = new ArrayBuffer[ ArrayBuffer[Int] ]

        val source = Source.fromFile(filename)
        val lineIterator = source.getLines()
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next
            val tokens = line.split(" ")
            tokens(0) match
            {
                case "SegmentBegin" => parseSegment(adjacencies, lineIterator)
                case "GroupBegin" => parseGroup(lineIterator)
                case _ =>
            }
        }
        source.close()

        // Finalize segment adjacencies
        for (i <- 0 until segments.length)
        {
            val adjlist = adjacencies(i)
            val segment = segments(i)
            segment.adjacencies ++= (for (index <- adjlist) yield segments(index))
        }

        // Finalize group adjacencies
        for (group <- groups)
        {
            val adjgroups = for (seg <- group.members; aseg <- seg.adjacencies) yield aseg.group
            group.adjacencies ++= adjgroups.distinct
        }
    }

    /** Private helpers **/

    private def parseSegment(adj:ArrayBuffer[ArrayBuffer[Int]], lineIterator:Iterator[String])
    {
        val newseg = new Segment(segments.length, this)
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next
            val tokens = line.split(" ")
            tokens(0) match
            {
                case "AdjacentTo" =>
                {
                    val adjlist = new ArrayBuffer[Int]
                    for (tok <- tokens slice(1, tokens.length))
                    {
                        adjlist += tok.toInt
                    }
                    adj += adjlist
                }
                case "SegmentEnd" =>
                {
                    newseg.extractQuickAccessFeatures()
                    segments += newseg
                    return
                }
                case _ =>
                {
                    val featureName = tokens(0)
                    val featureVals = Tensor1(tokens.length-1)
                    for (i <- 1 until tokens.length)
                    {
                        featureVals.update(i-1, tokens(i).toDouble)
                    }
                    newseg.features(featureName) = featureVals
                }
            }
        }
    }

    private def parseGroup(lineIterator:Iterator[String])
    {
        var newgroup = new SegmentGroup(groups.length, this)
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next
            val tokens = line.split(" ")
            tokens(0) match
            {
                case "ObservedColor" =>
                {
                    // Assuming that colors are given in RGB
                    val c = Color.RGBColor(tokens(1).toDouble, tokens(2).toDouble, tokens(3).toDouble)
                    newgroup.color = gen(newgroup, c)
                }
                case "Members" =>
                {
                    for (tok <- tokens slice(1,tokens.length))
                    {
                        val seg = segments(tok.toInt)
                        seg.group = newgroup
                        newgroup.members += seg
                    }
                }
                case "GroupEnd" =>
                {
                    groups += newgroup
                    return
                }
                case _ =>
            }
        }
    }

    /** Output **/
    def saveColorAssignments(filename:String)
    {
        val out = new FileWriter(filename)
        for (group <- groups)
        {
            out.write(group.color.getColor.componentString + "\n")
        }
        out.close()
    }

    /** Evaluation **/
    def scoreAssignment():Double =
    {
      //just scoring by the color difference between the observed and assigned colors of each group, weighted uniformly
      //the smaller the score, the better
      val diffs = groups.map(
        g=>{if (g.color.observedColor==null) 0 else g.color.observedColor.distance(g.color.getColor)}
      )
      diffs.sum/groups.length
    }

  def scoreAssignment(assign:Seq[Color]):Double =
  {
    var diffs = 0.0

    for (i <- groups.indices)
    {
      if (groups(i).color.observedColor == null)
        diffs += 0.0
      else
        diffs += groups(i).color.observedColor.distance(assign(i))
    }

    diffs/groups.length

  }
}
