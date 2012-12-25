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
import cc.factorie.la.DenseTensor1
import io.Source
import java.io.FileWriter
import collection.mutable

class SegmentAdjacency(val neighbor:Segment, val strength:Double, val originEnclosure:Double, val neighborEnclosure:Double)

class Segment(val index:Int, val owner:SegmentMesh)
{
    val features = new mutable.HashMap[String, Tensor1]
    val adjacencies = new HashSet[SegmentAdjacency]
    var group : SegmentGroup = null

    // Quick-access to features that are accessed frequently
    // in inference inner loop
    var size:Double = 0.0
    var isNoise:Boolean = false

    def extractQuickAccessFeatures()
    {
        size = features("RelativeSize")(0)
        isNoise = features("Label")(2) == 1
    }
}
object Segment
{

    def getUnaryRegressionFeatures(seg:Segment) : Tensor1 =
    {
        val blockList = Set("HuMoments","RelativeCentroid")
        val featureList = seg.features.filterKeys(name => !blockList.contains(name)).values

        //check for Nans
        for (f<-featureList; i<-f)
        {
          assert(i == i, "Segment: getUnaryRegressionFeatures: NaNs detected!")
        }

        MathUtils.concatVectors(featureList)
    }

    def getBinaryRegressionFeatures(seg1:Segment, adj:SegmentAdjacency) : Tensor1 =
    {
        val seg2 = adj.neighbor


        //ad the enclosure strength as a feature
        val fvec1 = MathUtils.concatVectors(getUnaryRegressionFeatures(seg1), Tensor1(adj.originEnclosure))
        val fvec2 = MathUtils.concatVectors(getUnaryRegressionFeatures(seg2), Tensor1(adj.neighborEnclosure))

        // Sort by distance from the origin in feature space
      //TODO: maybe we want to sort by something more meaningful, like relative size?
        if (fvec1.twoNormSquared < fvec2.twoNormSquared)
            MathUtils.concatVectors(Array(fvec1, fvec2))
        else
            MathUtils.concatVectors(Array(fvec2, fvec1))

    }

    def orderSegmentsByFeatures(seg1:Segment, seg2:Segment): (Segment, Segment) =
    {
        val fvec1 = getUnaryRegressionFeatures(seg1)
        val fvec2 = getUnaryRegressionFeatures(seg2)

        // Sort by distance from the origin in feature space
        if (fvec1.twoNormSquared < fvec2.twoNormSquared)
            (seg1, seg2)
        else
            (seg2, seg1)
    }
}

class SegmentGroup(val index:Int, val owner:SegmentMesh)
{
    var color:ColorVariable = null
    val features = new mutable.HashMap[String, Tensor1]
    val members = new ArrayBuffer[Segment]
    val adjacencies = new HashSet[SegmentGroup]

    // Quick-access to features that are accessed frequently
    // in inference inner loop
    var size:Double = 0.0

    def extractQuickAccessFeatures()
    {
        size = features("RelativeSize")(0)
    }
}
object SegmentGroup
{
    def getRegressionFeatures(seg:SegmentGroup) : Tensor1 =
    {
        val blockList = Set[String]()
        val featureList = seg.features.filterKeys(name => !blockList.contains(name)).values

        //check for Nans
        for (f<-featureList; i<-f)
        {
          assert(i == i, "SegmentGroup: getRegressionFeatures: NaNs detected!")
        }

        MathUtils.concatVectors(featureList)
    }
}

class SegmentMesh(private val gen:ColorVariableGenerator)
{
    /** Data members **/
    val segments = new ArrayBuffer[Segment]
    val groups = new ArrayBuffer[SegmentGroup]
    var name:String = "NO FILE LOADED"

    /** Constructor **/
    def this(gen:ColorVariableGenerator, filename:String)
    {
        this(gen)

        name = filename

        val adjacencies = new ArrayBuffer[ ArrayBuffer[(Int, Double, Double, Double)] ]

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
            segment.adjacencies ++= {for (adj <- adjlist) yield new SegmentAdjacency(segments(adj._1), adj._2, adj._3, adj._4)}
        }

        // Finalize group adjacencies
        for (group <- groups)
        {
            val adjgroups = for (seg <- group.members; aseg <- seg.adjacencies) yield aseg.neighbor.group
            group.adjacencies ++= adjgroups.distinct
        }
    }

    /** Private helpers **/

    private def parseSegment(adj: ArrayBuffer[ ArrayBuffer[(Int, Double, Double, Double)] ], lineIterator:Iterator[String])
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
                    val adjlist = new ArrayBuffer[(Int, Double, Double, Double)]
                    for (tok <- tokens slice(1, tokens.length))
                    {
                        val fields = tok.split('^')
                        adjlist += ((fields(0).toInt, fields(1).toDouble, fields(2).toDouble, fields(3).toDouble) )
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
                    val featureVals = new DenseTensor1(tokens.length-1)

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
            val line = lineIterator.next()
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
                    newgroup.extractQuickAccessFeatures()
                    groups += newgroup
                    return
                }
                case _ =>
                {
                    val featureName = tokens(0)
                    val featureVals = new DenseTensor1(tokens.length-1)

                    for (i <- 1 until tokens.length)
                    {
                        featureVals.update(i-1, tokens(i).toDouble)
                    }
                    newgroup.features(featureName) = featureVals
                }
            }
        }
    }

    def setVariableValuesToObserved()
    {
        for (group <- groups) group.color.setColor(group.color.observedColor)
    }

    def variablesAs[ColorVar<:ColorVariable] : IndexedSeq[ColorVar] =
    {
        for (group <- groups) yield group.color.asInstanceOf[ColorVar]
    }

    /** Output **/
    def saveColorAssignments(filename:String)
    {
        val out = new FileWriter(filename)
        for (group <- groups)
        {
            out.write(group.color.getColor.copyIfNeededTo(RGBColorSpace).componentString + "\n")
        }
        out.close()
    }

    /** Evaluation **/
    def scoreAssignment():Double =
    {
      //just scoring by the color difference between the observed and assigned colors of each group, weighted uniformly
      //the smaller the score, the better
      val diffs = groups.map(
        g=>{if (g.color.observedColor==null) 0 else g.size*Color.perceptualDifference(g.color.observedColor, g.color.getColor)/100.0}
      )
      -1*diffs.sum///groups.length
    }

  def scoreAssignment(assign:Seq[Color]):Double =
  {
    var diffs = 0.0

    for (i <- groups.indices)
    {
      if (groups(i).color.observedColor == null)
        diffs += 0.0
      else
        diffs += groups(i).size*Color.perceptualDifference(groups(i).color.observedColor, assign(i))/100.0
    }

    -1*diffs///groups.length

  }
}
