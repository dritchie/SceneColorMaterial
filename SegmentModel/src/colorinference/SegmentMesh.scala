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
import cc.factorie.Variable
import cc.factorie.la.Tensor1
import cc.factorie.la.DenseTensor1
import io.Source
import java.io.FileWriter
import collection.mutable

class SegmentAdjacency(val neighbor:Segment, val strength:Double, val originEnclosure:Double, val neighborEnclosure:Double)

class Segment(val index:Int, var owner:SegmentMesh)
{
    val features = new mutable.HashMap[String, Tensor1]
    val adjacencies = new mutable.HashMap[Segment, SegmentAdjacency]
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

    def getUnaryRegressionFeatures(seg:Segment) : (Tensor1, Array[String]) =
    {
        val blockList = Set("HuMoments","RelativeCentroid")
        val featureList = seg.features.filterKeys(name => !blockList.contains(name)).values
        val featureNames = seg.features.filterKeys(name=> !blockList.contains(name)).keys

        //check for Nans
        for (f<-featureList; i<-f)
        {
          assert(i == i, "Segment: getUnaryRegressionFeatures: NaNs detected!")
        }

        val repeatFeatureNames = new ArrayBuffer[String]
        for (k<-featureNames; i<-seg.features(k))
        {
           repeatFeatureNames += k
        }

        (MathUtils.concatVectors(featureList), repeatFeatureNames.toArray)
    }

    def getBinaryRegressionFeatures(seg1:Segment, adj:SegmentAdjacency) : (Tensor1, Array[String]) =
    {
//        getBinaryRegressionFeatures_Differences(seg1, adj)
//        getBinaryRegressionFeatures_Concatentation(seg1, adj)
        getBinaryRegressionFeatures_Interleaved(seg1, adj)
    }

    def getBinaryRegressionFeatures_Differences(seg1:Segment, adj:SegmentAdjacency) : (Tensor1, Array[String]) =
    {
        val seg2 = adj.neighbor

        val f1 = getUnaryRegressionFeatures(seg1)
        val f2 = getUnaryRegressionFeatures(seg2)

        // The binary feature vector is the absolute difference between these
        // two vectors, plus (min, max) enclosure features
        val len = f1._1.length
        val bf = new DenseTensor1(len+2)
        for (i <- 0 until len) bf.update(i, math.abs(f1._1(i) - f2._1(i)))
        val minenc = math.min(adj.originEnclosure, adj.neighborEnclosure)
        val maxenc = math.max(adj.originEnclosure, adj.neighborEnclosure)
        bf.update(len, minenc)
        bf.update(len, maxenc)
        val bfnames = Array.concat(f1._2, Array("minEnclosure", "maxEnclosure"))
        (bf, bfnames)
    }

    def getBinaryRegressionFeatures_Concatentation(seg1:Segment, adj:SegmentAdjacency) : (Tensor1, Array[String]) =
    {
        val seg2 = adj.neighbor

        val f1 = getUnaryRegressionFeatures(seg1)
        val f2 = getUnaryRegressionFeatures(seg2)

        //add the enclosure strength as a feature
        val fvec1 = MathUtils.concatVectors(f1._1, Tensor1(adj.originEnclosure))
        val fvec2 = MathUtils.concatVectors(f2._1, Tensor1(adj.neighborEnclosure))

        //modify the feature names
        val fnames1 = Array.concat(f1._2 , Array("enclosure"))
        val fnames2 = Array.concat(f2._2, Array("enclosure"))

        // Sort by distance from the origin in feature space
        //TODO: maybe we want to sort by something more meaningful, like relative size?
        if (fvec1.twoNormSquared < fvec2.twoNormSquared)
            (MathUtils.concatVectors(Array(fvec1, fvec2)),Array.concat(fnames1, fnames2))
        else
            (MathUtils.concatVectors(Array(fvec2, fvec1)), Array.concat(fnames2, fnames1))
    }

    def getBinaryRegressionFeatures_Interleaved(seg1:Segment, adj:SegmentAdjacency) : (Tensor1, Array[String]) =
    {
        val seg2 = adj.neighbor

        val f1 = getUnaryRegressionFeatures(seg1)
        val f2 = getUnaryRegressionFeatures(seg2)

        //add the enclosure strength as a feature
        val fvec1 = MathUtils.concatVectors(f1._1, Tensor1(adj.originEnclosure))
        val fvec2 = MathUtils.concatVectors(f2._1, Tensor1(adj.neighborEnclosure))

        //modify the feature names
        val fnames1 = Array.concat(f1._2 , Array("enclosure"))
        val fnames2 = Array.concat(f2._2, Array("enclosure"))

        // Interleave each pair of features in (min, max) order
        val features = new DenseTensor1(fvec1.length + fvec2.length)
        val featureNames = new Array[String](fnames1.length + fnames2.length)
        val n = fvec1.length
        for (i <- 0 until n)
        {
            val minval = math.min(fvec1(i), fvec2(i))
            val maxval = math.max(fvec1(i), fvec2(i))
            features.update(2*i, minval)
            features.update(2*i+1, maxval)
            featureNames.update(2*i, fnames1(i))
            featureNames.update(2*i+1, fnames2(i))
        }

        (features, featureNames)
    }
}

class SegmentGroup(val index:Int, var owner:SegmentMesh)
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
    def getRegressionFeatures(seg:SegmentGroup) : (Tensor1, Array[String]) =
    {
        val blockList = Set[String]()
        val featureList = seg.features.filterKeys(name => !blockList.contains(name)).values
        val featureNames = seg.features.filterKeys(name=> !blockList.contains(name)).keys

        //check for Nans
        for (f<-featureList; i<-f)
        {
          assert(i == i, "SegmentGroup: getRegressionFeatures: NaNs detected!")
        }

        val repeatFeatureNames = new ArrayBuffer[String]
        for (k<-featureNames; i<-seg.features(k))
        {
          repeatFeatureNames += k
        }

        (MathUtils.concatVectors(featureList), repeatFeatureNames.toArray)
    }
}

trait VariableStructure
{
    def variablesAs[V<:Variable] : IndexedSeq[V]
}

class SegmentMesh(private val gen:ColorVariableGenerator) extends VariableStructure with Copyable[SegmentMesh]
{
    /** Data members **/
    val segments = new ArrayBuffer[Segment]
    val groups = new ArrayBuffer[SegmentGroup]
    var name:String = "NO FILE LOADED"

    def copy: SegmentMesh =
    {
        // Seriously, it's easiest just to reload the damn file.
        val newmesh = new SegmentMesh(this.gen, this.name)
        // TODO: If we make any changes to the mesh after loading it (e.g. marking some variables
        // TODO: as observed, those will need to be manually propagated here.
        newmesh
    }

    /** Constructor **/
    def this(gen:ColorVariableGenerator, filename:String)
    {
        this(gen)

        load(filename)
    }

    private def load(filename:String)
    {
        name = filename

        val adjacencies = new ArrayBuffer[ ArrayBuffer[(Int, Double, Double, Double)] ]

        val source = Source.fromFile(filename)
        val lineIterator = source.getLines()
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next()
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
            segment.adjacencies ++= {for (adj <- adjlist) yield (segments(adj._1), new SegmentAdjacency(segments(adj._1), adj._2, adj._3, adj._4))}
        }

        // Finalize group adjacencies
        for (group <- groups)
        {
            val adjgroups = for (seg <- group.members; aseg <- seg.adjacencies.values) yield aseg.neighbor.group
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

    def variablesAs[V<:Variable] : IndexedSeq[V] =
    {
        for (group <- groups) yield group.color.asInstanceOf[V]
    }

    def randomizeVariableAssignments()
    {
        val c = groups(0).color
        c match
        {
            case dc:DiscreteColorVariable => discreteRandomizeVariableAssignments()
            case cc:ContinuousColorVariable => continuousRandomizeVariableAssignments()
        }
    }

    private def discreteRandomizeVariableAssignments()
    {
        val numVals = DiscreteColorVariable.domain.size
        val allPerms = (0 until numVals).toList.permutations.toList
        val randP = allPerms(util.Random.nextInt(allPerms.length))

        for (i <-groups.indices)
        {
            groups(i).color.setColor(DiscreteColorVariable.domain.category(randP(i)))
        }
    }

    private def continuousRandomizeVariableAssignments()
    {
        for (g <- groups)
        {
            val c = Color.RGBColor(math.random, math.random, math.random)
            g.color.setColor(c)
        }
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
