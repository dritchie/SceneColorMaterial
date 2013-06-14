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
    val adjacencies = new mutable.HashMap[Segment, SegmentAdjacency] //the relevant/filtered adjacencies
    val allAdjacencies = new mutable.HashMap[Segment, SegmentAdjacency]  //all the original adjacencies
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
        val blockList = Set("HuMoments","RelativeCentroid","OneInteriorPoint")
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
        getBinaryRegressionFeatures_NormConcatenation(seg1, adj)
//        getBinaryRegressionFeatures_SizeConcatenation(seg1, adj)
//        getBinaryRegressionFeatures_Interleaved(seg1, adj)
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

    def getBinaryRegressionFeatures_NormConcatenation(seg1:Segment, adj:SegmentAdjacency) : (Tensor1, Array[String]) =
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
        if (fvec1.twoNormSquared < fvec2.twoNormSquared)
            (MathUtils.concatVectors(Array(fvec1, fvec2)),Array.concat(fnames1, fnames2))
        else
            (MathUtils.concatVectors(Array(fvec2, fvec1)), Array.concat(fnames2, fnames1))
    }

    def getBinaryRegressionFeatures_SizeConcatenation(seg1:Segment, adj:SegmentAdjacency) : (Tensor1, Array[String]) =
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

        // Sort by segment size
        if (seg1.size < seg2.size)
            (MathUtils.concatVectors(Array(fvec1, fvec2)),Array.concat(fnames1, fnames2))
        else
            (MathUtils.concatVectors(Array(fvec2, fvec1)), Array.concat(fnames2, fnames1))
    }

    def getBinaryRegressionFeatures_Interleaved(seg1:Segment, adj:SegmentAdjacency) : (Tensor1, Array[String]) =
    {
        val seg2 = adj.neighbor
        val seg2adj = seg2.adjacencies(seg1) //the enclosures are not symmetric, due to the discrete nature of the pixels

        val f1 = getUnaryRegressionFeatures(seg1)
        val f2 = getUnaryRegressionFeatures(seg2)

        //add the enclosure strength as a feature
        val fvec1 = MathUtils.concatVectors(f1._1, Tensor1(adj.originEnclosure))
        val fvec2 = MathUtils.concatVectors(f2._1, Tensor1(seg2adj.originEnclosure))

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
    val members = new ArrayBuffer[Segment]    //filtered/relevant members
    val allMembers = new ArrayBuffer[Segment]  //all members
    val adjacencies = new HashSet[SegmentGroup]  //TODO: not sure if we need to filter this as well

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
    val segments = new ArrayBuffer[Segment] //filtered/relevant segments
    val allSegments = new ArrayBuffer[Segment] //all segments
    val groups = new ArrayBuffer[SegmentGroup]
    var name:String = "NO FILE LOADED"

    //filtering parameters
    private var segK = -1
    private var adjK = -1
    private var ignoreNoise = false

    def copy: SegmentMesh =
    {
        // Seriously, it's easiest just to reload the damn file.
        val newmesh = new SegmentMesh(this.gen, this.name)

        // TODO: If we make any changes to the mesh after loading it (e.g. marking some variables
        // TODO: as observed), those will need to be manually propagated here.
        for (i <- 0 until this.groups.length)
        {
            val fixed = this.groups(i).color.fixed
            newmesh.groups(i).color.fixed = fixed
            if (fixed)
                newmesh.groups(i).color.setColor(this.groups(i).color.getColor)
        }

        newmesh.filter(segK, adjK, ignoreNoise)

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

      //make a backup of the original adjacencies and segments
      for (s <- segments)
        allSegments += s
      for (seg <- segments; (s,a) <- seg.adjacencies)
        seg.allAdjacencies += s -> a
      for (g <- groups; s<-g.members)
        g.allMembers += s


      //TODO: this is a bit hacky now, but just experimenting with the features without having to regenerate all the meshes
      computeAdditionalFeatures()
    }

    /** Private helpers **/
    private def computeAdditionalFeatures()
    {
      //adding an additional features, which are relative size normalized by the max size
      //this might help detect cases where the segments are all roughly the same size, even if they are small
      val maxSegSize = segments.map(_.size).max
      for (seg <- segments)
        seg.features("RelativeSizeOverMax") = Tensor1(seg.size/maxSegSize)

      val maxGroupSize = groups.map(_.size).max
      for (group <- groups)
         group.features("RelativeSizeOverMax") = Tensor1(group.size/maxGroupSize)

    }


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
        for (group <- groups if !group.color.fixed) yield group.color.asInstanceOf[V]
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


  /** Filtering **/
   def filter(sK:Int, aK:Int, noNoise:Boolean)
  {
    //filtering the top segK segments per group, and the top adj K adjacencies per relevant segment
    //we're filtering by group to allow more equal representation across color groups

    //TODO: probably what we really would like to do is to merge similar segments together into one factor and just increase the weight on that factor, but this is easier for now...
    //TODO: this doesn't preserve the order of the segments, but I don't think we really use that order anywhere, other than accessing the index field
    //TODO: this filtering basically is like setting the value of the associated factors to 0.
    // Should we renormalize the sizes (when used in factor weighting)? It doesn't affect the relative scores for a particular mesh

    segK = sK
    adjK = aK
    ignoreNoise = noNoise

    if (segK >= 0 || ignoreNoise)
    {
      //clear the segments
      segments.clear()

      for (g <- groups)
      {
        g.members.clear()
        var sortedMembers = g.allMembers.filter(a=>(!ignoreNoise || !a.isNoise)).sortBy(a => (a.size, a.index))
        if (segK >= 0)
          sortedMembers = sortedMembers.takeRight(segK)
        g.members ++= sortedMembers
        segments ++= g.members
      }
    }
    //filter the adjacencies per segment
    //compute the top K adjacencies per segment. Note: the adjacencies might involve segments that are not in the top segK segments per group
    //in case there are things the same size, sort by index as well
    if (adjK >= 0 || ignoreNoise)
    {
      val segToTopK = new mutable.HashMap[Segment, Seq[Segment]]()
      for (seg <- segments)
      {
        var topK = seg.allAdjacencies.values.toList.filter(a=>(!ignoreNoise || !a.neighbor.isNoise)).sortBy(a => (a.strength, a.neighbor.index))
        if (adjK >= 0)
          topK = topK.takeRight(aK)
        segToTopK(seg) = topK.map(_.neighbor)
      }

      for (seg <- segments)
      {
        seg.adjacencies.clear()

        //add the adjacency if it is in the topK of either the origin segment or its neighbor
        for ((seg2, adj) <- seg.allAdjacencies)
        {
            if (segToTopK(seg).contains(seg2) || (segToTopK.contains(seg2) && segToTopK(seg2).contains(seg)))
            {
              seg.adjacencies(seg2) = adj
              seg2.adjacencies(seg) = seg2.allAdjacencies(seg)
            }

        }

      }
    }
  }
}
