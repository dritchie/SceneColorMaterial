/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 11:24 AM
 * To change this template use File | Settings | File Templates.
 */

import collection.mutable.ArrayBuffer
import collection.mutable.HashSet
import cc.factorie.la.GrowableDenseTensor1
import io.Source


class Segment(val owner:SegmentMesh)
{
    val features = new ArrayBuffer[SegmentFeature]
    val adjacencies = new HashSet[Segment]
    var group : SegmentGroup = null
}

class SegmentFeature(val name:String)
{
    val values = new GrowableDenseTensor1(0)
}

class SegmentGroup(val owner:SegmentMesh)
{
    var color:DiscreteColorVariable = null
    val members = new ArrayBuffer[Segment]
    val adjacencies = new HashSet[SegmentGroup]
}

class SegmentMesh
{
    /** Constructor **/
    def this(filename:String)
    {
        this()  // Invoke the primary constructor (which does nothing, in our case)

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
        val newseg = new Segment(this)
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
                    segments += newseg
                    return
                }
                case _ =>
                {
                    val feature = new SegmentFeature(tokens(0))
                    feature.values.ensureDimensions(tokens.length-1)
                    for (i <- 1 until tokens.length)
                    {
                        feature.values.update(i-1, tokens(i).toDouble)
                    }
                    newseg.features += feature
                }
            }
        }
    }

    private def parseGroup(lineIterator:Iterator[String])
    {
        var newgroup = new SegmentGroup(this)
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next
            val tokens = line.split(" ")
            tokens(0) match
            {
                case "ObservedColor" =>
                {
                    // Assuming that colors are given in RGB
                    val c = new RGBColor(tokens(1).toDouble, tokens(2).toDouble, tokens(3).toDouble)
                    newgroup.color = new DiscreteColorVariable(newgroup, c)
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

    /** Data members **/
    val segments = new ArrayBuffer[Segment]
    val groups = new ArrayBuffer[SegmentGroup]
}
