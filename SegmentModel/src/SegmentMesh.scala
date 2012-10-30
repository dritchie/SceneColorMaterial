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


class Segment
{
    val features = new ArrayBuffer[SegmentFeature]
}

class SegmentFeature(val name:String)
{
    val values = new GrowableDenseTensor1(0)
}

class SegmentGroup
{
    var color:Color = null
    val members = new ArrayBuffer[Int]
}

class SegmentMesh
{
    /** Constructor **/
    def this(filename:String)
    {
        this()  // Invoke the primary constructor (which does nothing, in our case)
        val source = Source.fromFile(filename)
        val lineIterator = source.getLines()
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next
            val tokens = line.split(" ")
            tokens(0) match
            {
                case "SegmentBegin" => parseSegment(lineIterator)
                case "GroupBegin" => parseGroup(lineIterator)
                case _ =>
            }
        }
        source.close()
    }

    /** Private helpers **/

    private def parseSegment(lineIterator:Iterator[String])
    {
        val newseg = new Segment
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next
            val tokens = line.split(" ")
            tokens(0) match
            {
                case "AdjacentTo" =>
                {
                    val adjlist = new HashSet[Int]
                    for (tok <- tokens slice(1, tokens.length))
                    {
                        adjlist += tok.toInt
                    }
                    adjacencies += adjlist
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
        var newgroup = new SegmentGroup
        while (lineIterator.hasNext)
        {
            val line = lineIterator.next
            val tokens = line.split(" ")
            tokens(0) match
            {
                case "ObservedColor" =>
                {
                    // Assuming that colors are given in RGB
                    newgroup.color = new RGBColor(tokens(1).toDouble, tokens(2).toDouble, tokens(3).toDouble)
                }
                case "Members" =>
                {
                    for (tok <- tokens slice(1,tokens.length))
                    {
                        newgroup.members += tok.toInt
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
    val adjacencies = new ArrayBuffer[ HashSet[Int] ]
}
