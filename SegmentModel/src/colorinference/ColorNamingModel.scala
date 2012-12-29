package colorinference

import collection.mutable.ArrayBuffer
import cc.factorie.la.{DenseTensor2,DenseLayeredTensor2}
import io.Source
import weka.core._
import neighboursearch.KDTree

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/16/12
 * Time: 4:23 PM
 * To change this template use File | Settings | File Templates.
 */
object ColorNamingModel
{
    var saliencyCacheSize = 100
    var similarityCacheSize = 1000
}
class ColorNamingModel(c3JsonFile:String)
{
    private val colors = new ArrayBuffer[Color]
    private val terms = new ArrayBuffer[String]
    private var counts:DenseLayeredTensor2 = null
    private var colorRowSums:IndexedSeq[Int] = null
    private var termColSums:IndexedSeq[Int] = null
    private var rowNorms:IndexedSeq[Double] = null
    private var kdtree:KDTree = null
    private val saliencyCache = new Cache[Int, Double](ColorNamingModel.saliencyCacheSize)

    private val similarityCache = new Cache[(Int,Int), Double](ColorNamingModel.similarityCacheSize)

    load(c3JsonFile)

    private def load(c3JsonFile:String)
    {
        colors.clear()
        terms.clear()

        // Load JSON text
        val source = Source.fromFile(c3JsonFile)
        val lines = source.getLines().toSeq
        val text = lines.reduceLeft((result, elem) => result + elem)
            .replace("{","").replace("}","").replace("[","").replace("]","")
        val fields = text.split("\"color\":|\"terms\":|\"T\":|\"A\":").filter(str => str.nonEmpty)
        assert(fields.length == 4, {println("Problem parsing c3 json file " + c3JsonFile)})

        // Populate color list (assume they come in LAB)
        val colorlist = fields(0).split(',')
        for (i <- 0 until (colorlist.length/3))
            colors += Color.LABColor(colorlist(3*i).toDouble, colorlist(3*i+1).toDouble, colorlist(3*i+2).toDouble)

        // Populate term list
        terms ++= fields(1).split(',')

        // Populate count matrix
        counts = new DenseLayeredTensor2(colors.length, terms.length)
        val countvals = fields(2).split(',')
        for (i <- 0 until (countvals.length/2))
            counts.update(countvals(2*i).toInt, countvals(2*i+1).toInt)

        // Pre-compute sums across rows and columns
        colorRowSums = for (w <- 0 until terms.length) yield
        {
            var sum = 0
            for (c <- 0 until colors.length) sum += counts(c, w).toInt
            sum
        }
        termColSums = for (c <- 0 until colors.length) yield
        {
            var sum = 0
            for (w <- 0 until terms.length) sum += counts(c, w).toInt
            sum
        }

        // Pre-compute column norms
        rowNorms = for (c <- 0 until colors.length) yield
        {
            var sum = 0
            for (w <- 0 until terms.length)
            {
                val count = counts(c, w).toInt
                sum += count*count
            }
            math.sqrt(sum)
        }

        // Build acceleration structure for nearest-color-bin lookups
        val attribs = new FastVector(3)
        attribs.addElement(new Attribute("L"))
        attribs.addElement(new Attribute("a"))
        attribs.addElement(new Attribute("b"))
        attribs.addElement(new Attribute("index"))
        val insts = new Instances("colors", attribs, colors.length)
        insts.setClassIndex(attribs.size-1)
        for (i <- 0 until colors.length)
        {
            val c = colors(i)
            insts.add(new Instance(1.0, Array(c(0), c(1), c(2), i)))
        }
        kdtree = new KDTree
        kdtree.setInstances(insts)
    }

    private def pwc(w:Int, c:Int) : Double =
    {
        counts(c, w) / math.max(termColSums(c), 1)
    }

    private def pcw(c:Int, w:Int) : Double =
    {
        counts(c, w) / math.max(colorRowSums(w), 1)
    }

    private def bin(color:Color) : Int =
    {
        val c = color.copyIfNeededTo(LABColorSpace)
        val inst = new Instance(1.0, Array(c(0), c(1), c(2), -1))
        inst.setDataset(kdtree.getInstances)
        val nn = kdtree.nearestNeighbour(inst)
        nn.classValue.toInt
    }

    private def saliency(c:Int) : Double =
    {
        // Check cache
        saliencyCache.get(c) match
        {
            case s:Some[Double] => s.get
            case None =>
            {
                val log2 = math.log(2)
                var sum = 0.0
                for (w <- 0 until terms.length)
                {
                    val p = pwc(w, c)
                    if (p > 0)
                        sum += p * math.log(p) / log2
                }
                saliencyCache.put(c, sum)
                sum
            }
        }
    }

    def saliency(color:Color) : Double =
    {
        val c = bin(color.copyIfNeededTo(LABColorSpace))
        saliency(c)
    }

    private def cosineSimilarity(c1:Int, c2:Int) : Double =
    {
        // Check cache
        val orderedPair = if (c1 < c2) (c1,c2) else (c2,c1)
        similarityCache.get(orderedPair) match
        {
            case s:Some[Double] => s.get
            case None =>
            {
                var sum = 0.0
                for (w <- 0 until terms.length)
                    sum += counts(c1, w) * counts(c2, w)
                val result = sum / math.max(rowNorms(c1)*rowNorms(c2), 1)
                similarityCache.put(orderedPair, result)
                result
            }
        }
    }

    def cosineSimilarity(color1:Color, color2:Color) : Double =
    {
        val c1 = bin(color1.copyIfNeededTo(LABColorSpace))
        val c2 = bin(color2.copyIfNeededTo(LABColorSpace))
        cosineSimilarity(c1, c2)
    }
}
