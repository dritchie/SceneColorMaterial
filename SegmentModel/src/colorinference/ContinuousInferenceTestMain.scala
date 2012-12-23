package colorinference

import cc.factorie._
import java.awt.image.BufferedImage
import java.awt
import javax.imageio.ImageIO
import java.io.{FileWriter, File}
import collection.mutable.HashSet

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/19/12
 * Time: 10:51 AM
 * To change this template use File | Settings | File Templates.
 */

object ContinuousInferenceTestMain
{
    def main(args:Array[String])
    {
        // Create a list of five color variables, initialized to random values
        println("Creating color vars...")
        val colorvars = for (i <- 0 until 5) yield
        {
            val cvar = new ContinuousColorVariable(null, null)
            cvar.setColor(Color.RGBColor(math.random, math.random, math.random))
            cvar
        }

        // Create a color compatibility factor over those variables
        println("Creating compatibility factor...")
        val family = new ColorCompatibilityFamily
        family.setWeight(1.0)
        val factor = new family.Factor
        factor.vars ++= colorvars

        // Define an itemized model that just contains this one factor
        println("Creating itemized model...")
        val model = new ItemizedColorInferenceModel(factor.asInstanceOf[Factor])
        val families = model.families

//        // TEST
//        checkPatternPaletteScores(colorvars, model)
//        return

        // Spit out an image depicting the initial state
        val initScore = model.currentScore(colorvars)
        val initRating = math.exp(initScore)*5
        println("Initial state rating: " + initRating)
        savePaletteImage(colorvars, "initPalette_%1.4f.png".format(initRating))

        // Set up an MH sampler to explore color assignments
        println("Creating sampler...")
        val diagnostics = new ContinuousColorSampling.Diagnostics
        val sampler = new ContinuousColorSampler(model, 0.01, 0.33, 0.05, 0.5, diagnostics)

        // Use this sampler to find the MAP assignment
        println("Maximizing...")
        val maximizer = new SamplingMaximizer(sampler)
        val iterations = 2000
        val initTemp = 1.0
        val finalTemp = 0.01
        val rounds = 40
        maximizer.maximize(Array(colorvars), iterations, initTemp, finalTemp, rounds)

        // Spit out an image depicting the MAP palette
        val mapScore = model.currentScore(colorvars)
        val mapRating = math.exp(mapScore)*5
        println("MAP rating: " + mapRating)
        savePaletteImage(colorvars, "optimalPalette_%1.4f.png".format(mapRating))

        // Visualize entire history of accepted moves
        println("Visualizing history of accepted states...")
        visAcceptedStates(colorvars, model, diagnostics)

        // Report diagnostics
        diagnostics.summarize()

        println("DONE")
    }

    def savePaletteImage(cvars:IndexedSeq[ContinuousColorVariable], filename:String)
    {
        val img = new BufferedImage(500, 100, BufferedImage.TYPE_INT_ARGB)
        for (c <- 0 until 5; y <- 0 until 100; x <- c*100 until (c+1)*100)
        {
            val color = cvars(c).value
            val awtcolor = new awt.Color(color(0).toFloat, color(1).toFloat, color(2).toFloat)
            img.setRGB(x, y, awtcolor.getRGB)
        }
        ImageIO.write(img, "png", new File(filename))
    }

    def visAcceptedStates(cvars:IndexedSeq[ContinuousColorVariable], m:Model, d:ContinuousColorSampling.Diagnostics)
    {
        // Ensure directory exists
        val dir = new File("acceptedStates")
        if (!dir.exists)
            dir.mkdir

        val hist = d.history.filter(entry => entry.accepted)
        for (e <- 0 until hist.length)
        {
            val entry = hist(e)
            for (i <- 0 until cvars.length) cvars(i).setColor(entry.state(i))
            val score = m.currentScore(cvars)
            val rating = math.exp(score)*5
            val filename = "acceptedStates/palette_%05d_%1.4f.png".format(e, rating)
            savePaletteImage(cvars, filename)
        }
    }

    def checkPatternPaletteScores(cvars:IndexedSeq[ContinuousColorVariable], m:Model)
    {
        println("Computing scores of all permutations of ColourLovers pattern palettes...")

        val datafile = "../Colourlovers/patterns.csv"
        val artists = HashSet("sugar!", "ghighi", "incal")
        val lines = io.Source.fromFile(datafile).getLines().map(_.split(',')).toSeq.filter(l => artists.contains(l(0)))

        case class PaletteDatum(palette:IndexedSeq[Color], originalOrder:Boolean, sizeOrder:Boolean, score:Double)
        {
            var optimalOrder = false
            def permutationType:String =
            {
                if (originalOrder) "ORIGINAL"
                else if (sizeOrder) "BYSIZE"
                else if (optimalOrder) "OPTIMAL"
                else "OTHER"
            }
        }

        // Open CSV
        val file = new FileWriter("permutationStats/permutationScores.csv")
        file.write("artist,id,permutationType,score\n")
        file.flush()

        // Extract data
        println("Extracting data...")
        var i = 0
        for (line <- lines)
        {
            // Fields are: artist, id, url, palette
            // Palette is a list of space separated 24bit hex RGB colors
            if (line.length == 4)
            {
                val artist = line(0)
                val id = line(1)
                val palette = line(3).split(' ').distinct

                // We can only really evaluate the model on 5 color palettes
                if (palette.length == 5)
                {
                    val colors = for (cstr <- palette) yield
                    {
                        val ac = awt.Color.decode("0x"+cstr)
                        val rgb = new Array[Float](3)
                        ac.getRGBColorComponents(rgb)
                        Color.RGBColor(rgb(0), rgb(1), rgb(2))
                    }

                    // We also need the corresponding SegmentMesh so we can identify the
                    // permutation that corresponds to group size order
                    val segmesh = new SegmentMesh(ContinuousColorVariable, "permutationStats/meshes/%s.txt".format(id))
                    val sizeOrder = for (g <- segmesh.groups.sortWith(_.size > _.size)) yield g.color.observedColor

                    // Generate and score all permutations of the palette
                    val datumlist = (for (cperm <- colors.permutations) yield
                    {
                        for (i <- 0 until cperm.length) cvars(i).setColor(cperm(i))
                        val score = math.exp(m.currentScore(cvars))*5
                        val isOrigOrder = (cperm, colors).zipped.map(_ equals _).reduce(_ && _)
                        val isSizeOrder = (cperm, sizeOrder).zipped.map(_.approxEquals(_,1e-4)).reduce(_ && _)
                        PaletteDatum(cperm, isOrigOrder, isSizeOrder, score)
                    }).toSeq
                    // Mark the optimal permutation
                    val opt = datumlist.reduce((a,b) => {if (a.score > b.score) a else b})
                    opt.optimalOrder = true
                    // Output CSV data for this palette
                    for (pdatum <- datumlist)
                    {
                        file.write("%s,%s,%s,%g\n".format(artist,id,pdatum.permutationType,pdatum.score))
                    }
                    file.flush()
                }
            }
            i += 1
            println("Done %d/%d".format(i, lines.length))
        }

        // Finish up
        file.close()
        println("DONE")
    }
}
