package colorinference

import cc.factorie._
import java.awt.image.BufferedImage
import java.awt
import javax.imageio.ImageIO
import java.io.File
import util.Hooks0

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
        val factor = new ColorCompatibilityFactor
        factor.setWeight(1.0)
        factor.vars ++= colorvars

        // Define an itemized model that just contains this one factor
        println("Creating itemized model...")
        val model = new ItemizedModel(factor)

        // Spit out an image depicting the initial state
        val initScore = model.currentScore(colorvars)
        val initRating = math.exp(initScore)*5
        println("Initial state rating: " + initRating)
        savePaletteImage(colorvars, "initPalette_%1.4f.png".format(initRating))

        // Set up an MH sampler to explore color assignments
        println("Creating sampler...")
        val diagnostics = new ContinuousColorSampler.Diagnostics
        val sampler = new ContinuousColorSampler(model, 0.01, 0.33, 0.05, 0.5, diagnostics)

        // Use this sampler to find the MAP assignment
        println("Maximizing...")
        val maximizer = new SamplingMaximizer(sampler)
        val iterations = 1000
        val initTemp = 1.0
        val finalTemp = 0.01
        val rounds = 10
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

    def visAcceptedStates(cvars:IndexedSeq[ContinuousColorVariable], m:Model, d:ContinuousColorSampler.Diagnostics)
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
}
