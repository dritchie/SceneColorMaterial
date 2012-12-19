package colorinference

import cc.factorie._
import cc.factorie.la.Tensor1

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/18/12
 * Time: 11:30 AM
 * To change this template use File | Settings | File Templates.
 */

//TODO: incorporate this more nicely with  Factorie?
//There are some ugly type casts here...hopefully some way to fix
object ExhaustiveInference
{

    def allCombinations(mesh:SegmentMesh, model:Model)
    {
        println("Starting exhaustive search through all combos")

        val numVals = DiscreteColorVariable.domain.size
        val vars = mesh.groups.map(g => g.color)
        val allCombs = MathUtils.comb[Int](vars.size, (0 until numVals).toList)
        var iters = 0

        for (c <- allCombs; p <- c.permutations)
            iters += 1

        println("Number of combinations: "+iters)

        var bestScore = Double.NegativeInfinity
        var currIter = 0
        val itemizedModel = model.itemizedModel(vars)
        for (c <- allCombs; p<-c.permutations)
        {
            if (currIter % 500 == 0) println("Current iteration ..." + currIter)
            currIter += 1
            //create the new assignment
            //TODO: learn DiffLists
            val assignment = new HashMapAssignment(vars)
            for (i <- mesh.groups.indices)
            {
                assignment.update(mesh.groups(i).color.asInstanceOf[DiscreteColorVariable], DiscreteColorVariable.domain(p(i)))
            }
            //TODO: this is ugly
            for (f <- itemizedModel.factors)
            {
                f.variables.foreach{ e => e match {
                    case(v:UnarySegmentTemplate.DatumVariable) => assignment.update(v, v.value)
                    case(b:BinarySegmentTemplate.DatumVariable) => assignment.update(b, b.value)
                    case (g:ColorGroupTemplate.DatumVariable) => assignment.update(g, g.value)
                    case _ => null
                }}
            }


            val currScore = model.assignmentScore(vars, assignment)
            if (currScore > bestScore)
            {
                //set the assignment
                for (i <- mesh.groups.indices)
                {
                    mesh.groups(i).color.setColor(DiscreteColorVariable.domain.category(p(i)))
                }
                bestScore = currScore
            }

        }

    }


    def allPermutations(mesh:SegmentMesh, model:Model)
    {

        println("Starting exhaustive search through all permutation")
        val numVals = DiscreteColorVariable.domain.size
        val vars = mesh.groups.map(g => g.color)
        assert(numVals==vars.size, "allPermutations: Number of variables is not equal to domain!")

        val allPerms = (0 until numVals).toList.permutations.toList

        println("Number of permutations " + allPerms.length)
        var currIter = 0
        var bestScore = Double.NegativeInfinity

        val itemizedModel = model.itemizedModel(vars)

        for (p <- allPerms)
        {
            if (currIter % 10 == 0) println("Current iteration ..." + currIter)
            currIter += 1
            //create the new assignment
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
                    case (g:ColorGroupTemplate.DatumVariable) => assignment.update(g, g.value)
                    case _ => null
                }}
            }


            val currScore = model.assignmentScore(vars, assignment)
            if (currScore > bestScore)
            {
                //set the assignment
                for (i <- mesh.groups.indices)
                {
                    mesh.groups(i).color.setColor(DiscreteColorVariable.domain.category(p(i)))
                }
                bestScore = currScore

            }

        }

    }
}

// NOTE: The proposals this sampler makes are intended for exploring the RGB color cube
class ContinuousColorSampler(override val model:Model, val minRadius:Double = 0.01, val maxRadius:Double = 0.33,
                             val minSwapProb:Double = 0.05, val maxSwapProb:Double = 0.5) extends MHSampler[SegmentMesh](model)
{
    private val mins = Tensor1(0, 0, 0)
    private val maxs = Tensor1(1, 1, 1)

    def propose(context:SegmentMesh)(implicit d:DiffList) : Double =
    {
        // Decide whether to peturb or to swap
        if (math.random < swapProbability)
            proposeSwap(context)(d)
        else
            proposePerturbation(context)(d)

        // Since the choice of which type of proposal to make is symmetric (i.e. doesn't depend
        // on whether we're making a forward or reverse jump), we don't need to include 'swapProbability'
        // in the q/qprime calculation -- it just cancels out
    }

    // TODO: If truncated gaussian is too slow or whatevs - Use a uniform cube instead of a Gaussian. Correction then requires box intersection volume.
    private def proposePerturbation(context:SegmentMesh)(d:DiffList) : Double =
    {
        val colorvars = for (g <- context.groups) yield g.color.asInstanceOf[ContinuousColorVariable]

        // Pick a random color variable
        val randvar = colorvars(random.nextInt(colorvars.length))

        // Generate a random perturbation
        val oldvalue = randvar.value.copyIfNeededTo(RGBColorSpace)
        val sigma = perturbationRadius
        val newvec = MathUtils.gaussianRandomIsotropicTruncated(oldvalue, sigma, mins, maxs)

        // Convert to color, clamp, and update the difflist
        val newvalue = new Color(newvec, RGBColorSpace)
        newvalue.clamp()    // Just in case (the truncated Gaussian should take care of this, though)
        d += randvar.SetTensorDiff(oldvalue, newvalue)

        // Compute q/q', the ratio of forward to backward jump probabilities
        // (Note that 1/colorvars.length is implicitly part of both terms, but it cancels out)
        val q = MathUtils.gaussianDistributionIsotropicTruncated(oldvalue, newvalue, sigma, mins, maxs)
        val qprime = MathUtils.gaussianDistributionIsotropicTruncated(newvalue, oldvalue, sigma, mins, maxs)
        q/qprime
    }

    private def proposeSwap(context:SegmentMesh)(d:DiffList) : Double =
    {
        // Pick two random variables to swap values
        val colorvars = for (g <- context.groups) yield g.color.asInstanceOf[ContinuousColorVariable]
        val rvar1 = colorvars(random.nextInt(colorvars.length))
        colorvars -= rvar1
        val rvar2 = colorvars(random.nextInt(colorvars.length))

        // Generate diffs for the value swap
        d += rvar1.SetTensorDiff(rvar1.value, rvar2.value)
        d += rvar2.SetTensorDiff(rvar2.value, rvar1.value)

        // This proposal is symmetric, so q/qprime is just 1
        1.0
    }

    private def perturbationRadius : Double =
    {
        // A function of the current temperature
        // - When temperature is high (close to 1), this should be large-ish
        // - When temperature is low (close to 0), this should be small-ish

        // For the time being, linear interpolation between provided
        // min and max sigmas
        (1-temperature)*minRadius + temperature*maxRadius
    }

    private def swapProbability : Double =
    {
        // Same concet as the above
        (1-temperature)*minSwapProb + temperature*maxSwapProb
    }
}
