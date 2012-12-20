package colorinference

import cc.factorie._
import cc.factorie.la.Tensor1
import collection.mutable.ArrayBuffer

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
                             val minSwapProb:Double = 0.05, val maxSwapProb:Double = 0.5, val diagnostics:ContinuousColorSampler.Diagnostics = null)
    extends MHSampler[IndexedSeq[ContinuousColorVariable]](model)
{
    import ContinuousColorSampler.ProposalType._

    private val mins = Tensor1(0, 0, 0)
    private val maxs = Tensor1(1, 1, 1)

    // IMPORTANT: MHSampler lies when it says that this method should return q/q'.
    // It should actually return log(q/q')
    def propose(context:IndexedSeq[ContinuousColorVariable])(implicit d:DiffList) : Double =
    {
        if (diagnostics != null)
            diagnostics.updateLastState(context)

        // Decide whether to peturb or to swap
        if (math.random < swapProbability)
            proposeSwap(context)(d)
        else
            proposePerturbation(context)(d)

        // Since the choice of which type of proposal to make is symmetric (i.e. doesn't depend
        // on whether we're making a forward or reverse jump), we don't need to include 'swapProbability'
        // in the q/q' calculation -- it just cancels out
    }

    // TODO: If truncated gaussian is too slow or whatevs - Use a uniform cube instead of a Gaussian. Correction then requires box intersection volume.
    private def proposePerturbation(colorvars:IndexedSeq[ContinuousColorVariable])(implicit d:DiffList) : Double =
    {
        if (diagnostics != null)
            diagnostics.lastProposal = Perturbation

        // Pick a random color variable
        val randvar = colorvars(random.nextInt(colorvars.length))

        // Generate a random perturbation
        val oldvalue = randvar.value.copyIfNeededTo(RGBColorSpace)
        val sigma = perturbationRadius
        val newvec = MathUtils.gaussianRandomIsotropicTruncated(oldvalue, sigma, mins, maxs)

        // Convert to color and clamp
        val newvalue = new Color(newvec, RGBColorSpace)
        newvalue.clamp()    // Just in case (the truncated Gaussian should take care of this, though)

        // Make the change (this will automagically update the implicit difflist)
        randvar.set(newvalue)

        // Compute log(q/q'), the ratio of backward to forward jump probabilities
        // (Note that 1/colorvars.length is implicitly part of both terms, but it cancels out)
        val q = MathUtils.gaussianDistributionIsotropicTruncated(oldvalue, newvalue, sigma, mins, maxs)
        val qprime = MathUtils.gaussianDistributionIsotropicTruncated(newvalue, oldvalue, sigma, mins, maxs)
        val ratio = q/qprime
        val logratio = math.log(ratio)
        logratio
    }

    private def proposeSwap(colorvars:IndexedSeq[ContinuousColorVariable])(implicit d:DiffList) : Double =
    {
        if (diagnostics != null)
            diagnostics.lastProposal = Swap

        // Pick two random variables to swap values
        val tmpvars = ArrayBuffer(colorvars:_*)
        val rvar1 = tmpvars(random.nextInt(tmpvars.length))
        tmpvars -= rvar1
        val rvar2 = tmpvars(random.nextInt(tmpvars.length))

        // Do the swap (this will automatically update the implicit difflist)
        val value1 = rvar1.value
        val value2 = rvar2.value
        rvar1.set(value2)
        rvar2.set(value1)

        // This proposal is symmetric, so q/q' is just 1
        // (and thus log(q/q') is 0)
        0.0
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
        // Same concept as the above
        (1-temperature)*minSwapProb + temperature*maxSwapProb
    }

    override def proposalHook(proposal:cc.factorie.Proposal)
    {
        // MHSampler forgets to call super.proposalHook, so I need to put this here
        this.proposalHooks(proposal)

        // I don't want this.proposalHooks called twice, so to make sure the code
        // is still fine if/when the factorie folks fix this bug, I should empty
        // out this.proposalHooks before calling the superclass method
        val savedHooks = Array(this.proposalHooks:_*)
        this.proposalHooks.clear()

        // Now we're safe to call the super method
        super.proposalHook(proposal)

        // Make sure to put the saved proposalHooks back
        this.proposalHooks ++= savedHooks

        // Finally, we can do the new stuff we *actually* wanted to
        // do with this method O_O
        if (diagnostics != null)
        {
            val p = proposal.asInstanceOf[Proposal]
            diagnostics.process(this, p)
        }
    }
}

object ContinuousColorSampler
{
    object ProposalType extends Enumeration
    {
        type ProposalType = Value
        val Perturbation, Swap = Value
    }
    import ProposalType._

    class Diagnostics
    {
        case class HistoryEntry(state:IndexedSeq[Color], propType:ProposalType, accepted:Boolean, temp:Double)

        var history = new ArrayBuffer[HistoryEntry]

        var lastProposal:ProposalType = Perturbation
        private var lastState:IndexedSeq[Color] = null

        var numProposedMoves = 0
        var numAcceptedMoves = 0
        var numNegativeMoves = 0
        var numProposedSwaps = 0
        var numAcceptedSwaps = 0
        var numProposedPerturbations = 0
        var numAcceptedPerturbations = 0

        def updateLastState(state:IndexedSeq[ContinuousColorVariable])
        {
            lastState = for (cvar <- state) yield cvar.value.copy
        }

        def process(sampler:ContinuousColorSampler, proposal:ContinuousColorSampler#Proposal)
        {
            // Record summary statistics
            numProposedMoves += 1
            lastProposal match
            {
                case Perturbation => numProposedPerturbations += 1
                case Swap => numProposedSwaps += 1
            }
            val accepted = !proposal.bfRatio.isNaN
            if (accepted)
            {
                numAcceptedMoves += 1
                lastProposal match
                {
                    case Perturbation => numAcceptedPerturbations += 1
                    case Swap => numAcceptedSwaps += 1
                }
            }
            if (proposal.modelScore < 0)
                numNegativeMoves += 1

            // Record history
            history += HistoryEntry(lastState, lastProposal, accepted, sampler.temperature)
        }

        def summarize()
        {
            println("ContinuousColorSampler Diagonistic Summary:")
            println("numProposedMoves: " + numProposedMoves)
            println("numProposedSwaps: %d (.%.2f of total)".format(numProposedSwaps, numProposedSwaps/numProposedMoves.toDouble))
            println("numProposedPerturbations: %d (%.2f of total)".format(numProposedPerturbations, numProposedPerturbations/numProposedMoves.toDouble))
            println("numAcceptedMoves: %d (%.2f of total)".format(numAcceptedMoves, numAcceptedMoves/numProposedMoves.toDouble))
            println("numNegativeMoves: " + numNegativeMoves)
            println("numAcceptedSwaps: %d (%.2f of total)".format(numAcceptedSwaps, numAcceptedSwaps/numAcceptedMoves.toDouble))
            println("numAcceptedPerturbations: %d (%.2f of total)".format(numAcceptedPerturbations, numAcceptedPerturbations/numAcceptedMoves.toDouble))
        }
    }
}