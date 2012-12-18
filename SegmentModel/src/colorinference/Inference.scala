package colorinference

import cc.factorie.{HashMapAssignment, Model}

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
