/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

/**
*  Base class for all templates that are defined between two color groups that have some adjacent segments
*/
abstract class PairwiseAdjacentColorTemplate extends Template2[DiscreteColorVariable, DiscreteColorVariable]
{
    // NOTE: unroll1 and unroll2 can generate duplicate factors. Fortunately, factorie deduplicates them
    // for us

    def unroll1(v1:DiscreteColorVariable) =
    {
        // Yield a factor for every variable whose group is adjacent to v1's group
        for (group <- v1.group.adjacencies) yield Factor(v1, group.color)
    }

    def unroll2(v2:DiscreteColorVariable) =
    {
        // Yield a factor for every variable whose group is adjacent to v2's group
        for (group <- v2.group.adjacencies) yield Factor(group.color, v2)
    }
}

/**
 * Given a segment mesh, builds a graph of factors that attemt to maintain the observed contrast between
 * adjacent color groups
 */
class MaintainObservedContrastModel(segmesh:SegmentMesh) extends ItemizedModel
{
    // For each group, add a factor for each adjacent group
    // (Deduplicate by only adding the (low, high) pair)
    for (group1 <- segmesh.groups)
    {
        for (group2 <- group1.adjacencies)
        {
            if (group1.index < group2.index)
                this += new PairwiseMaintainObservedContrastFactor(group1.color, group2.color)
        }
    }
}

