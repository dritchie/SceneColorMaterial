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

    override def unroll1(v1:DiscreteColorVariable) =
    {
        // Yield a factor for every variable whose group is adjacent to v1's group
        for (group <- v1.group.adjacencies) yield Factor(v1, group.color)
    }

    override def unroll2(v2:DiscreteColorVariable) =
    {
        // Yield a factor for every variable whose group is adjacent to v2's group
        for (group <- v2.group.adjacencies) yield Factor(group.color, v2)
    }
}

/**
 * Tries to keep the contrast between adjacent colors similar to the contrasts between the
 * original observed colors
 */
class PairwiseContrastTemplate extends PairwiseAdjacentColorTemplate
{
    override def score(v1:DiscreteColorVariable#Value, v2:DiscreteColorVariable#Value) =
    {
        throw new Error("Not yet implemented")
    }
}
