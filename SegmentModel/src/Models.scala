/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 10/30/12
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._

/**
 * Given a segment mesh, builds a graph of factors that attemt to maintain the observed contrast between
 * adjacent color groups
 */
class MaintainObservedContrastModel(segmesh:SegmentMesh[DiscreteColorVariable]) extends ItemizedModel
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

