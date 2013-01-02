package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 1/1/13
 * Time: 12:28 PM
 * To change this template use File | Settings | File Templates.
 */

import cc.factorie._
import cc.factorie.la.Tensor

/*
Base class of all factors that touch an arbitrary number of the same type of variable
 */
abstract class FactorN[V<:Variable](val varlist:V*) extends Factor
{
    def variables: Seq[Variable] = varlist
    def numVariables: Int = varlist.length
    def variable(index: Int): Variable = varlist(index)

    def score(values:Seq[V#Value]): Double
    def statistics(values:Seq[V#Value]): StatisticsType = values.asInstanceOf[StatisticsType]

    def currentScore: Double = score(for (v <- varlist) yield v.value.asInstanceOf[V#Value])
    override def currentStatistics: StatisticsType = statistics(for (v <- varlist) yield v.value.asInstanceOf[V#Value])

    def currentAssignment = new HashMapAssignment(varlist)
    def assignmentScore(a:Assignment) = score(for (v <- a.variables.toSeq) yield a(v).asInstanceOf[V#Value])
    override final def assignmentStatistics(a:Assignment): StatisticsType =
        statistics(for (v <- a.variables.toSeq) yield a(v).asInstanceOf[V#Value])

    def valuesIterator: ValuesIterator = new ValuesIterator
    {
        def factor: FactorN[V] = FactorN.this
        def hasNext = false
        def next() = null.asInstanceOf[Assignment]  // Not sure if this will work...
        def score: Double = Double.NaN
        def valuesTensor: Tensor = null
    }
}

abstract class TensorFactorN[V<:Variable](varlist:V*) extends FactorN[V](varlist:_*)
{
    type StatisticsType = Tensor
    override def statistics(values:Seq[V#Value]): Tensor
    final def score(values:Seq[V#Value]): Double = statisticsScore(statistics(values))
    def scoreAndStatistics(values:Seq[V#Value]): (Double, Tensor) = {
        val tensor = statistics(values)
        (statisticsScore(tensor), tensor)
    }
    def statisticsScore(t:Tensor): Double
}

abstract class DotFactorN[V<:Variable](varlist:V*) extends TensorFactorN[V](varlist:_*)
{
    def weights: Tensor
    override def statisticsScore(t:Tensor): Double = weights dot t
}


/*
A family of factors that touch an arbitrary number of the same type of variable
 */
trait FamilyN[V<:Variable] extends FamilyWithNeighborDomains
{
    type FactorType = Factor

    // Stupid thing from Family that we have to define
    type NeighborType1 = V

    def neighborDomain1: Domain[V#Value] = null
    def neighborDomains = Seq(neighborDomain1)

    class Factor(varlist:V*) extends FactorN[V](varlist:_*) with super.Factor
    {
        // Another stupid thing from Family#Factor that we need to define
        def _1:NeighborType1 = null.asInstanceOf[NeighborType1]

        // Ignore the red squigglies below--this actually compiles just fine.
        override def equalityPrerequisite: AnyRef = FamilyN.this
        override def score(values:Seq[V#Value]): Double = FamilyN.this.score(values)
        override def statistics(values:Seq[V#Value]): StatisticsType = FamilyN.this.statistics(values)
        def scoreAndStatistics(values:Seq[V#Value]): (Double,StatisticsType) = FamilyN.this.scoreAndStatistics(values)
    }

    /* Methods that the inner Factor class links to */
    def score(values:Seq[V#Value]): Double
    def statistics(values:Seq[V#Value]): StatisticsType = values.asInstanceOf[StatisticsType]
    def scoreAndStatistics(values:Seq[V#Value]): (Double,StatisticsType) = (score(values), statistics(values))
}

trait TensorFamilyN[V<:Variable] extends FamilyN[V] with TensorFamily
{
    override def statistics(values:Seq[V#Value]): Tensor
}

trait DotFamilyN[V<:Variable] extends FamilyN[V] with DotFamily
{
    def score(values:Seq[V#Value]): Double = statisticsScore(statistics(values))
}



abstract class TemplateN[V<:Variable](implicit nm:Manifest[V]) extends ModelWithFactorType with FamilyN[V] with ModelAsTemplate {
    val neighborClass1 = nm.erasure
    def neighborClasses: Seq[Class[_]] = Seq(neighborClass1)
    val nc1a = { val ta = nm.typeArguments; if (classOf[ContainerVariable[_]].isAssignableFrom(neighborClass1)) { assert(ta.length == 1); ta.head.erasure } else null }

    override def addFactors(v:Variable, ret:scala.collection.mutable.Set[cc.factorie.Factor]): Unit = {
        if (neighborClass1.isAssignableFrom(v.getClass) && ((neighborDomain1 eq null) || (neighborDomain1 eq v.domain))) ret ++= unroll(v.asInstanceOf[V])
        if ((nc1a ne null) && nc1a.isAssignableFrom(v.getClass)) ret ++= unrolls(v.asInstanceOf[V#ContainedVariableType])
        if (tryCascade) { val cascadeVariables = unrollCascade(v); if (cascadeVariables.size > 0) cascadeVariables.foreach(addFactors(_, ret)) }
        ret
    }
    def unroll(v:V): Iterable[FactorType]
    def unrolls(v:V#ContainedVariableType): Iterable[FactorType] = throw new Error("You must override unrolls.")
}

abstract class TensorTemplateN[V<:Variable:Manifest] extends TemplateN[V] with TensorFamilyN[V]
abstract class DotTemplateN[V<:Variable:Manifest] extends TemplateN[V] with DotFamilyN[V]
