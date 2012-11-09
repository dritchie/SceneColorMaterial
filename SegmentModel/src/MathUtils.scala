/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 11/9/12
 * Time: 1:45 PM
 * To change this template use File | Settings | File Templates.
 */
object MathUtils
{
    def gaussianKernel(x:Double, mu:Double, sigma:Double) =
    {
        val xminusmu = x - mu
        (-xminusmu*xminusmu) / (2*sigma*sigma)
    }

    def gaussianDistribution(x:Double, mu:Double, sigma:Double) =
    {
        val coeff = 1.0 / (sigma * math.sqrt(2*math.Pi))
        coeff * gaussianKernel(x, mu, sigma)
    }
}
