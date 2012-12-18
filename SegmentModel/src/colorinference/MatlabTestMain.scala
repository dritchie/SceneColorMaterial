package colorinference

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/17/12
 * Time: 11:56 PM
 * To change this template use File | Settings | File Templates.
 */

import matlabcontrol._

object MatlabTestMain
{
    def main(args:Array[String])
    {
        //Create a proxy, which we will use to control MATLAB
        val options = new MatlabProxyFactoryOptions.Builder().setUsePreviouslyControlledSession(true).build()
        val factory = new MatlabProxyFactory(options)
        val proxy = new MatlabProxyScalaWrapper(factory.getProxy)

        proxy.eval("cd ../odonovan")
        proxy.eval("setup_rating_env")
        val retval = proxy.returningFeval("getRating", 1, Array(0.0, 0.0, 0.0, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 1.0, 1.0, 1.0))
        val rating = retval(0).asInstanceOf[Array[Double]](0)
        println("rating:  " + rating)

        //Disconnect the proxy from MATLAB
        proxy.proxy.disconnect()
    }
}
