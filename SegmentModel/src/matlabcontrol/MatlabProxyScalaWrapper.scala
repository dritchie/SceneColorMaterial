package matlabcontrol

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/17/12
 * Time: 6:22 PM
 * To change this template use File | Settings | File Templates.
 */


/*
Incredibly stupid, but this is required to prevent some IllegalAccessExceptions when trying to call
some matlabcontrol methods from within our own package in scala.
 */
class MatlabProxyScalaWrapper(val proxy:MatlabProxy)
{
    def eval(command:String)
    {
        proxy.eval(command)
    }

    def returningEval(command:String, nargout:Int) : Array[java.lang.Object] =
    {
        proxy.returningEval(command, nargout)
    }

    def feval(functionName:String, args:java.lang.Object*)
    {
        proxy.feval(functionName, args:_*)
    }

    def returningFeval(functionName:String, nargout:Int, args:java.lang.Object*) : Array[java.lang.Object] =
    {
        proxy.returningFeval(functionName, nargout, args:_*)
    }
}
