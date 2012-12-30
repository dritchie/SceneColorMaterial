package colorinference

import java.util.LinkedHashMap
import java.util.Collections
import java.util.Map

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/24/12
 * Time: 12:40 PM
 * To change this template use File | Settings | File Templates.
 */

/*
 * General class for caching stuff. Basically acts as a capacity-constrained HashMap
 * Passing -1 for the capacity means that it has unlimited capacity
 */
class Cache[K,V](val capacity:Int = -1)
{
    private val cache = Collections.synchronizedMap(new LinkedHashMap[K,V]
    {
        protected override def removeEldestEntry(eldest:Map.Entry[K,V]) : Boolean =
        {
            size > capacity
        }
    })

    def get(k:K) : Option[V] =
    {
        val result = cache.get(k)
        if (result == null)
            None
        else
            Some(result)
    }

    def put(k:K, v:V)
    {
        cache.put(k, v)
    }
}
