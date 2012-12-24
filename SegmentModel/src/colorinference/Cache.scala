package colorinference

import collection.mutable
import compat.Platform

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
class Cache[K,V](var capacity:Int = -1)
{
    private class Entry(val v:V, var lastAccessed:Long)
    private val cache = new mutable.HashMap[K,Entry]
    var hits = 0
    var misses = 0
    var evictions = 0

    def get(k:K) : Option[V] =
    {
        var entry:Entry = null
        try { entry = cache(k) }
        catch { case nsee:NoSuchElementException => misses += 1; return None}

        hits += 1
        Some(entry.v)
    }

    def put(k:K, v:V)
    {
        // Only add something if this mapping doesn't already exist
        try {cache(k)}
        catch { case nsee:NoSuchElementException =>
        {
            // Evict least recently used entry if we're at capacity
            if (cache.size == capacity)
                evictLRU()

            // Add the mapping
            val entry = new Entry(v, Platform.currentTime)
            cache.put(k, entry)
        }}
    }

    private def evictLRU()
    {
        evictions += 1
        val lru = cache.reduce((a,b) => if (a._2.lastAccessed < b._2.lastAccessed) a else b)
        cache.remove(lru._1)
    }

    def report()
    {
        println("hits: " + hits)
        println("misses: " + misses)
        println("evictions: " + evictions)
    }
}
