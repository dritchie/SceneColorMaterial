using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Engine
{
    //adapted from: http://programanddesign.com/cs/a-simple-priority-queue-in-cs/
    public class PriorityQueue<TValue> : PriorityQueue<TValue, int> { }

    public class PriorityQueue<TValue, TPriority> where TPriority : IComparable
    {
        private SortedDictionary<TPriority, SortedSet<TValue>> dict = new SortedDictionary<TPriority, SortedSet<TValue>>();
        private Dictionary<TValue, TPriority> keyToVal = new Dictionary<TValue, TPriority>();

        public int Count { get; private set; }
        public bool Empty { get { return Count == 0; } }

        public void Enqueue(TValue val)
        {
            Enqueue(val, default(TPriority));
        }

        public void Enqueue(TValue val, TPriority pri)
        {
            ++Count;
            if (!dict.ContainsKey(pri)) dict[pri] = new SortedSet<TValue>();
            dict[pri].Add(val);

            keyToVal.Add(val, pri);

        }

        public KeyValuePair<TValue, TPriority> Dequeue()
        {
            --Count;
            var item = dict.Last();
            if (item.Value.Count == 1) dict.Remove(item.Key);

            TValue removed = item.Value.First();
            item.Value.Remove(removed);

            keyToVal.Remove(removed);

            return new KeyValuePair<TValue, TPriority>(removed, item.Key);
        }

        public void Remove(TValue key)
        {
            if (keyToVal.ContainsKey(key))
            {
                --Count;
                TPriority pri = keyToVal[key];
                keyToVal.Remove(key);
                var set = dict[pri];
                if (set.Count == 1) 
                    dict.Remove(pri);
                else 
                    set.Remove(key);
            }

        }

    }
}
