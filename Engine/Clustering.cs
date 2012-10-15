using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Diagnostics;
using System.IO;

namespace Engine
{
    /**
     * For K-means Clustering
     **/
    public class Cluster
    {
        public int id;
        public CIELAB lab;
        private CIELAB sumlab;
        public double count;

        public Cluster()
        {
            lab = new CIELAB();
            sumlab = new CIELAB();
            count = 0;
            id = -1;
        }

        public CIELAB MeanColor()
        {
            if (count == 0)
                return lab;
            return new CIELAB(sumlab.L / (double)count, sumlab.A / (double)count, sumlab.B / (double)count);
        }

        public void AddColor(CIELAB color, double weight=1)
        {
            sumlab.L += color.L*weight;
            sumlab.A += color.A*weight;
            sumlab.B += color.B*weight;
            count += weight;
        }
        public void Reset()
        {
            lab = MeanColor();
            count = 0;
            sumlab = new CIELAB(0, 0, 0);
        }
    }

    /**
     * For agglomerative clustering
     **/
    public class PixelCluster
    {
        public int id;
        public CIELAB lab;
        public int count;

        public SortedSet<int> neighbors;
        public int parentId;
        public int[] children;

        public PixelCluster()
        {
            id = -1;
            lab = new CIELAB();
            count = 1;

            neighbors = new SortedSet<int>();
            parentId = -1;
            children = new int[] { };
        }

        public PixelCluster(int i, CIELAB color)
        {
            id = i;
            lab = color;
            count = 1;

            neighbors = new SortedSet<int>();
            parentId = i;
            children = new int[] { };
        }
    }

    public class Clustering
    {
        public static List<Cluster> InitializePictureSeeds(List<CIELAB> colors, int k)
        {
            //initialize k seeds, randomly choose colors in LAB space
            //find extents
            List<Cluster> seeds = new List<Cluster>();
            Random random = new Random();

            //sample colors in LAB bounding box
            double Lmin = double.PositiveInfinity;
            double Lmax = double.NegativeInfinity;
            double Amin = double.PositiveInfinity;
            double Amax = double.NegativeInfinity;
            double Bmin = double.PositiveInfinity;
            double Bmax = double.NegativeInfinity;

            for (int i = 0; i < colors.Count(); i++)
            {
                CIELAB lab = colors[i];
                Lmin = Math.Min(Lmin, lab.L);
                Lmax = Math.Max(Lmax, lab.L);
                Amin = Math.Min(Amin, lab.A);
                Amax = Math.Max(Amax, lab.A);
                Bmin = Math.Min(Bmin, lab.B);
                Bmax = Math.Max(Bmax, lab.B);
            }



            //initialize the seeds (stratified) randomly
            //within the bounding box
            if (k <= 10)
            {
                for (int i = 0; i < k; i++)
                {
                    double L = random.NextDouble() * (Lmax - Lmin) + Lmin;
                    double A = random.NextDouble() * (Amax - Amin) + Amin;
                    double B = random.NextDouble() * (Bmax - Bmin) + Bmin;
                    CIELAB seed = new CIELAB(L, A, B);
                    Cluster cluster = new Cluster();
                    cluster.id = i;
                    cluster.lab = seed;
                    seeds.Add(cluster);
                }
            }
            else
            {
                //stratified
                //find closest floor perfect square. 
                //TODO: need to generalize this better, doesn't work for non-perfect squares
                int sideLength = 2;

                int numSamples = (int)Math.Floor(k / (double)(sideLength*sideLength*sideLength));
                int i = 0;

                for (int l = 0; l < sideLength; l++)
                {
                    double dLmax = (Lmax - Lmin) / sideLength * (l+1) + Lmin;
                    double dLmin = (Lmax - Lmin) / sideLength * l + Lmin;

                    for (int a = 0; a < sideLength; a++)
                    {
                        double dAmax = (Amax - Amin) / sideLength * (a + 1) + Amin;
                        double dAmin = (Amax - Amin) / sideLength * a + Amin;

                        for (int b = 0; b < sideLength; b++)
                        {
                            double dBmax = (Bmax - Bmin) / sideLength * (b + 1) + Bmin;
                            double dBmin = (Bmax - Bmin) / sideLength * b + Bmin;

                            int dSamples = numSamples;

                            if (b == sideLength - 1 && a == sideLength - 1 && l == sideLength - 1)
                            {
                                //figure out leftovers
                                dSamples = k - numSamples*(sideLength * sideLength * sideLength - 1);

                            }
                            for (int s = 0; s < dSamples; s++)
                            {
                                double L = random.NextDouble() * (dLmax - dLmin) + dLmin;
                                double A = random.NextDouble() * (dAmax - dAmin) + dAmin;
                                double B = random.NextDouble() * (dBmax - dBmin) + dBmin;
                                CIELAB seed = new CIELAB(L, A, B);
                                Cluster cluster = new Cluster();
                                cluster.id = i;
                                cluster.lab = seed;
                                seeds.Add(cluster);
                                i++;
                            }

                        }
                    }
                }


            }


            return seeds;
        }

        public static double CMeansPicture(List<CIELAB> colors, List<Cluster> seeds, int m=2)
        {
            //cluster colors given seeds
            //return score
            List<double> weights = new List<double>();
            double[,] memberships = new double[colors.Count(), seeds.Count()]; //pixel to cluster
            int numSeeds = seeds.Count();
            int numColors = colors.Count();

            int maxIters = 50;//100;
            double epsilon = 0.0001;

            double J = Double.PositiveInfinity;

            int changes = 0;
            for (int t = 0; t < maxIters; t++)
            {
                changes = 0;
                for (int i = 0; i < colors.Count(); i++)
                {
                    //calculate the memberships
                    double[] dists = new double[numSeeds];
                    double factor = 0;
                    for (int k = 0; k < numSeeds; k++)
                    {
                        dists[k] = Math.Max(epsilon, Math.Pow(Math.Sqrt(colors[i].SqDist(seeds[k].lab)), 2.0/(m-1)));
                        factor += (1.0 / dists[k]);
                    }
                    for (int k = 0; k < numSeeds; k++)
                    {
                        double oldval = memberships[i, k];
                        memberships[i, k] = 1.0 / (dists[k] * factor);
                        if (oldval != memberships[i, k])
                            changes++;
                    }        
                }

                //update the centers
                for (int k = 0; k < numSeeds; k++)
                {
                    CIELAB center = new CIELAB();
                    double total = 0;
                    for (int i = 0; i < numColors; i++)
                    {
                        double u = Math.Pow(memberships[i, k], m);
                        center += colors[i]*u;
                        total += u;
                    }
                    center = center / total;
                    seeds[k].lab = center;
                }

                //find J
                double thisJ = 0;
                for (int i = 0; i < numColors; i++)
                {
                    for (int k = 0; k < numSeeds; k++)
                    {
                        double u = memberships[i, k];
                        thisJ += Math.Pow(u, m)* Math.Max(epsilon, seeds[k].lab.SqDist(colors[i]));
                    }
                }

                if (thisJ >= J)
                    break;

                J = thisJ;

                if (changes == 0)
                    break;
            }



            return J;

        }


        public static double KMeansPicture(List<CIELAB> colors, List<Cluster> seeds, List<double> inWeights=null)
        {
            //cluster colors given seeds
            //return score
            List<double> weights = new List<double>();
            if (inWeights == null)
            {
                for (int i = 0; i < colors.Count(); i++)
                    weights.Add(1);
            }
            else
            {
                weights = new List<double>(inWeights);
            }

            Random r = new Random();

            //go through colors
            int[] assignments = new int[colors.Count()];
            int changes = 1;
            int maxIters = 50;//100;//30;//100;

            for (int t = 0; t < maxIters; t++)
            {
                changes = 0;
                for (int i = 0; i < colors.Count(); i++)
                {
                    double bestDist = double.PositiveInfinity;
                    int bestSeed = -1;

                    //go through seeds and pick best one
                    foreach (Cluster seed in seeds)
                    {
                        double dist = seed.lab.SqDist(colors[i]);
                        if (dist < bestDist)
                        {
                            bestDist = dist;
                            bestSeed = seed.id;
                        }
                    }

                    //check the assignment
                    if (assignments[i] != bestSeed)
                        changes++;
                    assignments[i] = bestSeed;
                    seeds[bestSeed].AddColor(colors[i],weights[i]);
                }

                //update means
                for (int i = 0; i < seeds.Count(); i++)
                {
                    //if seed is starved, try again, just pick a random color
                    if (seeds[i].count == 0)
                    {
                        seeds[i].lab = colors[r.Next(colors.Count)];
                        changes++;
                    }
                    //return double.PositiveInfinity;
                    else
                        seeds[i].Reset();
                }

                if (changes == 0)
                    break;
            }

            //find the sq error
            double score = 0;
            for (int i = 0; i < assignments.Count(); i++)
            {
                int seed = assignments[i];
                score += weights[i]*colors[i].SqDist(seeds[seed].lab);
            }

            return score;

        }
    }

    public class AgglomerativeClustering
    {
        public Dictionary<int, PixelCluster> clusters;
        public SortedSet<int> rootIds;

        private double minL = 0;
        private double maxL = 100;
        private double minA = -100;
        private double maxA = 100;
        private double minB = -100;
        private double maxB = 100;

        private double Lbins = 10;
        private double Abins = 10;
        private double Bbins = 10;

        public AgglomerativeClustering()
        {
            clusters = new Dictionary<int, PixelCluster>();
        }


        private int ToIndex(int x, int y, int width)
        {
            return width * y + x;
        }

        private int GetBin(double value, double min, double max, double numBins)
        {
            return (int)Math.Min(Math.Floor(numBins * (Math.Max(value, min) - min) / (max - min)), numBins - 1);
        }

        private int GetBinId(CIELAB color)
        {
            int Lbin = GetBin(color.L, minL, maxL, Lbins);
            int Abin = GetBin(color.A, minA, maxA, Abins);
            int Bbin = GetBin(color.B, minB, maxB, Bbins);

            return (int)(Abins * Bbins * Lbin + Abins * Bbin + Abin);
        }

        public void ClusterColors(List<CIELAB> colors, int width, int height)
        {
            //Bin the colors
            int minRegions = 5;
            double maxDist = 10*10;
            SortedSet<int> activeClusterIds = new SortedSet<int>();
            String logFile = "log-colorspace.txt";
            StreamWriter log = File.AppendText(logFile);
            log.WriteLine("\n\tCluster Color Space " + DateTime.Now.ToString());
            log.Flush();

            //the smaller id comes first in the dictionary for pairwise distances
            PriorityQueue<Tuple<int, int>, double> pq = new PriorityQueue<Tuple<int, int>, double>();

            clusters = new Dictionary<int, PixelCluster>();
            int counter = 0;

            foreach (CIELAB color in colors)
            {
                //bin it into one of the clusters
                //index is a first, then b, then L
                int id = GetBinId(color);

                if (id > counter)
                    counter = id;

                if (!clusters.ContainsKey(id))
                {
                    clusters.Add(id, new PixelCluster(id, color));
                } 
                else
                {
                    clusters[id].lab = (clusters[id].lab * clusters[id].count + color) / (clusters[id].count + 1);
                    clusters[id].count++;
                }
            }
            counter++;

            activeClusterIds = new SortedSet<int>(clusters.Keys);

            List<int> ids = activeClusterIds.ToList<int>();
            for (int i=0; i<ids.Count(); i++)
            {
                PixelCluster a = clusters[ids[i]];

                //calculate distances to neighbors larger than current id
                for (int j=i+1; j<ids.Count(); j++)
                {
                    PixelCluster b = clusters[ids[j]];

                    double newDist = a.lab.SqDist(b.lab);
                    //newDist = newDist * Math.Sqrt(2 * a.count * b.count / (a.count + b.count));
                    pq.Enqueue(new Tuple<int, int>(a.id, b.id), -1*newDist);
                }

            }

            Stopwatch timer = new Stopwatch();
            timer.Start();

            while (activeClusterIds.Count > minRegions)
            {
                //Find the pair with the smallest distance
                KeyValuePair<Tuple<int, int>, double> result = BestPair(pq, activeClusterIds);
                Tuple<int, int> pair = result.Key;
                double bestDist = -1 * result.Value;

                Console.WriteLine("num clusters: " + activeClusterIds.Count());

                if (bestDist > maxDist)
                    break;

                PixelCluster a = clusters[pair.Item1];
                PixelCluster b = clusters[pair.Item2];

                //Create a new cluster with unique id, don't care about neighbors
                PixelCluster merged = new PixelCluster();
                merged.id = counter++;
                merged.lab = (a.lab * a.count + b.lab * b.count) / (a.count + b.count);
                merged.count = a.count + b.count;
                merged.children = new int[] { a.id, b.id };
                merged.parentId = merged.id;
                a.parentId = merged.id;
                b.parentId = merged.id;
                clusters.Add(merged.id, merged);

                //Update the active cluster set
                activeClusterIds.Remove(a.id);
                activeClusterIds.Remove(b.id);
                activeClusterIds.Add(merged.id);

                double totalCount = a.count + b.count;

                //Update the distances, based on old distances
                foreach (int i in activeClusterIds)
                {
                    //Debug.Assert(i != merged.id && activeClusterIds.Contains(i));

                    //TODO: Ward's method with minimum variance
                    //For now, just use the dist between the centroids
                    if (i == merged.id)
                        continue;

                    PixelCluster c = clusters[i];
                    double newDist = merged.lab.SqDist(c.lab);


                    //Add in Ward's variance  (variation in Color Segmentation using Region Merging)
                    //http://www.mathworks.com/help/toolbox/stats/linkage.html
                    //newDist = newDist * Math.Sqrt(2 * a.count * b.count / (a.count + b.count));

                    if (c.id < merged.id)
                        pq.Enqueue(new Tuple<int, int>(c.id, merged.id), -1 * newDist);
                    else
                        pq.Enqueue(new Tuple<int, int>(merged.id, c.id), -1 * newDist);

                }

                //Remove the old clusters
                foreach (int i in a.neighbors)
                {
                    if (i > a.id)
                        pq.Remove(new Tuple<int, int>(a.id, i));
                    else
                        pq.Remove(new Tuple<int, int>(i, a.id));
                }
                foreach (int i in b.neighbors)
                {
                    if (i > b.id)
                        pq.Remove(new Tuple<int, int>(b.id, i));
                    else
                        pq.Remove(new Tuple<int, int>(i, b.id));
                }



                //Repeat until termination criteria

                if (activeClusterIds.Count() % 1000 == 0)
                {
                    //write to file
                    log.WriteLine("Merge loop: " + timer.ElapsedMilliseconds / 1000.0 + " # Clusters: " + activeClusterIds.Count() + " pqCount: " + pq.Count + " merged # neighbors: " + merged.neighbors.Count);
                    log.Flush();
                    timer.Restart();
                }

            }
            rootIds = activeClusterIds;
            timer.Stop();
            log.WriteLine("End: " + timer.ElapsedMilliseconds / 1000.0 + " # Clusters: " + activeClusterIds.Count() + " pqCount: " + pq.Count);
            log.WriteLine("End Time: " + DateTime.Now.ToString());
            log.Flush();
            log.Close();
            
            

        }

        /**
         * Now the Agglomerative Clustering. May want to pass in additional parameters, like the termination
         * criteria and parameters.
         * Assumes list of colors are in row-order
         **/
        public void Cluster(List<CIELAB> colors, int width, int height)
        {
            double maxDist = 50*10;
            SortedSet<int> activeClusterIds = new SortedSet<int>();
            String logFile = "log.txt";
            StreamWriter log = File.AppendText(logFile);
            log.WriteLine("\n\tCluster Spatial Run " + DateTime.Now.ToString());
            log.Flush();
            

            //the smaller id comes first in the dictionary for pairwise distances
            PriorityQueue<Tuple<int, int>, double> pq = new PriorityQueue<Tuple<int, int>, double>();

            clusters = new Dictionary<int, PixelCluster>();

            int counter = 0;

            //Initialize the clusters in row-order
            for (int j = 0; j < height; j++)
            {
                for (int i = 0; i < width; i++)
                {
                    activeClusterIds.Add(counter);
                    PixelCluster p = new PixelCluster(counter, colors[width * j + i]);
                    counter++;
                    
                    //Initialize the 4-neighbors
                    if (i > 0)
                        p.neighbors.Add(ToIndex(i - 1, j, width));
                    if (j > 0)
                        p.neighbors.Add(ToIndex(i, j - 1, width));
                    if (i < width - 1)
                        p.neighbors.Add(ToIndex(i + 1, j, width));
                    if (j < height - 1)
                        p.neighbors.Add(ToIndex(i, j + 1, width));

                    clusters.Add(p.id, p);
                }
            }

            foreach (int i in activeClusterIds)
            {
                //calculate distances to neighbors larger than current id
                SortedSet<int> neighbors = Simplify(clusters[i].neighbors);
                foreach (int j in neighbors)
                {
                    if (i < j)
                    {
                        pq.Enqueue(new Tuple<int, int>(i, j), -1*clusters[i].lab.SqDist(clusters[j].lab));
                    }
                }

            }

            Stopwatch timer = new Stopwatch();
            timer.Start();

            while (activeClusterIds.Count > 1)
            {

                //Find the pair with the smallest distance
                KeyValuePair<Tuple<int, int>, double> result = BestPair(pq, activeClusterIds);
                Tuple<int, int> pair = result.Key;
                double bestDist = -1*result.Value;

                Console.WriteLine("num clusters: " + activeClusterIds.Count());

                if (bestDist > maxDist)
                    break;

                PixelCluster a = clusters[pair.Item1];
                PixelCluster b = clusters[pair.Item2];

                //Create a new cluster with unique id
                PixelCluster merged = new PixelCluster();
                merged.id = counter++;
                merged.lab = (a.lab * a.count + b.lab * b.count) / (a.count + b.count);
                merged.count = a.count + b.count;
                merged.children = new int[] { a.id, b.id };
                merged.neighbors = MergeNeighbors(a.id, b.id);
                merged.parentId = merged.id;
                a.parentId = merged.id;
                b.parentId = merged.id;
                clusters.Add(merged.id, merged);

                //Update the active cluster set
                activeClusterIds.Remove(a.id);
                activeClusterIds.Remove(b.id);
                activeClusterIds.Add(merged.id);

                double totalCount = a.count + b.count;

                //Update the distances, based on old distances
                foreach (int i in merged.neighbors)
                {
                    //Debug.Assert(i != merged.id && activeClusterIds.Contains(i));

                    //TODO: Ward's method with minimum variance
                    //For now, just use the dist between the centroids
                    PixelCluster c = clusters[i];
                    double newDist = merged.lab.SqDist(c.lab);


                    //Add in Ward's variance  (variation in Color Segmentation using Region Merging)
                    //http://www.mathworks.com/help/toolbox/stats/linkage.html
                    newDist = newDist * Math.Sqrt(2 * a.count * b.count / (a.count + b.count));

                    if (c.id < merged.id)
                        pq.Enqueue(new Tuple<int, int>(c.id, merged.id), -1 * newDist);
                    else
                        pq.Enqueue(new Tuple<int, int>(merged.id, c.id), -1 * newDist);

                }

                //Remove the old clusters
                foreach (int i in a.neighbors)
                {
                    if (i > a.id)
                        pq.Remove(new Tuple<int, int>(a.id, i));
                    else
                        pq.Remove(new Tuple<int, int>(i, a.id));
                }
                foreach (int i in b.neighbors)
                {
                    if (i > b.id)
                        pq.Remove(new Tuple<int, int>(b.id, i));
                    else
                        pq.Remove(new Tuple<int, int>(i, b.id));
                }



                //Repeat until termination criteria

                if (activeClusterIds.Count() % 1000 == 0)
                {
                    //write to file
                    log.WriteLine("Merge loop: " + timer.ElapsedMilliseconds/1000.0 + " # Clusters: " + activeClusterIds.Count() + " pqCount: " + pq.Count + " merged # neighbors: " + merged.neighbors.Count );
                    log.Flush();
                    timer.Restart();
                }

            }
            rootIds = activeClusterIds;
            timer.Stop();
            log.WriteLine("End: " + timer.ElapsedMilliseconds/1000.0 + " # Clusters: " + activeClusterIds.Count() + " pqCount: " + pq.Count);
            log.WriteLine("End Time: " + DateTime.Now.ToString());
            log.Flush();
            log.Close();
        }


        //Cluster the final clusters into color space
        public void ClusterColorSpace()
        {
            double maxDist = 20*20;
            int minRegions = 5;

            SortedSet<int> activeClusterIds = new SortedSet<int>(rootIds);
            String logFile = "colorlog.txt";
            StreamWriter log = File.AppendText(logFile);
            log.WriteLine("\n\tCluster ColorSpace Run " + DateTime.Now.ToString());
            log.Flush();


            //the smaller id comes first in the dictionary for pairwise distances
            PriorityQueue<Tuple<int, int>, double> pq = new PriorityQueue<Tuple<int, int>, double>();

            int counter = activeClusterIds.Last()+1;

            int[] ids = activeClusterIds.ToArray<int>();

            //Calculate the initial distances
            for (int i = 0; i < ids.Count(); i++)
            {
                for (int j = i+1; j < ids.Count(); j++)
                {
                    //log.WriteLine(ids[i] + ", " + ids[j] + " dist " + -1 * clusters[ids[i]].lab.SqDist(clusters[ids[j]].lab));
                    //log.Flush();

                    //pq.Enqueue(new Tuple<int, int>(ids[i], ids[j]), -1 * clusters[ids[i]].lab.SqDist(clusters[ids[j]].lab));
                    PixelCluster a = clusters[ids[i]];
                    PixelCluster b = clusters[ids[j]];

                    double newDist = a.lab.SqDist(b.lab);

                    //Add in Ward's variance  (variation in Color Segmentation using Region Merging)
                    //http://www.mathworks.com/help/toolbox/stats/linkage.html
                    //newDist = newDist * Math.Sqrt(2 * a.count * b.count / (a.count + b.count));

                    pq.Enqueue(new Tuple<int, int>(ids[i], ids[j]), -1 * newDist);
                }
            }

            Stopwatch timer = new Stopwatch();
            timer.Start();

            while (activeClusterIds.Count > minRegions)
            {

                //Find the pair with the smallest distance
                KeyValuePair<Tuple<int, int>, double> result = BestPair(pq, activeClusterIds);
                Tuple<int, int> pair = result.Key;
                double bestDist = -1 * result.Value;

                Console.WriteLine("num clusters: " + activeClusterIds.Count());

                if (bestDist > maxDist)
                    break;

                PixelCluster a = clusters[pair.Item1];
                PixelCluster b = clusters[pair.Item2];

                //Create a new cluster with unique id, we don't care about the neighbors
                PixelCluster merged = new PixelCluster();
                merged.id = counter++;
                merged.lab = (a.lab * a.count + b.lab * b.count) / (a.count + b.count);
                merged.count = a.count + b.count;
                merged.children = new int[] { a.id, b.id };
                merged.parentId = merged.id;
                a.parentId = merged.id;
                b.parentId = merged.id;
                clusters.Add(merged.id, merged);

                //Update the active cluster set
                activeClusterIds.Remove(a.id);
                activeClusterIds.Remove(b.id);
                activeClusterIds.Add(merged.id);

                double totalCount = a.count + b.count;

                //Update the distances, based on old distances
                foreach (int i in activeClusterIds)
                {
                    if (i == merged.id)
                        continue;

                    //TODO: Ward's method with minimum variance
                    //For now, just use the dist between the centroids
                    PixelCluster c = clusters[i];
                    double newDist = merged.lab.SqDist(c.lab);


                    //Add in Ward's variance  (variation in Color Segmentation using Region Merging)
                    //http://www.mathworks.com/help/toolbox/stats/linkage.html
                    //newDist = newDist * Math.Sqrt(2*a.count * b.count / (a.count + b.count));
                   
                    if (c.id < merged.id)
                        pq.Enqueue(new Tuple<int, int>(c.id, merged.id), -1 * newDist);
                    else
                        pq.Enqueue(new Tuple<int, int>(merged.id, c.id), -1 * newDist);

                }

                //Remove the old clusters
                foreach (int i in activeClusterIds)
                {
                    if (i > a.id)
                        pq.Remove(new Tuple<int, int>(a.id, i));
                    else
                        pq.Remove(new Tuple<int, int>(i, a.id));
                }
                foreach (int i in activeClusterIds)
                {
                    if (i > b.id)
                        pq.Remove(new Tuple<int, int>(b.id, i));
                    else
                        pq.Remove(new Tuple<int, int>(i, b.id));
                }



                //Repeat until termination criteria

                if (activeClusterIds.Count() % 1000 == 0)
                {
                    //write to file
                    log.WriteLine("Merge loop: " + timer.ElapsedMilliseconds / 1000.0 + " # Clusters: " + activeClusterIds.Count() + " pqCount: " + pq.Count + " merged # neighbors: " + merged.neighbors.Count);
                    log.Flush();
                    timer.Restart();
                }

            }
            rootIds = activeClusterIds;
            timer.Stop();
            log.WriteLine("End: " + timer.ElapsedMilliseconds / 1000.0 + " # Clusters: " + activeClusterIds.Count() + " pqCount: " + pq.Count);
            log.WriteLine("End Time: " + DateTime.Now.ToString());
            log.Flush();
            log.Close();
        }

        public Bitmap DebugImage(int width, int height)
        {
            Bitmap result = new Bitmap(width, height);
            
            //row-order ids
            for (int j = 0; j < height; j++)
            {
                for (int i = 0; i < width; i++)
                {
                    //find the parent
                    int clusterId = Find(j * width + i);
                    Debug.Assert(rootIds.Contains(clusterId));

                    Color color = Util.LABtoRGB(clusters[clusterId].lab);
                    result.SetPixel(i, j, color);
                }
            }

            return result;
        }

        //Debug image if clustered in color space
        public Bitmap DebugImageColors(List<CIELAB> colors, int width, int height)
        {
            Bitmap result = new Bitmap(width, height);

            //row-order ids
            for (int j = 0; j < height; j++)
            {
                for (int i = 0; i < width; i++)
                {
                    //get the bin
                    int id = GetBinId(colors[j * width + i]);

                    //find the parent
                    int clusterId = Find(id);
                    Debug.Assert(rootIds.Contains(clusterId));

                    Color color = Util.LABtoRGB(clusters[clusterId].lab);
                    result.SetPixel(i, j, color);
                }
            }

            return result;
        }


        private KeyValuePair<Tuple<int, int>, double> BestPair(PriorityQueue<Tuple<int, int>, double> pq, SortedSet<int> activeClusterIds)
        {
            KeyValuePair<Tuple<int, int>, double> best = pq.Dequeue();
            while (!(activeClusterIds.Contains(best.Key.Item1) && activeClusterIds.Contains(best.Key.Item2)))
            {
                best = pq.Dequeue();
            }
            return best;
        }

        //follow the parent pointer until we reach the root
        private int Find(int id)
        {
            PixelCluster p = clusters[id];
            if (p.id == p.parentId)
                return p.id;

            int parentId = p.parentId;
            while (parentId != clusters[parentId].parentId)
            {
                parentId = clusters[parentId].parentId;
            }
            //compress the path
            p.parentId = parentId;

            return parentId;
        }

        private SortedSet<int> MergeNeighbors(int i, int j)
        {
            PixelCluster a = clusters[i];
            PixelCluster b = clusters[j];
            SortedSet<int> merged = Simplify(a.neighbors);
            merged.UnionWith(Simplify(b.neighbors));
            merged.Remove(a.id);
            merged.Remove(b.id);

            return merged;
        }

        private SortedSet<int> Simplify(SortedSet<int> neighbors)
        {
            SortedSet<int> result = new SortedSet<int>();
            foreach (int i in neighbors)
            {
                result.Add(Find(i));
            }
            return result;
        }

        private bool ValidNeighbors(int i, int j)
        {
            //check that j's cluster is a neighbor of i's cluster
            SortedSet<int> ni = Simplify(clusters[i].neighbors);
            int pj = Find(j);

            return ni.Contains(pj);
        }


    }

}
