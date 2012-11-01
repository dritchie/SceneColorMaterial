using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace Engine
{
    //Simple Union Find for images
    public class UnionFind<T>
    {
        int[] parents;
        Func<T, T, bool> compare;
       
        public UnionFind(Func<T,T,bool> comp)
        {
            compare = comp;
        }

        public Bitmap RenderComponents(int[,] assignments)
        {
            //render the components
            int width = assignments.GetLength(0);
            int height = assignments.GetLength(1);

            Dictionary<int, Color> idToColor = new Dictionary<int, Color>();
            idToColor.Add(-1, Color.White);

            Bitmap result = new Bitmap(width, height);

            Random random = new Random();

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    int id = assignments[i, j];
                    if (!idToColor.ContainsKey(id))
                        idToColor.Add(id, Color.FromArgb(random.Next(0, 256), random.Next(0, 256), random.Next(256)));
                    result.SetPixel(i,j,idToColor[id]);
                }
            }

            return result;

        }

        //8-connected
        public int[,] ConnectedComponents(T[,] image, T ignore, int neighborDist=2)
        {
            int width = image.GetLength(0);
            int height = image.GetLength(1);

            int totalSize = width * height;
            parents = new int[totalSize];

            //initialize each pixel to its own component
            for (int i = 0; i < totalSize; i++)
            {
                Point p = IndexToPoint(i, width);
                if (compare(image[p.X, p.Y], ignore))
                    parents[i] = -1;
                else
                    parents[i] = i;
            }

            //now start merging components
            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    int id = PointToIndex(new Point(i, j), width);

                    //check the 8 neighbors
                    for (int dx = -neighborDist; dx <= neighborDist; dx++)
                    {
                        for (int dy = -neighborDist; dy <= neighborDist; dy++)
                        {
                            int x = i + dx;
                            int y = j + dy;
                            if (x >= 0 && x < width && y >= 0 && y < height)
                            {
                                int nid = PointToIndex(new Point(x, y), width);
                                Union(id, nid, image);
                            }
                        }
                    }

                }
            }

            int[,] result = new int[width, height];

            //relabel the result with the root ids, renumbered
            int[] vals = parents.Distinct().ToArray();
            Dictionary<int, int> renumber = new Dictionary<int, int>();
            int counter = 0;

            renumber.Add(-1, -1);
            foreach (int v in vals)
            {
                if (!renumber.ContainsKey(v))
                    renumber.Add(v, counter++);
            }

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    int id = PointToIndex(new Point(i, j), width);
                    result[i, j] = renumber[Find(id)];
                }
            }


            return result;
        }

        public int[,] ConnectedComponents(T[,] image, int neighborDist=2)
        {
            int width = image.GetLength(0);
            int height = image.GetLength(1);

            int totalSize = width * height;
            parents = new int[totalSize];

            //initialize each pixel to its own component
            for (int i = 0; i < totalSize; i++)
            {
                parents[i] = i;
            }

            //now start merging components
            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    int id = PointToIndex(new Point(i, j), width);
                  
                    //check the 8 neighbors (to 2 pixels)
                    for (int dx = -neighborDist; dx <= neighborDist; dx++)
                    {
                        for (int dy = -neighborDist; dy <= neighborDist; dy++)
                        {
                            int x = i + dx;
                            int y = j + dy;
                            if (x >= 0 && x < width && y >= 0 && y < height)
                            {
                                int nid = PointToIndex(new Point(x,y), width);
                                Union(id, nid, image);
                            }
                        }
                    }

                }
            }

            int[,] result = new int[width, height];

            //relabel the result with the root ids, renumbered
            int[] vals = parents.Distinct().ToArray();
            Dictionary<int, int> renumber = new Dictionary<int, int>();
            int counter = 0;
            foreach (int v in vals)
            {
                if (!renumber.ContainsKey(v))
                    renumber.Add(v, counter++);
            }

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    int id = PointToIndex(new Point(i, j), width);
                    result[i, j] = renumber[Find(id)];
                }
            }


            return result;
        }

        private Point IndexToPoint(int idx, int width)
        {
            Point result = new Point();
            result.X = idx % width;
            result.Y = idx / width;
            return result;
        }

        private int PointToIndex(Point p, int width)
        {
            int idx = p.Y * width + p.X;
            return idx;
        }

        

        private bool Union(int aid, int bid, T[,] image)
        {
            //union the two if they can be unioned
            int width = image.GetLength(0);
            Point a = IndexToPoint(aid, width);
            Point b = IndexToPoint(bid, width);

            //ignore background
            if (Find(aid) < 0 || Find(bid) < 0)
                return false;

            //already unioned
            if (Find(aid) == Find(bid))
                return false;

            //not equal, and so cannot be unioned
            if (!compare(image[a.X, a.Y], image[b.X, b.Y]))
                return false;
            
            //now union the two
            int pa = Find(aid);
            int pb = Find(bid);

            if (pa < pb)
                parents[pb] = pa;
            else
                parents[pa] = pb;

            return true;
        }

        private int Find(int id)
        {
            int p = id;

            if (p < 0 || parents[p] < 0)
                return -1;

            if (p == parents[p])
                return p;

            int parentId = parents[p];
            while (parentId != parents[parentId])
            {
                parentId = parents[parentId];
            }
            //compress the path
            parents[parentId] = parentId;

            return parentId;
        }

    }
}
