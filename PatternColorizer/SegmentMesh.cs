using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Engine;
using System.Drawing;
using System.IO;
using Emgu.CV;
using Emgu.Util;
using Emgu.CV.Structure;


namespace PatternColorizer
{
    class NamedFeature
    {
        public String name;
        public List<Double> values;

        public NamedFeature(String n)
        {
            name = n;
            values = new List<Double>();
        }
        public NamedFeature(String n, List<Double> v)
        {
            name = n;
            values = new List<Double>(v);
        }
    }

    class Segment
    {
        public SortedSet<int> adjacencies;
        public List<NamedFeature> features;
        public List<Point> points;
        public int groupId;
        public int assignmentId;

        public Segment(int id)
        {
            adjacencies = new SortedSet<int>();
            features = new List<NamedFeature>();
            points = new List<Point>();
            assignmentId = id;
        }
    }

    class SegmentGroup
    {
        public SortedSet<int> members;
        public List<NamedFeature> features;
        public Color observed;

        public SegmentGroup(Color c)
        {
            observed = c;
            members = new SortedSet<int>();
            features = new List<NamedFeature>();
        }
    }

    class SegmentMesh
    {
        public List<Segment> segments;
        public List<SegmentGroup> groups;
        int imageWidth = 0;
        int imageHeight = 0;
        int[,] assignments;


        public SegmentMesh(ColorTemplate template)
        {
            int numGroups = template.NumSlots();
            imageWidth = template.Width();
            imageHeight = template.Height();

            groups = new List<SegmentGroup>();
            segments = new List<Segment>();

            //now populate the segments and segment groups
            //filter out groups that have no members (due to quantization)
            Dictionary<int, int> slotToGroup = new Dictionary<int, int>();
            for (int i = 0; i < numGroups; i++)
            {
                if (template.PixelsInSlot(i) > 0)
                {
                    slotToGroup.Add(i, groups.Count());
                    groups.Add(new SegmentGroup(template.OriginalSlotColor(i)));
                }
            }

            UnionFind<Color> uf = new UnionFind<Color>((a, b) => (a.GetHashCode() == b.GetHashCode()));
            Bitmap image = template.DebugQuantization();
            assignments = uf.ConnectedComponentsNoiseRemoval(Util.BitmapToArray(image));
            

            Dictionary<int, Segment> idToSegment = new Dictionary<int, Segment>();

           
            //populate segments
            int width = image.Width;
            int height = image.Height;
            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    int id = assignments[i, j];
                    if (!idToSegment.ContainsKey(id))
                        idToSegment.Add(id, new Segment(id));

                    idToSegment[id].points.Add(new Point(i, j));
                    idToSegment[id].groupId = slotToGroup[template.GetSlotId(i, j)];

                    //look for 8-neighbor adjacencies, 2 pixels away
                    //TODO: measure adjacency strength and filter?
                    for (int dx = -2; dx <= 2; dx++)
                    {
                        for (int dy = -2; dy <= 2; dy++)
                        {
                            int x = i + dx;
                            int y = j + dy;
                            if (x >= 0 && x < width && y >= 0 && y < height)
                            {
                                int nid = assignments[x, y];
                                if (nid != id)
                                    idToSegment[id].adjacencies.Add(nid);
                            }
                        }
                    }
                }
            }

            //finalize segment list and adjacency list
            Dictionary<int, int> idToIdx = new Dictionary<int, int>();
            foreach (int id in idToSegment.Keys)
            {
                segments.Add(idToSegment[id]);
                idToIdx.Add(id, segments.Count() - 1);
            }

            //finalize adjacencies
            for (int i = 0; i < segments.Count(); i++)
            {
                SortedSet<int> renamedAdj = new SortedSet<int>();
                foreach (int a in segments[i].adjacencies)
                    renamedAdj.Add(idToIdx[a]);
                segments[i].adjacencies = renamedAdj;
            }

            //finalize groups
            for (int i = 0; i < segments.Count(); i++)
            {
                int groupId = segments[i].groupId;
                groups[groupId].members.Add(i);
            }

            //finalize segment list

            ClassifySegments();

            foreach (Segment s in segments)
            {
                ComputeFeatures(s);
            }

            foreach (SegmentGroup g in groups)
            {
                ComputeFeatures(g);
            }

        }

        public List<Segment> getSegments()
        {
            return segments;
        }

        public List<SegmentGroup> getGroups()
        {
            return groups;
        }


        public void ClassifySegments()
        {
            //label some segments as noise
            
            //label the color group with the largest segment as a background element
            //label all segments in the color group as a background (?), unless it is noise
            //001-noise, 010-background, 100-foreground
            double largestSize = 0;
            int segIdx = -1;
            
            for (int i = 0; i < segments.Count(); i++)
            {
                double size = segments[i].points.Count();
                if (segments[i].assignmentId >= 0 && largestSize < size)
                {
                    largestSize = size;
                    segIdx = i;
                }
            }

            int groupId = segments[segIdx].groupId;

            NamedFeature bg = new NamedFeature("Label");
            bg.values = new List<Double>{0, 1, 0};
            foreach (int idx in groups[groupId].members)
            {
                if (segments[idx].assignmentId >=0)
                    segments[idx].features.Add(bg);
            }

            NamedFeature noise = new NamedFeature("Label");
            noise.values = new List<Double>{0, 0, 1};

            NamedFeature fg = new NamedFeature("Label");
            fg.values = new List<Double> { 1, 0, 0 };

            foreach (Segment s in segments)
            {
                if (s.assignmentId < 0)
                    s.features.Add(noise);
                else if (s.groupId != groupId)
                    s.features.Add(fg);
            }

        }

        private double Dist(PointF a, PointF b)
        {
            return Math.Sqrt(Math.Pow(a.X-b.X,2) + Math.Pow(a.Y-b.Y,2));
        }

        public void ComputeFeatures(Segment s)
        {
            //Add the relative size
            NamedFeature f = new NamedFeature("RelativeSize");
            f.values.Add(s.points.Count() / (double)(imageWidth * imageHeight));
            s.features.Add(f);

            PointF c = NormalizedCentroid(s);
            s.features.Add(new NamedFeature("RelativeCentroid", new List<double>{c.X, c.Y}));


            //Radial distance
            s.features.Add(new NamedFeature("RadialDistance", new List<double>{Math.Sqrt(c.X*c.X+c.Y*c.Y)}));


            //Normalized Discrete Compactness http://www.m-hikari.com/imf-password2009/25-28-2009/bribiescaIMF25-28-2009.pdf
            //Find the segment id
            Point sp = s.points.First();
            int sidx = assignments[sp.X, sp.Y];
            
            //count number of perimeter edges
            int perimeter = 0;
            foreach (Point p in s.points)
            {
                for (int i = -1; i <= 1; i++)
                {
                    for (int j = -1; j <= 1; j++)
                    {
                        if (Math.Abs(i) == Math.Abs(j))
                            continue;
                        if (Util.InBounds(p.X + i, p.Y + j, imageWidth, imageHeight) && assignments[p.X + i, p.Y + j] != sidx)
                            perimeter++;
                    }
                }
            }
            int n = s.points.Count();
            double CD = (4.0 * n - perimeter) / 2;
            double CDmin = n - 1;
            double CDmax = (4 * n - 4 * Math.Sqrt(n)) / 2;
            double CDN = (CD - CDmin) / Math.Max(1,(CDmax - CDmin));
            s.features.Add(new NamedFeature("NormalizedDiscreteCompactness", new List<double> { CDN }));


            //Add elongation (width/length normalized between 0-square to 1-long http://hal.archives-ouvertes.fr/docs/00/44/60/37/PDF/ARS-Journal-SurveyPatternRecognition.pdf         
            PointF[] points = s.points.Select<Point, PointF>(p => new PointF(p.X, p.Y)).ToArray<PointF>();
            Emgu.CV.Structure.MCvBox2D box = Emgu.CV.PointCollection.MinAreaRect(points);

            PointF[] vertices = box.GetVertices();
            double elongation = 1 - Math.Min(box.size.Width + 1, box.size.Height + 1) / Math.Max(box.size.Width + 1, box.size.Height + 1);
            s.features.Add(new NamedFeature("Elongation", new List<double>{elongation}));


            //Add Hu shape moments, invariant to translation, scale, and rotation (not sure what each measure refers to intuitively though, or if there is an intuitive analog)
            //They may also do badly on noisy data however. See: http://hal.archives-ouvertes.fr/docs/00/44/60/37/PDF/ARS-Journal-SurveyPatternRecognition.pdf (called Invariant Moments)

            Bitmap regionBitmap = new Bitmap(imageWidth, imageHeight);
            Graphics g = Graphics.FromImage(regionBitmap);
            g.FillRectangle(new SolidBrush(Color.Black), 0, 0, imageWidth, imageHeight);
            foreach (Point p in s.points)
            {
                regionBitmap.SetPixel(p.X, p.Y, Color.White);
            }

            Emgu.CV.Image<Gray, byte> region = new Emgu.CV.Image<Gray, byte>(regionBitmap);

            MCvMoments moment = region.GetMoments(true);
            MCvHuMoments hu = moment.GetHuMoment();
            s.features.Add(new NamedFeature("HuMoments", new List<double> {hu.hu1, hu.hu2, hu.hu3,hu.hu4,hu.hu5, hu.hu6, hu.hu7 }));
            region.Dispose();
            regionBitmap.Dispose();
        }

        public PointF NormalizedCentroid(Segment s)
        {
            //normalize the points (relative to the image center (0,0))
            PointF[] normalizedPoints = s.points.Select<Point, PointF>(p => new PointF(-0.5f + (float)p.X / imageWidth, -0.5f + (float)p.Y / imageHeight)).ToArray<PointF>();

            //Add the relative centroid 
            float cX = 0;
            float cY = 0;
            foreach (PointF p in normalizedPoints)
            {
                cX += p.X;
                cY += p.Y;
            }
            cX /= normalizedPoints.Count();
            cY /= normalizedPoints.Count();
            return new PointF(cX, cY);
        }

        public void ComputeFeatures(SegmentGroup g)
        {
            double imarea = (double)(imageWidth * imageHeight);

            // Amount of the image this group takes up
            NamedFeature sizef = new NamedFeature("RelativeSize");
            int size = 0;
            foreach (int i in g.members)
            {
                Segment s = segments[i];
                size += s.points.Count();
            }
            sizef.values.Add(size / imarea);
            g.features.Add(sizef);

            // (Normalized) number of segments in the group
            NamedFeature numsegf = new NamedFeature("NormalizedNumSegs");
            numsegf.values.Add(g.members.Count() / (double)(segments.Count()));
            g.features.Add(numsegf);

            // min, max, mean, stddev segment size
            int minsegsize, maxsegsize, segsizesum;
            minsegsize = Int32.MaxValue;
            maxsegsize = 0;
            segsizesum = 0;
            foreach (int i in g.members)
            {
                int s = segments[i].points.Count;
                minsegsize = Math.Min(s, minsegsize);
                maxsegsize = Math.Max(s, maxsegsize);
                segsizesum += s;
            }
            double meansegsize = ((double)segsizesum) / g.members.Count;
            double stddevsegsize = 0;
            foreach (int i in g.members)
            {
                int s = segments[i].points.Count;
                double diff = (s - meansegsize);
                stddevsegsize += (diff * diff);
            }
            stddevsegsize /= g.members.Count;
            NamedFeature segsizestatsf = new NamedFeature("SegmentSizeStatistics");
            segsizestatsf.values.Add(minsegsize / imarea);
            segsizestatsf.values.Add(maxsegsize / imarea);
            segsizestatsf.values.Add(meansegsize / imarea);
            segsizestatsf.values.Add(stddevsegsize / imarea);
            g.features.Add(segsizestatsf);

            // "spread" - covariance of segment centroids
            PointF meanc = new PointF(0, 0);
            foreach (int i in g.members)
            {
                PointF c = NormalizedCentroid(segments[i]);
                meanc.X += c.X;
                meanc.Y += c.Y;
            }
            meanc.X /= g.members.Count;
            meanc.Y /= g.members.Count;
            double[,] covar = new double[,] { { 0, 0 }, { 0, 0 } };
            foreach (int i in g.members)
            {
                PointF c = NormalizedCentroid(segments[i]);
                float x = c.X - meanc.X;
                float y = c.Y - meanc.Y;
                covar[0, 0] += x * x;
                covar[0, 1] += x * y;
                covar[1, 0] += x * y;
                covar[1, 1] += y * y;
            }
            covar[0, 0] /= g.members.Count;
            covar[0, 1] /= g.members.Count;
            covar[1, 0] /= g.members.Count;
            covar[1, 1] /= g.members.Count;
            NamedFeature spreadf = new NamedFeature("SegmentSpread",
                new List<double>{covar[0,0], covar[0,1], covar[1,0], covar[1,1]});
            g.features.Add(spreadf);
        }


        public void WriteToFile(String filename)
        {
            List<String> lines = new List<String>();
            //dump to a file
            foreach (Segment s in segments)
            {
                lines.Add("SegmentBegin");

                foreach (NamedFeature f in s.features)
                {
                    lines.Add(f.name + " " + String.Join(" ", f.values.Select<Double,String>((d)=>d.ToString()).ToArray<String>()));
                }

                lines.Add("AdjacentTo " + String.Join(" ", s.adjacencies.Select<int, String>((d)=>d.ToString()).ToArray<String>()));

                lines.Add("SegmentEnd");
            }
            lines.Add("");
            //write all groups
            foreach (SegmentGroup g in groups)
            {
                lines.Add("GroupBegin");
                foreach (NamedFeature f in g.features)
                {
                    lines.Add(f.name + " " + String.Join(" ", f.values.Select<Double, String>((d) => d.ToString()).ToArray<String>()));
                }
                lines.Add("ObservedColor " + g.observed.R / 255.0 + " " + g.observed.G / 255.0 + " " + g.observed.B / 255.0);
                lines.Add("Members " + String.Join(" ", g.members.Select<int,String>(d=>d.ToString()).ToArray<String>()));
                lines.Add("GroupEnd");
            }

            File.WriteAllLines(filename, lines.ToArray<String>());
        }

    }


}
