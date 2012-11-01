using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Engine;
using System.Drawing;
using System.IO;

namespace PatternColorizer
{
    class SegmentFeature
    {
        public String name;
        public List<Double> values;

        public SegmentFeature(String n)
        {
            name = n;
            values = new List<Double>();
        }
    }

    class Segment
    {
        public SortedSet<int> adjacencies;
        public List<SegmentFeature> features;
        public List<Point> points;
        public int groupId;

        public Segment()
        {
            adjacencies = new SortedSet<int>();
            features = new List<SegmentFeature>();
            points = new List<Point>();
        }

    }

    class SegmentGroup
    {
        public SortedSet<int> members;
        public Color observed;

        public SegmentGroup(Color c)
        {
            observed = c;
            members = new SortedSet<int>();
        }
    }

    class SegmentMesh
    {
        private List<Segment> segments;
        private List<SegmentGroup> groups;
        int imageWidth = 0;
        int imageHeight = 0;

        public SegmentMesh(ColorTemplate template)
        {
            int numGroups = template.NumSlots();
            imageWidth = template.Width();
            imageHeight = template.Height();

            groups = new List<SegmentGroup>();
            segments = new List<Segment>();

            //now populate the segments and segment groups
            for (int i = 0; i < numGroups; i++)
            {
                groups.Add(new SegmentGroup(template.OriginalSlotColor(i)));
            }

            UnionFind<Color> uf = new UnionFind<Color>((a, b) => (a.GetHashCode() == b.GetHashCode()));
            Bitmap image = template.DebugQuantization();
            int[,] assignments = uf.ConnectedComponents(Util.BitmapToArray(image));
            

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
                        idToSegment.Add(id, new Segment());

                    idToSegment[id].points.Add(new Point(i, j));
                    idToSegment[id].groupId = template.GetSlotId(i, j);

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

            //finalize segment list
            foreach (int id in idToSegment.Keys)
            {
                segments.Add(idToSegment[id]);
            }

            //finalize groups
            for (int i = 0; i < segments.Count(); i++)
            {
                int groupId = segments[i].groupId;
                groups[groupId].members.Add(i);
            }

            //finalize segment list
            foreach (Segment s in segments)
            {
                ComputeFeatures(s);
            }

        }

        public void ComputeFeatures(Segment s)
        {
            //Add the relative size
            SegmentFeature f = new SegmentFeature("RelativeSize");
            f.values.Add(s.points.Count() / (double)(imageWidth * imageHeight));
            s.features.Add(f);
        }


        public void WriteToFile(String filename)
        {
            List<String> lines = new List<String>();
            //dump to a file
            foreach (Segment s in segments)
            {
                lines.Add("SegmentBegin");

                foreach (SegmentFeature f in s.features)
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
                lines.Add("ObservedColor " + g.observed.R / 255.0 + " " + g.observed.G / 255.0 + " " + g.observed.B / 255.0);
                lines.Add("Members " + String.Join(" ", g.members.Select<int,String>(d=>d.ToString()).ToArray<String>()));
                lines.Add("GroupEnd");
            }

            File.WriteAllLines(filename, lines.ToArray<String>());
        }

    }


}
