using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace Engine
{
    public class ColorTemplate
    {
        CIELAB[,] template; //relative LAB difference (or change to RGB difference?)
        Segmentation slots;

        //original
        PaletteData originalPalette;
        int[] segToColor;

        public ColorTemplate(Bitmap image, PaletteData palette)
        {
            //create the segmentation based on the palette
            //TODO: There's a problem that sometimes a palette color won't appear in the image (perhaps due to color blending), and so the slot will have no pixels associated with it
            int width = image.Width;
            int height = image.Height;
            slots = new Segmentation(palette.colors.Count(), width, height);
            segToColor = new int[slots.numSegments];

            template = new CIELAB[width, height];

            CIELAB[,] recolored = ModeRecolor(Util.Map<Color, CIELAB>(Util.BitmapToArray(image), c => Util.RGBtoLAB(c)), palette);

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    template[i, j] = new CIELAB();
                    int bestIdx = -1;
                    CIELAB lab = recolored[i, j];
                    double bestDist = Double.PositiveInfinity;

                    for (int c = 0; c < palette.lab.Count(); c++)
                    {
                        double dist = palette.lab[c].SqDist(lab);
                        if (dist < bestDist)
                        {
                            bestDist = dist;
                            bestIdx = c;

                            if (dist < 0.00001)
                                break;

                        }
                    }

                    slots.assignments[i, j] = bestIdx;
                    slots.counts[bestIdx]++;
                    segToColor[bestIdx] = bestIdx;
                }
            }

            originalPalette = new PaletteData(palette);
        }


        public ColorTemplate(Bitmap image, Segmentation seg, PaletteData palette)
        {
            slots = seg; //TODO: need to copy the segmentation?
            originalPalette = new PaletteData(palette);

            int width = image.Width;
            int height = image.Height;

            template = new CIELAB[width, height];

            //initialize the template
            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    template[i, j] = Util.RGBtoLAB(image.GetPixel(i, j));
                }
            }

            segToColor = new int[seg.numSegments];

            //map each segment to the closest palette color, based on its mean color
            //TODO: if two adjacent segments have the same mean color, we could merge them...
            for (int i = 0; i < seg.numSegments; i++)
            {
                CIELAB lab = seg.segToMeanColor[i];
                int bestColor = -1;
                double bestDist = Double.PositiveInfinity;
                for (int j = 0; j < palette.lab.Count(); j++)
                {
                    double dist = palette.lab[j].SqDist(lab);

                    if (dist < bestDist)
                    {
                        bestDist = dist;
                        bestColor = j;
                    }
                }
                segToColor[i] = bestColor;
            }

            //subtract the mean color from the template
            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    int coloridx = segToColor[slots.assignments[i, j]];
                    template[i, j] -= palette.lab[coloridx];
                    //template[i, j] /= palette.lab[coloridx];
                }
            }
        }

       /* private bool inBounds(int x, int y, int width, int height)
        {
            return x >= 0 && x < width && y >= 0 && y < height;
        }*/


        public int PixelsInSlot(int idx)
        {
            return slots.counts[idx];
        }

        private int ClosestColorIndex(CIELAB color, List<CIELAB> palette)
        {
            int bestc = 0;
            Double bestDist = Double.PositiveInfinity;
            for (int c = 0; c < palette.Count(); c++)
            {
                double dist = color.SqDist(palette[c]);
                if (dist < bestDist)
                {
                    bestDist = dist;
                    bestc = c;
                } 
            }
            return bestc;

        }

        private CIELAB ClosestColor(CIELAB color, List<CIELAB> palette)
        {
            int bestc = 0;
            Double bestDist = Double.PositiveInfinity;
            for (int c = 0; c < palette.Count(); c++)
            {
                double dist = color.SqDist(palette[c]);
                if (dist < bestDist)
                {
                    bestDist = dist;
                    bestc = c;
                }
            }
            return palette[bestc];

        }


        private CIELAB[,] EdgeUnmixRecolor(CIELAB[,] image, PaletteData palette)
        {
            int width = image.GetLength(0);
            int height = image.GetLength(1);
            CIELAB[,] result = new CIELAB[width, height];

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    double[] scores = new double[palette.colors.Count()];

                    //check if the color is in the palette
                    CIELAB color = image[i, j];
                    if (palette.lab.Contains(color))
                    {
                        result[i, j] = color;
                        continue;
                    }

                    //otherwise, find the best color 
                    //see if this is an edge
                    //if it is, assign the color to be one of the edge color candidates
                    HashSet<CIELAB> tbCandidates = new HashSet<CIELAB>();
                    HashSet<CIELAB> lrCandidates = new HashSet<CIELAB>();
                    List<HashSet<CIELAB>> unmixCandidates = new List<HashSet<CIELAB>> { tbCandidates, lrCandidates };

                    //check edge types
                    //horizontal and vertical
                    HashSet<CIELAB> oneSide = new HashSet<CIELAB>();
                    HashSet<CIELAB> otherSide = new HashSet<CIELAB>();
                    Point[] top = new Point[] { new Point(i - 1, j - 1), new Point(i, j - 1), new Point(i + 1, j - 1) };
                    Point[] bottom = new Point[] { new Point(i - 1, j + 1), new Point(i - 1, j), new Point(i - 1, j + 1) };
                    Point[] left = new Point[] { new Point(i - 1, j - 1), new Point(i - 1, j), new Point(i - 1, j + 1) };
                    Point[] right = new Point[] { new Point(i + 1, j - 1), new Point(i + 1, j), new Point(i + 1, j + 1) };

                    List<Point[]> oneCompare = new List<Point[]> { top, left };
                    List<Point[]> otherCompare = new List<Point[]> { bottom, right };

                    bool edge = false;
                    for (int c = 0; c < oneCompare.Count(); c++)
                    {
                        oneSide.Clear();
                        otherSide.Clear();

                        foreach (Point p in oneCompare[c])
                        {
                            if (Util.InBounds(p.X, p.Y, width, height))
                            {
                                CIELAB rc = ClosestColor(image[p.X, p.Y], palette.lab);
                                //check if in the set
                                if (oneSide.Contains(rc))
                                {
                                    //yes, we found a majority
                                    unmixCandidates[c].Add(image[p.X, p.Y]);
                                    break;
                                }
                                else
                                {
                                    oneSide.Add(rc);
                                }
                            }
                        }

                        foreach (Point p in otherCompare[c])
                        {
                            
                            if (Util.InBounds(p.X, p.Y, width, height))
                            {
                                CIELAB rc = ClosestColor(image[p.X, p.Y], palette.lab);
                                //check if in the set
                                if (otherSide.Contains(rc))
                                {
                                    //yes, we found a majority
                                    unmixCandidates[c].Add(rc);
                                    break;
                                }
                                else
                                {
                                    otherSide.Add(rc);
                                }
                            }
                        }

                        //is it an edge?
                        if (unmixCandidates[c].Count() >= 2)
                        {
                            result[i, j] = ClosestColor(image[i, j], unmixCandidates[c].ToList<CIELAB>());
                            edge = true;
                            break;
                        }
                    }


                    //TODO:
                    //45 degrees
                    
                    //45 degrees-flipped

                    if (!edge)
                    {
                        for (int dx = -1; dx <= 1; dx++)
                        {
                            for (int dy = -1; dy <= 1; dy++)
                            {
                                int x = i + dx;
                                int y = j + dy;
                                double weight = (dx == 0 && dy == 0) ? 4 : 1;
                                if (Util.InBounds(x, y, width, height))
                                {
                                    int bestc = ClosestColorIndex(image[x, y], palette.lab);
                                    scores[bestc] -= weight;
                                }
                            }
                        }

                        //pick the color with the min score
                        double minScore = Double.PositiveInfinity;
                        int bestIdx = 0;
                        for (int c = 0; c < palette.colors.Count(); c++)
                        {
                            if (scores[c] < minScore)
                            {
                                minScore = scores[c];
                                bestIdx = c;
                            }
                        }

                        result[i, j] = palette.lab[bestIdx];
                    }

                }
            }

            return result;

        }
        


        private CIELAB[,] ModeRecolor(CIELAB[,] image, PaletteData palette)
        {
            int width = image.GetLength(0);
            int height = image.GetLength(1);
            CIELAB[,] result = new CIELAB[width, height];

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    double[] scores = new double[palette.colors.Count()];

                    //check if the color is in the palette
                    CIELAB color = image[i, j];
                    if (palette.lab.Contains(color))
                    {
                        result[i, j] = color;
                        continue;
                    }

                    //otherwise, find the best color by mode
                    for (int dx = -1; dx <= 1; dx++)
                    {
                        for (int dy = -1; dy <= 1; dy++)
                        {
                            int x = i + dx;
                            int y = j + dy;
                            double weight = (dx == 0 && dy == 0) ? 4 : 1;
                            if (Util.InBounds(x, y, width, height))
                            {
                                int bestc = ClosestColorIndex(image[x, y], palette.lab);
                                scores[bestc] -= weight;
                            }
                        }
                    }

                    //pick the color with the min score
                    double minScore = Double.PositiveInfinity;
                    int bestIdx = 0;
                    for (int c = 0; c < palette.colors.Count(); c++)
                    {
                        if (scores[c] < minScore)
                        {
                            minScore = scores[c];
                            bestIdx = c;
                        }
                    }

                    result[i, j] = palette.lab[bestIdx];

                }
            }

            return result;

        }

        private CIELAB[,] AverageRecolor(CIELAB[,] image, PaletteData palette)
        {
            int width = image.GetLength(0);
            int height = image.GetLength(1);
            CIELAB[,] result = new CIELAB[width, height];

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    double[] scores = new double[palette.colors.Count()];

                    //check if the color is in the palette
                    CIELAB color = image[i, j];
                    if (palette.lab.Contains(color))
                    {
                        result[i, j] = color;
                        continue;
                    }

                    //otherwise, find the best color
                    for (int dx = -1; dx <= 1; dx++)
                    {
                        for (int dy = -1; dy <= 1; dy++)
                        {
                            int x = i + dx;
                            int y = j + dy;
                            double weight = (dx == 0 && dy==0)? 4:1;
                            if (Util.InBounds(x,y,width,height))
                            {
                                for (int c = 0; c < palette.colors.Count(); c++)
                                {
                                    scores[c] += weight*image[x, y].SqDist(palette.lab[c]);
                                }
                            }
                        }
                    }

                    //pick the color with the min score
                    double minScore = Double.PositiveInfinity;
                    int bestIdx = 0;
                    for (int c = 0; c < palette.colors.Count(); c++)
                    {
                        if (scores[c] < minScore)
                        {
                            minScore = scores[c];
                            bestIdx = c;
                        }
                    }

                    result[i, j] = palette.lab[bestIdx];

                }
            }

            return result;

        }


        public Color OriginalSlotColor(int idx)
        {
            return originalPalette.colors[segToColor[idx]];
        }

        public int GetSlotId(int x, int y)
        {
            return slots.assignments[x,y];
        }

        //TODO: learn getters and setters
        public int Width()
        {
            return template.GetLength(0);
        }

        public int Height()
        {
            return template.GetLength(1);
        }

        public int NumSlots()
        {
            return slots.numSegments;
        }

        public Bitmap RenderSlot(int idx)
        {
            int width = Width();
            int height = Height();
            Bitmap result = new Bitmap(width, height);

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    if (slots.assignments[i, j] == idx)
                        result.SetPixel(i, j, Color.Black);
                }
            } 
            return result;
        }


        public Bitmap ColorWith(PaletteData palette, int[] segToColor)
        {
            int width = Width();
            int height = Height();
            Bitmap result = new Bitmap(width, height);

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    CIELAB lab = template[i, j] + palette.lab[segToColor[slots.assignments[i, j]]];
                    //CIELAB lab = template[i, j] * palette.lab[segToColor[segments.assignments[i, j]]];
                    result.SetPixel(i, j, Util.LABtoRGB(lab));
                }
            }

            return result;
        }

        public Bitmap SolidColor(PaletteData palette, int[] slotToColor)
        {
            int width = Width();
            int height = Height();
            Bitmap result = new Bitmap(width, height);

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    CIELAB lab = palette.lab[slotToColor[slots.assignments[i, j]]];
                    result.SetPixel(i, j, Util.LABtoRGB(lab));
                }
            }

            return result;
        }

        public Bitmap DebugQuantization()
        {
            Bitmap result = SolidColor(originalPalette, segToColor);
            return result;
        }

        public Bitmap Render()
        {
            //render the template (assuming possible values are between -200 and 200)
            //must clamp to be valid RGB values
            int width = template.GetLength(0);
            int height = template.GetLength(1);
            Bitmap result = new Bitmap(width, height);

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    CIELAB lab = template[i, j];

                    //add gray to the lab value
                    CIELAB gray = new CIELAB(53.5850, 0, 0);

                    //convert (first clamp to reasonable LAB values?)
                    Color rgb = Util.LABtoRGB(lab + gray);
                    //Color rgb = Util.LABtoRGB(lab * gray);

                    result.SetPixel(i, j, rgb);
                }
            }

            return result;
        }

    }
}
