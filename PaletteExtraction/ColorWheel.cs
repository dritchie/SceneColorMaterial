using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

using Engine;

namespace ColorVis
{
    class ColorWheel
    {
        int Radius;
        int Thickness;

        Bitmap Wheel;
        Bitmap Disk;

        public ColorWheel(int r, int thickness)
        {
            Radius = r;
            Thickness = thickness;

            Wheel = new Bitmap(Radius * 2, Radius * 2);
            Point center = new Point(Radius, Radius);

            //render the wheel
            for (int i = 0; i < Wheel.Width; i++)
            {
                for (int j = 0; j < Wheel.Height; j++)
                {
                    //calculate distance to the center
                    double dx = center.X - i;
                    double dy = center.Y - j;
                    double dist = Math.Sqrt(Math.Pow(dx, 2) + Math.Pow(dy, 2));
                    if (dist >= Radius - Thickness && dist <= Radius)
                    {
                        //calculate angle
                        double rad = Math.Atan2(dy, dx) + Math.PI; //atan2 output between -pi and pi
                        double degrees = rad * 180.0 / Math.PI;

                        Color rgb = Util.HSVtoRGB(new HSV(degrees, 1, 0.8));
                        Wheel.SetPixel(i, j, rgb);
                    }
                }
            }

            //render the disk
            Disk = new Bitmap(Radius*2, Radius*2);
            for (int i = 0; i < Disk.Width; i++)
            {
                for (int j = 0; j < Disk.Height; j++)
                {
                    //calculate distance to the center
                    double dx = center.X - i;
                    double dy = center.Y - j;
                    double dist = Math.Sqrt(Math.Pow(dx, 2) + Math.Pow(dy, 2));
                    if (dist <= Radius)
                    {
                        //calculate angle
                        double rad = Math.Atan2(dy, dx) + Math.PI; //atan2 output between -pi and pi
                        double degrees = rad * 180.0 / Math.PI;

                        double saturation = dist / Radius;

                        Color rgb = Util.HSVtoRGB(new HSV(degrees, saturation, 0.8));
                        Disk.SetPixel(i, j, rgb);
                    }
                }
            }


        }

        public Bitmap RenderHueHistogram(List<HSV> colors, int binSize=5)
        {
            Bitmap result = new Bitmap(Radius * 2, Radius * 2);

            //render the wheel
            Graphics graphics = Graphics.FromImage(result);
            graphics.DrawImage(Wheel, 0, 0);

            Point center = new Point(Radius, Radius);

            //compute the hue histogram
            double[] hist = new double[360 / binSize];
            double max = 0;
            foreach (HSV color in colors)
            {
                int idx = (int)Math.Min(Math.Floor(color.H/binSize), hist.Count()-1);

                //TODO: weight counts by saturation and value?
                //for now just accept all colors
                hist[idx]++;

                max = Math.Max(hist[idx], max);

            }

            //draw the histogram
            for (int i = 0; i < hist.Count(); i++)
            {
                double angle = 360.0 * i/ (double)hist.Count();
                HSV hsv = new HSV(angle, 1, 0.8);
                Color color = Util.HSVtoRGB(hsv);
                Pen pen = new Pen(color,5);

                double rad = angle * Math.PI/180.0;
                int x = (int)Math.Round(center.X + (Radius-Thickness) * Math.Cos(rad));
                int y = (int)Math.Round(center.Y + (Radius-Thickness) * Math.Sin(rad));

                double height = (Radius-Thickness) - hist[i]/max * (Radius-Thickness);

                int ex = (int)Math.Round(center.X + height * Math.Cos(rad));
                int ey = (int)Math.Round(center.Y + height * Math.Sin(rad));

                graphics.DrawLine(pen, x, y, ex, ey);

            }


            return result;
        }

        public Bitmap RenderDisk(List<HSV> colors)
        {
            Bitmap result = new Bitmap(Radius*2, Radius*2);

            //render the disk
            Graphics graphics = Graphics.FromImage(result);
            graphics.DrawImage(Disk, 0, 0);

            Point center = new Point(Radius, Radius);
            Pen pen = new Pen(Color.Black, 2);

            //plot the points
            foreach (HSV hsv in colors)
            {
                double dist = hsv.S * Radius;
                double angle = hsv.H;

                //find dx and dy
                int dx = (int)Math.Round(dist * Math.Cos(angle * Math.PI / 180));
                int dy = (int)Math.Round(dist * Math.Sin(angle * Math.PI / 180));

                int x = center.X + dx;
                int y = center.Y + dy;

                graphics.DrawEllipse(pen, x, y, 5, 5);
                graphics.DrawLine(pen, center.X, center.Y, x, y);
            }

            return result;
        }


    }
}
