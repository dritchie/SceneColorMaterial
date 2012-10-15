using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace Engine
{
    public class CIELAB : IEquatable<CIELAB>
    {
        public double L;
        public double A;
        public double B;

        public CIELAB()
        {
            L = 0;
            A = 0;
            B = 0;
        }

        public CIELAB(double l, double a, double b)
        {
            L = l;
            A = a;
            B = b;
        }

        public double SqDist(CIELAB lab)
        {
            double ldiff = this.L - lab.L;
            double adiff = this.A - lab.A;
            double bdiff = this.B - lab.B;
            return Math.Pow(ldiff, 2.0) + Math.Pow(adiff, 2.0) + Math.Pow(bdiff, 2.0);
        }

        public bool Equals(CIELAB other)
        {
            return L == other.L && A == other.A && B == other.B;
        }


        public override int GetHashCode()
        {
            //convert to RGB and see
            //CIELAB lab = new CIELAB(L, A, B);
            //Color c = Util.LABtoRGB(lab);
            int hCode = (int)Math.Round(L) ^ (int)Math.Round(A) ^ (int)Math.Round(B);
            //int hCode = c.R ^ c.G ^ c.B;
            return hCode.GetHashCode();
        }

        public static CIELAB operator +(CIELAB a, CIELAB b)
        {
            return new CIELAB(a.L + b.L, a.A + b.A, a.B + b.B);
        }

        public static CIELAB operator *(CIELAB a, CIELAB b)
        {
            return new CIELAB(a.L * b.L, a.A * b.A, a.B * b.B);
        }
        public static CIELAB operator -(CIELAB a, CIELAB b)
        {
            return new CIELAB(a.L - b.L, a.A - b.A, a.B - b.B);
        }

        public static CIELAB operator /(CIELAB a, double s)
        {
            return new CIELAB(a.L / s, a.A / s, a.B / s);
        }

        public static CIELAB operator *(CIELAB a, double s)
        {
            return new CIELAB(a.L * s, a.A * s, a.B * s);
        }

        public override string ToString()
        {
            //return base.ToString();
            return "("+ L + ", " + A + ", " + B + ")";
        }
    }

    public class Util
    {
        public static Color LABtoRGB(CIELAB lab)
        {
            double gamma = 2.2;
            double e = 216 / 24389.0;
            double k = 24389 / 27.0;

            double XR = 0.95047;
            double YR = 1.00000;
            double ZR = 1.08883;

            double fy = (lab.L + 16) / 116.0;
            double fx = lab.A / 500.0 + fy;
            double fz = fy - lab.B / 200.0;

            double[,] xyzTorgbMatrix = new double[3, 3] {{3.2404542, -1.5371385, -0.4985314},
                                                        {-0.9692660,  1.8760108,  0.0415560},
                                                        {0.0556434, -0.2040259,  1.0572252}};
            double xR = Math.Pow(fx, 3.0);
            double zR = Math.Pow(fz, 3.0);

            xR = (xR > e) ? xR : (116 * fx - 16) / k;
            double yR = (lab.L > k * e) ? Math.Pow((lab.L + 16) / 116.0, 3.0) : lab.L / k;
            zR = (zR > e) ? zR : (116 * fz - 16) / k;

            double x = xR * XR;
            double y = yR * YR;
            double z = zR * ZR;

            //xyz to rgb
            double r = xyzTorgbMatrix[0, 0] * x + xyzTorgbMatrix[0, 1] * y + xyzTorgbMatrix[0, 2] * z;
            double g = xyzTorgbMatrix[1, 0] * x + xyzTorgbMatrix[1, 1] * y + xyzTorgbMatrix[1, 2] * z;
            double b = xyzTorgbMatrix[2, 0] * x + xyzTorgbMatrix[2, 1] * y + xyzTorgbMatrix[2, 2] * z;

            int red = (int)Math.Round(255 * (Math.Pow(clamp(r), 1.0 / gamma)));
            int green = (int)Math.Round(255 * (Math.Pow(clamp(g), 1.0 / gamma)));
            int blue = (int)Math.Round(255 * (Math.Pow(clamp(b), 1.0 / gamma)));

            return Color.FromArgb(red, green, blue);
        }

        private static double clamp(double value)
        {
            return Math.Min(Math.Max(value, 0.0), 1.0);
        }


        public static CIELAB RGBtoLAB(Color rgb)
        {
            double gamma = 2.2;
            double red = Math.Pow(rgb.R / 255.0, gamma); //range from 0 to 1.0
            double green = Math.Pow(rgb.G / 255.0, gamma);
            double blue = Math.Pow(rgb.B / 255.0, gamma);


            //assume rgb is already linear
            //sRGB to xyz
            //http://www.brucelindbloom.com/
            double[,] rgbToxyzMatrix = new double[3, 3]{
                                            {0.4124564,  0.3575761,  0.1804375},
                                            {0.2126729,  0.7151522,  0.0721750},
                                            {0.0193339,  0.1191920,  0.9503041}};

            double x = rgbToxyzMatrix[0, 0] * red + rgbToxyzMatrix[0, 1] * green + rgbToxyzMatrix[0, 2] * blue;
            double y = rgbToxyzMatrix[1, 0] * red + rgbToxyzMatrix[1, 1] * green + rgbToxyzMatrix[1, 2] * blue;
            double z = rgbToxyzMatrix[2, 0] * red + rgbToxyzMatrix[2, 1] * green + rgbToxyzMatrix[2, 2] * blue;

            double XR = 0.95047;
            double YR = 1.00000;
            double ZR = 1.08883;

            double e = 216 / 24389.0;
            double k = 24389 / 27.0;

            double xR = x / XR;
            double yR = y / YR;
            double zR = z / ZR;

            double fx = (xR > e) ? Math.Pow(xR, 1.0 / 3.0) : (k * xR + 16) / 116.0;
            double fy = (yR > e) ? Math.Pow(yR, 1.0 / 3.0) : (k * yR + 16) / 116.0;
            double fz = (zR > e) ? Math.Pow(zR, 1.0 / 3.0) : (k * zR + 16) / 116.0;

            double cieL = 116 * fy - 16;
            double cieA = 500 * (fx - fy);
            double cieB = 200 * (fy - fz);

            return new CIELAB(cieL, cieA, cieB);

        }

    }
}
