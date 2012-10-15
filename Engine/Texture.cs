using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace Engine
{
    class Texture
    {
        Bitmap source;
        Rectangle rect;

        int[,] coMatrix;

        public Texture(Bitmap image, Rectangle roi)
        {
            source = image;
            rect = roi;

            coMatrix = new int[256, 256];
        }

        private double EdgesPerArea()
        {
            return 0;
        }

        private double Entropy()
        {
            return 0;
        }

        private double Contrast()
        {
            return 0;
        }

        private double Correlation()
        {
            return 0;
        }

        //also angular 2nd moment

        public double Distance(Texture other)
        {
            return 0;
        }

    }
}
