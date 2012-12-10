using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Engine
{
    class Filter
    {
        double[,] elements;
        public Filter(int width, int height)
        {
            elements = new double[width, height];
        }

        public Filter(double[,] data)
        {
            int width = data.GetLength(0);
            int height = data.GetLength(1);
            elements = new double[width, height];

            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    elements[i, j] = data[i, j];
                }
            }
        }

    }
}
