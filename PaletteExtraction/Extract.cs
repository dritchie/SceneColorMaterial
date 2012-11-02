using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;

using Engine;

namespace ColorVis
{
    public partial class ColorVis : Form
    {
        String config = "../../localconfig.txt";
        String dir;
        String json;
        String weightsDir;
 
        public ColorVis()
        {
            InitializeComponent();

            //load the config file
            String[] lines = File.ReadAllLines(config);
            foreach (String l in lines)
            {
                String[] fields = l.Split('>');
                String param = fields.First().Trim();
                switch (param)
                {
                    case "dir":
                        dir = fields.Last().Trim();
                        break;
                    case "json":
                        json = fields.Last().Trim();
                        break;
                    case "weightsDir":
                        weightsDir = fields.Last().Trim();
                        break;
                    default:
                        break;
                }
            }

        }

        private void ResaveImages(String imageDir)
        {
            String[] files = System.IO.Directory.GetFiles(imageDir, "*.*").Where(file => file.EndsWith("jpg") || file.EndsWith("gif") || file.EndsWith("png")).ToArray<String>();

            foreach (String f in files)
            {
                String basename = f.Replace(imageDir, "");
                try
                {
                    FileStream fs = new FileStream(f, FileMode.Open);
                    Image image = Image.FromStream(fs);
                    fs.Close();

                    Bitmap bitmap = new Bitmap(image);

                    bitmap.Save(f.Replace(".gif", ".png").Replace(".jpg", ".png"), System.Drawing.Imaging.ImageFormat.Png);

                    bitmap.Dispose();
                    image.Dispose();
                }
                catch (Exception e)
                {
                    Console.WriteLine("Problem: " + basename);
                    Console.WriteLine(e.ToString());
                }

            }
        }

        private void extractTheme_Click(object sender, EventArgs e)
        {
            //Use the Palette Extractor to extract the theme
            PaletteExtractor extractor = new PaletteExtractor(dir,weightsDir,json);

            String[] files = Directory.GetFiles(dir, "*.png");
            String outfile = dir+"\\palettes.tsv";
            String headers = "pid\tid\timage\tcolors\tnumColors\tlog\n";
            File.WriteAllText(outfile, "");
            File.AppendAllText(outfile, headers);
            int count = 0;
            foreach (String f in files)
            {
                count++;
                String basename = f.Replace(dir+"\\", "");

                //The saliency pattern "_Judd" is just an additional annotation after the image filename if it exists
                //i.e. if the image filename is A.png, the saliency map filename is A_Judd.png
                PaletteData data = extractor.HillClimbPalette(basename, "_Judd", true);

                //save to file
                String colorString = data.ToString();
                File.AppendAllText(outfile, count + "\t-1\t" + basename + "\t" + colorString + "\t5\t\n");
            }

        }

        private Dictionary<String, List<PaletteData>> LoadFilePalettes(String file)
        {
            //load art palettes
            String[] lines = File.ReadAllLines(file);

            Dictionary<String, List<PaletteData>> plist = new Dictionary<String, List<PaletteData>>();

            for (int i = 1; i < lines.Count(); i++)
            {
                String line = lines[i];
                String[] fields = line.Replace("\"", "").Split('\t');
                PaletteData data = new PaletteData();
                data.id = Int32.Parse(fields[0]);
                data.workerNum = Int32.Parse(fields[1]);
                String key = fields[2];
                String[] colors = fields[3].Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries);
                foreach (String s in colors)
                {
                    String[] comp = s.Split(',');
                    Color c = Color.FromArgb(Int32.Parse(comp[0]), Int32.Parse(comp[1]), Int32.Parse(comp[2]));
                    CIELAB l = Util.RGBtoLAB(c);
                    data.colors.Add(c);
                    data.lab.Add(l);
                }
                if (!plist.ContainsKey(key))
                    plist.Add(key, new List<PaletteData>());
                plist[key].Add(data);
            }
            return plist;
        }

        private void SavePaletteToImage(String dir, String key, String filename, PaletteData data)
        {
            int colorSize = 100;
            int numColors = data.colors.Count();
            int gridWidth = 10;
            int padding = 20;

            int imageSize = 500;

            Bitmap image = new Bitmap(Image.FromFile(dir + "\\" + key));

            int imageWidth = imageSize;
            int imageHeight = imageSize;

            if (image.Width > image.Height)
                imageHeight = (int)Math.Round(imageSize / (double)image.Width * image.Height);
            else
                imageWidth = (int)Math.Round(imageSize / (double)image.Height * image.Width);

            int width = Math.Max(colorSize * Math.Min(gridWidth, numColors), imageSize)+2*padding;
            int height = imageHeight + 3*padding + colorSize * (int)(Math.Ceiling(numColors / (double)gridWidth));

            Bitmap bitmap = new Bitmap(width, height);
            Graphics g = Graphics.FromImage(bitmap);


            //fill with black
            g.FillRectangle(new SolidBrush(Color.Black), 0, 0, bitmap.Width, bitmap.Height);


            //draw image
            g.DrawImage(image, padding, padding, imageWidth, imageHeight);

            //draw out the clusters
            for (int i = 0; i < numColors; i++)
            {
                int row = (int)Math.Floor(i / (double)gridWidth);
                int col = i - row * gridWidth;
                Pen pen = new Pen(data.colors[i]);
                g.FillRectangle(pen.Brush, col * colorSize+padding, imageHeight + 2*padding +  row * colorSize, colorSize - padding, colorSize - padding);

                double brightness = pen.Color.GetBrightness();
                Brush brush = new SolidBrush(Color.White);
                if (brightness > 0.5)
                    brush = new SolidBrush(Color.Black);

            }

            bitmap.Save(filename);

        }

        private void RenderThemes_Click(object sender, EventArgs e)
        {
            Directory.CreateDirectory(dir + "\\renders\\");
            
            String infile = dir + "\\palettes.tsv";
            Dictionary<String, List<PaletteData>> palettes = LoadFilePalettes(infile);
            foreach (String key in palettes.Keys)
            {
                List<PaletteData> list = palettes[key];
                foreach (PaletteData data in list)
                {
                    //assume one palette per key in this function
                    SavePaletteToImage(dir, key, dir + "\\renders\\" + key, data);
                }
            }
        }

        private void ExtractTemplate_Click(object sender, EventArgs e)
        {
            //extract templates from images in a directory
            String[] files = Directory.GetFiles(dir, "*.png");
            String infile = dir + "\\palettes.tsv";
            Dictionary<String, List<PaletteData>> palettes = LoadFilePalettes(infile);

             Directory.CreateDirectory(dir + "\\templates\\");
             Directory.CreateDirectory(dir + "\\recolor\\");

            Random random = new Random();

            foreach (String f in files)
            {
                Bitmap image = new Bitmap(Image.FromFile(f));
                Segmentation seg = new Segmentation();
                String basename = f.Replace(dir+"\\", "");
                String segFile = dir + "/segments/" + basename;
                seg.LoadFromFile(f, segFile);

                ColorTemplate template = new ColorTemplate(image, seg, palettes[basename].First());
                Bitmap render = template.Render();
                render.Save(dir + "\\templates\\" + basename);

                //testing random color assignments
                int numSeg = template.NumSlots();
                
                int[] segToColor = new int[numSeg];
                for (int t = 0; t < 3; t++)
                {
                    for (int i = 0; i < numSeg; i++)
                        segToColor[i] = random.Next(palettes[basename].First().colors.Count());

                    Bitmap recolor = template.ColorWith(palettes[basename].First(), segToColor);
                    recolor.Save(dir + "\\recolor\\" + Util.ConvertFileName(basename,"_"+t));

                    Bitmap solidcolor = template.SolidColor(palettes[basename].First(), segToColor);
                    solidcolor.Save(dir + "\\recolor\\" + Util.ConvertFileName(basename, "_" + t + "_solid"));
                }


            }



        }

        private void RenderWheels_Click(object sender, EventArgs e)
        {
            String[] files = Directory.GetFiles(dir, "*.png");
            String infile = dir + "\\palettes.tsv";
            Dictionary<String, List<PaletteData>> palettes = LoadFilePalettes(infile);

            String oinfile = dir + "\\oracle.csv";
            Dictionary<String, List<PaletteData>> oracle = LoadFilePalettes(oinfile);


            Directory.CreateDirectory(dir + "\\wheels\\");

            Random random = new Random();


            ColorWheel wheel = new ColorWheel(400, 10);

            foreach (String f in files)
            {
                Bitmap image = new Bitmap(Image.FromFile(f));
                Segmentation seg = new Segmentation();
                String basename = f.Replace(dir + "\\", "");

                List<HSV> phsv = new List<HSV>();
                List<HSV> ihsv = new List<HSV>();
                List<HSV> ohsv = new List<HSV>();

                for (int i = 0; i < image.Width; i++)
                {
                    for (int j = 0; j < image.Height; j++)
                    {
                        ihsv.Add(Util.RGBtoHSV(image.GetPixel(i,j)));
                    }
                }
                foreach (Color c in palettes[basename].First().colors)
                {
                    phsv.Add(Util.RGBtoHSV(c));
                }
                foreach (Color c in oracle[basename].First().colors)
                {
                    ohsv.Add(Util.RGBtoHSV(c));
                }

                Bitmap iw = wheel.RenderHueHistogram(ihsv);
                Bitmap pd = wheel.RenderDisk(phsv);
                Bitmap od = wheel.RenderDisk(ohsv);

                iw.Save(dir + "\\wheels\\" + Util.ConvertFileName(basename, "_hist"));
                pd.Save(dir + "\\wheels\\" + Util.ConvertFileName(basename, "_disk"));
                od.Save(dir + "\\wheels\\" + Util.ConvertFileName(basename, "_disko"));



            }


        }


    }
}
