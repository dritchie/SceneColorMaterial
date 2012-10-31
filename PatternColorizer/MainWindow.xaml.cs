using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.IO;
using System.Drawing;
using Engine;


using Color = System.Drawing.Color;
using Path = System.IO.Path;

namespace PatternColorizer
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        String config = "../../localconfig.txt";
        String palettedir;
        String imagedir;
        String outdir;
        String json;
        String weightsDir;

        public MainWindow()
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
                    case "imagedir":
                        imagedir = fields.Last().Trim();
                        break;
                    case "palettedir":
                        palettedir = fields.Last().Trim();
                        break;
                    case "outdir":
                        outdir = fields.Last().Trim();
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


        //Load PaletteData
        private Dictionary<String, PaletteData> LoadFilePalettes(String file)
        {
            FileInfo finfo = new FileInfo(file);

            Dictionary<String, PaletteData> plist = new Dictionary<String, PaletteData>();
            String[] lines = File.ReadAllLines(file);

            //extracted palettes
            if (finfo.Extension == "tsv")
            {
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
                        plist.Add(key, data);
                    else
                        throw new IOException("More than one palette per key");
                }
            }
            else //pattern template palettes
            {
                for (int i = 0; i < lines.Count(); i++)
                {
                    String line = lines[i];
                    String[] fields = line.Replace("\"", "").Split(',');

                    PaletteData data = new PaletteData();
                    data.id = Int32.Parse(fields[1]);
                    data.workerName = fields[0];
                    String[] colors = fields[3].Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries);
                    colors = colors.Distinct<String>().ToArray<String>();
                    String key = fields[1]+".png";
                    foreach (String s in colors)
                    {
                        Color c = ColorTranslator.FromHtml(s);
                        data.colors.Add(c);
                        data.lab.Add(Util.RGBtoLAB(c));
                        
                    }

                    if (!plist.ContainsKey(key))
                        plist.Add(key, data);
                    else
                        throw new IOException("More than one palette per key");

                }
            }
            return plist;
        }

        private void TemplateLayers_Click(object sender, RoutedEventArgs e)
        {
            //save the templates to layers
            Directory.CreateDirectory(outdir + "\\layers\\");
            Directory.CreateDirectory(outdir + "\\cc\\");

            //read in the patterns and save out their layers
            String[] paletteVals = { "#CCCCCC", "#999999", "#666666", "#333333", "#000000"};
            List<Color> colors = paletteVals.Select<String, Color>(s => ColorTranslator.FromHtml(s)).ToList<Color>();
            List<CIELAB> lab = colors.Select<Color, CIELAB>(c => Util.RGBtoLAB(c)).ToList<CIELAB>();
            PaletteData palette = new PaletteData();

            String[] files = System.IO.Directory.GetFiles(System.IO.Path.Combine(imagedir));
            
            //find the darkest color
            foreach (String f in files)
            {
                int numColors = 0;
                Bitmap image= new Bitmap(f);
                double darkestL = 100;

                String basename = new FileInfo(f).Name;
            
                for (int i = 0; i < image.Width; i++)
                {
                    for (int j = 0; j < image.Height; j++)
                    {
                        double L = Util.RGBtoLAB(image.GetPixel(i, j)).L;
                        darkestL = Math.Min(L, darkestL);
                    }
                }

                //find the number of colors
                for (int i = 0; i < lab.Count(); i++)
                {
                    if (lab[i].L >= darkestL)
                    {
                        numColors = i + 1;
                    }
                }


                palette.colors = colors.Take<Color>(numColors).ToList<Color>();
                palette.lab = lab.Take<CIELAB>(numColors).ToList<CIELAB>();


                ColorTemplate template = new ColorTemplate(image, palette);

                for (int i = 0; i < numColors; i++)
                {
                    Bitmap result = template.RenderSegment(i);
                    result.Save(Path.Combine(outdir, "layers",Util.ConvertFileName(basename, "_" + i)));

                    //save the connected components
                    UnionFind<Color> uf = new UnionFind<Color>((a, b) => (a.GetHashCode() == b.GetHashCode()));
                    int[,] cc = uf.ConnectedComponents(Util.BitmapToArray(result), Color.FromArgb(0,0,0,0));
                    Bitmap debug = uf.RenderComponents(cc);
                    debug.Save(Path.Combine(outdir, "cc", Util.ConvertFileName(basename, "_" + i)));

                    result.Dispose();
                    debug.Dispose();
                }



            }




        }
    }
}
