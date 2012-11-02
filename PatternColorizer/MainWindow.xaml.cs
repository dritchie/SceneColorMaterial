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
        String palettefile;
        String imagedir;
        String outdir;
        String json;
        String weightsDir;

        Dictionary<String, PaletteData> palettes;

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
                    case "palettefile":
                        palettefile = fields.Last().Trim();
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

            //read the palettes
            palettes = LoadFilePalettes(palettefile);
        }


        //Load PaletteData
        private Dictionary<String, PaletteData> LoadFilePalettes(String file)
        {
            FileInfo finfo = new FileInfo(file);

            Dictionary<String, PaletteData> plist = new Dictionary<String, PaletteData>();
            String[] lines = File.ReadAllLines(file);

            //extracted palettes file format
            if (finfo.Extension == ".tsv")
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
            else //pattern template file format
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
                        Color c = ColorTranslator.FromHtml("#"+s);
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

        //Test the quantization, connected components on different color layers, and output the descriptor
        private void OutputPatterns_Click(object sender, RoutedEventArgs e)
        {
            //Create output directories
            Directory.CreateDirectory(outdir + "\\cc\\");
            Directory.CreateDirectory(outdir + "\\quantized\\");
            Directory.CreateDirectory(outdir + "\\mesh\\");

            //read in the patterns and save out their layers
            String[] files = System.IO.Directory.GetFiles(System.IO.Path.Combine(imagedir));
             
            foreach (String f in files)
            {
              
                Bitmap image= new Bitmap(f);
                String basename = new FileInfo(f).Name;
                PaletteData palette = palettes[basename];

                ColorTemplate template = new ColorTemplate(image, palette);

                //output the template descriptor
                String filename = Path.Combine(outdir, "mesh", Util.ConvertFileName(basename, "", ".txt"));
                SegmentMesh mesh = new SegmentMesh(template);
                mesh.WriteToFile(filename);


                Bitmap result = template.DebugQuantization();
                result.Save(Path.Combine(outdir, "quantized", Util.ConvertFileName(basename,"_quantized",".png")));
                image.Save(Path.Combine(outdir, "quantized", Util.ConvertFileName(basename, "_original", ".png")));


                //save the connected components
                UnionFind<Color> uf = new UnionFind<Color>((a, b) => (a.GetHashCode() == b.GetHashCode()));
                Color[,] resultArray = Util.BitmapToArray(result);
                int[,] cc = uf.ConnectedComponentsNoiseRemoval(resultArray);

                int numColors = palette.colors.Count();
                for (int i = 0; i < numColors; i++)
                {
                    Bitmap debug = uf.RenderComponents(cc, resultArray, palette.colors[i]);
                    debug.Save(Path.Combine(outdir, "cc", Util.ConvertFileName(basename, "_" + i)));
                    debug.Dispose();
                }
                result.Dispose();

            }

        }

        private void Recolor()
        {
            Directory.CreateDirectory(outdir + "\\recolored\\");

            //read in the patterns and save out their layers
            String[] files = System.IO.Directory.GetFiles(System.IO.Path.Combine(imagedir));

            foreach (String f in files)
            {

                Bitmap image = new Bitmap(f);
                String basename = new FileInfo(f).Name;
                PaletteData palette = palettes[basename];

                //TODO: save and reload color templates functionality
                ColorTemplate template = new ColorTemplate(image, palette);

                //Read the recoloring description if available
                String specs = Path.Combine(outdir, "specs", Util.ConvertFileName(basename,"",".txt"));
                PaletteData data = new PaletteData();
               
                if (File.Exists(specs))
                {
                    String[] lines = File.ReadAllLines(specs);
                    int[] slotToColor = new int[template.NumSlots()];
                    for (int i=0; i<slotToColor.Count(); i++)
                        slotToColor[i] = i;
                    foreach (String line in lines)
                    {
                        //rgb floats
                        int[] fields = line.Split(new string[]{" "},StringSplitOptions.RemoveEmptyEntries).Select<String, int>(s=>((int)(Math.Round(double.Parse(s)*255)))).ToArray<int>();
                        Color color = Color.FromArgb(fields[0], fields[1], fields[2]);
                        data.colors.Add(color);
                        data.lab.Add(Util.RGBtoLAB(color));
                    }

                    Bitmap orig = template.DebugQuantization();
                    orig.Save(Path.Combine(outdir, "recolored", Util.ConvertFileName(basename, "_original",".png"))); 
                    orig.Dispose();

                    Bitmap result = template.SolidColor(data, slotToColor);
                    result.Save(Path.Combine(outdir, "recolored", Util.ConvertFileName(basename, "_recolor", ".png")));
                    result.Dispose();
                }


            }
        }

        private void RecolorPatterns_Click(object sender, RoutedEventArgs e)
        {
            Recolor();
        }

    }
}
