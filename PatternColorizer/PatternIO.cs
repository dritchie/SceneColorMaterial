using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.IO;
using Engine;

namespace PatternColorizer
{
    class PatternItem
    {
        public String Directory;
        public String Name;
        public String FullPath;

        public PatternItem(String dir, String n, String fp)
        {
            Directory = dir;
            Name = n;
            FullPath = fp;
        }
    }

    class PatternIO
    {
        //get pattern files
        public static List<PatternItem> GetPatterns(String baseDir)
        {
            List<PatternItem> results = new List<PatternItem>();
            String[] folders = Directory.GetDirectories(baseDir, "*", System.IO.SearchOption.AllDirectories);

            foreach (String folder in folders)
            {
                String[] files = Directory.GetFiles(folder);
                DirectoryInfo dinfo = new DirectoryInfo(folder);
                foreach (String file in files)
                {
                    FileInfo finfo = new FileInfo(file);
                    results.Add(new PatternItem(dinfo.Name, finfo.Name, file));
                }
            }

            return results;
        }

        //save a png image
        public static void SavePattern(Bitmap pattern, PatternItem info, String outDir, String label = "")
        {
            String outPath = Path.Combine(outDir, info.Directory);
            Directory.CreateDirectory(outPath);
            pattern.Save(Path.Combine(outPath, Util.ConvertFileName(info.Name, label, ".png")));

        }

        //save a text file
        public static void SavePattern(String[] lines, PatternItem info, String outDir, String label = "")
        {
            String outPath = Path.Combine(outDir, info.Directory);
            Directory.CreateDirectory(outPath);
            File.WriteAllLines(Path.Combine(outPath, Util.ConvertFileName(info.Name, label, ".txt")), lines);
        }

        //save a mesh
        public static void SaveMesh(SegmentMesh mesh, PatternItem info, String outDir)
        {
            String outPath = Path.Combine(outDir, info.Directory);
            Directory.CreateDirectory(outPath);
            mesh.WriteToFile(Path.Combine(outPath, Util.ConvertFileName(info.Name, "", ".txt")));
        }

        //get a file name and create the needed directories
        public static String GetAndEnsureFilename(PatternItem info, String outDir, String extension, String label="")
        {
            Directory.CreateDirectory(outDir);
            String outPath = Path.Combine(outDir, info.Directory);
            String filename = Path.Combine(outPath, Util.ConvertFileName(info.Name, label, extension));
            return filename;
        }
    }




}
