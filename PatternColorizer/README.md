# PatternColorizer

Dependencies
* Engine
* EmguCV [http://www.emgu.com/wiki/index.php/Emgu_CV](http://www.emgu.com/wiki/index.php/Emgu_CV)
 * Download the 64bit, version 2.4.2.
 * Copy `Emgu.CV.dll`, `Emgu.CV.UI.dll`, and `Emgu.Util.dll` from the bin directory to the PatternColorizer/Emgu directory.
 * Copy all dlls in the bin/x64 to the PatternColorizer/Emgu/x64 directory.
 * NOTE: The copy of `opencv_core242.dll` that ships with Emgu is built against the Nvidia CUDA DLL. If you don't have an Nvidia graphics card, it won't work. Instead, you can grab the official OpenCV 2.4.2 release and copy the 64-bit version of `opencv_core242.dll` from there (in build\x64\vc10\bin). Note that this depends upon `tbb.dll` (in build\common\tbb\intel64\vc10); you should put this somewhere in your DLL search path (C:\Windows\System32 is the go-to default choice).