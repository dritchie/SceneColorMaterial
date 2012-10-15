# Engine

A collection of classes in C# for image processing
Working files at this time include:
* Color - Conversions from RGB to LAB
* Clustering - KMeans and CMeans clustering. There is Agglomerative clustering too, but it is slow
* ColorNames - Color Name Cosine Distance calculation (ported from [c3](https://github.com/StanfordHCI/c3))
* HungarianAlgorithm - Solve assignment problems with the Hungarian Algorithm

* PaletteExtractor - Feature calculation and Palette Extraction as detailed in the "Modeling How People Extract Color Themes from Images" submission

For the saliency maps and segmentation maps:
* [Judd Saliency code](http://people.csail.mit.edu/tjudd/WherePeopleLook/index.html) 
(It has a few dependencies, documented in its README. Some of the C code in the linked dependencies needs a bit of tweaking to work on Windows, but it shouldn't be too bad)
* [Segmentation maps](http://www.cs.brown.edu/~pff/segment/)


