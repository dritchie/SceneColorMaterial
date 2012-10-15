#include <string>
#include <boost/filesystem.hpp>
#include "ImageStack.h"

using namespace std;
using namespace boost::filesystem;
using namespace ImageStack;


int main(int argc, char** argv)
{
	if (argc != 2)
	{
		printf("usage: AverageScene sceneImgDir");
		exit(1);
	}
	string imgdir = argv[1];
	path p(imgdir);

	// 1st pass - average dimensions
	unsigned int numimgs = 0;
	float w = 0;
	float h = 0;
	for (auto it = directory_iterator(p); it != directory_iterator(); it++)
	{
		path imgpath = *it;
		if (imgpath.string().find("_diff.jpg") == string::npos)
		{
			numimgs++;
			Image img = Load::apply(imgpath.string());
			w += img.width;
			h += img.height;
		}
	}
	int width = (int)(w/numimgs);
	int height = (int)(h/numimgs);

	// 2nd pass - average image
	Image avg(width, height, 1, 3);
	for (auto it = directory_iterator(p); it != directory_iterator(); it++)
	{
		path imgpath = *it;
		if (imgpath.string().find("_diff.jpg") == string::npos)
		{
			Image img = Load::apply(imgpath.string());
			img = Resample::apply(img, width, height);
			Add::apply(avg, img);
		}
	}
	Scale::apply(avg, 1.0f/numimgs);
	Save::apply(avg, "averageScene.jpg");

	// 3rd pass - difference images
	for (auto it = directory_iterator(p); it != directory_iterator(); it++)
	{
		path imgpath = *it;
		if (imgpath.string().find("_diff.jpg") == string::npos)
		{
			Image img = Load::apply(imgpath.string());
			img = Resample::apply(img, width, height);
			Subtract::apply(img, avg);
			//Normalize::apply(img);
			imgpath.replace_extension();
			imgpath += "_diff.jpg";
			Save::apply(img, imgpath.string());
		}
	}

	return 0;
}