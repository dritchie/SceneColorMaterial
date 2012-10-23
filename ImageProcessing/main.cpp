#include "SegmentMesh.h"
#include "SegmentFeatures.h"

using namespace ImageStack;
using namespace std;

void TestSegmentMesh(int argc, char** argv)
{
	if (argc != 3)
	{
		printf("need two arguments: imgfilename segmapfilename");
		exit(1);
	}

	Image img = Load::apply(argv[1]);
	Image segmap = Load::apply(argv[2]);

	SegmentMesh segmesh(img, segmap);
	//segmesh.DebugSaveRawSegments("output");

	vector<SegmentFeature*> features;
	features.push_back(new RelativeSizeSegmentFeature);
	features.push_back(new RelativeXSegmentFeature);
	features.push_back(new RelativeYSegmentFeature);
	features.push_back(new RelativeHorizDistFromCenterSegmentFeature);
	features.push_back(new RelativeVertDistFromCenterSegmentFeature);
	segmesh.ExtractSegmentFeatures(features, "segfeatures.txt");
	for (UINT i = 0; i < features.size(); i++)
		delete features[i];
}

int main(int argc, char** argv)
{
	TestSegmentMesh(argc, argv);
	return 0;
}