#include "SegmentMesh.h"

using namespace std;
using namespace ImageStack;

int main(int argc, char** argv)
{
	if (argc != 4)
		FatalError(string("usage: SegmentationRecoloring segmapfilename colorassignmentfilename outfilename"))

	Image segmap = Load::apply(argv[1]);
	SegmentMesh segmesh(segmap, segmap, true);
	Image recoloring = segmesh.Recolor(argv[2]);
	Save::apply(recoloring, argv[3]);

	return 0;
}