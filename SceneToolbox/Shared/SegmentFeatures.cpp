#include "SegmentMesh.h"
#include "SegmentFeatures.h"
#include "Eigen/Core"

using namespace std;
using namespace Eigen;


void RelativeSizeFeature::Extract(Segment& segment, vector<float>& featureVec)
{
	UINT numMaskPixels = 0;
	for (int y = 0; y < segment.mask.height; y++) for (int x = 0; x < segment.mask.width; x++)
	{
		numMaskPixels += (*segment.mask(x,y) == 1.0f);
	}
	float totalPixels = (float)segment.srcImg.width*segment.srcImg.height;
	featureVec.clear();
	featureVec.push_back(numMaskPixels/totalPixels);
}

void RelativePositionFeature::Extract(Segment& segment, vector<float>& featureVec)
{
	featureVec.clear();
	featureVec.push_back(segment.Center().x() / segment.srcImg.width);
	featureVec.push_back(segment.Center().y() / segment.srcImg.height);
}

void RelativeCenterDistFeature::Extract(Segment& segment, vector<float>& featureVec)
{
	featureVec.clear();

	// Absolute x distance from center
	float imgCenterX = segment.srcImg.width / 2.0f;
	featureVec.push_back(fabs(segment.Center().x() - imgCenterX) / imgCenterX);

	// Absolute y distance from center
	float imgCenterY = segment.srcImg.height / 2.0f;
	featureVec.push_back(fabs(segment.Center().y() - imgCenterY) / imgCenterY);

	// Euclidean distance from center
	Vector2f center(imgCenterX, imgCenterY);
	featureVec.push_back((center - segment.Center()).norm());
}
