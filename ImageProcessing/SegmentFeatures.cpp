#include "SegmentFeatures.h"
#include "SegmentMesh.h"



void RelativeSizeSegmentFeature::Extract(Segment& segment, std::vector<float>& featureVec)
{
	UINT numMaskPixels = 0;
	for (UINT y = 0; y < segment.mask.height; y++) for (UINT x = 0; x < segment.mask.width; x++)
	{
		numMaskPixels += (*segment.mask(x,y) == 1.0f);
	}
	float totalPixels = segment.srcImg.width*segment.srcImg.height;
	featureVec.clear();
	featureVec.push_back(numMaskPixels/totalPixels);
}

void RelativeXSegmentFeature::Extract(Segment& segment, std::vector<float>& featureVec)
{
	featureVec.clear();
	featureVec.push_back(segment.Center().first / segment.srcImg.width);
}

void RelativeYSegmentFeature::Extract(Segment& segment, std::vector<float>& featureVec)
{
	featureVec.clear();
	featureVec.push_back(segment.Center().second / segment.srcImg.height);
}

void RelativeHorizDistFromCenterSegmentFeature::Extract(Segment& segment, std::vector<float>& featureVec)
{
	featureVec.clear();
	float imgCenterX = segment.srcImg.width / 2.0f;
	featureVec.push_back(fabs(segment.Center().first - imgCenterX) / imgCenterX);
}

void RelativeVertDistFromCenterSegmentFeature::Extract(Segment& segment, std::vector<float>& featureVec)
{
	featureVec.clear();
	float imgCenterY = segment.srcImg.height / 2.0f;
	featureVec.push_back(fabs(segment.Center().second - imgCenterY) / imgCenterY);
}
