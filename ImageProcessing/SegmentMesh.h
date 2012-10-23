#pragma once

#ifndef __SEGMENT_MESH_H
#define __SEGMENT_MESH_H

#include "Common.h"
#include <vector>
#include <unordered_set>
#include <utility>
#include "ImageStack.h"

struct Segment
{
	UINT origX;
	UINT origY;
	ImageStack::Window crop;
	ImageStack::Image mask;
	ImageStack::Image srcImg;

	std::pair<float, float> Center()
	{
		return std::make_pair(origX + mask.width/2.0f, origY + mask.width/2.0f);
	}
};

// Forward declaration
class SegmentFeature;

class SegmentMesh
{
public:
	SegmentMesh(ImageStack::Image img, ImageStack::Image segmap);
	void ExtractSegmentFeatures(const std::vector<SegmentFeature*>& features, const std::string outputFilename);

	void DebugSaveRawSegments(const std::string& outputdir);

private:
	ImageStack::Image image;
	std::vector<Segment> segments;
	std::vector< std::unordered_set<UINT> > adjacencies;
};

#endif

