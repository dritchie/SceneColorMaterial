#pragma once

#ifndef __SEGMENT_MESH_H
#define __SEGMENT_MESH_H

#include "ImageStack.h"
#include "Common/Common.h"
#include "Eigen/Core"
#include <unordered_set>

struct Segment
{
	Eigen::Vector2i origin;
	ImageStack::Window crop;
	ImageStack::Image mask;
	ImageStack::Image srcImg;

	Eigen::Vector2f Center()
	{
		return Eigen::Vector2f(origin.x() + mask.width/2.0f, origin.y() + mask.height/2.0f);
	}
};

class SegmentMesh
{
public:
	SegmentMesh(ImageStack::Image img, ImageStack::Image segmap, bool splitGroups = false);

	void DebugSaveRawSegments(const std::string& outputdir);

private:
	ImageStack::Image image;
	std::vector<Segment> segments;
	std::vector< vector<UINT> > groups;
	std::vector< std::unordered_set<UINT> > adjacencies;
};


#endif

