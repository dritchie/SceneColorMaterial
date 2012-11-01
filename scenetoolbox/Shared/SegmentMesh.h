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

	Eigen::Vector2f Center();
	Eigen::Vector3f AverageColor();
};

class SegmentMesh
{
public:
	SegmentMesh(ImageStack::Image img, ImageStack::Image segmap, bool splitGroups = false);
	SegmentMesh(ImageStack::Image img, UINT* groupMask, bool splitGroups = false);

	void SaveMasksAndCrops(const std::string& outputdir);
	void SaveGroupAndSegmentMasks(const std::string& outputdir);
	void SaveModelDescription(const std::string& outfilename);

	ImageStack::Image Recolor(const std::vector<Eigen::Vector3f>& groupColors);
	ImageStack::Image Recolor(const std::string& colorAssignmentFilename);

private:

	void Construct(ImageStack::Image img, UINT* groupMask, bool splitGroups);
	static void RandomColorMappings(UINT numIds, std::vector<Eigen::Vector3f>& colors);

	ImageStack::Image image;
	std::vector<Segment> segments;
	std::vector< std::vector<UINT> > groups;
	std::vector< std::unordered_set<UINT> > adjacencies;
};


#endif

