#pragma once

#ifndef __SEGMENT_FEATURES_H
#define __SEGMENT_FEATURES_H

#include "Common/Common.h"

// Forward declare the image segment struct
struct Segment;

// Abstract base class for segment features
class SegmentFeature
{
public:
	virtual void Extract(Segment& segment, std::vector<float>& featureVec) = 0;
	virtual std::string Name() = 0;
};

class RelativeSizeFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativeSize"; }
};

class RelativePositionFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativePosition"; }
};

class RelativeCenterDistFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativeCenterDist"; }
};

#endif

