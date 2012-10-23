#pragma once

#ifndef __SEGMENT_FEATURES_H
#define __SEGMENT_FEATURES_H

#include <vector>
#include <string>

// Forward declare the image segment struct
struct Segment;

// Abstract base class for segment features
class SegmentFeature
{
public:
	virtual void Extract(Segment& segment, std::vector<float>& featureVec) = 0;
	virtual std::string Name() = 0;
};

class RelativeSizeSegmentFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativeSize"; }
};

class RelativeXSegmentFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativeX"; }
};

class RelativeYSegmentFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativeY"; }
};

class RelativeHorizDistFromCenterSegmentFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativeHorizDistFromCenter"; }
};

class RelativeVertDistFromCenterSegmentFeature : public SegmentFeature
{
public:
	void Extract(Segment& segment, std::vector<float>& featureVec);
	std::string Name() { return "RelativeVertDistFromCenter"; }
};

#endif

