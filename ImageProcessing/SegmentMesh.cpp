#include "SegmentMesh.h"
#include "SegmentFeatures.h"
#include "BBox2D.h"
#include <map>
#include <fstream>

using namespace std;
using namespace ImageStack;


SegmentMesh::SegmentMesh(Image img, Image segmap)
	: image(img)
{
	assert(img.width == segmap.width && img.height == segmap.height,
		"SegmentMesh::SegmentMesh - img and segmap have different dimensions.\n");

	struct SegMapColor
	{
		float* rgb;
		SegMapColor(float* data) : rgb(data) {}
		bool operator < (const SegMapColor& c) const
		{
			return rgb[0] < c.rgb[0] ||
				   rgb[0] == c.rgb[0] && rgb[1] < c.rgb[1] ||
				   rgb[0] == c.rgb[0] && rgb[1] == c.rgb[1] && rgb[2] < c.rgb[2];
		}
	};

	// Map colors to segment ids while generating a segmentation mask
	map<SegMapColor, UINT> color2id;
	UINT nextId = 0;
	UINT* segmask = new UINT[segmap.width*segmap.height];
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
	{
		SegMapColor c(segmap(x,y));
		auto it = color2id.find(c);
		if (it == color2id.end())
		{
			color2id[c] = nextId;
			segmask[y*segmap.width + x] = nextId;
			nextId++;
		}
		else
		{
			segmask[y*segmap.width + x] = it->second;
		}
	}

	UINT numsegs = color2id.size();

	// Figure out the bboxes for each segment
	vector< BBox2D<UINT> > segbounds(numsegs);
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
	{
		UINT id = segmask[y*segmap.width + x];
		segbounds[id].Expand(x,y);
	}

	// Construct the actual segments
	segments.resize(numsegs);
	for (UINT i = 0; i < numsegs; i++)
	{
		Segment& seg = segments[i];
		const BBox2D<UINT>& bbox = segbounds[i];
		seg.origX = bbox.mins[0];
		seg.origY = bbox.mins[1];
		seg.crop = Window(img, bbox.mins[0], bbox.mins[1], 0, bbox.maxs[0] - bbox.mins[0] + 1, bbox.maxs[1] - bbox.mins[1] + 1, 1);
		seg.mask = Image(seg.crop.width, seg.crop.height, 1, 1);
		for (int y = 0; y < seg.mask.height; y++) for (int x = 0; x < seg.mask.width; x++)
		{
			int xx = x + bbox.mins[0];
			int yy = y + bbox.mins[1];
			if (segmask[yy*segmap.width + xx] == i)
				*seg.mask(x,y) = 1.0f;
		}
		seg.srcImg = image;
	}

	// Figure out the adjacencies by looking at neighbors in the segmask
	adjacencies.resize(numsegs);
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
	{
		UINT id = segmask[y*segmap.width + x];
		unordered_set<UINT>& neighbors = adjacencies[id];
		if (x-1 >= 0 && segmask[y*segmap.width + (x-1)] != id)
			neighbors.insert(segmask[y*segmap.width + (x-1)]);
		if (x+1 < segmap.width && segmask[y*segmap.width + (x+1)] != id)
			neighbors.insert(segmask[y*segmap.width + (x+1)]);
		if (y-1 >= 0 && segmask[(y-1)*segmap.width + x] != id)
			neighbors.insert(segmask[(y-1)*segmap.width + x]);
		if (y+1 < segmap.height && segmask[(y+1)*segmap.width + x] != id)
			neighbors.insert(segmask[(y+1)*segmap.width + x]);
	}

	// Cleanup
	delete[] segmask;
}


void SegmentMesh::DebugSaveRawSegments(const std::string& outputdir)
{
	char fname[1024];
	for (UINT i = 0; i < segments.size(); i++)
	{
		const Segment& seg = segments[i];
		sprintf_s(fname, "%s/%u_crop.png", outputdir.c_str(), i);
		Save::apply(seg.crop, fname);
		sprintf_s(fname, "%s/%u_mask.png", outputdir.c_str(), i);
		Save::apply(seg.mask, fname);
	}
}


void SegmentMesh::ExtractSegmentFeatures(const std::vector<SegmentFeature*>& features, const std::string outputFilename)
{
	ofstream outfile(outputFilename.c_str());
	assert(outfile.is_open(), "SegmentMesh::ExtractSegmentFeatures - could not open file '%s'\n", outputFilename.c_str());

	vector<float> featureVec;
	for (UINT i = 0; i < segments.size(); i++)
	{
		Segment& segment = segments[i];
		outfile << "SegmentBegin" << endl;
		for (UINT j = 0; j < features.size(); j++)
		{
			featureVec.clear();
			features[j]->Extract(segment, featureVec);
			outfile << features[j]->Name() << " ";
			for (UINT k = 0; k < featureVec.size(); k++)
			{
				outfile << featureVec[k] << " ";
			}
			outfile << endl;
		}
		outfile << "SegmentEnd" << endl;
	}
	outfile.close();
}