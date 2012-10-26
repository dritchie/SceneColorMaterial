#include "SegmentMesh.h"
#include "Eigen/Geometry"
#include <map>
#include <fstream>
#include <queue>

using namespace std;
using namespace ImageStack;
using namespace Eigen;


SegmentMesh::SegmentMesh(Image img, Image segmap, bool splitGroups)
	: image(img)
{
	if (img.width != segmap.width || img.height != segmap.height)
		FatalError(string("SegmentMesh::SegmentMesh - img and segmap have different dimensions."))

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

	// Map colors to group ids while generating a group mask
	map<SegMapColor, UINT> color2id;
	UINT nextId = 0;
	UINT* groupMask = new UINT[segmap.width*segmap.height];
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
	{
		SegMapColor c(segmap(x,y));
		auto it = color2id.find(c);
		if (it == color2id.end())
		{
			color2id[c] = nextId;
			groupMask[y*segmap.width + x] = nextId;
			nextId++;
		}
		else
		{
			groupMask[y*segmap.width + x] = it->second;
		}
	}

	UINT numgroups = color2id.size();
	groups.resize(numgroups);

	// Fill in initial segmask (using the groupmask)
	UINT* segMask = new UINT[segmap.width*segmap.height];
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
			segMask[y*segmap.width + x] = groupMask[y*segmap.width + x];

	UINT numsegs;

	// Split groups into segments (connected components), if directed to do so.
	if (splitGroups)
	{
		// Flood fill
		UINT nextSegId = 0;
		bool* seenMask = new bool[segmap.width*segmap.height];
		queue<Vector2i> fringe;
		for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
		{
			if (!seenMask[y*segmap.width + x])
			{
				UINT groupId = groupMask[y*segmap.width + x];

				auto validNeighbor = [&segmap, &seenMask, &groupMask, groupId](int xn, int yn)
				{
					return xn >= 0 && yn >= 0 && xn < segmap.width && yn < segmap.height &&
						!seenMask[yn*segmap.width + xn] && (groupId == groupMask[yn*segmap.width + xn]);
				};

				fringe.push(Vector2i(x,y));
				while(!fringe.empty())
				{
					const Vector2i& pix = fringe.front();
					int xx = pix.x(); int yy = pix.y();
					segMask[yy*segmap.width + xx] = nextSegId;
					seenMask[yy*segmap.width + xx] = true;

					if (validNeighbor(xx-1, y))
						fringe.push(Vector2i(xx-1, yy));
					if (validNeighbor(xx+1, y))
						fringe.push(Vector2i(xx-1, yy));
					if (validNeighbor(xx, y-1))
						fringe.push(Vector2i(xx-1, yy));
					if (validNeighbor(xx, y+1))
						fringe.push(Vector2i(xx-1, yy));

					fringe.pop();
				}

				groups[groupId].push_back(nextSegId);
				nextSegId++;
			}
		}

		numsegs = nextSegId;
		delete[] seenMask;
	}
	// Otherwise, just assume that there's one segment per group (use the initial
	// segmask)
	else
	{
		for (UINT i = 0; i < numgroups; i++)
			groups[i].push_back(i);
		numsegs = numgroups;
	}

	// Figure out the bboxes for each segment
	vector< AlignedBox2i > segbounds(numsegs);
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
	{
		UINT id = segMask[y*segmap.width + x];
		segbounds[id].extend(Vector2i(x,y));
	}

	// Construct the actual segments
	segments.resize(numsegs);
	for (UINT i = 0; i < numsegs; i++)
	{
		Segment& seg = segments[i];
		const AlignedBox2i& bbox = segbounds[i];
		seg.origin = bbox.min();
		seg.crop = Window(img, bbox.min()[0], bbox.min()[1], 0, bbox.max()[0] - bbox.min()[0] + 1, bbox.max()[1] - bbox.min()[1] + 1, 1);
		seg.mask = Image(seg.crop.width, seg.crop.height, 1, 1);
		for (int y = 0; y < seg.mask.height; y++) for (int x = 0; x < seg.mask.width; x++)
		{
			int xx = x + bbox.min()[0];
			int yy = y + bbox.min()[1];
			if (segMask[yy*segmap.width + xx] == i)
				*seg.mask(x,y) = 1.0f;
		}
		seg.srcImg = image;
	}

	// Build segment adjacency map
	adjacencies.resize(numsegs);
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
	{
		UINT id = segMask[y*segmap.width + x];
		unordered_set<UINT>& neighbors = adjacencies[id];
		if (x-1 >= 0 && segMask[y*segmap.width + (x-1)] != id)
			neighbors.insert(segMask[y*segmap.width + (x-1)]);
		if (x+1 < segmap.width && segMask[y*segmap.width + (x+1)] != id)
			neighbors.insert(segMask[y*segmap.width + (x+1)]);
		if (y-1 >= 0 && segMask[(y-1)*segmap.width + x] != id)
			neighbors.insert(segMask[(y-1)*segmap.width + x]);
		if (y+1 < segmap.height && segMask[(y+1)*segmap.width + x] != id)
			neighbors.insert(segMask[(y+1)*segmap.width + x]);
	}

	// Cleanup
	delete[] groupMask;
	delete[] segMask;
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
