#include "SegmentMesh.h"
#include "SegmentFeatures.h"
#include "Eigen/Geometry"
#include <map>
#include <fstream>
#include <stack>

using namespace std;
using namespace ImageStack;
using namespace Eigen;



Vector2f Segment::Center()
{
	return Vector2f(origin.x() + mask.width/2.0f, origin.y() + mask.height/2.0f);
}

Vector3f Segment::AverageColor()
{
	Vector3f c(0, 0, 0);
	UINT numpixels = 0;
	for (int y = 0; y < mask.height; y++) for (int x = 0; x < mask.width; x++)
	{
		if (*mask(x,y))
		{
			numpixels++;
			c += Vector3f(crop(x,y));
		}
	}
	return c / (float)numpixels;
}




SegmentMesh::SegmentMesh(Image img, Image segmap, bool splitGroups)
	: image(img)
{
	if (img.width != segmap.width || img.height != segmap.height)
		FatalError(string("SegmentMesh::SegmentMesh - img and segmap have different dimensions."))

	auto ColorQuantize = [](float* rgb) -> UINT
	{
		UINT r = (UINT)(rgb[0]*255.0f);
		UINT g = (UINT)(rgb[1]*255.0f);
		UINT b = (UINT)(rgb[2]*255.0f);
		UINT a = 255;
		return (r << 24) | (g << 16) | (b << 8) | a;
	};

	// Map colors to group ids while generating a group mask
	map<UINT, UINT> color2id;
	UINT nextId = 0;
	UINT* groupMask = new UINT[segmap.width*segmap.height];
	for (int y = 0; y < segmap.height; y++) for (int x = 0; x < segmap.width; x++)
	{
		UINT c = ColorQuantize(segmap(x,y));
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

	// Do the rest of the construction using the groupmask
	Construct(img, groupMask, splitGroups);

	// Cleanup
	delete[] groupMask;
}


SegmentMesh::SegmentMesh(Image img, UINT* groupMask, bool splitGroups)
{
	Construct(img, groupMask, splitGroups);
}


void SegmentMesh::Construct(Image img, UINT* groupMask, bool splitGroups)
{
	int w = img.width;
	int h = img.height;
	
	// Pull the number of groups out of the groupMask
	unordered_set<UINT> groupids;
	for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
	{
		groupids.insert(groupMask[y*w + x]);
	}

	UINT numgroups = groupids.size();
	groups.resize(numgroups);

	UINT* segMask = new UINT[w*h];
	UINT numsegs;

	// Split groups into segments (connected components), if directed to do so.
	if (splitGroups)
	{
		// Flood fill
		UINT nextSegId = 0;
		bool* seenMask = new bool[w*h];
		for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
			seenMask[y*w + x] = false;
		std::stack<Vector2i> fringe;

		auto validNeighbor = [w, h, &seenMask, &groupMask](int xn, int yn, UINT groupId)
		{
			return xn >= 0 && yn >= 0 && xn < w && yn < h &&
				!seenMask[yn*w + xn] && (groupId == groupMask[yn*w + xn]);
		};

		for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
		{
			if (!seenMask[y*w + x])
			{
				UINT groupId = groupMask[y*w + x];

				fringe.push(Vector2i(x,y));
				while(!fringe.empty())
				{
					const Vector2i pix = fringe.top();
					fringe.pop();
					int xx = pix.x(); int yy = pix.y();
					segMask[yy*w + xx] = nextSegId;
					seenMask[yy*w + xx] = true;

					if (validNeighbor(xx-1, yy, groupId))
						fringe.push(Vector2i(xx-1, yy));
					if (validNeighbor(xx+1, yy, groupId))
						fringe.push(Vector2i(xx+1, yy));
					if (validNeighbor(xx, yy-1, groupId))
						fringe.push(Vector2i(xx, yy-1));
					if (validNeighbor(xx, yy+1, groupId))
						fringe.push(Vector2i(xx, yy+1));
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
		for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
			segMask[y*w + x] = groupMask[y*w + x];
		for (UINT i = 0; i < numgroups; i++)
			groups[i].push_back(i);
		numsegs = numgroups;
	}

	// Figure out the bboxes for each segment
	vector< AlignedBox2i > segbounds(numsegs);
	for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
	{
		UINT id = segMask[y*w + x];
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
			if (segMask[yy*w + xx] == i)
				*seg.mask(x,y) = 1.0f;
		}
		seg.srcImg = image;
	}

	// Build segment adjacency map
	adjacencies.resize(numsegs);
	for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
	{
		UINT id = segMask[y*w + x];
		unordered_set<UINT>& neighbors = adjacencies[id];
		if (x-1 >= 0 && segMask[y*w + (x-1)] != id)
			neighbors.insert(segMask[y*w + (x-1)]);
		if (x+1 < w && segMask[y*w + (x+1)] != id)
			neighbors.insert(segMask[y*w + (x+1)]);
		if (y-1 >= 0 && segMask[(y-1)*w + x] != id)
			neighbors.insert(segMask[(y-1)*w + x]);
		if (y+1 < h && segMask[(y+1)*w + x] != id)
			neighbors.insert(segMask[(y+1)*w + x]);
	}

	// Cleanup
	delete[] segMask;
}

void SegmentMesh::RandomColorMappings(UINT numIds, std::vector<Eigen::Vector3f>& colors)
{
	colors.resize(numIds);

	for (UINT id = 0; id < numIds; id++)
	{
		colors[id] = Vector3f(((float)rand())/RAND_MAX,
							  ((float)rand())/RAND_MAX,
							  ((float)rand())/RAND_MAX);
	}
}

void SegmentMesh::SaveMasksAndCrops(const std::string& outputdir)
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

void SegmentMesh::SaveGroupAndSegmentMasks(const std::string& outdir)
{
	vector<Vector3f> groupColors;
	vector<Vector3f> segColors;
	RandomColorMappings(groups.size(), groupColors);
	RandomColorMappings(segments.size(), segColors);

	Image groupMask(image.width, image.height, 1, 3);
	Image segMask(image.width, image.height, 1, 3);

	for (UINT g = 0; g < groups.size(); g++)
	{
		for (UINT i = 0; i < groups[g].size(); i++)
		{
			UINT segid = groups[g][i];
			Segment& seg = segments[segid];
			for (int y = 0; y < seg.mask.height; y++) for (int x = 0; x < seg.mask.width; x++)
			{
				if (*seg.mask(x,y))
				{
					float* gpixel = groupMask(seg.origin.x() + x, seg.origin.y() + y);
					gpixel[0] = groupColors[g][0];
					gpixel[1] = groupColors[g][1];
					gpixel[2] = groupColors[g][2];
					float* spixel = segMask(seg.origin.x() + x, seg.origin.y() + y);
					spixel[0] = segColors[segid][0];
					spixel[1] = segColors[segid][1];
					spixel[2] = segColors[segid][2];
				}
			}
		}
	}

	Save::apply(groupMask, outdir + "/groupMask.png");
	Save::apply(segMask, outdir + "/segMask.png");
}

template<typename CollectionType>
void __OutputIterableCollection(ostream& stream, const CollectionType& stuff)
{
	for (auto it = stuff.begin(); it != stuff.end(); it++)
		stream << *it << " ";
}

void SegmentMesh::SaveModelDescription(const std::string& outfilename)
{
	// Things to save:
	// - Segments and their features
	// - Groups (which segments) and their observed colors
	// - Segment adjacency lists

	// Define the set of features to be extracted here.
	vector<SegmentFeature*> features;
	features.push_back(new RelativeSizeFeature);
	features.push_back(new RelativePositionFeature);
	features.push_back(new RelativeCenterDistFeature);

	ofstream outfile(outfilename.c_str());
	if (!outfile.is_open())
		FatalError(string("SegmentMesh::SaveModelDescripion - Could not open file '" + outfilename +"' for writing"))

	// Output segments
	vector<float> tmpfvec;
	for (UINT i = 0; i < segments.size(); i++)
	{
		outfile << "SegmentBegin" << endl;

		// Features
		for (UINT f = 0; f < features.size(); f++)
		{
			features[f]->Extract(segments[i], tmpfvec);
			outfile << features[f]->Name() << " ";
			__OutputIterableCollection(outfile, tmpfvec);
			outfile << endl;
		}

		// Adjacencies
		outfile << "AdjacentTo ";
		__OutputIterableCollection(outfile, adjacencies[i]);
		outfile << endl;

		outfile << "SegmentEnd" << endl;
	}
	outfile << endl;

	// Output groups
	for (UINT g = 0; g < groups.size(); g++)
	{
		outfile << "GroupBegin" << endl;

		// Color
		Vector3f c(0, 0, 0);
		for (UINT i = 0; i < groups[g].size(); i++)
			c += segments[groups[g][i]].AverageColor();
		c /= (float)groups[g].size();
		outfile << "ObservedColor " << c[0] << " " << c[1] << " " << c[2] << endl;

		// Membership
		outfile << "Members ";
		__OutputIterableCollection(outfile, groups[g]);
		outfile << endl;

		outfile << "GroupEnd" << endl;
	}

	// Cleanup
	for (UINT i = 0; i < features.size(); i++)
		delete features[i];
}
