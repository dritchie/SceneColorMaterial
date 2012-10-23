#pragma once

#ifndef __BBOX_2D_H
#define __BBOX_2D_H

#include "Common.h"
#include <algorithm>

template<typename T>
class BBox2D
{
public:

	BBox2D() : isNull(true) {}

	void Expand(const T& x, const T& y)
	{
		if (isNull)
		{
			isNull = false;
			mins[0] = maxs[0] = x;
			mins[1] = maxs[1] = y;
		}
		else
		{
			mins[0] = std::min(mins[0], x);
			maxs[0] = std::max(maxs[0], x);
			mins[1] = std::min(mins[1], y);
			maxs[1] = std::max(maxs[1], y);
		}
	}

	T mins[2];
	T maxs[2];

private:
	bool isNull;
};

#endif

