#pragma once

#ifndef __PICKER_H
#define __PICKER_H

#include "Framebuffer.h"
#include "Eigen/Core"
#include <utility>

class Picker
{
public:
	static Eigen::Vector4f PackIDs(unsigned short modelID, unsigned short compID);
	static std::pair<unsigned short, unsigned short> UnpackIDs(unsigned char *pixel);
	std::pair<int, int> Pick(int x, int y);

	Framebuffer pickBuffer;
};


#endif

