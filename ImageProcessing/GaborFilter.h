#pragma once

#ifndef __GABOR_FILTER_H
#define __GABOR_FILTER_H

#include "Common.h"
#include "ImageStack.h"

class GaborFilter
{
public:
	static ImageStack::Image apply(ImageStack::Window image, float orientation, float scale); 
};

#endif

