#pragma once

#ifndef __COLOR_GROUP_H
#define __COLOR_GROUP_H

#include "Model.h"
#include <set>

struct ColorGroup
{
	std::string name;
	float color[3];
	std::set<ModelComponent*> members;
	bool isFixed;

	ColorGroup() : isFixed(false) {}
};

#endif