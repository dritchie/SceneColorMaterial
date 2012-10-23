#pragma once

#ifndef __SCENE_H
#define __SCENE_H

#include "Common/Common.h"
#include "UTF8Model.h"
#include "Eigen/Core"

// This is additionally required to get stl vectors to properly
// hold fixed-width vectorizable Eigen types
// (see http://eigen.tuxfamily.org/dox/TopicStlContainers.html)
#include "Eigen/StdVector"

class WSSScene
{
public:
	void Load(const std::string& sceneFilename, const std::string& dataDir);
	void Render();

	std::vector<UTF8Model, Eigen::aligned_allocator<UTF8Model> > models;
};

#endif

