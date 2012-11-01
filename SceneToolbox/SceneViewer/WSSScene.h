#pragma once

#ifndef __WSS_SCENE_H
#define __WSS_SCENE_H

#include "Common/Common.h"
#include "UTF8Model.h"

class WSSScene
{
public:
	~WSSScene();
	void Load(const std::string& sceneFilename, const std::string& dataDir);

	std::vector<UTF8Model*> models;

private:
	void FreeMemory();
};

#endif

