#pragma once

#ifndef __SCENE_H
#define __SCENE_H

#include "WSSScene.h"
#include "Model.h"
#include "ColorGroup.h"

class Scene
{
public:
	~Scene();
	void LoadFromWSS(WSSScene* wss);
	void Render(const RenderOptions& opts);
	void Pick(const RenderOptions& opts);

	std::vector<Model*> models;
	std::map<std::string, ColorGroup*> colorGroups;

private:
	void FreeMemory();
};

#endif

