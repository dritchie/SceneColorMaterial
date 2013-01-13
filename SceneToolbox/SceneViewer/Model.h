#pragma once

#ifndef __MODEL_H
#define __MODEL_H

#include "UTF8Model.h"
#include "RenderOptions.h"

class Model;
struct ColorGroup;

struct ModelComponent
{
	GraphicsEngine::CommonMesh* mesh;
	ColorGroup* colorGroup;
	int index;
	Model* owner;

	ModelComponent() : mesh(NULL), colorGroup(NULL), index(-1), owner(NULL) {}
	~ModelComponent() { delete mesh; }

	void SetColorGroup(ColorGroup* newGroup);
};

class Model
{
public:
	EIGEN_MAKE_ALIGNED_OPERATOR_NEW
	Model() : transform(GraphicsEngine::Transform::Identity()), index(-1) {}
	~Model();
	void LoadFromUTF8(UTF8Model* utf8);
	void Render(const RenderOptions& opts);
	void Pick(const RenderOptions& opts);
	void SavePBRT(const std::string& filename, const GraphicsEngine::Transform& cameraTransform);

	GraphicsEngine::Transform transform;
	std::vector<ModelComponent*> components;
	int index;

private:
	void FreeMemory();
};

#endif

