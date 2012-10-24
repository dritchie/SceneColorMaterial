#pragma once

#ifndef __MODEL_H
#define __MODEL_H

#include "UTF8Model.h"

class Model;

struct ModelComponent
{
	GraphicsEngine::CommonMesh* mesh;
	float color[3];
	int index;
	bool isFixed;
	Model* owner;

	ModelComponent() : mesh(NULL), index(-1), isFixed(false), owner(NULL) {}
	~ModelComponent() { delete mesh; }
};

class Model
{
public:
	EIGEN_MAKE_ALIGNED_OPERATOR_NEW
	Model() : transform(GraphicsEngine::Transform::Identity()), index(-1) {}
	~Model();
	void LoadFromUTF8(UTF8Model* utf8);
	void Render();
	void Pick();

	GraphicsEngine::Transform transform;
	std::vector<ModelComponent*> components;
	int index;

private:
	void FreeMemory();
};

#endif

