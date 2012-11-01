#pragma once

#ifndef _UTF8LOADER_H
#define _UTF8LOADER_H

#include "Common/Common.h"
#include <map>
#include "Assets/Mesh/CommonMesh.h"
#include "Math/Transform.h"

struct UTF8Material
{
	std::string name;
	float color[3];
	std::string texFilename;

	UTF8Material() : texFilename("") {}
};

struct UTF8ModelComponent
{
	GraphicsEngine::CommonMesh* mesh;
	UTF8Material* material;

	UTF8ModelComponent() : mesh(NULL) {}
	~UTF8ModelComponent() { delete mesh; }
};

class UTF8Model
{
public:
	EIGEN_MAKE_ALIGNED_OPERATOR_NEW
	UTF8Model() : transform(GraphicsEngine::Transform::Identity()) {}
	~UTF8Model();
	void Load(const std::string mid, const std::string& jsonDir, const std::string& utf8Dir, const std::string& texDir);

	std::string modelID;
	std::vector<UTF8ModelComponent*> components;
	std::map<std::string, UTF8Material> materials;
	GraphicsEngine::Transform transform;

private:
	void FreeMemory();
};

#endif

