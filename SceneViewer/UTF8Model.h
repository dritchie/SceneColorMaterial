#pragma once

#ifndef _UTF8LOADER_H
#define _UTF8LOADER_H

#include "Common/Common.h"
#include <map>
#include "Eigen/Core"
#include "Eigen/Geometry"
#include "Assets/Mesh/CommonMesh.h"
#include "Math/Transform.h"

struct UTF8Material
{
	Eigen::Vector3f color;
};

struct UTF8ModelComponent
{
	GraphicsEngine::CommonMesh* mesh;
	UTF8Material* material;
	int index;

	UTF8ModelComponent() : mesh(NULL), index(-1) {}
};

class UTF8Model
{
public:
	EIGEN_MAKE_ALIGNED_OPERATOR_NEW
	UTF8Model() : transform(GraphicsEngine::Transform::Identity()), index(-1) {}
	void Load(const std::string& jsonfilename, const std::string& utf8Dir, const std::string& texDir);
	Eigen::AlignedBox3f Bounds() const;
	void Render();
	void Pick();

	std::vector<UTF8ModelComponent> components;
	std::map<std::string, UTF8Material> materials;
	GraphicsEngine::Transform transform;
	int index;

private:
	void FreeMemory();
};

#endif

