#pragma once

#ifndef _UTF8LOADER_H
#define _UTF8LOADER_H

#include "Common/Common.h"
#include <map>
#include "Eigen/Core"
#include "Eigen/Geometry"
#include "Assets/Mesh/CommonMesh.h"
#include "Assets/Texture/Texture2D.h"
#include "Math/Transform.h"

struct UTF8Material
{
	Eigen::Vector3f color;
	double transparency;
	GraphicsEngine::Texture2D* tex;

	UTF8Material() : tex(NULL) {}
};

struct UTF8ModelComponent
{
	GraphicsEngine::CommonMesh* mesh;
	UTF8Material* material;

	UTF8ModelComponent() : mesh(NULL) {}
};

class UTF8Model
{
public:
	EIGEN_MAKE_ALIGNED_OPERATOR_NEW
	UTF8Model() : transform(GraphicsEngine::Transform::Identity()) {}
	void Load(const std::string& jsonfilename, const std::string& utf8Dir, bool loadTextures = false, const std::string& texDir = "");
	Eigen::AlignedBox3f Bounds() const;

	std::vector<UTF8ModelComponent> components;
	std::map<std::string, UTF8Material> materials;
	GraphicsEngine::Transform transform;

private:
	void FreeMemory();
};

#endif

