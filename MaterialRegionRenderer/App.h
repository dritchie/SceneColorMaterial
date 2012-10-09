#pragma once

#ifndef APP_H
#define APP_H

#include "GraphicsApp.h"
#include "Viewing/Camera.h"

namespace GraphicsEngine
{
	class Shader;
	class ShaderProgram;
}
namespace ImageStack
{
	class Image;
}
class UTF8Model;

/**
Main application logic goes here
**/
class App : public GraphicsApp
{
public:

	App();
	~App();

	void Init(const std::string& paramfile);
	void InitGraphics(GraphicsEngine::GraphicsContext* ctx);

	void Render();

	void KeyDown(int key, const GraphicsEngine::ModifierKeys& mods);
	void KeyUp(int key, const GraphicsEngine::ModifierKeys& mods);

	void MouseDown(int button, int x, int y, const GraphicsEngine::ModifierKeys& mods);
	void MouseUp(int button, int x, int y, const GraphicsEngine::ModifierKeys& mods);
	void MouseMove(int x, int y, const GraphicsEngine::ModifierKeys& mods);
	void MouseDrag(int button, int x, int y, const GraphicsEngine::ModifierKeys& mods);
	void MouseWheel(int delta, const GraphicsEngine::ModifierKeys& mods);

	void Timestep(float dt);

private:

	void InitCamera();
	GraphicsEngine::InteractiveCamera* camera;

	void RenderNormal();
	void RenderCapture();


	struct Viewpoint
	{
		GraphicsEngine::Camera camera;
		float fovy;
		float znear;
		float zfar;
	};

	void ViewpointPriors(UTF8Model* model, const Viewpoint& viewpoint, std::vector<ImageStack::Image>& priors);
	float SegmentImage_FromSingleViewpoint(ImageStack::Image& img, UTF8Model* model, const Viewpoint& viewpoint, short* mapAssignment);	// Returns MAP energy
	float SegmentImage_FindOptimalViewpoint(ImageStack::Image& img, UTF8Model* model, short* mapAssignment);	// Returns MAP energy


	UTF8Model* model;
	GraphicsEngine::Shader* vs;
	GraphicsEngine::Shader* fs;
	GraphicsEngine::ShaderProgram* prog;
	bool capturing;
};


#endif