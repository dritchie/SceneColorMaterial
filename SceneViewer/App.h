#pragma once

#ifndef APP_H
#define APP_H

#include "GraphicsApp.h"
#include "Viewing/Camera.h"

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
};


#endif