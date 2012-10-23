#pragma once

#ifndef APP_H
#define APP_H

#include "GraphicsApp.h"
#include "Eigen/Core"
#include "Viewing/Camera.h"
#include "WSSScene.h"
#include "Assets/Shader/Shader.h"
#include "Assets/Shader/ShaderProgram.h"

//class Fl_Widget;
#include "FL/Fl_Widget.H"


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
	GraphicsEngine::GraphicsContext* InitAndShowUI(int argc, char** argv);

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

	// UI callbacks
	static Fl_Callback DisplayShadedCallback;
	static Fl_Callback DisplayFlatCallback;


	/** Various state (May split this out at some point) **/

	GraphicsEngine::InteractiveCamera* camera;

	WSSScene* scene;

	GraphicsEngine::Shader *shaded_vs, *shaded_fs;
	GraphicsEngine::ShaderProgram* shaded_prog;
	GraphicsEngine::Shader *flat_vs, *flat_fs;
	GraphicsEngine::ShaderProgram* flat_prog;

	Eigen::Vector3f lightDir;

	enum DisplayType
	{
		FLAT = 0,
		SHADED
	};
	DisplayType displayType;
};


#endif