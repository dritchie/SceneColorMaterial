#pragma once

#ifndef APP_H
#define APP_H

#include "GraphicsApp.h"
#include "Eigen/Core"
#include "Viewing/Camera.h"
#include "Scene.h"
#include "Assets/Shader/Shader.h"
#include "Assets/Shader/ShaderProgram.h"
#include "Picker.h"
#include "FL/Fl_Widget.H"
#include "ColorPanel.h"
#include "ComponentPanel.h"
#include "RenderOptions.h"


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

	// Render passes
	void DrawingRenderPass();
	void PickingRenderPass();

	// UI callbacks
	static Fl_Callback DisplayShadedCallback;
	static Fl_Callback DisplayFlatCallback;
	static Fl_Callback DisplayFixedCallback;
	static Fl_Callback SaveFrameCallback;
	static Fl_Callback GenerateSegmentationCallback;


	/** Various state (May split this out at some point) **/

	GraphicsEngine::InteractiveCamera* camera;

	Scene scene;

	GraphicsEngine::Shader *shaded_vs, *shaded_fs;
	GraphicsEngine::ShaderProgram* shaded_prog;
	GraphicsEngine::Shader *flat_vs, *flat_fs;
	GraphicsEngine::ShaderProgram* flat_prog;
	GraphicsEngine::Shader *picking_vs, *picking_fs;
	GraphicsEngine::ShaderProgram* picking_prog;

	Eigen::Vector3f lightDir;

	RenderOptions renderOptions;

	Picker picker;

	ColorPanel* colorPanel;
	ComponentPanel* compPanel;
};


#endif