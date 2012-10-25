#include "App.h"
#include "Common/GL.h"
#include "Viewing/TinkercadCamera.h"
#include "Math/TransformStack.h"
#include "Math/Transform.h"
#include "Assets/Mesh/CommonMesh.h"
#include "FLTKGraphicsWindow.h"
#include "FL/Fl_Menu_Bar.H"
#include "FLTKUtils.h"

using namespace std;
using namespace GraphicsEngine;
using Eigen::Vector3f;
using Eigen::Vector4f;


App::App()
{
}

App::~App()
{
	SAFEDELETE(camera);
	SAFEDELETE(shaded_prog);
	SAFEDELETE(shaded_vs);
	SAFEDELETE(shaded_fs);
	SAFEDELETE(flat_prog);
	SAFEDELETE(flat_vs);
	SAFEDELETE(flat_fs);
	SAFEDELETE(picking_prog);
	SAFEDELETE(picking_vs);
	SAFEDELETE(picking_fs);
	SAFEDELETE(colorPanel);
}

/** UI Callbacks **/

void App::DisplayShadedCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;
	app->renderOptions.displayType = RenderOptions::SHADED;
	app->context->Redraw();
}

void App::DisplayFlatCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;
	app->renderOptions.displayType = RenderOptions::FLAT;
	app->context->Redraw();
}

void App::DisplayFixedCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;
	Fl_Menu_Bar* menuBar = (Fl_Menu_Bar*)w;
	app->renderOptions.highlightFixed = (bool)menuBar->find_item("&Display/Highlight Fixed")->value();
	app->context->Redraw();
}

void App::DisplayShowObjectsCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;
	Fl_Menu_Bar* menuBar = (Fl_Menu_Bar*)w;
	app->renderOptions.showObjects = (bool)menuBar->find_item("&Display/Show Objects")->value();
	app->context->Redraw();
}

void App::DisplayShowConstraintsCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;
	Fl_Menu_Bar* menuBar = (Fl_Menu_Bar*)w;
	app->renderOptions.showConstraints = (bool)menuBar->find_item("&Display/Show Constraints")->value();
	app->context->Redraw();
}

GraphicsContext* App::InitAndShowUI(int argc, char** argv)
{
	Fl_Window* window = new Fl_Window(params.IntParam("windowWidth"), params.IntParam("windowHeight"), params.StringParam("appName").c_str());

	// Menu bar
	Fl_Menu_Bar* menuBar = new Fl_Menu_Bar(0, 0, params.IntParam("windowWidth"), params.IntParam("menuBarHeight"));
	Fl_Menu_Item menuitems[] = {
		{ "&Display",              0, 0, 0, FL_SUBMENU },
			{ "Shaded",  0, DisplayShadedCallback, this, FL_MENU_RADIO | FL_MENU_VALUE },
			{ "Flat", 0, DisplayFlatCallback, this, FL_MENU_RADIO | FL_MENU_DIVIDER},
			{ "Highlight Fixed",  0, DisplayFixedCallback, this, FL_MENU_TOGGLE },
			{ "Show Objects",  0, DisplayShowObjectsCallback, this, FL_MENU_TOGGLE | FL_MENU_VALUE },
			{ "Show Constraints",  0, DisplayShowConstraintsCallback, this, FL_MENU_TOGGLE | FL_MENU_VALUE },
		{ 0 },
	{ 0 }
	};
	menuBar->copy(menuitems);

	// Graphics window
	FLTKGraphicsWindow* gwind = new FLTKGraphicsWindow(this, 0, params.IntParam("menuBarHeight"), params.IntParam("windowWidth") - params.IntParam("sidePanelWidth"),
		params.IntParam("windowHeight") - params.IntParam("menuBarHeight"), params.StringParam("appName").c_str());
	gwind->end();

	// Color group panel
	colorPanel = new ColorPanel(&scene, gwind, gwind->w(), params.IntParam("menuBarHeight"), params.IntParam("sidePanelWidth"), params.IntParam("colorPanelHeight"));
	colorPanel->end();

	// Component Panel
	compPanel = new ComponentPanel(&scene, gwind, gwind->w(), fl_below(colorPanel, 40), colorPanel->w(), 100);
	compPanel->end();

	window->end();
	window->resizable(gwind);
	window->show(argc, argv);

	return gwind;
}

void App::Init(const string& paramfile)
{
	GraphicsApp::Init(paramfile);
	renderOptions.params = &params;
	InitCamera();
}

void App::InitGraphics(GraphicsContext* ctx)
{
	GraphicsApp::InitGraphics(ctx);

	// 3D warehouse models have terrible problems with z-fighting if we
	// don't do backface culling
	glEnable(GL_CULL_FACE);

	// Shaders
	///
	shaded_vs = new Shader(Shader::VertexShader); shaded_vs->FromFile("Shaded.vert");
	shaded_fs = new Shader(Shader::FragmentShader); shaded_fs->FromFile("Shaded.frag");
	vector<Shader*> shaders; shaders.push_back(shaded_vs); shaders.push_back(shaded_fs);
	shaded_prog = new ShaderProgram(shaders);
	///
	flat_vs = new Shader(Shader::VertexShader); flat_vs->FromFile("Flat.vert");
	flat_fs = new Shader(Shader::FragmentShader); flat_fs->FromFile("Flat.frag");
	shaders.clear(); shaders.push_back(flat_vs); shaders.push_back(flat_fs);
	flat_prog = new ShaderProgram(shaders);
	///
	picking_vs = new Shader(Shader::VertexShader); picking_vs->FromFile("Picking.vert");
	picking_fs = new Shader(Shader::FragmentShader); picking_fs->FromFile("Picking.frag");
	shaders.clear(); shaders.push_back(picking_vs); shaders.push_back(picking_fs);
	picking_prog = new ShaderProgram(shaders);
	///

	// Init lighting
	lightDir = Vector3f(-0.35f, -0.45f, 1.0f);

	// Load scene
	WSSScene wss; wss.Load(params.StringParam("sceneToLoad"), params.StringParam("dataRoot"));
	scene.LoadFromWSS(&wss);

	// Now that the scene is loaded, tell the UI panels to update their lists of available color groups
	colorPanel->RefreshColorGroupList();
	compPanel->RefreshColorGroupList();
}

void App::InitCamera()
{
	TinkercadCamera* tinkerCam = new TinkercadCamera(Vector3f::Zero(), -5.0f*Vector3f::UnitY(), Vector3f::UnitZ(), Vector3f::UnitZ());
	tinkerCam->OrbitSpeed() = (float)params.FloatParam("orbitSpeed");
	tinkerCam->ScrollZoomSpeed() = (float)params.FloatParam("zoomSpeed");
	tinkerCam->DollySpeed() = (float)params.FloatParam("dollySpeed");
	camera = tinkerCam;
}

void App::Render()
{
	DrawingRenderPass();
	PickingRenderPass();
}

void App::DrawingRenderPass()
{
	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Bind correct shader given display state
	if (renderOptions.displayType == RenderOptions::SHADED)
		shaded_prog->Bind();
	if (renderOptions.displayType == RenderOptions::FLAT)
		flat_prog->Bind();

	TransformStack::Projection().Bind();
	TransformStack::Modelview().Load(camera->GetLookAtTransform());
	int lightloc = ShaderProgram::CurrentProgram()->GetUniformLocation("LightDir");
	if (lightloc >= 0)
	{
		Vector3f xformedLight = TransformStack::Modelview().Top().TransformVector(lightDir);
		glUniform3fv(lightloc, 1, &xformedLight[0]);
	}

	scene.Render(renderOptions);
}

void App::PickingRenderPass()
{
	int w = ((FLTKGraphicsWindow*)context)->w();
	int h = ((FLTKGraphicsWindow*)context)->h();
	picker.pickBuffer.BindForDrawing(w, h);

	// IMPORTANT: Clear color must be set to 0 so the 'background' of the
	// scene gets the correct picking id.
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	picking_prog->Bind();

	TransformStack::Projection().Bind();
	TransformStack::Modelview().Load(camera->GetLookAtTransform());

	scene.Pick(renderOptions);

	picker.pickBuffer.Unbind();
}

void App::KeyDown(int key, const ModifierKeys& mods)
{
	if (camera->KeyDown(key, mods))
		context->Redraw();
}

void App::KeyUp(int key, const ModifierKeys& mods)
{
	if (camera->KeyUp(key, mods))
		context->Redraw();
}

void App::MouseDown(int button, int x, int y, const GraphicsEngine::ModifierKeys& mods)
{
	if (button == MOUSE_LEFT_BUTTON)
	{
		y = ((FLTKGraphicsWindow*)context)->h() - y;
		auto ids = picker.Pick(x, y);
		if (ids.first == -1)
		{
			compPanel->SetActiveComponent(NULL);
		}
		else
		{
			compPanel->SetActiveComponent(scene.models[ids.first]->components[ids.second]);
		}
	}
	else if (camera->MouseDown(button, x, y, mods))
		context->Redraw();
}

void App::MouseUp(int button, int x, int y, const GraphicsEngine::ModifierKeys& mods)
{
	if (camera->MouseUp(button, x, y, mods))
		context->Redraw();
}

void App::MouseMove(int x, int y, const ModifierKeys& mods)
{
	if (camera->MouseMove(x, y, mods))
		context->Redraw();
}

void App::MouseDrag(int button, int x, int y, const ModifierKeys& mods)
{
	if (camera->MouseDrag(button, x, y, mods))
		context->Redraw();
}

void App::MouseWheel(int delta, const ModifierKeys& mods)
{	
	if (camera->MouseWheel(delta, mods))
		context->Redraw();
}

void App::Timestep(float dt)
{
	if (camera->Timestep(dt))
		context->Redraw();
}