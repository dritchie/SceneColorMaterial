#include "ImageStack.h"
#include "App.h"
#include "Common/GL.h"
#include "Viewing/TinkercadCamera.h"
#include "Math/TransformStack.h"
#include "Math/Transform.h"
#include "Assets/Mesh/CommonMesh.h"
#include "FLTKGraphicsWindow.h"
#include "FL/Fl_Menu_Bar.H"
#include "FLTKUtils.h"
#include "SegmentMesh.h"

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

void App::SaveFrameCallback(Fl_Widget* w, void* v)
{
	int viewport[4];
	glGetIntegerv(GL_VIEWPORT, viewport);
	int xStart = viewport[0];
	int yStart = viewport[1];
	int width = viewport[2];
	int height = viewport[3];
	
	float* pixels = new float[3*width*height];
	glReadPixels(xStart, yStart, width, height, GL_RGB, GL_FLOAT, pixels);

	ImageStack::Image img(width, height, 1, 3, pixels);
	ImageStack::Flip::apply(img, 'y');
	ImageStack::Save::apply(img, "savedFrame.png");

	delete[] pixels;
}

void App::GenerateSegmentationCallback(Fl_Widget* w, void* v)
{
	// Note: this assumes that the Display has been set to 'Flat'
	// (todo(?): make it invoke a flat display, then return to normal state?)

	int viewport[4];
	glGetIntegerv(GL_VIEWPORT, viewport);
	int xStart = viewport[0];
	int yStart = viewport[1];
	int width = viewport[2];
	int height = viewport[3];
	
	// IMPORTANT(!): glReadPixels pads all rows to a multiple of 4 bytes. That's why we need to read
	// out GL_RGBA, even though we'll only ever use the RGB data.
	GLubyte* pixels = new GLubyte[4*width*height];
	glReadPixels(xStart, yStart, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels);

	// Convert to floating point for ImageStack
	// (a bit annoying, given that we're just going to convert it back, anyways, but
	// ImageStack has nice utilities)
	float* fpixels = new float[3*width*height];
	for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
	{
		int inbase = 4*(y*width + x);
		int outbase = 3*(y*width + x);
		fpixels[outbase] = pixels[inbase] / 255.0f;
		fpixels[outbase+1] = pixels[inbase+1] / 255.0f;
		fpixels[outbase+2] = pixels[inbase+2] / 255.0f;
	}

	ImageStack::Image img(width, height, 1, 3, fpixels);
	ImageStack::Flip::apply(img, 'y');
	ImageStack::Save::apply(img, "../Output/segmentation.png");

	SegmentMesh* segmesh = new SegmentMesh(img, img, true);
	segmesh->SaveGroupAndSegmentMasks("../Output");
	segmesh->SaveModelDescription("../Output/segDescription.txt");

	delete[] pixels;
	delete[] fpixels;
	delete segmesh;
}

void App::SaveCameraCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;

	ofstream camout("../Output/camera.txt");
	if (!camout.is_open())
		FatalError(string("App::SaveCameraCallback - Could not open Output/camera.txt"));

	app->camera->Serialize(camout);
	camout.close();
}

void App::RestoreCameraCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;
	string camerafile = app->params.StringParam("savedCamera");
	ifstream camin(camerafile.c_str());
	if (camin.is_open())
	{
		app->camera->Deserialize(camin);
		camin.close();
		app->context->Redraw();
	}
}

void App::SaveColorAssignmentsCallback(Fl_Widget* w, void* v)
{
	App* app = (App*)v;

	ofstream assfile("../Output/assignments.txt");
	if (!assfile.is_open())
		FatalError(string("App::SaveColorAssignmentsCallback - Could not open Output/assignments.txt"));

	// Save colors
	for (auto it = app->scene.colorGroups.begin(); it != app->scene.colorGroups.end(); it++)
	{
		string name = it->first;
		ColorGroup* cg = it->second;
		assfile << "Color " << name << " " << cg->color[0] << " " << cg->color[1] << " " << cg->color[2] << endl;
	}

	// Save assignments
	for (UINT i = 0; i < app->scene.models.size(); i++)
	{
		Model* m = app->scene.models[i];
		for (UINT j = 0; j < m->components.size(); j++)
		{
			ModelComponent* mc = m->components[j];
			assfile << "Assignment " << i << " " << j << " " << mc->colorGroup->name << endl;
		}
	}
	assfile.close();
}

GraphicsContext* App::InitAndShowUI(int argc, char** argv)
{
	Fl_Window* window = new Fl_Window(params.IntParam("windowWidth"), params.IntParam("windowHeight"), params.StringParam("appName").c_str());

	// Menu bar
	Fl_Menu_Bar* menuBar = new Fl_Menu_Bar(0, 0, params.IntParam("windowWidth"), params.IntParam("menuBarHeight"));
	Fl_Menu_Item menuitems[] = {
		{ "&Display", 0, 0, 0, FL_SUBMENU },
			{ "Shaded",  0, DisplayShadedCallback, this, FL_MENU_RADIO | FL_MENU_VALUE },
			{ "Flat", 0, DisplayFlatCallback, this, FL_MENU_RADIO | FL_MENU_DIVIDER},
			{ "Highlight Fixed",  0, DisplayFixedCallback, this, FL_MENU_TOGGLE },
		{ 0 },
		{ "&Tools", 0, 0, 0, FL_SUBMENU },
			{ "Save Frame",  0, SaveFrameCallback, this },
			{ "Generate Segmentation",  0, GenerateSegmentationCallback, this },
			{ "Save Camera",  0, SaveCameraCallback, this },
			{ "Restore Saved Camera",  0, RestoreCameraCallback, this },
			{ "Save Color Assignments",  0, SaveColorAssignmentsCallback, this },
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

	// If there's an available color assignments file, read that and modify the scene accordingly
	LoadColorAssignments();
}

void App::LoadColorAssignments()
{
	ifstream assfile(params.StringParam("savedColorAssignments"));
	if (assfile.is_open())
	{
		char line[1024];
		while (!assfile.eof())
		{
			assfile.getline(line, 1023);
			stringstream ss;
			ss.str(line);
			string command;
			ss >> command;
			if (command == "Assignment")
			{
				int modelindex, compindex;
				string colorname;
				ss >> modelindex >> compindex >> colorname;
				ColorGroup* cg = scene.colorGroups[colorname];
				scene.models[modelindex]->components[compindex]->SetColorGroup(cg);
			}
			else if (command == "Color")
			{
				string colorname;
				float r, g, b;
				ss >> colorname >> r >> g >> b;
				ColorGroup* cg = scene.colorGroups[colorname];
				cg->color[0] = r; cg->color[1] = g; cg->color[2] = b;
			}
		}
		assfile.close();
		context->Redraw();
	}
}

void App::InitCamera()
{
	TinkercadCamera* tinkerCam = new TinkercadCamera(Vector3f::Zero(), -5.0f*Vector3f::UnitY(), Vector3f::UnitZ(), Vector3f::UnitZ());
	tinkerCam->OrbitSpeed() = (float)params.FloatParam("orbitSpeed");
	tinkerCam->ScrollZoomSpeed() = (float)params.FloatParam("zoomSpeed");
	tinkerCam->DollySpeed() = (float)params.FloatParam("dollySpeed");
	camera = tinkerCam;

	// If the params specify a camera file, load up the configuration from this file
	string camerafile = params.StringParam("savedCamera");
	ifstream camin(camerafile.c_str());
	if (camin.is_open())
	{
		camera->Deserialize(camin);
		camin.close();
	}
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