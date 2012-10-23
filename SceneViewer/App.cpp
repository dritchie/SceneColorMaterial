#include "App.h"
#include "Common/GL.h"
#include "Viewing/TinkercadCamera.h"
#include "Math/TransformStack.h"
#include "Math/Transform.h"
#include "Assets/Shader/Shader.h"
#include "Assets/Shader/ShaderProgram.h"
#include "Assets/Mesh/CommonMesh.h"
#include "WSSScene.h"

using namespace std;
using namespace GraphicsEngine;
using Eigen::Vector3f;
using Eigen::Vector4f;


WSSScene* scene;
Shader *vs, *fs;
ShaderProgram* prog;
Vector3f lightDir;


App::App()
{
}

App::~App()
{
	SAFEDELETE(camera);
	SAFEDELETE(scene);
	SAFEDELETE(prog);
	SAFEDELETE(vs);
	SAFEDELETE(fs);
}

void App::Init(const string& paramfile)
{
	GraphicsApp::Init(paramfile);
	InitCamera();
}

void App::InitGraphics(GraphicsContext* ctx)
{
	GraphicsApp::InitGraphics(ctx);

	// 3D warehouse models have terrible problems with z-fighting if we
	// don't do backface culling
	glEnable(GL_CULL_FACE);

	// Shader
	vs = new Shader(Shader::VertexShader); vs->FromFile("Model.vert");
	fs = new Shader(Shader::FragmentShader); fs->FromFile("Model.frag");
	vector<Shader*> shaders; shaders.push_back(vs); shaders.push_back(fs);
	prog = new ShaderProgram(shaders);
	prog->Bind();

	// Init lighting
	lightDir = Vector3f(-0.35f, -0.45f, 1.0f);

	// Load scene
	scene = new WSSScene; scene->Load("Scenes/helloWorld.json", params.StringParam("dataRoot"));
}

void App::InitCamera()
{
	TinkercadCamera* tinkerCam = new TinkercadCamera(Vector3f::Zero(), -5.0f*Vector3f::UnitY(), Vector3f::UnitZ(), Vector3f::UnitZ());
	tinkerCam->OrbitSpeed() = params.FloatParam("orbitSpeed");
	tinkerCam->ScrollZoomSpeed() = params.FloatParam("zoomSpeed");
	tinkerCam->DollySpeed() = params.FloatParam("dollySpeed");
	camera = tinkerCam;
}

void App::Render()
{
	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	TransformStack::Projection().Bind();
	TransformStack::Modelview().Load(camera->GetLookAtTransform());
	int lightloc = ShaderProgram::CurrentProgram()->GetUniformLocation("LightDir");
	Vector3f xformedLight = TransformStack::Modelview().Top().TransformVector(lightDir);
	glUniform3fv(lightloc, 1, &xformedLight[0]);

	scene->Render();
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
	if (camera->MouseDown(button, x, y, mods))
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