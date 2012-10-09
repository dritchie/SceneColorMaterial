#include "ImageStack.h"

#include "App.h"
#include "Common/GL.h"
#include "Viewing/MayaCamera.h"
#include "Math/TransformStack.h"
#include "Math/Transform.h"
#include "Assets/Shader/Shader.h"
#include "Assets/Shader/ShaderProgram.h"
#include "UTF8Model.h"
#include "Utility/FileSystem.h"
#include "Math/Math.h"
#include <fstream>
#include <limits>
#include "densecrf.h"

using namespace std;
using namespace GraphicsEngine;
using namespace ImageStack;
using Eigen::Vector3f;
using Eigen::Vector4f;
using Eigen::AlignedBox3f;


Vector4f UintToFloat4(UINT i)
{
	char* bytePtr = (char*)(&i);
	Vector4f float4;
	float4[0] = bytePtr[0] / 255.0f;
	float4[1] = bytePtr[1] / 255.0f;
	float4[2] = bytePtr[2] / 255.0f;
	float4[3] = bytePtr[3] / 255.0f;
	return float4;
}

Vector3f RandomColor()
{
	Vector3f color;
	color[0] = ((float)rand()) / RAND_MAX;
	color[1] = ((float)rand()) / RAND_MAX;
	color[2] = ((float)rand()) / RAND_MAX;
	return color;
}

unsigned char QuantizeColorChannel(float inchan)
{
	inchan = inchan > 1.0f? 1.0f : inchan;
	return (unsigned char)(inchan * 255);
}


App::App()
{
	model = NULL;
	vs = NULL;
	fs = NULL;
	prog = NULL;
	capturing = false;
}

App::~App()
{
	SAFEDELETE(camera);
	SAFEDELETE(model);
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

	vs = new Shader(Shader::VertexShader); vs->FromFile("MatIDColor.vert");
	fs = new Shader(Shader::FragmentShader); fs->FromFile("MatIDColor.frag");
	vector<Shader*> shaders; shaders.push_back(vs); shaders.push_back(fs);
	prog = new ShaderProgram(shaders);
	prog->Bind();

	model = new UTF8Model;
	model->Load(params.StringParam("inputModel"), params.StringParam("utf8Dir"));
}

void App::InitCamera()
{
	MayaCamera* mayaCam = new MayaCamera(Vector3f::Zero(), -5.0f*Vector3f::UnitY(), Vector3f::UnitZ(), Vector3f::UnitZ());
	camera = mayaCam;
}

void App::ViewpointPriors(UTF8Model* model, const Viewpoint& viewpoint, vector<Image>& priors)
{
	// Get viewport statistics
	int viewport[4];
	glGetIntegerv(GL_VIEWPORT, viewport);
	int x = viewport[0];
	int y = viewport[1];
	int width = viewport[2];
	int height = viewport[3];

	TransformStack::Modelview().Load(viewpoint.camera.GetLookAtTransform());
	TransformStack::Projection().Load(Transform::Perspective(viewpoint.fovy, ((float)width/height), viewpoint.znear, viewpoint.zfar));

	//////// TODO: Get rid of this  //////////
	// We want to instead scale down to some uniform size (i.e. make the longest bbox axis be 1)
	// and translate the centroid to the origin.
	Transform t = Transform::Scale(0.1f);
	TransformStack::Modelview().Multiply(t);
	//////////////////////////////////////////

	TransformStack::Modelview().Bind();
	TransformStack::Projection().Bind();

	Vector4f clearColor = UintToFloat4(0);
	glClearColor(clearColor[0], clearColor[1], clearColor[2], clearColor[3]);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	int colloc = ShaderProgram::CurrentProgram()->GetUniformLocation("Color");
	for (UINT i = 0; i < model->components.size(); i++)
	{
		Vector4f float4 = UintToFloat4(i+1);
		glUniform4fv(colloc, 1, float4.data());
		model->components[i].mesh->Render();
	}

	// Read pixels;
	UINT* data = new UINT[width*height];
	glReadPixels(x, y, width, height, GL_RGBA, GL_UNSIGNED_BYTE, data);

	// Extract and save separate image for each segment
	UINT numSegments = model->components.size()+1;
	priors.resize(numSegments);
	for (UINT i = 0; i < numSegments; i++)
	{
		// One frame, one channel
		priors[i] = Image(width, height, 1, 1);
	}
	for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
	{
		UINT segIndex = data[y*width + x];
		*(priors[segIndex](x,height-1-y)) = 1.0f;
	}
	char fname[1024];
	for (UINT i = 0; i < priors.size(); i++)
	{
		SafePrintf(fname, "output/segment_%u.png", i);
		Save::apply(priors[i], fname);
	}

	delete[] data;

	// Turn segments into 'prior' images, save them.
	float blurWidthFactor = params.FloatParam("gaussianWidthFactor");
	float threshold = params.FloatParam("threshold");
	float blurWidth = blurWidthFactor * (width > height ? width : height);

	// 'Grow' the segments (except the empty space segment)
	for (UINT i = 1; i < numSegments; i++)
	{
		FastBlur::apply(priors[i], blurWidth, blurWidth, 1);
		Threshold::apply(priors[i], threshold);
	}

	// Blur the expanded segments
	for (UINT i = 0; i < numSegments; i++)
	{
		FastBlur::apply(priors[i], blurWidth, blurWidth, 1);
		SafePrintf(fname, "output/prior_%u.png", i);
		Save::apply(priors[i], fname);
	}
}

float App::SegmentImage_FromSingleViewpoint(Image& img, UTF8Model* model, const Viewpoint& viewpoint, short* mapAssignment)
{
	vector<Image> priors;
	ViewpointPriors(model, viewpoint, priors);

	UINT numSegments = priors.size();
	int width = img.width;
	int height = img.height;

	// Form the CFG unary energy term from the prior images.
	float* unary = new float[width*height*numSegments];
	for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
	{
		float* pixPtr = unary + y*width*numSegments + x*numSegments;

		// Plug in the raw values, then normalize & negative log
		float totalmass = 0.0f;
		for (UINT i = 0; i < numSegments; i++)
		{
			float mass = *(priors[i](x,y));
			pixPtr[i] = mass;
			totalmass += mass;
		}
		for (UINT i = 0; i < numSegments; i++)
			pixPtr[i] = -log(pixPtr[i] / totalmass);
	}

	// Convert the image into 24-bit color (expected by CRF inference code)
	unsigned char* im = new unsigned char[width*height*3];
	for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
	{
		unsigned char* baseptr = im + y*width*3 + x*3;
		baseptr[0] = QuantizeColorChannel(img(x,y)[0]);
		baseptr[1] = QuantizeColorChannel(img(x,y)[1]);
		baseptr[2] = QuantizeColorChannel(img(x,y)[2]);
	}

	// Set up CRF
	DenseCRF2D crf(width, height, numSegments);
	// Specify the unary potential as an array of size W*H*(#classes)
	// packing order: x0y0l0 x0y0l1 x0y0l2 .. x1y0l0 x1y0l1 ...
	crf.setUnaryEnergy(unary);
	// add a color independent term (feature = pixel location 0..W-1, 0..H-1)
	// x_stddev = 3
	// y_stddev = 3
	// weight = 3
	crf.addPairwiseGaussian( 3, 3, 3 );
	// add a color dependent term (feature = xyrgb)
	// x_stddev = 60
	// y_stddev = 60
	// r_stddev = g_stddev = b_stddev = 20
	// weight = 10
	crf.addPairwiseBilateral( 60, 60, 20, 20, 20, im, 10 );
	
	// Do map inference
	crf.map(10, mapAssignment);
	
	// Colorize and output result
	vector<Vector3f> colormap(numSegments);
	Image outimg(width, height, 1, 3);
	for (int i = 0; i < numSegments; i++)
		colormap[i] = RandomColor();
	for (int y = 0; y < height; y++) for (int x = 0; x < width; x++)
	{
		int idx = y*width + x;
		const Vector3f& color = colormap[mapAssignment[idx]];
		outimg(x,y)[0] = color[0];
		outimg(x,y)[1] = color[1];
		outimg(x,y)[2] = color[2];
	}
	Save::apply(outimg, "output/segmentation.png");

	// Also save the energy of the MAP to a text file
	float* energies = new float[width*height];
	float totalUnaryEnergy = 0.0f;
	float totalPairwiseEnergy = 0.0f;
	crf.unaryEnergy(mapAssignment, energies);
	for (UINT i = 0; i < width*height; i++)
		totalUnaryEnergy += energies[i];
	crf.pairwiseEnergy(mapAssignment, energies);
	for (UINT i = 0; i < width*height; i++)
		totalPairwiseEnergy += energies[i];
	float totalEnergy = totalUnaryEnergy + totalPairwiseEnergy;
	ofstream energystream("output/MAPenergy.txt");
	energystream << "Unary Energy: " << totalUnaryEnergy << endl;
	energystream << "Pairwise Energy: " << totalPairwiseEnergy << endl;
	energystream << "Total Energy: " << totalEnergy << endl;
	energystream.close();

	delete[] energies;
	delete[] unary;
	delete[] im;

	return totalEnergy;
}

float App::SegmentImage_FindOptimalViewpoint(Image& img, UTF8Model* model, short* mapAssignment)
{
	// Sample upper hemisphere
	// TODO: archive3D models are y-up; have to do some trickery to make them z-up.

	const int phiStrata = params.IntParam("phiStrata");
	const int thetaStrata = params.IntParam("thetaStrata");
	const int samplesPerStratum = params.IntParam("samplesPerStratum");

	Viewpoint currView;
	Viewpoint bestView;
	float bestEnergy = std::numeric_limits<float>::max();

	for (int i = 0; i < phiStrata; i++)
	{
		float phimin = ((float)i)/phiStrata;
		float phimax = ((float)(i+1))/phiStrata;
		float phisample = UniformRandom(phimin, phimax);
		for (int j = 0; j < thetaStrata; j++)
		{
			float thetamin = ((float)j)/thetaStrata;
			float thetamax = ((float)(j+1))/thetaStrata;
			float thetasample = UniformRandom(thetamin, thetamax);

			// Figure out scale for model + distance to camera

		}
	}

	// TODO: Make this work
	return 0.0f;
}

void App::RenderNormal()
{
	TransformStack::Modelview().Load(camera->GetLookAtTransform());

	Transform t = Transform::Scale(0.1f);
	TransformStack::Modelview().Multiply(t);

	TransformStack::Modelview().Bind();
	TransformStack::Projection().Bind();

	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	int colloc = ShaderProgram::CurrentProgram()->GetUniformLocation("Color");
	for (UINT i = 0; i < model->components.size(); i++)
	{
		glUniform4f(colloc, 1.0f, 1.0f, 1.f, 1.0f);
		model->components[i].mesh->Render();
	}
}

void App::RenderCapture()
{
	Viewpoint viewpoint;
	viewpoint.camera = Camera(*(this->camera));
	viewpoint.fovy = params.FloatParam("fovy");
	viewpoint.znear = params.FloatParam("znear");
	viewpoint.zfar = params.FloatParam("zfar");

	Image img = Load::apply(params.StringParam("inputImage"));
	short* map = new short[img.width*img.height];

	float mapEnergy = SegmentImage_FromSingleViewpoint(img, this->model, viewpoint, map);
}

void App::Render()
{
	if (capturing)
	{
		RenderCapture();
		capturing = false;
	}
	else
	{
		RenderNormal();
	}
}

void App::KeyDown(int key, const ModifierKeys& mods)
{
	if (camera->KeyDown(key, mods))
		context->Redraw();
}

void App::KeyUp(int key, const ModifierKeys& mods)
{
	if (key == 'c')
	{
		capturing = true;
		context->Redraw();
	}
	else if (key == 's')
	{
		ofstream outfile("output/camera.txt");
		camera->Serialize(outfile);
		outfile.close();
	}
	else if (key == 'l')
	{
		if (FileExists("output/camera.txt"))
		{
			ifstream infile("output/camera.txt");
			camera->Deserialize(infile);
			infile.close();
			context->Redraw();
		}
	}

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