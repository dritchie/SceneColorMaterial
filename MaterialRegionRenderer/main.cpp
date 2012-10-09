#include "GraphicsApp.h"
#include "App.h"


int main(int argc, char** argv)
{
	App* app = new App;
	return GraphicsApp::Launch(app, "Parameters.txt", argc, argv);
}