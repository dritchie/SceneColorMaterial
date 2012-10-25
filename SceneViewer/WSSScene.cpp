#include "WSSScene.h"
#include "json/reader.h"
#include "JsonUtils.h"
#include <fstream>

using namespace std;

void WSSScene::Load(const string& sceneFilename, const string& dataDir)
{
	FreeMemory();

	ifstream jsonstream(sceneFilename.c_str());
	if (!jsonstream.is_open())
	{
		FatalError(string("WSSScene::Load - No such file '") + sceneFilename + "'")
	}
	Json::Value root;
	Json::Reader reader;
	bool parseSuccess = reader.parse(jsonstream, root);
	jsonstream.close();
	if (!parseSuccess)
	{
		string msg = "WSSScene::Load - Failed to parse JSON file '" + sceneFilename + "'\n";
		msg += reader.getFormatedErrorMessages();
		FatalError(msg)
	}

	string jsonDir = RStrip(dataDir, '/') + "/model";
	string utf8Dir = RStrip(dataDir, '/') + "/geometry";
	string texDir = RStrip(dataDir, '/') + "/texture";

	// Parse models
	models.resize(root.size());
	for (UINT i = 0; i < root.size(); i++)
	{
		const Json::Value m = root[i];
		UTF8Model* model = new UTF8Model;
		string mid = JsonUtils::GetRequiredJsonProp(m, "modelID").asString();
		model->Load(mid, jsonDir, utf8Dir, texDir);

		vector<float> transformData;
		JsonUtils::ParseFloatArrayProp(JsonUtils::GetRequiredJsonProp(m, "transform"), transformData);
		model->transform = GraphicsEngine::Transform(Eigen::Matrix4f(&transformData[0]));

		models[i] = model;
	}
}

WSSScene::~WSSScene()
{
	FreeMemory();
}

void WSSScene::FreeMemory()
{
	for (UINT i = 0; i < models.size(); i++)
		delete models[i];
	models.clear();
}