#include "WSSScene.h"
#include "json/reader.h"
#include "JsonUtils.h"
#include <fstream>

using namespace std;

void WSSScene::Load(const string& sceneFilename, const string& dataDir)
{
	ifstream jsonstream(sceneFilename.c_str());
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
		UTF8Model& model = models[i];
		string mid = JsonUtils::GetRequiredJsonProp(m, "modelID").asString();
		string modelfilename = jsonDir + "/" + mid + ".json";
		model.Load(modelfilename, utf8Dir, texDir);

		vector<float> transformData;
		JsonUtils::ParseFloatArrayProp(JsonUtils::GetRequiredJsonProp(m, "transform"), transformData);
		model.transform = GraphicsEngine::Transform(Eigen::Matrix4f(&transformData[0]));
	}
}

void WSSScene::Render()
{
	for (UINT i = 0; i < models.size(); i++)
	{
		models[i].Render();
	}
}

void WSSScene::Pick()
{
	for (UINT i = 0; i < models.size(); i++)
	{
		// +1, so that id 0 is reserved for the 'background' of the scene
		models[i].Pick(i+1);
	}
}