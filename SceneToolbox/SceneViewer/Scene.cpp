#include "Scene.h"

using namespace std;

void Scene::LoadFromWSS(WSSScene* wss)
{
	FreeMemory();

	// Convert the models one by one, also
	// assigning indices
	models.resize(wss->models.size());
	for (UINT i = 0; i < models.size(); i++)
	{
		Model* model = new Model;
		model->LoadFromUTF8(wss->models[i]);
		model->index = (int)i;

		models[i] = model;
	}

	// Go through and pull out all the ColorGroups
	map<string, ColorGroup*> tempGroups;
	for (UINT i = 0; i < models.size(); i++)
	{
		Model* m = models[i];
		for (UINT j = 0; j < m->components.size(); j++)
		{
			ColorGroup* cg = m->components[j]->colorGroup;
			// It's possible that this color group already exists in our map
			// (e.g. two instances of the same model). In this case, delete
			// this color group, and assign the component to the equivalent
			// color group that we've already recorded.
			auto it = tempGroups.find(cg->name);
			if (it != tempGroups.end())
			{
				m->components[j]->SetColorGroup(it->second);
				delete cg;
			}
			else tempGroups[cg->name] = cg;
		}
	}

	// Rename ColorGroups
	UINT counter = 0;
	char newname[1024];
	for (auto it = tempGroups.begin(); it != tempGroups.end(); it++)
	{
		SafePrintf(newname, "Color_%u", counter);
		it->second->name = newname;
		colorGroups[newname] = it->second;
		counter++;
	}
}

void Scene::FreeMemory()
{
	for (UINT i = 0; i < models.size(); i++)
		delete models[i];

	for (auto it = colorGroups.begin(); it != colorGroups.end(); it++)
		delete it->second;

	models.clear();
	colorGroups.clear();
}

Scene::~Scene()
{
	FreeMemory();
}

void Scene::Render(const RenderOptions& opts)
{
	for (UINT i = 0; i < models.size(); i++)
	{
		models[i]->Render(opts);
	}
}

void Scene::Pick(const RenderOptions& opts)
{
	for (UINT i = 0; i < models.size(); i++)
	{
		models[i]->Pick(opts);
	}
}
