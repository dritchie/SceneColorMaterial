#include "ImageStack.h"
#include "Model.h"
#include "Math/TransformStack.h"
#include "Assets/Shader/ShaderProgram.h"
#include "Common/GL.h"
#include "Picker.h"
#include "Application/GraphicsAppParams.h"
#include "ColorGroup.h"
#include "Eigen/Core"

using namespace std;
using namespace GraphicsEngine;

void ModelComponent::SetColorGroup(ColorGroup* newGroup)
{
	if (colorGroup)
	{
		auto it = colorGroup->members.find(this);
		if (it != colorGroup->members.end())
			colorGroup->members.erase(it);
	}
	newGroup->members.insert(this);
	colorGroup = newGroup;
}

void Model::LoadFromUTF8(UTF8Model* utf8)
{
	FreeMemory();

	transform = utf8->transform;
	components.resize(utf8->components.size());
	for (UINT i = 0; i < components.size(); i++)
	{
		ModelComponent* mycomp = new ModelComponent;
		UTF8ModelComponent* ucomp = utf8->components[i];

		/** Copy the color group info **/

		ColorGroup* cg = new ColorGroup;
		mycomp->SetColorGroup(cg);

		char groupname[1024];
		SafePrintf(groupname, "%s_%u", utf8->modelID.c_str(), i);
		cg->name = std::string(groupname);

		memcpy(cg->color, ucomp->material->color, 3*sizeof(float));
		
		// For textured things, we just use the average color of the texture
		if (!ucomp->material->texFilename.empty())
		{
			ImageStack::Image teximg = ImageStack::Load::apply(ucomp->material->texFilename);
			Eigen::Vector3f color(0.0f, 0.0f, 0.0f);
			for (int y = 0; y < teximg.height; y++) for (int x = 0; x < teximg.width; x++)
			{
				float* pixel = teximg(x,y);
				color += Eigen::Vector3f(pixel);
			}
			color /= (float)(teximg.width*teximg.height);
			memcpy(cg->color, &color[0], 3*sizeof(float));
		}

		/** Copy the mesh **/
		mycomp->mesh = new GraphicsEngine::CommonMesh(*ucomp->mesh);

		/** Fill in other stuff **/
		mycomp->index = (int)i;
		mycomp->owner = this;

		components[i] = mycomp;
	}

	///** Split all components into connected components **/
	//vector<ModelComponent*> newcomps;
	//for (UINT i = 0; i < components.size(); i++)
	//{
	//	ModelComponent* mc = components[i];
	//	vector<CommonMesh*> ccs;
	//	mc->mesh->ConnectedComponents(ccs, true);
	//	for (UINT j = 0; j < ccs.size(); j++)
	//	{
	//		ModelComponent* newmc = new ModelComponent(*mc);
	//		newmc->mesh = ccs[j];
	//		newcomps.push_back(newmc);
	//	}
	//}
	//// Update indices
	//for (UINT i = 0; i < newcomps.size(); i++)
	//	newcomps[i]->index = (int)i;
	//// Delete/replace old comps
	//for (UINT i = 0; i < components.size(); i++)
	//	delete components[i];
	//components = newcomps;
}

Model::~Model()
{
	FreeMemory();
}

void Model::FreeMemory()
{
	for (UINT i = 0; i < components.size(); i++)
	{
		delete components[i];
	}

	components.clear();
}

void Model::Render(const RenderOptions& opts)
{
	TransformStack::Modelview().Push();
	TransformStack::Modelview().Multiply(transform);
	TransformStack::Modelview().Bind();

	for (UINT i = 0; i < components.size(); i++)
	{
		ModelComponent* comp = components[i];
		int colloc = ShaderProgram::CurrentProgram()->GetUniformLocation("Color");
		if (colloc >= 0)
		{
			if (comp->colorGroup->isFixed && opts.highlightFixed)
				glUniform3f(colloc, (float)opts.params->FloatParam("fixedColorR"),
									(float)opts.params->FloatParam("fixedColorG"), 
									(float)opts.params->FloatParam("fixedColorB"));
			else
				glUniform3fv(colloc, 1, &(comp->colorGroup->color[0]));
		}
		components[i]->mesh->Render();
	}

	TransformStack::Modelview().Pop();
}

void Model::Pick(const RenderOptions& opts)
{
	TransformStack::Modelview().Push();
	TransformStack::Modelview().Multiply(transform);
	TransformStack::Modelview().Bind();

	// +1, so that id 0 is reserved for the 'background' of the scene
	UINT myIndex = (UINT)(index + 1);

	for (UINT i = 0; i < components.size(); i++)
	{
		Eigen::Vector4f floats = Picker::PackIDs((unsigned short)myIndex, (unsigned short)i);
		int bytesloc = ShaderProgram::CurrentProgram()->GetUniformLocation("IdBytes");
		if (bytesloc >= 0)
			glUniform4fv(bytesloc, 1, &floats[0]);
		components[i]->mesh->Render();
	}

	TransformStack::Modelview().Pop();
}


void Model::SavePBRT(const std::string& filename, const GraphicsEngine::Transform& cameraTransform)
{
	ofstream file(filename.c_str());
	for (UINT i = 0; i < components.size(); i++)
	{
		ModelComponent* mc = components[i];
		ColorGroup* cg = mc->colorGroup;

		// Find all front-facing faces
		GraphicsEngine::Transform normalTransform = (cameraTransform * transform).Inverse().Transpose();
		vector<UINT> frontFacingFaces;
		for (UINT i = 0; i < mc->mesh->NumFaces(); i++)
		{
			const MeshFace& f = mc->mesh->Faces()[i];
			Eigen::Vector3f normal = normalTransform.TransformVector(mc->mesh->Normal(f));
			bool isFrontFacing = (normal.z() > 0.0f);

			// Sepcial case: We use all triangles of the room geometry (it ends up having some holes
			// if we apply the front-facing test...)
			if (this->index == 0)
				isFrontFacing = true;

			if (isFrontFacing)
				frontFacingFaces.push_back(i);
		}

		if (frontFacingFaces.size() > 0)
		{
			// Describe material
			file << "Material \"matte\" \"color Kd\" [" << cg->color[0] << ' ' << cg->color[1] << ' ' << cg->color[2] << "]" << endl;

			// Describe shape
			file << "AttributeBegin" << endl;
			file << "Shape \"trianglemesh\"" << endl;
			file << "  \"point P\" [" << endl;
			// Vertices
			for(UINT vertexIndex = 0; vertexIndex < mc->mesh->NumVertices(); vertexIndex++)
			{
				Eigen::Vector3f v = mc->mesh->Vertices()[vertexIndex];
				Eigen::Vector3f pos = transform.TransformPoint(v);
				file << "    " << pos.x() << ' ' << pos.y() << ' ' << pos.z() << endl;
			}
			file << "  ]" << endl;

			// Indices
			file << "  \"integer indices\" [" << endl;
			for(UINT faceIndex = 0; faceIndex < frontFacingFaces.size(); faceIndex++)
			{
				const MeshFace& f = mc->mesh->Faces()[frontFacingFaces[faceIndex]];
				file << "	" << f.i[0] << " " << f.i[1] << " " << f.i[2] << endl;
			}
			file << "  ]" << endl;
			file << "AttributeEnd" << endl;
		}
	}
	file.close();
}