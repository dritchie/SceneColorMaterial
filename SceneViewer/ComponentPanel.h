#pragma once

#ifndef __COMPONENT_PANEL
#define __COMPONENT_PANEL

#include "Model.h"
#include "Application/GraphicsContext.h"
#include "FL/Fl_Group.H"
#include "FL/Fl_Box.H"
#include "FL/Fl_Choice.H"

class ComponentPanel : public Fl_Group
{
public:
	ComponentPanel(GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h);
	void SetActiveComponent(ModelComponent* comp);

private:

	ModelComponent* activeComponent;
	GraphicsEngine::GraphicsContext* context;

	// Callbacks
	static Fl_Callback AssignedColorGroupCallback;

	char labelText[1024];
	Fl_Box* label;
	Fl_Choice* assignedColorGroup;
};

#endif

