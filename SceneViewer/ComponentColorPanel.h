#pragma once

#ifndef __COMPONENT_COLOR_PANEL_H
#define __COMPONENT_COLOR_PANEL_H

#include "UTF8Model.h"
#include "Application/GraphicsContext.h"
#include "FL/Fl_Group.H"
#include "FL/Fl_Color_Chooser.H"
#include "FL/Fl_Box.H"
#include "FL/Fl_Button.H"


class ComponentColorPanel : public Fl_Group
{
public:
	ComponentColorPanel(GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h);
	void SetActiveComponent(UTF8Model* model, int compIndex);

private:

	// Callbacks
	static Fl_Callback ColorChooserCallback;

	GraphicsEngine::GraphicsContext* context;
	UTF8ModelComponent* activeComponent;
	char labelText[1024];
	Fl_Box* label;
	Fl_Button* colorChip;
	Fl_Color_Chooser* colorChooser;
};

#endif

