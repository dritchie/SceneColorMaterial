#pragma once

#ifndef __COMPONENT_COLOR_PANEL_H
#define __COMPONENT_COLOR_PANEL_H

#include "Model.h"
#include "Application/GraphicsContext.h"
#include "FL/Fl_Group.H"
#include "FL/Fl_Color_Chooser.H"
#include "FL/Fl_Box.H"
#include "FL/Fl_Button.H"
#include "FL/Fl_Check_Button.H"


class ComponentColorPanel : public Fl_Group
{
public:
	ComponentColorPanel(GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h);
	void SetActiveComponent(ModelComponent* comp);

private:

	void SetActiveColor(float r, float g, float b);

	// Callbacks
	static Fl_Callback ColorChooserCallback;
	static Fl_Callback ResetButtonCallback;
	static Fl_Callback FixedToggleCallback;

	GraphicsEngine::GraphicsContext* context;

	ModelComponent* activeComponent;
	char labelText[1024];
	Fl_Box* label;
	Fl_Button* activeColorChip;
	Fl_Button* resetColorChip;
	float resetColor[3];
	Fl_Color_Chooser* colorChooser;
	Fl_Button* resetButton;
	Fl_Check_Button* fixedToggle;
};

#endif

