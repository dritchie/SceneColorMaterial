#pragma once

#ifndef __COLOR_PANEL_H
#define __COLOR_PANEL_H

#include "Model.h"
#include "Application/GraphicsContext.h"
#include "FL/Fl_Group.H"
#include "FL/Fl_Color_Chooser.H"
#include "FL/Fl_Box.H"
#include "FL/Fl_Button.H"
#include "FL/Fl_Check_Button.H"
#include "FL/Fl_Choice.H"


class ColorPanel : public Fl_Group
{
public:
	ColorPanel(GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h);
	void SetActiveComponent(ModelComponent* comp);

private:

	void SetActiveColor(float r, float g, float b);

	// Callbacks
	static Fl_Callback ColorChooserCallback;
	static Fl_Callback ResetButtonCallback;
	static Fl_Callback FixedToggleCallback;
	static Fl_Callback WhichColorGroupCallback;

	GraphicsEngine::GraphicsContext* context;

	ModelComponent* activeComponent;

	Fl_Choice* whichColorGroup;
	Fl_Button* activeColorChip;
	Fl_Button* resetColorChip;
	float resetColor[3];
	Fl_Color_Chooser* colorChooser;
	Fl_Button* resetButton;
	Fl_Check_Button* fixedToggle;
};

#endif

