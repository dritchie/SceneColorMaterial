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

class Scene;
struct ColorGroup;

class ColorPanel : public Fl_Group
{
public:
	ColorPanel(Scene* scene, GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h);
	void RefreshColorGroupList();

private:

	void SetActiveColor(float r, float g, float b);

	// Callbacks
	static Fl_Callback ColorChooserCallback;
	static Fl_Callback ResetButtonCallback;
	static Fl_Callback FixedToggleCallback;
	static Fl_Callback WhichColorGroupCallback;

	ColorGroup* activeColorGroup;
	Scene* scene;
	GraphicsEngine::GraphicsContext* context;

	Fl_Choice* whichColorGroup;
	Fl_Button* activeColorChip;
	Fl_Button* resetColorChip;
	float resetColor[3];
	Fl_Color_Chooser* colorChooser;
	Fl_Button* resetButton;
	Fl_Check_Button* fixedToggle;
};

#endif

