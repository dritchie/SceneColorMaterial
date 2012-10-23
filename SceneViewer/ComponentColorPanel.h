#pragma once

#ifndef __COMPONENT_COLOR_PANEL_H
#define __COMPONENT_COLOR_PANEL_H

#include "FL/Fl_Pack.H"
#include "FL/Fl_Button.H"

struct UTF8ModelComponent;

class ComponentColorPanel : public Fl_Pack
{
public:
	ComponentColorPanel(int x, int y, int w, int h);
	void SetActiveComponent(UTF8ModelComponent* comp);

private:

	// Callbacks
	static Fl_Callback ColorButtonCallback;

	UTF8ModelComponent* activeComponent;
	Fl_Button* colorButton;
};

#endif

