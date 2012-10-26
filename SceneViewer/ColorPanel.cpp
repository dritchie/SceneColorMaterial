#include "ColorPanel.h"
#include "FL/Fl_Color_Chooser.H"
#include "FL/Fl.H"
#include "FLTKUtils.h"
#include "Scene.h"
#include "ColorGroup.h"

void ColorPanel::SetActiveColor(float r, float g, float b)
{
	// Set the color chooser color (this may be a no op, if this function is being called
	// in response to the color chooser callback)
	colorChooser->rgb(r, g, b);

	// Copy the color into the active color chip color
	activeColorChip->color(fl_rgb_color((uchar)(r*255), (uchar)(g*255), (uchar)(b*255)));
	this->redraw();

	// Update the actual material color
	activeColorGroup->color[0] = r;
	activeColorGroup->color[1] = g;
	activeColorGroup->color[2] = b;

	// Tell the GL window to redraw
	context->Redraw();
}

void ColorPanel::ColorChooserCallback(Fl_Widget* w, void* v)
{
	ColorPanel* panel = (ColorPanel*)v;

	// Copy the color chooser color into the active color chip color
	float r = (float)panel->colorChooser->r();
	float g = (float)panel->colorChooser->g();
	float b = (float)panel->colorChooser->b();
	panel->SetActiveColor(r, g, b);
}

void ColorPanel::ResetButtonCallback(Fl_Widget* w, void* v)
{
	ColorPanel* panel = (ColorPanel*)v;
	
	// Reset the color of the active chip
	panel->SetActiveColor(panel->resetColor[0], panel->resetColor[1], panel->resetColor[2]);
}

void ColorPanel::FixedToggleCallback(Fl_Widget* w, void* v)
{
	ColorPanel* panel = (ColorPanel*)v;
	panel->activeColorGroup->isFixed = panel->fixedToggle->value();
}

void ColorPanel::WhichColorGroupCallback(Fl_Widget* w, void* v)
{
	ColorPanel* panel = (ColorPanel*)v;
	
	int selectedIndex = panel->whichColorGroup->value();
	const char* selectedName = panel->whichColorGroup->menu()[selectedIndex].label();
	ColorGroup* cg = panel->scene->colorGroups[selectedName];
	panel->activeColorGroup = cg;

	/** Update all the color widgets **/

	float* color = cg->color;

	// Update both color chips to be the same color
	panel->activeColorChip->color(fl_rgb_color((uchar)(color[0]*255), (uchar)(color[1]*255), (uchar)(color[2]*255)));
	panel->resetColorChip->color(fl_rgb_color((uchar)(color[0]*255), (uchar)(color[1]*255), (uchar)(color[2]*255)));
	panel->resetColor[0] = color[0];
	panel->resetColor[1] = color[1];
	panel->resetColor[2] = color[2];

	// Update the color chooser
	panel->colorChooser->rgb(color[0], color[1], color[2]);

	// Update the fixed toggle
	panel->fixedToggle->value(cg->isFixed);

	panel->redraw();
}


ColorPanel::ColorPanel(Scene* _scene, GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h)
	: Fl_Group(x, y, w, h), context(gContext), scene(_scene)
{
	// Setup UI

	int xx, yy;
	fl_window_to_widget(this, 10, 10, xx, yy);

	// Combo box for selecting Color Groups
	whichColorGroup = new Fl_Choice(xx+80, yy, 120, 30, "Color Group:");
	RefreshColorGroupList();

	// Color chips
	activeColorChip = new Fl_Button(xx, fl_below(whichColorGroup, 10), 95, 40);
	resetColorChip = new Fl_Button(fl_right_of(activeColorChip, 10), fl_below(whichColorGroup, 10), 95, 40);

	// Color chooser
	colorChooser = new Fl_Color_Chooser(xx, fl_below(activeColorChip, 10), 200, 95);		// Recommended dimensions
	colorChooser->callback(ColorChooserCallback, this);

	// Reset button
	resetButton = new Fl_Button(xx, fl_below(colorChooser, 10), 200, 40, "Reset");
	resetButton->callback(ResetButtonCallback, this);

	// Fixed check button
	fixedToggle = new Fl_Check_Button(xx, fl_below(resetButton, 10), 200, 30, "Fix this color");
	fixedToggle->callback(FixedToggleCallback, this);
}

void ColorPanel::RefreshColorGroupList()
{
	for (auto it = scene->colorGroups.begin(); it != scene->colorGroups.end(); it++)
	{
		ColorGroup* cg = it->second;
		whichColorGroup->add(cg->name.c_str(), 0, WhichColorGroupCallback, this);
	}
	whichColorGroup->value(0);	// Initially, show the first item
	if (whichColorGroup->value() == 0)
		WhichColorGroupCallback(whichColorGroup, this);
}