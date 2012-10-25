#include "ColorPanel.h"
#include "FL/Fl_Color_Chooser.H"
#include "FL/Fl.H"
#include "FLTKUtils.h"

void ColorPanel::SetActiveColor(float r, float g, float b)
{
	// Set the color chooser color (this may be a no op, if this function is being called
	// in response to the color chooser callback)
	colorChooser->rgb(r, g, b);

	// Copy the color into the active color chip color
	activeColorChip->color(fl_rgb_color((uchar)(r*255), (uchar)(g*255), (uchar)(b*255)));
	this->redraw();

	// Update the actual material color
	activeComponent->color[0] = r;
	activeComponent->color[1] = g;
	activeComponent->color[2] = b;

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
	panel->activeComponent->isFixed = panel->fixedToggle->value();
}

void ColorPanel::WhichColorGroupCallback(Fl_Widget* w, void* v)
{
	ColorPanel* panel = (ColorPanel*)v;
	// TODO: FILL THIS IN
}


ColorPanel::ColorPanel(GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h)
	: Fl_Group(x, y, w, h), context(gContext)
{
	// Setup UI

	int xx, yy;
	fl_window_to_widget(this, 10, 10, xx, yy);

	// Combo box for selecting Color Groups
	whichColorGroup = new Fl_Choice(xx+80, yy, 120, 30, "Color Group:");

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

	SetActiveComponent(NULL);
}

void ColorPanel::SetActiveComponent(ModelComponent* comp)
{
	if (comp)
	{
		activeComponent = comp;

		float* color = activeComponent->color;

		// Update both color chips to be the same color
		activeColorChip->color(fl_rgb_color((uchar)(color[0]*255), (uchar)(color[1]*255), (uchar)(color[2]*255)));
		resetColorChip->color(fl_rgb_color((uchar)(color[0]*255), (uchar)(color[1]*255), (uchar)(color[2]*255)));
		resetColor[0] = color[0];
		resetColor[1] = color[1];
		resetColor[2] = color[2];

		// Update the color chooser
		colorChooser->rgb(color[0], color[1], color[2]);

		// Update the fixed toggle
		fixedToggle->value(comp->isFixed);

		this->redraw();
		this->show();
	}
	else
	{
		this->hide();
	}
}