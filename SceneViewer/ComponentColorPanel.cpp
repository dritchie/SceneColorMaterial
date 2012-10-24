#include "ComponentColorPanel.h"
#include "FL/Fl_Color_Chooser.H"
#include "FL/Fl.H"
#include "FLTKUtils.h"
#include "Eigen/Core"

void ComponentColorPanel::SetActiveColor(float r, float g, float b)
{
	// Set the color chooser color (this may be a no op, if this function is being called
	// in response to the color chooser callback)
	colorChooser->rgb(r, g, b);

	// Copy the color into the active color chip color
	activeColorChip->color(fl_rgb_color((uchar)(r*255), (uchar)(g*255), (uchar)(b*255)));
	this->redraw();

	// Update the actual material color
	activeComponent->material->color[0] = r;
	activeComponent->material->color[1] = g;
	activeComponent->material->color[2] = b;

	// Tell the GL window to redraw
	context->Redraw();
}

void ComponentColorPanel::ColorChooserCallback(Fl_Widget* w, void* v)
{
	ComponentColorPanel* panel = (ComponentColorPanel*)v;

	// Copy the color chooser color into the active color chip color
	float r = (float)panel->colorChooser->r();
	float g = (float)panel->colorChooser->g();
	float b = (float)panel->colorChooser->b();
	panel->SetActiveColor(r, g, b);
}

void ComponentColorPanel::ResetButtonCallback(Fl_Widget* w, void* v)
{
	ComponentColorPanel* panel = (ComponentColorPanel*)v;
	
	// Reset the color of the active chip
	uchar r, g, b;
	Fl::get_color(panel->resetColorChip->color(), r, g, b);
	panel->SetActiveColor(r/255.0f, g/255.0f, b/255.0f);
}

ComponentColorPanel::ComponentColorPanel(GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h)
	: Fl_Group(x, y, w, h), context(gContext)
{
	// Setup UI

	// Label
	int xx, yy;
	fl_window_to_widget(this, 10, 10, xx, yy);
	label = new Fl_Box(xx, yy, this->w(), 20, "");
	label->align(FL_ALIGN_INSIDE | FL_ALIGN_LEFT);

	// Color chips
	activeColorChip = new Fl_Button(xx, label->y() + label->h() + 10, 95, 40);
	resetColorChip = new Fl_Button(xx + activeColorChip->w() + 10, label->y() + label->h() + 10, 95, 40);

	// Color chooser
	colorChooser = new Fl_Color_Chooser(xx, activeColorChip->y() + activeColorChip->h() + 10, 200, 95);		// Recommended dimensions
	colorChooser->callback(ColorChooserCallback, this);

	// Reset button
	resetButton = new Fl_Button(xx, colorChooser->y() + colorChooser->h() + 10, 200, 40);
	resetButton->label("Reset");
	resetButton->callback(ResetButtonCallback, this);

	SetActiveComponent(NULL, 0);
}

void ComponentColorPanel::SetActiveComponent(UTF8Model* model, int compIndex)
{
	if (model)
	{
		activeComponent = &model->components[compIndex];

		// Update the text label
		SafePrintf(labelText, "Model %d, Component %d", model->index, compIndex);
		label->label(labelText);

		const Eigen::Vector3f& color = activeComponent->material->color;

		// Update both color chips to be the same color
		activeColorChip->color(fl_rgb_color((uchar)(color[0]*255), (uchar)(color[1]*255), (uchar)(color[2]*255)));
		resetColorChip->color(fl_rgb_color((uchar)(color[0]*255), (uchar)(color[1]*255), (uchar)(color[2]*255)));

		// Update the color chooser
		colorChooser->rgb(color[0], color[1], color[2]);

		this->redraw();
		this->show();
	}
	else
	{
		this->hide();
	}
}