#include "ComponentColorPanel.h"
#include "FL/Fl_Color_Chooser.H"
#include "FLTKUtils.h"
#include "Eigen/Core"

void ComponentColorPanel::ColorChooserCallback(Fl_Widget* w, void* v)
{
	ComponentColorPanel* panel = (ComponentColorPanel*)v;

	// Copy the color chooser color into the color chip color
	double r = panel->colorChooser->r();
	double g = panel->colorChooser->g();
	double b = panel->colorChooser->b();
	panel->colorChip->color(fl_rgb_color((uchar)(r*255), (uchar)(g*255), (uchar)(b*255)));
	panel->redraw();

	// Update the actual material color
	panel->activeComponent->material->color[0] = (float)r;
	panel->activeComponent->material->color[1] = (float)g;
	panel->activeComponent->material->color[2] = (float)b;

	// Tell the GL window to redraw
	panel->context->Redraw();
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

	// Color chip
	colorChip = new Fl_Button(xx, label->y() + label->h() + 10, 200, 40);

	// Color chooser
	colorChooser = new Fl_Color_Chooser(xx, colorChip->y() + colorChip->h() + 10, 200, 95);		// Recommended dimensions
	colorChooser->callback(ColorChooserCallback, this);

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

		// Update the color chip
		colorChip->color(fl_rgb_color((uchar)(color[0]*255), (uchar)(color[1]*255), (uchar)(color[2]*255)));

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