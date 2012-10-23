#include "ComponentColorPanel.h"
#include "UTF8Model.h"
#include "Eigen/Core"

void ComponentColorPanel::ColorButtonCallback(Fl_Widget* w, void* v)
{
	ComponentColorPanel* panel = (ComponentColorPanel*)v;
	// TODO: Launch the color picker window
}

ComponentColorPanel::ComponentColorPanel(int x, int y, int w, int h)
	: Fl_Pack(x, y, w, h)
{
	// Setup UI

	colorButton = new Fl_Button(0, 0, 100, 40);

	SetActiveComponent(NULL);
}

void ComponentColorPanel::SetActiveComponent(UTF8ModelComponent* comp)
{
	if (comp)
	{
		activeComponent = comp;
		const Eigen::Vector3f& color = activeComponent->material->color;
		unsigned int flColor = fl_rgb_color((unsigned char)(color[0]*255),
			                                (unsigned char)(color[1]*255),
											(unsigned char)(color[2]*255));
		colorButton->color(flColor);

		this->redraw();
		this->show();
	}
	else
	{
		this->hide();
	}
}