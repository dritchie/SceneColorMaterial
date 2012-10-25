#include "ComponentPanel.h"
#include "FLTKUtils.h"


void ComponentPanel::AssignedColorGroupCallback(Fl_Widget* w, void* v)
{
	// TODO: DO STUFF HERE!
}

ComponentPanel::ComponentPanel(GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h)
	: Fl_Group(x, y, w, h), context(gContext)
{
	// Setup UI

	int xx, yy;
	fl_window_to_widget(this, 10, 10, xx, yy);

	// Label
	label = new Fl_Box(xx, yy, 200, 30);
	label->align(FL_ALIGN_INSIDE | FL_ALIGN_LEFT);

	// Combo box for selecting Color Groups
	assignedColorGroup = new Fl_Choice(xx+80, fl_below(label, 10), 120, 30, "Assigned To:");

	SetActiveComponent(NULL);
}

void ComponentPanel::SetActiveComponent(ModelComponent* comp)
{
	if (comp)
	{
		activeComponent = comp;

		SafePrintf(labelText, "Model %d, Component %d/%d", comp->owner->index, comp->index, (int)comp->owner->components.size());
		label->label(labelText);

		// TODO: DO STUFF HERE

		this->redraw();
		this->show();
	}
	else
	{
		this->hide();
	}
}
