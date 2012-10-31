#include "ComponentPanel.h"
#include "FLTKUtils.h"
#include "Scene.h"


void ComponentPanel::AssignedColorGroupCallback(Fl_Widget* w, void* v)
{
	// Assign the currently selected color group to the active component,
	// then redraw
	ComponentPanel* panel = (ComponentPanel*)v;
	int selectedIndex = panel->assignedColorGroup->value();
	const char* selectedName = panel->assignedColorGroup->menu()[selectedIndex].label();
	ColorGroup* cg = panel->scene->colorGroups[selectedName];
	panel->activeComponent->SetColorGroup(cg);
	
	panel->context->Redraw();
}

ComponentPanel::ComponentPanel(Scene* _scene, GraphicsEngine::GraphicsContext* gContext, int x, int y, int w, int h)
	: Fl_Group(x, y, w, h), context(gContext), scene(_scene)
{
	// Setup UI

	int xx, yy;
	fl_window_to_widget(this, 10, 10, xx, yy);

	// Label
	label = new Fl_Box(xx, yy, 200, 30);
	label->align(FL_ALIGN_INSIDE | FL_ALIGN_LEFT);

	// Combo box for selecting Color Groups
	assignedColorGroup = new Fl_Choice(xx+80, fl_below(label, 10), 120, 30, "Assigned To:");
	for (auto it = scene->colorGroups.begin(); it != scene->colorGroups.end(); it++)
	{
		ColorGroup* cg = it->second;
		assignedColorGroup->add(cg->name.c_str(), 0, AssignedColorGroupCallback, this);
	}

	SetActiveComponent(NULL);
}

void ComponentPanel::SetActiveComponent(ModelComponent* comp)
{
	if (comp)
	{
		activeComponent = comp;

		SafePrintf(labelText, "Model %d, Component %d/%d", comp->owner->index, comp->index, (int)comp->owner->components.size());
		label->label(labelText);

		// Make sure the combo box shows the current color group for this component
		assignedColorGroup->value(assignedColorGroup->find_index(comp->colorGroup->name.c_str()));

		this->redraw();
		this->show();
	}
	else
	{
		this->hide();
	}
}

void ComponentPanel::RefreshColorGroupList()
{
	for (auto it = scene->colorGroups.begin(); it != scene->colorGroups.end(); it++)
	{
		ColorGroup* cg = it->second;
		assignedColorGroup->add(cg->name.c_str(), 0, AssignedColorGroupCallback, this);
	}
	assignedColorGroup->value(0);	// Initially, show the first item
}