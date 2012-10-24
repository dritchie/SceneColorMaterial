#pragma once

#ifndef __RENDER_OPTIONS_H
#define __RENDER_OPTIONS_H

namespace GraphicsEngine
{
	class GraphicsAppParams;
}

struct RenderOptions
{
	GraphicsEngine::GraphicsAppParams* params;

	enum DisplayType
	{
		FLAT = 0,
		SHADED
	};
	DisplayType displayType;

	bool showObjects;
	bool showConstraints;
	bool highlightFixed;

	RenderOptions() :
			params(NULL),
			displayType(SHADED),
			showObjects(true),
			showConstraints(true),
			highlightFixed(false)
	{}
};


#endif