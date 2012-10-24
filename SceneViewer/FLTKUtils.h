#pragma once

#ifndef __FLTK_UTILS_H
#define __FLTK_UTILS_H

#include "Common/Common.h"
#include "FL/Fl_Widget.H"

// Convert a coordinate pair from window-relative to widget-relative
__forceinline void fl_window_to_widget(Fl_Widget* w, int x, int y, int& xx, int& yy)
{
	xx = x + w->x();
	yy = y + w->y();
}

// Return x coordinate that is 'sep' to the right of the given widget
__forceinline int fl_right_of(Fl_Widget* w, int sep)
{
	return w->x() + w->w() + sep;
}

// Return y coordinate that is 'sep' below the given widget
__forceinline int fl_below(Fl_Widget* w, int sep)
{
	return w->y() + w->h() + sep;
}

#endif