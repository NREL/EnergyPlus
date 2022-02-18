/***************************************************************************
 * window.h is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 3 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifndef _MGL_WINDOW_H_
#define _MGL_WINDOW_H_

#include "mgl2/qt.h"
#include "mgl2/fltk.h"
//#include "mgl2/wx.h"
//-----------------------------------------------------------------------------
/// Wrapper class for all windows displaying graphics
class MGL_EXPORT mglWindow : public mglWnd
{
	mglWindow(const mglWindow &t) {}	// copying is not allowed
	const mglWindow &operator=(const mglWindow &t)	{	return t;	}
	int wnd;	///< Type of window
public:
	mglWindow(const char *title="MathGL") : mglWnd()
	{	wnd=1;	gr = mgl_create_graph_fltk(0,title,0,0);	}
	mglWindow(int (*draw)(HMGL gr, void *p), const char *title="MathGL", void *par=NULL, int kind=0, void (*load)(void *p)=0) : mglWnd()
	{
		wnd=kind;
		if(wnd==1)	gr = mgl_create_graph_qt(draw,title,par,load);
		else		gr = mgl_create_graph_fltk(draw,title,par,load);
	}
	mglWindow(int (*draw)(mglGraph *gr), const char *title="MathGL", int kind=0) : mglWnd()
	{
		wnd=kind;
		if(wnd==1)	gr = mgl_create_graph_qt(draw?mgl_draw_graph:0,title,(void*)draw,0);
		else		gr = mgl_create_graph_fltk(draw?mgl_draw_graph:0,title,(void*)draw,0);
	}
	mglWindow(mglDraw *draw, const char *title="MathGL", int kind=0) : mglWnd()
	{
		wnd=kind;
		if(wnd==1)	gr = mgl_create_graph_qt(draw?mgl_draw_class:0,title,draw,mgl_reload_class);
		else		gr = mgl_create_graph_fltk(draw?mgl_draw_class:0,title,draw,mgl_reload_class);
		mgl_set_click_func(gr, mgl_click_class);
	}
	/// Run main loop for event handling
	int Run()	{	return wnd==0? mgl_fltk_run():mgl_qt_run();	}
	/// Run main loop for event handling in separate thread (for FLTK only)
	inline int RunThr()	{	return wnd==0 ? mgl_fltk_thr():0;	}
};
//-----------------------------------------------------------------------------
#endif
