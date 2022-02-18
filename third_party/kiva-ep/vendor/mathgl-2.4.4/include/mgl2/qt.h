/***************************************************************************
 * qt.h is part of Math Graphic Library
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
#ifndef _MGL_QT_H_
#define _MGL_QT_H_
#include <mgl2/abstract.h>
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif
/// Creates Qt window for plotting
HMGL MGL_EXPORT mgl_create_graph_qt(int (*draw)(HMGL gr, void *p), const char *title, void *par, void (*load)(void *p));
uintptr_t MGL_EXPORT mgl_create_graph_qt_(const char *title, int);
/// Run main Qt loop for event handling.
int MGL_EXPORT mgl_qt_run();
int MGL_EXPORT mgl_qt_run_();
/// Return pointer to widget (QMathGL*) used for plotting
MGL_EXPORT_PURE void *mgl_qt_widget(HMGL gr);
#ifdef __cplusplus
}
//-----------------------------------------------------------------------------
#include <mgl2/wnd.h>
//-----------------------------------------------------------------------------
/// Wrapper class for windows displaying graphics
class MGL_EXPORT mglQT : public mglWnd
{
	mglQT(const mglQT &) {}	// copying is not allowed
	const mglQT &operator=(const mglQT &t)	{	return t;	}
public:
	mglQT(const char *title="MathGL") : mglWnd()
	{	gr = mgl_create_graph_qt(0,title,0,0);	}
	mglQT(int (*draw)(HMGL gr, void *p), const char *title="MathGL", void *par=NULL, void (*load)(void *p)=0) : mglWnd()
	{	gr = mgl_create_graph_qt(draw,title,par,load);	}
	mglQT(int (*draw)(mglGraph *gr), const char *title="MathGL") : mglWnd()
	{	gr = mgl_create_graph_qt(draw?mgl_draw_graph:0,title,(void*)draw,0);	}
	mglQT(mglDraw *draw, const char *title="MathGL") : mglWnd()
	{	gr = mgl_create_graph_qt(draw?mgl_draw_class:0,title,draw,mgl_reload_class);
		mgl_set_click_func(gr, mgl_click_class);	}
	virtual ~mglQT() {}
	int Run()	{	return mgl_qt_run();	}	///< Run main loop for event handling
};
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_ask_qt(const wchar_t *quest, wchar_t *res);
//-----------------------------------------------------------------------------
#endif
#endif
