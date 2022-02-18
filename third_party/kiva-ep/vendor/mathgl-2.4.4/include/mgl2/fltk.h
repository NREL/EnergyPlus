/***************************************************************************
 * fltk.h is part of Math Graphic Library
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
#ifndef _MGL_FLTK_H_
#define _MGL_FLTK_H_

#include <mgl2/abstract.h>
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif
/// Creates FLTK window for plotting
HMGL MGL_EXPORT mgl_create_graph_fltk(int (*draw)(HMGL gr, void *p), const char *title, void *par, void (*load)(void *p));
uintptr_t MGL_EXPORT mgl_create_graph_fltk_(const char *title, int);
/// Run main FLTK loop for event handling.
int MGL_EXPORT mgl_fltk_run();
int MGL_EXPORT mgl_fltk_run_();
/// Run main FLTK loop for event handling in separate thread.
int MGL_EXPORT mgl_fltk_thr();
/// FLTK function for asking user.
void MGL_EXPORT mgl_ask_fltk(const wchar_t *quest, wchar_t *res);
/// FLTK function for displaying progress of something.
void MGL_EXPORT mgl_progress_fltk(int value, int maximal, HMGL gr);
/// Return pointer to widget (Fl_MGLView*) used for plotting
MGL_EXPORT_PURE void *mgl_fltk_widget(HMGL gr);
#ifdef __cplusplus
}
//-----------------------------------------------------------------------------
#include <mgl2/wnd.h>
//-----------------------------------------------------------------------------
/// Wrapper class for windows displaying graphics
class MGL_EXPORT mglFLTK : public mglWnd
{
	mglFLTK(const mglFLTK &) {}	// copying is not allowed
	const mglFLTK &operator=(const mglFLTK &t)	{	return t;	}
public:
	mglFLTK(const char *title="MathGL") : mglWnd()
	{	gr = mgl_create_graph_fltk(0,title,0,0);	}
	mglFLTK(int (*draw)(HMGL gr, void *p), const char *title="MathGL", void *par=NULL, void (*load)(void *p)=0) : mglWnd()
	{	gr = mgl_create_graph_fltk(draw,title,par,load);	}
	mglFLTK(int (*draw)(mglGraph *gr), const char *title="MathGL") : mglWnd()
	{	gr = mgl_create_graph_fltk(draw?mgl_draw_graph:0,title,(void*)draw,0);	}
	mglFLTK(mglDraw *draw, const char *title="MathGL") : mglWnd()
	{	gr = mgl_create_graph_fltk(draw?mgl_draw_class:0,title,draw,mgl_reload_class);
#if MGL_HAVE_PTHR_WIDGET
		mgl_wnd_set_mutex(gr, &(draw->mutex));
#endif
		mgl_set_click_func(gr, mgl_click_class);	}
    virtual ~mglFLTK() {}
	int Run()	{	return mgl_fltk_run();	}	///< Run main loop for event handling
	int RunThr()	{	return mgl_fltk_thr();	}	///< Run main loop for event handling in separate thread
};
//-----------------------------------------------------------------------------
#endif
#endif
