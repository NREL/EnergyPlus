/***************************************************************************
 * glut.h is part of Math Graphic Library
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
//-----------------------------------------------------------------------------
#ifndef _MGL_GLUT_H_
#define _MGL_GLUT_H_
#ifdef __cplusplus
#include <mgl2/wnd.h>
//-----------------------------------------------------------------------------
extern "C" {
#endif
void _mgl_key_up(unsigned char ch,int ,int );
HMGL MGL_EXPORT mgl_create_graph_glut(int (*draw)(HMGL gr, void *p), const char *title, void *par, void (*load)(void *p));


/// Switch on/off transparency (do not overwrite user settings)
void MGL_EXPORT mgl_glut_toggle_alpha(HMGL gr);
/// Switch on/off lighting (do not overwrite user settings)
void MGL_EXPORT mgl_glut_toggle_light(HMGL gr);
/// Switch off all zooming and rotation
void MGL_EXPORT mgl_glut_toggle_no(HMGL gr);
/// Update picture by calling user drawing function
void MGL_EXPORT mgl_glut_update(HMGL gr);
/// Reload user data and update picture
void MGL_EXPORT mgl_glut_reload(HMGL gr);
/// Show next frame (if one)
void MGL_EXPORT mgl_glut_next_frame(HMGL gr);
/// Show previous frame (if one)
void MGL_EXPORT mgl_glut_prev_frame(HMGL gr);
/// Run slideshow (animation) of frames
void MGL_EXPORT mgl_glut_animation(HMGL gr);

#ifdef __cplusplus
}
//-----------------------------------------------------------------------------
class MGL_EXPORT mglGLUT: public mglGraph
{
	mglGLUT(const mglGLUT &) {}	// copying is not allowed
	const mglGLUT &operator=(const mglGLUT &t)	{	return t;	}
public:
	mglGLUT(int (*draw)(HMGL gr, void *p), const char *title="MathGL", void *par=0, void (*load)(void *p)=0) : mglGraph(-1)
	{	gr = mgl_create_graph_glut(draw,title,par,load);	}
	mglGLUT(int (*draw)(mglGraph *gr), const char *title="MathGL") : mglGraph(-1)
	{	gr = mgl_create_graph_glut(draw?mgl_draw_graph:0,title,(void*)draw,0);	}
	mglGLUT(mglDraw *draw=0, const char *title="MathGL") : mglGraph(-1)
	{	gr = mgl_create_graph_glut(draw?mgl_draw_class:0,title,draw,mgl_reload_class);	}
    virtual ~mglGLUT() {}

	inline void ToggleAlpha()	///< Switch on/off transparency (do not overwrite user settings)
	{	mgl_glut_toggle_alpha(gr);	}
	inline void ToggleLight()	///< Switch on/off lighting (do not overwrite user settings)
	{	mgl_glut_toggle_light(gr);	}
	inline void ToggleNo()		///< Switch off all zooming and rotation
	{	mgl_glut_toggle_no(gr);	}
	inline void Update()		///< Update picture by calling user drawing function
	{	mgl_glut_update(gr);	}
	inline void ReLoad()		///< Reload user data and update picture
	{	mgl_glut_reload(gr);	}
	inline void NextFrame()		///< Show next frame (if one)
	{	mgl_glut_next_frame(gr);	}
	inline void PrevFrame()		///< Show previous frame (if one)
	{	mgl_glut_prev_frame(gr);	}
	inline void Animation()		///< Run slideshow (animation) of frames
	{	mgl_glut_animation(gr);	}
	inline int Run() {return 0;};		///< Run main loop for event handling (placed for similarity to mglWnd)
};
//-----------------------------------------------------------------------------
#endif
#endif
