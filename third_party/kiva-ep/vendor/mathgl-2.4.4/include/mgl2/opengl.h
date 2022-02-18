/***************************************************************************
 * opengl.h is part of Math Graphic Library
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
#ifndef MGL_CANVAS_GL_H
#define MGL_CANVAS_GL_H
//-----------------------------------------------------------------------------
#include "mgl2/canvas.h"
//-----------------------------------------------------------------------------
class MGL_EXPORT mglCanvasGL : public mglCanvas
{
public:
	mglCanvasGL();
	virtual ~mglCanvasGL();

	void SetQuality(int =0)	{	Quality=2;	}
	void Finish();
	void SetSize(int ,int ,bool clf=true)	{	if(clf)	Clf();	}
	void View(mreal tetX,mreal tetY,mreal tetZ);
	void Zoom(mreal x1, mreal y1, mreal x2, mreal y2);
/*	int NewFrame();
	void EndFrame();
	void DelFrame(long ){}*/

	bool Alpha(bool enable);
	void Fog(mreal d, mreal dz=0.25);
	bool Light(bool enable);
	void Light(int n, bool enable);
	void AddLight(int n,mglPoint r,mglPoint d, char c='w', mreal bright=0.5, mreal ap=0);
	void Clf(mglColor Back=NC);
	void Clf(const char *col);

protected:
	// provide fastest variant for usual points (not glyphs or marks)
	void line_draw(long n1, long n2);
	void trig_draw(long n1, long n2, long n3);
	void quad_draw(long n1, long n2, long n3, long n4);
	// variant for glyphs or marks
	void line_draw(const mglPnt &p1, const mglPnt &p2, const mglDrawReg *d);
	void trig_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, bool anorm, const mglDrawReg *d);
	void quad_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4, const mglDrawReg *d);
	void pnt_draw(const mglPnt &p, const mglDrawReg *d);
	void mark_draw(const mglPnt &q, char type, mreal size, mglDrawReg *d);
	void glyph_fill(mreal phi, const mglPnt &p, mreal f, const mglGlyph &g, const mglDrawReg *d);
	
	unsigned char **GetRGBLines(long &w, long &h, unsigned char *&f, bool solid=true);
	void LightScale(const mglMatrix *M);

	void gl_clf(mglColor Back=WC);
};
//-----------------------------------------------------------------------------
#endif

