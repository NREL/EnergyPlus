/***************************************************************************
 * canvas_wnd.h is part of Math Graphic Library
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
#ifndef _MGL_CANVAS_WND_H_
#define _MGL_CANVAS_WND_H_

#include "mgl2/canvas.h"
#include "mgl2/wnd.h"
//-----------------------------------------------------------------------------
/// Base class for windows containing MathGL graphics
class MGL_EXPORT mglCanvasWnd : public mglCanvas
{
public:
	mglPoint LastMousePos;			///< Last mouse position
	void (*ClickFunc)(void *par);	///< Callback function on click
#if MGL_HAVE_PTHR_WIDGET
	pthread_mutex_t *mutex;
#endif

	mglCanvasWnd();
	virtual ~mglCanvasWnd();

	void SetSize(int w,int h,bool clf=true);
	void EndFrame();
	void SetFrame(long i);
	void DelFrame(long i);
	const unsigned char *GetBits();
	inline int GetNumFig() const	{	return NumFig;	}
	inline int GetCurFig() const	{	return CurFig;	}
	void SetCurFig(int c);
	void ResetFrames();
	inline mglPoint GetMousePos() const	{	return LastMousePos;}
	inline void SetMousePos(mglPoint p)	{	LastMousePos=p;	}
	inline void Setup(bool clf_upd=true, bool showpos=false)
	{	set(showpos,MGL_SHOW_POS);	set(clf_upd,MGL_CLF_ON_UPD);
		if(!clf_upd)	ResetFrames();	}

	virtual void ToggleAlpha()=0;	///< Switch on/off transparency (do not overwrite user settings)
	virtual void ToggleLight()=0;	///< Switch on/off lighting (do not overwrite user settings)
	virtual void ToggleZoom()=0;	///< Switch on/off zooming by mouse
	virtual void ToggleRotate()=0;	///< Switch on/off rotation by mouse
	virtual void ToggleNo()=0;		///< Switch off all zooming and rotation
	virtual void Update()=0;		///< Update picture by calling user drawing function
	virtual void Adjust()=0;		///< Adjust size of bitmap to window size
	virtual void GotoFrame(int d)=0;///< Show arbitrary frame (use relative step)
	virtual void NextFrame()	{GotoFrame(+1);}	///< Show next frame (if one)
	virtual void PrevFrame()	{GotoFrame(-1);}	///< Show previous frame (if one)
	virtual void Animation()=0;		///< Run slideshow (animation) of frames
	
	virtual void *Window()=0;		///< Return pointer to widget (Fl_Window* or QMainWindow*) used for plotting
	virtual void *Widget()=0;		///< Return pointer to widget (Fl_MGLView* or QMathGL*) used for plotting
	virtual void WndSize(int w, int h)=0;	///< Resize window
	virtual void WndMove(int x, int y)=0;	///< Move window
	
	void ReLoad();					///< Reload user data and update picture
	/// Create a window for plotting based on callback function (can be NULL).
	virtual void Window(int argc, char **argv, int (*draw)(mglBase *gr, void *p),
						const char *title, void *par=NULL,
						void (*reload)(void *p)=NULL, bool maximize=false)=0;
	void SetDrawFunc(int (*draw)(mglBase *gr, void *p), void *par=NULL, void (*reload)(void *p)=NULL);
	/// Set callback function for properties setup
	void SetPropFunc(void (*prop)(char id, const char *val, void *p), void *par=NULL)
	{	PropFunc = prop;	PropPar = par;	}
	inline void SetParam(char id, const char *val)	///< Set parameter (usually from custom dialog)
	{	if(PropFunc)	{	PropFunc(id,val,PropPar);	Update();	}	}
	///< Make custom dialog
	virtual void MakeDialog(const char *ids, char const * const *args, const char *title="")=0;

private:
	int CurFig;			///< Current figure in the list.

	unsigned char *GG;	///< images for all frames (may be too LARGE !!!)
	int NumFig;			///< Number of figures in the list. If 0 then no list and mglCanvas::DrawFunc will called for each drawing.
	void (*LoadFunc)(void *par);
	void *FuncPar;		///< Parameters for drawing function mglCanvas::DrawFunc.
	/// Drawing function for window procedure. It should return the number of frames.
	int (*DrawFunc)(mglBase *gr, void *par);
	void *PropPar;	///< Parameters for prop_func().
	/// Function for setting properties.
	void (*PropFunc)(char id, const char *val, void *par);
};
//-----------------------------------------------------------------------------
#endif
