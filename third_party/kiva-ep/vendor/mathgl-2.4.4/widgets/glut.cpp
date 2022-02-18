/***************************************************************************
 * glut.cpp is part of Math Graphic Library
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
#define GLUT_NO_LIB_PRAGMA
#define GLUT_NO_WARNING_DISABLE

#ifdef __APPLE__
	#include <OpenGL/gl.h>
	#include <GLUT/glut.h>
#else
	#if defined(_MSC_VER) || defined(__BORLANDC__)
		#include <windows.h>
		#include <GL/gl.h>
		#include "glut.h"
	#else
		#include <GL/gl.h>
		#include <GL/glut.h>
	#endif
#endif

#include "mgl2/opengl.h"
#include "mgl2/glut.h"

void _mgl_key_up(unsigned char ch,int ,int );
//-----------------------------------------------------------------------------
/// Class allows the window creation under OpenGL with the help of GLUT library
class mglCanvasGLUT : public mglCanvasGL
{
friend void _mgl_display();
friend void _mgl_key_up(unsigned char ch,int ,int );
friend void _mgl_timer(int);
public:
	mglCanvasGLUT();
	virtual ~mglCanvasGLUT();
	/// Create a window for plotting. Now implemeted only for GLUT.
	void Window(int argc, char **argv, int (*draw)(mglBase *gr, void *p),
						const char *title,void *par=NULL,
			   			void (*reload)(void *p)=NULL, bool maximize=false);
	void Window(int argc, char **argv, int (*draw)(mglGraph *gr),
				const char *title, bool maximize=false)
	{	Window(argc,argv,draw?mgl_draw_graph:0,title,(void*)draw,0,maximize);	}
	/// Create a window for plotting based on class mglDraw.
	void Window(int argc, char **argv, const char *title, mglDraw *draw, bool maximize=false)
	{	Window(argc, argv, draw?mgl_draw_class:0, title, draw, mgl_reload_class, maximize);	}
	/// Switch on/off transparency (do not overwrite switches in user drawing function)
	void ToggleAlpha()	{	_mgl_key_up('r',0,0);	}
	/// Switch on/off lighting (do not overwrite switches in user drawing function)
	void ToggleLight()	{	_mgl_key_up('f',0,0);	}
	void ToggleNo()		{	_mgl_key_up('n',0,0);	}	///< Switch off all zooming and rotation
	void Update()		{	_mgl_key_up(' ',0,0);	}	///< Update picture by calling user drawing function
	void ReLoad()		{	_mgl_key_up('[',0,0);	}	///< Reload user data and update picture
	void NextFrame()	{	_mgl_key_up('.',0,0);	}	///< Show next frame (if one)
	void PrevFrame()	{	_mgl_key_up(',',0,0);	}	///< Show previous frame (if one)
	void Animation()	{	_mgl_key_up('m',0,0);	}	///< Run slideshow (animation) of frames
private:
	void (*LoadFunc)(void *par);
	void *FuncPar;		///< Parameters for drawing function mglCanvas::DrawFunc.
	/// Drawing function for window procedure. It should return the number of frames.
	int (*DrawFunc)(mglBase *gr, void *par);
	int NumFig;		///< Number of figures in the list. If 0 then no list and mglCanvas::DrawFunc will called for each drawing.
	int curr_fig;	///< Current figure in the list.
	int tt;			///< Temporal variable
} *_mgl_glwnd;
//-----------------------------------------------------------------------------
void _mgl_timer(int)
{
	if(!_mgl_glwnd)	return;
	if(_mgl_glwnd->tt)
	{
		_mgl_glwnd->curr_fig++;
		if(_mgl_glwnd->curr_fig > _mgl_glwnd->NumFig)
			_mgl_glwnd->curr_fig = 1;
		glutPostRedisplay();
	}
	glutTimerFunc(int(_mgl_glwnd->Delay*1000),_mgl_timer,0);
}
//-----------------------------------------------------------------------------
void _mgl_key_up(unsigned char ch,int ,int )
{
	if(!_mgl_glwnd)	return;
	static bool Alpha=false;
	static bool Light=false;
	static double rL=3,tL=0,pL=0;

	if(ch=='h')
	{
		printf(_("Use 'a', 'd', 'w', 's', 'q', 'e' for changing view angles\n"
		"Use 'j', 'l', 'i', 'k' for changing light angles\n"
		"Use 'u', 'o' for changing distance to light\n"
		"Use 'r' for switching transparency\nUse 'f' for switching lightning\n"
		"Use 'E' for exporting to EPS file\nUse 'S' for exporting to SVG file\n"
		"Use 'J' for exporting to JPEG file\nUse 'P' for exporting to PNG file\n"
		"Use ',', '.' for show other frames\nUse 'm' for view movie\n"
		"Use 'h' for view this text\nUse 'x' for exit\n") );

	}
	if(ch=='w')	_mgl_glwnd->View(-10,0,0);
	if(ch=='s')	_mgl_glwnd->View(10,0,0);
	if(ch=='a')	_mgl_glwnd->View(0,0,-10);
	if(ch=='d')	_mgl_glwnd->View(0,0,10);
	if(ch=='q')	_mgl_glwnd->View(0,-10,0);
	if(ch=='e')	_mgl_glwnd->View(0,10,0);
	if(ch=='n')	_mgl_glwnd->Restore();
	if(ch==',')
		_mgl_glwnd->curr_fig = _mgl_glwnd->curr_fig == 0 ? _mgl_glwnd->NumFig-1 : _mgl_glwnd->curr_fig-1;
	if(ch=='.')
		_mgl_glwnd->curr_fig = _mgl_glwnd->curr_fig == _mgl_glwnd->NumFig-1 ? 0 : _mgl_glwnd->curr_fig+1;
	if(ch=='r')	Alpha = !Alpha;
	if(ch=='f')	Light = !Light;
	if(ch=='u')	rL += 0.1;
	if(ch=='o')	rL -= 0.1;
	if(ch=='i')	tL += M_PI*0.1;
	if(ch=='k')	tL -= M_PI*0.1;
	if(ch=='l')	pL += 2*M_PI*0.1;
	if(ch=='j')	pL -= 2*M_PI*0.1;
	if(ch=='[' && _mgl_glwnd->LoadFunc)
	{
		glDeleteLists(1,_mgl_glwnd->NumFig);
		_mgl_glwnd->LoadFunc(_mgl_glwnd->FuncPar);
		if(_mgl_glwnd->DrawFunc)
		{
			_mgl_glwnd->ResetFrames();
			(_mgl_glwnd->DrawFunc)(_mgl_glwnd,_mgl_glwnd->FuncPar);
		}
		_mgl_glwnd->Finish();
	}
	if(ch=='P')
	{
		char str[128];
		snprintf(str,128,"%s_%d.png",_mgl_glwnd->PlotId.c_str(),_mgl_glwnd->curr_fig);
		str[127]=0;	mgl_write_png(_mgl_glwnd, str, "MathGL");
	}
	if(ch=='J')
	{
		char str[128];
		snprintf(str,128,"%s_%d.jpg",_mgl_glwnd->PlotId.c_str(),_mgl_glwnd->curr_fig);
		str[127]=0;	mgl_write_jpg(_mgl_glwnd, str, "MathGL");
	}
	if(ch=='E')
	{
		char str[128];
		snprintf(str,128,"%s_%d.eps",_mgl_glwnd->PlotId.c_str(),_mgl_glwnd->curr_fig);
		str[127]=0;	mgl_write_eps(_mgl_glwnd, str, "MathGL");
	}
	if(ch=='S')
	{
		char str[128];
		snprintf(str,128,"%s_%d.svg",_mgl_glwnd->PlotId.c_str(),_mgl_glwnd->curr_fig);
		str[127]=0;	mgl_write_svg(_mgl_glwnd, str, "MathGL");
	}
	if(ch==' ')	_mgl_glwnd->Clf();
	if(ch=='m')	_mgl_glwnd->tt = 1-_mgl_glwnd->tt;
	rL = rL<0 ? 0 : (rL>5 ? 5 : rL);
	_mgl_glwnd->AddLight(0,mglPoint(rL*cos(pL)*sin(tL), rL*sin(pL)*sin(tL), rL*cos(tL)),false);
	_mgl_glwnd->Alpha(Alpha);
	_mgl_glwnd->Light(Light);
//	glEnable(GL_BLEND);
	if(strchr("ijkl",ch))
		printf("Light: %g, %g, %g\n",rL*cos(pL)*sin(tL), rL*sin(pL)*sin(tL), rL*cos(tL));

	if(ch=='x')	exit(0);
	else		glutPostRedisplay();
}
//-----------------------------------------------------------------------------
void _mgl_display()
{
	if(!_mgl_glwnd)	return;
//	glEnable(GL_LINE_SMOOTH);
//	_mgl_glwnd->CurFrameId = 1;
//	if(_mgl_glwnd->get(MGL_CLF_ON_UPD))
	_mgl_glwnd->Clf();
//	_mgl_glwnd->gl_clf();
	_mgl_glwnd->InPlot(0,1,0,1,false);
	if(_mgl_glwnd->NumFig>0)
	{
		_mgl_glwnd->GetFrame(_mgl_glwnd->curr_fig);
		_mgl_glwnd->Finish();
//		glCallList(_mgl_glwnd->curr_fig);
	}
	else
	{
		if(_mgl_glwnd->DrawFunc)
		{
			_mgl_glwnd->ResetFrames();
			(_mgl_glwnd->DrawFunc)(_mgl_glwnd,_mgl_glwnd->FuncPar);
		}
		_mgl_glwnd->Finish();
	}
	glFinish();
}
//-----------------------------------------------------------------------------
mglCanvasGLUT::~mglCanvasGLUT()	{	_mgl_glwnd = 0;	}
//-----------------------------------------------------------------------------
void mglCanvasGLUT::Window(int argc, char **argv,int (*draw)(mglBase *gr, void *p),const char *title, void *par, void (*reload)(void *p), bool /*maximize*/)
{
	NumFig=0;	curr_fig=1;	tt=0;
	_mgl_glwnd = this;
	CurFrameId = 1;

	char *tmp[1];	tmp[0]=new char[1];	tmp[0][0]=0;
	glutInit(&argc, argv ? argv:tmp);
	delete []tmp[0];
	glutInitDisplayMode(GLUT_RGB);
	glutInitWindowSize(600, 600);
	glutCreateWindow("MathGL");

	AddLight(0,mglPoint(0,0,3),false);
	if(draw)
	{
		NumFig = draw(this,par)-1;	Finish();
		DrawFunc = draw;	FuncPar = par;
	}
	else
	{	NumFig = 0;	DrawFunc=0;	FuncPar=0;	}
	LoadFunc = reload;
	glutSetWindowTitle(title);

	glutDisplayFunc(_mgl_display);
	glutKeyboardUpFunc(_mgl_key_up);
	glutTimerFunc(int(1000*Delay),_mgl_timer,0);

	// TODO Add window maximazing at start up ???

	glutMainLoop();
	if(NumFig>0)	glDeleteLists(1,NumFig);
}
//-----------------------------------------------------------------------------
HMGL MGL_EXPORT mgl_create_graph_glut(int (*draw)(HMGL gr, void *p), const char *title, void *par, void (*load)(void *p))
{
	mglCanvasGLUT *g = new mglCanvasGLUT;
	g->Window(0,0,draw,title,par, load);
	return g;
}
//-----------------------------------------------------------------------------
mglCanvasGLUT::mglCanvasGLUT() : mglCanvasGL()	{}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_glut_toggle_alpha(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->ToggleAlpha();	}
void MGL_EXPORT mgl_glut_toggle_light(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->ToggleLight();	}
void MGL_EXPORT mgl_glut_toggle_no(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->ToggleNo();	}
void MGL_EXPORT mgl_glut_update(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->Update();	}
void MGL_EXPORT mgl_glut_reload(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->ReLoad();	}
void MGL_EXPORT mgl_glut_next_frame(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->NextFrame();	}
void MGL_EXPORT mgl_glut_prev_frame(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->PrevFrame();	}
void MGL_EXPORT mgl_glut_animation(HMGL gr)
{	mglCanvasGLUT *g = dynamic_cast<mglCanvasGLUT*>(gr);
	if(g)	g->Animation();	}
//-----------------------------------------------------------------------------
