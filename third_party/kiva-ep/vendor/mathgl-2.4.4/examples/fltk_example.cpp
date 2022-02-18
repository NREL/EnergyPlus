/***************************************************************************
 * fltk_example.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include "mgl2/fltk.h"
//-----------------------------------------------------------------------------
#if defined(WIN32) || defined(_MSC_VER) || defined(__BORLANDC__)
#include <windows.h>
#else
#include <unistd.h>
#endif
void long_calculations()	// just delay which correspond to simulate calculations
{
#if defined(WIN32) || defined(_MSC_VER) || defined(__BORLANDC__)
	Sleep(1000);
#else
	sleep(1);	// which can be very long
#endif
}
//-----------------------------------------------------------------------------
#if defined(PTHREAD_SAMPLE1)	// first variant of multi-threading usage of mglFLTK window
mglFLTK *gr=NULL;
void *calc(void *)
{
	mglPoint pnt;
	for(int i=0;i<10;i++)		// do calculation
	{
		long_calculations();	// which can be very long
		pnt.Set(2*mgl_rnd()-1,2*mgl_rnd()-1);
		if(gr)
		{
			gr->Clf();			// make new drawing
			gr->Line(mglPoint(),pnt,"Ar2");
			char str[10] = "i=0";	str[2] = '0'+i;
			gr->Puts(mglPoint(),str);
			gr->Update();		// update window
		}
	}
	exit(0);
}
int main(int argc,char **argv)
{
	mgl_textdomain(argv?argv[0]:NULL);
	static pthread_t thr;
	pthread_create(&thr,0,calc,0);
	pthread_detach(thr);
	gr = new mglFLTK;
	gr->Run();	return 0;
}
#elif defined(PTHREAD_SAMPLE2)	// another variant of multi-threading usage of mglFLTK window. Work only if pthread was enabled for MathGL
mglPoint pnt;	// some global variable for changeable data
int main(int argc,char **argv)
{
	mgl_textdomain(argv?argv[0]:NULL);
	mglFLTK gr("test");
	gr.RunThr();	// <-- need MathGL version which use pthread
	for(int i=0;i<10;i++)	// do calculation
	{
		long_calculations();// which can be very long
		pnt.Set(2*mgl_rnd()-1,2*mgl_rnd()-1);
		gr.Clf();			// make new drawing
		gr.Line(mglPoint(),pnt,"Ar2");
		char str[10] = "i=0";	str[3] = '0'+i;
		gr->Puts(mglPoint(),str);
		gr.Update();		// update window
	}
	return 0;	// finish calculations and close the window
}
#else		// just default samples
//-----------------------------------------------------------------------------
int test_wnd(mglGraph *gr);
int sample(mglGraph *gr);
int sample_1(mglGraph *gr);
int sample_2(mglGraph *gr);
int sample_3(mglGraph *gr);
int sample_d(mglGraph *gr);
//-----------------------------------------------------------------------------
#if MGL_HAVE_PTHR_WIDGET
class myDraw : public mglDraw
{
	mglPoint pnt;	// some variable for changeable data
	long i;			// another variable to be shown
	mglWnd *wnd;	// external window for plotting
public:
	myDraw(mglWnd *w=0) : mglDraw()	{	i=0;	wnd=w;	}
	void SetWnd(mglWnd *w)	{	wnd=w;	}
	int Draw(mglGraph *gr)
	{
		gr->Line(mglPoint(),pnt,"Ar2");
		char str[16];	snprintf(str,15,"i=%ld",i);
		gr->Puts(mglPoint(),str);
		return 0;
	}
	void Calc()
	{
		for(i=0;;i++)	// do calculation
		{
			Check();	// check if need pause
			long_calculations();// which can be very long
			pnt.Set(2*mgl_rnd()-1,2*mgl_rnd()-1);
			if(wnd)	wnd->Update();
		}
	}
} dr;
#endif
//-----------------------------------------------------------------------------
int main(int argc,char **argv)
{
	mgl_textdomain(argv?argv[0]:NULL,"");
	mglFLTK *gr;
	char key = 0;
	if(argc>1)	key = argv[1][0]!='-' ? argv[1][0]:argv[1][1];
	else	printf("You may specify argument '1', '2', '3', 'd' for viewing examples of 1d, 2d, 3d, dual plotting,\nor 'm' for multi-threading sample.\n");
	switch(key)
	{
	case '0':	gr = new mglFLTK((mglDraw *)NULL,"1D plots");
				gr->Rotate(40,60);	gr->Box();	gr->Light(true);
				gr->FSurf("sin(4*pi*x*y)");	gr->Update();	break;
	case '1':	gr = new mglFLTK(sample_1,"1D plots");	break;
	case '2':	gr = new mglFLTK(sample_2,"2D plots");	break;
	case '3':	gr = new mglFLTK(sample_3,"3D plots");	break;
	case 'd':	gr = new mglFLTK(sample_d,"Dual plots");break;
	case 't':	gr = new mglFLTK(test_wnd,"Testing");	break;
	case 'f':	gr = new mglFLTK("Frame drawing");
				gr->NewFrame();	gr->Box();	gr->EndFrame();	break;
#if MGL_HAVE_PTHR_WIDGET
	case 'm':	gr = new mglFLTK(&dr,"Multi-threading test");
	dr.SetWnd(gr);	dr.Run();	break;
#endif
	default:	gr = new mglFLTK(sample,"Drop and waves");	break;
	}
	gr->Run();	return 0;
}
#endif
//-----------------------------------------------------------------------------
