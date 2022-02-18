/***************************************************************************
 * glut_example.cpp is part of Math Graphic Library
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
#include "mgl2/glut.h"
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
#if defined(PTHREAD_SAMPLE)
mglGLUT *gr=NULL;
void *calc(void *)
{
	mglPoint pnt;
	for(int i=0;i<10;i++)	// do calculation
	{
		sleep(1);           // which can be very long
		pnt.Set(2*mgl_rnd()-1,2*mgl_rnd()-1);
printf("i=%d, gr=%p\n",i,gr);	fflush(stdout);
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
	gr = new mglGLUT;
printf("Create gr=%p\n",gr);	fflush(stdout);
	gr->Run();	return 0;
}
#else
int test_wnd(mglGraph *gr);
int sample(mglGraph *gr);
int sample_m(mglGraph *gr);
int sample_1(mglGraph *gr);
int sample_2(mglGraph *gr);
int sample_3(mglGraph *gr);
int sample_d(mglGraph *gr);
//-----------------------------------------------------------------------------
typedef int (*draw_func)(mglGraph *gr);
int main(int argc,char **argv)
{
	mgl_textdomain(argv?argv[0]:NULL,"");
	char key = 0;
	if(argc>1)	key = argv[1][0]!='-' ? argv[1][0] : argv[1][1];
	else	printf("You may specify argument '1', '2', '3' or 'd' for viewing examples of 1d, 2d, 3d or dual plotting\n");
	switch(key)
	{
	case '1':	new mglGLUT(sample_1, "1D plots");	break;
	case '2':	new mglGLUT(sample_2, "2D plots");	break;
	case '3':	new mglGLUT(sample_3, "3D plots");	break;
	case 'd':	new mglGLUT(sample_d, "Dual plots");	break;
	case 't':	new mglGLUT(test_wnd, "Testing");	break;
	default:	new mglGLUT(sample, "Example of molecules");	break;
	}
	return 0;
}
#endif
