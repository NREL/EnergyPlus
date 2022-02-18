/***************************************************************************
 * samples.cpp is part of Math Graphic Library
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
#include "mgl2/mgl.h"
//-----------------------------------------------------------------------------
void mgls_prepare1d(mglData *y, mglData *y1=0, mglData *y2=0, mglData *x1=0, mglData *x2=0);
void mgls_prepare2d(mglData *a, mglData *b=0, mglData *v=0);
void mgls_prepare3d(mglData *a, mglData *b=0);
void mgls_prepare2v(mglData *a, mglData *b);
void mgls_prepare3v(mglData *ex, mglData *ey, mglData *ez);
extern int big;
//-----------------------------------------------------------------------------
struct mglSample	/// Structure for list of samples
{
	const char *name;
	void (*func)(mglGraph*);
	const char *mgl;
	const char *info;
};
//-----------------------------------------------------------------------------
//		MGL functions for preparing data
//-----------------------------------------------------------------------------
const char *mmgl_dat_prepare = "\nfunc 'prepare1d'\n\
new y 50 3\nmodify y '0.7*sin(2*pi*x)+0.5*cos(3*pi*x)+0.2*sin(pi*x)'\n\
modify y 'sin(2*pi*x)' 1\nmodify y 'cos(2*pi*x)' 2\n\
new x1 50 'x'\nnew x2 50 '0.05-0.03*cos(pi*x)'\n\
new y1 50 '0.5-0.3*cos(pi*x)'\nnew y2 50 '-0.3*sin(pi*x)'\n\
return\n\nfunc 'prepare2d'\n\
new a 50 40 '0.6*sin(pi*(x+1))*sin(1.5*pi*(y+1))+0.4*cos(0.75*pi*(x+1)*(y+1))'\n\
new b 50 40 '0.6*cos(pi*(x+1))*cos(1.5*pi*(y+1))+0.4*cos(0.75*pi*(x+1)*(y+1))'\n\
return\n\nfunc 'prepare3d'\n\
new c 61 50 40 '-2*(x^2+y^2+z^4-z^2)+0.2'\n\
new d 61 50 40 '1-2*tanh((x+y)*(x+y))'\n\
return\n\nfunc 'prepare2v'\n\
new a 20 30 '0.6*sin(pi*(x+1))*sin(1.5*pi*(y+1))+0.4*cos(0.75*pi*(x+1)*(y+1))'\n\
new b 20 30 '0.6*cos(pi*(x+1))*cos(1.5*pi*(y+1))+0.4*cos(0.75*pi*(x+1)*(y+1))'\n\
return\n\nfunc 'prepare3v'\n\
define $1 pow(x*x+y*y+(z-0.3)*(z-0.3)+0.03,1.5)\n\
define $2 pow(x*x+y*y+(z+0.3)*(z+0.3)+0.03,1.5)\n\
new ex 10 10 10 '0.2*x/$1-0.2*x/$2'\n\
new ey 10 10 10 '0.2*y/$1-0.2*y/$2'\n\
new ez 10 10 10 '0.2*(z-0.3)/$1-0.2*(z+0.3)/$2'\nreturn";
//-----------------------------------------------------------------------------
//		Sample functions (v.2.*)
//-----------------------------------------------------------------------------
const char *mmgl_refill="new x 10 '0.5+rnd':cumsum x 'x':norm x -1 1\ncopy y sin(pi*x)/1.5\n"
"subplot 2 2 0 '<_':title 'Refill sample'\nbox:axis:plot x y 'o ':fplot 'sin(pi*x)/1.5' 'B:'\n"
"new r 100:refill r x y:plot r 'r'\n\n"
"subplot 2 2 1 '<_':title 'Global spline'\nbox:axis:plot x y 'o ':fplot 'sin(pi*x)/1.5' 'B:'\n"
"new r 100:gspline r x y:plot r 'r'\n\nnew y 10 '0.5+rnd':cumsum y 'x':norm y -1 1\n"
"copy xx x:extend xx 10\ncopy yy y:extend yy 10:transpose yy\ncopy z sin(pi*xx*yy)/1.5\n"
"alpha on:light on\n"
"subplot 2 2 2:title '2d regular':rotate 40 60\nbox:axis:mesh xx yy z 'k'\n"
"new rr 100 100:refill rr x y z:surf rr\n\n"
"new xx 10 10 '(x+1)/2*cos(y*pi/2-1)':new yy 10 10 '(x+1)/2*sin(y*pi/2-1)'\ncopy z sin(pi*xx*yy)/1.5\n"
"subplot 2 2 3:title '2d non-regular':rotate 40 60\nbox:axis:plot xx yy z 'ko '\n"
"new rr 100 100:refill rr xx yy z:surf rr";
void smgl_refill(mglGraph *gr)
{
	mglData x(10), y(10), r(100);
	x.Modify("0.5+rnd");	x.CumSum("x");	x.Norm(-1,1);
	y.Modify("sin(pi*v)/1.5",x);
	if(big!=3)	{	gr->SubPlot(2,2,0,"<_");	gr->Title("Refill sample");	}
	gr->Axis();	gr->Box();	gr->Plot(x,y,"o ");
	gr->Refill(r,x,y);	// or you can use r.Refill(x,y,-1,1);
	gr->Plot(r,"r");	gr->FPlot("sin(pi*x)/1.5","B:");
	if(big==3)	return;
	gr->SubPlot(2,2,1,"<_");	gr->Title("Global spline");
	gr->Axis();	gr->Box();	gr->Plot(x,y,"o ");
	r.RefillGS(x,y,-1,1);	gr->Plot(r,"r");
	gr->FPlot("sin(pi*x)/1.5","B:");

	gr->Alpha(true);	gr->Light(true);
	mglData z(10,10), xx(10,10), yy(10,10), rr(100,100);
	y.Modify("0.5+rnd");	y.CumSum("x");	y.Norm(-1,1);
	for(int i=0;i<10;i++)	for(int j=0;j<10;j++)
		z.a[i+10*j] = sin(M_PI*x.a[i]*y.a[j])/1.5;
	gr->SubPlot(2,2,2);	gr->Title("2d regular");	gr->Rotate(40,60);
	gr->Axis();	gr->Box();	gr->Mesh(x,y,z,"k");
	gr->Refill(rr,x,y,z);	gr->Surf(rr);

	gr->Fill(xx,"(x+1)/2*cos(y*pi/2-1)");
	gr->Fill(yy,"(x+1)/2*sin(y*pi/2-1)");
	for(int i=0;i<10*10;i++)
		z.a[i] = sin(M_PI*xx.a[i]*yy.a[i])/1.5;
	gr->SubPlot(2,2,3);	gr->Title("2d non-regular");	gr->Rotate(40,60);
	gr->Axis();	gr->Box();	gr->Plot(xx,yy,z,"ko ");
	gr->Refill(rr,xx,yy,z);	gr->Surf(rr);
}
//-----------------------------------------------------------------------------
const char *mmgl_indirect="subplot 1 1 0 '':title 'SubData vs Evaluate'\n"
"new in 9 'x^3/1.1':plot in 'ko ':box\nnew arg 99 '4*x+4'\n"
"evaluate e in arg off:plot e 'b.'; legend 'Evaluate'\n"
"subdata s in arg:plot s 'r.';legend 'SubData'\nlegend 2";
void smgl_indirect(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"");	gr->Title("SubData vs Evaluate");
	mglData in(9), arg(99), e, s;
	gr->Fill(in,"x^3/1.1");	gr->Fill(arg,"4*x+4");
	gr->Plot(in,"ko ");		gr->Box();
	e = in.Evaluate(arg,false);	gr->Plot(e,"b.","legend 'Evaluate'");
	s = in.SubData(arg);	gr->Plot(s,"r.","legend 'SubData'");
	gr->Legend(2);
}
//-----------------------------------------------------------------------------
const char *mmgl_ode="subplot 2 2 0 '<_':title 'Cont':box\naxis:xlabel 'x':ylabel '\\dot{x}'\n"
"new f 100 100 'y^2+2*x^3-x^2-0.5':cont f\n\n"
"subplot 2 2 1 '<_':title 'Flow':box\naxis:xlabel 'x':ylabel '\\dot{x}'\n"
"new fx 100 100 'x-3*x^2'\nnew fy 100 100 'y'\nflow fy fx 'v';value 7\n\n"
"subplot 2 2 2 '<_':title 'ODE':box\naxis:xlabel 'x':ylabel '\\dot{x}'\n"
"for $x -1 1 0.1\n  ode r 'y;x-3*x^2' 'xy' [$x,0]\n  plot r(0) r(1)\n"
"  ode r '-y;-x+3*x^2' 'xy' [$x,0]\n  plot r(0) r(1)\nnext";
void smgl_ode(mglGraph *gr)
{
	gr->SubPlot(2,2,0,"<_");	gr->Title("Cont");	gr->Box();
	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"\\dot{x}");
	mglData f(100,100);	gr->Fill(f,"y^2+2*x^3-x^2-0.5");
	gr->Cont(f);
	gr->SubPlot(2,2,1,"<_");	gr->Title("Flow");	gr->Box();
	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"\\dot{x}");
	mglData fx(100,100), fy(100,100);	gr->Fill(fx,"x-3*x^2");	gr->Fill(fy,"y");
	gr->Flow(fy,fx,"v","value 7");
	gr->SubPlot(2,2,2,"<_");	gr->Title("ODE");	gr->Box();
	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"\\dot{x}");
	for(double x=-1;x<1;x+=0.1)
	{
		mglData in(2), r;	in.a[0]=x;
		r = mglODE("y;x-3*x^2","xy",in);
		gr->Plot(r.SubData(0), r.SubData(1));
		r = mglODE("-y;-x+3*x^2","xy",in);
		gr->Plot(r.SubData(0), r.SubData(1));
	}
}
//-----------------------------------------------------------------------------
const char *mmgl_correl="new a 100 'exp(-10*x^2)'\n"
"new b 100 'exp(-10*(x+0.5)^2)'\n"
"yrange 0 1\nsubplot 1 2 0 '_':title 'Input fields'\n"
"plot a:plot b:box:axis\n"
"correl r a b 'x'\nnorm r 0 1:swap r 'x' # make it human readable\n"
"subplot 1 2 1 '_':title 'Correlation of a and b'\n"
"plot r 'r':axis:box\nline 0.5 0 0.5 1 'B|'";
void smgl_correl(mglGraph *gr)
{
	mglData a(100),b(100);
	gr->Fill(a,"exp(-10*x^2)");	gr->Fill(b,"exp(-10*(x+0.5)^2)");
	gr->SetRange('y',0,1);
	gr->SubPlot(1,2,0,"_");	gr->Title("Input fields");
	gr->Plot(a);	gr->Plot(b);	gr->Axis();	gr->Box();
	mglData r = a.Correl(b,"x");
	r.Norm(0,1);	r.Swap("x");	// make it human readable
	gr->SubPlot(1,2,1,"_");	gr->Title("Correlation of a and b");
	gr->Plot(r,"r");	gr->Axis();	gr->Box();
	gr->Line(mglPoint(0.5,0),mglPoint(0.5,1),"B|");
}
//-----------------------------------------------------------------------------
const char *mmgl_mask="new a 10 10 'x'\n"
"subplot 5 4 0 '':title '\"-\" mask':dens a '3-'\n"
"subplot 5 4 1 '':title '\"+\" mask':dens a '3+'\n"
"subplot 5 4 2 '':title '\"=\" mask':dens a '3='\n"
"subplot 5 4 3 '':title '\";\" mask':dens a '3;'\n"
"subplot 5 4 4 '':title '\";I\" mask':dens a '3;I'\n"
"subplot 5 4 5 '':title '\"o\" mask':dens a '3o'\n"
"subplot 5 4 6 '':title '\"O\" mask':dens a '3O'\n"
"subplot 5 4 7 '':title '\"s\" mask':dens a '3s'\n"
"subplot 5 4 8 '':title '\"S\" mask':dens a '3S'\n"
"subplot 5 4 9 '':title '\";/\" mask':dens a '3;/'\n"
"subplot 5 4 10 '':title '\"~\" mask':dens a '3~'\n"
"subplot 5 4 11 '':title '\"<\" mask':dens a '3<'\n"
"subplot 5 4 12 '':title '\">\" mask':dens a '3>'\n"
"subplot 5 4 13 '':title '\"j\" mask':dens a '3j'\n"
"subplot 5 4 14 '':title '\"-;\\\" mask':dens a '3\\;'\n"
"subplot 5 4 15 '':title '\"d\" mask':dens a '3d'\n"
"subplot 5 4 16 '':title '\"D\" mask':dens a '3D'\n"
"subplot 5 4 17 '':title '\"*\" mask':dens a '3*'\n"
"subplot 5 4 18 '':title '\"\\^\" mask':dens a '3^'\n"
"subplot 5 4 19 '':title 'manual mask'\n"
"mask '+' '24242424FF0101FF':dens a '3+'";
void smgl_mask(mglGraph *gr)
{
	mglData a(10,10);	a.Fill(-1,1);
	gr->SubPlot(5,4,0,"");	gr->Title("'-' mask");	gr->Dens(a,"3-");
	gr->SubPlot(5,4,1,"");	gr->Title("'+' mask");	gr->Dens(a,"3+");
	gr->SubPlot(5,4,2,"");	gr->Title("'=' mask");	gr->Dens(a,"3=");
	gr->SubPlot(5,4,3,"");	gr->Title("';' mask");	gr->Dens(a,"3;");
	gr->SubPlot(5,4,4,"");	gr->Title("';I' mask");	gr->Dens(a,"3;I");
	gr->SubPlot(5,4,5,"");	gr->Title("'o' mask");	gr->Dens(a,"3o");
	gr->SubPlot(5,4,6,"");	gr->Title("'O' mask");	gr->Dens(a,"3O");
	gr->SubPlot(5,4,7,"");	gr->Title("'s' mask");	gr->Dens(a,"3s");
	gr->SubPlot(5,4,8,"");	gr->Title("'S' mask");	gr->Dens(a,"3S");
	gr->SubPlot(5,4,9,"");	gr->Title("';/' mask");	gr->Dens(a,"3;/");
	gr->SubPlot(5,4,10,"");	gr->Title("'~' mask");	gr->Dens(a,"3~");
	gr->SubPlot(5,4,11,"");	gr->Title("'<' mask");	gr->Dens(a,"3<");
	gr->SubPlot(5,4,12,"");	gr->Title("'>' mask");	gr->Dens(a,"3>");
	gr->SubPlot(5,4,13,"");	gr->Title("'j' mask");	gr->Dens(a,"3j");
	gr->SubPlot(5,4,14,"");	gr->Title("';\\\\' mask");	gr->Dens(a,"3;\\");
	gr->SubPlot(5,4,15,"");	gr->Title("'d' mask");	gr->Dens(a,"3d");
	gr->SubPlot(5,4,16,"");	gr->Title("'D' mask");	gr->Dens(a,"3D");
	gr->SubPlot(5,4,17,"");	gr->Title("'*' mask");	gr->Dens(a,"3*");
	gr->SubPlot(5,4,18,"");	gr->Title("'\\^' mask");	gr->Dens(a,"3^");
	gr->SubPlot(5,4,19,"");	gr->Title("manual mask");
	gr->SetMask('+', "24242424FF0101FF");	gr->Dens(a,"3+");
}
//-----------------------------------------------------------------------------
const char *mmgl_export="new a 100 100 'x^2*y':new b 100 100\n"
"export a 'test_data.png' 'BbcyrR' -1 1\n"
"import b 'test_data.png' 'BbcyrR' -1 1\n"
"subplot 2 1 0 '':title 'initial':box:dens a\n"
"subplot 2 1 1 '':title 'imported':box:dens b";
void smgl_export(mglGraph *gr)	// basic data operations
{
	mglData a(100,100), b; gr->Fill(a,"x^2*y");
	a.Export("test_data.png","BbcyrR");
	b.Import("test_data.png","BbcyrR",-1,1);
	gr->SubPlot(2,1,0,"");	gr->Title("initial");	gr->Box();	gr->Dens(a);
	gr->SubPlot(2,1,1,"");	gr->Title("imported");	gr->Box();	gr->Dens(b);
}
//-----------------------------------------------------------------------------
const char *mmgl_data1="new a 40 50 60 'exp(-x^2-4*y^2-16*z^2)'\n"
"light on:alpha on\n"
"copy b a:diff b 'x':subplot 5 3 0:call 'splot'\n"
"copy b a:diff2 b 'x':subplot 5 3 1:call 'splot'\n"
"copy b a:cumsum b 'x':subplot 5 3 2:call 'splot'\n"
"copy b a:integrate b 'x':subplot 5 3 3:call 'splot'\n"
"mirror b 'x':subplot 5 3 4:call 'splot'\n"
"copy b a:diff b 'y':subplot 5 3 5:call 'splot'\n"
"copy b a:diff2 b 'y':subplot 5 3 6:call 'splot'\n"
"copy b a:cumsum b 'y':subplot 5 3 7:call 'splot'\n"
"copy b a:integrate b 'y':subplot 5 3 8:call 'splot'\n"
"mirror b 'y':subplot 5 3 9:call 'splot'\n"
"copy b a:diff b 'z':subplot 5 3 10:call 'splot'\n"
"copy b a:diff2 b 'z':subplot 5 3 11:call 'splot'\n"
"copy b a:cumsum b 'z':subplot 5 3 12:call 'splot'\n"
"copy b a:integrate b 'z':subplot 5 3 13:call 'splot'\n"
"mirror b 'z':subplot 5 3 14:call 'splot'\n"
"stop\nfunc splot 0\n"
"title 'max=',b.max:norm b -1 1 on:rotate 70 60:box:surf3 b\n"
"return";
inline void splot1(mglGraph *gr, mglData &b)
{b.Norm(-1,1,true);gr->Rotate(70,60);gr->Box();gr->Surf3(b);}
void smgl_data1(mglGraph *gr)	// basic data operations
{
	mglData a(40,50,60),b;	gr->Fill(a,"exp(-x^2-4*y^2-16*z^2)");
	gr->Light(true);		gr->Alpha(true);
	b.Set(a);	b.Diff("x");	gr->SubPlot(5,3,0);	splot1(gr,b);
	b.Set(a);	b.Diff2("x");	gr->SubPlot(5,3,1);	splot1(gr,b);
	b.Set(a);	b.CumSum("x");	gr->SubPlot(5,3,2);	splot1(gr,b);
	b.Set(a);	b.Integral("x");gr->SubPlot(5,3,3);	splot1(gr,b);
	b.Mirror("x");	gr->SubPlot(5,3,4);	splot1(gr,b);
	b.Set(a);	b.Diff("y");	gr->SubPlot(5,3,5);	splot1(gr,b);
	b.Set(a);	b.Diff2("y");	gr->SubPlot(5,3,6);	splot1(gr,b);
	b.Set(a);	b.CumSum("y");	gr->SubPlot(5,3,7);	splot1(gr,b);
	b.Set(a);	b.Integral("y");gr->SubPlot(5,3,8);	splot1(gr,b);
	b.Mirror("y");	gr->SubPlot(5,3,9);	splot1(gr,b);
	b.Set(a);	b.Diff("z");	gr->SubPlot(5,3,10);splot1(gr,b);
	b.Set(a);	b.Diff2("z");	gr->SubPlot(5,3,11);splot1(gr,b);
	b.Set(a);	b.CumSum("z");	gr->SubPlot(5,3,12);splot1(gr,b);
	b.Set(a);	b.Integral("z");gr->SubPlot(5,3,13);splot1(gr,b);
	b.Mirror("z");	gr->SubPlot(5,3,14);splot1(gr,b);
}
//-----------------------------------------------------------------------------
const char *mmgl_data2="new a 40 50 60 'exp(-x^2-4*y^2-16*z^2)'\n"
"light on:alpha on\n"
"copy b a:sinfft b 'x':subplot 5 3 0:call 'splot'\n"
"copy b a:cosfft b 'x':subplot 5 3 1:call 'splot'\n"
"copy b a:hankel b 'x':subplot 5 3 2:call 'splot'\n"
"copy b a:swap b 'x':subplot 5 3 3:call 'splot'\n"
"copy b a:smooth b 'x':subplot 5 3 4:call 'splot'\n"
"copy b a:sinfft b 'y':subplot 5 3 5:call 'splot'\n"
"copy b a:cosfft b 'y':subplot 5 3 6:call 'splot'\n"
"copy b a:hankel b 'y':subplot 5 3 7:call 'splot'\n"
"copy b a:swap b 'y':subplot 5 3 8:call 'splot'\n"
"copy b a:smooth b 'y':subplot 5 3 9:call 'splot'\n"
"copy b a:sinfft b 'z':subplot 5 3 10:call 'splot'\n"
"copy b a:cosfft b 'z':subplot 5 3 11:call 'splot'\n"
"copy b a:hankel b 'z':subplot 5 3 12:call 'splot'\n"
"copy b a:swap b 'z':subplot 5 3 13:call 'splot'\n"
"copy b a:smooth b 'z':subplot 5 3 14:call 'splot'\n"
"stop\nfunc splot 0\n"
"title 'max=',b.max:norm b -1 1 on:rotate 70 60:box\n"
"surf3 b 0.5:surf3 b -0.5\nreturn";
inline void splot2(mglGraph *gr, mglData &b)
{b.Norm(-1,1,true);gr->Rotate(70,60);gr->Box();gr->Surf3(0.5,b);gr->Surf3(-0.5,b);}
void smgl_data2(mglGraph *gr)	// data transforms
{
	mglData a(40,50,60),b;	gr->Fill(a,"exp(-x^2-4*y^2-16*z^2)");
	gr->Light(true);		gr->Alpha(true);
	b.Set(a);	b.SinFFT("x");	gr->SubPlot(5,3,0);	splot2(gr,b);
	b.Set(a);	b.CosFFT("x");	gr->SubPlot(5,3,1);	splot2(gr,b);
	b.Set(a);	b.Hankel("x");	gr->SubPlot(5,3,2);	splot2(gr,b);
	b.Set(a);	b.Swap("x");	gr->SubPlot(5,3,3);	splot2(gr,b);
	b.Set(a);	b.Smooth("x");	gr->SubPlot(5,3,4);	splot2(gr,b);
	b.Set(a);	b.SinFFT("y");	gr->SubPlot(5,3,5);	splot2(gr,b);
	b.Set(a);	b.CosFFT("y");	gr->SubPlot(5,3,6);	splot2(gr,b);
	b.Set(a);	b.Hankel("y");	gr->SubPlot(5,3,7);	splot2(gr,b);
	b.Set(a);	b.Swap("y");	gr->SubPlot(5,3,8);	splot2(gr,b);
	b.Set(a);	b.Smooth("y");	gr->SubPlot(5,3,9);	splot2(gr,b);
	b.Set(a);	b.SinFFT("z");	gr->SubPlot(5,3,10);splot2(gr,b);
	b.Set(a);	b.CosFFT("z");	gr->SubPlot(5,3,11);splot2(gr,b);
	b.Set(a);	b.Hankel("z");	gr->SubPlot(5,3,12);splot2(gr,b);
	b.Set(a);	b.Swap("z");	gr->SubPlot(5,3,13);splot2(gr,b);
	b.Set(a);	b.Smooth("z");	gr->SubPlot(5,3,14);splot2(gr,b);
}
//-----------------------------------------------------------------------------
const char *mmgl_param1="new x 100 'sin(pi*x)'\nnew y 100 'cos(pi*x)'\n"
"new z 100 'sin(2*pi*x)'\nnew c 100 'cos(2*pi*x)'\n\n"
"subplot 4 3 0:rotate 40 60:box:plot x y z\n"
"subplot 4 3 1:rotate 40 60:box:area x y z\n"
"subplot 4 3 2:rotate 40 60:box:tens x y z c\n"
"subplot 4 3 3:rotate 40 60:box:bars x y z\n"
"subplot 4 3 4:rotate 40 60:box:stem x y z\n"
"subplot 4 3 5:rotate 40 60:box:textmark x y z c*2 '\\alpha'\n"
"subplot 4 3 6:rotate 40 60:box:tube x y z c/10\n"
"subplot 4 3 7:rotate 40 60:box:mark x y z c 's'\n"
"subplot 4 3 8:box:error x y z/10 c/10\n"
"subplot 4 3 9:rotate 40 60:box:step x y z\n"
"subplot 4 3 10:rotate 40 60:box:torus x z 'z';light on\n"
"subplot 4 3 11:rotate 40 60:box:label x y z '%z'";
void smgl_param1(mglGraph *gr)	// 1d parametric plots
{
	mglData x(100), y(100), z(100), c(100);
	gr->Fill(x,"sin(pi*x)");	gr->Fill(y,"cos(pi*x)");
	gr->Fill(z,"sin(2*pi*x)");	gr->Fill(c,"cos(2*pi*x)");

	gr->SubPlot(4,3,0);	gr->Rotate(40,60);	gr->Box();	gr->Plot(x,y,z);
	gr->SubPlot(4,3,1);	gr->Rotate(40,60);	gr->Box();	gr->Area(x,y,z);
	gr->SubPlot(4,3,2);	gr->Rotate(40,60);	gr->Box();	gr->Tens(x,y,z,c);
	gr->SubPlot(4,3,3);	gr->Rotate(40,60);	gr->Box();	gr->Bars(x,y,z);
	gr->SubPlot(4,3,4);	gr->Rotate(40,60);	gr->Box();	gr->Stem(x,y,z);
	gr->SubPlot(4,3,5);	gr->Rotate(40,60);	gr->Box();	gr->TextMark(x,y,z,c*2,"\\alpha");
	gr->SubPlot(4,3,6);	gr->Rotate(40,60);	gr->Box();	gr->Tube(x,y,z,c/10,"","light on");
	gr->SubPlot(4,3,7);	gr->Rotate(40,60);	gr->Box();	gr->Mark(x,y,z,c,"s");
	gr->SubPlot(4,3,8);	gr->Rotate(40,60);	gr->Box();	gr->Error(x,y,z/10,c/10);
	gr->SubPlot(4,3,9);	gr->Rotate(40,60);	gr->Box();	gr->Step(x,y,z);
	gr->SubPlot(4,3,10);gr->Rotate(40,60);	gr->Box();	gr->Torus(x,z,"z","light on");
	gr->SubPlot(4,3,11);gr->Rotate(40,60);	gr->Box();	gr->Label(x,y,z,"%z");
}
//-----------------------------------------------------------------------------
const char *mmgl_param2="new x 100 100 'sin(pi*(x+y)/2)*cos(pi*y/2)'\n"
"new y 100 100 'cos(pi*(x+y)/2)*cos(pi*y/2)'\n"
"new z 100 100 'sin(pi*y/2)'\nnew c 100 100 'cos(pi*x)'\n\n"
"subplot 4 4 0:rotate 40 60:box:surf x y z\n"
"subplot 4 4 1:rotate 40 60:box:surfc x y z c\n"
"subplot 4 4 2:rotate 40 60:box:surfa x y z c;alpha 1\n"
"subplot 4 4 3:rotate 40 60:box:mesh x y z;meshnum 10\n"
"subplot 4 4 4:rotate 40 60:box:tile x y z;meshnum 10\n"
"subplot 4 4 5:rotate 40 60:box:tiles x y z c;meshnum 10\n"
"subplot 4 4 6:rotate 40 60:box:axial x y z;alpha 0.5;light on\n"
"subplot 4 4 7:rotate 40 60:box:cont x y z\n"
"subplot 4 4 8:rotate 40 60:box:contf x y z;light on:contv x y z;light on\n"
"subplot 4 4 9:rotate 40 60:box:belt x y z 'x';meshnum 10;light on\n"
"subplot 4 4 10:rotate 40 60:box:dens x y z;alpha 0.5\n"
"subplot 4 4 11:rotate 40 60:box\n"
"fall x y z 'g';meshnum 10:fall x y z 'rx';meshnum 10\n"
"subplot 4 4 12:rotate 40 60:box:belt x y z '';meshnum 10;light on\n"
"subplot 4 4 13:rotate 40 60:box:boxs x y z '';meshnum 10;light on\n"
"subplot 4 4 14:rotate 40 60:box:boxs x y z '#';meshnum 10;light on\n"
"subplot 4 4 15:rotate 40 60:box:boxs x y z '@';meshnum 10;light on";
void smgl_param2(mglGraph *gr)	// 2d parametric plots
{
	mglData x(100,100), y(100,100), z(100,100), c(100,100);
	gr->Fill(x,"sin(pi*(x+y)/2)*cos(pi*y/2)");	gr->Fill(y,"cos(pi*(x+y)/2)*cos(pi*y/2)");
	gr->Fill(z,"sin(pi*y/2)");	gr->Fill(c,"cos(pi*x)");

	gr->SubPlot(4,4,0);	gr->Rotate(40,60);	gr->Box();	gr->Surf(x,y,z);
	gr->SubPlot(4,4,1);	gr->Rotate(40,60);	gr->Box();	gr->SurfC(x,y,z,c);
	gr->SubPlot(4,4,2);	gr->Rotate(40,60);	gr->Box();	gr->SurfA(x,y,z,c,"","alpha 1");
	gr->SubPlot(4,4,3);	gr->Rotate(40,60);	gr->Box();	gr->Mesh(x,y,z,"","meshnum 10");
	gr->SubPlot(4,4,4);	gr->Rotate(40,60);	gr->Box();	gr->Tile(x,y,z,"","meshnum 10");
	gr->SubPlot(4,4,5);	gr->Rotate(40,60);	gr->Box();	gr->TileS(x,y,z,c,"","meshnum 10");
	gr->SubPlot(4,4,6);	gr->Rotate(40,60);	gr->Box();	gr->Axial(x,y,z,"","alpha 0.5;light on");
	gr->SubPlot(4,4,7);	gr->Rotate(40,60);	gr->Box();	gr->Cont(x,y,z);
	gr->SubPlot(4,4,8);	gr->Rotate(40,60);	gr->Box();	gr->ContF(x,y,z,"","light on");	gr->ContV(x,y,z,"","light on");
	gr->SubPlot(4,4,9);	gr->Rotate(40,60);	gr->Box();	gr->Belt(x,y,z,"x","meshnum 10;light on");
	gr->SubPlot(4,4,10);gr->Rotate(40,60);	gr->Box();	gr->Dens(x,y,z,"","alpha 0.5");
	gr->SubPlot(4,4,11);gr->Rotate(40,60);	gr->Box();
	gr->Fall(x,y,z,"g","meshnum 10");	gr->Fall(x,y,z,"rx","meshnum 10");
	gr->SubPlot(4,4,12);	gr->Rotate(40,60);	gr->Box();	gr->Belt(x,y,z,"","meshnum 10;light on");
	gr->SubPlot(4,4,13);	gr->Rotate(40,60);	gr->Box();	gr->Boxs(x,y,z,"","meshnum 10;light on");
	gr->SubPlot(4,4,14);	gr->Rotate(40,60);	gr->Box();	gr->Boxs(x,y,z,"#","meshnum 10");
	gr->SubPlot(4,4,15);	gr->Rotate(40,60);	gr->Box();	gr->Boxs(x,y,z,"@","meshnum 10;light on");
}
//-----------------------------------------------------------------------------
const char *mmgl_param3="new x 50 50 50 '(x+2)/3*sin(pi*y/2)'\n"
"new y 50 50 50 '(x+2)/3*cos(pi*y/2)'\nnew z 50 50 50 'z'\n"
"new c 50 50 50 '-2*(x^2+y^2+z^4-z^2)+0.2'\n"
"new d 50 50 50 '1-2*tanh(2*(x+y)^2)'\n\n"
"alpha on:light on\n"
"subplot 4 3 0:rotate 40 60:box:surf3 x y z c\n"
"subplot 4 3 1:rotate 40 60:box:surf3c x y z c d\n"
"subplot 4 3 2:rotate 40 60:box:surf3a x y z c d\n"
"subplot 4 3 3:rotate 40 60:box:cloud x y z c\n"
"subplot 4 3 4:rotate 40 60:box:cont3 x y z c:cont3 x y z c 'x':cont3 x y z c 'z'\n"
"subplot 4 3 5:rotate 40 60:box:contf3 x y z c:contf3 x y z c 'x':contf3 x y z c 'z'\n"
"subplot 4 3 6:rotate 40 60:box:dens3 x y z c:dens3 x y z c 'x':dens3 x y z c 'z'\n"
"subplot 4 3 7:rotate 40 60:box:dots x y z c;meshnum 15\n"
"subplot 4 3 8:rotate 40 60:box:densx c '' 0:densy c '' 0:densz c '' 0\n"
"subplot 4 3 9:rotate 40 60:box:contx c '' 0:conty c '' 0:contz c '' 0\n"
"subplot 4 3 10:rotate 40 60:box:contfx c '' 0:contfy c '' 0:contfz c '' 0";
void smgl_param3(mglGraph *gr)	// 3d parametric plots
{
	mglData x(50,50,50), y(50,50,50), z(50,50,50), c(50,50,50), d(50,50,50);
	gr->Fill(x,"(x+2)/3*sin(pi*y/2)");	gr->Fill(y,"(x+2)/3*cos(pi*y/2)");	gr->Fill(z,"z");
	gr->Fill(c,"-2*(x^2+y^2+z^4-z^2)+0.2");	gr->Fill(d,"1-2*tanh(2*(x+y)^2)");

	gr->Light(true);	gr->Alpha(true);
	gr->SubPlot(4,3,0);	gr->Rotate(40,60);	gr->Box();	gr->Surf3(x,y,z,c);
	gr->SubPlot(4,3,1);	gr->Rotate(40,60);	gr->Box();	gr->Surf3C(x,y,z,c,d);
	gr->SubPlot(4,3,2);	gr->Rotate(40,60);	gr->Box();	gr->Surf3A(x,y,z,c,d);
	gr->SubPlot(4,3,3);	gr->Rotate(40,60);	gr->Box();	gr->Cloud(x,y,z,c);
	gr->SubPlot(4,3,4);	gr->Rotate(40,60);	gr->Box();	gr->Cont3(x,y,z,c);	gr->Cont3(x,y,z,c,"x");	gr->Cont3(x,y,z,c,"z");
	gr->SubPlot(4,3,5);	gr->Rotate(40,60);	gr->Box();	gr->ContF3(x,y,z,c);gr->ContF3(x,y,z,c,"x");gr->ContF3(x,y,z,c,"z");
	gr->SubPlot(4,3,6);	gr->Rotate(40,60);	gr->Box();	gr->Dens3(x,y,z,c);	gr->Dens3(x,y,z,c,"x");	gr->Dens3(x,y,z,c,"z");
	gr->SubPlot(4,3,7);	gr->Rotate(40,60);	gr->Box();	gr->Dots(x,y,z,c,"","meshnum 15");
	gr->SubPlot(4,3,8);	gr->Rotate(40,60);	gr->Box();	gr->DensX(c,"",0);	gr->DensY(c,"",0);	gr->DensZ(c,"",0);
	gr->SubPlot(4,3,9);	gr->Rotate(40,60);	gr->Box();	gr->ContX(c,"",0);	gr->ContY(c,"",0);	gr->ContZ(c,"",0);
	gr->SubPlot(4,3,10);gr->Rotate(40,60);	gr->Box();	gr->ContFX(c,"",0);	gr->ContFY(c,"",0);	gr->ContFZ(c,"",0);
}
//-----------------------------------------------------------------------------
const char *mmgl_paramv="new x 20 20 20 '(x+2)/3*sin(pi*y/2)'\n"
"new y 20 20 20 '(x+2)/3*cos(pi*y/2)'\nnew z 20 20 20 'z+x'\n"
"new ex 20 20 20 'x'\nnew ey 20 20 20 'x^2+y'\nnew ez 20 20 20 'y^2+z'\n\n"
"new x1 50 50 '(x+2)/3*sin(pi*y/2)'\n"
"new y1 50 50 '(x+2)/3*cos(pi*y/2)'\n"
"new e1 50 50 'x'\nnew e2 50 50 'x^2+y'\n\n"
"subplot 3 3 0:rotate 40 60:box:vect x1 y1 e1 e2\n"
"subplot 3 3 1:rotate 40 60:box:flow x1 y1 e1 e2\n"
"subplot 3 3 2:rotate 40 60:box:pipe x1 y1 e1 e2\n"
"subplot 3 3 3:rotate 40 60:box:dew x1 y1 e1 e2\n"
"subplot 3 3 4:rotate 40 60:box:vect x y z ex ey ez\n"
"subplot 3 3 5:rotate 40 60:box\n"
"vect3 x y z ex ey ez:vect3 x y z ex ey ez 'x':vect3 x y z ex ey ez 'z'\n"
"grid3 x y z z '{r9}':grid3 x y z z '{g9}x':grid3 x y z z '{b9}z'\n"
"subplot 3 3 6:rotate 40 60:box:flow x y z ex ey ez\n"
"subplot 3 3 7:rotate 40 60:box:pipe x y z ex ey ez";
void smgl_paramv(mglGraph *gr)	// parametric plots for vector field
{
	mglData x(20,20,20), y(20,20,20), z(20,20,20), ex(20,20,20), ey(20,20,20), ez(20,20,20);
	gr->Fill(x,"(x+2)/3*sin(pi*y/2)");	gr->Fill(y,"(x+2)/3*cos(pi*y/2)");	gr->Fill(z,"x+z");
	gr->Fill(ex,"x");	gr->Fill(ey,"x^2+y");	gr->Fill(ez,"y^2+z");
	mglData x1(20,20), y1(20,20), e1(20,20), e2(20,20);
	gr->Fill(x1,"(x+2)/3*sin(pi*y/2)");	gr->Fill(y1,"(x+2)/3*cos(pi*y/2)");
	gr->Fill(e1,"x");	gr->Fill(e2,"x^2+y");

	gr->SubPlot(3,3,0);	gr->Rotate(40,60);	gr->Box();	gr->Vect(x1,y1,e1,e2);
	gr->SubPlot(3,3,1);	gr->Rotate(40,60);	gr->Box();	gr->Flow(x1,y1,e1,e2);
	gr->SubPlot(3,3,2);	gr->Rotate(40,60);	gr->Box();	gr->Pipe(x1,y1,e1,e2);
	gr->SubPlot(3,3,3);	gr->Rotate(40,60);	gr->Box();	gr->Dew(x1,y1,e1,e2);
	gr->SubPlot(3,3,4);	gr->Rotate(40,60);	gr->Box();	gr->Vect(x,y,z,ex,ey,ez);
	gr->SubPlot(3,3,5);	gr->Rotate(40,60);	gr->Box();
	gr->Vect3(x,y,z,ex,ey,ez);	gr->Vect3(x,y,z,ex,ey,ez,"x");	gr->Vect3(x,y,z,ex,ey,ez,"z");
	gr->Grid3(x,y,z,z,"{r9}");	gr->Grid3(x,y,z,z,"{g9}x");		gr->Grid3(x,y,z,z,"{b9}z");
	gr->SubPlot(3,3,6);	gr->Rotate(40,60);	gr->Box();	gr->Flow(x,y,z,ex,ey,ez);
	gr->SubPlot(3,3,7);	gr->Rotate(40,60);	gr->Box();	gr->Pipe(x,y,z,ex,ey,ez);
}
//-----------------------------------------------------------------------------
const char *mmgl_solve="zrange 0 1\nnew x 20 30 '(x+2)/3*cos(pi*y)'\n"
"new y 20 30 '(x+2)/3*sin(pi*y)'\nnew z 20 30 'exp(-6*x^2-2*sin(pi*y)^2)'\n\n"
"subplot 2 1 0:title 'Cartesian space':rotate 30 -40\naxis 'xyzU':box\nxlabel 'x':ylabel 'y'\n"
"origin 1 1:grid 'xy'\nmesh x y z\n\n"
"# section along 'x' direction\nsolve u x 0.5 'x'\nvar v u.nx 0 1\n"
"evaluate yy y u v\nevaluate xx x u v\nevaluate zz z u v\nplot xx yy zz 'k2o'\n\n"
"# 1st section along 'y' direction\nsolve u1 x -0.5 'y'\nvar v1 u1.nx 0 1\n"
"evaluate yy y v1 u1\nevaluate xx x v1 u1\nevaluate zz z v1 u1\nplot xx yy zz 'b2^'\n\n"
"# 2nd section along 'y' direction\nsolve u2 x -0.5 'y' u1\n"
"evaluate yy y v1 u2\nevaluate xx x v1 u2\nevaluate zz z v1 u2\nplot xx yy zz 'r2v'\n\n"
"subplot 2 1 1:title 'Accompanied space'\nranges 0 1 0 1:origin 0 0\n"
"axis:box:xlabel 'i':ylabel 'j':grid2 z 'h'\n\n"
"plot u v 'k2o':line 0.4 0.5 0.8 0.5 'kA'\n"
"plot v1 u1 'b2^':line 0.5 0.15 0.5 0.3 'bA'\n"
"plot v1 u2 'r2v':line 0.5 0.7 0.5 0.85 'rA'";
void smgl_solve(mglGraph *gr)	// solve and evaluate
{
	gr->SetRange('z',0,1);
	mglData x(20,30), y(20,30), z(20,30), xx,yy,zz;
	gr->Fill(x,"(x+2)/3*cos(pi*y)");
	gr->Fill(y,"(x+2)/3*sin(pi*y)");
	gr->Fill(z,"exp(-6*x^2-2*sin(pi*y)^2)");

	gr->SubPlot(2,1,0);	gr->Title("Cartesian space");	gr->Rotate(30,-40);
	gr->Axis("xyzU");	gr->Box();	gr->Label('x',"x");	gr->Label('y',"y");
	gr->SetOrigin(1,1);	gr->Grid("xy");
	gr->Mesh(x,y,z);

	// section along 'x' direction
	mglData u = x.Solve(0.5,'x');
	mglData v(u.nx);	v.Fill(0,1);
	xx = x.Evaluate(u,v);	yy = y.Evaluate(u,v);	zz = z.Evaluate(u,v);
	gr->Plot(xx,yy,zz,"k2o");

	// 1st section along 'y' direction
	mglData u1 = x.Solve(-0.5,'y');
	mglData v1(u1.nx);	v1.Fill(0,1);
	xx = x.Evaluate(v1,u1);	yy = y.Evaluate(v1,u1);	zz = z.Evaluate(v1,u1);
	gr->Plot(xx,yy,zz,"b2^");

	// 2nd section along 'y' direction
	mglData u2 = x.Solve(-0.5,'y',u1);
	xx = x.Evaluate(v1,u2);	yy = y.Evaluate(v1,u2);	zz = z.Evaluate(v1,u2);
	gr->Plot(xx,yy,zz,"r2v");

	gr->SubPlot(2,1,1);	gr->Title("Accompanied space");
	gr->SetRanges(0,1,0,1);	gr->SetOrigin(0,0);
	gr->Axis();	gr->Box();	gr->Label('x',"i");	gr->Label('y',"j");
	gr->Grid(z,"h");

	gr->Plot(u,v,"k2o");	gr->Line(mglPoint(0.4,0.5),mglPoint(0.8,0.5),"kA");
	gr->Plot(v1,u1,"b2^");	gr->Line(mglPoint(0.5,0.15),mglPoint(0.5,0.3),"bA");
	gr->Plot(v1,u2,"r2v");	gr->Line(mglPoint(0.5,0.7),mglPoint(0.5,0.85),"rA");
}
//-----------------------------------------------------------------------------
const char *mmgl_triangulation="new x 100 '2*rnd-1':new y 100 '2*rnd-1':copy z x^2-y^2\n"
"new g 30 30:triangulate d x y\n"
"title 'Triangulation'\nrotate 50 60:box:light on\n"
"triplot d x y z:triplot d x y z '#k'\ndatagrid g x y z:mesh g 'm'";
void smgl_triangulation(mglGraph *gr)	// surface triangulation
{
	mglData x(100), y(100), z(100);
	gr->Fill(x,"2*rnd-1");	gr->Fill(y,"2*rnd-1");	gr->Fill(z,"v^2-w^2",x,y);
	mglData d = mglTriangulation(x,y), g(30,30);

	if(big!=3)	gr->Title("Triangulation");
	gr->Rotate(40,60);	gr->Box();	gr->Light(true);
	gr->TriPlot(d,x,y,z);	gr->TriPlot(d,x,y,z,"#k");

	gr->DataGrid(g,x,y,z);	gr->Mesh(g,"m");
}
//-----------------------------------------------------------------------------
const char *mmgl_alpha="call 'prepare2d'\nsubplot 2 2 0:title 'default':rotate 50 60:box\nsurf a\n"
"subplot 2 2 1:title 'light on':rotate 50 60:box\nlight on:surf a\n"
"subplot 2 2 3:title 'light on; alpha on':rotate 50 60:box\nalpha on:surf a\n"
"subplot 2 2 2:title 'alpha on':rotate 50 60:box\nlight off:surf a";
void smgl_alpha(mglGraph *gr)	// alpha and lighting
{
	mglData a;	mgls_prepare2d(&a);
	gr->SubPlot(2,2,0);	gr->Title("default");	gr->Rotate(50,60);
	gr->Box();	gr->Surf(a);
	gr->SubPlot(2,2,1);	gr->Title("light on");	gr->Rotate(50,60);
	gr->Box();	gr->Light(true);	gr->Surf(a);
	gr->SubPlot(2,2,3);	gr->Title("alpha on; light on");	gr->Rotate(50,60);
	gr->Box();	gr->Alpha(true);	gr->Surf(a);
	gr->SubPlot(2,2,2);	gr->Title("alpha on");	gr->Rotate(50,60);
	gr->Box();	gr->Light(false);	gr->Surf(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_schemes="new x 100 100 'x':new y 100 100 'y'\n"
"call 'sch' 0 'kw'\ncall 'sch' 1 '%gbrw'\ncall 'sch' 2 'kHCcw'\ncall 'sch' 3 'kBbcw'\n"
"call 'sch' 4 'kRryw'\ncall 'sch' 5 'kGgew'\ncall 'sch' 6 'BbwrR'\ncall 'sch' 7 'BbwgG'\n"
"call 'sch' 8 'GgwmM'\ncall 'sch' 9 'UuwqR'\ncall 'sch' 10 'QqwcC'\ncall 'sch' 11 'CcwyY'\n"
"call 'sch' 12 'bcwyr'\ncall 'sch' 13 'bwr'\ncall 'sch' 14 'wUrqy'\ncall 'sch' 15 'UbcyqR'\n"
"call 'sch' 16 'BbcyrR'\ncall 'sch' 17 'bgr'\ncall 'sch' 18 'BbcyrR|'\ncall 'sch' 19 'b{g,0.3}r'\n"
"stop\nfunc 'sch' 2\nsubplot 2 10 $1 '<>_^' 0.2 0:surfa x y $2\n"
"text 0.07+0.5*mod($1,2) 0.92-0.1*int($1/2) $2 'A'\nreturn";
void smgl_schemes(mglGraph *gr)	// Color table
{
	mglData a(256,2), b(256,2);	a.Fill(-1,1);	b.Fill(-1,1,'y');
	gr->SubPlot(2,10,0,NULL,0.2);	gr->Dens(a,"kw");		gr->Puts(0.07, 0.92, "kw", "A");
	gr->SubPlot(2,10,1,NULL,0.2);	gr->SurfA(a,b,"%gbrw");	gr->Puts(0.57, 0.92, "%gbrw", "A");
	gr->SubPlot(2,10,2,NULL,0.2);	gr->Dens(a,"kHCcw");	gr->Puts(0.07, 0.82, "kHCcw", "A");
	gr->SubPlot(2,10,3,NULL,0.2);	gr->Dens(a,"kBbcw");	gr->Puts(0.57, 0.82, "kBbcw", "A");
	gr->SubPlot(2,10,4,NULL,0.2);	gr->Dens(a,"kRryw");	gr->Puts(0.07, 0.72, "kRryw", "A");
	gr->SubPlot(2,10,5,NULL,0.2);	gr->Dens(a,"kGgew");	gr->Puts(0.57, 0.72, "kGgew", "A");
	gr->SubPlot(2,10,6,NULL,0.2);	gr->Dens(a,"BbwrR");	gr->Puts(0.07, 0.62, "BbwrR", "A");
	gr->SubPlot(2,10,7,NULL,0.2);	gr->Dens(a,"BbwgG");	gr->Puts(0.57, 0.62, "BbwgG", "A");
	gr->SubPlot(2,10,8,NULL,0.2);	gr->Dens(a,"GgwmM");	gr->Puts(0.07, 0.52, "GgwmM", "A");
	gr->SubPlot(2,10,9,NULL,0.2);	gr->Dens(a,"UuwqR");	gr->Puts(0.57, 0.52, "UuwqR", "A");
	gr->SubPlot(2,10,10,NULL,0.2);	gr->Dens(a,"QqwcC");	gr->Puts(0.07, 0.42, "QqwcC", "A");
	gr->SubPlot(2,10,11,NULL,0.2);	gr->Dens(a,"CcwyY");	gr->Puts(0.57, 0.42, "CcwyY", "A");
	gr->SubPlot(2,10,12,NULL,0.2);	gr->Dens(a,"bcwyr");	gr->Puts(0.07, 0.32, "bcwyr", "A");
	gr->SubPlot(2,10,13,NULL,0.2);	gr->Dens(a,"bwr");		gr->Puts(0.57, 0.32, "bwr", "A");
	gr->SubPlot(2,10,14,NULL,0.2);	gr->Dens(a,"wUrqy");	gr->Puts(0.07, 0.22, "wUrqy", "A");
	gr->SubPlot(2,10,15,NULL,0.2);	gr->Dens(a,"UbcyqR");	gr->Puts(0.57, 0.22, "UbcyqR", "A");
	gr->SubPlot(2,10,16,NULL,0.2);	gr->Dens(a,"BbcyrR");	gr->Puts(0.07, 0.12, "BbcyrR", "A");
	gr->SubPlot(2,10,17,NULL,0.2);	gr->Dens(a,"bgr");		gr->Puts(0.57, 0.12, "bgr", "A");
	gr->SubPlot(2,10,18,NULL,0.2);	gr->Dens(a,"BbcyrR|");	gr->Puts(0.07, 0.02, "BbcyrR|", "A");
	gr->SubPlot(2,10,19,NULL,0.2);	gr->Dens(a,"b{g,0.3}r");		gr->Puts(0.57, 0.02, "b\\{g,0.3\\}r", "A");
}
//-----------------------------------------------------------------------------
const char *mmgl_curvcoor="origin -1 1 -1\nsubplot 2 2 0:title 'Cartesian':rotate 50 60:fplot '2*t-1' '0.5' '0' '2r':axis:grid\n"
"axis 'y*sin(pi*x)' 'y*cos(pi*x)' '':subplot 2 2 1:title 'Cylindrical':rotate 50 60:fplot '2*t-1' '0.5' '0' '2r':axis:grid\n"
"axis '2*y*x' 'y*y - x*x' '':subplot 2 2 2:title 'Parabolic':rotate 50 60:fplot '2*t-1' '0.5' '0' '2r':axis:grid\n"
"axis 'y*sin(pi*x)' 'y*cos(pi*x)' 'x+z':subplot 2 2 3:title 'Spiral':rotate 50 60:fplot '2*t-1' '0.5' '0' '2r':axis:grid";
void smgl_curvcoor(mglGraph *gr)	// curvilinear coordinates
{
	gr->SetOrigin(-1,1,-1);

	gr->SubPlot(2,2,0);	gr->Title("Cartesian");	gr->Rotate(50,60);
	gr->FPlot("2*t-1","0.5","0","r2");
	gr->Axis(); gr->Grid();

	gr->SetFunc("y*sin(pi*x)","y*cos(pi*x)",0);
	gr->SubPlot(2,2,1);	gr->Title("Cylindrical");	gr->Rotate(50,60);
	gr->FPlot("2*t-1","0.5","0","r2");
	gr->Axis(); gr->Grid();

	gr->SetFunc("2*y*x","y*y - x*x",0);
	gr->SubPlot(2,2,2);	gr->Title("Parabolic");	gr->Rotate(50,60);
	gr->FPlot("2*t-1","0.5","0","r2");
	gr->Axis(); gr->Grid();

	gr->SetFunc("y*sin(pi*x)","y*cos(pi*x)","x+z");
	gr->SubPlot(2,2,3);	gr->Title("Spiral");	gr->Rotate(50,60);
	gr->FPlot("2*t-1","0.5","0","r2");
	gr->Axis(); gr->Grid();
	gr->SetFunc(0,0,0);	// set to default Cartesian
}
//-----------------------------------------------------------------------------
const char *mmgl_style="";
void smgl_style(mglGraph *gr)	// pen styles
{
	gr->SubPlot(2,2,0);
	double d,x1,x2,x0,y=1.1, y1=1.15;
	d=0.3, x0=0.2, x1=0.5, x2=0.6;
	gr->Line(mglPoint(x0,y1-0*d),mglPoint(x1,y1-0*d),"k-");	gr->Puts(mglPoint(x2,y-0*d),"Solid '-'",":rL");
	gr->Line(mglPoint(x0,y1-1*d),mglPoint(x1,y1-1*d),"k|");	gr->Puts(mglPoint(x2,y-1*d),"Long Dash '|'",":rL");
	gr->Line(mglPoint(x0,y1-2*d),mglPoint(x1,y1-2*d),"k;");	gr->Puts(mglPoint(x2,y-2*d),"Dash ';'",":rL");
	gr->Line(mglPoint(x0,y1-3*d),mglPoint(x1,y1-3*d),"k=");	gr->Puts(mglPoint(x2,y-3*d),"Small dash '='",":rL");
	gr->Line(mglPoint(x0,y1-4*d),mglPoint(x1,y1-4*d),"kj");	gr->Puts(mglPoint(x2,y-4*d),"Dash-dot 'j'",":rL");
	gr->Line(mglPoint(x0,y1-5*d),mglPoint(x1,y1-5*d),"ki");	gr->Puts(mglPoint(x2,y-5*d),"Small dash-dot 'i'",":rL");
	gr->Line(mglPoint(x0,y1-6*d),mglPoint(x1,y1-6*d),"k:");	gr->Puts(mglPoint(x2,y-6*d),"Dots ':'",":rL");
	gr->Line(mglPoint(x0,y1-7*d),mglPoint(x1,y1-7*d),"k ");	gr->Puts(mglPoint(x2,y-7*d),"None ' '",":rL");
	gr->Line(mglPoint(x0,y1-8*d),mglPoint(x1,y1-8*d),"k{df090}");	gr->Puts(mglPoint(x2,y-8*d),"Manual '{df090}'",":rL");

	d=0.25; x1=-1; x0=-0.8;	y = -0.05;
	gr->Mark(mglPoint(x1,5*d),"k.");		gr->Puts(mglPoint(x0,y+5*d),"'.'",":rL");
	gr->Mark(mglPoint(x1,4*d),"k+");		gr->Puts(mglPoint(x0,y+4*d),"'+'",":rL");
	gr->Mark(mglPoint(x1,3*d),"kx");		gr->Puts(mglPoint(x0,y+3*d),"'x'",":rL");
	gr->Mark(mglPoint(x1,2*d),"k*");		gr->Puts(mglPoint(x0,y+2*d),"'*'",":rL");
	gr->Mark(mglPoint(x1,d),"ks");		gr->Puts(mglPoint(x0,y+d),"'s'",":rL");
	gr->Mark(mglPoint(x1,0),"kd");		gr->Puts(mglPoint(x0,y),"'d'",":rL");
	gr->Mark(mglPoint(x1,-d,0),"ko");	gr->Puts(mglPoint(x0,y-d),"'o'",":rL");
	gr->Mark(mglPoint(x1,-2*d,0),"k^");	gr->Puts(mglPoint(x0,y-2*d),"'\\^'",":rL");
	gr->Mark(mglPoint(x1,-3*d,0),"kv");	gr->Puts(mglPoint(x0,y-3*d),"'v'",":rL");
	gr->Mark(mglPoint(x1,-4*d,0),"k<");	gr->Puts(mglPoint(x0,y-4*d),"'<'",":rL");
	gr->Mark(mglPoint(x1,-5*d,0),"k>");	gr->Puts(mglPoint(x0,y-5*d),"'>'",":rL");

	d=0.25; x1=-0.5; x0=-0.3;	y = -0.05;
	gr->Mark(mglPoint(x1,5*d),"k#.");	gr->Puts(mglPoint(x0,y+5*d),"'\\#.'",":rL");
	gr->Mark(mglPoint(x1,4*d),"k#+");	gr->Puts(mglPoint(x0,y+4*d),"'\\#+'",":rL");
	gr->Mark(mglPoint(x1,3*d),"k#x");	gr->Puts(mglPoint(x0,y+3*d),"'\\#x'",":rL");
	gr->Mark(mglPoint(x1,2*d),"k#*");	gr->Puts(mglPoint(x0,y+2*d),"'\\#*'",":rL");
	gr->Mark(mglPoint(x1,d),"k#s");		gr->Puts(mglPoint(x0,y+d),"'\\#s'",":rL");
	gr->Mark(mglPoint(x1,0),"k#d");		gr->Puts(mglPoint(x0,y),"'\\#d'",":rL");
	gr->Mark(mglPoint(x1,-d,0),"k#o");	gr->Puts(mglPoint(x0,y-d),"'\\#o'",":rL");
	gr->Mark(mglPoint(x1,-2*d,0),"k#^");	gr->Puts(mglPoint(x0,y-2*d),"'\\#\\^'",":rL");
	gr->Mark(mglPoint(x1,-3*d,0),"k#v");	gr->Puts(mglPoint(x0,y-3*d),"'\\#v'",":rL");
	gr->Mark(mglPoint(x1,-4*d,0),"k#<");	gr->Puts(mglPoint(x0,y-4*d),"'\\#<'",":rL");
	gr->Mark(mglPoint(x1,-5*d,0),"k#>");	gr->Puts(mglPoint(x0,y-5*d),"'\\#>'",":rL");

	gr->SubPlot(2,2,1);
	double a=0.1,b=0.4,c=0.5;
	gr->Line(mglPoint(a,1),mglPoint(b,1),"k-A");		gr->Puts(mglPoint(c,1),"Style 'A' or 'A\\_'",":rL");
	gr->Line(mglPoint(a,0.8),mglPoint(b,0.8),"k-V");	gr->Puts(mglPoint(c,0.8),"Style 'V' or 'V\\_'",":rL");
	gr->Line(mglPoint(a,0.6),mglPoint(b,0.6),"k-K");	gr->Puts(mglPoint(c,0.6),"Style 'K' or 'K\\_'",":rL");
	gr->Line(mglPoint(a,0.4),mglPoint(b,0.4),"k-I");	gr->Puts(mglPoint(c,0.4),"Style 'I' or 'I\\_'",":rL");
	gr->Line(mglPoint(a,0.2),mglPoint(b,0.2),"k-D");	gr->Puts(mglPoint(c,0.2),"Style 'D' or 'D\\_'",":rL");
	gr->Line(mglPoint(a,0),mglPoint(b,0),"k-S");		gr->Puts(mglPoint(c,0),"Style 'S' or 'S\\_'",":rL");
	gr->Line(mglPoint(a,-0.2),mglPoint(b,-0.2),"k-O");	gr->Puts(mglPoint(c,-0.2),"Style 'O' or 'O\\_'",":rL");
	gr->Line(mglPoint(a,-0.4),mglPoint(b,-0.4),"k-T");	gr->Puts(mglPoint(c,-0.4),"Style 'T' or 'T\\_'",":rL");
	gr->Line(mglPoint(a,-0.6),mglPoint(b,-0.6),"k-X");	gr->Puts(mglPoint(c,-0.6),"Style 'X' or 'X\\_'",":rL");
	gr->Line(mglPoint(a,-0.8),mglPoint(b,-0.8),"k-_");	gr->Puts(mglPoint(c,-0.8),"Style '\\_' or none",":rL");
	gr->Line(mglPoint(a,-1),mglPoint(b,-1),"k-AS");		gr->Puts(mglPoint(c,-1),"Style 'AS'",":rL");
	gr->Line(mglPoint(a,-1.2),mglPoint(b,-1.2),"k-_A");	gr->Puts(mglPoint(c,-1.2),"Style '\\_A'",":rL");

	a=-1;	b=-0.7;	c=-0.6;
	gr->Line(mglPoint(a,1),mglPoint(b,1),"kAA");		gr->Puts(mglPoint(c,1),"Style 'AA'",":rL");
	gr->Line(mglPoint(a,0.8),mglPoint(b,0.8),"kVV");	gr->Puts(mglPoint(c,0.8),"Style 'VV'",":rL");
	gr->Line(mglPoint(a,0.6),mglPoint(b,0.6),"kKK");	gr->Puts(mglPoint(c,0.6),"Style 'KK'",":rL");
	gr->Line(mglPoint(a,0.4),mglPoint(b,0.4),"kII");	gr->Puts(mglPoint(c,0.4),"Style 'II'",":rL");
	gr->Line(mglPoint(a,0.2),mglPoint(b,0.2),"kDD");	gr->Puts(mglPoint(c,0.2),"Style 'DD'",":rL");
	gr->Line(mglPoint(a,0),mglPoint(b,0),"kSS");		gr->Puts(mglPoint(c,0),"Style 'SS'",":rL");
	gr->Line(mglPoint(a,-0.2),mglPoint(b,-0.2),"kOO");	gr->Puts(mglPoint(c,-0.2),"Style 'OO'",":rL");
	gr->Line(mglPoint(a,-0.4),mglPoint(b,-0.4),"kTT");	gr->Puts(mglPoint(c,-0.4),"Style 'TT'",":rL");
	gr->Line(mglPoint(a,-0.6),mglPoint(b,-0.6),"kXX");	gr->Puts(mglPoint(c,-0.6),"Style 'XX'",":rL");
	gr->Line(mglPoint(a,-0.8),mglPoint(b,-0.8),"k-__");	gr->Puts(mglPoint(c,-0.8),"Style '\\_\\_'",":rL");
	gr->Line(mglPoint(a,-1),mglPoint(b,-1),"k-VA");		gr->Puts(mglPoint(c,-1),"Style 'VA'",":rL");
	gr->Line(mglPoint(a,-1.2),mglPoint(b,-1.2),"k-AV");	gr->Puts(mglPoint(c,-1.2),"Style 'AV'",":rL");

	gr->SubPlot(2,2,2);
	//#LENUQ
	gr->FaceZ(mglPoint(-1,	-1), 0.4, 0.3, "L#");	gr->Puts(mglPoint(-0.8,-0.9), "L", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-1), 0.4, 0.3, "E#");	gr->Puts(mglPoint(-0.4,-0.9), "E", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-1), 0.4, 0.3, "N#");	gr->Puts(mglPoint(0,  -0.9), "N", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-1), 0.4, 0.3, "U#");	gr->Puts(mglPoint(0.4,-0.9), "U", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-1), 0.4, 0.3, "Q#");	gr->Puts(mglPoint(0.8,-0.9), "Q", "w:C", -1.4);
	//#lenuq
	gr->FaceZ(mglPoint(-1,	-0.7), 0.4, 0.3, "l#");	gr->Puts(mglPoint(-0.8,-0.6), "l", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-0.7), 0.4, 0.3, "e#");	gr->Puts(mglPoint(-0.4,-0.6), "e", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-0.7), 0.4, 0.3, "n#");	gr->Puts(mglPoint(0,  -0.6), "n", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-0.7), 0.4, 0.3, "u#");	gr->Puts(mglPoint(0.4,-0.6), "u", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-0.7), 0.4, 0.3, "q#");	gr->Puts(mglPoint(0.8,-0.6), "q", "k:C", -1.4);
	//#CMYkP
	gr->FaceZ(mglPoint(-1,	-0.4), 0.4, 0.3, "C#");	gr->Puts(mglPoint(-0.8,-0.3), "C", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-0.4), 0.4, 0.3, "M#");	gr->Puts(mglPoint(-0.4,-0.3), "M", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-0.4), 0.4, 0.3, "Y#");	gr->Puts(mglPoint(0,  -0.3), "Y", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-0.4), 0.4, 0.3, "k#");	gr->Puts(mglPoint(0.4,-0.3), "k", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-0.4), 0.4, 0.3, "P#");	gr->Puts(mglPoint(0.8,-0.3), "P", "w:C", -1.4);
	//#cmywp
	gr->FaceZ(mglPoint(-1,	-0.1), 0.4, 0.3, "c#");	gr->Puts(mglPoint(-0.8, 0), "c", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-0.1), 0.4, 0.3, "m#");	gr->Puts(mglPoint(-0.4, 0), "m", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-0.1), 0.4, 0.3, "y#");	gr->Puts(mglPoint(0,   0), "y", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-0.1), 0.4, 0.3, "w#");	gr->Puts(mglPoint(0.4, 0), "w", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-0.1), 0.4, 0.3, "p#");	gr->Puts(mglPoint(0.8, 0), "p", "k:C", -1.4);
	//#BGRHW
	gr->FaceZ(mglPoint(-1,	0.2), 0.4, 0.3, "B#");	gr->Puts(mglPoint(-0.8, 0.3), "B", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,0.2), 0.4, 0.3, "G#");	gr->Puts(mglPoint(-0.4, 0.3), "G", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,0.2), 0.4, 0.3, "R#");	gr->Puts(mglPoint(0,   0.3), "R", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	0.2), 0.4, 0.3, "H#");	gr->Puts(mglPoint(0.4, 0.3), "H", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	0.2), 0.4, 0.3, "W#");	gr->Puts(mglPoint(0.8, 0.3), "W", "w:C", -1.4);
	//#bgrhw
	gr->FaceZ(mglPoint(-1,	0.5), 0.4, 0.3, "b#");	gr->Puts(mglPoint(-0.8, 0.6), "b", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,0.5), 0.4, 0.3, "g#");	gr->Puts(mglPoint(-0.4, 0.6), "g", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,0.5), 0.4, 0.3, "r#");	gr->Puts(mglPoint(0,   0.6), "r", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	0.5), 0.4, 0.3, "h#");	gr->Puts(mglPoint(0.4, 0.6), "h", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	0.5), 0.4, 0.3, "w#");	gr->Puts(mglPoint(0.8, 0.6), "w", "k:C", -1.4);
	//#brighted
	gr->FaceZ(mglPoint(-1,	0.8), 0.4, 0.3, "{r1}#");	gr->Puts(mglPoint(-0.8, 0.9), "\\{r1\\}", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,0.8), 0.4, 0.3, "{r3}#");	gr->Puts(mglPoint(-0.4, 0.9), "\\{r3\\}", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,0.8), 0.4, 0.3, "{r5}#");	gr->Puts(mglPoint(0,   0.9), "\\{r5\\}", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	0.8), 0.4, 0.3, "{r7}#");	gr->Puts(mglPoint(0.4, 0.9), "\\{r7\\}", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	0.8), 0.4, 0.3, "{r9}#");	gr->Puts(mglPoint(0.8, 0.9), "\\{r9\\}", "k:C", -1.4);
	// HEX
	gr->FaceZ(mglPoint(-1, -1.3), 1, 0.3, "{xff9966}#");	gr->Puts(mglPoint(-0.5,-1.2), "\\{xff9966\\}", "k:C", -1.4);
	gr->FaceZ(mglPoint(0,  -1.3), 1, 0.3, "{x83CAFF}#");	gr->Puts(mglPoint( 0.5,-1.2), "\\{x83CAFF\\}", "k:C", -1.4);

	gr->SubPlot(2,2,3);
	char stl[3]="r1", txt[4]="'1'";
	for(int i=0;i<10;i++)
	{
		txt[1]=stl[1]='0'+i;
		gr->Line(mglPoint(-1,0.2*i-1),mglPoint(1,0.2*i-1),stl);
		gr->Puts(mglPoint(1.05,0.2*i-1),txt,":L");
	}
}
//-----------------------------------------------------------------------------
const char *mmgl_text="call 'prepare1d'\nsubplot 2 2 0 ''\ntext 0 1 'Text can be in ASCII and in Unicode'\n"
"text 0 0.6 'It can be \\wire{wire}, \\big{big} or #r{colored}'\n"
"text 0 0.2 'One can change style in string: \\b{bold}, \\i{italic, \\b{both}}'\n"
"text 0 -0.2 'Easy to \\a{overline} or \\u{underline}'\n"
"text 0 -0.6 'Easy to change indexes ^{up} _{down} @{center}'\n"
"text 0 -1 'It parse TeX: \\int \\alpha \\cdot \\\n\\sqrt3{sin(\\pi x)^2 + \\gamma_{i_k}} dx'\n"
"subplot 2 2 1 ''\n text 0 0.5 '\\sqrt{\\frac{\\alpha^{\\gamma^2}+\\overset 1{\\big\\infty}}{\\sqrt3{2+b}}}' '@' -2\n"
"text 0 -0.1 'More text position: \\frac{a}{b}, \\dfrac{a}{b}, [\\stack{a}{bbb}], [\\stackl{a}{bbb}], [\\stackr{a}{bbb}], \\sup{a}{sup}, \\sub{a}{sub}'"
"text 0 -0.5 'Text can be printed\\n{}on several lines'\n"
"text 0 -0.9 'or with color gradient' 'BbcyrR'\n"
"subplot 2 2 2 '':box:plot y(:,0)\ntext y 'This is very very long string drawn along a curve' 'k'\ntext y 'Another string drawn above a curve' 'Tr'\n"
"subplot 2 2 3 '':line -1 -1 1 -1 'rA':text 0 -1 1 -1 'Horizontal'\n"
"line -1 -1 1 1 'rA':text 0 0 1 1 'At angle' '@'\nline -1 -1 -1 1 'rA':text -1 0 -1 1 'Vertical'";
void smgl_text(mglGraph *gr)	// text drawing
{
	if(big!=3)	gr->SubPlot(2,2,0,"");
	gr->Putsw(mglPoint(0,1),L"Text can be in ASCII and in Unicode");
	gr->Puts(mglPoint(0,0.6),"It can be \\wire{wire}, \\big{big} or #r{colored}");
	gr->Puts(mglPoint(0,0.2),"One can change style in string: "
	"\\b{bold}, \\i{italic, \\b{both}}");
	gr->Puts(mglPoint(0,-0.2),"Easy to \\a{overline} or "
	"\\u{underline}");
	gr->Puts(mglPoint(0,-0.6),"Easy to change indexes ^{up} _{down} @{center}");
	gr->Puts(mglPoint(0,-1),"It parse TeX: \\int \\alpha \\cdot "
	"\\sqrt3{sin(\\pi x)^2 + \\gamma_{i_k}} dx");
	if(big==3)	return;

	gr->SubPlot(2,2,1,"");
	gr->Puts(mglPoint(0,0.5), "\\sqrt{\\frac{\\alpha^{\\gamma^2}+\\overset 1{\\big\\infty}}{\\sqrt3{2+b}}}", "@", -2);
	gr->Puts(mglPoint(0,-0.1),"More text position: \\frac{a}{b}, \\dfrac{a}{b}, [\\stack{a}{bbb}], [\\stackl{a}{bbb}], [\\stackr{a}{bbb}], \\sup{a}{sup}, \\sub{a}{sub}");
	gr->Puts(mglPoint(0,-0.5),"Text can be printed\non several lines");
	gr->Puts(mglPoint(0,-0.9),"or with col\bor gradient","BbcyrR");

	gr->SubPlot(2,2,2,"");
	mglData y;	mgls_prepare1d(&y);
	gr->Box();	gr->Plot(y.SubData(-1,0));
	gr->Text(y,"This is very very long string drawn along a curve","k");
	gr->Text(y,"Another string drawn under a curve","Tr");

	gr->SubPlot(2,2,3,"");
	gr->Line(mglPoint(-1,-1),mglPoint(1,-1),"rA");	gr->Puts(mglPoint(0,-1),mglPoint(1,-1),"Horizontal");
	gr->Line(mglPoint(-1,-1),mglPoint(1,1),"rA");	gr->Puts(mglPoint(0,0),mglPoint(1,1),"At angle","@");
	gr->Line(mglPoint(-1,-1),mglPoint(-1,1),"rA");	gr->Puts(mglPoint(-1,0),mglPoint(-1,1),"Vertical");
}
//-----------------------------------------------------------------------------
const char *mmgl_text2="call 'prepare1d'\n"
"subplot 1 3 0 '':box:plot y(:,0)\ntext y 'This is very very long string drawn along a curve' 'k'\ntext y 'Another string drawn under a curve' 'Tr'\n"
"subplot 1 3 1 '':box:plot y(:,0)\ntext y 'This is very very long string drawn along a curve' 'k:C'\ntext y 'Another string drawn under a curve' 'Tr:C'\n"
"subplot 1 3 2 '':box:plot y(:,0)\ntext y 'This is very very long string drawn along a curve' 'k:R'\ntext y 'Another string drawn under a curve' 'Tr:R'";
void smgl_text2(mglGraph *gr)	// text drawing
{
	mglData y;	mgls_prepare1d(&y);
	if(big!=3)	gr->SubPlot(1,3,0,"");
	gr->Box();	gr->Plot(y.SubData(-1,0));
	gr->Text(y,"This is very very long string drawn along a curve","k");
	gr->Text(y,"Another string drawn under a curve","Tr");
	if(big==3)	return;

	gr->SubPlot(1,3,1,"");
	gr->Box();	gr->Plot(y.SubData(-1,0));
	gr->Text(y,"This is very very long string drawn along a curve","k:C");
	gr->Text(y,"Another string drawn under a curve","Tr:C");

	gr->SubPlot(1,3,2,"");
	gr->Box();	gr->Plot(y.SubData(-1,0));
	gr->Text(y,"This is very very long string drawn along a curve","k:R");
	gr->Text(y,"Another string drawn under a curve","Tr:R");
}
//-----------------------------------------------------------------------------
const char *mmgl_fonts="define d 0.25\nloadfont 'STIX':text 0 1.1 'default font (STIX)'\nloadfont 'adventor':text 0 1.1-d 'adventor font'\n"
"loadfont 'bonum':text 0 1.1-2*d 'bonum font'\nloadfont 'chorus':text 0 1.1-3*d 'chorus font'\nloadfont 'cursor':text 0 1.1-4*d 'cursor font'\n"
"loadfont 'heros':text 0 1.1-5*d 'heros font'\nloadfont 'heroscn':text 0 1.1-6*d 'heroscn font'\nloadfont 'pagella':text 0 1.1-7*d 'pagella font'\n"
"loadfont 'schola':text 0 1.1-8*d 'schola font'\nloadfont 'termes':text 0 1.1-9*d 'termes font'\nloadfont ''";
void smgl_fonts(mglGraph *gr)	// font typefaces
{
	double h=1.1, d=0.25;
	gr->LoadFont("STIX");		gr->Puts(mglPoint(0,h), "default font (STIX)");
	gr->LoadFont("adventor");	gr->Puts(mglPoint(0,h-d), "adventor font");
	gr->LoadFont("bonum");		gr->Puts(mglPoint(0,h-2*d), "bonum font");
	gr->LoadFont("chorus");		gr->Puts(mglPoint(0,h-3*d), "chorus font");
	gr->LoadFont("cursor");		gr->Puts(mglPoint(0,h-4*d), "cursor font");
	gr->LoadFont("heros");		gr->Puts(mglPoint(0,h-5*d), "heros font");
	gr->LoadFont("heroscn");	gr->Puts(mglPoint(0,h-6*d), "heroscn font");
	gr->LoadFont("pagella");	gr->Puts(mglPoint(0,h-7*d), "pagella font");
	gr->LoadFont("schola");		gr->Puts(mglPoint(0,h-8*d), "schola font");
	gr->LoadFont("termes");		gr->Puts(mglPoint(0,h-9*d), "termes font");
	gr->LoadFont("");
}
//-----------------------------------------------------------------------------
const char *mmgl_bars="new ys 10 3 '0.8*sin(pi*(x+y/4+1.25))+0.2*rnd':origin 0 0 0\n"
"subplot 3 2 0 '':title 'Bars plot (default)':box:bars ys\nsubplot 3 2 1 '':title '2 colors':box:bars ys 'cbgGyr'\n"
"subplot 3 2 4 '':title '\"\\#\" style':box:bars ys '#'\n"
"new yc 30 'sin(pi*x)':new xc 30 'cos(pi*x)':new z 30 'x'\nsubplot 3 2 5:title '3d variant':rotate 50 60:box:bars xc yc z 'r'\n"
"ranges -1 1 -3 3:subplot 3 2 2 '':title '\"a\" style':box:bars ys 'a'\nsubplot 3 2 3 '':title '\"f\" style':box:bars ys 'f'";
void smgl_bars(mglGraph *gr)
{
	mglData ys(10,3);	ys.Modify("0.8*sin(pi*(2*x+y/2))+0.2*rnd");
	gr->SetOrigin(0,0,0);
	if(big!=3)	{	gr->SubPlot(3,2,0,"");	gr->Title("Bars plot (default)");	}
	gr->Box();	gr->Bars(ys);
	if(big==3)	return;
	gr->SubPlot(3,2,1,"");	gr->Title("2 colors");	gr->Box();	gr->Bars(ys,"cbgGyr");
	gr->SubPlot(3,2,4,"");	gr->Title("'\\#' style");	gr->Box();	gr->Bars(ys,"#");
	gr->SubPlot(3,2,5);	gr->Title("3d variant");	gr->Rotate(50,60);	gr->Box();
	mglData yc(30), xc(30), z(30);	z.Modify("2*x-1");
	yc.Modify("sin(pi*(2*x-1))");	xc.Modify("cos(pi*2*x-pi)");
	gr->Bars(xc,yc,z,"r");
	gr->SetRanges(-1,1,-3,3);	// increase range since summation can exceed [-1,1]
	gr->SubPlot(3,2,2,"");	gr->Title("'a' style");	gr->Box();	gr->Bars(ys,"a");
	gr->SubPlot(3,2,3,"");	gr->Title("'f' style");	gr->Box();	gr->Bars(ys,"f");
}
//-----------------------------------------------------------------------------
const char *mmgl_barh="new ys 10 3 '0.8*sin(pi*(x+y/4+1.25))+0.2*rnd':origin 0 0 0\n"
"subplot 2 2 0 '':title 'Barh plot (default)':box:barh ys\nsubplot 2 2 1 '':title '2 colors':box:barh ys 'cbgGyr'\n"
"ranges -3 3 -1 1:subplot 2 2 2 '':title '\"a\" style':box:barh ys 'a'\nsubplot 2 2 3 '': title '\"f\" style':box:barh ys 'f'";
void smgl_barh(mglGraph *gr)
{
	mglData ys(10,3);	ys.Modify("0.8*sin(pi*(2*x+y/2))+0.2*rnd");
	gr->SetOrigin(0,0,0);
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Barh plot (default)");	}
	gr->Box();	gr->Barh(ys);
	if(big==3)	return;
	gr->SubPlot(2,2,1,"");	gr->Title("2 colors");	gr->Box();	gr->Barh(ys,"cbgGyr");
	gr->SetRanges(-3,3,-1,1);	// increase range since summation can exceed [-1,1]
	gr->SubPlot(2,2,2,"");	gr->Title("'a' style");	gr->Box();	gr->Barh(ys,"a");
	gr->SubPlot(2,2,3,"");	gr->Title("'f' style");	gr->Box();	gr->Barh(ys,"f");
}
//-----------------------------------------------------------------------------
const char *mmgl_area="call 'prepare1d'\norigin 0 0 0\nsubplot 2 2 0 '':title 'Area plot (default)':box:area y\n"
"subplot 2 2 1 '':title '2 colors':box:area y 'cbgGyr'\nsubplot 2 2 2 '':title '\"!\" style':box:area y '!'\n"
"new yc 30 'sin(pi*x)':new xc 30 'cos(pi*x)':new z 30 'x'\nsubplot 2 2 3:title '3d variant':rotate 50 60:box\n"
"area xc yc z 'r'\narea xc -yc z 'b#'";
void smgl_area(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);	gr->SetOrigin(0,0,0);
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Area plot (default)");	}
	gr->Box();	gr->Area(y);
	if(big==3)	return;
	gr->SubPlot(2,2,1,"");	gr->Title("2 colors");	gr->Box();	gr->Area(y,"cbgGyr");
	gr->SubPlot(2,2,2,"");	gr->Title("'!' style");	gr->Box();	gr->Area(y,"!");
	gr->SubPlot(2,2,3);	gr->Title("3d variant");	gr->Rotate(50,60);	gr->Box();
	mglData yc(30), xc(30), z(30);	z.Modify("2*x-1");
	yc.Modify("sin(pi*(2*x-1))");	xc.Modify("cos(pi*2*x-pi)");
	gr->Area(xc,yc,z,"r");
	yc.Modify("-sin(pi*(2*x-1))");	gr->Area(xc,yc,z,"b#");
}
//-----------------------------------------------------------------------------
const char *mmgl_plot="call 'prepare1d'\nsubplot 2 2 0 '':title 'Plot plot (default)':box:plot y\n"
"subplot 2 2 2 '':title ''!' style; 'rgb' palette':box:plot y 'o!rgb'\nsubplot 2 2 3 '':title 'just markers':box:plot y ' +'\n"
"new yc 30 'sin(pi*x)':new xc 30 'cos(pi*x)':new z 30 'x'\nsubplot 2 2 1:title '3d variant':rotate 50 60:box:plot xc yc z 'rs'";
void smgl_plot(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);	gr->SetOrigin(0,0,0);
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Plot plot (default)");	}
	gr->Box();	gr->Plot(y);
	if(big==3)	return;
	gr->SubPlot(2,2,2,"");	gr->Title("'!' style; 'rgb' palette");	gr->Box();	gr->Plot(y,"o!rgb");
	gr->SubPlot(2,2,3,"");	gr->Title("just markers");	gr->Box();	gr->Plot(y," +");
	gr->SubPlot(2,2,1);	gr->Title("3d variant");	gr->Rotate(50,60);	gr->Box();
	mglData yc(30), xc(30), z(30);	z.Modify("2*x-1");
	yc.Modify("sin(pi*(2*x-1))");	xc.Modify("cos(pi*2*x-pi)");
	gr->Plot(xc,yc,z,"rs");
}
//-----------------------------------------------------------------------------
const char *mmgl_tens="call 'prepare1d'\nsubplot 2 2 0 '':title 'Tens plot (default)':box:tens y(:,0) y(:,1)\n"
"subplot 2 2 2 '':title '\" \" style':box:tens y(:,0) y(:,1) 'o '\n"
"new yc 30 'sin(pi*x)':new xc 30 'cos(pi*x)':new z 30 'x'\n"
"subplot 2 2 1:title '3d variant':rotate 50 60:box:tens xc yc z z 's'";
void smgl_tens(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);	gr->SetOrigin(0,0,0);
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Tens plot (default)");	}
	gr->Box();	gr->Tens(y.SubData(-1,0), y.SubData(-1,1));
	if(big==3)	return;
	gr->SubPlot(2,2,2,"");	gr->Title("' ' style");	gr->Box();	gr->Tens(y.SubData(-1,0), y.SubData(-1,1),"o ");
	gr->SubPlot(2,2,1);	gr->Title("3d variant");	gr->Rotate(50,60);	gr->Box();
	mglData yc(30), xc(30), z(30);	z.Modify("2*x-1");
	yc.Modify("sin(pi*(2*x-1))");	xc.Modify("cos(pi*2*x-pi)");
	gr->Tens(xc,yc,z,z,"s");
}
//-----------------------------------------------------------------------------
const char *mmgl_region="call 'prepare1d'\ncopy y1 y(:,1):copy y2 y(:,2)\n"
"subplot 2 2 0 '':title 'Region plot (default)':box:region y1 y2:plot y1 'k2':plot y2 'k2'\n"
"subplot 2 2 1 '':title '2 colors':box:region y1 y2 'yr':plot y1 'k2':plot y2 'k2'\n"
"subplot 2 2 2 '':title '\"i\" style':box:region y1 y2 'ir':plot y1 'k2':plot y2 'k2'\n"
"subplot 2 2 3 '^_':title '3d variant':rotate 40 60:box\n"
"new x1 100 'sin(pi*x)':new y1 100 'cos(pi*x)':new z 100 'x'\n"
"new x2 100 'sin(pi*x+pi/3)':new y2 100 'cos(pi*x+pi/3)'\n"
"plot x1 y1 z 'r2':plot x2 y2 z 'b2'\nregion x1 y1 z x2 y2 z 'cmy!'";
void smgl_region(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);
	mglData y1 = y.SubData(-1,1), y2 = y.SubData(-1,2);	gr->SetOrigin(0,0,0);
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Region plot (default)");	}
	gr->Box();	gr->Region(y1,y2);	gr->Plot(y1,"k2");	gr->Plot(y2,"k2");
	if(big==3)	return;
	gr->SubPlot(2,2,1,"");	gr->Title("2 colors");	gr->Box();	gr->Region(y1,y2,"yr");	gr->Plot(y1,"k2");	gr->Plot(y2,"k2");
	gr->SubPlot(2,2,2,"");	gr->Title("'i' style");	gr->Box();	gr->Region(y1,y2,"ir");	gr->Plot(y1,"k2");	gr->Plot(y2,"k2");
	gr->SubPlot(2,2,3,"^_");	gr->Title("3d variant");	gr->Rotate(40,60);	gr->Box();
	gr->Fill(y1,"cos(pi*x)");	gr->Fill(y2,"cos(pi*x+pi/3)");
	mglData x1(y1.nx), x2(y1.nx), z(y1.nx);
	gr->Fill(x1,"sin(pi*x)");	gr->Fill(x2,"sin(pi*x+pi/3)");	gr->Fill(z,"x");
	gr->Plot(x1,y1,z,"r2");		gr->Plot(x2,y2,z,"b2");
	gr->Region(x1,y1,z,x2,y2,z,"cmy!");
}
//-----------------------------------------------------------------------------
const char *mmgl_stem="call 'prepare1d'\norigin 0 0 0:subplot 2 2 0 '':title 'Stem plot (default)':box:stem y\n"
"new yc 30 'sin(pi*x)':new xc 30 'cos(pi*x)':new z 30 'x'\nsubplot 2 2 1:title '3d variant':rotate 50 60:box:stem xc yc z 'rx'\n"
"subplot 2 2 2 '':title '\"!\" style':box:stem y 'o!rgb'";
void smgl_stem(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);	gr->SetOrigin(0,0,0);
	mglData yc(30), xc(30), z(30);	z.Modify("2*x-1");
	yc.Modify("sin(pi*(2*x-1))");	xc.Modify("cos(pi*2*x-pi)");
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Stem plot (default)");	}
	gr->Box();	gr->Stem(y);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("3d variant");	gr->Rotate(50,60);
	gr->Box();	gr->Stem(xc,yc,z,"rx");
	gr->SubPlot(2,2,2,"");	gr->Title("'!' style");	gr->Box();	gr->Stem(y,"o!rgb");
}
//-----------------------------------------------------------------------------
const char *mmgl_step="call 'prepare1d'\norigin 0 0 0:subplot 2 2 0 '':title 'Step plot (default)':box:step y\n"
"new yc 30 'sin(pi*x)':new xc 30 'cos(pi*x)':new z 30 'x'\nsubplot 2 2 1:title '3d variant':rotate 50 60:box:step xc yc z 'r'\n"
"subplot 2 2 2 '':title '\"!\" style':box:step y 's!rgb'";
void smgl_step(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);	gr->SetOrigin(0,0,0);
	mglData yc(30), xc(30), z(30);	z.Modify("2*x-1");
	yc.Modify("sin(pi*(2*x-1))");	xc.Modify("cos(pi*2*x-pi)");
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Step plot (default)");	}
	gr->Box();	gr->Step(y);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("3d variant");	gr->Rotate(50,60);
	gr->Box();	gr->Step(xc,yc,z,"r");
	gr->SubPlot(2,2,2,"");	gr->Title("'!' style");	gr->Box();	gr->Step(y,"s!rgb");
}
//-----------------------------------------------------------------------------
const char *mmgl_boxplot="new a 10 7 '(2*rnd-1)^3/2'\nsubplot 1 1 0 '':title 'Boxplot plot':box:boxplot a";
void smgl_boxplot(mglGraph *gr)	// flow threads and density plot
{
	mglData a(10,7);	a.Modify("(2*rnd-1)^3/2");
	if(big!=3)	{	gr->SubPlot(1,1,0,"");	gr->Title("Boxplot plot");	}
	gr->Box();	gr->BoxPlot(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_ohlc="new o 10 '0.5*sin(pi*x)'\nnew c 10 '0.5*sin(pi*(x+2/9))'\n"
"new l 10 '0.3*rnd-0.8'\nnew h 10 '0.3*rnd+0.5'\n"
"subplot 1 1 0 '':title 'OHLC plot':box:ohlc o h l c";
void smgl_ohlc(mglGraph *gr)	// flow threads and density plot
{
	mglData o(10), h(10), l(10), c(10);
	gr->Fill(o,"0.5*sin(pi*x)");	gr->Fill(c,"0.5*sin(pi*(x+2/9))");
	gr->Fill(l,"0.3*rnd-0.8");		gr->Fill(h,"0.3*rnd+0.5");
	if(big!=3)	{	gr->SubPlot(1,1,0,"");	gr->Title("OHLC plot");	}
	gr->Box();	gr->OHLC(o,h,l,c);
}
//-----------------------------------------------------------------------------
const char *mmgl_type0="call 'prepare2d'\nalpha on:light on:transptype 0:clf\nsubplot 2 2 0:rotate 50 60:surf a:box\n"
"subplot 2 2 1:rotate 50 60:dens a:box\nsubplot 2 2 2:rotate 50 60:cont a:box\n"
"subplot 2 2 3:rotate 50 60:axial a:box";
void smgl_type0(mglGraph *gr)	// TranspType = 0
{
	gr->Alpha(true);	gr->Light(true);
	mglData a;	mgls_prepare2d(&a);
	gr->SetTranspType(0);	gr->Clf();
	gr->SubPlot(2,2,0);	gr->Rotate(50,60);	gr->Surf(a);	gr->Box();
	gr->SubPlot(2,2,1);	gr->Rotate(50,60);	gr->Dens(a);	gr->Box();
	gr->SubPlot(2,2,2);	gr->Rotate(50,60);	gr->Cont(a);	gr->Box();
	gr->SubPlot(2,2,3);	gr->Rotate(50,60);	gr->Axial(a);	gr->Box();
}
//-----------------------------------------------------------------------------
const char *mmgl_type1="call 'prepare2d'\nalpha on:light on:transptype 1:clf\nsubplot 2 2 0:rotate 50 60:surf a:box\n"
"subplot 2 2 1:rotate 50 60:dens a:box\nsubplot 2 2 2:rotate 50 60:cont a:box\n"
"subplot 2 2 3:rotate 50 60:axial a:box";
void smgl_type1(mglGraph *gr)	// TranspType = 1
{
	gr->Alpha(true);	gr->Light(true);
	mglData a;	mgls_prepare2d(&a);
	gr->SetTranspType(1);	gr->Clf();
	gr->SubPlot(2,2,0);	gr->Rotate(50,60);	gr->Surf(a);	gr->Box();
	gr->SubPlot(2,2,1);	gr->Rotate(50,60);	gr->Dens(a);	gr->Box();
	gr->SubPlot(2,2,2);	gr->Rotate(50,60);	gr->Cont(a);	gr->Box();
	gr->SubPlot(2,2,3);	gr->Rotate(50,60);	gr->Axial(a);	gr->Box();
}
//-----------------------------------------------------------------------------
const char *mmgl_type2="call 'prepare2d'\nalpha on:light on:transptype 2:clf\nsubplot 2 2 0:rotate 50 60:surf a:box\n"
"subplot 2 2 1:rotate 50 60:dens a:box\nsubplot 2 2 2:rotate 50 60:cont a:box\n"
"subplot 2 2 3:rotate 50 60:axial a:box";
void smgl_type2(mglGraph *gr)	// TranspType = 2
{
	gr->Alpha(true);	gr->Light(true);
	mglData a;	mgls_prepare2d(&a);
	gr->SetTranspType(2);	gr->Clf();
	gr->SubPlot(2,2,0);	gr->Rotate(50,60);	gr->Surf(a);	gr->Box();
	gr->SubPlot(2,2,1);	gr->Rotate(50,60);	gr->Dens(a);	gr->Box();
	gr->SubPlot(2,2,2);	gr->Rotate(50,60);	gr->Cont(a);	gr->Box();
	gr->SubPlot(2,2,3);	gr->Rotate(50,60);	gr->Axial(a);	gr->Box();
}
//-----------------------------------------------------------------------------
const char *mmgl_molecule="alpha on:light on\n"
"subplot 2 2 0 '':title 'Methane, CH_4':rotate 60 120\n"
"sphere 0 0 0 0.25 'k':drop 0 0 0 0 0 1 0.35 'h' 1 2:sphere 0 0 0.7 0.25 'g'\n"
"drop 0 0 0 -0.94 0 -0.33 0.35 'h' 1 2:sphere -0.66 0 -0.23 0.25 'g'\n"
"drop 0 0 0 0.47 0.82 -0.33 0.35 'h' 1 2:sphere 0.33 0.57 -0.23 0.25 'g'\n"
"drop 0 0 0 0.47 -0.82 -0.33 0.35 'h' 1 2:sphere 0.33 -0.57 -0.23 0.25 'g'\n"
"subplot 2 2 1 '':title 'Water, H{_2}O':rotate 60 100\n"
"sphere 0 0 0 0.25 'r':drop 0 0 0 0.3 0.5 0 0.3 'm' 1 2:sphere 0.3 0.5 0 0.25 'g'\n"
"drop 0 0 0 0.3 -0.5 0 0.3 'm' 1 2:sphere 0.3 -0.5 0 0.25 'g'\n"
"subplot 2 2 2 '':title 'Oxygen, O_2':rotate 60 120\n"
"drop 0 0.5 0 0 -0.3 0 0.3 'm' 1 2:sphere 0 0.5 0 0.25 'r'\n"
"drop 0 -0.5 0 0 0.3 0 0.3 'm' 1 2:sphere 0 -0.5 0 0.25 'r'\n"
"subplot 2 2 3 '':title 'Ammonia, NH_3':rotate 60 120\n"
"sphere 0 0 0 0.25 'b':drop 0 0 0 0.33 0.57 0 0.32 'n' 1 2\n"
"sphere 0.33 0.57 0 0.25 'g':drop 0 0 0 0.33 -0.57 0 0.32 'n' 1 2\n"
"sphere 0.33 -0.57 0 0.25 'g':drop 0 0 0 -0.65 0 0 0.32 'n' 1 2\n"
"sphere -0.65 0 0 0.25 'g'";
void smgl_molecule(mglGraph *gr)	// example of moleculas
{
	gr->VertexColor(false);	gr->Compression(false); // per-vertex colors and compression are detrimental to transparency
	gr->DoubleSided(false); // we do not get into atoms, while rendering internal surface has negative impact on trasparency
	gr->Alpha(true);	gr->Light(true);

	gr->SubPlot(2,2,0,"");	gr->Title("Methane, CH_4");
	gr->StartGroup("Methane");
	gr->Rotate(60,120);
	gr->Sphere(mglPoint(0,0,0),0.25,"k");
	gr->Drop(mglPoint(0,0,0),mglPoint(0,0,1),0.35,"h",1,2);
	gr->Sphere(mglPoint(0,0,0.7),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(-0.94,0,-0.33),0.35,"h",1,2);
	gr->Sphere(mglPoint(-0.66,0,-0.23),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.47,0.82,-0.33),0.35,"h",1,2);
	gr->Sphere(mglPoint(0.33,0.57,-0.23),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.47,-0.82,-0.33),0.35,"h",1,2);
	gr->Sphere(mglPoint(0.33,-0.57,-0.23),0.25,"g");
	gr->EndGroup();

	gr->SubPlot(2,2,1,"");	gr->Title("Water, H_{2}O");
	gr->StartGroup("Water");
	gr->Rotate(60,100);
	gr->StartGroup("Water_O");
	gr->Sphere(mglPoint(0,0,0),0.25,"r");
	gr->EndGroup();
	gr->StartGroup("Water_Bond_1");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.3,0.5,0),0.3,"m",1,2);
	gr->EndGroup();
	gr->StartGroup("Water_H_1");
	gr->Sphere(mglPoint(0.3,0.5,0),0.25,"g");
	gr->EndGroup();
	gr->StartGroup("Water_Bond_2");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.3,-0.5,0),0.3,"m",1,2);
	gr->EndGroup();
	gr->StartGroup("Water_H_2");
	gr->Sphere(mglPoint(0.3,-0.5,0),0.25,"g");
	gr->EndGroup();
	gr->EndGroup();

	gr->SubPlot(2,2,2,"");	gr->Title("Oxygen, O_2");
	gr->StartGroup("Oxygen");
	gr->Rotate(60,120);
	gr->Drop(mglPoint(0,0.5,0),mglPoint(0,-0.3,0),0.3,"m",1,2);
	gr->Sphere(mglPoint(0,0.5,0),0.25,"r");
	gr->Drop(mglPoint(0,-0.5,0),mglPoint(0,0.3,0),0.3,"m",1,2);
	gr->Sphere(mglPoint(0,-0.5,0),0.25,"r");
	gr->EndGroup();

	gr->SubPlot(2,2,3,"");	gr->Title("Ammonia, NH_3");
	gr->StartGroup("Ammonia");
	gr->Rotate(60,120);
	gr->Sphere(mglPoint(0,0,0),0.25,"b");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.33,0.57,0),0.32,"n",1,2);
	gr->Sphere(mglPoint(0.33,0.57,0),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.33,-0.57,0),0.32,"n",1,2);
	gr->Sphere(mglPoint(0.33,-0.57,0),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(-0.65,0,0),0.32,"n",1,2);
	gr->Sphere(mglPoint(-0.65,0,0),0.25,"g");
	gr->EndGroup();
	gr->DoubleSided( true ); // put back
}
//-----------------------------------------------------------------------------
const char *mmgl_error2="new x0 10 'rnd':new ex 10 '0.1'\nnew y0 10 'rnd':new ey 10 '0.1'\nranges 0 1 0 1\n"
"subplot 4 3 0 '':box:error x0 y0 ex ey '#+@'\n"
"subplot 4 3 1 '':box:error x0 y0 ex ey '#x@'\n"
"subplot 4 3 2 '':box:error x0 y0 ex ey '#s@'; alpha 0.5\n"
"subplot 4 3 3 '':box:error x0 y0 ex ey 's@'\n"
"subplot 4 3 4 '':box:error x0 y0 ex ey 'd@'\n"
"subplot 4 3 5 '':box:error x0 y0 ex ey '#d@'; alpha 0.5\n"
"subplot 4 3 6 '':box:error x0 y0 ex ey '+@'\n"
"subplot 4 3 7 '':box:error x0 y0 ex ey 'x@'\n"
"subplot 4 3 8 '':box:error x0 y0 ex ey 'o@'\n"
"subplot 4 3 9 '':box:error x0 y0 ex ey '#o@'; alpha 0.5\n"
"subplot 4 3 10 '':box:error x0 y0 ex ey '#.@'\n"
"subplot 4 3 11 '':box:error x0 y0 ex ey; alpha 0.5";
void smgl_error2(mglGraph *gr)
{
	mglData x0(10), y0(10), ex(10), ey(10);
	for(int i=0;i<10;i++)
	{	x0.a[i] = mgl_rnd();	y0.a[i] = mgl_rnd();	ey.a[i] = ex.a[i] = 0.1;	}
	gr->SetRanges(0,1,0,1);	gr->Alpha(true);
	gr->SubPlot(4,3,0,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"#+@");
	gr->SubPlot(4,3,1,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"#x@");
	gr->SubPlot(4,3,2,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"#s@","alpha 0.5");
	gr->SubPlot(4,3,3,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"s@");
	gr->SubPlot(4,3,4,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"d@");
	gr->SubPlot(4,3,5,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"#d@","alpha 0.5");
	gr->SubPlot(4,3,6,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"+@");
	gr->SubPlot(4,3,7,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"x@");
	gr->SubPlot(4,3,8,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"o@");
	gr->SubPlot(4,3,9,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"#o@","alpha 0.5");
	gr->SubPlot(4,3,10,"");	gr->Box();	gr->Error(x0,y0,ex,ey,"#.@");
	gr->SubPlot(4,3,11,"");	gr->Box();	gr->Error(x0,y0,ex,ey);
}
//-----------------------------------------------------------------------------
const char *mmgl_error="call 'prepare1d'\nnew y 50 '0.7*sin(pi*x-pi) + 0.5*cos(3*pi*(x+1)/2) + 0.2*sin(pi*(x+1)/2)'\n"
"new x0 10 'x + 0.1*rnd-0.05':new ex 10 '0.1':new ey 10 '0.2'\n"
"new y0 10 '0.7*sin(pi*x-pi) + 0.5*cos(3*pi*(x+1)/2) + 0.2*sin(pi*(x+1)/2) + 0.2*rnd-0.1'\n"
"subplot 2 2 0 '':title 'Error plot (default)':box:plot y:error x0 y0 ex ey 'k'\n"
"subplot 2 2 1 '':title '\"!\" style; no e_x':box:plot y:error x0 y0 ey 'o!rgb'\n"
"subplot 2 2 2 '':title '\"\\@\" style':alpha on:box:plot y:error x0 y0 ex ey '@'; alpha 0.5\n"
"subplot 2 2 3:title '3d variant':rotate 50 60:axis\n"
"for $1 0 9\n\terrbox 2*rnd-1 2*rnd-1 2*rnd-1 0.2 0.2 0.2 'bo'\nnext";
void smgl_error(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);
	mglData x0(10), y0(10), ex0(10), ey0(10);
	for(int i=0;i<10;i++)
	{
		double x = i/9.;
		x0.a[i] = 2*x-1 + 0.1*mgl_rnd()-0.05;
		y0.a[i] = 0.7*sin(2*M_PI*x)+0.5*cos(3*M_PI*x)+0.2*sin(M_PI*x)+0.2*mgl_rnd()-0.1;
		ey0.a[i]=0.2;	ex0.a[i]=0.1;
	}
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Error plot (default)");	}
	gr->Box();	gr->Plot(y.SubData(-1,0));	gr->Error(x0,y0,ex0,ey0,"ko");
	if(big==3)	return;
	gr->SubPlot(2,2,1,"");	gr->Title("'!' style; no e_x");
	gr->Box();	gr->Plot(y.SubData(-1,0));	gr->Error(x0,y0,ey0,"o!rgb");
	gr->SubPlot(2,2,2,"");	gr->Title("'\\@' style");	gr->Alpha(true);
	gr->Box();	gr->Plot(y.SubData(-1,0));	gr->Error(x0,y0,ex0,ey0,"@","alpha 0.5");
	gr->SubPlot(2,2,3);	gr->Title("3d variant");	gr->Rotate(50,60);
	for(int i=0;i<10;i++)
		gr->Error(mglPoint(2*mgl_rnd()-1,2*mgl_rnd()-1,2*mgl_rnd()-1), mglPoint(0.2,0.2,0.2),"bo");
	gr->Axis();
}
//-----------------------------------------------------------------------------
const char *mmgl_chart="new ch 7 2 'rnd+0.1':light on\n"
"subplot 2 2 0:title 'Chart plot (default)':rotate 50 60:box:chart ch\n"
"subplot 2 2 1:title '\"\\#\" style':rotate 50 60:box:chart ch '#'\n"
"subplot 2 2 2:title 'Pie chart; \" \" color':rotate 50 60:\n"
"axis '(y+1)/2*cos(pi*x)' '(y+1)/2*sin(pi*x)' '':box:chart ch 'bgr cmy#'\n"
"subplot 2 2 3:title 'Ring chart; \" \" color':rotate 50 60:\n"
"axis '(y+2)/3*cos(pi*x)' '(y+2)/3*sin(pi*x)' '':box:chart ch 'bgr cmy#'";
void smgl_chart(mglGraph *gr)
{
	mglData ch(7,2);	for(int i=0;i<7*2;i++)	ch.a[i]=mgl_rnd()+0.1;
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Chart plot (default)");	}
	gr->Light(true);	gr->Rotate(50,60);	gr->Box();	gr->Chart(ch);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'\\#' style");
	gr->Rotate(50,60);	gr->Box();	gr->Chart(ch,"#");
	gr->SubPlot(2,2,2);	gr->Title("Pie chart; ' ' color");
	gr->SetFunc("(y+1)/2*cos(pi*x)","(y+1)/2*sin(pi*x)","");
	gr->Rotate(50,60);	gr->Box();	gr->Chart(ch,"bgr cmy#");
	gr->SubPlot(2,2,3);	gr->Title("Ring chart; ' ' color");
	gr->SetFunc("(y+2)/3*cos(pi*x)","(y+2)/3*sin(pi*x)","");
	gr->Rotate(50,60);	gr->Box();	gr->Chart(ch,"bgr cmy#");
}
//-----------------------------------------------------------------------------
const char *mmgl_mark="call 'prepare1d'\nsubplot 1 1 0 '':title 'Mark plot (default)':box:mark y y1 's'";
void smgl_mark(mglGraph *gr)
{
	mglData y,y1;	mgls_prepare1d(&y,&y1);
	if(big!=3)	{	gr->SubPlot(1,1,0,"");	gr->Title("Mark plot (default)");	}
	gr->Box();	gr->Mark(y,y1,"s");
}
//-----------------------------------------------------------------------------
const char *mmgl_radar="new yr 10 3 '0.4*sin(pi*(x+1.5+y/2)+0.1*rnd)'\n"
"subplot 1 1 0 '':title 'Radar plot (with grid, \"\\#\")':radar yr '#'";
void smgl_radar(mglGraph *gr)
{
	mglData yr(10,3);	yr.Modify("0.4*sin(pi*(2*x+y))+0.1*rnd");
	if(big!=3)	{	gr->SubPlot(1,1,0,"");	gr->Title("Radar plot (with grid, '\\#')");	}
	gr->Radar(yr,"#");
}
//-----------------------------------------------------------------------------
const char *mmgl_candle="new y 30 'sin(pi*x/2)^2'\n"
"subplot 1 1 0 '':title 'Candle plot (default)'\nyrange 0 1:box\ncandle y y/2 (y+1)/2";
void smgl_candle(mglGraph *gr)
{
	mglData y(30);	gr->Fill(y,"sin(pi*x/2)^2");
	mglData y1(30);	gr->Fill(y1,"v/2",y);
	mglData y2(30);	gr->Fill(y2,"(1+v)/2",y);
	if(big!=3)	{	gr->SubPlot(1,1,0,"");	gr->Title("Candle plot (default)");	}
	gr->SetRange('y',0,1);	gr->Box();	gr->Candle(y,y1,y2);
}
//-----------------------------------------------------------------------------
const char *mmgl_textmark="call 'prepare1d'\nsubplot 1 1 0 '':title 'TextMark plot (default)':box:textmark y y1 '\\gamma' 'r'";
void smgl_textmark(mglGraph *gr)
{
	mglData y,y1;	mgls_prepare1d(&y,&y1);
	if(big!=3)	{	gr->SubPlot(1,1,0,"");	gr->Title("TextMark plot (default)");	}
	gr->Box();	gr->TextMark(y,y1,"\\gamma","r");
}
//-----------------------------------------------------------------------------
const char *mmgl_tube="call 'prepare1d'\nlight on\n"
"new yc 50 'sin(pi*x)':new xc 50 'cos(pi*x)':new z 50 'x':divto y1 20\n"
"subplot 2 2 0 '':title 'Tube plot (default)':box:tube y 0.05\n"
"subplot 2 2 1 '':title 'variable radius':box:tube y y1\n"
"subplot 2 2 2 '':title '\"\\#\" style':box:tube y 0.05 '#'\n"
"subplot 2 2 3:title '3d variant':rotate 50 60:box:tube xc yc z y2 'r'";
void smgl_tube(mglGraph *gr)
{
	mglData y,y1,y2;	mgls_prepare1d(&y,&y1,&y2);	y1/=20;
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Tube plot (default)");	}
	gr->Light(true);	gr->Box();	gr->Tube(y,0.05);
	if(big==3)	return;
	gr->SubPlot(2,2,1,"");	gr->Title("variable radius");	gr->Box();	gr->Tube(y,y1);
	gr->SubPlot(2,2,2,"");	gr->Title("'\\#' style");	gr->Box();	gr->Tube(y,0.05,"#");
	mglData yc(50), xc(50), z(50);	z.Modify("2*x-1");
	yc.Modify("sin(pi*(2*x-1))");	xc.Modify("cos(pi*2*x-pi)");
	gr->SubPlot(2,2,3);	gr->Title("3d variant");	gr->Rotate(50,60);	gr->Box();	gr->Tube(xc,yc,z,y2,"r");
}
//-----------------------------------------------------------------------------
const char *mmgl_tape="call 'prepare1d'\nnew yc 50 'sin(pi*x)':new xc 50 'cos(pi*x)':new z 50 'x'\n"
"subplot 2 2 0 '':title 'Tape plot (default)':box:tape y:plot y 'k'\n"
"subplot 2 2 1:title '3d variant, 2 colors':rotate 50 60:light on\n"
"box:plot xc yc z 'k':tape xc yc z 'rg'\n"
"subplot 2 2 2:title '3d variant, x only':rotate 50 60\n"
"box:plot xc yc z 'k':tape xc yc z 'xr':tape xc yc z 'xr#'\n"
"subplot 2 2 3:title '3d variant, z only':rotate 50 60\n"
"box:plot xc yc z 'k':tape xc yc z 'zg':tape xc yc z 'zg#'";
void smgl_tape(mglGraph *gr)
{
	mglData y;	mgls_prepare1d(&y);
	mglData xc(50), yc(50), z(50);
	yc.Modify("sin(pi*(2*x-1))");
	xc.Modify("cos(pi*2*x-pi)");	z.Fill(-1,1);
	if(big!=3)	{	gr->SubPlot(2,2,0,"");	gr->Title("Tape plot (default)");	}
	gr->Box();	gr->Tape(y);	gr->Plot(y,"k");
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("3d variant, 2 colors");	gr->Rotate(50,60);	gr->Light(true);
	gr->Box();	gr->Plot(xc,yc,z,"k");	gr->Tape(xc,yc,z,"rg");
	gr->SubPlot(2,2,2);	gr->Title("3d variant, x only");	gr->Rotate(50,60);
	gr->Box();	gr->Plot(xc,yc,z,"k");	gr->Tape(xc,yc,z,"xr");	gr->Tape(xc,yc,z,"xr#");
	gr->SubPlot(2,2,3);	gr->Title("3d variant, z only");	gr->Rotate(50,60);
	gr->Box();	gr->Plot(xc,yc,z,"k");	gr->Tape(xc,yc,z,"zg");	gr->Tape(xc,yc,z,"zg#");
}
//-----------------------------------------------------------------------------
const char *mmgl_fog="call 'prepare2d'\ntitle 'Fog sample':rotate 50 60:light on:fog 1\nbox:surf a:cont a 'y'";
void smgl_fog(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	gr->Title("Fog sample");
	gr->Light(true);	gr->Rotate(50,60);	gr->Fog(1);	gr->Box();
	gr->Surf(a);	gr->Cont(a,"y");
}
//-----------------------------------------------------------------------------
const char *mmgl_map="new a 50 40 'x':new b 50 40 'y':zrange -2 2:text 0 0 '\\to'\n"
"subplot 2 1 0:text 0 1.1 '\\{x, y\\}' '' -2:box:map a b 'brgk'\n"
"subplot 2 1 1:text 0 1.1 '\\{\\frac{x^3+y^3}{2}, \\frac{x-y}{2}\\}' '' -2\n"
"box:fill a '(x^3+y^3)/2':fill b '(x-y)/2':map a b 'brgk'";
void smgl_map(mglGraph *gr)	// example of mapping
{
	mglData a(50, 40), b(50, 40);
	gr->Puts(mglPoint(0, 0), "\\to", ":C", -1.4);
	gr->SetRanges(-1,1,-1,1,-2,2);

	gr->SubPlot(2, 1, 0);
	gr->Fill(a,"x");	gr->Fill(b,"y");
	gr->Puts(mglPoint(0, 1.1), "\\{x, y\\}", ":C", -2);		gr->Box();
	gr->Map(a, b, "brgk");

	gr->SubPlot(2, 1, 1);
	gr->Fill(a,"(x^3+y^3)/2");	gr->Fill(b,"(x-y)/2");
	gr->Puts(mglPoint(0, 1.1), "\\{\\frac{x^3+y^3}{2}, \\frac{x-y}{2}\\}", ":C", -2);
	gr->Box();
	gr->Map(a, b, "brgk");
}
//-----------------------------------------------------------------------------
const char *mmgl_stfa="new a 2000:new b 2000\nfill a 'cos(50*pi*x)*(x<-.5)+cos(100*pi*x)*(x<0)*(x>-.5)+\\\n"
"cos(200*pi*x)*(x<.5)*(x>0)+cos(400*pi*x)*(x>.5)'\n"
"subplot 1 2 0 '<_':title 'Initial signal':plot a:axis:xlabel '\\i t'\n"
"subplot 1 2 1 '<_':title 'STFA plot':stfa a b 64:axis:ylabel '\\omega' 0:xlabel '\\i t'";
void smgl_stfa(mglGraph *gr)	// STFA sample
{
	mglData a(2000), b(2000);
	gr->Fill(a,"cos(50*pi*x)*(x<-.5)+cos(100*pi*x)*(x<0)*(x>-.5)+\
	cos(200*pi*x)*(x<.5)*(x>0)+cos(400*pi*x)*(x>.5)");
	gr->SubPlot(1, 2, 0,"<_");	gr->Title("Initial signal");
	gr->Plot(a);
	gr->Axis();
	gr->Label('x', "\\i t");

	gr->SubPlot(1, 2, 1,"<_");	gr->Title("STFA plot");
	gr->STFA(a, b, 64);
	gr->Axis();
	gr->Label('x', "\\i t");
	gr->Label('y', "\\omega", 0);
}
//-----------------------------------------------------------------------------
const char *mmgl_qo2d="define $1 'p^2+q^2-x-1+i*0.5*(y+x)*(y>-x)'\n"
"subplot 1 1 0 '<_':title 'Beam and ray tracing'\n"
"ray r $1 -0.7 -1 0 0 0.5 0 0.02 2:plot r(0) r(1) 'k'\naxis:xlabel '\\i x':ylabel '\\i z'\n"
"new re 128 'exp(-48*x^2)':new im 128\nnew xx 1:new yy 1\nqo2d a $1 re im r 1 30 xx yy\n"
"crange 0 1:dens xx yy a 'wyrRk':fplot '-x' 'k|'\n"
"text 0 0.85 'absorption: (x+y)/2 for x+y>0'\ntext 0.7 -0.05 'central ray'";
void smgl_qo2d(mglGraph *gr)
{
	mglData r, xx, yy, a, im(128), re(128);
	const char *ham = "p^2+q^2-x-1+i*0.5*(y+x)*(y>-x)";
	r = mglRay(ham, mglPoint(-0.7, -1), mglPoint(0, 0.5), 0.02, 2);
	if(big!=3)	{gr->SubPlot(1,1,0,"<_");	gr->Title("Beam and ray tracing");}
	gr->Plot(r.SubData(0), r.SubData(1), "k");
	gr->Axis();	gr->Label('x', "\\i x");	gr->Label('y', "\\i y");
	// now start beam tracing
	gr->Fill(re,"exp(-48*x^2)");
	a = mglQO2d(ham, re, im, r, xx, yy, 1, 30);
	gr->SetRange('c',0, 1);
	gr->Dens(xx, yy, a, "wyrRk");
	gr->FPlot("-x", "k|");
	gr->Puts(mglPoint(0, 0.85), "absorption: (x+y)/2 for x+y>0");
	gr->Puts(mglPoint(0.7, -0.05), "central ray");
}
//-----------------------------------------------------------------------------
const char *mmgl_pde="new re 128 'exp(-48*(x+0.7)^2)':new im 128\n"
"pde a 'p^2+q^2-x-1+i*0.5*(z+x)*(z>-x)' re im 0.01 30\ntranspose a\n"
"subplot 1 1 0 '<_':title 'PDE solver'\n"
"axis:xlabel '\\i x':ylabel '\\i z'\ncrange 0 1:dens a 'wyrRk'\n"
"fplot '-x' 'k|'\n"
"text 0 0.95 'Equation: ik_0\\partial_zu + \\Delta u + x\\cdot u + i \\frac{x+z}{2}\\cdot u = 0\\n{}absorption: (x+z)/2 for x+z>0'";
void smgl_pde(mglGraph *gr)	// PDE sample
{
	mglData a,re(128),im(128);
	gr->Fill(re,"exp(-48*(x+0.7)^2)");
	a = gr->PDE("p^2+q^2-x-1+i*0.5*(z+x)*(z>-x)", re, im, 0.01, 30);
	a.Transpose("yxz");
	if(big!=3)	{gr->SubPlot(1,1,0,"<_");	gr->Title("PDE solver");	}
	gr->SetRange('c',0,1);	gr->Dens(a,"wyrRk");
	gr->Axis();	gr->Label('x', "\\i x");	gr->Label('y', "\\i z");
	gr->FPlot("-x", "k|");
	gr->Puts(mglPoint(0, 0.95), "Equation: ik_0\\partial_zu + \\Delta u + x\\cdot u + i \\frac{x+z}{2}\\cdot u = 0\nabsorption: (x+z)/2 for x+z>0");
}
//-----------------------------------------------------------------------------
const char *mmgl_cont3="call 'prepare3d'\ntitle 'Cont3 sample':rotate 50 60:box\ncont3 c 'x':cont3 c:cont3 c 'z'";
void smgl_cont3(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	gr->Title("Cont3 sample");
	gr->Rotate(50,60);	gr->Box();
	gr->Cont3(c,"x");	gr->Cont3(c);	gr->Cont3(c,"z");
}
//-----------------------------------------------------------------------------
const char *mmgl_contf3="call 'prepare3d'\ntitle 'Cont3 sample':rotate 50 60:box:light on\n"
"contf3 c 'x':contf3 c:contf3 c 'z'\ncont3 c 'xk':cont3 c 'k':cont3 c 'zk'";
void smgl_contf3(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	gr->Title("ContF3 sample");
	gr->Rotate(50,60);	gr->Light(true);	gr->Box();
	gr->ContF3(c,"x");	gr->ContF3(c);		gr->ContF3(c,"z");
	gr->Cont3(c,"kx");	gr->Cont3(c,"k");	gr->Cont3(c,"kz");
}
//-----------------------------------------------------------------------------
const char *mmgl_dens3="call 'prepare3d'\ntitle 'Dens3 sample':rotate 50 60:alpha on:alphadef 0.7\n"
"origin 0 0 0:box:axis '_xyz'\ndens3 c 'x':dens3 c ':y':dens3 c 'z'";
void smgl_dens3(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	gr->Title("Dens3 sample");
	gr->Rotate(50,60);	gr->Alpha(true);	gr->SetAlphaDef(0.7);
	gr->SetOrigin(0,0,0);	gr->Axis("_xyz");	gr->Box();
	gr->Dens3(c,"x");	gr->Dens3(c);	gr->Dens3(c,"z");
}
//-----------------------------------------------------------------------------
const char *mmgl_dens_xyz="call 'prepare3d'\ntitle 'Dens[XYZ] sample':rotate 50 60:box\n"
"densx {sum c 'x'} '' -1:densy {sum c 'y'} '' 1:densz {sum c 'z'} '' -1";
void smgl_dens_xyz(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	gr->Title("Dens[XYZ] sample");
	gr->Rotate(50,60);	gr->Box();	gr->DensX(c.Sum("x"),0,-1);
	gr->DensY(c.Sum("y"),0,1);		gr->DensZ(c.Sum("z"),0,-1);
}
//-----------------------------------------------------------------------------
const char *mmgl_cont_xyz="call 'prepare3d'\ntitle 'Cont[XYZ] sample':rotate 50 60:box\n"
"contx {sum c 'x'} '' -1:conty {sum c 'y'} '' 1:contz {sum c 'z'} '' -1";
void smgl_cont_xyz(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	gr->Title("Cont[XYZ] sample");
	gr->Rotate(50,60);	gr->Box();	gr->ContX(c.Sum("x"),"",-1);
	gr->ContY(c.Sum("y"),"",1);		gr->ContZ(c.Sum("z"),"",-1);
}
//-----------------------------------------------------------------------------
const char *mmgl_contf_xyz="call 'prepare3d'\ntitle 'ContF[XYZ] sample':rotate 50 60:box\n"
"contfx {sum c 'x'} '' -1:contfy {sum c 'y'} '' 1:contfz {sum c 'z'} '' -1";
void smgl_contf_xyz(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	gr->Title("ContF[XYZ] sample");
	gr->Rotate(50,60);	gr->Box();	gr->ContFX(c.Sum("x"),"",-1);
	gr->ContFY(c.Sum("y"),"",1);	gr->ContFZ(c.Sum("z"),"",-1);
}
//-----------------------------------------------------------------------------
const char *mmgl_cloud="call 'prepare3d'\nsubplot 2 2 0:title 'Cloud plot':rotate 50 60:alpha on:box:cloud c 'wyrRk'\n"
"subplot 2 2 1:title '\"i\" style':rotate 50 60:box:cloud c 'iwyrRk'\n"
"subplot 2 2 2:title '\".\" style':rotate 50 60:box:cloud c '.wyrRk'\n"
"subplot 2 2 3:title 'meshnum 10':rotate 50 60:box:cloud c 'wyrRk'; meshnum 10";
void smgl_cloud(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Cloud plot");	}
	gr->Rotate(50,60);	gr->Alpha(true);
	gr->Box();	gr->Cloud(c,"wyrRk");
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'i' style");
	gr->Rotate(50,60);	gr->Box();	gr->Cloud(c,"iwyrRk");
	gr->SubPlot(2,2,2);	gr->Title("'.' style");
	gr->Rotate(50,60);	gr->Box();	gr->Cloud(c,".wyrRk");
	gr->SubPlot(2,2,3);	gr->Title("meshnum 10");
	gr->Rotate(50,60);	gr->Box();	gr->Cloud(c,"wyrRk","meshnum 10");
}
//-----------------------------------------------------------------------------
const char *mmgl_cont="call 'prepare2d'\nlist v -0.5 -0.15 0 0.15 0.5\nsubplot 2 2 0:title 'Cont plot (default)':rotate 50 60:box:cont a\n"
"subplot 2 2 1:title 'manual levels':rotate 50 60:box:cont v a\n"
"subplot 2 2 2:title '\"\\_\" and \".\" styles':rotate 50 60:box:cont a '_':cont a '_.2k'\n"
"subplot 2 2 3 '':title '\"t\" style':box:cont a 't'";
void smgl_cont(mglGraph *gr)
{
	mglData a,v(5);	mgls_prepare2d(&a);	v.a[0]=-0.5;	v.a[1]=-0.15;	v.a[2]=0;	v.a[3]=0.15;	v.a[4]=0.5;
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Cont plot (default)");	}
	gr->Rotate(50,60);	gr->Box();	gr->Cont(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("manual levels");
	gr->Rotate(50,60);	gr->Box();	gr->Cont(v,a);
	gr->SubPlot(2,2,2);	gr->Title("'\\_' and '.' styles");
	gr->Rotate(50,60);	gr->Box();	gr->Cont(a,"_");	gr->Cont(a,"_.2k");
	gr->SubPlot(2,2,3,"");	gr->Title("'t' style");
	gr->Box();	gr->Cont(a,"t");
}
//-----------------------------------------------------------------------------
const char *mmgl_contf="call 'prepare2d'\nlist v -0.5 -0.15 0 0.15 0.5\n"
"new a1 30 40 3 '0.6*sin(2*pi*x+pi*(z+1)/2)*sin(3*pi*y+pi*z) + 0.4*cos(3*pi*(x*y)+pi*(z+1)^2/2)'\n"
"subplot 2 2 0:title 'ContF plot (default)':rotate 50 60:box:contf a\n"
"subplot 2 2 1:title 'manual levels':rotate 50 60:box:contf v a\n"
"subplot 2 2 2:title '\"\\_\" style':rotate 50 60:box:contf a '_'\n"
"subplot 2 2 3:title 'several slices':rotate 50 60:box:contf a1";
void smgl_contf(mglGraph *gr)
{
	mglData a,v(5),a1(30,40,3);	mgls_prepare2d(&a);	v.a[0]=-0.5;
	v.a[1]=-0.15;	v.a[2]=0;	v.a[3]=0.15;	v.a[4]=0.5;
	gr->Fill(a1,"0.6*sin(2*pi*x+pi*(z+1)/2)*sin(3*pi*y+pi*z) + 0.4*cos(3*pi*(x*y)+pi*(z+1)^2/2)");

	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("ContF plot (default)");	}
	gr->Rotate(50,60);	gr->Box();	gr->ContF(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("manual levels");
	gr->Rotate(50,60);	gr->Box();	gr->ContF(v,a);
	gr->SubPlot(2,2,2);	gr->Title("'\\_' style");
	gr->Rotate(50,60);	gr->Box();	gr->ContF(a,"_");
	gr->SubPlot(2,2,3);	gr->Title("several slices");
	gr->Rotate(50,60);	gr->Box();	gr->ContF(a1);
}
//-----------------------------------------------------------------------------
const char *mmgl_contd="call 'prepare2d'\nlist v -0.5 -0.15 0 0.15 0.5\n"
"new a1 30 40 3 '0.6*sin(2*pi*x+pi*(z+1)/2)*sin(3*pi*y+pi*z) + 0.4*cos(3*pi*(x*y)+pi*(z+1)^2/2)'\n"
"subplot 2 2 0:title 'ContD plot (default)':rotate 50 60:box:contd a\n"
"subplot 2 2 1:title 'manual levels':rotate 50 60:box:contd v a\n"
"subplot 2 2 2:title '\"\\_\" style':rotate 50 60:box:contd a '_'\n"
"subplot 2 2 3:title 'several slices':rotate 50 60:box:contd a1";
void smgl_contd(mglGraph *gr)
{
	mglData a,v(5),a1(30,40,3);	mgls_prepare2d(&a);	v.a[0]=-0.5;
	v.a[1]=-0.15;	v.a[2]=0;	v.a[3]=0.15;	v.a[4]=0.5;
	gr->Fill(a1,"0.6*sin(2*pi*x+pi*(z+1)/2)*sin(3*pi*y+pi*z) + 0.4*cos(3*pi*(x*y)+pi*(z+1)^2/2)");

	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("ContD plot (default)");	}
	gr->Rotate(50,60);	gr->Box();	gr->ContD(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("manual levels");
	gr->Rotate(50,60);	gr->Box();	gr->ContD(v,a);
	gr->SubPlot(2,2,2);	gr->Title("'\\_' style");
	gr->Rotate(50,60);	gr->Box();	gr->ContD(a,"_");
	gr->SubPlot(2,2,3);	gr->Title("several slices");
	gr->Rotate(50,60);	gr->Box();	gr->ContD(a1);
}
//-----------------------------------------------------------------------------
const char *mmgl_contv="call 'prepare2d'\nlist v -0.5 -0.15 0 0.15 0.5\n"
"subplot 2 2 0:title 'ContV plot (default)':rotate 50 60:box:contv a\n"
"subplot 2 2 1:title 'manual levels':rotate 50 60:box:contv v a\n"
"subplot 2 2 2:title '\"\\_\" style':rotate 50 60:box:contv a '_'\n"
"subplot 2 2 3:title 'ContV and ContF':rotate 50 60:light on:box\ncontv a:contf a:cont a 'k'";
void smgl_contv(mglGraph *gr)
{
	mglData a,v(5);	mgls_prepare2d(&a);	v.a[0]=-0.5;
	v.a[1]=-0.15;	v.a[2]=0;	v.a[3]=0.15;	v.a[4]=0.5;
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("ContV plot (default)");	}
	gr->Rotate(50,60);	gr->Box();	gr->ContV(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("manual levels");
	gr->Rotate(50,60);	gr->Box();	gr->ContV(v,a);
	gr->SubPlot(2,2,2);	gr->Title("'\\_' style");
	gr->Rotate(50,60);	gr->Box();	gr->ContV(a,"_");
	gr->SubPlot(2,2,3);	gr->Title("ContV and ContF");
	gr->Rotate(50,60);	gr->Box();	gr->Light(true);
	gr->ContV(a);	gr->ContF(a);	gr->Cont(a,"k");
}
//-----------------------------------------------------------------------------
const char *mmgl_torus="call 'prepare1d'\nsubplot 2 2 0:title 'Torus plot (default)':light on:rotate 50 60:box:torus y1 y2\n"
"subplot 2 2 1:title '\"x\" style':light on:rotate 50 60:box:torus y1 y2 'x'\n"
"subplot 2 2 2:title '\"z\" style':light on:rotate 50 60:box:torus y1 y2 'z'\n"
"subplot 2 2 3:title '\"\\#\" style':light on:rotate 50 60:box:torus y1 y2 '#'";
void smgl_torus(mglGraph *gr)
{
	mglData y1,y2;	mgls_prepare1d(0,&y1,&y2);
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Torus plot (default)");	}
	gr->Light(true);	gr->Rotate(50,60);	gr->Box();	gr->Torus(y1,y2);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'x' style");	gr->Rotate(50,60);	gr->Box();	gr->Torus(y1,y2,"x");
	gr->SubPlot(2,2,2);	gr->Title("'z' style");	gr->Rotate(50,60);	gr->Box();	gr->Torus(y1,y2,"z");
	gr->SubPlot(2,2,3);	gr->Title("'\\#' style");	gr->Rotate(50,60);	gr->Box();	gr->Torus(y1,y2,"#");
}
//-----------------------------------------------------------------------------
const char *mmgl_axial="call 'prepare2d'\nsubplot 2 2 0:title 'Axial plot (default)':light on:alpha on:rotate 50 60:box:axial a\n"
"subplot 2 2 1:title '\"x\" style;\".\" style':light on:rotate 50 60:box:axial a 'x.'\n"
"subplot 2 2 2:title '\"z\" style':light on:rotate 50 60:box:axial a 'z'\n"
"subplot 2 2 3:title '\"\\#\" style':light on:rotate 50 60:box:axial a '#'";
void smgl_axial(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Axial plot (default)");	}
	gr->Light(true);	gr->Alpha(true);	gr->Rotate(50,60);	gr->Box();	gr->Axial(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'x' style; '.'style");	gr->Rotate(50,60);	gr->Box();	gr->Axial(a,"x.");
	gr->SubPlot(2,2,2);	gr->Title("'z' style");	gr->Rotate(50,60);	gr->Box();	gr->Axial(a,"z");
	gr->SubPlot(2,2,3);	gr->Title("'\\#' style");	gr->Rotate(50,60);	gr->Box();	gr->Axial(a,"#");
}
//-----------------------------------------------------------------------------
const char *mmgl_several_light="call 'prepare2d'\ntitle 'Several light sources':rotate 50 60:light on\n"
"light 1 0 1 0 'c':light 2 1 0 0 'y':light 3 0 -1 0 'm'\nbox:surf a 'h'";
void smgl_several_light(mglGraph *gr)	// several light sources
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	gr->Title("Several light sources");
	gr->Rotate(50,60);	gr->Light(true);	gr->AddLight(1,mglPoint(0,1,0),'c');
	gr->AddLight(2,mglPoint(1,0,0),'y');	gr->AddLight(3,mglPoint(0,-1,0),'m');
	gr->Box();	gr->Surf(a,"h");
}
//-----------------------------------------------------------------------------
const char *mmgl_light="light on:attachlight on\ncall 'prepare2d'\n"
"subplot 2 2 0:title 'Default':rotate 50 60:box:surf a\nline -1 -0.7 1.7 -1 -0.7 0.7 'BA'\n\n"
"subplot 2 2 1:title 'Local':rotate 50 60\nlight 0 1 0 1 -2 -1 -1\nline 1 0 1 -1 -1 0 'BAO':box:surf a\n\n"
"subplot 2 2 2:title 'no diffuse':rotate 50 60\ndiffuse 0\nline 1 0 1 -1 -1 0 'BAO':box:surf a\n\n"
"subplot 2 2 3:title 'diffusive only':rotate 50 60\ndiffuse 0.5:light 0 1 0 1 -2 -1 -1 'w' 0\n"
"line 1 0 1 -1 -1 0 'BAO':box:surf a";
void smgl_light(mglGraph *gr)	// local light sources
{
	mglData a;	mgls_prepare2d(&a);
	gr->Light(true);	gr->AttachLight(true);
	if(big==3)
	{	gr->Rotate(50,60);	gr->Box();	gr->Surf(a);	return;	}
	gr->SubPlot(2,2,0);	gr->Title("Default");	gr->Rotate(50,60);
	gr->Line(mglPoint(-1,-0.7,1.7),mglPoint(-1,-0.7,0.7),"BA");	gr->Box();	gr->Surf(a);
	gr->SubPlot(2,2,1);	gr->Title("Local");	gr->Rotate(50,60);
	gr->AddLight(0,mglPoint(1,0,1),mglPoint(-2,-1,-1));
	gr->Line(mglPoint(1,0,1),mglPoint(-1,-1,0),"BAO");	gr->Box();	gr->Surf(a);
	gr->SubPlot(2,2,2);	gr->Title("no diffuse");	gr->Rotate(50,60);
	gr->SetDiffuse(0);
	gr->Line(mglPoint(1,0,1),mglPoint(-1,-1,0),"BAO");	gr->Box();	gr->Surf(a);
	gr->SubPlot(2,2,3);	gr->Title("diffusive only");	gr->Rotate(50,60);
	gr->SetDiffuse(0.5);
	gr->AddLight(0,mglPoint(1,0,1),mglPoint(-2,-1,-1),'w',0);
	gr->Line(mglPoint(1,0,1),mglPoint(-1,-1,0),"BAO");	gr->Box();	gr->Surf(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_surf3="call 'prepare3d'\nlight on:alpha on\n"
"subplot 2 2 0:title 'Surf3 plot (default)'\nrotate 50 60:box:surf3 c\n"
"subplot 2 2 1:title '\"\\#\" style'\nrotate 50 60:box:surf3 c '#'\n"
"subplot 2 2 2:title '\".\" style'\nrotate 50 60:box:surf3 c '.'";
void smgl_surf3(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Surf3 plot (default)");	}
	gr->Rotate(50,60);	gr->Light(true);	gr->Alpha(true);
	gr->Box();	gr->Surf3(c);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'\\#' style");
	gr->Rotate(50,60);	gr->Box();	gr->Surf3(c,"#");
	gr->SubPlot(2,2,2);	gr->Title("'.' style");
	gr->Rotate(50,60);	gr->Box();	gr->Surf3(c,".");
}
//-----------------------------------------------------------------------------
const char *mmgl_surf3a="call 'prepare3d'\ntitle 'Surf3A plot':rotate 50 60:light on:alpha on:box:surf3a c d";
void smgl_surf3a(mglGraph *gr)
{
	mglData c,d;	mgls_prepare3d(&c,&d);
	if(big!=3)	gr->Title("Surf3A plot");
	gr->Rotate(50,60);	gr->Light(true);	gr->Alpha(true);
	gr->Box();	gr->Surf3A(c,d);
}
//-----------------------------------------------------------------------------
const char *mmgl_surf3c="call 'prepare3d'\ntitle 'Surf3C plot':rotate 50 60:light on:alpha on:box:surf3c c d";
void smgl_surf3c(mglGraph *gr)
{
	mglData c,d;	mgls_prepare3d(&c,&d);
	if(big!=3)	gr->Title("Surf3C plot");
	gr->Rotate(50,60);	gr->Light(true);	gr->Alpha(true);
	gr->Box();	gr->Surf3C(c,d);
}
//-----------------------------------------------------------------------------
const char *mmgl_surf3ca="call 'prepare3d'\ntitle 'Surf3CA plot':rotate 50 60:light on:alpha on:box:surf3ca c d c";
void smgl_surf3ca(mglGraph *gr)
{
	mglData c,d;	mgls_prepare3d(&c,&d);
	if(big!=3)	gr->Title("Surf3CA plot");
	gr->Rotate(50,60);	gr->Light(true);	gr->Alpha(true);
	gr->Box();	gr->Surf3CA(c,d,c);
}
//-----------------------------------------------------------------------------
const char *mmgl_cut="call 'prepare2d'\ncall 'prepare3d'\nsubplot 2 2 0:title 'Cut on (default)':rotate 50 60:light on:box:surf a; zrange -1 0.5\n"
"subplot 2 2 1:title 'Cut off':rotate 50 60:box:surf a; zrange -1 0.5; cut off\n"
"subplot 2 2 2:title 'Cut in box':rotate 50 60:box:alpha on\ncut 0 -1 -1 1 0 1.1:surf3 c\ncut 0 0 0 0 0 0\t# restore back\n"
"subplot 2 2 3:title 'Cut by formula':rotate 50 60:box\ncut '(z>(x+0.5*y-1)^2-1) & (z>(x-0.5*y-1)^2-1)':surf3 c";
void smgl_cut(mglGraph *gr)	// cutting
{
	mglData a,c,v(1);	mgls_prepare2d(&a);	mgls_prepare3d(&c);	v.a[0]=0.5;
	gr->SubPlot(2,2,0);	gr->Title("Cut on (default)");	gr->Rotate(50,60);	gr->Light(true);
	gr->Box();	gr->Surf(a,"","zrange -1 0.5");
	gr->SubPlot(2,2,1);	gr->Title("Cut off");		gr->Rotate(50,60);
	gr->Box();	gr->Surf(a,"","zrange -1 0.5; cut off");
	gr->SubPlot(2,2,2);	gr->Title("Cut in box");	gr->Rotate(50,60);
	gr->SetCutBox(mglPoint(0,-1,-1), mglPoint(1,0,1.1));
	gr->Alpha(true);	gr->Box();	gr->Surf3(c);
	gr->SetCutBox(mglPoint(0), mglPoint(0));	// switch it off
	gr->SubPlot(2,2,3);	gr->Title("Cut by formula");	gr->Rotate(50,60);
	gr->CutOff("(z>(x+0.5*y-1)^2-1) & (z>(x-0.5*y-1)^2-1)");
	gr->Box();	gr->Surf3(c);	gr->CutOff("");	// switch it off
}
//-----------------------------------------------------------------------------
const char *mmgl_traj="call 'prepare1d'\nsubplot 1 1 0 '':title 'Traj plot':box:plot x1 y:traj x1 y y1 y2";
void smgl_traj(mglGraph *gr)
{
	mglData x,y,y1,y2;	mgls_prepare1d(&y,&y1,&y2,&x);
	if(big!=3)	{gr->SubPlot(1,1,0,"");	gr->Title("Traj plot");}
	gr->Box();	gr->Plot(x,y);	gr->Traj(x,y,y1,y2);
}
//-----------------------------------------------------------------------------
const char *mmgl_mesh="call 'prepare2d'\ntitle 'Mesh plot':rotate 50 60:box:mesh a";
void smgl_mesh(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	gr->Title("Mesh plot");
	gr->Rotate(50,60);	gr->Box();	gr->Mesh(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_fall="call 'prepare2d'\ntitle 'Fall plot':rotate 50 60:box:fall a";
void smgl_fall(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	gr->Title("Fall plot");
	gr->Rotate(50,60);	gr->Box();	gr->Fall(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_surf="call 'prepare2d'\nsubplot 2 2 0:title 'Surf plot (default)':rotate 50 60:light on:box:surf a\n"
"subplot 2 2 1:title '\"\\#\" style; meshnum 10':rotate 50 60:box:surf a '#'; meshnum 10\n"
"subplot 2 2 2:title '\".\" style':rotate 50 60:box:surf a '.'\n"
"new x 50 40 '0.8*sin(pi*x)*sin(pi*(y+1)/2)'\nnew y 50 40 '0.8*cos(pi*x)*sin(pi*(y+1)/2)'\nnew z 50 40 '0.8*cos(pi*(y+1)/2)'\n"
"subplot 2 2 3:title 'parametric form':rotate 50 60:box:surf x y z 'BbwrR'";
void smgl_surf(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Surf plot (default)");	}
	gr->Light(true);	gr->Rotate(50,60);	gr->Box();	gr->Surf(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'\\#' style; meshnum 10");
	gr->Rotate(50,60);	gr->Box();	gr->Surf(a,"#","meshnum 10");
	gr->SubPlot(2,2,2);	gr->Title("'.' style");
	gr->Rotate(50,60);	gr->Box();	gr->Surf(a,".");
	gr->SubPlot(2,2,3);	gr->Title("parametric form");
	mglData x(50,40),y(50,40),z(50,40);
	gr->Fill(x,"0.8*sin(pi*x)*sin(pi*(y+1)/2)");
	gr->Fill(y,"0.8*cos(pi*x)*sin(pi*(y+1)/2)");
	gr->Fill(z,"0.8*cos(pi*(y+1)/2)");
	gr->Rotate(50,60);	gr->Box();	gr->Surf(x,y,z,"BbwrR");
}
//-----------------------------------------------------------------------------
const char *mmgl_parser="title 'MGL parser sample'\n# call function\ncall 'sample'\n"
"\n# ordinary for-loop\nfor $0 -1 1 0.1\n"
"if $0<0:line 0 0 1 $0 'r':else:line 0 0 1 $0 'g':endif\nnext\n"
"\n# if-elseif-else\nfor $i -1 1 0.5\nif $i<0\ntext 1.1 $i '$i' 'b'\n"
"elseif $i>0\ntext 1.1 $i '$i' 'r'\nelse\ntext 1.1 $i '$i'\nendif\nnext\n"
"\n# ordinary do-while\ndo\ndefnum $i $i-0.2\nline 0 0 $i 1 'b'\nwhile $i>0\n"
"\n# do-next-break\ndo\ndefnum $i $i-0.2\nif $i<-1 then break\nline 0 0 $i 1 'm'\nnext\n"
"\n# for-while-continue\nfor $i -5 10\ntext $i/5 1.1 'a'+($i+5)\nif $i<0\n"
"text $i/5-0.06 1.1 '--' 'b'\nelseif mod($i,2)=0\ntext $i/5-0.06 1.1 '~' 'r'\n"
"else\n# NOTE: 'continue' bypass the 'while'!\ncontinue\nendif\n"
"# NOTE: 'while' limit the actual number of iterations\nwhile $i<5\n"
"\n# nested loops\nfor $i 0 1 0.1\nfor $j 0 1 0.1\nball $i $j\n"
"if $j>0.5 then continue\nball $i $j 'b+'\nnext\nnext\n"
"\nfunc 'sample'\nnew dat 100 'sin(2*pi*(i/99+1))'\nplot dat;xrange -1 0\n"
"box:axis\nxlabel 'x':ylabel 'y'\nreturn";
void smgl_parser(mglGraph *gr)	// example of MGL parsing
{	// NOTE: MGL version show much more variants of loops and conditions.
	gr->Title("MGL parser sample");
	double a[100];   // let a_i = sin(4*pi*x), x=0...1
	for(int i=0;i<100;i++)a[i]=sin(2*M_PI*i/99);
	mglParse *parser = new mglParse;
	// Add MGL variable and set yours data to it.
	mglData *d = dynamic_cast<mglData*>(parser->AddVar("dat"));
	if(d)	d->Set(a,100);
	parser->Execute(gr, "plot dat; xrange -1 0\nbox\naxis");
	// You may break script at any line do something
	// and continue after that.
	parser->Execute(gr, "xlabel 'x'\nylabel 'y'\nbox");
	// Also you may use cycles or conditions in script.
	parser->Execute(gr, "for $0 -1 1 0.1\nif $0<0\n"
		"line 0 0 1 $0 'r':else:line 0 0 1 $0 'g'\n"
		"endif\nnext");
	// You may use for or do-while loops as C/C++ one
	double i=1;
	do	{
		char buf[64];	sprintf(buf,"line 0 0 %g 1 'b'",i);
		parser->Execute(gr, buf);	i=i-0.2;
	} while(i>0);
	// or as MGL one.
	parser->Execute(gr, "for $i -1 1 0.5\n"
		"if $i<0\ntext 1.1 $i '$i' 'b'\n"
		"elseif $i>0\ntext 1.1 $i '$i' 'r'\n"
		"else\ntext 1.1 $i '$i'\nendif\nnext\n");
	// There are 'break' and 'continue' commands in MGL too.
	// NOTE: 'next' act as "while(1)" in do-while loops.
	parser->Execute(gr, "do\ndefnum $i $i-0.2\n"
		"if $i<-1 then break\nline 0 0 $i 1 'm'\nnext\n");
	// One issue with 'continue' -- it bypass 'while' checking
	parser->Execute(gr, "for $i -5 10\ntext $i/5 1.1 'a'+($i+5)\nif $i<0\n"
		"text $i/5-0.06 1.1 '--' 'b'\n"
		"elseif mod($i,2)=0\ntext $i/5-0.06 1.1 '~' 'r'\n"
		"else\ncontinue\nendif\n"
		// NOTE: 'while' limit the actual number of iterations in for-loop.
		"while $i<5\n");
	// Finally, MGL support nested loops too.
	parser->Execute(gr, "for $i 0 1 0.1\nfor $j 0 1 0.1\nball $i $j\n"
		"if $j>0.5 then continue\nball $i $j 'b+'\nnext\nnext\n");
	// Clean up memory.
	delete parser;
}
//-----------------------------------------------------------------------------
const char *mmgl_belt="call 'prepare2d'\ntitle 'Belt plot':rotate 50 60:box:belt a";
void smgl_belt(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	gr->Title("Belt plot");
	gr->Rotate(50,60);	gr->Box();	gr->Belt(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_dens="call 'prepare2d'\nnew a1 30 40 3 '0.6*sin(2*pi*x+pi*(z+1)/2)*sin(3*pi*y+pi*z) + 0.4*cos(3*pi*(x*y)+pi*(z+1)^2/2)'\n"
"subplot 2 2 0 '':title 'Dens plot (default)':box:dens a\n"
"subplot 2 2 1:title '3d variant':rotate 50 60:box:dens a\n"
"subplot 2 2 2 '':title '\"\\#\" style; meshnum 10':box:dens a '#'; meshnum 10\n"
"subplot 2 2 3:title 'several slices':rotate 50 60:box:dens a1";
void smgl_dens(mglGraph *gr)
{
	mglData a,a1(30,40,3);	mgls_prepare2d(&a);
	gr->Fill(a1,"0.6*sin(2*pi*x+pi*(z+1)/2)*sin(3*pi*y+pi*z) + 0.4*cos(3*pi*(x*y)+pi*(z+1)^2/2)");
	if(big!=3)	{gr->SubPlot(2,2,0,"");	gr->Title("Dens plot (default)");}
	gr->Box();	gr->Dens(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("3d variant");
	gr->Rotate(50,60);	gr->Box();	gr->Dens(a);
	gr->SubPlot(2,2,2,"");	gr->Title("'\\#' style; meshnum 10");
	gr->Box();	gr->Dens(a,"#","meshnum 10");
	gr->SubPlot(2,2,3);	gr->Title("several slices");
	gr->Rotate(50,60);		gr->Box();	gr->Dens(a1);
}
//-----------------------------------------------------------------------------
const char *mmgl_surfc="call 'prepare2d'\ntitle 'SurfC plot':rotate 50 60:light on:box:surfc a b";
void smgl_surfc(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2d(&a,&b);
	if(big!=3)	gr->Title("SurfC plot");
	gr->Rotate(50,60);	gr->Light(true);	gr->Box();	gr->SurfC(a,b);
}
//-----------------------------------------------------------------------------
const char *mmgl_surfca="call 'prepare2d'\ntitle 'SurfCA plot':rotate 50 60:light on:alpha on:box:surfca a b a";
void smgl_surfca(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2d(&a,&b);
	if(big!=3)	gr->Title("SurfCA plot");
	gr->Rotate(50,60);	gr->Alpha(true);	gr->Light(true);	gr->Box();
	gr->SurfCA(a,b,a);
}
//-----------------------------------------------------------------------------
const char *mmgl_surfa="call 'prepare2d'\ntitle 'SurfA plot':rotate 50 60:light on:alpha on:box:surfa a b";
void smgl_surfa(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2d(&a,&b);
	if(big!=3)	gr->Title("SurfA plot");
	gr->Rotate(50,60);	gr->Alpha(true);	gr->Light(true);	gr->Box();
	gr->SurfA(a,b);
}
//-----------------------------------------------------------------------------
const char *mmgl_tile="call 'prepare2d'\ntitle 'Tile plot':rotate 50 60:box:tile a";
void smgl_tile(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	gr->Title("Tile plot");
	gr->Rotate(40,60);	gr->Box();	gr->Tile(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_tiles="call 'prepare2d'\nsubplot 1 1 0 '':title 'Tiles plot':box:tiles a b";
void smgl_tiles(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2d(&a,&b);
	if(big!=3)	{gr->SubPlot(1,1,0,"");	gr->Title("TileS plot");}
	gr->Box();	gr->TileS(a,b);
}
//-----------------------------------------------------------------------------
const char *mmgl_boxs="call 'prepare2d'\norigin 0 0 0\nsubplot 2 2 0:title 'Boxs plot (default)':rotate 40 60:light on:box:boxs a\n"
"subplot 2 2 1:title '\"\\@\" style':rotate 50 60:box:boxs a '@'\n"
"subplot 2 2 2:title '\"\\#\" style':rotate 50 60:box:boxs a '#'\n"
"subplot 2 2 3:title 'compare with Tile':rotate 50 60:box:tile a";
void smgl_boxs(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	gr->SetOrigin(0,0,0);	gr->Light(true);
	if(big!=3)	{gr->SubPlot(2,2,0);	gr->Title("Boxs plot (default)");}
	gr->Rotate(40,60);	gr->Box();	gr->Boxs(a);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'\\@' style");
	gr->Rotate(50,60);	gr->Box();	gr->Boxs(a,"@");
	gr->SubPlot(2,2,2);	gr->Title("'\\#' style");
	gr->Rotate(50,60);	gr->Box();	gr->Boxs(a,"#");
	gr->SubPlot(2,2,3);	gr->Title("compare with Tile");
	gr->Rotate(50,60);	gr->Box();	gr->Tile(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_fit="new dat 100 '0.4*rnd+0.1+sin(2*pi*x)'\nnew in 100 '0.3+sin(2*pi*x)'\n"
"list ini 1 1 3:fit res dat 'a+b*sin(c*x)' 'abc' ini\n"
"title 'Fitting sample':yrange -2 2:box:axis:plot dat 'k. '\n"
"plot res 'r':plot in 'b'\ntext -0.9 -1.3 'fitted:' 'r:L'\n"
"putsfit 0 -1.8 'y = ' 'r':text 0 2.2 'initial: y = 0.3+sin(2\\pi x)' 'b'";
void smgl_fit(mglGraph *gr)	// nonlinear fitting
{
	mglData dat(100), in(100), res;
	gr->Fill(dat,"0.4*rnd+0.1+sin(2*pi*x)");
	gr->Fill(in,"0.3+sin(2*pi*x)");
	double ini[3] = {1,1,3};
	mglData Ini(3,ini);
	res = gr->Fit(dat, "a+b*sin(c*x)", "abc", Ini);
	if(big!=3)	gr->Title("Fitting sample");
	gr->SetRange('y',-2,2);	gr->Box();	gr->Plot(dat, "k. ");
	gr->Axis();		gr->Plot(res, "r");	gr->Plot(in, "b");
	gr->Puts(mglPoint(-0.9, -1.3), "fitted:", "r:L");
	gr->PutsFit(mglPoint(0, -1.8), "y = ", "r");
	gr->Puts(mglPoint(0, 2.2), "initial: y = 0.3+sin(2\\pi x)", "b");
//	gr->SetRanges(mglPoint(-1,-1,-1),mglPoint(1,1,1));	gr->SetOrigin(0,0,0);
}
//-----------------------------------------------------------------------------
const char *mmgl_vect3="call 'prepare3v'\nsubplot 2 1 0:title 'Vect3 sample':rotate 50 60\n"
"origin 0 0 0:box:axis '_xyz'\nvect3 ex ey ez 'x':vect3 ex ey ez:vect3 ex ey ez 'z'\n"
"subplot 2 1 1:title '\"f\" style':rotate 50 60\n"
"origin 0 0 0:box:axis '_xyz'\nvect3 ex ey ez 'fx':vect3 ex ey ez 'f':vect3 ex ey ez 'fz'\n"
"grid3 ex 'Wx':grid3 ex 'W':grid3 ex 'Wz'";
void smgl_vect3(mglGraph *gr)
{
	mglData ex,ey,ez;	mgls_prepare3v(&ex,&ey,&ez);
	if(big!=3)	{	gr->SubPlot(2,1,0);	gr->Title("Vect3 sample");	}
	gr->Rotate(50,60);	gr->SetOrigin(0,0,0);	gr->Axis("_xyz");	gr->Box();
	gr->Vect3(ex,ey,ez,"x");	gr->Vect3(ex,ey,ez);	gr->Vect3(ex,ey,ez,"z");
	if(big==3)	return;
	gr->SubPlot(2,1,1);	gr->Title("'f' style");
	gr->Rotate(50,60);	gr->SetOrigin(0,0,0);	gr->Axis("_xyz");	gr->Box();
	gr->Vect3(ex,ey,ez,"fx");	gr->Vect3(ex,ey,ez,"f");	gr->Vect3(ex,ey,ez,"fz");
	gr->Grid3(ex,"Wx");	gr->Grid3(ex,"W");	gr->Grid3(ex,"Wz");
}
//-----------------------------------------------------------------------------
const char *mmgl_vect="call 'prepare2v'\ncall 'prepare3v'\nsubplot 3 2 0 '':title 'Vect plot (default)':box:vect a b\n"
"subplot 3 2 1 '':title '\".\" style; \"=\" style':box:vect a b '.='\n"
"subplot 3 2 2 '':title '\"f\" style':box:vect a b 'f'\n"
"subplot 3 2 3 '':title '\">\" style':box:vect a b '>'\n"
"subplot 3 2 4 '':title '\"<\" style':box:vect a b '<'\n"
"subplot 3 2 5:title '3d variant':rotate 50 60:box:vect ex ey ez";
void smgl_vect(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2v(&a,&b);
	if(big!=3)	{gr->SubPlot(3,2,0,"");	gr->Title("Vect plot (default)");}
	gr->Box();	gr->Vect(a,b);
	if(big==3)	return;
	gr->SubPlot(3,2,1,"");	gr->Title("'.' style; '=' style");	gr->Box();	gr->Vect(a,b,"=.");
	gr->SubPlot(3,2,2,"");	gr->Title("'f' style");
	gr->Box();	gr->Vect(a,b,"f");
	gr->SubPlot(3,2,3,"");	gr->Title("'>' style");
	gr->Box();	gr->Vect(a,b,">");
	gr->SubPlot(3,2,4,"");	gr->Title("'<' style");
	gr->Box();	gr->Vect(a,b,"<");
	mglData ex,ey,ez;	mgls_prepare3v(&ex,&ey,&ez);
	gr->SubPlot(3,2,5);	gr->Title("3d variant");	gr->Rotate(50,60);
	gr->Box();	gr->Vect(ex,ey,ez);
}
//-----------------------------------------------------------------------------
const char *mmgl_flow="call 'prepare2v'\ncall 'prepare3v'\nsubplot 2 2 0 '':title 'Flow plot (default)':box:flow a b\n"
"subplot 2 2 1 '':title '\"v\" style':box:flow a b 'v'\n"
"subplot 2 2 2 '':title '\"#\" and \".\" styles':box:flow a b '#':flow a b '.2k'\n"
"subplot 2 2 3:title '3d variant':rotate 50 60:box:flow ex ey ez";
void smgl_flow(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2v(&a,&b);
	if(big!=3)	{gr->SubPlot(2,2,0,"");	gr->Title("Flow plot (default)");}
	gr->Box();	gr->Flow(a,b);
	if(big==3)	return;
	gr->SubPlot(2,2,1,"");	gr->Title("'v' style");
	gr->Box();	gr->Flow(a,b,"v");
	gr->SubPlot(2,2,2,"");	gr->Title("'\\#' and '.' styles");
	gr->Box();	gr->Flow(a,b,"#");	gr->Flow(a,b,".2k");
	mglData ex,ey,ez;	mgls_prepare3v(&ex,&ey,&ez);
	gr->SubPlot(2,2,3);	gr->Title("3d variant");	gr->Rotate(50,60);
	gr->Box();	gr->Flow(ex,ey,ez);
}
//-----------------------------------------------------------------------------
const char *mmgl_flow3="call 'prepare3v'\n"
"subplot 2 2 0:title 'Flow3 plot (default)':rotate 50 60:box\nflow3 ex ey ez\n"
"subplot 2 2 1:title '\"v\" style, from boundary':rotate 50 60:box\nflow3 ex ey ez 'v' 0\n"
"subplot 2 2 2:title '\"t\" style':rotate 50 60:box\nflow3 ex ey ez 't' 0\n"
"subplot 2 2 3:title 'from \\i z planes':rotate 50 60:box\nflow3 ex ey ez 'z' 0\nflow3 ex ey ez 'z' 9";
void smgl_flow3(mglGraph *gr)
{
	mglData ex,ey,ez;	mgls_prepare3v(&ex,&ey,&ez);
	if(big!=3)	{gr->SubPlot(2,2,0);	gr->Title("Flow3 plot (default)");}
	gr->Rotate(50,60);	gr->Box();		gr->Flow3(ex,ey,ez);
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("'v' style, from boundary");
	gr->Rotate(50,60);	gr->Box();	gr->Flow3(ex,ey,ez,"v",0);
	gr->SubPlot(2,2,2);	gr->Title("'t' style");
	gr->Rotate(50,60);	gr->Box();	gr->Flow3(ex,ey,ez,"t",0);
	gr->SubPlot(2,2,3);	gr->Title("from \\i z planes");
	gr->Rotate(50,60);	gr->Box();	gr->Flow3(ex,ey,ez,"z",0);	gr->Flow3(ex,ey,ez,"z",9);
}
//-----------------------------------------------------------------------------
const char *mmgl_pipe="call 'prepare2v'\ncall 'prepare3v'\nsubplot 2 2 0 '':title 'Pipe plot (default)':light on:box:pipe a b\n"
"subplot 2 2 1 '':title '\"i\" style':box:pipe a b 'i'\n"
"subplot 2 2 2 '':title 'from edges only':box:pipe a b '#'\n"
"subplot 2 2 3:title '3d variant':rotate 50 60:box:pipe ex ey ez '' 0.1";
void smgl_pipe(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2v(&a,&b);
	if(big!=3)	{gr->SubPlot(2,2,0,"");	gr->Title("Pipe plot (default)");}
	gr->Light(true);	gr->Box();	gr->Pipe(a,b);
	if(big==3)	return;
	gr->SubPlot(2,2,1,"");	gr->Title("'i' style");	gr->Box();	gr->Pipe(a,b,"i");
	gr->SubPlot(2,2,2,"");	gr->Title("'\\#' style");	gr->Box();	gr->Pipe(a,b,"#");
	mglData ex,ey,ez;	mgls_prepare3v(&ex,&ey,&ez);
	gr->SubPlot(2,2,3);	gr->Title("3d variant");	gr->Rotate(50,60);
	gr->Box();	gr->Pipe(ex,ey,ez,"",0.1);
}
//-----------------------------------------------------------------------------
const char *mmgl_dew="call 'prepare2v'\nsubplot 1 1 0 '':title 'Dew plot':light on:box:dew a b";
void smgl_dew(mglGraph *gr)
{
	mglData a,b;	mgls_prepare2v(&a,&b);
	if(big!=3)	{gr->SubPlot(1,1,0,"");	gr->Title("Dew plot");}
	gr->Box();	gr->Light(true);	gr->Dew(a,b);
}
//-----------------------------------------------------------------------------
const char *mmgl_grad="call 'prepare2d'\nsubplot 1 1 0 '':title 'Grad plot':box:grad a:dens a '{u8}w{q8}'";
void smgl_grad(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	if(big!=3)	{gr->SubPlot(1,1,0,"");	gr->Title("Grad plot");}
	gr->Box();	gr->Grad(a);	gr->Dens(a,"{u8}w{q8}");
}
//-----------------------------------------------------------------------------
const char *mmgl_cones="new ys 10 3 '0.8*sin(pi*(x+y/4+1.25))+0.2*rnd'\nlight on:origin 0 0 0\n"
"subplot 3 2 0:title 'Cones plot':rotate 50 60:box:cones ys\n"
"subplot 3 2 1:title '2 colors':rotate 50 60:box:cones ys 'cbgGyr'\n"
"subplot 3 2 2:title '\"\\#\" style':rotate 50 60:box:cones ys '#'\n"
"subplot 3 2 3:title '\"a\" style':rotate 50 60:zrange -2 2:box:cones ys 'a'\n"
"subplot 3 2 4:title '\"t\" style':rotate 50 60:box:cones ys 't'\n"
"subplot 3 2 5:title '\"4\" style':rotate 50 60:box:cones ys '4'";
void smgl_cones(mglGraph *gr)
{
	mglData ys(10,3);	ys.Modify("0.8*sin(pi*(2*x+y/2))+0.2*rnd");
	gr->Light(true);	gr->SetOrigin(0,0,0);
	if(big!=3)	{	gr->SubPlot(3,2,0);	gr->Title("Cones plot");	}
	gr->Rotate(50,60);	gr->Box();	gr->Cones(ys);
	if(big==3)	return;
	gr->SubPlot(3,2,1);	gr->Title("2 colors");
	gr->Rotate(50,60);	gr->Box();	gr->Cones(ys,"cbgGyr");
	gr->SubPlot(3,2,2);	gr->Title("'\\#' style");
	gr->Rotate(50,60);	gr->Box();	gr->Cones(ys,"#");
	gr->SubPlot(3,2,3);	gr->Title("'a' style");
	gr->SetRange('z',-2,2);	// increase range since summation can exceed [-1,1]
	gr->Rotate(50,60);	gr->Box();	gr->Cones(ys,"a");
	gr->SubPlot(3,2,4);	gr->Title("'t' style");
	gr->Rotate(50,60);	gr->Box();	gr->Cones(ys,"t");
	gr->SubPlot(3,2,5);	gr->Title("'4' style");
	gr->Rotate(50,60);	gr->Box();	gr->Cones(ys,"4");
}
//-----------------------------------------------------------------------------
const char *mmgl_aspect="subplot 2 2 0:box:text -1 1.1 'Just box' ':L'\n"
"inplot 0.2 0.5 0.7 1 off:box:text 0 1.2 'InPlot example'\n"
"subplot 2 2 1:title 'Rotate only':rotate 50 60:box\n"
"subplot 2 2 2:title 'Rotate and Aspect':rotate 50 60:aspect 1 1 2:box\n"
"subplot 2 2 3:title 'Shear':box 'c':shear 0.2 0.1:box";
void smgl_aspect(mglGraph *gr)	// transformation
{
	gr->SubPlot(2,2,0);	gr->Box();
	gr->Puts(mglPoint(-1,1.1),"Just box",":L");
	gr->InPlot(0.2,0.5,0.7,1,false);	gr->Box();
	gr->Puts(mglPoint(0,1.2),"InPlot example");
	gr->SubPlot(2,2,1);	gr->Title("Rotate only");
	gr->Rotate(50,60);	gr->Box();
	gr->SubPlot(2,2,2);	gr->Title("Rotate and Aspect");
	gr->Rotate(50,60);	gr->Aspect(1,1,2);	gr->Box();
	gr->SubPlot(2,2,3);	gr->Title("Shear");
	gr->Box("c");		gr->Shear(0.2,0.1);	gr->Box();
}
//-----------------------------------------------------------------------------
const char *mmgl_inplot="subplot 3 2 0:title 'StickPlot'\nstickplot 3 0 20 30:box 'r':text 0 0 0 '0' 'r'\n"
"stickplot 3 1 20 30:box 'g':text 0 0 0 '1' 'g'\nstickplot 3 2 20 30:box 'b':text 0 9 0 '2' 'b'\n"
"subplot 3 2 3 '':title 'ColumnPlot'\ncolumnplot 3 0:box 'r':text 0 0 '0' 'r'\n"
"columnplot 3 1:box 'g':text 0 0 '1' 'g'\ncolumnplot 3 2:box 'b':text 0 0 '2' 'b'\n"
"subplot 3 2 4 '':title 'GridPlot'\ngridplot 2 2 0:box 'r':text 0 0 '0' 'r'\n"
"gridplot 2 2 1:box 'g':text 0 0 '1' 'g'\ngridplot 2 2 2:box 'b':text 0 0 '2' 'b'\n"
"gridplot 2 2 3:box 'm':text 0 0 '3' 'm'\nsubplot 3 2 5 '':title 'InPlot':box\n"
"inplot 0.4 1 0.6 1 on:box 'r'\nmultiplot 3 2 1 2 1 '':title 'MultiPlot and ShearPlot':box\n"
"shearplot 3 0 0.2 0.1:box 'r':text 0 0 '0' 'r'\nshearplot 3 1 0.2 0.1:box 'g':text 0 0 '1' 'g'\n"
"shearplot 3 2 0.2 0.1:box 'b':text 0 0 '2' 'b'";
void smgl_inplot(mglGraph *gr)
{
	gr->SubPlot(3,2,0);	gr->Title("StickPlot");
	gr->StickPlot(3, 0, 20, 30);	gr->Box("r");	gr->Puts(mglPoint(0),"0","r");
	gr->StickPlot(3, 1, 20, 30);	gr->Box("g");	gr->Puts(mglPoint(0),"1","g");
	gr->StickPlot(3, 2, 20, 30);	gr->Box("b");	gr->Puts(mglPoint(0),"2","b");
	gr->SubPlot(3,2,3,"");	gr->Title("ColumnPlot");
	gr->ColumnPlot(3, 0);	gr->Box("r");	gr->Puts(mglPoint(0),"0","r");
	gr->ColumnPlot(3, 1);	gr->Box("g");	gr->Puts(mglPoint(0),"1","g");
	gr->ColumnPlot(3, 2);	gr->Box("b");	gr->Puts(mglPoint(0),"2","b");
	gr->SubPlot(3,2,4,"");	gr->Title("GridPlot");
	gr->GridPlot(2, 2, 0);	gr->Box("r");	gr->Puts(mglPoint(0),"0","r");
	gr->GridPlot(2, 2, 1);	gr->Box("g");	gr->Puts(mglPoint(0),"1","g");
	gr->GridPlot(2, 2, 2);	gr->Box("b");	gr->Puts(mglPoint(0),"2","b");
	gr->GridPlot(2, 2, 3);	gr->Box("m");	gr->Puts(mglPoint(0),"3","m");
	gr->SubPlot(3,2,5,"");	gr->Title("InPlot");	gr->Box();
	gr->InPlot(0.4, 1, 0.6, 1, true);	gr->Box("r");
	gr->MultiPlot(3,2,1, 2, 1,"");	gr->Title("MultiPlot and ShearPlot");	gr->Box();
	gr->ShearPlot(3, 0, 0.2, 0.1);	gr->Box("r");	gr->Puts(mglPoint(0),"0","r");
	gr->ShearPlot(3, 1, 0.2, 0.1);	gr->Box("g");	gr->Puts(mglPoint(0),"1","g");
	gr->ShearPlot(3, 2, 0.2, 0.1);	gr->Box("b");	gr->Puts(mglPoint(0),"2","b");
}
//-----------------------------------------------------------------------------
const char *mmgl_combined="call 'prepare2v'\ncall 'prepare3d'\nnew v 10:fill v -0.5 1:copy d sqrt(a^2+b^2)\n"
"subplot 2 2 0:title 'Surf + Cont':rotate 50 60:light on:box:surf a:cont a 'y'\n"
"subplot 2 2 1 '':title 'Flow + Dens':light off:box:flow a b 'br':dens d\n"
"subplot 2 2 2:title 'Mesh + Cont':rotate 50 60:box:mesh a:cont a '_'\n"
"subplot 2 2 3:title 'Surf3 + ContF3':rotate 50 60:light on\n"
"box:contf3 v c 'z' 0:contf3 v c 'x':contf3 v c\ncut 0 -1 -1 1 0 1.1\ncontf3 v c 'z' c.nz-1:surf3 c -0.5";
void smgl_combined(mglGraph *gr)	// flow threads and density plot
{
	mglData a,b,d;	mgls_prepare2v(&a,&b);	d = a;
	for(int i=0;i<a.nx*a.ny;i++)	d.a[i] = hypot(a.a[i],b.a[i]);
	mglData c;	mgls_prepare3d(&c);
	mglData v(10);	v.Fill(-0.5,1);
	gr->SubPlot(2,2,1,"");	gr->Title("Flow + Dens");
	gr->Flow(a,b,"br");	gr->Dens(d);	gr->Box();
	gr->SubPlot(2,2,0);	gr->Title("Surf + Cont");	gr->Rotate(50,60);
	gr->Light(true);	gr->Surf(a);	gr->Cont(a,"y");	gr->Box();
	gr->SubPlot(2,2,2);	gr->Title("Mesh + Cont");	gr->Rotate(50,60);
	gr->Box();	gr->Mesh(a);	gr->Cont(a,"_");
	gr->SubPlot(2,2,3);	gr->Title("Surf3 + ContF3");gr->Rotate(50,60);
	gr->Box();	gr->ContF3(v,c,"z",0);	gr->ContF3(v,c,"x");	gr->ContF3(v,c);
	gr->SetCutBox(mglPoint(0,-1,-1), mglPoint(1,0,1.1));
	gr->ContF3(v,c,"z",c.nz-1);	gr->Surf3(-0.5,c);
}
//-----------------------------------------------------------------------------
const char *mmgl_axis="subplot 2 2 0:title 'Axis origin, Grid':origin 0 0:axis:grid:fplot 'x^3'\n"
"subplot 2 2 1:title '2 axis':ranges -1 1 -1 1:origin -1 -1:axis:ylabel 'axis_1':fplot 'sin(pi*x)' 'r2'\n"
"ranges 0 1 0 1:origin 1 1:axis:ylabel 'axis_2':fplot 'cos(pi*x)'\n"
"subplot 2 2 3:title 'More axis':origin nan nan:xrange -1 1:axis:xlabel 'x' 0:ylabel 'y_1' 0:fplot 'x^2' 'k'\n"
"yrange -1 1:origin -1.3 -1:axis 'y' 'r':ylabel '#r{y_2}' 0.2:fplot 'x^3' 'r'\n\n"
"subplot 2 2 2:title '4 segments, inverted axis':origin 0 0:\n"
"inplot 0.5 1 0.5 1 on:ranges 0 10 0 2:axis\nfplot 'sqrt(x/2)':xlabel 'W' 1:ylabel 'U' 1\n"
"inplot 0 0.5 0.5 1 on:ranges 1 0 0 2:axis 'x':fplot 'sqrt(x)+x^3':xlabel '\\tau' 1\n"
"inplot 0.5 1 0 0.5 on:ranges 0 10 4 0:axis 'y':fplot 'x/4':ylabel 'L' -1\n"
"inplot 0 0.5 0 0.5 on:ranges 1 0 4 0:fplot '4*x^2'";
void smgl_axis(mglGraph *gr)
{
	gr->SubPlot(2,2,0);	gr->Title("Axis origin, Grid");	gr->SetOrigin(0,0);
	gr->Axis();	gr->Grid();	gr->FPlot("x^3");

	gr->SubPlot(2,2,1);	gr->Title("2 axis");
	gr->SetRanges(-1,1,-1,1);	gr->SetOrigin(-1,-1,-1);	// first axis
	gr->Axis();	gr->Label('y',"axis 1",0);	gr->FPlot("sin(pi*x)","r2");
	gr->SetRanges(0,1,0,1);		gr->SetOrigin(1,1,1);		// second axis
	gr->Axis();	gr->Label('y',"axis 2",0);	gr->FPlot("cos(pi*x)");

	gr->SubPlot(2,2,3);	gr->Title("More axis");	gr->SetOrigin(NAN,NAN);	gr->SetRange('x',-1,1);
	gr->Axis();	gr->Label('x',"x",0);	gr->Label('y',"y_1",0);	gr->FPlot("x^2","k");
	gr->SetRanges(-1,1,-1,1);	gr->SetOrigin(-1.3,-1);	// second axis
	gr->Axis("y","r");	gr->Label('y',"#r{y_2}",0.2);	gr->FPlot("x^3","r");

	gr->SubPlot(2,2,2);	gr->Title("4 segments, inverted axis");		gr->SetOrigin(0,0);
	gr->InPlot(0.5,1,0.5,1);	gr->SetRanges(0,10,0,2);	gr->Axis();
	gr->FPlot("sqrt(x/2)");		gr->Label('x',"W",1);	gr->Label('y',"U",1);
	gr->InPlot(0,0.5,0.5,1);	gr->SetRanges(1,0,0,2);	gr->Axis("x");
	gr->FPlot("sqrt(x)+x^3");	gr->Label('x',"\\tau",-1);
	gr->InPlot(0.5,1,0,0.5);	gr->SetRanges(0,10,4,0);	gr->Axis("y");
	gr->FPlot("x/4");	gr->Label('y',"L",-1);
	gr->InPlot(0,0.5,0,0.5);	gr->SetRanges(1,0,4,0);	gr->FPlot("4*x^2");
}
//-----------------------------------------------------------------------------
const char *mmgl_ticks="subplot 3 3 0:title 'Usual axis with \":\" style'\naxis ':'\n\n"
"subplot 3 3 1:title 'Too big/small range'\nranges -1000 1000 0 0.001:axis\n\n"
"subplot 3 3 2:title 'LaTeX-like labels'\naxis 'F!'\n\n"
"subplot 3 3 3:title 'Too narrow range'\nranges 100 100.1 10 10.01:axis\n\n"
"subplot 3 3 4:title 'No tuning, manual \"+\"'\naxis '+!'\n"
"# for version <2.3 you can use\n#tuneticks off:axis\n\n"
"subplot 3 3 5:title 'Template for ticks'\nxtick 'xxx:%g':ytick 'y:%g'\naxis\n\n"
"xtick '':ytick '' # switch it off for other plots\n\n"
"subplot 3 3 6:title 'No tuning, higher precision'\naxis '!4'\n\n"
"subplot 3 3 7:title 'Manual ticks'\nranges -pi pi 0 2\n"
"xtick pi 3 '\\pi'\nxtick 0.886 'x^*' on # note this will disable subticks drawing\n"
"# or you can use\n#xtick -pi '\\pi' -pi/2 '-\\pi/2' 0 '0' 0.886 'x^*' pi/2 '\\pi/2' pi 'pi'\n"
"list v 0 0.5 1 2:ytick v '0\n0.5\n1\n2'\n"
"axis:grid:fplot '2*cos(x^2)^2' 'r2'\n\n"
"subplot 3 3 8:title 'Time ticks'\nxrange 0 3e5:ticktime 'x':axis";
void smgl_ticks(mglGraph *gr)
{
	gr->SubPlot(3,3,0);	gr->Title("Usual axis with ':' style");	gr->Axis(":");
	gr->SubPlot(3,3,1);	gr->Title("Too big/small range");
	gr->SetRanges(-1000,1000,0,0.001);	gr->Axis();
	gr->SubPlot(3,3,2);	gr->Title("LaTeX-like labels");
	gr->Axis("F!");
	gr->SubPlot(3,3,3);	gr->Title("Too narrow range");
	gr->SetRanges(100,100.1,10,10.01);	gr->Axis();
	gr->SubPlot(3,3,4);	gr->Title("No tuning, manual '+'");
	// for version<2.3 you need first call gr->SetTuneTicks(0);
	gr->Axis("+!");
	gr->SubPlot(3,3,5);	gr->Title("Template for ticks");
	gr->SetTickTempl('x',"xxx:%g");	gr->SetTickTempl('y',"y:%g");
	gr->Axis();
	// now switch it off for other plots
	gr->SetTickTempl('x',"");	gr->SetTickTempl('y',"");
	gr->SubPlot(3,3,6);	gr->Title("No tuning, higher precision");
	gr->Axis("!4");
	gr->SubPlot(3,3,7);	gr->Title("Manual ticks");	gr->SetRanges(-M_PI,M_PI, 0, 2);
	gr->SetTicks('x',M_PI,0,0,"\\pi");	gr->AddTick('x',0.886,"x^*");
	// alternatively you can use following lines
	double val[]={0, 0.5, 1, 2};
	gr->SetTicksVal('y', mglData(4,val), "0\n0.5\n1\n2");
	gr->Axis();	gr->Grid();	gr->FPlot("2*cos(x^2)^2", "r2");
	gr->SubPlot(3,3,8);	gr->Title("Time ticks");	gr->SetRange('x',0,3e5);
	gr->SetTicksTime('x',0);	gr->Axis();
}
//-----------------------------------------------------------------------------
const char *mmgl_box="subplot 2 2 0:title 'Box (default)':rotate 50 60:box\n"
"subplot 2 2 1:title 'colored':rotate 50 60:box 'r'\n"
"subplot 2 2 2:title 'with faces':rotate 50 60:box '@'\n"
"subplot 2 2 3:title 'both':rotate 50 60:box '@cm'";
void smgl_box(mglGraph *gr)
{
	gr->SubPlot(2,2,0);	gr->Title("Box (default)");	gr->Rotate(50,60);	gr->Box();
	gr->SubPlot(2,2,1);	gr->Title("colored");		gr->Rotate(50,60);	gr->Box("r");
	gr->SubPlot(2,2,2);	gr->Title("with faces");	gr->Rotate(50,60);	gr->Box("@");
	gr->SubPlot(2,2,3);	gr->Title("both");	gr->Rotate(50,60);	gr->Box("@cm");
}
//-----------------------------------------------------------------------------
const char *mmgl_loglog="subplot 2 2 0 '<_':title 'Semi-log axis':ranges 0.01 100 -1 1:axis 'lg(x)' '' ''\n"
"axis:grid 'xy' 'g':fplot 'sin(1/x)':xlabel 'x' 0:ylabel 'y = sin 1/x' 0\n"
"subplot 2 2 1 '<_':title 'Log-log axis':ranges 0.01 100 0.1 100:axis 'lg(x)' 'lg(y)' ''\n"
"axis:grid '!' 'h=':grid:fplot 'sqrt(1+x^2)'\nxlabel 'x' 0:ylabel 'y = \\sqrt{1+x^2}' 0\n"
"subplot 2 2 2 '<_':title 'Minus-log axis':ranges -100 -0.01 -100 -0.1:axis '-lg(-x)' '-lg(-y)' ''\n"
"axis:fplot '-sqrt(1+x^2)':xlabel 'x' 0:ylabel 'y = -\\sqrt{1+x^2}' 0\n"
"subplot 2 2 3 '<_':title 'Log-ticks':ranges 0.01 100 0 100:axis 'sqrt(x)' '' ''\n"
"axis:fplot 'x':xlabel 'x' 1:ylabel 'y = x' 0";
void smgl_loglog(mglGraph *gr)	// log-log axis
{
	gr->SubPlot(2,2,0,"<_");	gr->Title("Semi-log axis");	gr->SetRanges(0.01,100,-1,1);	gr->SetFunc("lg(x)","");
	gr->Axis();	gr->Grid("xy","g");	gr->FPlot("sin(1/x)");	gr->Label('x',"x",0); gr->Label('y', "y = sin 1/x",0);
	gr->SubPlot(2,2,1,"<_");	gr->Title("Log-log axis");	gr->SetRanges(0.01,100,0.1,100);	gr->SetFunc("lg(x)","lg(y)");
	gr->Axis();	gr->Grid("!","h=");	gr->Grid();	gr->FPlot("sqrt(1+x^2)");	gr->Label('x',"x",0); gr->Label('y', "y = \\sqrt{1+x^2}",0);
	gr->SubPlot(2,2,2,"<_");	gr->Title("Minus-log axis");	gr->SetRanges(-100,-0.01,-100,-0.1);	gr->SetFunc("-lg(-x)","-lg(-y)");
	gr->Axis();	gr->FPlot("-sqrt(1+x^2)");	gr->Label('x',"x",0); gr->Label('y', "y = -\\sqrt{1+x^2}",0);
	gr->SubPlot(2,2,3,"<_");	gr->Title("Log-ticks");	gr->SetRanges(0.1,100,0,100);	gr->SetFunc("sqrt(x)","");
	gr->Axis();	gr->FPlot("x");	gr->Label('x',"x",1); gr->Label('y', "y = x",0);
}
//-----------------------------------------------------------------------------
const char *mmgl_venn="list x -0.3 0 0.3:list y 0.3 -0.3 0.3:list e 0.7 0.7 0.7\n"
"subplot 1 1 0:title 'Venn-like diagram'\ntransptype 1:alpha on:error x y e e '!rgb@#o';alpha 0.1";
void smgl_venn(mglGraph *gr)
{
	double xx[3]={-0.3,0,0.3}, yy[3]={0.3,-0.3,0.3}, ee[3]={0.7,0.7,0.7};
	mglData x(3,xx), y(3,yy), e(3,ee);
	gr->SubPlot(1,1,0);	gr->Title("Venn-like diagram");
	gr->SetTranspType(1);	gr->Alpha(true);	gr->Error(x,y,e,e,"!rgb@#o","alpha 0.1");
}
//-----------------------------------------------------------------------------
const char *mmgl_stereo="call 'prepare2d'\nlight on\nsubplot 2 1 0:rotate 50 60+1:box:surf a\nsubplot 2 1 1:rotate 50 60-1:box:surf a";
void smgl_stereo(mglGraph *gr)
{
	mglData a;	mgls_prepare2d(&a);
	gr->Light(true);
	gr->SubPlot(2,1,0);	gr->Rotate(50,60+1);
	gr->Box();	gr->Surf(a);
	gr->SubPlot(2,1,1);	gr->Rotate(50,60-1);
	gr->Box();	gr->Surf(a);
}
//-----------------------------------------------------------------------------
const char *mmgl_hist="new x 10000 '2*rnd-1':new y 10000 '2*rnd-1':copy z exp(-6*(x^2+y^2))\n"
"hist xx x z:norm xx 0 1:hist yy y z:norm yy 0 1\nmultiplot 3 3 3 2 2 '':ranges -1 1 -1 1 0 1:box:dots x y z 'wyrRk'\n"
"multiplot 3 3 0 2 1 '':ranges -1 1 0 1:box:bars xx\nmultiplot 3 3 5 1 2 '':ranges 0 1 -1 1:box:barh yy\n"
"subplot 3 3 2:text 0.5 0.5 'Hist and\\n{}MultiPlot\\n{}sample' 'a' -3";
void smgl_hist(mglGraph *gr)
{
	mglData x(10000), y(10000), z(10000);	gr->Fill(x,"2*rnd-1");	gr->Fill(y,"2*rnd-1");	gr->Fill(z,"exp(-6*(v^2+w^2))",x,y);
	mglData xx=gr->Hist(x,z), yy=gr->Hist(y,z);	xx.Norm(0,1);	yy.Norm(0,1);
	gr->MultiPlot(3,3,3,2,2,"");	gr->SetRanges(-1,1,-1,1,0,1);	gr->Box();	gr->Dots(x,y,z,"wyrRk");
	gr->MultiPlot(3,3,0,2,1,"");	gr->SetRanges(-1,1,0,1);	gr->Box();	gr->Bars(xx);
	gr->MultiPlot(3,3,5,1,2,"");	gr->SetRanges(0,1,-1,1);	gr->Box();	gr->Barh(yy);
	gr->SubPlot(3,3,2);		gr->Puts(mglPoint(0.5,0.5),"Hist and\nMultiPlot\nsample","a",-3);
}
//-----------------------------------------------------------------------------
const char *mmgl_primitives="subplot 2 2 0 '':title 'Line, Curve, Rhomb, Ellipse' '' -1.5\n"
"line -1 -1 -0.5 1 'qAI'\ncurve -0.6 -1 1 1 0 1 1 1 'rA'\nball 0 -0.5 '*':ball 1 -0.1 '*'\n"
"rhomb 0 0.4 1 0.9 0.2 'b#'\nrhomb 0 0 1 0.4 0.2 'cg@'\n"
"ellipse 0 -0.5 1 -0.1 0.2 'u#'\nellipse 0 -1 1 -0.6 0.2 'm@'\n\n"
"subplot 2 3 1 '':title 'Arc, Polygon, Symbol';size -1.2\n"
"arc -0.6 0 -0.6 0.3 180 '2kA':ball -0.6 0\npolygon 0 0 0 0.4 6 'r'\n"
"new x 50 'cos(3*pi*x)':new y 50 'sin(pi*x)'\n"
"addsymbol 'a' x y\nsymbol 0.7 0 'a'\n\n"
"light on\nsubplot 2 3 3 '<^>' 0 -0.2:title 'Face[xyz]';size -1.5:rotate 50 60:box\n"
"facex 1 0 -1 1 1 'r':facey -1 -1 -1 1 1 'g':facez 1 -1 -1 -1 1 'b'\n"
"face -1 -1 1 -1 1 1 1 -1 0 1 1 1 'bmgr'\n\n"
"subplot 2 3 5 '':title 'Cone';size -1.5\n"
"cone -0.7 -0.3 0 -0.7 0.7 0.5 0.2 0.1 'b':text -0.7 -0.7 'no edges\\n(default)';size -1.5\n"
"cone 0 -0.3 0 0 0.7 0.5 0.2 0.1 'g@':text 0 -0.7 'with edges\\n(\"\\@\" style)';size -1.5\n"
"cone 0.7 -0.3 0 0.7 0.7 0.5 0.2 0 'Ggb':text 0.7 -0.7 '\"arrow\" with\\n{}gradient';size -1.5\n"
"subplot 2 2 2 '':title 'Sphere and Drop'\nline -0.9 0 1 0.9 0 1\n"
"text -0.9 0.4 'sh=0':drop -0.9 0 0 1 0.5 'r' 0:ball -0.9 0 1 'k'\n"
"text -0.3 0.6 'sh=0.33':drop -0.3 0 0 1 0.5 'r' 0.33:ball -0.3 0 1 'k'\n"
"text 0.3 0.8 'sh=0.67':drop 0.3 0 0 1 0.5 'r' 0.67:ball 0.3 0 1 'k'\n"
"text 0.9 1. 'sh=1':drop 0.9 0 0 1 0.5 'r' 1:ball 0.9 0 1 'k'\n\n"
"text -0.9 -1.1 'asp=0.33':drop -0.9 -0.7 0 1 0.5 'b' 0 0.33\n"
"text -0.3 -1.1 'asp=0.67':drop -0.3 -0.7 0 1 0.5 'b' 0 0.67\n"
"text 0.3 -1.1 'asp=1':drop 0.3 -0.7 0 1 0.5 'b' 0 1\n"
"text 0.9 -1.1 'asp=1.5':drop 0.9 -0.7 0 1 0.5 'b' 0 1.5";
void smgl_primitives(mglGraph *gr)	// flag #
{
	gr->SubPlot(2,2,0,"");	gr->Title("Line, Curve, Rhomb, Ellipse","",-1.5);
	gr->Line(mglPoint(-1,-1),mglPoint(-0.5,1),"qAI");
	gr->Curve(mglPoint(-0.6,-1),mglPoint(1,1),mglPoint(0,1),mglPoint(1,1),"rA");
	gr->Rhomb(mglPoint(0,0.4),mglPoint(1,0.9),0.2,"b#");
	gr->Rhomb(mglPoint(0,0),mglPoint(1,0.4),0.2,"cg@");
	gr->Ellipse(mglPoint(0,-0.5),mglPoint(1,-0.1),0.2,"u#");
	gr->Ellipse(mglPoint(0,-1),mglPoint(1,-0.6),0.2,"m@");
	gr->Mark(mglPoint(0,-0.5),"*");	gr->Mark(mglPoint(1,-0.1),"*");

	gr->SubPlot(2,3,1,"");	gr->Title("Arc, Polygon, Symbol","", -1.2*2);
	gr->Arc(mglPoint(-0.6,0), mglPoint(-0.6,0.3), 180, "2kA");	gr->Ball(-0.6,0);
	gr->Polygon(mglPoint(), mglPoint(0,0.4), 6, "r");
	mglData x(50), y(50);	gr->Fill(x,"cos(3*pi*x)");	gr->Fill(y,"sin(pi*x)");
	gr->DefineSymbol('a',x,y);	gr->Symbol(mglPoint(0.7),'a');

	gr->Light(true);
	gr->SubPlot(2,3,3,"<^>",0,-0.2);	gr->Title("Face[xyz]", "", -1.5*2);
	gr->Rotate(50,60);	gr->Box();
	gr->FaceX(mglPoint(1,0,-1),1,1,"r");
	gr->FaceY(mglPoint(-1,-1,-1),1,1,"g");
	gr->FaceZ(mglPoint(1,-1,-1),-1,1,"b");
	gr->Face(mglPoint(-1,-1,1),mglPoint(-1,1,1),mglPoint(1,-1,0),mglPoint(1,1,1),"bmgr");

	gr->SubPlot(2,3,5,"");	gr->Title("Cone", "", -1.5*2);
	gr->Cone(mglPoint(-0.7,-0.3),mglPoint(-0.7,0.7,0.5),0.2,0.1,"b");
	gr->Puts(mglPoint(-0.7,-0.7),"no edges\n(default)","", -1.5);
	gr->Cone(mglPoint(0,-0.3),mglPoint(0,0.7,0.5),0.2,0.1,"g@");
	gr->Puts(mglPoint(0,-0.7),"with edges\n('\\@' style)","", -1.5);
	gr->Cone(mglPoint(0.7,-0.3),mglPoint(0.7,0.7,0.5),0.2,0,"ry");
	gr->Puts(mglPoint(0.7,-0.7),"'arrow' with\ngradient","", -1.5);

	gr->SubPlot(2,2,2,"");	gr->Title("Sphere and Drop");	gr->Alpha(false);
	gr->Puts(mglPoint(-0.9,0.4),"sh=0");		gr->Ball(mglPoint(-0.9,0,1),'k');
	gr->Drop(mglPoint(-0.9,0),mglPoint(0,1),0.5,"r",0);
	gr->Puts(mglPoint(-0.3,0.6),"sh=0.33");	gr->Ball(mglPoint(-0.3,0,1),'k');
	gr->Drop(mglPoint(-0.3,0),mglPoint(0,1),0.5,"r",0.33);
	gr->Puts(mglPoint(0.3,0.8),"sh=0.67");		gr->Ball(mglPoint(0.3,0,1),'k');
	gr->Drop(mglPoint(0.3,0),mglPoint(0,1),0.5,"r",0.67);
	gr->Puts(mglPoint(0.9,1),"sh=1");			gr->Ball(mglPoint(0.9,0,1),'k');
	gr->Drop(mglPoint(0.9,0),mglPoint(0,1),0.5,"r",1);
	gr->Line(mglPoint(-0.9,0,1),mglPoint(0.9,0,1),"b");

	gr->Puts(mglPoint(-0.9,-1.1),"asp=0.33");
	gr->Drop(mglPoint(-0.9,-0.7),mglPoint(0,1),0.5,"b",0,0.33);
	gr->Puts(mglPoint(-0.3,-1.1),"asp=0.67");
	gr->Drop(mglPoint(-0.3,-0.7),mglPoint(0,1),0.5,"b",0,0.67);
	gr->Puts(mglPoint(0.3,-1.1),"asp=1");
	gr->Drop(mglPoint(0.3,-0.7),mglPoint(0,1),0.5,"b",0,1);
	gr->Puts(mglPoint(0.9,-1.1),"asp=1.5");
	gr->Drop(mglPoint(0.9,-0.7),mglPoint(0,1),0.5,"b",0,1.5);
}
//-----------------------------------------------------------------------------
const char *mmgl_table="new ys 10 3 '0.8*sin(pi*(x+y/4+1.25))+0.2*rnd'\n"
"subplot 2 2 0:title 'Table sample':box\ntable ys 'y_1\\n{}y_2\\n{}y_3'\n\n"
"subplot 2 2 1:title 'no borders, colored'\ntable ys 'y_1\\n{}y_2\\n{}y_3' 'r|'\n\n"
"subplot 2 2 2:title 'no font decrease'\ntable ys 'y_1\\n{}y_2\\n{}y_3' '#'\n\n"
"subplot 2 2 3:title 'manual width and position':box\n"
"table 0.5 0.95 ys 'y_1\\n{}y_2\\n{}y_3' '#';value 0.7";
void smgl_table(mglGraph *gr)
{
	mglData ys(10,3);	ys.Modify("0.8*sin(pi*(2*x+y/2))+0.2*rnd");
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Table plot");	}
	gr->Table(ys,"y_1\ny_2\ny_3");	gr->Box();
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("no borders, colored");
	gr->Table(ys,"y_1\ny_2\ny_3","r|");
	gr->SubPlot(2,2,2);	gr->Title("no font decrease");
	gr->Table(ys,"y_1\ny_2\ny_3","#");
	gr->SubPlot(2,2,3);	gr->Title("manual width, position");
	gr->Table(0.5, 0.95, ys,"y_1\ny_2\ny_3","#", "value 0.7");	gr->Box();
}
//-----------------------------------------------------------------------------
const char *mmgl_label="new ys 10 '0.2*rnd-0.8*sin(pi*x)'\n"
"subplot 1 1 0 '':title 'Label plot':box:plot ys ' *':label ys 'y=%y'";
void smgl_label(mglGraph *gr)
{
	mglData ys(10);	ys.Modify("0.8*sin(pi*2*x)+0.2*rnd");
	if(big!=3)	{	gr->SubPlot(1,1,0,"");	gr->Title("Label plot");	}
	gr->Box();	gr->Plot(ys," *");	gr->Label(ys,"y=%y");
}
//-----------------------------------------------------------------------------
const char *mmgl_colorbar="call 'prepare2d'\nnew v 9 'x'\nsubplot 2 2 0:title 'Colorbar out of box':box\n"
"colorbar '<':colorbar '>':colorbar '_':colorbar '^'\n"
"subplot 2 2 1:title 'Colorbar near box':box\n"
"colorbar '<I':colorbar '>I':colorbar '_I':colorbar '^I'\n"
"subplot 2 2 2:title 'manual colors':box:contd v a\n"
"colorbar v '<':colorbar v '>':colorbar v '_':colorbar v '^'\n"
"subplot 2 2 3:title '':text -0.5 1.55 'Color positions' ':C' -2\n"
"colorbar 'bwr>' 0.25 0:text -0.9 1.2 'Default'\n"
"colorbar 'b{w,0.3}r>' 0.5 0:text -0.1 1.2 'Manual'\ncrange 0.01 1e3\n"
"colorbar '>' 0.75 0:text 0.65 1.2 'Normal scale':colorbar '>':text 1.35 1.2 'Log scale'";
void smgl_colorbar(mglGraph *gr)
{
	gr->SubPlot(2,2,0);	gr->Title("Colorbar out of box");	gr->Box();
	gr->Colorbar("<");	gr->Colorbar(">");	gr->Colorbar("_");	gr->Colorbar("^");
	gr->SubPlot(2,2,1);	gr->Title("Colorbar near box");		gr->Box();
	gr->Colorbar("<I");	gr->Colorbar(">I");	gr->Colorbar("_I");	gr->Colorbar("^I");
	gr->SubPlot(2,2,2);	gr->Title("manual colors");
	mglData a,v;	mgls_prepare2d(&a,0,&v);
	gr->Box();	gr->ContD(v,a);
	gr->Colorbar(v,"<");	gr->Colorbar(v,">");	gr->Colorbar(v,"_");	gr->Colorbar(v,"^");

	gr->SubPlot(2,2,3);	gr->Title(" ");
	gr->Puts(mglPoint(-0.5,1.55),"Color positions",":C",-2);
	gr->Colorbar("bwr>",0.25,0);	gr->Puts(mglPoint(-0.9,1.2),"Default");
	gr->Colorbar("b{w,0.3}r>",0.5,0);	gr->Puts(mglPoint(-0.1,1.2),"Manual");

	gr->Puts(mglPoint(1,1.55),"log-scale",":C",-2);
	gr->SetRange('c',0.01,1e3);
	gr->Colorbar(">",0.75,0);	gr->Puts(mglPoint(0.65,1.2),"Normal scale");
	gr->SetFunc("","","","lg(c)");
	gr->Colorbar(">");		gr->Puts(mglPoint(1.35,1.2),"Log scale");
}
//-----------------------------------------------------------------------------
const char *mmgl_legend="addlegend 'sin(\\pi {x^2})' 'b':addlegend 'sin(\\pi x)' 'g*'\n"
"addlegend 'sin(\\pi \\sqrt{x})' 'rd':addlegend 'jsut text' ' ':addlegend 'no indent for this' ''\n"
"subplot 2 2 0 '':title 'Legend (default)':box:legend\n"
"legend 1 0.5 '^':text 0.49 0.88 'Style \"\\^\"' 'A:L'\n"
"legend 3 'A#':text 0.75 0.65 'Absolute position' 'A'\n"
"subplot 2 2 2 '':title 'coloring':box:legend 0 'r#':legend 1 'Wb#':legend 2 'ygr#'\n"
"subplot 2 2 3 '':title 'manual position':box\n"
"legend 0.5 1:text 0.5 0.5 'at x=0.5, y=1' 'a'\n"
"legend 1 '#-':text 0.75 0.25 'Horizontal legend' 'a'";
void smgl_legend(mglGraph *gr)
{
	gr->AddLegend("sin(\\pi {x^2})","b");
	gr->AddLegend("sin(\\pi x)","g*");
	gr->AddLegend("sin(\\pi \\sqrt{x})","rd");
	gr->AddLegend("just text"," ");
	gr->AddLegend("no indent for this","");
	if(big!=3)	{gr->SubPlot(2,2,0,"");	gr->Title("Legend (default)");}
	gr->Box();	gr->Legend();
	if(big==3)	return;
	gr->Legend(1,0.5,"^");	gr->Puts(0.49, 0.88, "Style '\\^'","A:L");
	gr->Legend(3,"A#");
	gr->Puts(mglPoint(0.75,0.65),"Absolute position","A");
	gr->SubPlot(2,2,2,"");	gr->Title("coloring");	gr->Box();
	gr->Legend(0,"r#");	gr->Legend(1,"Wb#");	gr->Legend(2,"ygr#");
	gr->SubPlot(2,2,3,"");	gr->Title("manual position");	gr->Box();
	gr->Legend(0.5,1);
	gr->Puts(mglPoint(0.5,0.5),"at x=0.5, y=1","a");
	gr->Legend(1,"#-");
	gr->Puts(mglPoint(0.75,0.25),"Horizontal legend","a");
}
//-----------------------------------------------------------------------------
const char *mmgl_dat_diff="ranges 0 1 0 1 0 1:new a 30 40 'x*y'\n"
"subplot 2 2 0:title 'a(x,y)':rotate 60 40:surf a:box\n"
"subplot 2 2 1:title 'da/dx':rotate 60 40:diff a 'x':surf a:box\n"
"subplot 2 2 2:title '\\int da/dx dxdy':rotate 60 40:integrate a 'xy':surf a:box\n"
"subplot 2 2 3:title '\\int {d^2}a/dxdy dx':rotate 60 40:diff2 a 'y':surf a:box";
void smgl_dat_diff(mglGraph *gr)	// differentiate
{
	gr->SetRanges(0,1,0,1,0,1);
	mglData a(30,40);	a.Modify("x*y");
	gr->SubPlot(2,2,0);	gr->Title("a(x,y)");	gr->Rotate(60,40);
	gr->Surf(a);		gr->Box();
	gr->SubPlot(2,2,1);	gr->Title("da/dx");		gr->Rotate(60,40);
	a.Diff("x");		gr->Surf(a);	gr->Box();
	gr->SubPlot(2,2,2);	gr->Title("\\int da/dx dxdy");	gr->Rotate(60,40);
	a.Integral("xy");	gr->Surf(a);	gr->Box();
	gr->SubPlot(2,2,3);	gr->Title("\\int {d^2}a/dxdy dx");	gr->Rotate(60,40);
	a.Diff2("y");	gr->Surf(a);	gr->Box();
}
//-----------------------------------------------------------------------------
const char *mmgl_dat_extra="subplot 2 2 0 '':title 'Envelop sample':new d1 1000 'exp(-8*x^2)*sin(10*pi*x)'\n"
"axis:plot d1 'b':envelop d1 'x':plot d1 'r'\n"
"subplot 2 2 1 '':title 'Smooth sample':ranges 0 1 0 1\nnew y0 30 '0.4*sin(pi*x) + 0.3*cos(1.5*pi*x) - 0.4*sin(2*pi*x)+0.5*rnd'\n"
"copy y1 y0:smooth y1 'x3':plot y1 'r';legend '\"3\" style'\ncopy y2 y0:smooth y2 'x5':plot y2 'g';legend '\"5\" style'\n"
"copy y3 y0:smooth y3 'x':plot y3 'b';legend 'default'\nplot y0 '{m7}:s';legend 'none'\nlegend:box\n"
"subplot 2 2 2:title 'Sew sample':rotate 50 60:light on:alpha on\nnew d2 100 100 'mod((y^2-(1-x)^2)/2,0.1)'\n"
"box:surf d2 'b':sew d2 'xy' 0.1:surf d2 'r'\n"
"subplot 2 2 3:title 'Resize sample (interpolation)'\nnew x0 10 'rnd':new v0 10 'rnd'\n"
"resize x1 x0 100:resize v1 v0 100\nplot x0 v0 'b+ ':plot x1 v1 'r-':label x0 v0 '%n'";
void smgl_dat_extra(mglGraph *gr)	// differentiate
{
	gr->SubPlot(2,2,0,"");	gr->Title("Envelop sample");
	mglData d1(1000);	gr->Fill(d1,"exp(-8*x^2)*sin(10*pi*x)");
	gr->Axis();			gr->Plot(d1, "b");
	d1.Envelop('x');	gr->Plot(d1, "r");

	gr->SubPlot(2,2,1,"");	gr->Title("Smooth sample");
	mglData y0(30),y1,y2,y3;
	gr->SetRanges(0,1,0,1);
	gr->Fill(y0, "0.4*sin(pi*x) + 0.3*cos(1.5*pi*x) - 0.4*sin(2*pi*x)+0.5*rnd");

	y1=y0;	y1.Smooth("x3");
	y2=y0;	y2.Smooth("x5");
	y3=y0;	y3.Smooth("x");

	gr->Plot(y0,"{m7}:s", "legend 'none'");	//gr->AddLegend("none","k");
	gr->Plot(y1,"r", "legend ''3' style'");
	gr->Plot(y2,"g", "legend ''5' style'");
	gr->Plot(y3,"b", "legend 'default'");
	gr->Legend();		gr->Box();

	gr->SubPlot(2,2,2);		gr->Title("Sew sample");
	mglData d2(100, 100);	gr->Fill(d2, "mod((y^2-(1-x)^2)/2,0.1)");
	gr->Rotate(50, 60);	gr->Light(true);	gr->Alpha(true);
	gr->Box();			gr->Surf(d2, "b");
	d2.Sew("xy", 0.1);	gr->Surf(d2, "r");

	gr->SubPlot(2,2,3);		gr->Title("Resize sample (interpolation)");
	mglData x0(10), v0(10), x1, v1;
	gr->Fill(x0,"rnd");		gr->Fill(v0,"rnd");
	x1 = x0.Resize(100);	v1 = v0.Resize(100);
	gr->Plot(x0,v0,"b+ ");	gr->Plot(x1,v1,"r-");
	gr->Label(x0,v0,"%n");
}
//-----------------------------------------------------------------------------
const char *mmgl_ternary="ranges 0 1 0 1 0 1\nnew x 50 '0.25*(1+cos(2*pi*x))'\n"
"new y 50 '0.25*(1+sin(2*pi*x))'\nnew z 50 'x'\nnew a 20 30 '30*x*y*(1-x-y)^2*(x+y<1)'\n"
"new rx 10 'rnd':new ry 10:fill ry '(1-v)*rnd' rx\nlight on\n\n"
"subplot 2 2 0:title 'Ordinary axis 3D':rotate 50 60\nbox:axis:grid\n"
"plot x y z 'r2':surf a '#'\nxlabel 'B':ylabel 'C':zlabel 'Z'\n\n"
"subplot 2 2 1:title 'Ternary axis (x+y+t=1)':ternary 1\nbox:axis:grid 'xyz' 'B;'\n"
"plot x y 'r2':plot rx ry 'q^ ':cont a:line 0.5 0 0 0.75 'g2'\nxlabel 'B':ylabel 'C':tlabel 'A'\n\n"
"subplot 2 2 2:title 'Quaternary axis 3D':rotate 50 60:ternary 2\nbox:axis:grid 'xyz' 'B;'\n"
"plot x y z 'r2':surf a '#'\nxlabel 'B':ylabel 'C':tlabel 'A':zlabel 'D'\n\n"
"subplot 2 2 3:title 'Ternary axis 3D':rotate 50 60:ternary 1\nbox:axis:grid 'xyz' 'B;'\n"
"plot x y z 'r2':surf a '#'\nxlabel 'B':ylabel 'C':tlabel 'A':zlabel 'Z'";
void smgl_ternary(mglGraph *gr)	// flag #
{
	gr->SetRanges(0,1,0,1,0,1);
	mglData x(50),y(50),z(50),rx(10),ry(10), a(20,30);
	a.Modify("30*x*y*(1-x-y)^2*(x+y<1)");
	x.Modify("0.25*(1+cos(2*pi*x))");
	y.Modify("0.25*(1+sin(2*pi*x))");
	rx.Modify("rnd"); ry.Modify("(1-v)*rnd",rx);
	z.Modify("x");

	gr->SubPlot(2,2,0);	gr->Title("Ordinary axis 3D");
	gr->Rotate(50,60);		gr->Light(true);
	gr->Plot(x,y,z,"r2");	gr->Surf(a,"BbcyrR#");
	gr->Axis(); gr->Grid();	gr->Box();
	gr->Label('x',"B",1);	gr->Label('y',"C",1);	gr->Label('z',"Z",1);

	gr->SubPlot(2,2,1);	gr->Title("Ternary axis (x+y+t=1)");
	gr->Ternary(1);
	gr->Plot(x,y,"r2");	gr->Plot(rx,ry,"q^ ");	gr->Cont(a);
	gr->Line(mglPoint(0.5,0), mglPoint(0,0.75), "g2");
	gr->Axis(); gr->Grid("xyz","B;");
	gr->Label('x',"B");	gr->Label('y',"C");	gr->Label('t',"A");

	gr->SubPlot(2,2,2);	gr->Title("Quaternary axis 3D");
	gr->Rotate(50,60);		gr->Light(true);
	gr->Ternary(2);
	gr->Plot(x,y,z,"r2");	gr->Surf(a,"BbcyrR#");
	gr->Axis(); gr->Grid();	gr->Box();
	gr->Label('t',"A",1);	gr->Label('x',"B",1);
	gr->Label('y',"C",1);	gr->Label('z',"D",1);

	gr->SubPlot(2,2,3);	gr->Title("Ternary axis 3D");
	gr->Rotate(50,60);		gr->Light(true);
	gr->Ternary(1);
	gr->Plot(x,y,z,"r2");	gr->Surf(a,"BbcyrR#");
	gr->Axis(); gr->Grid();	gr->Box();
	gr->Label('t',"A",1);	gr->Label('x',"B",1);
	gr->Label('y',"C",1);	gr->Label('z',"Z",1);
}
//-----------------------------------------------------------------------------
const char *mmgl_projection="ranges 0 1 0 1 0 1\nnew x 50 '0.25*(1+cos(2*pi*x))'\n"
"new y 50 '0.25*(1+sin(2*pi*x))'\nnew z 50 'x'\nnew a 20 30 '30*x*y*(1-x-y)^2*(x+y<1)'\n"
"new rx 10 'rnd':new ry 10:fill ry '(1-v)*rnd' rx\nlight on\n\n"
"title 'Projection sample':ternary 4:rotate 50 60\nbox:axis:grid\n"
"plot x y z 'r2':surf a '#'\nxlabel 'X':ylabel 'Y':zlabel 'Z'";
void smgl_projection(mglGraph *gr)	// flag #
{
	gr->SetRanges(0,1,0,1,0,1);
	mglData x(50),y(50),z(50),rx(10),ry(10), a(20,30);
	a.Modify("30*x*y*(1-x-y)^2*(x+y<1)");
	x.Modify("0.25*(1+cos(2*pi*x))");
	y.Modify("0.25*(1+sin(2*pi*x))");
	rx.Modify("rnd"); ry.Modify("(1-v)*rnd",rx);
	z.Modify("x");

	if(big!=3)	gr->Title("Projection sample");
	gr->Ternary(4);
	gr->Rotate(50,60);		gr->Light(true);
	gr->Plot(x,y,z,"r2");	gr->Surf(a,"#");
	gr->Axis(); gr->Grid();	gr->Box();
	gr->Label('x',"X",1);	gr->Label('y',"Y",1);	gr->Label('z',"Z",1);
}
//-----------------------------------------------------------------------------
const char *mmgl_projection5="ranges 0 1 0 1 0 1\nnew x 50 '0.25*(1+cos(2*pi*x))'\n"
"new y 50 '0.25*(1+sin(2*pi*x))'\nnew z 50 'x'\nnew a 20 30 '30*x*y*(1-x-y)^2*(x+y<1)'\n"
"new rx 10 'rnd':new ry 10:fill ry '(1-v)*rnd' rx\nlight on\n\n"
"title 'Projection sample (ternary)':ternary 5:rotate 50 60\nbox:axis:grid\n"
"plot x y z 'r2':surf a '#'\nxlabel 'X':ylabel 'Y':zlabel 'Z'";
void smgl_projection5(mglGraph *gr)	// flag #
{
	gr->SetRanges(0,1,0,1,0,1);
	mglData x(50),y(50),z(50),rx(10),ry(10), a(20,30);
	a.Modify("30*x*y*(1-x-y)^2*(x+y<1)");
	x.Modify("0.25*(1+cos(2*pi*x))");
	y.Modify("0.25*(1+sin(2*pi*x))");
	rx.Modify("rnd"); ry.Modify("(1-v)*rnd",rx);
	z.Modify("x");

	if(big!=3)	gr->Title("Projection sample (ternary)");
	gr->Ternary(5);
	gr->Rotate(50,60);		gr->Light(true);
	gr->Plot(x,y,z,"r2");	gr->Surf(a,"#");
	gr->Axis(); gr->Grid();	gr->Box();
	gr->Label('x',"X",1);	gr->Label('y',"Y",1);	gr->Label('z',"Z",1);
}
//-----------------------------------------------------------------------------
const char *mmgl_triplot="list q 0 1 2 3 | 4 5 6 7 | 0 2 4 6 | 1 3 5 7 | 0 4 1 5 | 2 6 3 7\n"
"list xq -1 1 -1 1 -1 1 -1 1\nlist yq -1 -1 1 1 -1 -1 1 1\nlist zq -1 -1 -1 -1 1 1 1 1\nlight on\n"
"subplot 2 2 0:title 'QuadPlot sample':rotate 50 60\n"
"quadplot q xq yq zq 'yr'\nquadplot q xq yq zq '#k'\n"
"subplot 2 2 2:title 'QuadPlot coloring':rotate 50 60\n"
"quadplot q xq yq zq yq 'yr'\nquadplot q xq yq zq '#k'\n"
"list t 0 1 2 | 0 1 3 | 0 2 3 | 1 2 3\n"
"list xt -1 1 0 0\nlist yt -1 -1 1 0\nlist zt -1 -1 -1 1\n"
"subplot 2 2 1:title 'TriPlot sample':rotate 50 60\n"
"triplot t xt yt zt 'b'\ntriplot t xt yt zt '#k'\n"
"subplot 2 2 3:title 'TriPlot coloring':rotate 50 60\n"
"triplot t xt yt zt yt 'cb'\ntriplot t xt yt zt '#k'\ntricont t xt yt zt 'B'";
void smgl_triplot(mglGraph *gr)
{
	double q[] = {0,1,2,3, 4,5,6,7, 0,2,4,6, 1,3,5,7, 0,4,1,5, 2,6,3,7};
	double xc[] = {-1,1,-1,1,-1,1,-1,1}, yc[] = {-1,-1,1,1,-1,-1,1,1}, zc[] = {-1,-1,-1,-1,1,1,1,1};
	mglData qq(6,4,q), xx(8,xc), yy(8,yc), zz(8,zc);
	gr->Light(true);	//gr->Alpha(true);
	gr->SubPlot(2,2,0);	gr->Title("QuadPlot sample");	gr->Rotate(50,60);
	gr->QuadPlot(qq,xx,yy,zz,"yr");
	gr->QuadPlot(qq,xx,yy,zz,"k#");
	gr->SubPlot(2,2,2);	gr->Title("QuadPlot coloring");	gr->Rotate(50,60);
	gr->QuadPlot(qq,xx,yy,zz,yy,"yr");
	gr->QuadPlot(qq,xx,yy,zz,"k#");

	double t[] = {0,1,2, 0,1,3, 0,2,3, 1,2,3};
	double xt[] = {-1,1,0,0}, yt[] = {-1,-1,1,0}, zt[] = {-1,-1,-1,1};
	mglData tt(4,3,t), uu(4,xt), vv(4,yt), ww(4,zt);
	gr->SubPlot(2,2,1);	gr->Title("TriPlot sample");	gr->Rotate(50,60);
	gr->TriPlot(tt,uu,vv,ww,"b");
	gr->TriPlot(tt,uu,vv,ww,"k#");
	gr->SubPlot(2,2,3);	gr->Title("TriPlot coloring");	gr->Rotate(50,60);
	gr->TriPlot(tt,uu,vv,ww,vv,"cb");
	gr->TriPlot(tt,uu,vv,ww,"k#");
	gr->TriCont(tt,uu,vv,ww,"B");
}
//-----------------------------------------------------------------------------
const char *mmgl_dots="new t 2000 'pi*(rnd-0.5)':new f 2000 '2*pi*rnd'\n"
"copy x 0.9*cos(t)*cos(f):copy y 0.9*cos(t)*sin(f):copy z 0.6*sin(t):copy c cos(2*t)\n"
"subplot 2 2 0:title 'Dots sample':rotate 50 60\nbox:dots x y z\nalpha on\n"
"subplot 2 2 1:title 'add transparency':rotate 50 60\nbox:dots x y z c\n"
"subplot 2 2 2:title 'add colorings':rotate 50 60\nbox:dots x y z x c\n"
"subplot 2 2 3:title 'Only coloring':rotate 50 60\nbox:tens x y z x ' .'";
void smgl_dots(mglGraph *gr)
{
	int i, n=1000;
	mglData x(n),y(n),z(n),c(n);
	for(i=0;i<n;i++)
	{
		double t=M_PI*(mgl_rnd()-0.5), f=2*M_PI*mgl_rnd();
		x.a[i] = 0.9*cos(t)*cos(f);
		y.a[i] = 0.9*cos(t)*sin(f);
		z.a[i] = 0.6*sin(t);
		c.a[i] = cos(2*t);
	}
	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Dots sample");	}
	gr->Rotate(50,60);	gr->Box();	gr->Dots(x,y,z);
	if(big==3)	return;
	gr->Alpha(true);
	gr->SubPlot(2,2,1);	gr->Title("add transparency");		gr->Rotate(50,60);	gr->Box();	gr->Dots(x,y,z,c);
	gr->SubPlot(2,2,2);	gr->Title("add coloring");	gr->Rotate(50,60);	gr->Box();	gr->Dots(x,y,z,x,c);
	gr->SubPlot(2,2,3);	gr->Title("Only coloring");		gr->Rotate(50,60);	gr->Box();	gr->Tens(x,y,z,x," .");
}
//-----------------------------------------------------------------------------
/*void smgl_surf3_rgbd(mglGraph *gr)
{
	mglData c;	mgls_prepare3d(&c);
	gr->Rotate(40,60);	gr->VertexColor(true);
	gr->Box();	gr->Surf3(c,"bgrd");
}*/
//-----------------------------------------------------------------------------
const char *mmgl_mirror="new a 31 41 '-pi*x*exp(-(y+1)^2-4*x^2)'\n"
"subplot 2 2 0:title 'Options for coordinates':alpha on:light on:rotate 40 60:box\n"
"surf a 'r';yrange 0 1:surf a 'b';yrange 0 -1\n"
"subplot 2 2 1:title 'Option \"meshnum\"':rotate 40 60:box\n"
"mesh a 'r'; yrange 0 1:mesh a 'b';yrange 0 -1; meshnum 5\n"
"subplot 2 2 2:title 'Option \"alpha\"':rotate 40 60:box\n"
"surf a 'r';yrange 0 1; alpha 0.7:surf a 'b';yrange 0 -1; alpha 0.3\n"
"subplot 2 2 3 '<_':title 'Option \"legend\"'\n"
"fplot 'x^3' 'r'; legend 'y = x^3':fplot 'cos(pi*x)' 'b'; legend 'y = cos \\pi x'\n"
"box:axis:legend 2";
void smgl_mirror(mglGraph *gr)	// flag #
{
	mglData a(31,41);
	gr->Fill(a,"-pi*x*exp(-(y+1)^2-4*x^2)");

	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Options for coordinates");	}
	gr->Alpha(true);	gr->Light(true);
	gr->Rotate(40,60);	gr->Box();
	gr->Surf(a,"r","yrange 0 1"); gr->Surf(a,"b","yrange 0 -1");
	if(big==3)	return;
	gr->SubPlot(2,2,1);	gr->Title("Option 'meshnum'");
	gr->Rotate(40,60);	gr->Box();
	gr->Mesh(a,"r","yrange 0 1"); gr->Mesh(a,"b","yrange 0 -1; meshnum 5");
	gr->SubPlot(2,2,2);	gr->Title("Option 'alpha'");
	gr->Rotate(40,60);	gr->Box();
	gr->Surf(a,"r","yrange 0 1; alpha 0.7"); gr->Surf(a,"b","yrange 0 -1; alpha 0.3");
	gr->SubPlot(2,2,3,"<_");	gr->Title("Option 'legend'");
	gr->FPlot("x^3","r","legend 'y = x^3'"); gr->FPlot("cos(pi*x)","b","legend 'y = cos \\pi x'");
	gr->Box();	gr->Axis();	gr->Legend(2,"");
}
//-----------------------------------------------------------------------------
const char *mmgl_pulse="subplot 1 1 0 '<_':title 'Pulse sample'\n"
"new a 100 'exp(-6*x^2)':ranges 0 a.nx-1 0 1\naxis:plot a\n\n"
"pulse b a 'x'\n\ndefine m a.max\n\nline b(1) 0 b(1) m 'r='\n"
"line b(1)-b(3)/2 0  b(1)-b(3)/2 m 'm|'\nline b(1)+b(3)/2 0  b(1)+b(3)/2 m 'm|'\n"
"line 0 0.5*m a.nx-1 0.5*m 'h'\nnew x 100 'x'\nplot b(0)*(1-((x-b(1))/b(2))^2) 'g'";
void smgl_pulse(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("Pulse sample");
	mglData a(100);	gr->Fill(a,"exp(-6*x^2)");
	gr->SetRanges(0, a.nx-1, 0, 1);
	gr->Axis();	gr->Plot(a);
	mglData b(a.Pulse('x'));
	double m = b[0];
	gr->Line(mglPoint(b[1],0), mglPoint(b[1],m),"r=");
	gr->Line(mglPoint(b[1]-b[3]/2,0), mglPoint(b[1]-b[3]/2,m),"m|");
	gr->Line(mglPoint(b[1]+b[3]/2,0), mglPoint(b[1]+b[3]/2,m),"m|");
	gr->Line(mglPoint(0,m/2), mglPoint(a.nx-1,m/2),"h");
	char func[128];	sprintf(func,"%g*(1-((x-%g)/%g)^2)",b[0],b[1],b[2]);
	gr->FPlot(func,"g");
}
//-----------------------------------------------------------------------------
const char *mmgl_scanfile="subplot 1 1 0 '<_':title 'Save and scanfile sample'\n"
"list a 1 -1 0\nsave 'This is test: 0 -> ',a(0),' q' 'test.txt' 'w'\n"
"save 'This is test: 1 -> ',a(1),' q' 'test.txt'\nsave 'This is test: 2 -> ',a(2),' q' 'test.txt'\n"
"\nscanfile a 'test.txt' 'This is test: %g -> %g'\nranges a(0) a(1):axis:plot a(0) a(1) 'o'";
void smgl_scanfile(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("Save and scanfile sample");
	FILE *fp=fopen("test.txt","w");
	fprintf(fp,"This is test: 0 -> 1 q\n");
	fprintf(fp,"This is test: 1 -> -1 q\n");
	fprintf(fp,"This is test: 2 -> 0 q\n");
	fclose(fp);

	mglData a;
	a.ScanFile("test.txt","This is test: %g -> %g");
	gr->SetRanges(a.SubData(0), a.SubData(1));
	gr->Axis();	gr->Plot(a.SubData(0),a.SubData(1),"o");
}
//-----------------------------------------------------------------------------
const char *mmgl_pendelta="quality 6\nlist a 0.25 0.5 1 2 4\nfor $0 0 4\n"
"pendelta a($0)\ndefine $1 0.5*$0-1\nline -1 $1 1 $1 'r'\ntext 0 $1 'delta=',a($0)\nnext";
void smgl_pendelta(mglGraph *gr)
{
	double a[5]={0.25,0.5,1,2,4};
	gr->SetQuality(6);
	char buf[64];
	for(int i=0;i<5;i++)
	{
		gr->SetPenDelta(a[i]);
		gr->Line(mglPoint(-1,0.5*i-1), mglPoint(1,0.5*i-1),"r");
		sprintf(buf,"delta=%g",a[i]);
		gr->Puts(mglPoint(0,0.5*i-1),buf);
	}
}
//-----------------------------------------------------------------------------
const char *mmgl_bifurcation="subplot 1 1 0 '<_':title 'Bifurcation sample'\n"
"ranges 0 4 0 1:axis\nbifurcation 0.005 'x*y*(1-y)' 'r'";
void smgl_bifurcation(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("Bifurcation sample");
	gr->SetRanges(0,4,0,1);	gr->Axis();
	gr->Bifurcation(0.005,"x*y*(1-y)","r");
}
//-----------------------------------------------------------------------------
const char *mmgl_lamerey="subplot 1 1 0 '<_':title 'Lamerey sample'\n"
"axis:xlabel '\\i x':ylabel '\\bar{\\i x} = 2 \\i{x}'\nfplot 'x' 'k='\nfplot '2*x' 'b'\n"
"lamerey 0.00097 '2*x' 'rv~';size 2\nlamerey -0.00097 '2*x' 'rv~';size 2";
void smgl_lamerey(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("Lamerey sample");
	gr->Axis();	gr->Label('x',"\\i x");	gr->Label('y',"\\bar{\\i x} = 2 \\i{x}");
	gr->FPlot("x","k=");	gr->FPlot("2*x","b");
	gr->Lamerey( 0.00097,"2*x","rv~");
	gr->Lamerey(-0.00097,"2*x","rv~");
}
//-----------------------------------------------------------------------------
const char *mmgl_pmap="subplot 1 1 0 '<_^':title 'Poincare map sample'\n"
"ode r 'cos(y)+sin(z);cos(z)+sin(x);cos(x)+sin(y)' 'xyz' [0.1,0,0] 0.1 100\n"
"rotate 40 60:copy x r(0):copy y r(1):copy z r(2)\nranges x y z\naxis:plot x y z 'b'\n"
"xlabel '\\i x' 0:ylabel '\\i y' 0:zlabel '\\i z'\n"
"pmap x y z z 'b#o'\nfsurf '0'";
void smgl_pmap(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"<_^");
	if(big!=3)	gr->Title("Poincare map sample");
	mglData ini(3);	ini[0]=0.1;
	mglData r(mglODE("cos(y)+sin(z);cos(z)+sin(x);cos(x)+sin(y)","xyz",ini,0.1,100));
	mglData x(r.SubData(0)),y(r.SubData(1)), z(r.SubData(2));
	gr->Rotate(40,60);	gr->SetRanges(x,y,z);
	gr->Axis();	gr->FSurf("0");	gr->Plot(x,y,z,"b");
	gr->Label('x',"\\i x",0);	gr->Label('y',"\\i y",0);	gr->Label('z',"\\i z",0);
	gr->Pmap(x,y,z,z, "b#o");
}
//-----------------------------------------------------------------------------
const char *mmgl_apde="ranges -1 1 0 2 0 2\nnew ar 256 'exp(-2*(x+0.0)^2)'\nnew ai 256\n\n"
"apde res1 'exp(-x^2-p^2)' ar ai 0.01:transpose res1\npde res2 'exp(-x^2-p^2)' ar ai 0.01\n\n"
"subplot 1 2 0 '_':title 'Advanced PDE solver'\nranges 0 2 -1 1:crange res1\ndens res1:box\n"
"axis:xlabel '\\i z':ylabel '\\i x'\n"
"text -0.5 0.2 'i\\partial_z\\i u = exp(-\\i x^2+\\partial_x^2)[\\i u]' 'y'\n\n"
"subplot 1 2 1 '_':title 'Simplified PDE solver'\n"
"dens res2:box\naxis:xlabel '\\i z':ylabel '\\i x'\n"
"text -0.5 0.2 'i\\partial_z\\i u \\approx\\ exp(-\\i x^2)\\i u+exp(\\partial_x^2)[\\i u]' 'y'";
void smgl_apde(mglGraph *gr)
{
	gr->SetRanges(-1,1,0,2,0,2);
	mglData ar(256), ai(256);	gr->Fill(ar,"exp(-2*(x+0.0)^2)");

	mglData res1(gr->APDE("exp(-x^2-p^2)",ar,ai,0.01));	res1.Transpose();
	mglData res2(gr->PDE("exp(-x^2-p^2)",ar,ai,0.01));

	gr->SubPlot(1,2,0,"_");	gr->Title("Advanced PDE solver");
	gr->SetRanges(0,2,-1,1);	gr->SetRange('c',res1);
	gr->Dens(res1);	gr->Axis();	gr->Box();
	gr->Label('x',"\\i z");	gr->Label('y',"\\i x");
	gr->Puts(mglPoint(-0.5,0.2),"i\\partial_z\\i u = exp(-\\i x^2+\\partial_x^2)[\\i u]","y");

	gr->SubPlot(1,2,1,"_");	gr->Title("Simplified PDE solver");
	gr->Dens(res2);	gr->Axis();	gr->Box();
	gr->Label('x',"\\i z");	gr->Label('y',"\\i x");
	gr->Puts(mglPoint(-0.5,0.2),"i\\partial_z\\i u \\approx\\ exp(-\\i x^2)\\i u+exp(\\partial_x^2)[\\i u]","y");
}
//-----------------------------------------------------------------------------
const char *mmgl_ifs2d="list A [0.33,0,0,0.33,0,0,0.2] [0.33,0,0,0.33,0.67,0,0.2] [0.33,0,0,0.33,0.33,0.33,0.2]\\\n\t"
"[0.33,0,0,0.33,0,0.67,0.2] [0.33,0,0,0.33,0.67,0.67,0.2]\nifs2d fx fy A 100000\n"
"subplot 1 1 0 '<_':title 'IFS 2d sample'\nranges fx fy:axis\nplot fx fy 'r#o ';size 0.05";
void smgl_ifs2d(mglGraph *gr)
{
	mglData A;
	A.SetList(35, 0.33,0.,0.,0.33,0.,0.,0.2, 0.33,0.,0.,0.33,0.67,0.,0.2, 0.33,0.,0.,0.33,0.33,0.33,0.2, 0.33,0.,0.,0.33,0.,0.67,0.2, 0.33,0.,0.,0.33,0.67,0.67,0.2);
	A.Rearrange(7);
	mglData f(mglIFS2d(A,100000));
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("IFS 2d sample");
	gr->SetRanges(f.SubData(0), f.SubData(1));
	gr->Axis();	gr->Plot(f.SubData(0), f.SubData(1),"r#o ","size 0.05");
}
//-----------------------------------------------------------------------------
const char *mmgl_ifs3d="list A [0,0,0,0,.18,0,0,0,0,0,0,0,.01] [.85,0,0,0,.85,.1,0,-0.1,0.85,0,1.6,0,.85]\\\n"
"\t[.2,-.2,0,.2,.2,0,0,0,0.3,0,0.8,0,.07] [-.2,.2,0,.2,.2,0,0,0,0.3,0,0.8,0,.07]\n"
"ifs3d f A 100000\ntitle 'IFS 3d sample':rotate 50 60\n"
"ranges f(0) f(1) f(2):axis:box\ndots f(0) f(1) f(2) 'G#o';size 0.05";
void smgl_ifs3d(mglGraph *gr)
{
	mglData A;
	A.SetList(52, 0.,0.,0.,0.,.18,0.,0.,0.,0.,0.,0.,0.,.01, .85,0.,0.,0.,.85,.1,0.,-0.1,0.85,0.,1.6,0.,.85,
			.2,-.2,0.,.2,.2,0.,0.,0.,0.3,0.,0.8,0.,.07, -.2,.2,0.,.2,.2,0.,0.,0.,0.3,0.,0.8,0.,.07);
	A.Rearrange(13);
	mglData f(mglIFS3d(A,100000));
	if(big!=3)	gr->Title("IFS 3d sample");
	gr->SetRanges(f.SubData(0), f.SubData(1), f.SubData(2));
	gr->Rotate(50,60);	gr->Axis();	gr->Box();
	gr->Dots(f.SubData(0), f.SubData(1), f.SubData(2),"G#o","size 0.05");
}
//-----------------------------------------------------------------------------
const char *mmgl_flame2d="list A [0.33,0,0,0.33,0,0,0.2] [0.33,0,0,0.33,0.67,0,0.2] [0.33,0,0,0.33,0.33,0.33,0.2]\\\n"
"\t[0.33,0,0,0.33,0,0.67,0.2] [0.33,0,0,0.33,0.67,0.67,0.2]\n"
"new B 2 3 A.ny '0.3'\nput B 3 0 0 -1\nput B 3 0 1 -1\nput B 3 0 2 -1\n"
"flame2d fx fy A B 1000000\nsubplot 1 1 0 '<_':title 'Flame2d sample'\n"
"ranges fx fy:box:axis\nplot fx fy 'r#o ';size 0.05";
void smgl_flame2d(mglGraph *gr)
{
	mglData A, B(2,3,5);
	A.SetList(35, 0.33,0.,0.,0.33,0.,0.,0.2, 0.33,0.,0.,0.33,0.67,0.,0.2, 0.33,0.,0.,0.33,0.33,0.33,0.2,
			0.33,0.,0.,0.33,0.,0.67,0.2, 0.33,0.,0.,0.33,0.67,0.67,0.2);
	A.Rearrange(7);
	for(long i=0;i<2*3*5;i++)	B.a[i] = 0.3;
	for(long i=0;i<5;i++)	B.a[2*3*i] = B.a[2*3*i+1*2] = B.a[2*3*i+2*2] = 3;
	mglData f(mglFlame2d(A,B,1000000));
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("Flame2d sample");
	gr->SetRanges(f.SubData(0), f.SubData(1));
	gr->Axis();	gr->Box();
	gr->Plot(f.SubData(0), f.SubData(1),"r#o ","size 0.05");
}
//-----------------------------------------------------------------------------
const char *mmgl_detect="subplot 1 1 0 '':title 'Detect sample'\n"
"new a 200 100 'exp(-30*(y-0.5*sin(pi*x))^2-rnd/10)+exp(-30*(y+0.5*sin(pi*x))^2-rnd/10)+exp(-30*(x+y)^2-rnd/10)'\n"
"ranges 0 a.nx 0 a.ny:box\nalpha on:crange a:dens a\n\n"
"detect r a 0.1 5\nplot r(0) r(1) '.'";
void smgl_detect(mglGraph *gr)
{
	mglData a(200, 100);
	gr->Fill(a,"exp(-30*(y-0.5*sin(pi*x))^2-rnd/10)+exp(-30*(y+0.5*sin(pi*x))^2-rnd/10)+exp(-30*(x+y)^2-rnd/10)");
	gr->SubPlot(1,1,0,"");
	if(big!=3)	gr->Title("Detect sample");
	gr->SetRanges(0,a.nx,0,a.ny);	gr->SetRange('c',a);
	gr->Alpha(true);	gr->Box();	gr->Dens(a);
	mglData r(a.Detect(0.1,5));
	gr->Plot(r.SubData(0), r.SubData(1), ".");
}
//-----------------------------------------------------------------------------
const char *mmgl_iris="read a 'iris.dat'\ncrop a 0 4 'x':rearrange a a.nx 50\n"
"subplot 1 1 0 '':title 'Iris plot'\n"
"iris a 'sepal\\n length;sepal\\n width;petal\\n length;petal\\n width' '. ';value -1.5;size -2";
void smgl_iris(mglGraph *gr)
{
	mglData a("iris.dat");	a.Crop(0,4,'x');	a.Rearrange(4,50);
	gr->SubPlot(1,1,0,"");
	if(big!=3)	gr->Title("Iris sample");
	gr->Iris(a, "sepal\nlength;sepal\nwidth;petal\nlength;petal\nwidth", ". ", "value -1.5;size -2");
}
//-----------------------------------------------------------------------------
const char *mmgl_dilate="subplot 2 2 0:title 'Dilate&Erode 1D sample'\n"
"new y 11:put y 1 5\nranges 0 10 0 1:axis:box\nplot y 'b*'\n"
"dilate y 0.5 2\nplot y 'rs'\nerode y 0.5 1\nplot y 'g#o'\n\n"
"subplot 2 2 1:title 'Dilate&Erode 2D sample':rotate 40 60\n"
"ranges 0 10 0 10 0 3\naxis:box\nnew z 11 11:put z 3 5 5\n"
"boxs z 'b':boxs z 'k#'\ndilate z 1 2\nboxs z 'r':boxs z 'k#'\n"
"erode z 1 1\nboxs 2*z 'g':boxs 2*z 'k#'\n\n"
"subplot 2 2 2\ntext 0.5 0.7 'initial' 'ba';size -2\n"
"text 0.5 0.5 'dilate=2' 'ra';size -2\ntext 0.5 0.3 'erode=1' 'ga';size -2\n\n"
"subplot 2 2 3:title 'Dilate&Erode 3D sample'\nrotate 60 50:light on:alpha on\n"
"ranges 0 10 0 10 0 10:crange 0 3\naxis:box\nnew a 11 11 11:put a 3 5 5 5\n"
"surf3a a a 1.5 'b'\ndilate a 1 2\nsurf3a a a 0.5 'r'\n"
"erode a 1 1\nsurf3a 2*a 2*a 1 'g'";
void smgl_dilate(mglGraph *gr)
{
	mglData y(11),	z(11,11), a(11,11,11);
	y.a[5]=1;	z.a[5+11*5]=a.a[5+11*(5+11*5)] = 3;

	if(big!=3)	{	gr->SubPlot(2,2,0);	gr->Title("Dilate&Erode 1D sample");	}
	else	gr->SubPlot(1,1,0,"");
	gr->SetRanges(0,10,0,1);	gr->Axis();	gr->Box();	gr->Plot(y,"b*");
	y.Dilate(1,2);	gr->Plot(y,"rs");
	y.Erode(1,1);	gr->Plot(y,"g#o");
	if(big==3)	return;
	
	gr->SubPlot(2,2,1);	gr->Title("Dilate&Erode 2D sample");
	gr->Rotate(40,60);	gr->SetRanges(0,10,0,10,0,3);
	gr->Axis();	gr->Box();	gr->Boxs(z,"b");	gr->Boxs(z,"k#");
	z.Dilate(1,2);			gr->Boxs(z,"r");	gr->Boxs(z,"k#");
	z.Erode(1,1);	z*=2;	gr->Boxs(z,"g");	gr->Boxs(z,"k#");
	
	gr->SubPlot(2,2,2);
	gr->Puts(0.5,0.7,"initial","ba",-2);
	gr->Puts(0.5,0.5,"dilate=2","ra",-2);
	gr->Puts(0.5,0.3,"erode=1","ga",-2);
	
	gr->SubPlot(2,2,3);	gr->Title("Dilate&Erode 3D sample");
	gr->Rotate(60,50);	gr->Alpha(true);	gr->Light(true);
	gr->SetRanges(0,10,0,10,0,10);	gr->SetRange('c',0,3);
	gr->Axis();	gr->Box();	gr->Surf3A(1.5,a,a,"b");
	a.Dilate(1,2);			gr->Surf3A(0.5,a,a,"r");
	a.Erode(1,1);	a*=2;	gr->Surf3A(1,a,a,"g");
}
//-----------------------------------------------------------------------------
const char *mmgl_section="subplot 1 1 0 '<_':title 'Section&Join sample'\n"
"axis:box:line -1 0 1 0 'h:'\n# first lets demonstrate 'join'\n"
"new aa 11 'x^2':new a1 3 '-x':new a2 15 'x^3'\njoin aa a1:join aa a2\n"
"# add x-coordinate\nnew xx aa.nx 'x':join aa xx\nplot aa(:,1) aa(:,0) '2y'\n"
"# now select 1-st (id=0) section between zeros\n"
"section b1 aa 0 'x' 0\nplot b1(:,1) b1(:,0) 'bo'\n"
"# next, select 3-d (id=2) section between zeros\n"
"section b3 aa 2 'x' 0\nplot b3(:,1) b3(:,0) 'gs'\n"
"# finally, select 2-nd (id=-2) section from the end\n"
"section b4 aa -2 'x' 0\nplot b4(:,1) b4(:,0) 'r#o'";
void smgl_section(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("Section&Join sample");
	gr->Axis();	gr->Box();	gr->Line(mglPoint(-1,0),mglPoint(1,0),"h:");
	// first lets demonstrate 'join'
	mglData aa(11), a1(3), a2(15);
	gr->Fill(aa,"x^2");	gr->Fill(a1,"-x");	gr->Fill(a2,"x^3");
	aa.Join(a1);	aa.Join(a2);
	// add x-coordinate
	mglData xx(aa.nx);	gr->Fill(xx,"x");	aa.Join(xx);
	gr->Plot(aa.SubData(-1,1), aa.SubData(-1,0), "2y");
	// now select 1-st (id=0) section between zeros
	mglData b1(aa.Section(0,'x',0));
	gr->Plot(b1.SubData(-1,1), b1.SubData(-1,0), "bo");
	// next, select 3-d (id=2) section between zeros
	mglData b2(aa.Section(2,'x',0));
	gr->Plot(b2.SubData(-1,1), b2.SubData(-1,0), "gs");
	// finally, select 2-nd (id=-2) section from the end
	mglData b3(aa.Section(-2,'x',0));
	gr->Plot(b3.SubData(-1,1), b3.SubData(-1,0), "r#o");
}
//-----------------------------------------------------------------------------
const char *mmgl_3wave="define t 50\n"
"ode !r '-b*f;a*conj(f);a*conj(b)-0.1*f' 'abf' [1,1e-3,0] 0.1 t\n"
"ranges 0 t 0 r.max\nplot r(0) 'b';legend 'a'\n"
"plot r(1) 'g';legend 'b'\nplot r(2) 'r';legend 'f'\n"
"axis:box:legend";
void smgl_3wave(mglGraph *gr)
{
	gr->SubPlot(1,1,0,"<_");
	if(big!=3)	gr->Title("Complex ODE sample");
	double t=50;
	mglData ini;	ini.SetList(3, 1., 1e-3, 0.);
	mglDataC r(mglODEc("-b*f;a*conj(f);a*conj(b)-0.1*f","abf",ini,0.1,t));
	gr->SetRanges(0, t, 0, r.Maximal());
	gr->Plot(r.SubData(0),"b","legend 'a'");
	gr->Plot(r.SubData(1),"g","legend 'b'");
	gr->Plot(r.SubData(2),"r","legend 'f'");
	gr->Axis();	gr->Box();	gr->Legend();
}
//-----------------------------------------------------------------------------
const char *mmgl_diffract="define n 32	#number of points\ndefine m 20 # number of iterations\n"
"define dt 0.01 # time step\nnew res n m+1\nranges -1 1 0 m*dt 0 1\n\n"
"#tridmat periodic variant\nnew !a n 'i',dt*(n/2)^2/2\ncopy !b !(1-2*a)\n\n"
"new !u n 'exp(-6*x^2)'\nput res u all 0\nfor $i 0 m\ntridmat u a b a u 'xdc'\n"
"put res u all $i+1\nnext\nsubplot 2 2 0 '<_':title 'Tridmat, periodic b.c.'\naxis:box:dens res\n\n"
"#fourier variant\nnew k n:fillsample k 'xk'\ncopy !e !exp(-i1*dt*k^2)\n\n"
"new !u n 'exp(-6*x^2)'\nput res u all 0\nfor $i 0 m\nfourier u 'x'\nmulto u e\nfourier u 'ix'\n"
"put res u all $i+1\nnext\nsubplot 2 2 1 '<_':title 'Fourier method'\naxis:box:dens res\n\n"
"#tridmat zero variant\nnew !u n 'exp(-6*x^2)'\nput res u all 0\nfor $i 0 m\ntridmat u a b a u 'xd'\n"
"put res u all $i+1\nnext\nsubplot 2 2 2 '<_':title 'Tridmat, zero b.c.'\naxis:box:dens res\n\n"
"#diffract exp variant\nnew !u n 'exp(-6*x^2)'\ndefine q dt*(n/2)^2/8 # need q<0.4 !!!\n"
"put res u all 0\nfor $i 0 m\nfor $j 1 8	# due to smaller dt\ndiffract u 'xe' q\nnext\n"
"put res u all $i+1\nnext\nsubplot 2 2 3 '<_':title 'Diffract, exp b.c.'\naxis:box:dens res";
void smgl_diffract(mglGraph *gr)
{
	long n=32;	// number of points
	long m=20;	// number of iterations
	double dt=0.01;	// time step
	mglData res(n,m+1);
	gr->SetRanges(-1,1, 0,m*dt, 0,1);

	// tridmat periodic variant
	mglDataC a(n), b(n);	a = dual(0,dt*n*n/8);
	for(long i=0;i<n;i++)	b.a[i] = mreal(1)-mreal(2)*a.a[i];
	mglDataC u(n);	gr->Fill(u,"exp(-6*x^2)");	res.Put(u,-1,0);
	for(long i=0;i<m;i++)
	{
		u = mglTridMatC(a,b,a,u,"xdc");
		res.Put(u,-1,i+1);
	}
	gr->SubPlot(2,2,0,"<_");	gr->Title("Tridmat, periodic b.c.");
	gr->Axis();	gr->Box();	gr->Dens(res);

	// fourier variant
	mglData k(n);	k.FillSample("xk");
	mglDataC e(n);	for(long i=0;i<n;i++)	e.a[i] = exp(-dual(0,dt*k.a[i]*k.a[i]));
	gr->Fill(u,"exp(-6*x^2)");	res.Put(u,-1,0);
	for(long i=0;i<m;i++)
	{
		u.FFT("x");	u *= e;	u.FFT("ix");
		res.Put(u,-1,i+1);
	}
	gr->SubPlot(2,2,1,"<_");	gr->Title("Fourier method");
	gr->Axis();	gr->Box();	gr->Dens(res);

	// tridmat zero variant
	gr->Fill(u,"exp(-6*x^2)");	res.Put(u,-1,0);
	for(long i=0;i<m;i++)
	{
		u = mglTridMatC(a,b,a,u,"xd");
		res.Put(u,-1,i+1);
	}
	gr->SubPlot(2,2,2,"<_");	gr->Title("Tridmat, zero b.c.");
	gr->Axis();	gr->Box();	gr->Dens(res);
	
	// diffract exp variant
	gr->Fill(u,"exp(-6*x^2)");	res.Put(u,-1,0);
	double q=dt*n*n/4/8;	// NOTE: need q<0.4 !!!
	for(long i=0;i<m;i++)
	{
		for(long j=0;j<8;j++)	// due to smaller dt
			u.Diffraction("xe",q);
		res.Put(u,-1,i+1);
	}
	gr->SubPlot(2,2,3,"<_");	gr->Title("Diffract, exp b.c.");
	gr->Axis();	gr->Box();	gr->Dens(res);
}
//-----------------------------------------------------------------------------
const char *mmgl_earth="import dat 'Equirectangular-projection.jpg' 'BbGYw' -1 1\n"
"subplot 1 1 0 '<>':title 'Earth in 3D':rotate 40 60\n"
"copy phi dat 'pi*x':copy tet dat 'pi*y/2'\n"
"copy x cos(tet)*cos(phi)\ncopy y cos(tet)*sin(phi)\ncopy z sin(tet)\n\n"
"light on\nsurfc x y z dat 'BbGYw'\ncontp [-0.51,-0.51] x y z dat 'y'";
void smgl_earth(mglGraph *gr)
{
	mglData dat;	dat.Import("Equirectangular-projection.jpg","BbGYw",-1,1);
	// Calc proper 3d coordinates from projection
	mglData phi(dat.nx,dat.ny);	phi.Fill(-M_PI,M_PI);
	mglData tet(dat.nx,dat.ny);	tet.Fill(-M_PI/2,M_PI/2,'y');
	mglData x(dat.nx,dat.ny), y(dat.nx,dat.ny), z(dat.nx,dat.ny);
#pragma omp parallel for
	for(long i=0;i<dat.nx*dat.ny;i++)
	{	x.a[i] = cos(tet.a[i])*cos(phi.a[i]);
		y.a[i] = cos(tet.a[i])*sin(phi.a[i]);
		z.a[i] = sin(tet.a[i]);	}

	gr->SubPlot(1,1,0,"<>");
	if(big!=3)	gr->Title("Earth in 3D");
	gr->Rotate(40,60);	gr->Light(true);
	gr->SurfC(x,y,z,dat,"BbGYw");
	mglData vals(1);	vals.a[0]=-0.51;
	gr->ContP(vals, x,y,z,dat,"y");
}
//-----------------------------------------------------------------------------
#define all_prims_str "subplot 3 2 0:define y 0.95\n\
define d 0.3:define x0 0.2:define x1 0.5:define x2 0.6\n\
line x0 1-0*d x1 1-0*d 'k-':text x2 y-0*d 'Solid `-`' ':rL'\n\
line x0 1-1*d x1 1-1*d 'k|':text x2 y-1*d 'Long Dash `|`' ':rL'\n\
line x0 1-2*d x1 1-2*d 'k;':text x2 y-2*d 'Dash 1;`' ':rL'\n\
line x0 1-3*d x1 1-3*d 'k=':text x2 y-3*d 'Small dash `=`' ':rL'\n\
line x0 1-4*d x1 1-4*d 'kj':text x2 y-4*d 'Dash-dot `j`' ':rL'\n\
line x0 1-5*d x1 1-5*d 'ki':text x2 y-5*d 'Small dash-dot `i`' ':rL'\n\
line x0 1-6*d x1 1-6*d 'k:':text x2 y-6*d 'Dots `:`' ':rL'\n\
line x0 1-7*d x1 1-7*d 'k ':text x2 y-7*d 'None ``' ':rL'\n\
define d 0.25:define x0 -0.8:define x1 -1:define x2 -0.05\n\
ball x1 5*d 'k.':text x0 5*d '.' ':rL'\n\
ball x1 4*d 'k+':text x0 4*d '+' ':rL'\n\
ball x1 3*d 'kx':text x0 3*d 'x' ':rL'\n\
ball x1 2*d 'k*':text x0 2*d '*' ':rL'\n\
ball x1 d 'ks':text x0 d 's' ':rL'\n\
ball x1 0 'kd':text x0 0 'd' ':rL'\n\
ball x1 -d 0 'ko':text x0 y-d 'o' ':rL'\n\
ball x1 -2*d 0 'k^':text x0 -2*d '\\^' ':rL'\n\
ball x1 -3*d 0 'kv':text x0 -3*d 'v' ':rL'\n\
ball x1 -4*d 0 'k<':text x0 -4*d '<' ':rL'\n\
ball x1 -5*d 0 'k>':text x0 -5*d '>' ':rL'\n\n\
define x0 -0.3:define x1 -0.5\n\
ball x1 5*d 'k#.':text x0 5*d '\\#.' ':rL'\n\
ball x1 4*d 'k#+':text x0 4*d '\\#+' ':rL'\n\
ball x1 3*d 'k#x':text x0 3*d '\\#x' ':rL'\n\
ball x1 2*d 'k#*':text x0 2*d '\\#*' ':rL'\n\
ball x1 d 'k#s':text x0 d '\\#s' ':rL'\n\
ball x1 0 'k#d':text x0 0 '\\#d' ':rL'\n\
ball x1 -d 0 'k#o':text x0 -d '\\#o' ':rL'\n\
ball x1 -2*d 0 'k#^':text x0 -2*d '\\#\\^' ':rL'\n\
ball x1 -3*d 0 'k#v':text x0 -3*d '\\#v' ':rL'\n\
ball x1 -4*d 0 'k#<':text x0 -4*d '\\#<' ':rL'\n\
ball x1 -5*d 0 'k#>':text x0 -5*d '\\#>' ':rL'\n\n\
subplot 3 2 1\ndefine a 0.1:define b 0.4:define c 0.5\n\
line a 1 b 1 'k-A':text c 1 'Style `A` or `A\\_`' ':rL'\n\
line a 0.8 b 0.8 'k-V':text c 0.8 'Style `V` or `V\\_`' ':rL'\n\
line a 0.6 b 0.6 'k-K':text c 0.6 'Style `K` or `K\\_`' ':rL'\n\
line a 0.4 b 0.4 'k-I':text c 0.4 'Style `I` or `I\\_`' ':rL'\n\
line a 0.2 b 0.2 'k-D':text c 0.2 'Style `D` or `D\\_`' ':rL'\n\
line a 0 b 0 'k-S':text c 0 'Style `S` or `S\\_`' ':rL'\n\
line a -0.2 b -0.2 'k-O':text c -0.2 'Style `O` or `O\\_`' ':rL'\n\
line a -0.4 b -0.4 'k-T':text c -0.4 'Style `T` or `T\\_`' ':rL'\n\
line a -0.6 b -0.6 'k-_':text c -0.6 'Style `\\_` or none' ':rL'\n\
line a -0.8 b -0.8 'k-AS':text c -0.8 'Style `AS`' ':rL'\n\
line a -1 b -1 'k-_A':text c -1 'Style `\\_A`' ':rL'\n\n\
define a -1:define b -0.7:define c -0.6\n\
line a 1 b 1 'kAA':text c 1 'Style `AA`' ':rL'\n\
line a 0.8 b 0.8 'kVV':text c 0.8 'Style `VV`' ':rL'\n\
line a 0.6 b 0.6 'kKK':text c 0.6 'Style `KK`' ':rL'\n\
line a 0.4 b 0.4 'kII':text c 0.4 'Style `II`' ':rL'\n\
line a 0.2 b 0.2 'kDD':text c 0.2 'Style `DD`' ':rL'\n\
line a 0 b 0 'kSS':text c 0 'Style `SS`' ':rL'\n\
line a -0.2 b -0.2 'kOO':text c -0.2 'Style `OO`' ':rL'\n\
line a -0.4 b -0.4 'kTT':text c -0.4 'Style `TT`' ':rL'\n\
line a -0.6 b -0.6 'k-__':text c -0.6 'Style `\\_\\_`' ':rL'\n\
line a -0.8 b -0.8 'k-VA':text c -0.8 'Style `VA`' ':rL'\n\
line a -1 b -1 'k-AV':text c -1 'Style `AV`' ':rL'\n\n\
subplot 3 2 2\n#LENUQ\n\n\
facez -1 -1 0 0.4 0.3 'L#':text -0.8 -0.9 'L' 'w:C' -1.4\n\
facez -0.6 -1 0 0.4 0.3 'E#':text -0.4 -0.9 'E' 'w:C' -1.4\n\
facez -0.2 -1 0 0.4 0.3 'N#':text 0 -0.9 'N' 'w:C' -1.4\n\
facez 0.2 -1 0 0.4 0.3 'U#':text 0.4 -0.9 'U' 'w:C' -1.4\n\
facez 0.6 -1 0 0.4 0.3 'Q#':text 0.8 -0.9 'Q' 'w:C' -1.4\n\
#lenuq\nfacez -1 -0.7 0 0.4 0.3 'l#':text -0.8 -0.6 'l' 'k:C' -1.4\n\
facez -0.6 -0.7 0 0.4 0.3 'e#':text -0.4 -0.6 'e' 'k:C' -1.4\n\
facez -0.2 -0.7 0 0.4 0.3 'n#':text 0 -0.6 'n' 'k:C' -1.4\n\
facez 0.2 -0.7 0 0.4 0.3 'u#':text 0.4 -0.6 'u' 'k:C' -1.4\n\
facez 0.6 -0.7 0 0.4 0.3 'q#':text 0.8 -0.6 'q' 'k:C' -1.4\n\
#CMYkP\nfacez -1 -0.4 0 0.4 0.3 'C#':text -0.8 -0.3 'C' 'w:C' -1.4\n\
facez -0.6 -0.4 0 0.4 0.3 'M#':text -0.4 -0.3 'M' 'w:C' -1.4\n\
facez -0.2 -0.4 0 0.4 0.3 'Y#':text 0 -0.3 'Y' 'w:C' -1.4\n\
facez 0.2 -0.4 0 0.4 0.3 'k#':text 0.4 -0.3 'k' 'w:C' -1.4\n\
facez 0.6 -0.4 0 0.4 0.3 'P#':text 0.8 -0.3 'P' 'w:C' -1.4\n\
#cmywp\nfacez -1 -0.1 0 0.4 0.3 'c#':text -0.8 0 'c' 'k:C' -1.4\n\
facez -0.6 -0.1 0 0.4 0.3 'm#':text -0.4 0 'm' 'k:C' -1.4\n\
facez -0.2 -0.1 0 0.4 0.3 'y#':text 0 0 'y' 'k:C' -1.4\n\
facez 0.2 -0.1 0 0.4 0.3 'w#':text 0.4 0 'w' 'k:C' -1.4\n\
facez 0.6 -0.1 0 0.4 0.3 'p#':text 0.8 0 'p' 'k:C' -1.4\n\
#BGRHW\nfacez -1 0.2 0 0.4 0.3 'B#':text -0.8 0.3 'B' 'w:C' -1.4\n\
facez -0.6 0.2 0 0.4 0.3 'G#':text -0.4 0.3 'G' 'w:C' -1.4\n\
facez -0.2 0.2 0 0.4 0.3 'R#':text 0 0.3 'R' 'w:C' -1.4\n\
facez 0.2 0.2 0 0.4 0.3 'H#':text 0.4 0.3 'H' 'w:C' -1.4\n\
facez 0.6 0.2 0 0.4 0.3 'W#':text 0.8 0.3 'W' 'w:C' -1.4\n\
#bgrhw\nfacez -1 0.5 0 0.4 0.3 'b#':text -0.8 0.6 'b' 'k:C' -1.4\n\
facez -0.6 0.5 0 0.4 0.3 'g#':text -0.4 0.6 'g' 'k:C' -1.4\n\
facez -0.2 0.5 0 0.4 0.3 'r#':text 0 0.6 'r' 'k:C' -1.4\n\
facez 0.2 0.5 0 0.4 0.3 'h#':text 0.4 0.6 'h' 'k:C' -1.4\n\
facez 0.6 0.5 0 0.4 0.3 'w#':text 0.8 0.6 'w' 'k:C' -1.4\n\
#brighted\nfacez -1 0.8 0 0.4 0.3 '{r1}#':text -0.8 0.9 '\\{r1\\}' 'w:C' -1.4\n\
facez -0.6 0.8 0 0.4 0.3 '{r3}#':text -0.4 0.9 '\\{r3\\}' 'w:C' -1.4\n\
facez -0.2 0.8 0 0.4 0.3 '{r5}#':text 0 0.9 '\\{r5\\}' 'k:C' -1.4\n\
facez 0.2 0.8 0 0.4 0.3 '{r7}#':text 0.4 0.9 '\\{r7\\}' 'k:C' -1.4\n\
facez 0.6 0.8 0 0.4 0.3 '{r9}#':text 0.8 0.9 '\\{r9\\}' 'k:C' -1.4\n\
# HEX\nfacez -1 -1.3 0 1 0.3 '{xff9966}#':text -0.5 -1.2 '\\{xff9966\\}' 'k:C' -1.4\n\
facez 0 -1.3 0 1 0.3 '{x83CAFF}#':text 0.5 -1.2 '\\{x83caff\\}' 'k:C' -1.4\n\n\
subplot 3 2 3\nfor $i 0 9\nline -1 0.2*$i-1 1 0.2*$i-1 'r','0'+$i\n\
text 1.05 0.2*$i-1 '0'+$i ':L'\nnext\n\n\
subplot 3 2 4:title 'TriPlot sample':rotate 50 60\n\
list tt 0 1 2 | 0 1 3 | 0 2 3 | 1 2 3\n\
list xt -1 1 0 0:list yt -1 -1 1 0:list zt -1 -1 -1 1:light on\n\
triplot tt xt yt zt 'b':triplot tt xt yt zt 'k#'\n\n\
subplot 3 2 5:new r 4 'i+1':ranges 1 4 1 4\naxis:mark r r 's':plot r 'b'\n"
void all_prims(mglGraph *gr)	// test drawing of all kinds
{
	gr->SubPlot(3,2,0);
	double d,x1,x2,x0,y=0.95;
	d=0.3, x0=0.2, x1=0.5, x2=0.6;
	gr->Line(mglPoint(x0,1-0*d),mglPoint(x1,1-0*d),"k-");	gr->Puts(mglPoint(x2,y-0*d),"Solid '-'",":rL");
	gr->Line(mglPoint(x0,1-1*d),mglPoint(x1,1-1*d),"k|");	gr->Puts(mglPoint(x2,y-1*d),"Long Dash '|'",":rL");
	gr->Line(mglPoint(x0,1-2*d),mglPoint(x1,1-2*d),"k;");	gr->Puts(mglPoint(x2,y-2*d),"Dash ';'",":rL");
	gr->Line(mglPoint(x0,1-3*d),mglPoint(x1,1-3*d),"k=");	gr->Puts(mglPoint(x2,y-3*d),"Small dash '='",":rL");
	gr->Line(mglPoint(x0,1-4*d),mglPoint(x1,1-4*d),"kj");	gr->Puts(mglPoint(x2,y-4*d),"Dash-dot 'j'",":rL");
	gr->Line(mglPoint(x0,1-5*d),mglPoint(x1,1-5*d),"ki");	gr->Puts(mglPoint(x2,y-5*d),"Small dash-dot 'i'",":rL");
	gr->Line(mglPoint(x0,1-6*d),mglPoint(x1,1-6*d),"k:");	gr->Puts(mglPoint(x2,y-6*d),"Dots ':'",":rL");
	gr->Line(mglPoint(x0,1-7*d),mglPoint(x1,1-7*d),"k ");	gr->Puts(mglPoint(x2,y-7*d),"None ' '",":rL");

	d=0.25; x1=-1; x0=-0.8;	y = -0.05;
	gr->Mark(mglPoint(x1,5*d),"k.");	gr->Puts(mglPoint(x0,y+5*d),"'.'",":rL");
	gr->Mark(mglPoint(x1,4*d),"k+");	gr->Puts(mglPoint(x0,y+4*d),"'+'",":rL");
	gr->Mark(mglPoint(x1,3*d),"kx");	gr->Puts(mglPoint(x0,y+3*d),"'x'",":rL");
	gr->Mark(mglPoint(x1,2*d),"k*");	gr->Puts(mglPoint(x0,y+2*d),"'*'",":rL");
	gr->Mark(mglPoint(x1,d),"ks");		gr->Puts(mglPoint(x0,y+d),"'s'",":rL");
	gr->Mark(mglPoint(x1,0),"kd");		gr->Puts(mglPoint(x0,y),"'d'",":rL");
	gr->Mark(mglPoint(x1,-d,0),"ko");	gr->Puts(mglPoint(x0,y-d),"'o'",":rL");
	gr->Mark(mglPoint(x1,-2*d,0),"k^");	gr->Puts(mglPoint(x0,y-2*d),"'\\^'",":rL");
	gr->Mark(mglPoint(x1,-3*d,0),"kv");	gr->Puts(mglPoint(x0,y-3*d),"'v'",":rL");
	gr->Mark(mglPoint(x1,-4*d,0),"k<");	gr->Puts(mglPoint(x0,y-4*d),"'<'",":rL");
	gr->Mark(mglPoint(x1,-5*d,0),"k>");	gr->Puts(mglPoint(x0,y-5*d),"'>'",":rL");

	d=0.25; x1=-0.5; x0=-0.3;	y = -0.05;
	gr->Mark(mglPoint(x1,5*d),"k#.");	gr->Puts(mglPoint(x0,y+5*d),"'\\#.'",":rL");
	gr->Mark(mglPoint(x1,4*d),"k#+");	gr->Puts(mglPoint(x0,y+4*d),"'\\#+'",":rL");
	gr->Mark(mglPoint(x1,3*d),"k#x");	gr->Puts(mglPoint(x0,y+3*d),"'\\#x'",":rL");
	gr->Mark(mglPoint(x1,2*d),"k#*");	gr->Puts(mglPoint(x0,y+2*d),"'\\#*'",":rL");
	gr->Mark(mglPoint(x1,d),"k#s");		gr->Puts(mglPoint(x0,y+d),"'\\#s'",":rL");
	gr->Mark(mglPoint(x1,0),"k#d");		gr->Puts(mglPoint(x0,y),"'\\#d'",":rL");
	gr->Mark(mglPoint(x1,-d,0),"k#o");	gr->Puts(mglPoint(x0,y-d),"'\\#o'",":rL");
	gr->Mark(mglPoint(x1,-2*d,0),"k#^");	gr->Puts(mglPoint(x0,y-2*d),"'\\#\\^'",":rL");
	gr->Mark(mglPoint(x1,-3*d,0),"k#v");	gr->Puts(mglPoint(x0,y-3*d),"'\\#v'",":rL");
	gr->Mark(mglPoint(x1,-4*d,0),"k#<");	gr->Puts(mglPoint(x0,y-4*d),"'\\#<'",":rL");
	gr->Mark(mglPoint(x1,-5*d,0),"k#>");	gr->Puts(mglPoint(x0,y-5*d),"'\\#>'",":rL");

	gr->SubPlot(3,2,1);
	double a=0.1,b=0.4,c=0.5;
	gr->Line(mglPoint(a,1),mglPoint(b,1),"k-A");		gr->Puts(mglPoint(c,1),"Style 'A' or 'A\\_'",":rL");
	gr->Line(mglPoint(a,0.8),mglPoint(b,0.8),"k-V");	gr->Puts(mglPoint(c,0.8),"Style 'V' or 'V\\_'",":rL");
	gr->Line(mglPoint(a,0.6),mglPoint(b,0.6),"k-K");	gr->Puts(mglPoint(c,0.6),"Style 'K' or 'K\\_'",":rL");
	gr->Line(mglPoint(a,0.4),mglPoint(b,0.4),"k-I");	gr->Puts(mglPoint(c,0.4),"Style 'I' or 'I\\_'",":rL");
	gr->Line(mglPoint(a,0.2),mglPoint(b,0.2),"k-D");	gr->Puts(mglPoint(c,0.2),"Style 'D' or 'D\\_'",":rL");
	gr->Line(mglPoint(a,0),mglPoint(b,0),"k-S");		gr->Puts(mglPoint(c,0),"Style 'S' or 'S\\_'",":rL");
	gr->Line(mglPoint(a,-0.2),mglPoint(b,-0.2),"k-O");	gr->Puts(mglPoint(c,-0.2),"Style 'O' or 'O\\_'",":rL");
	gr->Line(mglPoint(a,-0.4),mglPoint(b,-0.4),"k-T");	gr->Puts(mglPoint(c,-0.4),"Style 'T' or 'T\\_'",":rL");
	gr->Line(mglPoint(a,-0.6),mglPoint(b,-0.6),"k-_");	gr->Puts(mglPoint(c,-0.6),"Style '\\_' or none",":rL");
	gr->Line(mglPoint(a,-0.8),mglPoint(b,-0.8),"k-AS");	gr->Puts(mglPoint(c,-0.8),"Style 'AS'",":rL");
	gr->Line(mglPoint(a,-1),mglPoint(b,-1),"k-_A");		gr->Puts(mglPoint(c,-1),"Style '\\_A'",":rL");

	a=-1;	b=-0.7;	c=-0.6;
	gr->Line(mglPoint(a,1),mglPoint(b,1),"kAA");		gr->Puts(mglPoint(c,1),"Style 'AA'",":rL");
	gr->Line(mglPoint(a,0.8),mglPoint(b,0.8),"kVV");	gr->Puts(mglPoint(c,0.8),"Style 'VV'",":rL");
	gr->Line(mglPoint(a,0.6),mglPoint(b,0.6),"kKK");	gr->Puts(mglPoint(c,0.6),"Style 'KK'",":rL");
	gr->Line(mglPoint(a,0.4),mglPoint(b,0.4),"kII");	gr->Puts(mglPoint(c,0.4),"Style 'II'",":rL");
	gr->Line(mglPoint(a,0.2),mglPoint(b,0.2),"kDD");	gr->Puts(mglPoint(c,0.2),"Style 'DD'",":rL");
	gr->Line(mglPoint(a,0),mglPoint(b,0),"kSS");		gr->Puts(mglPoint(c,0),"Style 'SS'",":rL");
	gr->Line(mglPoint(a,-0.2),mglPoint(b,-0.2),"kOO");	gr->Puts(mglPoint(c,-0.2),"Style 'OO'",":rL");
	gr->Line(mglPoint(a,-0.4),mglPoint(b,-0.4),"kTT");	gr->Puts(mglPoint(c,-0.4),"Style 'TT'",":rL");
	gr->Line(mglPoint(a,-0.6),mglPoint(b,-0.6),"k-__");	gr->Puts(mglPoint(c,-0.6),"Style '\\_\\_'",":rL");
	gr->Line(mglPoint(a,-0.8),mglPoint(b,-0.8),"k-VA");	gr->Puts(mglPoint(c,-0.8),"Style 'VA'",":rL");
	gr->Line(mglPoint(a,-1),mglPoint(b,-1),"k-AV");		gr->Puts(mglPoint(c,-1),"Style 'AV'",":rL");

	gr->SubPlot(3,2,2);
	//#LENUQ
	gr->FaceZ(mglPoint(-1,	-1), 0.4, 0.3, "L#");	gr->Puts(mglPoint(-0.8,-0.9), "L", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-1), 0.4, 0.3, "E#");	gr->Puts(mglPoint(-0.4,-0.9), "E", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-1), 0.4, 0.3, "N#");	gr->Puts(mglPoint(0,  -0.9), "N", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-1), 0.4, 0.3, "U#");	gr->Puts(mglPoint(0.4,-0.9), "U", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-1), 0.4, 0.3, "Q#");	gr->Puts(mglPoint(0.8,-0.9), "Q", "w:C", -1.4);
	//#lenuq
	gr->FaceZ(mglPoint(-1,	-0.7), 0.4, 0.3, "l#");	gr->Puts(mglPoint(-0.8,-0.6), "l", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-0.7), 0.4, 0.3, "e#");	gr->Puts(mglPoint(-0.4,-0.6), "e", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-0.7), 0.4, 0.3, "n#");	gr->Puts(mglPoint(0,  -0.6), "n", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-0.7), 0.4, 0.3, "u#");	gr->Puts(mglPoint(0.4,-0.6), "u", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-0.7), 0.4, 0.3, "q#");	gr->Puts(mglPoint(0.8,-0.6), "q", "k:C", -1.4);
	//#CMYkP
	gr->FaceZ(mglPoint(-1,	-0.4), 0.4, 0.3, "C#");	gr->Puts(mglPoint(-0.8,-0.3), "C", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-0.4), 0.4, 0.3, "M#");	gr->Puts(mglPoint(-0.4,-0.3), "M", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-0.4), 0.4, 0.3, "Y#");	gr->Puts(mglPoint(0,  -0.3), "Y", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-0.4), 0.4, 0.3, "k#");	gr->Puts(mglPoint(0.4,-0.3), "k", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-0.4), 0.4, 0.3, "P#");	gr->Puts(mglPoint(0.8,-0.3), "P", "w:C", -1.4);
	//#cmywp
	gr->FaceZ(mglPoint(-1,	-0.1), 0.4, 0.3, "c#");	gr->Puts(mglPoint(-0.8, 0), "c", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,-0.1), 0.4, 0.3, "m#");	gr->Puts(mglPoint(-0.4, 0), "m", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,-0.1), 0.4, 0.3, "y#");	gr->Puts(mglPoint(0,   0), "y", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	-0.1), 0.4, 0.3, "w#");	gr->Puts(mglPoint(0.4, 0), "w", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	-0.1), 0.4, 0.3, "p#");	gr->Puts(mglPoint(0.8, 0), "p", "k:C", -1.4);
	//#BGRHW
	gr->FaceZ(mglPoint(-1,	0.2), 0.4, 0.3, "B#");	gr->Puts(mglPoint(-0.8, 0.3), "B", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,0.2), 0.4, 0.3, "G#");	gr->Puts(mglPoint(-0.4, 0.3), "G", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,0.2), 0.4, 0.3, "R#");	gr->Puts(mglPoint(0,   0.3), "R", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	0.2), 0.4, 0.3, "H#");	gr->Puts(mglPoint(0.4, 0.3), "H", "w:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	0.2), 0.4, 0.3, "W#");	gr->Puts(mglPoint(0.8, 0.3), "W", "w:C", -1.4);
	//#bgrhw
	gr->FaceZ(mglPoint(-1,	0.5), 0.4, 0.3, "b#");	gr->Puts(mglPoint(-0.8, 0.6), "b", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,0.5), 0.4, 0.3, "g#");	gr->Puts(mglPoint(-0.4, 0.6), "g", "k:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,0.5), 0.4, 0.3, "r#");	gr->Puts(mglPoint(0,   0.6), "r", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	0.5), 0.4, 0.3, "h#");	gr->Puts(mglPoint(0.4, 0.6), "h", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	0.5), 0.4, 0.3, "w#");	gr->Puts(mglPoint(0.8, 0.6), "w", "k:C", -1.4);
	//#brighted
	gr->FaceZ(mglPoint(-1,	0.8), 0.4, 0.3, "{r1}#");	gr->Puts(mglPoint(-0.8, 0.9), "\\{r1\\}", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.6,0.8), 0.4, 0.3, "{r3}#");	gr->Puts(mglPoint(-0.4, 0.9), "\\{r3\\}", "w:C", -1.4);
	gr->FaceZ(mglPoint(-0.2,0.8), 0.4, 0.3, "{r5}#");	gr->Puts(mglPoint(0,   0.9), "\\{r5\\}", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.2,	0.8), 0.4, 0.3, "{r7}#");	gr->Puts(mglPoint(0.4, 0.9), "\\{r7\\}", "k:C", -1.4);
	gr->FaceZ(mglPoint(0.6,	0.8), 0.4, 0.3, "{r9}#");	gr->Puts(mglPoint(0.8, 0.9), "\\{r9\\}", "k:C", -1.4);
	// HEX
	gr->FaceZ(mglPoint(-1, -1.3), 1, 0.3, "{xff9966}#");	gr->Puts(mglPoint(-0.5,-1.2), "\\{xff9966\\}", "k:C", -1.4);
	gr->FaceZ(mglPoint(0,  -1.3), 1, 0.3, "{x83CAFF}#");	gr->Puts(mglPoint( 0.5,-1.2), "\\{x83CAFF\\}", "k:C", -1.4);

	gr->SubPlot(3,2,3);
	char stl[3]="r1", txt[4]="'1'";
	for(int i=0;i<10;i++)
	{
		txt[1]=stl[1]='0'+i;
		gr->Line(mglPoint(-1,0.2*i-1),mglPoint(1,0.2*i-1),stl);
		gr->Puts(mglPoint(1.05,0.2*i-1),txt,":L");
	}

	gr->SubPlot(3,2,4);	gr->Title("TriPlot sample");	gr->Rotate(50,60);
	double t[] = {0,1,2, 0,1,3, 0,2,3, 1,2,3};
	double xt[] = {-1,1,0,0}, yt[] = {-1,-1,1,0}, zt[] = {-1,-1,-1,1};
	mglData tt(4,3,t), uu(4,xt), vv(4,yt), ww(4,zt);
	gr->TriPlot(tt,uu,vv,ww,"b");
	gr->TriPlot(tt,uu,vv,ww,"k#");

	gr->SubPlot(3,2,5);
	mglData r(4);	r.Fill(1,4);
	gr->SetRanges(1,4,1,4);	gr->Axis();
	gr->Mark(r,r,"s");
	gr->Plot(r,"b");
}
//-----------------------------------------------------------------------------
const char *mmgl_minmax="define $p 30\n"
"new h 300 300 '-sqrt(1-x^2-y^2)*(3*x*y^2*$p-x^3*$p+6*y)/(3*sqrt(2))+x*y+(y^2+x^2)*$p/3 -7*(y^2+x^2)^2*$p/24+y^2+3*x^2'\n"
"\nminmax e h\n\ncrange h:dens h:box\nfplot 'sin(2*pi*t)' 'cos(2*pi*t)' '0' 'k'\nplot e(0)*2-1 e(1)*2-1 '. c'";
void smgl_minmax(mglGraph *gr)	// test minmax
{
	mglData h(300,300);
	gr->Fill(h,"-sqrt(1-x^2-y^2)*(3*x*y^2*30-x^3*30+6*y)/(3*sqrt(2))+x*y+(y^2+x^2)*10 -7*(y^2+x^2)^2*30/24+y^2+3*x^2");
	mglData e=h.MinMax();
	gr->SetRange('c',h);	gr->Dens(h);	gr->Box();
	gr->FPlot("sin(2*pi*t)","cos(2*pi*t)","0","k");
	e*=2;	e-=1;
//	for(long i=0;i<x.nx;i++)	{	x.a[i]=e.a[2*i]*2-1;	y.a[i]=e.a[2*i+1]*2-1;	}
	gr->Plot(e(0),e(1),". c");
}
//-----------------------------------------------------------------------------
const char *mmgl_conts="new a 10 10 'sin(2*pi*x*y)'\nrotate 40 60\n"
"dens a '#'\ncont [0,0] a 'r'\nconts r 0 a\nplot 2*r(0)-1 2*r(1)-1 1+r(2) '2c'";
void smgl_conts(mglGraph *gr)	// test conts
{
	mglData a(10,10);	gr->Fill(a,"sin(2*pi*x*y)");
	mglData v, r=a.Conts(0);
	gr->Rotate(40,60);	gr->Dens(a,"#");	gr->Cont(v,a,"r");
	mglData x(r.ny),y(r.ny),z(r.ny);
	for(long i=0;i<x.nx;i++)	{	x[i]=r[r.nx*i]*2-1;	y[i]=r[r.nx*i+1]*2-1;	z[i]=1;	}
	gr->Plot(x,y,z,"2c");
}
//-----------------------------------------------------------------------------
const char *mmgl_fexport=all_prims_str
"write 'fexport.jpg':#write 'fexport.png'\nwrite 'fexport.bmp':write 'fexport.tga'\n"
"write 'fexport.eps':write 'fexport.svg'\nwrite 'fexport.gif':write 'fexport.xyz'\n"
"write 'fexport.stl':write 'fexport.off'\nwrite 'fexport.tex':write 'fexport.obj'\n"
"write 'fexport.prc':write 'fexport.json'\nwrite 'fexport.mgld'";
void smgl_fexport(mglGraph *gr)	// test file export
{
	all_prims(gr);
	gr->WriteJPEG("fexport.jpg");
//	gr->WritePNG("fexport.png");
	gr->WriteBMP("fexport.bmp");
	gr->WriteTGA("fexport.tga");
	gr->WriteEPS("fexport.eps");
	gr->WriteSVG("fexport.svg");
	gr->WriteGIF("fexport.gif");

	gr->WriteXYZ("fexport.xyz");
	gr->WriteSTL("fexport.stl");
	gr->WriteOFF("fexport.off");
	gr->WriteTEX("fexport.tex");
	gr->WriteOBJ("fexport.obj");
	gr->WritePRC("fexport.prc");
	gr->WriteJSON("fexport.json");

	gr->ExportMGLD("fexport.mgld");
	gr->Clf();
	gr->ImportMGLD("fexport.mgld");
}
//-----------------------------------------------------------------------------
const char *mmgl_quality0="quality 0\n"
all_prims_str;
void smgl_quality0(mglGraph *gr)	// test file export
{
	gr->SetQuality(0);	all_prims(gr);
}
//-----------------------------------------------------------------------------
const char *mmgl_quality1="quality 1\n"
all_prims_str;
void smgl_quality1(mglGraph *gr)	// test file export
{
	gr->SetQuality(1);	all_prims(gr);	
}
//-----------------------------------------------------------------------------
const char *mmgl_quality2="quality 2\n"
all_prims_str;
void smgl_quality2(mglGraph *gr)	// test file export
{
	gr->SetQuality(2);	all_prims(gr);	
}
//-----------------------------------------------------------------------------
const char *mmgl_quality4="quality 4\n"
all_prims_str;
void smgl_quality4(mglGraph *gr)	// test file export
{
	gr->SetQuality(4);	all_prims(gr);	
}
//-----------------------------------------------------------------------------
const char *mmgl_quality5="quality 5\n"
all_prims_str;
void smgl_quality5(mglGraph *gr)	// test file export
{
	gr->SetQuality(5);	all_prims(gr);	
}
//-----------------------------------------------------------------------------
const char *mmgl_quality6="quality 6\n"
all_prims_str;
void smgl_quality6(mglGraph *gr)	// test file export
{
	gr->SetQuality(6);	all_prims(gr);	
}
//-----------------------------------------------------------------------------
const char *mmgl_quality8="quality 8\n"
all_prims_str;
void smgl_quality8(mglGraph *gr)	// test file export
{
	gr->SetQuality(8);	all_prims(gr);	
}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
mglSample samp[] = {
	{"3wave", smgl_3wave, mmgl_3wave, "Example of complex @ref{ode} on basis of 3-wave decay."},
	{"alpha", smgl_alpha, mmgl_alpha, "Example of @ref{light} and @ref{alpha} (transparency)."},
	{"apde", smgl_apde, mmgl_apde,  "Comparison of advanced PDE solver (@ref{apde}) and ordinary one (@ref{pde})."},
	{"area", smgl_area, mmgl_area, "Function @ref{area} fill the area between curve and axis plane. It support gradient filling if 2 colors per curve is specified."}, 
	{"aspect", smgl_aspect, mmgl_aspect, "Example of @ref{subplot}, @ref{inplot}, @ref{rotate}, @ref{aspect}, @ref{shear}."},
	{"axial", smgl_axial, mmgl_axial, "Function @ref{axial} draw surfaces of rotation for contour lines. You can draw wire surfaces (@samp{#} style) or ones rotated in other directions (@samp{x}, @samp{z} styles)."},
	{"axis", smgl_axis, mmgl_axis, "Different forms of @ref{axis} position."},
	{"barh", smgl_barh, mmgl_barh, "Function @ref{barh} is the similar to @ref{bars} but draw horizontal bars."},
	{"bars", smgl_bars, mmgl_bars, "Function @ref{bars} draw vertical bars. It have a lot of options: bar-above-bar (@samp{a} style), fall like (@samp{f} style), 2 colors for positive and negative values, wired bars (@samp{#} style), 3D variant."},
	{"belt", smgl_belt, mmgl_belt, "Function @ref{belt} draw surface by belts. You can use @samp{x} style for drawing lines in other direction."},
	{"bifurcation", smgl_bifurcation, mmgl_bifurcation, "Function @ref{bifurcation} draw Bifurcation diagram for multiple stationary points of the map (like logistic map)."},
	{"box", smgl_box, mmgl_box, "Different styles of bounding @ref{box}."},
	{"boxplot", smgl_boxplot, mmgl_boxplot, "Function @ref{boxplot} draw box-and-whisker diagram."},
	{"boxs", smgl_boxs, mmgl_boxs, "Function @ref{boxs} draw surface by boxes. You can use @samp{#} for drawing wire plot."},
	{"candle", smgl_candle, mmgl_candle, "Function @ref{candle} draw candlestick chart. This is a combination of a line-chart and a bar-chart, in that each bar represents the range of price movement over a given time interval."},
	{"chart", smgl_chart, mmgl_chart, "Function @ref{chart} draw colored boxes with width proportional to data values. Use @samp{ } for empty box. It produce well known pie chart if drawn in polar coordinates."},
	{"cloud", smgl_cloud, mmgl_cloud , "Function @ref{cloud} draw cloud-like object which is less transparent for higher data values. Similar plot can be created using many (about 10...20 -- @code{surf3a a a;value 10}) isosurfaces @ref{surf3a}."},
	{"colorbar", smgl_colorbar, mmgl_colorbar, "Example of @ref{colorbar} position and styles."},
	{"combined", smgl_combined, mmgl_combined , "Example of several plots in the same axis."},
	{"cones", smgl_cones, mmgl_cones, "Function @ref{cones} is similar to @ref{bars} but draw cones."},
	{"cont", smgl_cont, mmgl_cont, "Function @ref{cont} draw contour lines for surface. You can select automatic (default) or manual levels for contours, print contour labels, draw it on the surface (default) or at plane (as @code{Dens})."},
	{"cont3", smgl_cont3, mmgl_cont3, "Function @ref{contf3} draw ordinary contour lines but at slices of 3D data. "},
	{"cont_xyz", smgl_cont_xyz, mmgl_cont_xyz, "Functions @ref{contz}, @ref{conty}, @ref{contx} draw contour lines on plane perpendicular to corresponding axis. One of possible application is drawing projections of 3D field."},
	{"contd", smgl_contd, mmgl_contd, "Function @ref{contd} is similar to @ref{contf} but with manual contour colors."},
	{"contf", smgl_contf, mmgl_contf, "Function @ref{contf} draw filled contours.  You can select automatic (default) or manual levels for contours."},
	{"contf3", smgl_contf3, mmgl_contf3, "Function @ref{contf3} draw ordinary filled contours but at slices of 3D data. "},
	{"contf_xyz", smgl_contf_xyz, mmgl_contf_xyz, "Functions @ref{contfz}, @ref{contfy}, @ref{contfx}, draw filled contours on plane perpendicular to corresponding axis. One of possible application is drawing projections of 3D field."},
	{"conts", smgl_conts, mmgl_conts, "Function @ref{conts} get contour coordinate as data array."},
	{"contv", smgl_contv, mmgl_contv, "Function @ref{contv} draw vertical cylinders (belts) at contour lines."},
	{"correl", smgl_correl, mmgl_correl, "Test of correlation function (@ref{correl})."},
//	{"crust", smgl_crust, mmgl_crust, ""},	// TODO: open after triangulation
	{"curvcoor", smgl_curvcoor, mmgl_curvcoor, "Some common curvilinear coordinates."},
	{"cut", smgl_cut, mmgl_cut, "Example of point cutting (@ref{cut}."},
	{"dat_diff", smgl_dat_diff, mmgl_dat_diff, "Example of @ref{diff} and @ref{integrate}."},
	{"dat_extra", smgl_dat_extra, mmgl_dat_extra , "Example of @ref{envelop}, @ref{sew}, @ref{smooth} and @ref{resize}."},
	{"data1", smgl_data1, mmgl_data1, ""},
	{"data2", smgl_data2, mmgl_data2, ""},
	{"dens", smgl_dens, mmgl_dens, "Function @ref{dens} draw density plot (also known as color-map) for surface."},
	{"dens3", smgl_dens3, mmgl_dens3, "Function @ref{dens3} draw ordinary density plots but at slices of 3D data."},
	{"dens_xyz", smgl_dens_xyz, mmgl_dens_xyz, "Functions @ref{densz}, @ref{densy}, @ref{densx} draw density plot on plane perpendicular to corresponding axis. One of possible application is drawing projections of 3D field."},
	{"detect", smgl_detect, mmgl_detect, "Example of curve @ref{detect}."},
	{"dew", smgl_dew, mmgl_dew, "Function @ref{dew} is similar to @ref{vect} but use drops instead of arrows."},
	{"diffract", smgl_diffract, mmgl_diffract, ""},
	{"dilate", smgl_dilate, mmgl_dilate, "Example of @ref{dilate} and @ref{erode}."},
	{"dots", smgl_dots, mmgl_dots, "Function @ref{dots} is another way to draw irregular points. @code{Dots} use color scheme for coloring (see @ref{Color scheme})."},
	{"earth", smgl_earth, mmgl_earth, "Example of Earth map by using @ref{import}."},
	{"error", smgl_error, mmgl_error, "Function @ref{error} draw error boxes around the points. You can draw default boxes or semi-transparent symbol (like marker, see @ref{Line styles}). Also you can set individual color for each box. See also @ref{error2 sample}."},
	{"error2", smgl_error2, mmgl_error2, "Example of @ref{error} kinds."},
	{"export", smgl_export, mmgl_export, "Example of data @ref{export} and @ref{import}."},
	{"fall", smgl_fall, mmgl_fall, "Function @ref{fall} draw waterfall surface. You can use @ref{meshnum} for changing number of lines to be drawn. Also you can use @samp{x} style for drawing lines in other direction."},
	{"fexport", smgl_fexport, mmgl_fexport, "Example of @ref{write} to different file formats."},
	{"fit", smgl_fit, mmgl_fit, "Example of nonlinear @ref{fit}."},
	{"flame2d", smgl_flame2d, mmgl_flame2d, "Function @ref{flame2d} generate points for flame fractals in 2d case."},
	{"flow", smgl_flow, mmgl_flow, "Function @ref{flow} is another standard way to visualize vector fields -- it draw lines (threads) which is tangent to local vector field direction. MathGL draw threads from edges of bounding box and from central slices. Sometimes it is not most appropriate variant -- you may want to use @code{flowp} to specify manual position of threads. The color scheme is used for coloring (see @ref{Color scheme}). At this warm color corresponds to normal flow (like attractor), cold one corresponds to inverse flow (like source)."}, // TODO @ref{flowp}
	{"flow3", smgl_flow3, mmgl_flow3, "Function @ref{flow3} draw flow threads, which start from given plane."},
	{"fog", smgl_fog, mmgl_fog, "Example of @ref{fog}."},
	{"fonts", smgl_fonts, mmgl_fonts, "Example of @ref{font} typefaces."},
	{"grad", smgl_grad, mmgl_grad, "Function @ref{grad} draw gradient lines for matrix."},
	{"hist", smgl_hist, mmgl_hist, "Example of @ref{hist} (histogram)."},
	{"ifs2d", smgl_ifs2d, mmgl_ifs2d, "Function @ref{ifs2d} generate points for fractals using iterated function system in 2d case."},
	{"ifs3d", smgl_ifs3d, mmgl_ifs3d, "Function @ref{ifs3d} generate points for fractals using iterated function system in 3d case."},
	{"indirect",smgl_indirect,mmgl_indirect, "Comparison of @ref{subdata} vs @ref{evaluate}/"},
	{"inplot", smgl_inplot, mmgl_inplot, "Example of @ref{inplot}, @ref{multiplot}, @ref{columnplot}, @ref{gridplot}, @ref{shearplot}, @ref{stickplot}."},
	{"iris", smgl_iris, mmgl_iris, "Function @ref{iris} draw Iris plot for columns of data array."},
	{"label", smgl_label, mmgl_label, "Function @ref{label} print text at data points. The string may contain @samp{%x}, @samp{%y}, @samp{%z} for x-, y-, z-coordinates of points, @samp{%n} for point index."},
	{"lamerey", smgl_lamerey, mmgl_lamerey, "Function @ref{lamerey} draw Lamerey diagram."},
	{"legend", smgl_legend, mmgl_legend , "Example of @ref{legend} styles."},
	{"light", smgl_light, mmgl_light, "Example of @ref{light} with different types."},
	{"loglog", smgl_loglog, mmgl_loglog, "Example of log- and log-log- axis labels."},
	{"map", smgl_map, mmgl_map, "Example of @ref{map}."},
	{"mark", smgl_mark, mmgl_mark, "Example of @ref{mark}."},
	{"mask", smgl_mask, mmgl_mask, "Example of @ref{mask} kinds."},
	{"mesh", smgl_mesh, mmgl_mesh, "Function @ref{mesh} draw wired surface. You can use @ref{meshnum} for changing number of lines to be drawn."},
	{"minmax", smgl_minmax, mmgl_minmax, "Function @ref{minmax} get position of local minimums and maximums."},
	{"mirror", smgl_mirror, mmgl_mirror , "Example of using options."},
	{"molecule", smgl_molecule, mmgl_molecule , "Example of drawing molecules."},
	{"ode", smgl_ode, mmgl_ode, "Example of phase plain created by @ref{ode} solving, contour lines (@ref{cont}) and @ref{flow} threads."},
	{"ohlc", smgl_ohlc, mmgl_ohlc, "Function @ref{ohlc} draw Open-High-Low-Close diagram. This diagram show vertical line for between maximal(high) and minimal(low) values, as well as horizontal lines before/after vertical line for initial(open)/final(close) values of some process."},
	{"param1", smgl_param1, mmgl_param1, "Example of parametric plots for 1D data."},
	{"param2", smgl_param2, mmgl_param2, "Example of parametric plots for 2D data."},
	{"param3", smgl_param3, mmgl_param3, "Example of parametric plots for 3D data."},
	{"paramv", smgl_paramv, mmgl_paramv, "Example of parametric plots for vector fields."},
	{"parser", smgl_parser, mmgl_parser, "Basic MGL script."},
	{"pde", smgl_pde, mmgl_pde, "Example of @ref{pde} solver."},
	{"pendelta", smgl_pendelta, mmgl_pendelta, "Example of @ref{pendelta} for lines and glyphs smoothing."},
	{"pipe", smgl_pipe, mmgl_pipe, "Function @ref{pipe} is similar to @ref{flow} but draw pipes (tubes) which radius is proportional to the amplitude of vector field. The color scheme is used for coloring (see @ref{Color scheme}). At this warm color corresponds to normal flow (like attractor), cold one corresponds to inverse flow (like source)."},
	{"plot", smgl_plot, mmgl_plot, "Function @ref{plot} is most standard way to visualize 1D data array. By default, @code{Plot} use colors from palette. However, you can specify manual color/palette, and even set to use new color for each points by using @samp{!} style. Another feature is @samp{ } style which draw only markers without line between points."},
	{"pmap", smgl_pmap, mmgl_pmap, "Function @ref{pmap} draw Poincare map -- show intersections of the curve and the surface."},
	{"primitives", smgl_primitives, mmgl_primitives , "Example of primitives: @ref{line}, @ref{curve}, @ref{rhomb}, @ref{ellipse}, @ref{face}, @ref{sphere}, @ref{drop}, @ref{cone}."},
	{"projection", smgl_projection, mmgl_projection , "Example of plot projection (@ref{ternary}=4)."},
	{"projection5", smgl_projection5, mmgl_projection5 , "Example of plot projection in ternary coordinates (@ref{ternary}=5)."},
	{"pulse", smgl_pulse, mmgl_pulse , "Example of @ref{pulse} parameter determining."},
	{"qo2d", smgl_qo2d, mmgl_qo2d, "Example of PDE solving by quasioptical approach @ref{qo2d}."},
	{"quality0", smgl_quality0, mmgl_quality0, "Show all kind of primitives in @ref{quality}=0."},
	{"quality1", smgl_quality1, mmgl_quality1, "Show all kind of primitives in @ref{quality}=1."},
	{"quality2", smgl_quality2, mmgl_quality2, "Show all kind of primitives in @ref{quality}=2."},
	{"quality4", smgl_quality4, mmgl_quality4, "Show all kind of primitives in @ref{quality}=4."},
	{"quality5", smgl_quality5, mmgl_quality5, "Show all kind of primitives in @ref{quality}=5."},
	{"quality6", smgl_quality6, mmgl_quality6, "Show all kind of primitives in @ref{quality}=6."},
	{"quality8", smgl_quality8, mmgl_quality8, "Show all kind of primitives in @ref{quality}=8."},
	{"radar", smgl_radar, mmgl_radar, "The @ref{radar} plot is variant of @ref{plot}, which make plot in polar coordinates and draw radial rays in point directions. If you just need a plot in polar coordinates then I recommend to use @ref{Curvilinear coordinates} or @ref{plot} in parametric form with @code{x=r*cos(fi); y=r*sin(fi);}."},
	{"refill", smgl_refill, mmgl_refill, "Example of @ref{refill} and @ref{gspline}."},
	{"region", smgl_region, mmgl_region, "Function @ref{region} fill the area between 2 curves. It support gradient filling if 2 colors per curve is specified. Also it can fill only the region y1<y<y2 if style @samp{i} is used."},
	{"scanfile", smgl_scanfile, mmgl_scanfile , "Example of @ref{scanfile} for reading 'named' data."},
	{"schemes", smgl_schemes, mmgl_schemes , "Example of popular color schemes."},
	{"section", smgl_section, mmgl_section, "Example of @ref{section} to separate data and @ref{join} it back."},
	{"several_light", smgl_several_light, mmgl_several_light , "Example of using several @ref{light} sources."},
	{"solve", smgl_solve, mmgl_solve, "Example of @ref{solve} for root finding."},
	{"stem", smgl_stem, mmgl_stem, "Function @ref{stem} draw vertical bars. It is most attractive if markers are drawn too."},
	{"step", smgl_step, mmgl_step, "Function @ref{step} plot data as stairs. At this stairs can be centered if sizes are differ by 1."},
	{"stereo", smgl_stereo, mmgl_stereo, "Example of stereo image of @ref{surf}."},
	{"stfa", smgl_stfa, mmgl_stfa, "Example of @ref{stfa}."},
	{"style", smgl_style, mmgl_style , "Example of colors and styles for plots."},
	{"surf", smgl_surf, mmgl_surf, "Function @ref{surf} is most standard way to visualize 2D data array. @code{Surf} use color scheme for coloring (see @ref{Color scheme}). You can use @samp{#} style for drawing black meshes on the surface."},
	{"surf3", smgl_surf3, mmgl_surf3, "Function @ref{surf3} is one of most suitable (for my opinion) functions to visualize 3D data. It draw the isosurface(s) -- surface(s) of constant amplitude (3D analogue of contour lines). You can draw wired isosurfaces if specify @samp{#} style."},
	{"surf3a", smgl_surf3a, mmgl_surf3a, "Function @ref{surf3c} is similar to @ref{surf3} but its transparency is determined by another data."},
	{"surf3c", smgl_surf3c, mmgl_surf3c, "Function @ref{surf3c} is similar to @ref{surf3} but its coloring is determined by another data."},
	{"surf3ca", smgl_surf3ca, mmgl_surf3ca, "Function @ref{surf3c} is similar to @ref{surf3} but its coloring and transparency is determined by another data arrays."},
	{"surfa", smgl_surfa, mmgl_surfa, "Function @ref{surfa} is similar to @ref{surf} but its transparency is determined by another data."},
	{"surfc", smgl_surfc, mmgl_surfc, "Function @ref{surfc} is similar to @ref{surf} but its coloring is determined by another data."},
	{"surfca", smgl_surfca, mmgl_surfca, "Function @ref{surfca} is similar to @ref{surf} but its coloring and transparency is determined by another data arrays."},
	{"table", smgl_table, mmgl_table, "Function @ref{table} draw table with data values."},
	{"tape", smgl_tape, mmgl_tape, "Function @ref{tape} draw tapes which rotate around the curve as transverse orts of accompanied coordinates."},
	{"tens", smgl_tens, mmgl_tens, "Function @ref{tens} is variant of @ref{plot} with smooth coloring along the curves. At this, color is determined as for surfaces (see @ref{Color scheme})."},
	{"ternary", smgl_ternary, mmgl_ternary , "Example of @ref{ternary} coordinates."},
	{"text", smgl_text, mmgl_text, "Example of @ref{text} possibilities."},
	{"text2", smgl_text2, mmgl_text2, "Example of @ref{text} along curve."},
	{"textmark", smgl_textmark, mmgl_textmark, "Function @ref{textmark} is similar to @ref{mark} but draw text instead of markers."},
	{"ticks", smgl_ticks, mmgl_ticks, "Example of @ref{axis} ticks."},
	{"tile", smgl_tile, mmgl_tile, "Function @ref{tile} draw surface by tiles."},
	{"tiles", smgl_tiles, mmgl_tiles, "Function @ref{tiles} is similar to @ref{tile} but tile sizes is determined by another data. This allows one to simulate transparency of the plot."},
	{"torus", smgl_torus, mmgl_torus , "Function @ref{torus} draw surface of the curve rotation."},
	{"traj", smgl_traj, mmgl_traj, "Function @ref{traj} is 1D analogue of @ref{vect}. It draw vectors from specified points."},
	{"triangulation",smgl_triangulation, mmgl_triangulation , "Example of use @ref{triangulate} for arbitrary placed points."},
	{"triplot", smgl_triplot, mmgl_triplot, "Functions @ref{triplot} and @ref{quadplot} draw set of triangles (or quadrangles, correspondingly) for irregular data arrays. Note, that you have to provide not only vertexes, but also the indexes of triangles or quadrangles. I.e. perform triangulation by some other library. See also @ref{triangulate}."},
	{"tube", smgl_tube, mmgl_tube, "Function @ref{tube} draw tube with variable radius."},
	{"type0", smgl_type0, mmgl_type0, "Example of ordinary transparency (@ref{transptype}=0)."},
	{"type1", smgl_type1, mmgl_type1, "Example of glass-like transparency (@ref{transptype}=1)."},
	{"type2", smgl_type2, mmgl_type2, "Example of lamp-like transparency (@ref{transptype}=2)."},
	{"vect", smgl_vect, mmgl_vect, "Function @ref{vect} is most standard way to visualize vector fields -- it draw a lot of arrows or hachures for each data cell. It have a lot of options which can be seen on the figure (and in the sample code), and use color scheme for coloring (see @ref{Color scheme})."},
	{"vect3", smgl_vect3, mmgl_vect3, "Function @ref{vect3} draw ordinary vector field plot but at slices of 3D data."},
	{"venn", smgl_venn, mmgl_venn, "Example of venn-like diagram."},
{"", NULL, NULL, NULL}};
//-----------------------------------------------------------------------------
