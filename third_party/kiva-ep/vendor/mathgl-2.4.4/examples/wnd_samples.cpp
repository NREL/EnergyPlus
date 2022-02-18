/***************************************************************************
 * wnd_sample.cpp is part of Math Graphic Library
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
//-----------------------------------------------------------------------------
#include "mgl2/mgl.h"
//-----------------------------------------------------------------------------
int test_wnd(mglGraph *gr)
{
mgl_set_test_mode(true);
//	gr->SetQuality(6);
	gr->NewFrame();	// 0
	gr->Box();
	gr->EndFrame();
	gr->NewFrame();	// 1
	gr->Axis();
	gr->EndFrame();
	gr->NewFrame();	// 2
	gr->Label('x',"XXXX",0);
	gr->Label('y',"YYYY",0);
	gr->EndFrame();
	gr->NewFrame();	// 3
	gr->Puts(mglPoint(0,0),"0");
	gr->EndFrame();

	gr->Clf();
	gr->ShowFrame(0);
	gr->ShowFrame(1);
	gr->ShowFrame(2);
	gr->ShowFrame(3);
//	gr->Puts(mglPoint(0,1),"1");
	gr->Finish();
	return 0;
}
//-----------------------------------------------------------------------------
int sample(mglGraph *gr)
{
	gr->Rotate(20,40);
	gr->SetRanges(-2,2,-2,2,-1,3);
	gr->FSurf("cos(2*pi*(x^2+y^2))/(x^2+y^2+1)^2/(x^2+y^2<4)");
	gr->Drop(mglPoint(0,0,3),mglPoint(0,0,-1),0.7,"b");
	return 0;
}
//-----------------------------------------------------------------------------
int sample_m(mglGraph *gr)
{
//	gr->Rotate(0,0);	gr->Axis();	gr->Box();	return 0;
//	gr->Alpha(true); gr->Light(true);

	gr->SubPlot(2,2,0);
	gr->Puts(mglPoint(0,1.1),"Methane, CH_4",0,-2); gr->Rotate(60,120);
	gr->Sphere(mglPoint(0,0,0),0.25,"k");
	gr->Drop(mglPoint(0,0,0),mglPoint(0,0,1),0.35,"h",1,2);
	gr->Sphere(mglPoint(0,0,0.7),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(-0.94,0,-0.33),0.35,"h",1,2);
	gr->Sphere(mglPoint(-0.66,0,-0.23),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.47,0.82,-0.33),0.35,"h",1,2);
	gr->Sphere(mglPoint(0.33,0.57,-0.23),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.47,-0.82,-0.33),0.35,"h",1,2);
	gr->Sphere(mglPoint(0.33,-0.57,-0.23),0.25,"g");

	gr->SubPlot(2,2,1);
	gr->Puts(mglPoint(0,1.1),"Water, H{_2}O",0,-2); gr->Rotate(60,100);
	gr->Sphere(mglPoint(0,0,0),0.25,"r");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.3,0.5,0),0.3,"m",1,2);
	gr->Sphere(mglPoint(0.3,0.5,0),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.3,-0.5,0),0.3,"m",1,2);
	gr->Sphere(mglPoint(0.3,-0.5,0),0.25,"g");

	gr->SubPlot(2,2,2);
	gr->Puts(mglPoint(0,1.1),"Oxygen, O_2",0,-2); gr->Rotate(60,120);
	gr->Drop(mglPoint(0,0.5,0),mglPoint(0,-0.3,0),0.3,"m",1,2);
	gr->Sphere(mglPoint(0,0.5,0),0.25,"r");
	gr->Drop(mglPoint(0,-0.5,0),mglPoint(0,0.3,0),0.3,"m",1,2);
	gr->Sphere(mglPoint(0,-0.5,0),0.25,"r");

	gr->SubPlot(2,2,3);
	gr->Puts(mglPoint(0,1.1),"Ammonia, NH_3",0,-2); gr->Rotate(60,120);
	gr->Sphere(mglPoint(0,0,0),0.25,"b");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.33,0.57,0),0.32,"n",1,2);
	gr->Sphere(mglPoint(0.33,0.57,0),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(0.33,-0.57,0),0.32,"n",1,2);
	gr->Sphere(mglPoint(0.33,-0.57,0),0.25,"g");
	gr->Drop(mglPoint(0,0,0),mglPoint(-0.65,0,0),0.32,"n",1,2);
	gr->Sphere(mglPoint(-0.65,0,0),0.25,"g");
 	return 0;
}
//-----------------------------------------------------------------------------
int sample_1(mglGraph *gr)
{
	mglData  a(50,15),d(50),d1(50),d2(50);
	d.Modify("0.7*sin(2*pi*x) + 0.5*cos(3*pi*x) + 0.2*sin(pi*x)");
	d1.Modify("cos(2*pi*x)");
	d2.Modify("sin(2*pi*x)");
	a.Modify("pow(x,4*y)");

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Simple plot of one curve");
	gr->Plot(d);
	gr->EndFrame();

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Three curves with manual styles");
	gr->Plot(d,"b");
	gr->Plot(d1,"ri");
	gr->Plot(d2,"m|^");
	gr->Plot(d,"l o");
	gr->EndFrame();

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Three curves with automatic styles");
	gr->Plot(d);
	gr->Plot(d1);
	gr->Plot(d2);
	gr->EndFrame();

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Curves from matrix");
	gr->Plot(a);
	gr->EndFrame();

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Parametrical curves in 2D");
	gr->Plot(d1,d2,"b");
	gr->Plot(d1,d,"ri");
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Parametrical curves in 3D");
	gr->Rotate(60,40);
	gr->Box();	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"y");	gr->Label('z',"z");
	gr->Plot(d1,d2,d,"b");
	gr->EndFrame();

	gr->NewFrame();
	gr->SubPlot(2,2,0);
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Area plot");
	gr->Area(d);
	gr->SubPlot(2,2,1);
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Step plot");
	gr->Step(d);
	gr->SubPlot(2,2,2);
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Stem plot");
	gr->Stem(d);
	gr->SubPlot(2,2,3);
	gr->Box();	gr->Axis("xy");	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Puts(mglPoint(0,1.2,1),"Bars plot");
	gr->Bars(d);
	gr->EndFrame();

	gr->NewFrame();
	gr->SubPlot(2,2,0);
	gr->Puts(mglPoint(0,1.2,1),"Area plot in 3D");
	gr->Rotate(60,40);
	gr->Box();	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Area(d1,d2,d);
	gr->SubPlot(2,2,1);
	gr->Puts(mglPoint(0,1.2,1),"Step plot in 3D");
	gr->Rotate(60,40);
	gr->Box();	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Step(d1,d2,d);
	gr->SubPlot(2,2,2);
	gr->Puts(mglPoint(0,1.2,1),"Stem plot in 3D");
	gr->Rotate(60,40);
	gr->Box();	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Stem(d1,d2,d);
	gr->SubPlot(2,2,3);
	gr->Puts(mglPoint(0,1.2,1),"Bars plot in 3D");
	gr->Rotate(60,40);
	gr->Box();	gr->Axis();	gr->Label('x',"x");	gr->Label('y',"y");
	gr->Bars(d1,d2,d);
	gr->EndFrame();
	return gr->GetNumFrame();
}
//-----------------------------------------------------------------------------
int sample_2(mglGraph *gr)
{
	mglData  a(50,50),b(50,50),c(50,50),d(50,50),m(50,50),c1(50,50,7),
		d1(50),d2(50);
	d1.Modify("2*x*x-1");	d2.Modify("sin(pi*(x-0.5))");
	a.Modify("0.6*sin(2*pi*x)*sin(3*pi*y) + 0.4*cos(3*pi*(x*y))");
	b.Modify("(2+sin(2*pi*x))*cos(2*pi*y)/3");
	c.Modify("(2+sin(2*pi*x))*sin(2*pi*y)/3");
	d.Modify("cos(2*pi*x)");
	m.Modify("cos(pi*x)");
	c1.Modify("(2-z)*(2*x-1)^2 + (z+1)*(2*y-1)^2");

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Simple surface");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf(a);
//	gr->Colorbar();
	gr->EndFrame();

	gr->NewFrame();
	gr->SubPlot(2,2,0);
	gr->Puts(mglPoint(0,1.2,1),"Gray color scheme 'kw'");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf(a,"kw");
	gr->SubPlot(2,2,1);
	gr->Puts(mglPoint(0,1.2,1),"Hot color scheme 'wyrRk'");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf(a,"wyrRk");
	gr->SubPlot(2,2,2);
	gr->Puts(mglPoint(0,1.2,1),"Along coordiantes 'rgbd'");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf(a,"rgbd");
	gr->SubPlot(2,2,3);
	gr->Puts(mglPoint(0,1.2,1),"Bicolor scheme 'BbwrR'");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf(a,"BbwrR");
	gr->EndFrame();

	gr->NewFrame();
	gr->Box();	gr->Axis();
	gr->Puts(mglPoint(0,1.2,1),"Density plot");
	gr->Dens(a);
	gr->InPlot(0.6,1,0.6,1,false);	// new axis in upper right corner
	gr->Box();	gr->Axis();
	gr->Puts(mglPoint(0,1.2,1),"... with bicolor");
	gr->Dens(a,"BbwrR");
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Mesh lines (previous scheme by default)");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Mesh(a);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Surface of boxes");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Boxs(a);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Contour plot");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Cont(a);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Contour isosurface y-rotation");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Axial(a,":y");
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Contour isosurface x-rotation");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Axial(a,"x");
	a.Transpose();
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Surface and contours");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Cont(a,"BbcyrR_");
	gr->Surf(a);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Parametrical surface (1)");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Mesh(d1,d2,b);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Parametrical surface (vase)");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf(b,c,m);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Parametrical surface (torus)");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf(b,c,d);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Contours for 3-tensor");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Cont(c1);
	gr->EndFrame();
	return gr->GetNumFrame();
}
//-----------------------------------------------------------------------------
int sample_3(mglGraph *gr)
{
	mglData  a(50,50,50),b(50,50,50), c(50,50,50),d(50,50,50), d1(50),d2(50),d3(50);
	d1.Modify("cos(2*pi*x)");
	d2.Modify("sin(2*pi*x)");
	d3.Modify("2*x*x-1");
	a.Modify("(-2*((2*x-1)^2 + (2*y-1)^2 + (2*z-1)^4 - (2*z-1)^2 + 0.1))");
	b.Modify("-2*((2*x-1)^2 + (2*y-1)^2)");
	c.Modify("exp(-8*((2*x-1)^2+(2*y-1)^2)/(1+z*z*4))");
	d.Modify("cos(32*z*((2*x-1)^2+(2*y-1)^2)/(1+z*z*4))");
//	c.Modify("0.5*sin(2*pi*x)*sin(3*pi*y)*sin(3*pi*z) + 0.5*cos(4*pi*(x*y*z-(x*y+y*z+z*x)/2))");

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Isosurface (try lightning!)");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf3(a);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Isosurface with 'rgbd' scheme");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf3(a,"rgbd");
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Cloud plot (switch alpha !!!)");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Cloud(a);
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Density at central slices");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Dens3(a,"x");	gr->Dens3(a);	gr->Dens3(a,"z");
	gr->EndFrame();

	gr->NewFrame();
	gr->Puts(mglPoint(0,1.2,1),"Contours at central slices");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Cont3(a,"x");	gr->Cont3(a);	gr->Cont3(a,"z");
	gr->EndFrame();

	gr->NewFrame();
	gr->SubPlot(2,1,0);
	gr->Puts(mglPoint(0,1.2,1),"Gauss difraction");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->SetRange('c',0,1);
	gr->Surf3(0.5,c,"g");
	gr->SubPlot(2,1,1);
	gr->Puts(mglPoint(0,1.2,1),"and its phase");
	gr->Rotate(40,60);
	gr->Box();	gr->Axis();
	gr->Surf3A(sin(M_PI/4),d,c,"q");
	gr->Surf3A(-sin(M_PI/4),d,c,"q");
	gr->EndFrame();

	return gr->GetNumFrame();
}
//-----------------------------------------------------------------------------
int sample_d(mglGraph *gr)
{
	mglData  a(50,50),b(50,50);
	mglData cx(50,50,50),cy(50,50,50),cz(50,50,50);
	a.Modify("0.6*sin(2*pi*x)*sin(3*pi*y) + 0.4*cos(3*pi*(x*y))");
	b.Modify("0.6*cos(2*pi*x)*cos(3*pi*y) + 0.4*cos(3*pi*(x*y))");
	cx.Modify("0.01*(x-0.3)/pow((x-0.3)^2+(y-0.5)^2+(z-0.5)^2,1.5) - 0.01*(x-0.7)/pow((x-0.7)^2+(y-0.5)^2+(z-0.5)^2,1.5)");
	cy.Modify("0.01*(y-0.5)/pow((x-0.3)^2+(y-0.5)^2+(z-0.5)^2,1.5) - 0.01*(y-0.5)/pow((x-0.7)^2+(y-0.5)^2+(z-0.5)^2,1.5)");
	cz.Modify("0.01*(z-0.5)/pow((x-0.3)^2+(y-0.5)^2+(z-0.5)^2,1.5) - 0.01*(z-0.5)/pow((x-0.7)^2+(y-0.5)^2+(z-0.5)^2,1.5)");

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");
	gr->Puts(mglPoint(0,1.2,1),"Vector field (color ~ \\sqrt{a^2})",":rC",8);
	gr->Vect(a,b,"","value 50");
	gr->EndFrame();

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");
	gr->Puts(mglPoint(0,1.2,1),"Vector field (length ~ \\sqrt{a^2})",":rC",8);
	gr->Vect(a,b);
	gr->EndFrame();

	gr->NewFrame();
	gr->Box();	gr->Axis("xy");
	gr->Puts(mglPoint(0,1.2,1),"Flow chart (blue - source)",":rC",8);
	gr->Flow(a,b);
	gr->EndFrame();

	return gr->GetNumFrame();
}
//-----------------------------------------------------------------------------
void mgls_prepare1d(mglData *y, mglData *y1, mglData *y2, mglData *x1, mglData *x2)
{
	long n=50;
	if(y)	y->Create(n,3);
	if(x1)	x1->Create(n);
	if(x2)	x2->Create(n);
	if(y1)	y1->Create(n);
	if(y2)	y2->Create(n);
	for(long i=0;i<n;i++)
	{
		double xx = i/(n-1.);
		if(y)
		{
			y->a[i] = 0.7*sin(2*M_PI*xx) + 0.5*cos(3*M_PI*xx) + 0.2*sin(M_PI*xx);
			y->a[i+n] = sin(2*M_PI*xx);
			y->a[i+2*n] = cos(2*M_PI*xx);
		}
		if(y1)	y1->a[i] = 0.5+0.3*cos(2*M_PI*xx);
		if(y2)	y2->a[i] = 0.3*sin(2*M_PI*xx);
		if(x1)	x1->a[i] = xx*2-1;
		if(x2)	x2->a[i] = 0.05+0.03*cos(2*M_PI*xx);
	}
}
//-----------------------------------------------------------------------------
void mgls_prepare2d(mglData *a, mglData *b, mglData *v)
{
	long n=50,m=40;
	if(a)	a->Create(n,m);
	if(b)	b->Create(n,m);
	if(v)	{	v->Create(9);	v->Fill(-1,1);	}
	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
	{
		double x = i/(n-1.), y = j/(m-1.);
		long i0 = i+n*j;
		if(a)	a->a[i0] = 0.6*sin(2*M_PI*x)*sin(3*M_PI*y)+0.4*cos(3*M_PI*x*y);
		if(b)	b->a[i0] = 0.6*cos(2*M_PI*x)*cos(3*M_PI*y)+0.4*cos(3*M_PI*x*y);
	}
}
//-----------------------------------------------------------------------------
void mgls_prepare3d(mglData *a, mglData *b)
{
	long n=61,m=50,l=40;
	if(a)	a->Create(n,m,l);
	if(b)	b->Create(n,m,l);
	for(long k=0;k<l;k++)	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
	{
		double x=2*i/(n-1.)-1, y=2*j/(m-1.)-1, z=2*k/(l-1.)-1;
		long i0 = i+n*(j+m*k);
		if(a)	a->a[i0] = -2*(x*x + y*y + z*z*z*z - z*z - 0.1);
		if(b)	b->a[i0] = 1-2*tanh((x+y)*(x+y));
	}
}
//-----------------------------------------------------------------------------
void mgls_prepare2v(mglData *a, mglData *b)
{
	long n=20,m=30;
	if(a)	a->Create(n,m);
	if(b)	b->Create(n,m);
	for(long j=0;j<m;j++)	for(long i=0;i<n;i++)
	{
		double x=i/(n-1.), y=j/(m-1.);
		long i0 = i+n*j;
		if(a)	a->a[i0] = 0.6*sin(2*M_PI*x)*sin(3*M_PI*y)+0.4*cos(3*M_PI*x*y);
		if(b)	b->a[i0] = 0.6*cos(2*M_PI*x)*cos(3*M_PI*y)+0.4*cos(3*M_PI*x*y);
	}
}
//-----------------------------------------------------------------------------
void mgls_prepare3v(mglData *ex, mglData *ey, mglData *ez)
{
	long n=10;
	double z0=0.3;
	if(!ex || !ey || !ez)	return;
	ex->Create(n,n,n);	ey->Create(n,n,n);	ez->Create(n,n,n);
	for(long k=0;k<n;k++)	for(long j=0;j<n;j++)	for(long i=0;i<n;i++)
	{
		double x=2*i/(n-1.)-1, y=2*j/(n-1.)-1, z=2*k/(n-1.)-1;
		long i0 = i+n*(j+k*n);
		double r1 = pow(x*x+y*y+(z-z0)*(z-z0)+0.03,1.5);
		double r2 = pow(x*x+y*y+(z+z0)*(z+z0)+0.03,1.5);
		ex->a[i0]=0.2*x/r1 - 0.2*x/r2;
		ey->a[i0]=0.2*y/r1 - 0.2*y/r2;
		ez->a[i0]=0.2*(z-z0)/r1 - 0.2*(z+z0)/r2;
	}
}
//-----------------------------------------------------------------------------
