/***************************************************************************
 * exec_2d.cpp is part of Math Graphic Library
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
#ifdef WIN32
#include <io.h>
#include <direct.h>
#else
#include <unistd.h>
#endif

#include "mgl2/base.h"
#include "mgl2/parser.h"
wchar_t *mgl_str_copy(const char *s);
//-----------------------------------------------------------------------------
int static mgls_arc(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))
		gr->Arc(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].v);
	else if(!strcmp(k,"nnnnns"))
		gr->Arc(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].v, a[5].s.s);
	else if(!strcmp(k,"nnnnnn"))
		gr->Arc(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v), a[5].v);
	else if(!strcmp(k,"nnnnnns"))
		gr->Arc(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v), a[5].v, a[6].s.s);
	else if(!strcmp(k,"nnnnnnnnnn"))
		gr->Arc(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v, a[5].v), mglPoint(a[6].v,a[7].v, a[8].v), a[9].v);
	else if(!strcmp(k,"nnnnnnnnnns"))
		gr->Arc(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v, a[5].v), mglPoint(a[6].v,a[7].v, a[8].v), a[9].v, a[10].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_axis(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]==0)	gr->Axis("xyz","",opt);
	else if(!strcmp(k,"s"))		gr->Axis(a[0].s.s, "",opt);
	else if(!strcmp(k,"ss"))	gr->Axis(a[0].s.s, a[1].s.s,opt);
	else if(!strcmp(k,"sss"))	gr->SetFunc(a[0].s.s,a[1].s.s,a[2].s.s,"");
	else if(!strcmp(k,"ssss"))	gr->SetFunc(a[0].s.s,a[1].s.s,a[2].s.s,a[3].s.s);
	else if(!strcmp(k,"n"))		gr->SetCoor(mgl_int(a[0].v));
	else if(!strcmp(k,"nnnn"))	gr->SetRanges(a[0].v,a[2].v, a[1].v,a[3].v);
	else if(!strcmp(k,"nnnnnn"))gr->SetRanges(a[0].v,a[3].v, a[1].v,a[4].v, a[2].v,a[5].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_background(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->LoadBackground(a[0].s.s);
	else if(!strcmp(k,"sn"))	gr->LoadBackground(a[0].s.s,a[1].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ball(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nn"))	gr->Mark(mglPoint(a[0].v,a[1].v,NAN),"r.");
	else if(!strcmp(k,"nns"))	gr->Mark(mglPoint(a[0].v,a[1].v,NAN),a[2].s.s);
	else if(!strcmp(k,"nnn"))	gr->Mark(mglPoint(a[0].v,a[1].v,a[2].v),"r.");
	else if(!strcmp(k,"nnns"))	gr->Mark(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_box(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(k[0]==0)	gr->Box();
	else if(!strcmp(k,"s"))		gr->Box(a[0].s.s);
	else if(!strcmp(k,"sn"))	gr->Box(a[0].s.s, a[1].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_circle(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnn"))	gr->Circle(mglPoint(a[0].v,a[1].v, NAN), a[2].v);
	else if(!strcmp(k,"nnns"))	gr->Circle(mglPoint(a[0].v,a[1].v, NAN), a[2].v, a[3].s.s);
	else if(!strcmp(k,"nnnn"))	gr->Circle(mglPoint(a[0].v,a[1].v,a[2].v), a[3].v);
	else if(!strcmp(k,"nnnns"))	gr->Circle(mglPoint(a[0].v,a[1].v,a[2].v), a[3].v, a[4].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_colorbar(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(k[0]==0)	gr->Colorbar();
	else if(!strcmp(k,"s"))		gr->Colorbar(a[0].s.s);
	else if(!strcmp(k,"d"))		gr->Colorbar(*(a[0].d));
	else if(!strcmp(k,"ds"))	gr->Colorbar(*(a[0].d), a[1].s.s);
	else if(!strcmp(k,"snn"))	gr->Colorbar(a[0].s.s, a[1].v, a[2].v);
	else if(!strcmp(k,"snnn"))	gr->Colorbar(a[0].s.s, a[1].v, a[2].v, a[3].v,1);
	else if(!strcmp(k,"snnnn"))	gr->Colorbar(a[0].s.s, a[1].v, a[2].v, a[3].v,a[4].v);
	else if(!strcmp(k,"dsnn"))	gr->Colorbar(*(a[0].d), a[1].s.s, a[2].v, a[3].v);
	else if(!strcmp(k,"dsnnn"))	gr->Colorbar(*(a[0].d), a[1].s.s, a[2].v, a[3].v, a[4].v,1);
	else if(!strcmp(k,"dsnnnn"))
		gr->Colorbar(*(a[0].d), a[1].s.s, a[2].v, a[3].v, a[4].v,a[5].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cone(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnnnn"))	gr->Cone(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v);
	else if(!strcmp(k,"nnnnnnns"))	gr->Cone(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v,-1, a[7].s.s);
	else if(!strcmp(k,"nnnnnnnn"))	gr->Cone(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v, a[7].v);
	else if(!strcmp(k,"nnnnnnnns"))	gr->Cone(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v, a[7].v, a[8].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_curve(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnnnnnn"))
		gr->Curve(mglPoint(a[0].v,a[1].v,NAN),
			mglPoint(a[2].v,a[3].v),
			mglPoint(a[4].v,a[5].v,NAN),
			mglPoint(a[6].v,a[7].v));
	else if(!strcmp(k,"nnnnnnnns"))
		gr->Curve(mglPoint(a[0].v,a[1].v,NAN),
			mglPoint(a[2].v,a[3].v),
			mglPoint(a[4].v,a[5].v,NAN),
			mglPoint(a[6].v,a[7].v), a[8].s.s);
	else if(!strcmp(k,"nnnnnnnnnnnn"))
		gr->Curve(mglPoint(a[0].v,a[1].v,a[2].v),
			mglPoint(a[3].v,a[4].v,a[5].v),
			mglPoint(a[6].v,a[7].v,a[8].v),
			mglPoint(a[9].v,a[10].v,a[11].v));
	else if(!strcmp(k,"nnnnnnnnnnnns"))
		gr->Curve(mglPoint(a[0].v,a[1].v,a[2].v),
			mglPoint(a[3].v,a[4].v,a[5].v),
			mglPoint(a[6].v,a[7].v,a[8].v),
			mglPoint(a[9].v,a[10].v,a[11].v), a[12].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_drop(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))
		gr->Drop(mglPoint(a[0].v,a[1].v), mglPoint(a[2].v,a[3].v), a[4].v);
	else if(!strcmp(k,"nnnnns"))
		gr->Drop(mglPoint(a[0].v,a[1].v), mglPoint(a[2].v,a[3].v), a[4].v, a[5].s.s);
	else if(!strcmp(k,"nnnnnsn"))
		gr->Drop(mglPoint(a[0].v,a[1].v), mglPoint(a[2].v,a[3].v), a[4].v, a[5].s.s, a[6].v);
	else if(!strcmp(k,"nnnnnsnn"))
		gr->Drop(mglPoint(a[0].v,a[1].v), mglPoint(a[2].v,a[3].v), a[4].v, a[5].s.s, a[6].v, a[7].v);
	else if(!strcmp(k,"nnnnnnn"))
		gr->Drop(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v);
	else if(!strcmp(k,"nnnnnnns"))
		gr->Drop(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v, a[7].s.s);
	else if(!strcmp(k,"nnnnnnnsn"))
		gr->Drop(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v, a[7].s.s, a[8].v);
	else if(!strcmp(k,"nnnnnnnsnn"))
		gr->Drop(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v, a[7].s.s, a[8].v, a[9].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ellipse(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))
		gr->Ellipse(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].v);
	else if(!strcmp(k,"nnnnns"))
		gr->Ellipse(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].v, a[5].s.s);
	else if(!strcmp(k,"nnnnnnn"))
		gr->Ellipse(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v);
	else if(!strcmp(k,"nnnnnnns"))
		gr->Ellipse(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v, a[7].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_errbox(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnn"))
		gr->Error(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN));
	else if(!strcmp(k,"nnnns"))
		gr->Error(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].s.s);
	else if(!strcmp(k,"nnnnnn"))
		gr->Error(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v));
	else if(!strcmp(k,"nnnnnns"))
		gr->Error(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_face(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnnnnn"))
		gr->Face(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN),
				mglPoint(a[4].v,a[5].v,NAN), mglPoint(a[6].v,a[7].v,NAN));
	else if(!strcmp(k,"nnnnnnnns"))
		gr->Face(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN),
				mglPoint(a[4].v,a[5].v,NAN), mglPoint(a[6].v,a[7].v,NAN), a[8].s.s);
	else if(!strcmp(k,"nnnnnnnnnnnn"))
		gr->Face(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v),
				mglPoint(a[6].v,a[7].v,a[8].v), mglPoint(a[9].v,a[10].v,a[11].v));
	else if(!strcmp(k,"nnnnnnnnnnnns"))
		gr->Face(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v),
				mglPoint(a[6].v,a[7].v,a[8].v), mglPoint(a[9].v,a[10].v,a[11].v), a[12].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_facex(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))	gr->FaceX(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v);
	else if(!strcmp(k,"nnnnns"))	gr->FaceX(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v, a[5].s.s);
	else if(!strcmp(k,"nnnnnsnn"))	gr->FaceX(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v, a[5].s.s,a[6].v,a[7].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_facey(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))	gr->FaceY(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v);
	else if(!strcmp(k,"nnnnns"))	gr->FaceY(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v, a[5].s.s);
	else if(!strcmp(k,"nnnnnsnn"))	gr->FaceY(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v, a[5].s.s,a[6].v,a[7].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_facez(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))	gr->FaceZ(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v);
	else if(!strcmp(k,"nnnnns"))	gr->FaceZ(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v, a[5].s.s);
	else if(!strcmp(k,"nnnnnsnn"))	gr->FaceZ(mglPoint(a[0].v, a[1].v, a[2].v), a[3].v, a[4].v, a[5].s.s,a[6].v,a[7].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fgets(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	char *buf;	buf = new char[4096];
	FILE *fp;
	if(!strncmp(k,"nns",3))
	{
		int i, n = (k[3]=='n'?mgl_int(a[3].v):0);
		fp = fopen(a[2].s.s,"rt");
		if(!fp)
		{
			gr->SetWarn(mglWarnOpen,a[2].s.s);
			delete []buf;	return res;
}
		for(i=0;i<n;i++)	if(!fgets(buf,4096,fp))	continue;
		memset(buf,0,4096);
		if(!fgets(buf,4096,fp))
		{
			gr->SetWarn(mglWarnOpen,(std::string(a[2].s.s)+" - line "+mgl_str_num(n)).c_str());
			fclose(fp);	delete []buf;	return res;
		}
		fclose(fp);
		gr->Puts(mglPoint(a[0].v,a[1].v,NAN),buf, (k[4]=='s')?a[4].s.s:"", k[5]=='n'?a[5].v:-1);
	}
	else if(!strncmp(k,"nnns",4))
	{
		int i, n = (k[4]=='n'?mgl_int(a[4].v):0);
		fp = fopen(a[3].s.s,"rt");
		if(!fp)
		{
			gr->SetWarn(mglWarnOpen,a[3].s.s);
			delete []buf;	return res;
		}
		for(i=0;i<n;i++)	if(!fgets(buf,4096,fp))	continue;
		memset(buf,0,4096);
		if(!fgets(buf,4096,fp))
		{
			gr->SetWarn(mglWarnOpen,(std::string(a[3].s.s)+" - line "+mgl_str_num(n)).c_str());
			fclose(fp);	delete []buf;	return res;
		}
		fclose(fp);
		gr->Puts(mglPoint(a[0].v,a[1].v,a[2].v),buf, (k[5]=='s')?a[5].s.s:"", k[6]=='n'?a[6].v:-1);
	}
	else res = 1;
	gr->Self()->LoadState();	delete []buf;	return res;
}
//-----------------------------------------------------------------------------
int static mgls_flame2d(mglGraph *, long, mglArg *a, const char *k, const char *)
{
	mglData *fx = dynamic_cast<mglData*>(a[0].d);
	mglData *fy = dynamic_cast<mglData*>(a[1].d);
	if(!fx)	return 1;
	int res = 0;
	if (!strcmp(k, "dddn"))	fx->Set(mglFlame2d(*(a[1].d), *(a[2].d), mgl_int(a[3].v)));
	else if (!strcmp(k, "ddddn") && fy)
	{
		mglData f(mglFlame2d(*(a[2].d), *(a[3].d), mgl_int(a[4].v)));
		fx->Set(f.SubData(0));	fy->Set(f.SubData(1));
	}
	else if (!strcmp(k, "dddnn"))	fx->Set(mglFlame2d(*(a[1].d), *(a[2].d), mgl_int(a[3].v), mgl_int(a[4].v)));
	else if (!strcmp(k, "ddddnn") && fy)
	{
		mglData f(mglFlame2d(*(a[2].d), *(a[3].d), mgl_int(a[4].v), mgl_int(a[5].v)));
		fx->Set(f.SubData(0));	fy->Set(f.SubData(1));
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fplot(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->FPlot(a[0].s.s, "",opt);
	else if(!strcmp(k,"ss"))	gr->FPlot(a[0].s.s, a[1].s.s,opt);
	else if(!strcmp(k,"sss"))	gr->FPlot(a[0].s.s, a[1].s.s, a[2].s.s, "",opt);
	else if(!strcmp(k,"ssss"))	gr->FPlot(a[0].s.s, a[1].s.s, a[2].s.s, a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fsurf(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->FSurf(a[0].s.s, "",opt);
	else if(!strcmp(k,"ss"))	gr->FSurf(a[0].s.s, a[1].s.s,opt);
	else if(!strcmp(k,"sss"))	gr->FSurf(a[0].s.s, a[1].s.s, a[2].s.s, "",opt);
	else if(!strcmp(k,"ssss"))	gr->FSurf(a[0].s.s, a[1].s.s, a[2].s.s, a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_grid(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]==0)	gr->Grid("xyzt", "B",opt);
	else if(!strcmp(k,"s"))	gr->Grid(a[0].s.s, "B",opt);
	else if(!strcmp(k,"ss"))gr->Grid(a[0].s.s, a[1].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ifs2d(mglGraph *, long, mglArg *a, const char *k, const char *)
{
	mglData *fx = dynamic_cast<mglData*>(a[0].d);
	mglData *fy = dynamic_cast<mglData*>(a[1].d);
	if(!fx)	return 1;
	int res = 0;
	if (!strcmp(k, "ddn"))	fx->Set(mglIFS2d(*(a[1].d), mgl_int(a[2].v)));
	else if (!strcmp(k, "dddn") && fy)
	{
		mglData f(mglIFS2d(*(a[2].d), mgl_int(a[3].v)));
		fx->Set(f.SubData(0));	fy->Set(f.SubData(1));
	}
	else if (!strcmp(k, "ddnn"))	fx->Set(mglIFS2d(*(a[1].d), mgl_int(a[2].v), mgl_int(a[3].v)));
	else if (!strcmp(k, "dddnn") && fy)
	{
		mglData f(mglIFS2d(*(a[2].d), mgl_int(a[3].v), mgl_int(a[4].v)));
		fx->Set(f.SubData(0));	fy->Set(f.SubData(1));
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ifs3d(mglGraph *, long, mglArg *a, const char *k, const char *)
{
	mglData *f = dynamic_cast<mglData*>(a[0].d);
	if(!f)	return 1;
	int res = 0;
	if (!strcmp(k, "ddn"))	f->Set(mglIFS3d(*(a[1].d), mgl_int(a[2].v)));
	else if (!strcmp(k, "ddnn"))	f->Set(mglIFS3d(*(a[1].d), mgl_int(a[2].v), mgl_int(a[3].v)));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ifsfile(mglGraph *, long, mglArg *a, const char *k, const char *)
{
	mglData *f = dynamic_cast<mglData*>(a[0].d);
	if(!f)	return 1;
	int res = 0;
	if (!strcmp(k, "dssn"))	f->Set(mglIFSfile(a[1].s.s, a[2].s.s, mgl_int(a[3].v)));
	else if (!strcmp(k, "dssnn"))	f->Set(mglIFSfile(a[1].s.s, a[2].s.s, mgl_int(a[3].v), mgl_int(a[4].v)));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_legend(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]==0)	gr->Legend(3, "#", opt);
	else if(!strcmp(k,"n"))	gr->Legend(mgl_int(a[0].v), "#", opt);
	else if(!strcmp(k,"ns"))	gr->Legend(mgl_int(a[0].v), a[1].s.s, opt);
	else if(!strcmp(k,"nn"))	gr->Legend(a[0].v, a[1].v, "#", opt);
	else if(!strcmp(k,"nns"))	gr->Legend(a[0].v, a[1].v, a[2].s.s, opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_line(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnn"))
		gr->Line(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN));
	else if(!strcmp(k,"nnnns"))
		gr->Line(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].s.s);
	else if(!strcmp(k,"nnnnnn"))
		gr->Line(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v));
	else if(!strcmp(k,"nnnnnns"))
		gr->Line(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_logo(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->Logo(a[0].s.s,false,opt);
	else if(!strcmp(k,"sn"))	gr->Logo(a[0].s.s,mgl_int(a[1].v),opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_polygon(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))
		gr->Polygon(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), mgl_int(a[4].v));
	else if(!strcmp(k,"nnnnns"))
		gr->Polygon(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), mgl_int(a[4].v), a[5].s.s);
	else if(!strcmp(k,"nnnnnnn"))
		gr->Polygon(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), mgl_int(a[6].v));
	else if(!strcmp(k,"nnnnnnns"))
		gr->Polygon(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), mgl_int(a[6].v), a[7].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_rect(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnn"))
		gr->Face(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[0].v,a[3].v,NAN),
				 mglPoint(a[2].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN));
	else if(!strcmp(k,"nnnns"))
		gr->Face(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[0].v,a[3].v,NAN),
					mglPoint(a[2].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN),a[4].s.s);
	else if(!strncmp(k,"nnnnnn",6))
	{
		if(a[0].v==a[3].v)	gr->Face(mglPoint(a[0].v,a[1].v,a[2].v),
			mglPoint(a[0].v,a[4].v,a[2].v),
			mglPoint(a[3].v,a[1].v,a[5].v),
			mglPoint(a[3].v,a[4].v,a[5].v),
			k[6]=='s' ? a[6].s.s : 0);
		else	gr->Face(mglPoint(a[0].v,a[1].v,a[2].v),
			mglPoint(a[0].v,a[4].v,a[5].v),
			mglPoint(a[3].v,a[1].v,a[2].v),
			mglPoint(a[3].v,a[4].v,a[5].v),
			k[6]=='s' ? a[6].s.s : 0);
	}
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_rhomb(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnnnn"))
		gr->Rhomb(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].v);
	else if(!strcmp(k,"nnnnns"))
		gr->Rhomb(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v,NAN), a[4].v, a[5].s.s);
	else if(!strcmp(k,"nnnnnnn"))
		gr->Rhomb(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v);
	else if(!strcmp(k,"nnnnnnns"))
		gr->Rhomb(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].v, a[7].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_sphere(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nnn"))	gr->Sphere(mglPoint(a[0].v,a[1].v), a[2].v);
	else if(!strcmp(k,"nnns"))	gr->Sphere(mglPoint(a[0].v,a[1].v), a[2].v, a[3].s.s);
	else if(!strcmp(k,"nnnn"))	gr->Sphere(mglPoint(a[0].v,a[1].v,a[2].v), a[3].v);
	else if(!strcmp(k,"nnnns"))	gr->Sphere(mglPoint(a[0].v,a[1].v,a[2].v), a[3].v, a[4].s.s);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_symbol(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)		// NOTE don't use options -- Puts can be part of group
{
	int res=0;
	if(k[0]=='n')
	{
		gr->Self()->SaveState(opt);
		if(!strcmp(k,"nns"))	gr->Symbol(mglPoint(a[0].v,a[1].v,NAN),a[2].s[0]);
		else if(!strcmp(k,"nnss"))	gr->Symbol(mglPoint(a[0].v,a[1].v,NAN),a[2].s[0], a[3].s.s);
		else if(!strcmp(k,"nnssn"))	gr->Symbol(mglPoint(a[0].v,a[1].v,NAN),a[2].s[0], a[3].s.s,a[4].v);
		else if(!strcmp(k,"nnns"))	gr->Symbol(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s[0]);
		else if(!strcmp(k,"nnnss"))	gr->Symbol(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s[0], a[4].s.s);
		else if(!strcmp(k,"nnnssn"))gr->Symbol(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s[0], a[4].s.s,a[5].v);
		else if(!strcmp(k,"nnnns"))		gr->Symbol(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v), a[4].s[0]);
		else if(!strcmp(k,"nnnnss"))	gr->Symbol(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v), a[4].s[0], a[5].s.s);
		else if(!strcmp(k,"nnnnssn"))	gr->Symbol(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v), a[4].s[0], a[5].s.s,a[6].v);
		else if(!strcmp(k,"nnnnnns"))	gr->Symbol(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s[0]);
		else if(!strcmp(k,"nnnnnnss"))	gr->Symbol(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s[0], a[7].s.s);
		else if(!strcmp(k,"nnnnnnssn"))	gr->Symbol(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s[0], a[7].s.s,a[8].v);
		else	res=1;
		gr->Self()->LoadState();
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_text(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)		// NOTE don't use options -- Puts can be part of group
{
	int res=0;
	if(k[0]=='n')
	{
		gr->Self()->SaveState(opt);
		if(!strcmp(k,"nns"))	gr->Putsw(mglPoint(a[0].v,a[1].v,NAN),a[2].s.w);
		else if(!strcmp(k,"nnss"))	gr->Putsw(mglPoint(a[0].v,a[1].v,NAN),a[2].s.w, a[3].s.s);
		else if(!strcmp(k,"nnssn"))	gr->Putsw(mglPoint(a[0].v,a[1].v,NAN),a[2].s.w, a[3].s.s,a[4].v);
		else if(!strcmp(k,"nnns"))		gr->Putsw(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s.w);
		else if(!strcmp(k,"nnnss"))		gr->Putsw(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s.w, a[4].s.s);
		else if(!strcmp(k,"nnnssn"))	gr->Putsw(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s.w, a[4].s.s,a[5].v);
		else if(!strcmp(k,"nnnns"))		gr->Putsw(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v), a[4].s.w);
		else if(!strcmp(k,"nnnnss"))	gr->Putsw(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v), a[4].s.w, a[5].s.s);
		else if(!strcmp(k,"nnnnssn"))	gr->Putsw(mglPoint(a[0].v,a[1].v,NAN), mglPoint(a[2].v,a[3].v), a[4].s.w, a[5].s.s,a[6].v);
		else if(!strcmp(k,"nnnnnns"))	gr->Putsw(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s.w);
		else if(!strcmp(k,"nnnnnnss"))	gr->Putsw(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s.w, a[7].s.s);
		else if(!strcmp(k,"nnnnnnssn"))	gr->Putsw(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v), a[6].s.w, a[7].s.s,a[8].v);
		else	res=1;
		gr->Self()->LoadState();
	}
	else if(!strcmp(k,"ds"))	gr->Text(*(a[0].d),a[1].s.w,"",opt);
	else if(!strcmp(k,"dss"))	gr->Text(*(a[0].d),a[1].s.w,a[2].s.s,opt);
	else if(!strcmp(k,"dds"))	gr->Text(*(a[0].d),*(a[1].d),a[2].s.w,"",opt);
	else if(!strcmp(k,"ddss"))	gr->Text(*(a[0].d),*(a[1].d),a[2].s.w,a[3].s.s,opt);
	else if(!strcmp(k,"ddds"))	gr->Text(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.w,"",opt);
	else if(!strcmp(k,"dddss"))	gr->Text(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.w,a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_title(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"s"))	gr->Title(a[0].s.w);
	else if(!strcmp(k,"ss"))	gr->Title(a[0].s.w, a[1].s.s);
	else if(!strcmp(k,"ssn"))	gr->Title(a[0].s.w, a[1].s.s,a[2].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_clabel(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->Label('c', a[0].s.w, 1, opt);
	else if(!strcmp(k,"sn"))	gr->Label('c', a[0].s.w, a[1].v, opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tlabel(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->Label('t', a[0].s.w, 1, opt);
	else if(!strcmp(k,"sn"))	gr->Label('t', a[0].s.w, a[1].v, opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_xlabel(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->Label('x', a[0].s.w, 1, opt);
	else if(!strcmp(k,"sn"))	gr->Label('x', a[0].s.w, a[1].v, opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ylabel(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->Label('y', a[0].s.w, 1, opt);
	else if(!strcmp(k,"sn"))	gr->Label('y', a[0].s.w, a[1].v, opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_zlabel(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->Label('z', a[0].s.w, 1, opt);
	else if(!strcmp(k,"sn"))	gr->Label('z', a[0].s.w, a[1].v, opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
mglCommand mgls_prm_cmd[] = {
	{"arc",_("Draw angle arc"),"arc x0 y0 x1 y1 a ['fmt']|x0 y0 z0 x1 y1 a ['fmt']|x0 y0 z0 xr yr zr x1 y1 z1 a ['fmt']", mgls_arc ,13},
	{"axis",_("Setup or draw axis"),"axis ['dir' 'fmt']|'fx' 'fy' 'fz' ['fc']|how", mgls_axis ,12},
	{"background",_("Load image for background"),"background 'fname' [alpha]", mgls_background ,12},
	{"ball",_("Draw point (ball)"),"ball posx posy ['fmt']|posx posy posz ['fmt']", mgls_ball ,13},
	{"box",_("Draw bounding box"),"box ['fmt' ticks]", mgls_box ,12},
	{"circle",_("Draw circle"),"circle x y r ['fmt']|x y z r ['fmt']", mgls_circle ,13},
	{"clabel",_("Draw label for colorbar"),"clabel 'txt' [pos]", mgls_clabel ,12},
	{"colorbar",_("Draw colorbar"),"colorbar ['fmt']|Vdat ['fmt']|'sch' x y [w h]|Vdat 'sch' x y [w h]", mgls_colorbar ,12},
	{"cone",_("Draw cone"),"cone x1 y1 z1 x2 y2 z2 r1 [r2 'fmt' edge]", mgls_cone ,13},
	{"curve",_("Draw curve"),"curve x1 y1 dx1 dy1 x2 y2 dx2 dy2 ['fmt']|x1 y1 z1 dx1 dy1 dz1 x2 y2 z2 dx2 dy2 dz2 ['fmt']", mgls_curve ,13},
	{"drop",_("Draw drop"),"drop x0 y0 dx dy r ['col' sh asp]|x0 y0 z0 dx dy dz r ['col' sh asp]", mgls_drop ,13},
	{"ellipse",_("Draw ellipse"),"ellipse x1 y1 x2 y2 r ['fmt']|x1 y1 z1 x2 y2 z2 r ['fmt']", mgls_ellipse ,13},
	{"errbox",_("Draw error box"),"errbox x y ex ey ['fmt']|x y z ex ey ez ['fmt']", mgls_errbox ,13},
	{"face",_("Draw face (quadrangle)"),"face x1 y1 x2 y2 x3 y3 x4 y4 ['fmt']|x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4 ['fmt']", mgls_face ,13},
	{"facex",_("Draw face perpendicular to x-axis"),"facex x0 y0 z0 wy wz ['fmt' d1 d2]", mgls_facex ,13},
	{"facey",_("Draw face perpendicular to y-axis"),"facex x0 y0 z0 wx wz ['fmt' d1 d2]", mgls_facey ,13},
	{"facez",_("Draw face perpendicular to z-axis"),"facex x0 y0 z0 wy wz ['fmt' d1 d2]", mgls_facez ,13},
	{"fgets",_("Print string from file"),"fgets x y z 'fname' [pos 'fmt' size]|x y z 'fname' [pos 'fmt' size]", mgls_fgets ,15},
	{"flame2d",_("Computes the flame fractal"), "flame2d F A B n [skip]|Fx Fy A B n [skip]", mgls_flame2d, 4},
	{"fplot",_("Plot curve by formula"),"fplot 'y(x)' ['fmt']|'x(t)' 'y(t)' 'z(t)' ['fmt']", mgls_fplot ,1},
	{"fsurf",_("Plot surface by formula"),"fsurf 'z(x,y)' ['fmt']|'x(u,v)' 'y(u,v)' 'z(u,v)' ['fmt']", mgls_fsurf ,1},
	{"grid",_("Draw grid"),"grid ['dir' 'fmt']", mgls_grid ,12},
	{"ifs2d",_("Computes the attractor of an IFS"), "ifs2d F A n [skip]|Fx Fy A n [skip]", mgls_ifs2d, 4},
	{"ifs3d",_("Computes the attractor of an IFS for 3d case"), "ifs3d F A n [skip]", mgls_ifs3d, 4},
	{"ifsfile",_("Computes the attractor of an IFS with parameters from *.ifs file"), "ifsfile F 'fname' 'name' n [skip]", mgls_ifsfile, 4},
	{"legend",_("Draw legend"),"legend [pos 'fmt']|x y ['fmt']", mgls_legend ,15},
	{"line",_("Draw line"),"line x1 y1 x2 y2 ['fmt']|x1 y1 z1 x2 y2 z2 ['fmt']", mgls_line ,13},
	{"logo",_("Draw bitmap (logo) along axis range"),"logo 'fname' [smooth]", mgls_logo ,13},
	{"polygon",_("Draw polygon"),"polygon x1 y1 x2 y2 num ['fmt']|x1 y1 z1 x2 y2 z2 num ['fmt']", mgls_polygon ,13},
	{"rect",_("Draw rectangle"),"rect x1 y1 x2 y2 ['fmt']|x1 y1 z1 x2 y2 z2 ['fmt']", mgls_rect ,13},
	{"rhomb",_("Draw rhombus"),"rhomb x1 y1 x2 y2 r ['fmt']|x1 y1 z1 x2 y2 z2 r ['fmt']", mgls_rhomb ,13},
	{"sphere",_("Draw sphere"),"sphere x0 y0 r ['fmt']|x0 y0 z0 r ['fmt']", mgls_sphere ,13},
	{"symbol",_("Draw user-defined symbol at given position and direction"),"symbol x y 'id' ['fmt' size]|x y z 'id' ['fmt' size]|x y dx dy 'id' ['fmt' size]|x y z dx dy dz 'id' ['fmt' size]", mgls_symbol ,15},
	{"text",_("Draw text at some position or along curve"),"text x y 'txt' ['fmt' size]|x y z 'txt' ['fmt' size]|x y dx dy 'txt' ['fmt' size]|x y z dx dy dz 'txt' ['fmt' size]|Ydat 'txt' ['font']|Xdat Ydat 'txt' ['font']|Xdat Ydat Zdat 'txt' ['font']", mgls_text ,15},
	{"title",_("Add title for current subplot/inplot"),"title 'txt' ['fmt' size]", mgls_title ,5},
	{"tlabel",_("Draw label for t-axis"),"tlabel 'txt' [pos]", mgls_tlabel ,12},
	{"xlabel",_("Draw label for x-axis"),"xlabel 'txt' [pos]", mgls_xlabel ,12},
	{"ylabel",_("Draw label for y-axis"),"ylabel 'txt' [pos]", mgls_ylabel,12},
	{"zlabel",_("Draw label for z-axis"),"zlabel 'txt' [pos]", mgls_zlabel,12},
{"","","",NULL,0}};
//-----------------------------------------------------------------------------
