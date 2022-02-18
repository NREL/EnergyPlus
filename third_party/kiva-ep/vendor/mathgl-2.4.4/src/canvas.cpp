/***************************************************************************
 * canvas.cpp is part of Math Graphic Library
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
#include <limits.h>
#include "mgl2/font.h"
#include "mgl2/canvas.h"
//-----------------------------------------------------------------------------
MGL_EXPORT std::string mglGlobalMess;	///< Buffer for receiving global messages
//-----------------------------------------------------------------------------
mglCanvas::mglCanvas(int w, int h) : mglBase()
{
	clr(MGL_DISABLE_SCALE);
	set(MGL_VECT_FRAME);	// NOTE: require a lot of memory!
	Z=0;	C=G=G4=GB=0;	OI=0;	gif=0;
	CurFrameId=0;	Delay=0.5;
	Width=Height=Depth=0;	ObjId=-1;
	fscl=ftet=0;		PlotId = _("frame");
	pnt_col = 0;

	ac.ch='c';
	ax.dir.Set(1,0,0);	ax.a.Set(0,1,0);	ax.b.Set(0,0,1);	ax.ch='x';
	ay.dir.Set(0,1,0);	ay.a.Set(1,0,0);	ay.b.Set(0,0,1);	ay.ch='y';
	az.dir.Set(0,0,1);	az.a.Set(0,1,0);	az.b.Set(1,0,0);	az.ch='z';

	SetSize(w,h);	SetQuality(MGL_DRAW_NORM);	DefaultPlotParam();
}
//-----------------------------------------------------------------------------
mglCanvas::~mglCanvas()
{
	if(G)	{	delete []G;	delete []C;	delete []Z;	delete []G4;delete []GB;delete []OI;	}
	if(pnt_col)	delete []pnt_col;
}
//-----------------------------------------------------------------------------
long mglCanvas::PushDrwDat()
{
	mglDrawDat d;
	d.Pnt=Pnt;	d.Prm=Prm;	d.Sub=Sub;	d.Glf=Glf;	d.Ptx=Ptx;	d.Txt=Txt;
#pragma omp critical(drw)
	MGL_PUSH(DrwDat,d,mutexDrw);
	return DrwDat.size();
}
//-----------------------------------------------------------------------------
void mglCanvas::ResetFrames()	{	CurFrameId=0;	DrwDat.clear();	}
//-----------------------------------------------------------------------------
void mglCanvas::SetFrame(long i)
{
	if(get(MGL_VECT_FRAME) && i>=0 && i<long(DrwDat.size()))
	{
		Finish();	CurFrameId--;
		mglDrawDat d;
		d.Pnt=Pnt;	d.Prm=Prm;	d.Sub=Sub;	d.Glf=Glf;	d.Ptx=Ptx;	d.Txt=Txt;
#if MGL_HAVE_PTHREAD
		pthread_mutex_lock(&mutexDrw);
		DrwDat[i] = d;
		pthread_mutex_unlock(&mutexDrw);
#else
#pragma omp critical(drw)
		DrwDat[i] = d;
#endif
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::GetFrame(long k)
{
	if(k<0 || (size_t)k>=DrwDat.size())	return;
	ClearFrame();
	const mglDrawDat &d=DrwDat[k];
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexPnt);
	pthread_mutex_lock(&mutexPrm);
	pthread_mutex_lock(&mutexSub);
	pthread_mutex_lock(&mutexGlf);
	pthread_mutex_lock(&mutexPtx);
	pthread_mutex_lock(&mutexTxt);
#endif
#pragma omp critical
	{	Pnt=d.Pnt;	Prm=d.Prm;	Sub=d.Sub;	Glf=d.Glf;	Ptx=d.Ptx;	Txt=d.Txt;	ClearPrmInd();	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexTxt);
	pthread_mutex_unlock(&mutexPtx);
	pthread_mutex_unlock(&mutexGlf);
	pthread_mutex_unlock(&mutexSub);
	pthread_mutex_unlock(&mutexPrm);
	pthread_mutex_unlock(&mutexPnt);
#endif
}
//-----------------------------------------------------------------------------
void mglCanvas::ClearFrame()
{
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexPnt);
	pthread_mutex_lock(&mutexPrm);
	pthread_mutex_lock(&mutexGlf);
	pthread_mutex_lock(&mutexPtx);
	pthread_mutex_lock(&mutexTxt);
	pthread_mutex_lock(&mutexSub);
	pthread_mutex_lock(&mutexLeg);
	pthread_mutex_lock(&mutexGrp);
	pthread_mutex_lock(&mutexAct);
#endif

#pragma omp critical
	{
		StartAutoGroup(NULL);
		Leg.clear();	Grp.clear();	Act.clear();	Glf.clear();
		Pnt.clear();	Prm.clear();	Ptx.clear();	ClearPrmInd();
		Txt.clear();	Txt.reserve(3);
//		mglBlock inpl = Sub[0];	Sub.clear();	Sub.push_back(inpl);	// NOTE at least one inplot should present!!!
		mglTexture t1(MGL_DEF_PAL,-1), t2(MGL_DEF_SCH,1);
		Txt.push_back(t1);	Txt.push_back(t2);	// No extra lock is required
	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexAct);
	pthread_mutex_unlock(&mutexGrp);
	pthread_mutex_unlock(&mutexLeg);
	pthread_mutex_unlock(&mutexSub);
	pthread_mutex_unlock(&mutexTxt);
	pthread_mutex_unlock(&mutexPtx);
	pthread_mutex_unlock(&mutexGlf);
	pthread_mutex_unlock(&mutexPrm);
	pthread_mutex_unlock(&mutexPnt);
#endif
	ClfZB(true);
}
//-----------------------------------------------------------------------------
void mglCanvas::ShowFrame(long k)
{
	if(k<0 || (size_t)k>=DrwDat.size())	return;
	ClfZB();
	size_t npnt=Pnt.size(), nglf=Glf.size(), nptx=Ptx.size(), ntxt=Txt.size(), nsub=Sub.size();
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexPnt);
	pthread_mutex_lock(&mutexPrm);
	pthread_mutex_lock(&mutexSub);
	pthread_mutex_lock(&mutexGlf);
	pthread_mutex_lock(&mutexPtx);
	pthread_mutex_lock(&mutexTxt);
#endif
#pragma omp critical
	{
		const mglDrawDat &d=DrwDat[k];
		Glf.resize(d.Glf.size());	for(size_t i=0;i<d.Glf.size();i++)	Glf.push_back(d.Glf[i]);
		Ptx.resize(d.Ptx.size());	for(size_t i=0;i<d.Ptx.size();i++)	Ptx.push_back(d.Ptx[i]);
		Sub.resize(d.Sub.size());	for(size_t i=0;i<d.Sub.size();i++)	Sub.push_back(d.Sub[i]);
		Txt.reserve(d.Pnt.size());	for(size_t i=0;i<d.Txt.size();i++)	Txt.push_back(d.Txt[i]);
		Pnt.reserve(d.Pnt.size());	ClearPrmInd();
		for(size_t i=0;i<d.Pnt.size();i++)
		{
			mglPnt p = d.Pnt[i]; 	p.c += ntxt;
			if(p.sub>=0)	p.sub += nsub;
			else	p.sub -= nsub;
			Pnt.push_back(p);
		}
		Prm.reserve(d.Prm.size());
		for(size_t i=0;i<d.Prm.size();i++)
		{
			mglPrim p = d.Prm[i];
			p.n1 += npnt;

			switch(p.type)
			{
			case 1:	p.n2 += npnt;	break;
			case 2:	p.n2 += npnt;	p.n3 += npnt;	break;
			case 3:	p.n2 += npnt;	p.n3 += npnt;	p.n4 += npnt;	break;
			case 4: p.n4 += nglf;	break;
			case 5:	p.n2 += npnt;	break;
			case 6: p.n3 += nptx;	break;
			}
			Prm.push_back(p);
		}
	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexPnt);
	pthread_mutex_unlock(&mutexPrm);
	pthread_mutex_unlock(&mutexSub);
	pthread_mutex_unlock(&mutexGlf);
	pthread_mutex_unlock(&mutexPtx);
	pthread_mutex_unlock(&mutexTxt);
#endif
}
//-----------------------------------------------------------------------------
const unsigned char *mglCanvas::GetBits()	{	Finish();	return G;	}
//-----------------------------------------------------------------------------
mreal mglCanvas::GetRatio() const	{	return inW/inH;	}
//-----------------------------------------------------------------------------
void mglCanvas::add_prim(mglPrim &a)
{
	if(a.n1>=0)
	{
		a.z = Pnt[a.n1].z;	// this is a bit less accurate but simpler for transformation
		a.id = ObjId;
#pragma omp critical(prm)
		MGL_PUSH(Prm,a,mutexPrm);
		ClearPrmInd();	clr(MGL_FINISHED);
	}
}
//-----------------------------------------------------------------------------
extern uint64_t mgl_mask_def[16];
void mglCanvas::DefaultPlotParam()
{
/* NOTE: following variables and mutex will not be changed by DefaultPlotParam()
long InUse;			///< Smart pointer (number of users)
mglFont *fnt;		///< Class for printing vector text
int Quality;		///< Quality of plot (0x0-pure, 0x1-fast; 0x2-fine; 0x4 - low memory)
int Width;			///< Width of the image
int Height;			///< Height of the image
int Depth;			///< Depth of the image
int CurFrameId;		///< Number of automaticle created frames
GifFileType *gif;*/
	SetDrawReg(1,1,0);		Perspective(0);	SetPenDelta(1);	SetBBox();
	memcpy(mgl_mask_val, mgl_mask_def, 16*sizeof(uint64_t));	// should be > 16*8
	ax.Clear();	ay.Clear();	az.Clear();	ac.Clear();
	mgl_clear_fft();		DefMaskAn=0;	ResetMask();
	SetTickRotate(true);	SetTickSkip(true);
	SetWarn(mglWarnNone,"");	mglGlobalMess = "";
	ObjId = -1;	HighId = INT_MIN;
	SetFunc(0,0);	CutOff(0);	Ternary(0);
	Stop=false;	event_cb = NULL;	event_par=NULL;
	SetRanges(mglPoint(-1,-1,-1,-1), mglPoint(1,1,1,1));
	SetOrigin(NAN,NAN,NAN,NAN);
	SetBarWidth(0.7);	SetMarkSize(1);	SetArrowSize(1);
	SetAlphaDef(0.5);		FontDef[0]=0;
	SetTranspType(0);		SetMeshNum(0);	// NOTE: default MeshNum=0
	SetRotatedText(true);	CurrPal = 0;
	SetLegendMarks();		SetFontSize(4);
	SetTuneTicks(3);		SetAmbient();	SetDiffuse();
	clr(MGL_DISABLE_SCALE);
	clr(MGL_USE_GMTIME);	clr(MGL_NOSUBTICKS);
	SetDifLight(false);		SetReduceAcc(false);
	SetDefScheme(MGL_DEF_SCH);	SetPalette(MGL_DEF_PAL);
	SetPenPal("k-1");		Alpha(false);
	stack.clear();	Restore();	DefColor('k');
	SetPlotFactor(0);	Sub.clear();
	InPlot(0,1,0,1,false);	clr(MGL_FULL_CURV);
	SetTickLen(0);	SetCut(true);
	AdjustTicks("xyzc",true);	Clf('w');

	for(int i=0;i<10;i++)	{	AddLight(i, mglPoint(0,0,1));	Light(i,false);	}
	Light(0,true);	Light(false);	SetDifLight(true);
}
//-----------------------------------------------------------------------------
//	Optimal axis position
//-----------------------------------------------------------------------------
mreal mglCanvas::FindOptOrg(char dir, int ind) const
{
	static mglPoint px, py, pz;
	static mglMatrix bb;
	mglPoint nn[8]={mglPoint(0,0,0), mglPoint(0,0,1), mglPoint(0,1,0,0), mglPoint(0,1,1),
					mglPoint(1,0,0), mglPoint(1,0,1), mglPoint(1,1,0), mglPoint(1,1,1)}, pp[8];
	memcpy(pp, nn, 8*sizeof(mglPoint));
	// do nothing if transformation matrix is the same
	if(B!=bb)
	{
		bb = B;
		for(long i=0;i<8;i++)	PostScale(&B,pp[i]);
		// find point with minimal y
		long j=0;
		for(long i=1;i<8;i++)	if(pp[i].y<pp[j].y)	j=i;
		pp[0]=pp[j];
		// first select 3 closest points
		pp[1].x=1-nn[j].x;	pp[1].y=nn[j].y;	pp[1].z=nn[j].z;	PostScale(&B,pp[1]);	pp[1]-=pp[0];
		pp[2].x=nn[j].x;	pp[2].y=1-nn[j].y;	pp[2].z=nn[j].z;	PostScale(&B,pp[2]);	pp[2]-=pp[0];
		pp[3].x=nn[j].x;	pp[3].y=nn[j].y;	pp[3].z=1-nn[j].z;	PostScale(&B,pp[3]);	pp[3]-=pp[0];
		// find cosine of axis projection
		mreal tx=fabs(pp[1].x/pp[1].y), ty=fabs(pp[2].x/pp[2].y), tz=fabs(pp[3].x/pp[3].y);
		px=py=pz=nn[j];
		if(tz==0 && (ty==0 || tx==0))	// (x- & z-) or (y- & z-) axis are vertical
		{	if(pp[1].x>pp[2].x)	pz.y=1-pz.y;	else	pz.x=1-pz.x;	}
		else if(tx==0 && ty==0)	// x- && y-axis is vertical
		{
			py.x=1-py.x;
			if(pp[1].x>pp[3].x)
			{	px.z=1-px.z;	py.z=1-py.z;	}
		}
		else if(tz<tx && tz<ty)	// z-axis is vertical
		{	if(pp[1].x>pp[2].x)	pz.y=1-pz.y;	else	pz.x=1-pz.x;	}
		else if(ty<tx && ty<tz)	// y-axis is vertical
		{	if(pp[1].x>pp[3].x)	py.z=1-py.z;	else	py.x=1-py.x;	}
		else if(tx<ty && tx<tz)	// x-axis is vertical
		{	if(pp[3].x>pp[2].x)	px.y=1-px.y;	else	px.z=1-px.z;	}
	}
	// return to normal variables
	mglPoint rx = Min+(Max-Min)/px;
	mglPoint ry = Min+(Max-Min)/py;
	mglPoint rz = Min+(Max-Min)/pz;
	mreal res = rx.val(ind);
	if(dir=='y')	res = ry.val(ind);
	if(dir=='z')	res = rz.val(ind);
	return res;
}
//-----------------------------------------------------------------------------
mreal mglCanvas::GetOrgX(char dir, bool inv) const
{
	mreal res = Org.x;
	if(mgl_isnan(res))
	{
		if(strchr("xyz",dir))	res = FindOptOrg(dir,0);
		else if(dir=='t')		res = Min.x;
		else res = B.b[6]>0 ? Max.x:Min.x;
		if(inv)	res = Min.x+Max.x-res;
	}
	return res;
}
//-----------------------------------------------------------------------------
mreal mglCanvas::GetOrgY(char dir, bool inv) const
{
	mreal res = Org.y;
	if(mgl_isnan(res))
	{
		if(strchr("xyz",dir))	res = FindOptOrg(dir,1);
		else if(dir=='t')	res = Min.y;
		else res = B.b[7]>0 ? Max.y:Min.y;
		if(inv)	res = Min.y+Max.y-res;
	}
	return res;
}
//-----------------------------------------------------------------------------
mreal mglCanvas::GetOrgZ(char dir, bool inv) const
{
	mreal res = Org.z;
	if(mgl_isnan(res))
	{
		if(strchr("xyz",dir))	res = FindOptOrg(dir,2);
		else if(dir=='t')	res = Min.z;
		else res = B.b[8]>0 ? Max.z:Min.z;
		if(inv)	res = Min.z+Max.z-res;
	}
	return res;
}
//-----------------------------------------------------------------------------
//	Put primitives
//-----------------------------------------------------------------------------
#define MGL_MARK_PLOT	if(Quality&MGL_DRAW_LMEM)	\
						{	mglDrawReg d;	d.set(this,dr_x,dr_y,dr_p);	d.PenWidth=pw;	\
							d.PDef = PDef;	d.pPos = pPos;	mark_draw(Pnt[p],type,size,&d);	}\
						else{	mglPrim a;	a.w = pw;	a.s = size;	\
							a.n1 = p;	a.n4 = type;	a.angl=0;	add_prim(a);	}
void mglCanvas::mark_plot(long p, char type, mreal size)
{
	if(p<0 || mgl_isnan(Pnt[p].x) || mgl_isnan(size))	return;
	if(type>128 || type<0)
	{	smbl_plot(p,type-128,20*MarkSize*(size?fabs(size):1));	return;	}
	long pp=p;
	mreal pw = 0.15/sqrt(font_factor);
	size = size?fabs(size):1;
	size *= MarkSize*0.35*font_factor;
	if(type=='.')	size = fabs(PenWidth)*sqrt(font_factor/400);
	if(TernAxis&12) for(int i=0;i<4;i++)
	{	p = ProjScale(i, pp);	if(p>=0)	{MGL_MARK_PLOT}	}
	else	{	MGL_MARK_PLOT	}
}
//-----------------------------------------------------------------------------
#define MGL_LINE_PLOT	if(Quality&MGL_DRAW_LMEM)	\
						{	mglDrawReg d;	d.set(this,dr_x,dr_y,dr_p);	d.PenWidth=pw;	\
							d.PDef = PDef;	d.pPos = pPos;	line_draw(Pnt[p1],Pnt[p2],&d);	}\
						else	{	mglPrim a(1);	a.n3=PDef;	a.s = pPos;	\
							a.n1 = p1;	a.n2 = p2;	a.w = pw;	a.angl=0;	add_prim(a);	}
void mglCanvas::line_plot(long p1, long p2)
{
	if(PDef==0)	return;
	if(SamePnt(p1,p2))	return;
	if(p1>p2)	{	long kk=p1;	p1=p2;	p2=kk;	}	// rearrange start/end for proper dashing
	long pp1=p1,pp2=p2;
	mreal pw = fabs(PenWidth)*sqrt(font_factor/400), d=0;
	if(TernAxis&12) for(int i=0;i<4;i++)
	{	p1 = ProjScale(i, pp1);	p2 = ProjScale(i, pp2);
		if(p1>=0&&p2>=0)
		{
			d += hypot(Pnt[p1].x-Pnt[p2].x, Pnt[p1].y-Pnt[p2].y);
			MGL_LINE_PLOT
			pPos = fmod(pPos+d/pw, 16);
		}
	}
	else
	{
		d = hypot(Pnt[p1].x-Pnt[p2].x, Pnt[p1].y-Pnt[p2].y);
		MGL_LINE_PLOT
		pPos = fmod(pPos+d/pw, 16);
	}
}
//-----------------------------------------------------------------------------
#define MGL_TRIG_PLOT	if(Quality&MGL_DRAW_LMEM)	\
						{	mglDrawReg d;	d.set(this,dr_x,dr_y,dr_p);	d.PenWidth=pw;	\
							trig_draw(Pnt[p1],Pnt[p2],Pnt[p3],true,&d);	}\
						else{	mglPrim a(2);	a.n1 = p1;	a.n2 = p2;	a.n3 = p3;	\
							a.m=mask;	a.angl=MaskAn;	a.w = pw;	add_prim(a);}
void mglCanvas::trig_plot(long p1, long p2, long p3)
{
	if(SamePnt(p1,p2) || SamePnt(p1,p3))	return;
	long pp1=p1,pp2=p2,pp3=p3;
	mreal pw = fabs(PenWidth)*sqrt(font_factor/400);
	if(TernAxis&12) for(int i=0;i<4;i++)
	{	p1 = ProjScale(i, pp1);	p2 = ProjScale(i, pp2);
		p3 = ProjScale(i, pp3);	if(p1>=0&&p2>=0&&p3>=0)	{MGL_TRIG_PLOT}	}
	else	{	MGL_TRIG_PLOT	}
}
//-----------------------------------------------------------------------------
#define MGL_QUAD_PLOT	if(Quality&MGL_DRAW_LMEM)	\
						{	mglDrawReg d;	d.set(this,dr_x,dr_y,dr_p);	d.PenWidth=pw;	\
							quad_draw(Pnt[p1],Pnt[p2],Pnt[p3],Pnt[p4],&d);	}\
						else{	mglPrim a(3);	a.n1 = p1;	a.n2 = p2;	a.n3 = p3;	a.n4 = p4;	\
							a.m=mask;	a.angl=MaskAn;	a.w = pw;	add_prim(a);	}
void mglCanvas::quad_plot(long p1, long p2, long p3, long p4)
{
	if(SamePnt(p1,p2))	{	trig_plot(p4,p2,p3);	return;	}
	if(SamePnt(p2,p4))	{	trig_plot(p1,p4,p3);	return;	}
	if(SamePnt(p1,p3))	{	trig_plot(p1,p2,p4);	return;	}
	if(SamePnt(p3,p4))	{	trig_plot(p1,p2,p3);	return;	}
	long pp1=p1,pp2=p2,pp3=p3,pp4=p4;
	mreal pw = fabs(PenWidth)*sqrt(font_factor/400);
	if(TernAxis&12) for(int i=0;i<4;i++)
	{	p1 = ProjScale(i, pp1);	p2 = ProjScale(i, pp2);
		p3 = ProjScale(i, pp3);	p4 = ProjScale(i, pp4);
		if(p1>=0&&p2>=0&&p3>=0&&p4>=0)	{MGL_QUAD_PLOT}	}
	else	{	MGL_QUAD_PLOT	}
}
//-----------------------------------------------------------------------------
mreal mglCanvas::text_plot(long p,const wchar_t *text,const char *font,mreal size,mreal sh,mreal col,bool rot)
{
	if(p<0 || mgl_isnan(Pnt[p].x) || !text || *text==0)	return 0;
	if(size<0)	size *= -FontSize;
	if(!font)	font="";

	if(TernAxis&4)	// text at projections
	{
		mreal res;
		TernAxis = TernAxis&(~4);
		for(int i=0;i<4;i++)
			res = text_plot(ProjScale(i,p,true),text,font,size/2,sh,col,rot);
		TernAxis = TernAxis|4;
		return res;
	}
	else if(TernAxis&8)	// text at projections
	{
		mreal res;
		TernAxis = TernAxis&(~8);
//		for(int i=0;i<4;i++)
			res = text_plot(ProjScale(3,p,true),text,font,size/2,sh,col,rot);
		TernAxis = TernAxis|8;
		return res;
	}


	mglPnt q=Pnt[p];
	mreal ll = q.u*q.u+q.v*q.v;
	bool inv=false;
//	if(rot && (q.u<0 || (q.u==0 && q.v<0)))		// NOTE this is 1st part of rotation changes (see also GetGlyphPhi())
//	{	q.u=-q.u;	q.v=-q.v;	q.w=-q.w;	inv=true;	}

	mreal fsize=size/6.5*font_factor, h = fnt->Height(font)*fsize, w, shift = -(sh+0.02)*h;
	// text drawing itself

#if MGL_HAVE_PTHREAD
pthread_mutex_lock(&mutexPtx);
#endif
#pragma omp critical(ptx)
	{
		Bt = B;	Bt.norot=(q.sub<0);	// NOTE check this later for mglInPlot
		inv = inv ^ (strchr(font,'T')!=0);
		if(strchr(font,'V'))	shift = 0.1*h;
		else
		{
			if(inv)	shift = 0.2*h-shift;
			shift += 0.015*h;	// Correction for glyph rotation around proper point
		}

		int align;
		float col1=col, col2=col;
		if(mglGetStyle(font,0,&align))
		{
			col1 = AddTexture(font);
			col2 = col1+1/MGL_FEPSILON;
		}
		else if(col<0)
			col1 = col2 = AddTexture(char(0.5-col));
		align = align&3;

		Bt.x = q.x;	Bt.y = q.y - shift;	Bt.z = q.z;
		if(ll>0)
		{
			Bt.x += shift*q.v/sqrt(ll);	Bt.y += shift*(1-q.u/sqrt(ll));
			if(q.u==0 && !get(MGL_ENABLE_RTEXT))	Bt.y -= 0.1*h;
		}
		fscl = fsize;	forg = p;

		if(mgl_isnan(ll) || !get(MGL_ENABLE_RTEXT))	ftet = 0;
		else if(ll)	ftet = -180*atan2(q.v,q.u)/M_PI;
		else 	ftet = NAN;

		if(!(Quality&MGL_DRAW_LMEM))	// add text itself
		{
			mglColor mc = Txt[long(col1)].GetC(col1);
			mglPrim a(6);	a.n1 = p;
			a.n2 = int(255*mc.r) + 256*(int(255*mc.g) + 256*int(255*mc.b));
			a.n3 = Ptx.size();	Ptx.push_back(mglText(text,font));
			a.s = size;	a.w = shift;	a.p=ftet;
			add_prim(a);
		}

		q.c=col1;	q.ta=0;	Txt[long(col1)].GetC(col1,0,q);
		q.u = q.v = NAN;	q.a=q.ta=1;
		memset(Bt.b,0,9*sizeof(float));
		Bt.b[0] = Bt.b[4] = Bt.b[8] = fscl;
		float opf = Bt.pf;
		Bt.RotateN(ftet,0,0,1);	Bt.pf = Bt.norot?1.55:opf;
		if(strchr(font,'@'))	// draw box around text
		{
			long k1,k2,k3,k4;	mglPnt pt;	mglPoint pp;
			float y1, y2;
			w = fnt->Width(text,font, &y1,&y2);	h = fnt->Height(font);
			float d=-w*align/2.-h*0.2;	w+=h*0.4;
			pt = q;	pp.Set(d,y1-h*0.2);		PostScale(&Bt,pp);
			pt.x=pt.xx=pp.x;	pt.y=pt.yy=pp.y;
#pragma omp critical(pnt)
			{k1=Pnt.size();	MGL_PUSH(Pnt,pt,mutexPnt);}
			pt = q;	pp.Set(w+d,y1-h*0.2);		PostScale(&Bt,pp);
			pt.x=pt.xx=pp.x;	pt.y=pt.yy=pp.y;
#pragma omp critical(pnt)
			{k2=Pnt.size();	MGL_PUSH(Pnt,pt,mutexPnt);}
			pt = q;	pp.Set(d,y2+h*0.2);			PostScale(&Bt,pp);
			pt.x=pt.xx=pp.x;	pt.y=pt.yy=pp.y;
#pragma omp critical(pnt)
			{k3=Pnt.size();	MGL_PUSH(Pnt,pt,mutexPnt);}
			pt = q;	pp.Set(w+d,y2+h*0.2);		PostScale(&Bt,pp);
			pt.x=pt.xx=pp.x;	pt.y=pt.yy=pp.y;
#pragma omp critical(pnt)
			{k4=Pnt.size();	MGL_PUSH(Pnt,pt,mutexPnt);}
			PDef = 0xffff;	// reset to solid line
			line_plot(k1,k2);	line_plot(k1,k3);
			line_plot(k4,k2);	line_plot(k4,k3);
			mreal bl = AddTexture('w');
			k1 = CopyNtoC(k1,bl);	k2 = CopyNtoC(k2,bl);
			k3 = CopyNtoC(k3,bl);	k4 = CopyNtoC(k4,bl);
			quad_plot(k1,k2,k3,k4);
		}
		const char *ffont = font;
		while(*ffont && *ffont!=':')	ffont++;
		fsize *= fnt->Puts(text,ffont,col1,col2)/2;
	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexPtx);
#endif
	return fsize;
}
//-----------------------------------------------------------------------------
void mglCanvas::Glyph(mreal x, mreal y, mreal f, int s, long j, mreal col)
{
	mglPrim a(4);	// NOTE: no projection since text_plot() did it
	a.s = fscl/Bt.pf;
	a.w = get(MGL_ENABLE_RTEXT)?ftet:1e5;
	a.p = f/fnt->GetFact(s&3);
	mreal cc = col<0 ? AddTexture(char(0.5-col)):col;
	if(cc<0)	cc = CDef;
	a.n1 = AddPnt(&Bt, mglPoint(Bt.x,Bt.y,Bt.z), cc, mglPoint(x,y,NAN), -1, -1);
	a.n2 = forg; 	a.n3 = s;	a.n4 = AddGlyph(s,j);
	if(a.n1<0)	return;

	if(Quality&MGL_DRAW_LMEM)
	{
		mglDrawReg d;	d.set(this,dr_x,dr_y,dr_p);
		d.PDef = s;		d.pPos = a.s;	d.PenWidth=a.w;
		glyph_draw(a,&d);
	}
	else	add_prim(a);
}
//-----------------------------------------------------------------------------
#define MGL_GLYPH_PLOT	if(Quality&MGL_DRAW_LMEM)	glyph_draw(a,&d);\
						else	add_prim(a);
void mglCanvas::smbl_plot(long p1, char id, double size)
{
	if(p1<0 || mgl_isnan(Pnt[p1].x))	return;
	mglPnt q=Pnt[p1];
	mreal ftet=NAN, ll = q.u*q.u+q.v*q.v;
	if(mgl_isnan(ll) || !get(MGL_ENABLE_RTEXT))	ftet = 0;
	else if(ll)	ftet = -180*atan2(q.v,q.u)/M_PI;
	long pk;	q.u=q.v=0;	q.w=NAN;
#pragma omp critical(pnt)
	{pk=Pnt.size();	MGL_PUSH(Pnt,q,mutexPnt);}

	mglPrim a(4);
	a.s = fabs(size)/6.5*font_factor/B.pf;
	a.w = get(MGL_ENABLE_RTEXT)?ftet:1e5;
	a.p = 1./(mgl_fact*mgl_fgen);
	a.n1 = pk;	a.n2 = p1; 	a.n3 = size<0?4:0;	a.n4 = AddGlyph(id);
	if(a.n4<0)	return;	// no symbol is defined by user
	mglDrawReg d;	d.set(this,dr_x,dr_y,dr_p);
	d.PDef = size<0?4:0;	d.pPos = a.s;	d.PenWidth=a.w;
	if(TernAxis&12) for(int i=0;i<4;i++)
	{	a.n1 = ProjScale(i, pk);	MGL_GLYPH_PLOT	}
	else	{	MGL_GLYPH_PLOT	}
}
//-----------------------------------------------------------------------------
//	Plot positioning functions
//-----------------------------------------------------------------------------
void mglCanvas::InPlot(mreal x1,mreal x2,mreal y1,mreal y2, const char *st)
{
	if(Width<=0 || Height<=0 || Depth<=0)	return;
	if(!st)		{	InPlot(x1,x2,y1,y2,false);	return;	}
	inW = Width*(x2-x1);	inH = Height*(y2-y1);
	inX=Width*x1;	inY=Height*y1;	ZMin=1;

	if(strchr(st,'T'))	{	y1*=0.9;	y2*=0.9;	}	// general title
	bool r = !(strchr(st,'r') || strchr(st,'R') || strchr(st,'>') || strchr(st,'g'));
	bool l = !(strchr(st,'l') || strchr(st,'L') || strchr(st,'<') || strchr(st,'g'));
	bool u = !(strchr(st,'u') || strchr(st,'U') || strchr(st,'_') || strchr(st,'g'));
	bool a = !(strchr(st,'a') || strchr(st,'A') || strchr(st,'^') || strchr(st,'g') || strchr(st,'t'));
	// let use simplified scheme -- i.e. no differences between axis, colorbar and/or title
	mreal xs=(x1+x2)/2, ys=(y1+y2)/2, f1 = 1.3, f2 = 1.1;
	if(strchr(st,'#'))	f1=f2=1.55;
	if(r && l)	{	x2=xs+(x2-xs)*f1;	x1=xs+(x1-xs)*f1;	}
	else if(r)	{	x2=xs+(x2-xs)*f1;	x1=xs+(x1-xs)*f2;	}
	else if(l)	{	x2=xs+(x2-xs)*f2;	x1=xs+(x1-xs)*f1;	}
	if(a && u)	{	y2=ys+(y2-ys)*f1;	y1=ys+(y1-ys)*f1;	}
	else if(a)	{	y2=ys+(y2-ys)*f1;	y1=ys+(y1-ys)*f2;	}
	else if(u)	{	y2=ys+(y2-ys)*f2;	y1=ys+(y1-ys)*f1;	}

	B.clear();
	if(get(MGL_AUTO_FACTOR)) B.pf = 1.55;	// Automatically change plot factor !!!
	B.x = (x1+x2)/2*Width;
	B.y = (y1+y2)/2*Height;
	B.b[0] = Width*(x2-x1);	B.b[4] = Height*(y2-y1);
	B.b[8] = sqrt(B.b[0]*B.b[4]);
	B.z = (1.f-B.b[8]/(2*Depth))*Depth;
	B1=B;	font_factor = B.b[0] < B.b[4] ? B.b[0] : B.b[4];

	mglBlock p;	p.AmbBr = AmbBr;	p.DifBr = DifBr;	p.B = B;
	for(int i=0;i<10;i++)	p.light[i] = light[i];
	p.id = ObjId;	p.n1=x1*Width;	p.n2=x2*Width;	p.n3=y1*Height;	p.n4=y2*Height;
#pragma omp critical(sub)
	MGL_PUSH(Sub,p,mutexSub);
}
//-----------------------------------------------------------------------------
void mglCanvas::InPlot(mglMatrix &M,mreal x1,mreal x2,mreal y1,mreal y2, bool rel)
{
	if(Width<=0 || Height<=0 || Depth<=0)	return;
	M.clear();
	if(get(MGL_AUTO_FACTOR)) M.pf = 1.55;	// Automatically change plot factor !!!
	if(rel)
	{
		M.x = B1.x + (x1+x2-1)/2*B1.b[0]/1.55;
		M.y = B1.y + (y1+y2-1)/2*B1.b[4]/1.55;
		M.b[0] = B1.b[0]*(x2-x1);	M.b[4] = B1.b[4]*(y2-y1);
		M.b[8] = sqrt(M.b[0]*M.b[4]);
		M.z = B1.z + (1.f-M.b[8]/(2*Depth))*B1.b[8];
	}
	else
	{
		M.x = (x1+x2)/2*Width;
		M.y = (y1+y2)/2*Height;
		M.b[0] = Width*(x2-x1);	M.b[4] = Height*(y2-y1);
		M.b[8] = sqrt(M.b[0]*M.b[4]);
		M.z = (1.f-M.b[8]/(2*Depth))*Depth;
		B1=M;
	}
	inW=M.b[0];	inH=M.b[4];	ZMin=1;
	inX=Width*x1;	inY=Height*y1;
	if(!rel || !get(MGL_NO_SCALE_REL))	font_factor = M.b[0] < M.b[4] ? M.b[0] : M.b[4];

	mglBlock p;	p.AmbBr = AmbBr;	p.DifBr = DifBr;	p.B = M;
	for(int i=0;i<10;i++)	p.light[i] = light[i];
	p.id = ObjId;	p.n1=x1*Width;	p.n2=x2*Width;	p.n3=y1*Height;	p.n4=y2*Height;
#pragma omp critical(sub)
	MGL_PUSH(Sub,p,mutexSub);
}
//-----------------------------------------------------------------------------
void mglCanvas::StickPlot(int num, int id, mreal tet, mreal phi)
{
	mreal dx,dy,wx,wy,x1,y1,f1,f2;
	mglPoint p1(-1,0,0), p2(1,0,0);
	// first iteration
	InPlot(0,1,0,1,true);	Rotate(tet, phi);
	PostScale(GetB(),p1);	PostScale(GetB(),p2);	f1 = B.pf;
	dx=(p2.x-p1.x)*1.55/B1.b[0];	dy=(p2.y-p1.y)*1.55/B1.b[4];
	wx=1/(1+(num-1)*fabs(dx));		wy=1/(1+(num-1)*fabs(dy));
	x1=dx>0?dx*id:dx*(id-num+1);	y1=dy>0?dy*id:dy*(id-num+1);
	InPlot(x1*wx,(x1+1)*wx,y1*wy,(y1+1)*wy,true);	Rotate(tet,phi);
	f2 = B.pf;	dx*=f1/f2;	dy*=f1/f2;	// add correction due to PlotFactor
	wx=1/(1+(num-1)*fabs(dx));		wy=1/(1+(num-1)*fabs(dy));
	x1=dx>0?dx*id:dx*(id-num+1);	y1=dy>0?dy*id:dy*(id-num+1);
	InPlot(x1*wx,(x1+1)*wx,y1*wy,(y1+1)*wy,true);	Rotate(tet,phi);
	f1=f2;	f2 = B.pf;	dx*=f1/f2;	dy*=f1/f2;	// add correction due to PlotFactor
	wx=1/(1+(num-1)*fabs(dx));		wy=1/(1+(num-1)*fabs(dy));
	x1=dx>0?dx*id:dx*(id-num+1);	y1=dy>0?dy*id:dy*(id-num+1);
	InPlot(x1*wx,(x1+1)*wx,y1*wy,(y1+1)*wy,true);	Rotate(tet,phi);
}
//-----------------------------------------------------------------------------
void mglCanvas::Rotate(mreal tetz,mreal tetx,mreal tety)
{
	B.Rotate(tetz,tetx,tety);
	if(get(MGL_AUTO_FACTOR))
	{
		float w=(fabs(B.b[3])+fabs(B.b[4])+fabs(B.b[5]))/B1.b[4];
		float h=(fabs(B.b[0])+fabs(B.b[1])+fabs(B.b[2]))/B1.b[0];
		B.pf = 1.55+0.6147*(w<h ? (h-1):(w-1));
	}
	size_t n = Sub.size();	if(n>0)	Sub[n-1].B = B;
}
//-----------------------------------------------------------------------------
void mglMatrix::Rotate(mreal tetz,mreal tetx,mreal tety)
{
//	RotateN(TetX,1.,0.,0.);
//	RotateN(TetY,0.,1.,0.);
//	RotateN(TetZ,0.,0.,1.);
	float R[9], O[9];
	float cx=cos(tetx*M_PI/180), sx=-sin(tetx*M_PI/180), cy=cos(tety*M_PI/180), sy=-sin(tety*M_PI/180), cz=cos(tetz*M_PI/180), sz=-sin(tetz*M_PI/180);
	R[0] = cx*cy;			R[1] = -cy*sx;			R[2] = sy;
	R[3] = cx*sy*sz+cz*sx;	R[4] = cx*cz-sx*sy*sz;	R[5] =-cy*sz;
	R[6] = sx*sz-cx*cz*sy;	R[7] = cx*sz+cz*sx*sy;	R[8] = cy*cz;
	memcpy(O,b,9*sizeof(float));
	b[0] = R[0]*O[0] + R[3]*O[1] + R[6]*O[2];
	b[1] = R[1]*O[0] + R[4]*O[1] + R[7]*O[2];
	b[2] = R[2]*O[0] + R[5]*O[1] + R[8]*O[2];
	b[3] = R[0]*O[3] + R[3]*O[4] + R[6]*O[5];
	b[4] = R[1]*O[3] + R[4]*O[4] + R[7]*O[5];
	b[5] = R[2]*O[3] + R[5]*O[4] + R[8]*O[5];
	b[6] = R[0]*O[6] + R[3]*O[7] + R[6]*O[8];
	b[7] = R[1]*O[6] + R[4]*O[7] + R[7]*O[8];
	b[8] = R[2]*O[6] + R[5]*O[7] + R[8]*O[8];
}
//-----------------------------------------------------------------------------
void mglCanvas::RotateN(mreal Tet,mreal x,mreal y,mreal z)
{
	B.RotateN(Tet,x,y,z);
	if(get(MGL_AUTO_FACTOR))
	{
		float w=(fabs(B.b[3])+fabs(B.b[4])+fabs(B.b[5]))/B1.b[4];
		float h=(fabs(B.b[0])+fabs(B.b[1])+fabs(B.b[2]))/B1.b[0];
		B.pf = 1.55+0.6147*(w<h ? (h-1):(w-1));
	}
	size_t n = Sub.size();	if(n>0)	Sub[n-1].B = B;
}
//-----------------------------------------------------------------------------
void mglMatrix::RotateN(mreal Tet,mreal vx,mreal vy,mreal vz)
{
	float R[9],T[9],c=cos(Tet*M_PI/180),s=-sin(Tet*M_PI/180),r=1-c,n=sqrt(vx*vx+vy*vy+vz*vz);
	vx/=n;	vy/=n;	vz/=n;
	T[0] = vx*vx*r+c;		T[1] = vx*vy*r-vz*s;	T[2] = vx*vz*r+vy*s;
	T[3] = vx*vy*r+vz*s;	T[4] = vy*vy*r+c;		T[5] = vy*vz*r-vx*s;
	T[6] = vx*vz*r-vy*s;	T[7] = vy*vz*r+vx*s;	T[8] = vz*vz*r+c;
	memcpy(R,b,9*sizeof(float));
	b[0] = T[0]*R[0] + T[3]*R[1] + T[6]*R[2];
	b[1] = T[1]*R[0] + T[4]*R[1] + T[7]*R[2];
	b[2] = T[2]*R[0] + T[5]*R[1] + T[8]*R[2];
	b[3] = T[0]*R[3] + T[3]*R[4] + T[6]*R[5];
	b[4] = T[1]*R[3] + T[4]*R[4] + T[7]*R[5];
	b[5] = T[2]*R[3] + T[5]*R[4] + T[8]*R[5];
	b[6] = T[0]*R[6] + T[3]*R[7] + T[6]*R[8];
	b[7] = T[1]*R[6] + T[4]*R[7] + T[7]*R[8];
	b[8] = T[2]*R[6] + T[5]*R[7] + T[8]*R[8];
}
//-----------------------------------------------------------------------------
void mglCanvas::View(mreal tetx,mreal tetz,mreal tety)
{	Bp.Rotate(-tetz,-tetx,-tety);	}
//-----------------------------------------------------------------------------
void mglCanvas::Zoom(mreal x1, mreal y1, mreal x2, mreal y2)
{
	Bp.pf=0;	Bp.clear();		ClfZB();
	if(x1==x2 || y1==y2)	{	x1=y1=0;	x2=y2=1;	}
	x1=2*x1-1;	x2=2*x2-1;	y1=2*y1-1;	y2=2*y2-1;
	Bp.b[0]=2/fabs(x2-x1);	Bp.b[4]=2/fabs(y2-y1);
	Bp.x=(x1+x2)/fabs(x2-x1);Bp.y=(y1+y2)/fabs(y2-y1);
}
//-----------------------------------------------------------------------------
int mglCanvas::GetSplId(long x,long y) const
{
	long id=-1;
	for(long i=Sub.size()-1;i>=0;i--)
	{
		const mglBlock &p = Sub[i];
		if(p.n1<=x && p.n2>=x && p.n3<=y && p.n4>=y)
		{	id=p.id;	break;	}
	}
	return id;
}
//-----------------------------------------------------------------------------
void mglCanvas::Aspect(mreal Ax,mreal Ay,mreal Az)
{
	if(mgl_isnan(Ax))
	{
		mreal dy = (Max.y-Min.y), dx = (Max.x-Min.x), dz = (Max.z-Min.z);
		if(mgl_islog(Min.x,Max.x) && fx)	dx = log10(Max.x/Min.x);
		if(mgl_islog(Min.y,Max.y) && fy)	dy = log10(Max.y/Min.y);
		if(mgl_islog(Min.z,Max.z) && fz)	dz = log10(Max.z/Min.z);
		mreal gy=exp(M_LN10*floor(0.5+log10(fabs(dy/dx))));
		mreal gz=exp(M_LN10*floor(0.5+log10(fabs(dz/dx))));
		if(Ay>0)	gy*=Ay;
		if(Az>0)	gz*=Az;
		Ax = inH*dx;	Ay = inW*dy*gy;	Az = sqrt(inW*inH)*dz*gz;
	}
	mreal a = fabs(Ax) > fabs(Ay) ? fabs(Ax) : fabs(Ay);
	a = a > fabs(Az) ? a : fabs(Az);
	if(a==0)	{	SetWarn(mglWarnZero,"Aspect");	return;	}
	Ax/=a;	Ay/=a;	Az/=a;
	B.b[0] *= Ax;	B.b[3] *= Ax;	B.b[6] *= Ax;
	B.b[1] *= Ay;	B.b[4] *= Ay;	B.b[7] *= Ay;
	B.b[2] *= Az;	B.b[5] *= Az;	B.b[8] *= Az;
	size_t n = Sub.size();	if(n>0)	Sub[n-1].B = B;
}
//-----------------------------------------------------------------------------
void mglCanvas::Shear(mreal Sx,mreal Sy)
{
	float Fx=1+fabs(Sx)*inH/inW, Fy=1+fabs(Sy)*inW/inH;
	const float R[6]={B.b[0],B.b[1],B.b[2],B.b[3],B.b[4],B.b[5]};
	B.b[0] = (R[0]+Sx*R[3])/Fx;	B.b[1] = (R[1]+Sx*R[4])/Fx;	B.b[2] = (R[2]+Sx*R[5])/Fx;
	B.b[3] = (R[3]+Sy*R[0])/Fy;	B.b[4] = (R[4]+Sy*R[1])/Fy;	B.b[5] = (R[5]+Sy*R[2])/Fy;
	size_t n = Sub.size();	if(n>0)	Sub[n-1].B = B;
}
//-----------------------------------------------------------------------------
void mglCanvas::ShearPlot(int num, int id, mreal sx, mreal sy, mreal xd, mreal yd)
{
	InPlot(0,1,0,1,true);
	if(!(fabs(xd)<=1 && fabs(yd)<=1))	{	xd=1;	yd=0;	}
	mreal wx,wy,dx,dy,wf,hf,x1,y1;
	int ix=sy>=0?id:num-id-1, iy=sx>=0?id:num-id-1;
	for(int i=0;i<3;i++)	// iterations to solve cubic equation
	{
		wx = fabs(sx)*inH/inW;	dx = xd + yd*wx;	wf = 1+wx+(num-1)*fabs(dx);
		wy = fabs(sy)*inW/inH;	dy = yd + xd*wy;	hf = 1+wy+(num-1)*fabs(dy);
		x1=(dx>=0?ix:(ix-num+1))*dx;
		y1=(dy>=0?iy:(iy-num+1))*dy;
		InPlot(x1/wf,(x1+1+wx)/wf,y1/hf,(y1+1+wy)/hf,true);
	}
	Shear(sx,sy);
}
//-----------------------------------------------------------------------------
//	Lighting and transparency
//-----------------------------------------------------------------------------
void mglCanvas::Fog(mreal d, mreal dz)	{	FogDist=d;	FogDz = dz;	}
//-----------------------------------------------------------------------------
void mglCanvas::Light(int n, bool enable)
{
	if(n<0 || n>9)	{	SetWarn(mglWarnLId,"Light");	return;	}
	light[n].n = enable;
	size_t m=Sub.size();	if(m>0)	Sub[m-1].light[n].n = enable;
}
//-----------------------------------------------------------------------------
void mglCanvas::AddLight(int n, mglPoint r, mglPoint d, char col, mreal br, mreal ap)
{
	if(n<0 || n>9)	{	SetWarn(mglWarnLId,"AddLight");	return;	}
	light[n].n = true;	light[n].a = ap>0?ap*ap:3;
	light[n].b = br;	light[n].r = r;
	light[n].d = d;		light[n].c.Set(col);
	size_t m=Sub.size();	if(m>0)	Sub[m-1].light[n] = light[n];
}
//-----------------------------------------------------------------------------
void mglCanvas::arrow_plot(long n1, long n2, char st)
{
	if(n1<0 || n2<0 || !strchr("AVKSDTIOX",st))	return;
	float ll = PenWidth*ArrowSize*0.35*font_factor;
	uint64_t m=mask;	int ma=MaskAn;
	ResetMask();
	if((Quality&3)==3)
		arrow_plot_3d(n1, n2, st, ll);
	else
		arrow_draw(n1, n2, st, ll);
	mask=m;	MaskAn=ma;
}
//-----------------------------------------------------------------------------
std::wstring MGL_EXPORT mgl_ftoa(double v, const char *fmt)
{
	char se[70], sf[70], ff[8]="%.3f", ee[8]="%.3e";
	int dig=3;
	for(const char *s="0123456789";*s;s++)	if(mglchr(fmt,*s))	dig = *s-'0';
	if(mglchr(fmt,'E'))	ee[3] = 'E';
	bool plus = mglchr(fmt,'+');
	bool tex = mglchr(fmt,'F');
	int fdig = int(log10(v));	fdig = fdig>0?(fdig<dig?dig-fdig:0):dig;
	ff[2] = fdig+'0';	ee[2] = dig+'0';
	snprintf(se,64,ee,v);	snprintf(sf,64,ff,v);
	se[63] = sf[63] = 0;
	long le=strlen(se), lf=strlen(sf), i;

	// clear fix format
	for(i=lf-1;i>=lf-fdig && sf[i]=='0';i--)	sf[i]=0;
	if(sf[i]=='.')	sf[i]=0;
	lf = strlen(sf);
	// parse -nan numbers
	if(!strcmp(sf,"-nan"))	memcpy(sf,"nan",4);


	// clear exp format
	int st = se[0]=='-'?1:0;
	if(strcmp(sf,"nan"))
	{
		if(plus || se[3+st+dig]=='-')	// first remove zeros after 'e'
		{
			for(i=(dig>0?4:3)+st+dig;i<le && se[i]=='0';i++);
			memmove(se+(dig>0?4:3)+st+dig,se+i,le-i+1);
		}
		else
		{
			for(i=(dig>0?3:2)+st+dig;i<le && (se[i]=='0' || se[i]=='+');i++);
			memmove(se+(dig>0?3:2)+st+dig,se+i,le-i+1);
		}
	}
	le=strlen(se);
	// don't allow '+' at the end
	if(le>0 && se[le-1]=='+')	se[--le]=0;
	// remove single 'e'
	if(le>0 && (se[le-1]=='e' || se[le-1]=='E'))	se[--le]=0;
	for(i=1+st+dig;i>st && se[i]=='0';i--);	// remove final '0'
	if(se[i]=='.')	i--;
	memmove(se+i+1,se+2+st+dig,le-dig);	le=strlen(se);
	// add '+' sign if required
	if(plus && !strchr("-0niNI",se[0]))
	{	for(size_t i=le+1;i>0;i--)	se[i]=se[i-1];
		for(size_t i=lf+1;i>0;i--)	sf[i]=sf[i-1];
		se[0] = sf[0] = '+';	}
	if((lf>le && !mglchr(fmt,'f')) || !strcmp(sf,"0") || !strcmp(sf,"-0"))	strcpy(sf,se);
	lf = strlen(sf);
	std::wstring res;	res.reserve(lf+8);

	if(mglchr(fmt,'-') && !(plus||tex))		// replace '-' by "\minus"
		for(i=0;i<lf;i++)	res += sf[i];
	else
		for(i=0;i<lf;i++)	res += sf[i]!='-'?wchar_t(sf[i]):0x2212;
	if(tex)	// TeX notation: 'e' -> "\cdot 10^{...}"
	{
		if(res[0]=='1' && (res[1]=='e' || res[1]=='E'))
		{	res.replace(0,2,L"10^{");	res += L'}';	}
		else if(wcschr(L"+-\u2212",res[0]) && res[1]=='1' && (res[2]=='e' || res[2]=='E'))
 		{	res.replace(1,2,L"10^{");	res += L'}';	}
		else
		{
			size_t p;
			for(p=1;p<res.length();p++)	if(res[p]==L'e' || res[p]==L'E')	break;
			if(p<res.length())
			{	res.replace(p,1,L"⋅10^{");	res += L'}';	}
		}
	}
	return res;
}
//-----------------------------------------------------------------------------
void mglCanvas::Legend(const std::vector<mglText> &leg, mreal x, mreal y, const char *font, const char *opt)
{
	long n=leg.size();
	mreal iw, ih;
	if(n<1)	{	SetWarn(mglWarnLeg,"Legend");	return;	}
	mreal ll = SaveState(opt);	if(mgl_isnan(ll))	ll=0.1;
	if(saved)	MarkSize=MSS;	// restore back size of marks
	static int cgid=1;	StartGroup("Legend",cgid++);
	if(ll<=0 || mgl_isnan(ll))	ll=0.1;
	ll *=font_factor;
	mreal size = 0.8*FontSize;
	// setup font and parse absolute coordinates
	if(!font)	font="#";
	char *pA, *ff = new char[strlen(font)+3];
	const char *fmt = strchr(font,':');
	strcpy(ff,fmt?fmt:"");	strcat(ff,":L");	Push();
	if((pA=strchr(ff,'A')))
	{	*pA = ' ';	InPlot(0,1,0,1,false);	iw=B1.b[0];	ih=B1.b[4];	}
	else if(mglchr(font,'A'))
	{	InPlot(0,1,0,1,false);	iw=B1.b[0];	ih=B1.b[4];	}
	else	{	iw=B1.b[0]/B1.pf;	ih=B1.b[4]/B1.pf;	}
	// find sizes
	mreal h=TextHeight(font,size);
	mreal dx = 0.03*iw, dy = 0.03*ih, w=0, t, sp=TextWidth(" ",font,size);
	for(long i=0;i<n;i++)		// find text length
	{
		t = TextWidth(leg[i].text.c_str(),font,size)+sp;
		if(leg[i].stl.empty())	t -= ll;
		w = w>t ? w:t;
	}
	w += ll+0.01*iw;	// add space for lines
	long j = long((ih*0.95)/h);	if(j<1)	j=1;
	long ncol = 1+(n-1)/j, nrow = (n+ncol-1)/ncol;
	if(strchr(font,'-'))	// horizontal legend
	{
		j = long((iw*0.95)/w);	if(j<1)	j=1;
		nrow = 1+(n-1)/j;
		ncol = (n+nrow-1)/nrow;
	}
	if(mglchr(font,'^'))	// use "external" positioning
	{
		x = x>=0.5 ? x*iw : x*iw-w*ncol-2*dx;
		y = y>=0.5 ? y*ih : y*ih-h*nrow-2*dy;
	}
	else
	{
		x *= iw-w*ncol-2*dx;
		y *= ih-h*nrow-2*dy;
	}
	x += B.x-iw/2+dx;	y += B.y-ih/2+dy;
	// draw it
	mglPoint p,q(NAN,NAN,NAN);

	mreal cc = AddTexture(font);
	mreal c1,c2;	//=AddTexture(char(k1?k1:'w')), c2=AddTexture(char(k2?k2:'k'));
	if(cc<2 || Txt[long(cc+0.5)].n==0)
	{	c1 = AddTexture('w');	cc = c2 = AddTexture('k');	}
	else switch(Txt[long(cc+0.5)].n)
	{
	case 1:	c1 = AddTexture('w');	c2 = AddTexture('k');	break;
	case 2:	c1 = cc;	cc+=1/MGL_FEPSILON;	c2 = AddTexture('k');	break;
	default:	c1 = cc;	c2 = cc+0.5;	cc += 1/MGL_FEPSILON;	break;
	}
	if((Flag&3)==2)	{	mreal tt=c1;	c2=c1;	c1=tt;	}

	mglMatrix M=B;	M.norot=true;
	if(strchr(font,'#'))	// draw bounding box
	{
		SetPenPal("k-");
		long k1=AddPnt(&M,mglPoint(x,y,Depth/1.01),c1,q,1,0);
		long k2=AddPnt(&M,mglPoint(x+w*ncol,y,Depth/1.01),c1,q,1,0);
		long k3=AddPnt(&M,mglPoint(x,y+h*nrow,Depth/1.01),c1,q,1,0);
		long k4=AddPnt(&M,mglPoint(x+w*ncol,y+h*nrow,Depth/1.01),c1,q,1,0);
		quad_plot(k1,k2,k3,k4);
		k1=CopyNtoC(k1,c2);	k2=CopyNtoC(k2,c2);
		k3=CopyNtoC(k3,c2);	k4=CopyNtoC(k4,c2);
		line_plot(k1,k2);	line_plot(k2,k4);
		line_plot(k4,k3);	line_plot(k3,k1);
	}
	for(long i=0;i<n;i++)	// draw lines and legend
	{
		long iy=nrow-(i%nrow)-1,ix=i/nrow;
		char m=SetPenPal(leg[i].stl.c_str());
		long k1=AddPnt(&M,mglPoint(x+ix*w+0.1*ll,y+iy*h+0.45*h,Depth),CDef,q,-1,0);
		long k2=AddPnt(&M,mglPoint(x+ix*w+0.9*ll,y+iy*h+0.45*h,Depth),CDef,q,-1,0);	pPos=0;
		if(!leg[i].stl.empty())	line_plot(k1,k2);
		if(m)	for(j=0;j<LegendMarks;j++)
		{
			p.Set(x+ix*w+0.1f*ll + (j+1)*0.8f*ll/(1.+LegendMarks),y+iy*h+0.45*h,Depth);
			mark_plot(AddPnt(&M,p,CDef,q,-1,0),m);
		}
		p.Set(x+ix*w+((!leg[i].stl.empty())?ll:0.01*iw), y+iy*h+0.15*h, Depth);
		text_plot(AddPnt(&M,p,-1,q,-1,0), leg[i].text.c_str(), ff, size,0,cc);
	}
	Pop();	EndGroup();	delete []ff;
}
//-----------------------------------------------------------------------------
void mglCanvas::Table(mreal x, mreal y, HCDT val, const wchar_t *text, const char *frm, const char *opt)
{
//	if(x>=1) 	{	SetWarn(mglWarnSpc,"Table");	return;	}
	long i,j,m=val->GetNy(),n=val->GetNx();
//	mreal pos=SaveState(opt);
	mreal vw = SaveState(opt);
	static int cgid=1;	StartGroup("Table",cgid++);
	bool grid = mglchr(frm,'#'), eqd = mglchr(frm,'='), lim = mglchr(frm,'|');
	if(mgl_isnan(vw))	vw=1;	else 	lim = true;
	if(!text)	text=L"";
	x=x<0?0:x; 	y=y<0?0:y; 	y=y>1?1:y;
//	if(vw>1-x)	vw=1-x;

	char fmt[8]="3",ss[2]=" ";
	for(const char *s="0123456789";*s;s++)	if(mglchr(frm,*s))	fmt[0]=*s;
	for(const char *s="f+E-F";*s;s++)	if(mglchr(frm,*s))
	{	ss[0] = *s;	strcat(fmt,ss);	}
	std::vector<std::wstring> str;
	for(i=0;i<n;i++)		// prepare list of strings first
	{
		std::wstring buf;
		for(j=0;j+1<m;j++)
			buf += mgl_ftoa(val->v(i,j),fmt)+L'\n';
		buf += mgl_ftoa(val->v(i,m-1),fmt);
		str.push_back(buf);
	}

	mreal sp=2*TextWidth(" ",frm,-1), w=*text ? sp+TextWidth(text,frm,-1):0, w1=0, ww, h;
	for(i=0;i<n;i++)		// find width for given font size
	{
		ww = TextWidth(str[i].c_str(),frm,-1)+sp;
		w1 = w1<ww?ww:w1;
		if(!eqd)	w += ww;
	}
	if(eqd)	w += n*w1;
	// reduce font size if table have to be inside inplot
	mreal fsize=FontSize;
	if(lim && w>vw*inW)
	{	h=vw*inW/w;	SetFontSize(-h); 	w*=h; 	w1*=h;	sp*=h;	}
	h = TextHeight(frm,-1);	// now we can determine text height

	x = x*(inW-w)+B.x-inW/2;
	y = y*(inH-h*m)+B.y-inH/2;

	mglPoint p,q(NAN,NAN);
	mreal xx,yy;
	if(grid)	// draw bounding box
	{
		SetPenPal("k-");
		long k1,k2;
		k1=AddPnt(&B,mglPoint(x,y,Depth),-1,q,-1,0);
		k2=AddPnt(&B,mglPoint(x,y+m*h,Depth),-1,q,-1,0);
		line_plot(k1,k2);
		ww = *text ? TextWidth(text,frm,-1)+sp:0;
		k1=AddPnt(&B,mglPoint(x+ww,y,Depth),-1,q,-1,0);
		k2=AddPnt(&B,mglPoint(x+ww,y+m*h,Depth),-1,q,-1,0);
		line_plot(k1,k2);
		for(i=0,xx=x+ww,yy=y;i<n;i++)
		{
			xx += eqd ? w1:(TextWidth(str[i].c_str(),frm,-1)+sp);
			k1=AddPnt(&B,mglPoint(xx,yy,Depth),-1,q,-1,0);
			k2=AddPnt(&B,mglPoint(xx,yy+m*h,Depth),-1,q,-1,0);
			line_plot(k1,k2);
		}
		for(i=0,xx=x,yy=y;i<=m;i++)
		{
			k1=AddPnt(&B,mglPoint(xx,yy,Depth),-1,q,-1,0);
			k2=AddPnt(&B,mglPoint(xx+w,yy,Depth),-1,q,-1,0);
			line_plot(k1,k2);	yy += h;
		}
	}
	int align;	mglGetStyle(frm, 0, &align);
	if(*text)
	{
		ww = TextWidth(text,frm,-1)+sp;
		long k1=AddPnt(&B,mglPoint(x+ww*align/2.,y+h*(m-0.9),Depth),-1,q,-1,0);
		text_plot(k1,text,frm);
	}
	else 	ww = 0;
	for(i=0,xx=x+ww,yy=y+h*(m-0.9);i<n;i++)	// draw lines and legend
	{
		ww = eqd ? w1:(TextWidth(str[i].c_str(),frm,-1)+sp);
		long k1=AddPnt(&B,mglPoint(xx+ww*align/2.,yy,Depth),-1,q,-1,0);
		text_plot(k1,str[i].c_str(),frm);	xx += ww;
	}
	FontSize = fsize;	EndGroup();
}
//-----------------------------------------------------------------------------
void mglCanvas::Title(const char *title,const char *stl,mreal size)
{
	if(!title)	title="";
	MGL_TO_WCS(title,Title(wcs, stl,size));
}
//-----------------------------------------------------------------------------
void mglCanvas::Title(const wchar_t *title,const char *stl,mreal size)
{
	mreal s = size>0 ? size/FontSize:-size, h=TextHeight(stl,size)*s/2;
	if(h>=inH)	{	SetWarn(mglWarnSpc,"Title");	return;	}
	static int cgid=1;	StartGroup("Title",cgid++);
	int align;
	bool box=mglchr(stl,'#'), col = mglGetStyle(stl,0,&align);
	align = align&3;
	mreal y=inY+inH-h, zpos = 0;//3*Depth;
	mglPoint p(inX + inW*align/2.,y,zpos),q(NAN,NAN,NAN);
	mglMatrix M=B;	M.norot=true;
	if(title)	text_plot(AddPnt(&M,p,-1,q,-1,0),title,stl,size);
	if(box)	//	draw boungind box
	{
		mreal c1=AddTexture('w'), c2=col?AddTexture(stl):AddTexture('k');
		if((Flag&3)==2 && !col)	{	mreal cc=c1;	c2=c1;	c1=cc;	}
		else if((Flag&3)==2)	c1=AddTexture('k');
		long k1,k2,k3,k4;
		k1=AddPnt(&M,mglPoint(inX,y-h*0.4,zpos),c1,q,-1,0);
		k2=AddPnt(&M,mglPoint(inX+inW,y-h*0.4,zpos),c1,q,-1,0);
		k3=AddPnt(&M,mglPoint(inX,y+h,zpos),c1,q,-1,0);
		k4=AddPnt(&M,mglPoint(inX+inW,y+h,zpos),c1,q,-1,0);
		quad_plot(k1,k2,k3,k4);
		k1=CopyNtoC(k1,c2);	k2=CopyNtoC(k2,c2);
		k3=CopyNtoC(k3,c2);	k4=CopyNtoC(k4,c2);
		line_plot(k1,k2);	line_plot(k2,k4);
		line_plot(k4,k3);	line_plot(k3,k1);
	}
	B1.y -= h/2;	B1.b[4] -= h;	B=B1;
	inH-=h;	font_factor = B.b[0] < B.b[4] ? B.b[0] : B.b[4];
	EndGroup();
}
//-----------------------------------------------------------------------------
void mglCanvas::StartAutoGroup (const char *lbl)
{
	static int id=1;
	if(lbl==NULL)	{	id=1;	grp_counter=0;	return;	}
	grp_counter++;
	if(grp_counter>1)	return;	// do nothing in "subgroups"
	if(ObjId<0)	{	ObjId = -id;	id++;	}
	size_t len = Grp.size();
	if(ObjId>=0 && (len==0 || (len>0 && ObjId!=Grp[len-1].Id)))
#pragma omp critical(grp)
	{	MGL_PUSH(Grp,mglGroup(lbl,ObjId),mutexGrp);}
	else if(ObjId<0)
#pragma omp critical(grp)
	{	MGL_PUSH(Grp,mglGroup(lbl,ObjId),mutexGrp);}
}
//-----------------------------------------------------------------------------
void mglCanvas::EndGroup()
{
	LoadState();
	if(Quality&MGL_DRAW_LMEM)
	{
		Pnt.clear();	Prm.clear();	Ptx.clear();	ClearPrmInd();
		Glf.clear();	Act.clear(); 	Grp.clear();
	}
	if(grp_counter>0)	grp_counter--;
}
//-----------------------------------------------------------------------------
int mglCanvas::IsActive(int xs, int ys,int &n)
{
	long i, h = (Width>Height ? Height:Width)/100;
	for(i=0;i<(long)Act.size();i++)
	{
		const mglActivePos &p=Act[i];
		if(abs(xs-p.x)<=h && abs(ys-p.y)<=h)
		{	n=p.n;	return p.id;		}
	}
	n=-1;	return GetObjId(xs,ys);
}
//-----------------------------------------------------------------------------
void mglCanvas::Push()
{
#pragma omp critical(stk)
	{MGL_PUSH(stack,B,mutexStk);}
}
//-----------------------------------------------------------------------------
void mglCanvas::Pop()
{
	B = stack.back();
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexStk);
	stack.pop_back();
	pthread_mutex_unlock(&mutexStk);
#else
#pragma omp critical(stk)
	stack.pop_back();
#endif
}
