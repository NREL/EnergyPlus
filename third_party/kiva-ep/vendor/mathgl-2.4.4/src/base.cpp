/***************************************************************************
 * base.cpp is part of Math Graphic Library
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
#include "mgl2/font.h"
#include "mgl2/base.h"
#include "mgl2/eval.h"
#if MGL_HAVE_OMP
#include <omp.h>
#endif

//-----------------------------------------------------------------------------
static unsigned mgl_pb=0;
unsigned MGL_EXPORT mgl_bsize(unsigned bsize)
{
	if(!mgl_pb)	mgl_pb = (bsize>0 && bsize<100)?bsize:16;
	return mgl_pb;
}
unsigned MGL_EXPORT mgl_bsize_(unsigned *bsize)
{	return mgl_bsize(*bsize);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mutex_unlock(void *mutex)
{
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock((pthread_mutex_t *)mutex);
#elif MGL_HAVE_OMP
	omp_unset_lock((omp_lock_t *)mutex);
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mutex_lock(void *mutex)
{
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock((pthread_mutex_t *)mutex);
#elif MGL_HAVE_OMP
	omp_set_lock((omp_lock_t *)mutex);
#endif
}
//-----------------------------------------------------------------------------
char *mgl_strdup(const char *s)
{
	char *r = (char *)malloc((strlen(s)+1)*sizeof(char));
	if(r)	memcpy(r,s,(strlen(s)+1)*sizeof(char));
	return r;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_create_cpp_font(HMGL gr, const wchar_t *how)
{
	unsigned long l=mgl_wcslen(how), i, n=0, m;
	wchar_t ch=*how;
	const mglFont *f = gr->GetFont();
	std::vector<wchar_t> s;	s.push_back(ch);
	for(i=1;i<l;i++)
		if(how[i]==',')	continue;
		else if(how[i]=='-' && i+1<l)
			for(ch++;ch<how[i+1];ch++)	s.push_back(ch);
		else	s.push_back(ch=how[i]);
	for(i=l=n=0;i<s.size();i++)
	{
		ch = f->Internal(s[i]);
		if(ch>=0)	{	l += 2*f->GetNl(0,ch);	n += 6*f->GetNt(0,ch);	}
	}
	printf("const unsigned long mgl_numg=%lu, mgl_cur=%lu;\n",(unsigned long)s.size(),l+n);
	printf("const float mgl_fact=%g;\n",f->GetFact(0)/mgl_fgen);
	printf("long mgl_gen_fnt[%lu][6] = {\n", (unsigned long)s.size());
	for(i=m=0;i<s.size();i++)	// first write symbols descriptions
	{
		ch = f->Internal(s[i]);
		if(ch<0)	continue;
		int m1 = f->GetNl(0,ch), m2 = f->GetNt(0,ch);
		printf("\t{0x%x,%d,%d,%lu,%d,%lu},\n",unsigned(s[i]),f->GetWidth(0,ch),m1,m,m2,m+2*m1);
		m += 2*m1+6*m2;
	}
	if(m!=l+n)	printf("#error \"%lu !=%lu + %lu\"",m,l,n);
	printf("};\nshort mgl_buf_fnt[%lu] = {\n",m);
	for(i=0;i<s.size();i++)		// now write data itself
	{
		ch = f->Internal(s[i]);
		if(ch<0)	continue;
		unsigned m1 = f->GetNl(0,ch), m2 = f->GetNt(0,ch);
		const short *ln = f->GetLn(0,ch), *tr = f->GetTr(0,ch);
		for(l=0;l<2*m1;l++)	printf("%d,",ln[l]);
		printf("\n");
		for(l=0;l<6*m2;l++)	printf("%d,",tr[l]);
		printf("\n");
	}
	printf("};\n");
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_strtrim(char *str)
{
	if(!str || *str==0)	return;
	size_t n=strlen(str), k, i;
	for(k=0;k<n;k++)	if(str[k]>' ')	break;
	for(i=n;i>k;i--)	if(str[i-1]>' ')	break;
	memmove(str, str+k, (i-k));
	str[i-k]=0;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_strlwr(char *str)
{
	size_t l=strlen(str);
	for(size_t k=0;k<l;k++)
		str[k] = (str[k]>='A' && str[k]<='Z') ? str[k]+'a'-'A' : str[k];
}
//-----------------------------------------------------------------------------
mglBase::mglBase()
{
	Flag=0;	saved=false;	PrmInd=NULL;
#if MGL_HAVE_PTHREAD
	pthread_mutex_init(&mutexPnt,0);
	pthread_mutex_init(&mutexTxt,0);
	pthread_mutex_init(&mutexSub,0);
	pthread_mutex_init(&mutexLeg,0);
	pthread_mutex_init(&mutexPrm,0);
	pthread_mutex_init(&mutexPtx,0);
	pthread_mutex_init(&mutexStk,0);
	pthread_mutex_init(&mutexGrp,0);
	pthread_mutex_init(&mutexGlf,0);
	pthread_mutex_init(&mutexAct,0);
	pthread_mutex_init(&mutexDrw,0);
	pthread_mutex_init(&mutexClf,0);
	Pnt.set_mutex(&mutexClf);
	Prm.set_mutex(&mutexClf);
//	Txt.set_mutex(&mutexClf);
#endif
#if MGL_HAVE_OMP
	lockClf = new omp_lock_t;
	omp_init_lock((omp_lock_t*)lockClf);
	Pnt.set_mutex(lockClf);
	Prm.set_mutex(lockClf);
//	Txt.set_mutex(&lockClf);
#else
	lockClf = NULL;
#endif
	fnt=0;	*FontDef=0;	fx=fy=fz=fa=fc=0;
	AMin.Set(0,0,0,0);	AMax.Set(1,1,1,1);

	InUse = 1;	SetQuality();	FaceNum = 0;
	// Always create default palette txt[0] and default scheme txt[1]
	mglTexture t1(MGL_DEF_PAL,-1), t2(MGL_DEF_SCH,1);
	Txt.reserve(3);
	MGL_PUSH(Txt,t1,mutexTxt);
	MGL_PUSH(Txt,t2,mutexTxt);

	strcpy(last_style,"__1 {dFFFF}k\0");
	MinS.Set(-1,-1,-1);	MaxS.Set(1,1,1);
	fnt = new mglFont;	fnt->gr = this;	PrevState=size_opt=NAN;
}
//-----------------------------------------------------------------------------
mglBase::~mglBase()
{
	ClearEq();	ClearPrmInd();	delete fnt;
	Pnt.set_mutex(0);	Prm.set_mutex(0);	//Txt.set_mutex(0);
#if MGL_HAVE_PTHREAD
	pthread_mutex_destroy(&mutexPnt);
	pthread_mutex_destroy(&mutexTxt);
	pthread_mutex_destroy(&mutexSub);
	pthread_mutex_destroy(&mutexLeg);
	pthread_mutex_destroy(&mutexPrm);
	pthread_mutex_destroy(&mutexPtx);
	pthread_mutex_destroy(&mutexStk);
	pthread_mutex_destroy(&mutexGrp);
	pthread_mutex_destroy(&mutexGlf);
	pthread_mutex_destroy(&mutexAct);
	pthread_mutex_destroy(&mutexDrw);
	pthread_mutex_destroy(&mutexClf);
#endif
#if MGL_HAVE_OMP
	omp_destroy_lock((omp_lock_t*)lockClf);
	delete ((omp_lock_t*)lockClf);
#endif
}
//-----------------------------------------------------------------------------
void mglBase::RestoreFont()	{	fnt->Restore();	}
void mglBase::LoadFont(const char *name, const char *path)
{	if(name && *name)	fnt->Load(name,path);	else	fnt->Restore();	}
void mglBase::CopyFont(mglBase *gr)	{	fnt->Copy(gr->GetFont());	}
//-----------------------------------------------------------------------------
mreal mglBase::TextWidth(const char *text, const char *font, mreal size) const
{	return (size<0?-size*FontSize:size)*font_factor*fnt->Width(text,(font&&*font)?font:FontDef)/20.16;	}
mreal mglBase::TextWidth(const wchar_t *text, const char *font, mreal size) const
{	return (size<0?-size*FontSize:size)*font_factor*fnt->Width(text,(font&&*font)?font:FontDef)/20.16;	}
mreal mglBase::TextHeight(const char *text, const char *font, mreal size) const
{	float y1,y2;	fnt->Width(text,(font&&*font)?font:FontDef,&y1,&y2);
	return (size<0?-size*FontSize:size)*font_factor*(y2-y1)/20.16;	}
mreal mglBase::TextHeight(const wchar_t *text, const char *font, mreal size) const
{	float y1,y2;	fnt->Width(text,(font&&*font)?font:FontDef,&y1,&y2);
	return (size<0?-size*FontSize:size)*font_factor*(y2-y1)/20.16;	}
mreal mglBase::TextHeight(const char *font, mreal size) const
{	return (size<0?-size*FontSize:size)*font_factor*fnt->Height(font?font:FontDef)/20.16; }
void mglBase::AddActive(long k,int n)
{
	if(k<0 || (size_t)k>=Pnt.size())	return;
	mglActivePos p;
	const mglPnt &q=Pnt[k];
	int h=GetHeight();
	p.x = int(q.x);	p.y = h>1?h-1-int(q.y):int(q.y);
	p.id = ObjId;	p.n = n;
#pragma omp critical(act)
	MGL_PUSH(Act,p,mutexAct);
}
//-----------------------------------------------------------------------------
mreal mglBase::GetRatio() const	{	return 1;	}
int mglBase::GetWidth()  const	{	return 1;	}
int mglBase::GetHeight() const	{	return 1;	}
//-----------------------------------------------------------------------------
void mglBase::StartGroup(const char *name, int id)
{
	LightScale(&B);
	char buf[128];
	snprintf(buf,128,"%s_%d",name,id);	buf[127]=0;
	StartAutoGroup(buf);
}
//-----------------------------------------------------------------------------
const char *mglWarn[mglWarnEnd] = {_("data dimension(s) is incompatible"),	//mglWarnDim
								_("data dimension(s) is too small"),		//mglWarnLow
								_("minimal data value is negative"),		//mglWarnNeg
								_("no file or wrong data dimensions"),		//mglWarnFile
								_("not enough memory"), 					//mglWarnMem
								_("data values are zero"),					//mglWarnZero
								_("no legend entries"),					//mglWarnLeg
								_("slice value is out of range"),			//mglWarnSlc
								_("number of contours is zero or negative"),//mglWarnCnt
								_("couldn't open file"),					//mglWarnOpen
								_("light: ID is out of range"),			//mglWarnLId
								_("size(s) is zero or negative"),			//mglWarnSize
								_("format is not supported for that build"),//mglWarnFmt
								_("axis ranges are incompatible"),			//mglWarnTern
								_("pointer is NULL"),						//mglWarnNull
								_("not enough space for plot"),			//mglWarnSpc
								_("There is wrong argument(s) in script"),	//mglScrArg
								_("There is wrong command(s) in script"),	//mglScrCmd
								_("There is too long string(s) in script"),	//mglScrLong
								_("There is unbalanced ' in script"),		//mglScrStr
								_("There is changing temporary data in script")};	//mglScrTemp
//-----------------------------------------------------------------------------
extern bool mglPrintWarn;
void mglBase::SetWarn(int code, const char *who)
{
	std::string warn;
	WarnCode = code>0 ? code:0;
	if(code>0 && code<mglWarnEnd)
	{
		if(who && *who)	warn = std::string(who)+": ";
		warn = warn+mglWarn[code-1];
	}
	else if(!code)	Mess="";
	else if(who && *who)	warn = who;
	if(mglPrintWarn && !warn.empty())	fprintf(stderr,_("MathGL message - %s\n"),warn.c_str());
	if(code && !warn.empty())	Mess = Mess+(code==-2?"":"\n")+warn;
	LoadState();
}
//-----------------------------------------------------------------------------
//		Add glyph to the buffer
//-----------------------------------------------------------------------------
void mglGlyph::Create(long Nt, long Nl)
{
//	if(Nt<0 || Nl<0)	return;
	nt=Nt;	nl=Nl;
#pragma omp critical(glf_create)
	{
		if(trig)	delete []trig;
		trig = nt>0?new short[6*nt]:0;
		if(line)	delete []line;
		line = nl>0?new short[2*nl]:0;
	}
}
//-----------------------------------------------------------------------------
bool mglGlyph::operator==(const mglGlyph &g) const
{
	if(nl!=g.nl || nt!=g.nt)	return false;
	if(trig && memcmp(trig,g.trig,6*nt*sizeof(short)))	return false;
	if(line && memcmp(line,g.line,2*nl*sizeof(short)))	return false;
	return true;
}
//-----------------------------------------------------------------------------
long mglBase::AddGlyph(int s, long j)
{
	// first create glyph for current typeface
	s = s&3;
	mglGlyph g(fnt->GetNt(s,j), fnt->GetNl(s,j));
	memcpy(g.trig, fnt->GetTr(s,j), 6*g.nt*sizeof(short));
	memcpy(g.line, fnt->GetLn(s,j), 2*g.nl*sizeof(short));
	// now let find the similar glyph
	for(size_t i=0;i<Glf.size();i++)
		if(g!=Glf[i])	continue;	else	return i;
	// if no one then let add it
	long k;
#pragma omp critical(glf)
	{k=Glf.size();	MGL_PUSH(Glf,g,mutexGlf);}	return k;
}
//-----------------------------------------------------------------------------
long mglBase::AddGlyph(unsigned char id)
{
	size_t j=0;
	for(size_t i=0;i<UserGlf.size();i++)
		if(UserGlf[i].nt==-id)	j=i+1;
	if(j==0)	return -1;
	const mglGlyph &g=UserGlf[j-1];
	// let find the similar glyph
	for(size_t i=0;i<Glf.size();i++)
		if(g!=Glf[i])	continue;	else	return i;
	// if no one then let add it
	long k;
#pragma omp critical(glf)
	{k=Glf.size();	MGL_PUSH(Glf,g,mutexGlf);}	return k;
}
//-----------------------------------------------------------------------------
void mglBase::DefineGlyph(HCDT x, HCDT y, unsigned char id)
{
	long n = x->GetNx();
	if(y->GetNx()!=n || n<2)	return;
	mglGlyph g(-id,n);
	mreal x1=1e10,x2=-1e10,y1=1e10,y2=-1e10;
	for(long i=0;i<n;i++)
	{
		mreal xx = x->v(i), yy = y->v(i);
		x1=x1>xx?xx:x1;	x2=x2<xx?xx:x2;
		y1=y1>yy?yy:y1;	y2=y2<yy?yy:y2;
	}
	mreal scale = 1;
	if(fabs(x1)<10 && fabs(x2)<10 && fabs(y1)<10 && fabs(y2)<10)
		scale=1000;
	for(long i=0;i<n;i++)
	{
		short sx = short(x->v(i)*scale), sy = short(y->v(i)*scale);
		g.line[2*i] = sx;	g.line[2*i+1] = sy;
	}
	UserGlf.push_back(g);
}
//-----------------------------------------------------------------------------
//		Add points to the buffer
//-----------------------------------------------------------------------------
long mglBase::PushPnts(size_t num, const mglPnt *qq)
{
	long k;
#pragma omp critical(pnt)
	MGL_PUSHs({k=Pnt.size();Pnt.push_back(num,qq);},mutexPnt);
	return k;
}
//-----------------------------------------------------------------------------
long mglBase::AllocPnts(size_t num)
{
	long k;
#pragma omp critical(pnt)
	MGL_PUSHs({k=Pnt.allocate(num);},mutexPnt);
	return k;
}
//-----------------------------------------------------------------------------
void inline mgl_put_inbox(mreal a1, mreal a2, mreal &a)
{
	if(a1<a2)	{	if(a<a1)	a=a1;	if(a>a2)	a=a2;	}
	else		{	if(a<a2)	a=a2;	if(a>a1)	a=a1;	}
}
static void mgl_coor_box(HMGL gr, mglPoint &p)
{
	mgl_put_inbox(gr->Min.x, gr->Max.x, p.x);
	mgl_put_inbox(gr->Min.y, gr->Max.y, p.y);
	mgl_put_inbox(gr->Min.z, gr->Max.z, p.z);
}
long mglBase::AddPnt(const mglMatrix *mat, mglPoint p, mreal c, mglPoint n, mreal a, int scl)
{
	mglPnt q;
	if(!AddPntQ(q,mat,p,c,n,a,scl))	return -1;
	long k;
#pragma omp critical(pnt)
	{k=Pnt.size();	MGL_PUSH(Pnt,q,mutexPnt);}	return k;
}
bool mglBase::AddPntQ(mglPnt &q, const mglMatrix *mat, mglPoint p, mreal c, mglPoint n, mreal a, int scl)
{
	// scl=0 -- no scaling
	// scl&1 -- usual scaling
	// scl&2 -- disable NAN at scaling
	// scl&4 -- disable NAN for normales if no light
	// scl&8 -- bypass palette for enabling alpha
	// scl&16 -- put points inside axis range
	if(mgl_isnan(c) || mgl_isnan(a))	{	q.x=NAN;	return false;	}
	bool norefr = mgl_isnan(n.x) && mgl_isnan(n.y) && !mgl_isnan(n.z), res=true;
	if(scl>0)
	{
		if(scl&16)	mgl_coor_box(this, p);
		res = ScalePoint(mat,p,n,!(scl&2));
	}
	if(mgl_isnan(p.x))	{	q.x=NAN;	return false;	}
	a = (a>=0 && a<=1) ? a : AlphaDef;
	c = (c>=0) ? c:CDef;

	if(get(MGL_REDUCEACC))
	{
		q.x=q.xx=int(p.x*10)*0.1;	q.y=q.yy=int(p.y*10)*0.1;	q.z=q.zz=int(p.z*10)*0.1;
		q.c=int(c*100)*0.01;		q.ta=int(a*100)*0.01;
		q.u=mgl_isnum(n.x)?int(n.x*100)*0.01:NAN;
		q.v=mgl_isnum(n.y)?int(n.y*100)*0.01:NAN;
		q.w=mgl_isnum(n.z)?int(n.z*100)*0.01:NAN;
	}
	else
	{
		q.x=q.xx=p.x;	q.y=q.yy=p.y;	q.z=q.zz=p.z;
		q.c=c;	q.ta=a;	q.u=n.x;	q.v=n.y;	q.w=n.z;
	}
	long ci=long(c);
	if(ci<0 || ci>=(long)Txt.size())	ci=0;	// NOTE never should be here!!!
	const mglTexture &txt=Txt[ci];
	txt.GetC(c,a,q);	// RGBA color
	if(get(MGL_GRAY_MODE))
	{
		float h = 0.3*q.r + 0.59*q.g + 0.11*q.b;
		q.r = q.g = q.b = h;
	}

	// add gap for texture coordinates for compatibility with OpenGL
	const mreal gap = 0./MGL_TEXTURE_COLOURS;
	q.c = ci+(q.c-ci)*(1-2*gap)+gap;
	q.ta = q.ta*(1-2*gap)+gap;

	if(scl&8 && scl>0)	q.a=a;	// bypass palette for enabling alpha in Error()
	if(!get(MGL_ENABLE_ALPHA))	{	q.a=1;	if(txt.Smooth!=2)	q.ta=1-gap;	}
	if(norefr)	q.v=0;
	if(!get(MGL_ENABLE_LIGHT) && !(scl&4))	q.u=q.v=NAN;
	q.sub=mat->norot?-1*(int)Sub.size():Sub.size()-1;
	return (scl&16)?res:true;
}
//-----------------------------------------------------------------------------
long mglBase::CopyNtoC(long from, mreal c)
{
	mglPnt q;
	if(!CopyNtoC(q,from,c))	return -1;
	long k;
#pragma omp critical(pnt)
	{k=Pnt.size();	MGL_PUSH(Pnt,q,mutexPnt);}	return k;
}
//-----------------------------------------------------------------------------
bool mglBase::CopyNtoC(mglPnt &q, long from, mreal c)
{
	if(from<0)	return false;
	q = Pnt[from];
	if(mgl_isnum(c))	{	q.c=c;	q.ta=1;	Txt[long(c)].GetC(c,0,q);	q.a=1;	}
	else	q.x = NAN;
	return mgl_isnum(q.x);
}
//-----------------------------------------------------------------------------
long mglBase::CopyProj(long from, mglPoint p, mglPoint n, short sub)
{
	mglPnt q;
	if(!CopyProj(q,from,p,n,sub))	return -1;
	long k;
#pragma omp critical(pnt)
	{k=Pnt.size();	MGL_PUSH(Pnt,q,mutexPnt);}	return k;
}
//-----------------------------------------------------------------------------
bool mglBase::CopyProj(mglPnt &q, long from, mglPoint p, mglPoint n, short sub)
{
	if(from<0)	return false;
	q=Pnt[from];	q.sub = sub;
	q.x=q.xx=p.x;	q.y=q.yy=p.y;	q.z=q.zz=p.z;
	q.u = n.x;		q.v = n.y;		q.w = n.z;
	return mgl_isnum(q.x);
}
//-----------------------------------------------------------------------------
void mglBase::Reserve(long n)
{
	if(TernAxis&12)	n*=4;
#pragma omp critical(pnt)
	Pnt.reserve(n);
#pragma omp critical(prm)
	Prm.reserve(n);
}
//-----------------------------------------------------------------------------
//		Boundaries and scaling
//-----------------------------------------------------------------------------
bool mglBase::RecalcCRange()
{
	bool wrong=false;
	if(!fa)
	{	FMin.c = Min.c;	FMax.c = Max.c;	}
	else
	{
		FMin.c = INFINITY;	FMax.c = -INFINITY;
		int n=30;
		for(int i=0;i<=n;i++)
		{
			mreal a = fa->Calc(0,0,0,Min.c+i*(Max.c-Min.c)/n);
			if(mgl_isbad(a))	wrong=true;
			if(a<FMin.c)	FMin.c=a;
			if(a>FMax.c)	FMax.c=a;
		}
	}
	return wrong;
}
//-----------------------------------------------------------------------------
void mglBase::RecalcBorder()
{
	ZMin = 1.;
	bool wrong=false;
	if(!fx && !fy && !fz)
	{	FMin = Min;	FMax = Max;	}
	else
	{
		FMin.Set( INFINITY, INFINITY, INFINITY);
		FMax.Set(-INFINITY,-INFINITY,-INFINITY);
		int n=30;
		for(int i=0;i<=n;i++)	for(int j=0;j<=n;j++)	// x range
		{
			if(SetFBord(Min.x, Min.y+i*(Max.y-Min.y)/n, Min.z+j*(Max.z-Min.z)/n))	wrong=true;
			if(SetFBord(Max.x, Min.y+i*(Max.y-Min.y)/n, Min.z+j*(Max.z-Min.z)/n))	wrong=true;
		}
		for(int i=0;i<=n;i++)	for(int j=0;j<=n;j++)	// y range
		{
			if(SetFBord(Min.x+i*(Max.x-Min.x)/n, Min.y, Min.z+j*(Max.z-Min.z)/n))	wrong=true;
			if(SetFBord(Min.x+i*(Max.x-Min.x)/n, Max.y, Min.z+j*(Max.z-Min.z)/n))	wrong=true;
		}
		for(int i=0;i<=n;i++)	for(int j=0;j<=n;j++)	// x range
		{
			if(SetFBord(Min.x+i*(Max.x-Min.x)/n, Min.y+j*(Max.y-Min.y)/n, Min.z))	wrong=true;
			if(SetFBord(Min.x+i*(Max.x-Min.x)/n, Min.y+j*(Max.y-Min.y)/n, Max.z))	wrong=true;
		}
		if(!fx)	{	FMin.x = Min.x;	FMax.x = Max.x;	}
		else	{	mreal d=0.01*(FMax.x-FMin.x);	FMin.x-=d;	FMax.x+=d;	}
		if(!fy)	{	FMin.y = Min.y;	FMax.y = Max.y;	}
		else	{	mreal d=0.01*(FMax.y-FMin.y);	FMin.y-=d;	FMax.y+=d;	}
		if(!fz)	{	FMin.z = Min.z;	FMax.z = Max.z;	}
		else	{	mreal d=0.01*(FMax.z-FMin.z);	FMin.z-=d;	FMax.z+=d;	}
	}
	if(RecalcCRange())	wrong=true;
	if(wrong)	SetWarn(mglWarnTern, "Curved coordinates");
}
//-----------------------------------------------------------------------------
bool mglBase::SetFBord(mreal x,mreal y,mreal z)
{
	bool wrong=false;
	if(fx)
	{
		mreal v = fx->Calc(x,y,z);
		if(mgl_isbad(v))	wrong = true;
		if(FMax.x < v)	FMax.x = v;
		if(FMin.x > v)	FMin.x = v;
	}
	if(fy)
	{
		mreal v = fy->Calc(x,y,z);
		if(mgl_isbad(v))	wrong = true;
		if(FMax.y < v)	FMax.y = v;
		if(FMin.y > v)	FMin.y = v;
	}
	if(fz)
	{
		mreal v = fz->Calc(x,y,z);
		if(mgl_isbad(v))	wrong = true;
		if(FMax.z < v)	FMax.z = v;
		if(FMin.z > v)	FMin.z = v;
	}
	return wrong;
}
//-----------------------------------------------------------------------------
bool mglBase::ScalePoint(const mglMatrix *, mglPoint &p, mglPoint &n, bool use_nan) const
{
	mreal &x=p.x, &y=p.y, &z=p.z;
	if(mgl_isnan(x) || mgl_isnan(y) || mgl_isnan(z))	{	x=NAN;	return false;	}
	mreal x1,y1,z1,x2,y2,z2;
	x1 = x>0?x*MGL_EPSILON:x/MGL_EPSILON;	x2 = x<0?x*MGL_EPSILON:x/MGL_EPSILON;
	y1 = y>0?y*MGL_EPSILON:y/MGL_EPSILON;	y2 = y<0?y*MGL_EPSILON:y/MGL_EPSILON;
	z1 = z>0?z*MGL_EPSILON:z/MGL_EPSILON;	z2 = z<0?z*MGL_EPSILON:z/MGL_EPSILON;
	bool res = true;
	if(x2>CutMin.x && x1<CutMax.x && y2>CutMin.y && y1<CutMax.y &&
		z2>CutMin.z && z1<CutMax.z)	res = false;
	if(fc && fc->Calc(x,y,z)!=0)	res = false;

	if(get(MGL_ENABLE_CUT) || !use_nan)
	{
//		if(x1<Min.x || x2>Max.x || y1<Min.y || y2>Max.y || z1<Min.z || z2>Max.z)	res = false;
		if((x1-Min.x)*(x1-Max.x)>0 && (x2-Min.x)*(x2-Max.x)>0)	res = false;
		if((y1-Min.y)*(y1-Max.y)>0 && (y2-Min.y)*(y2-Max.y)>0)	res = false;
		if((z1-Min.z)*(z1-Max.z)>0 && (z2-Min.z)*(z2-Max.z)>0)	res = false;
	}
	else
	{
		if(Min.x<Max.x)
		{
			if(x1<Min.x)	{x=Min.x;	n.Set(1,0,0);}
			if(x2>Max.x)	{x=Max.x;	n.Set(1,0,0);}
		}
		else
		{
			if(x1<Max.x)	{x=Max.x;	n.Set(1,0,0);}
			if(x2>Min.x)	{x=Min.x;	n.Set(1,0,0);}
		}
		if(Min.y<Max.y)
		{
			if(y1<Min.y)	{y=Min.y;	n.Set(0,1,0);}
			if(y2>Max.y)	{y=Max.y;	n.Set(0,1,0);}
		}
		else
		{
			if(y1<Max.y)	{y=Max.y;	n.Set(0,1,0);}
			if(y2>Min.y)	{y=Min.y;	n.Set(0,1,0);}
		}
		if(Min.z<Max.z)
		{
			if(z1<Min.z)	{z=Min.z;	n.Set(0,0,1);}
			if(z2>Max.z)	{z=Max.z;	n.Set(0,0,1);}
		}
		else
		{
			if(z1<Max.z)	{z=Max.z;	n.Set(0,0,1);}
			if(z2>Min.z)	{z=Min.z;	n.Set(0,0,1);}
		}
	}

	x1=x;	y1=y;	z1=z;
	mreal xx=1,xy=0,xz=0,yx=0,yy=1,yz=0,zx=0,zy=0,zz=1;
	if(fx)	{	x1 = fx->Calc(x,y,z);	xx = fx->CalcD('x',x,y,z);	xy = fx->CalcD('y',x,y,z);	xz = fx->CalcD('z',x,y,z);	}
	if(fy)	{	y1 = fy->Calc(x,y,z);	yx = fy->CalcD('x',x,y,z);	yy = fy->CalcD('y',x,y,z);	yz = fy->CalcD('z',x,y,z);	}
	if(fz)	{	z1 = fz->Calc(x,y,z);	zx = fz->CalcD('x',x,y,z);	zy = fz->CalcD('y',x,y,z);	zz = fz->CalcD('z',x,y,z);	}
	if(mgl_isnan(x1) || mgl_isnan(y1) || mgl_isnan(z1))	{	x=NAN;	return false;	}

	mreal d;
	d = 1/(FMax.x - FMin.x);	x = (2*x1 - FMin.x - FMax.x)*d;	xx /= d;	xy /= d;	xz /= d;
	d = 1/(FMax.y - FMin.y);	y = (2*y1 - FMin.y - FMax.y)*d;	yx /= d;	yy /= d;	yz /= d;
	d = 1/(FMax.z - FMin.z);	z = (2*z1 - FMin.z - FMax.z)*d;	zx /= d;	zy /= d;	zz /= d;
	mreal nx=n.x, ny=n.y, nz=n.z;
	n.x = nx*xx+ny*xy+nz*xz;
	n.y = nx*yx+ny*yy+nz*yz;
	n.z = nx*zx+ny*zy+nz*zz;
	if((TernAxis&3)==1)	// usual ternary axis
	{
		if(x+y>0)
		{
			if(get(MGL_ENABLE_CUT))	res = false;
			else	y = -x;
		}
		x += (y+1)/2;	n.x += n.y/2;
	}
	else if((TernAxis&3)==2)	// quaternary axis
	{
		if(x+y+z>-1)
		{
			if(get(MGL_ENABLE_CUT))	res = false;
			else	z = -1-y-x;
		}
		x += 1+(y+z)/2;		y += (z+1)/3;
		n.x += (n.y+n.z)/2;	n.y += n.z/3;
	}
	if(fabs(x)>MGL_FEPSILON || fabs(y)>MGL_FEPSILON || fabs(z)>MGL_FEPSILON)	res = false;

	if(!res && use_nan)	x = NAN;	// extra sign that point shouldn't be plotted
	return res;
}
//-----------------------------------------------------------------------------
//		Ranges
//-----------------------------------------------------------------------------
void mglScaleAxis(mreal &v1, mreal &v2, mreal &v0, mreal x1, mreal x2)
{
	if(!mgl_isrange(x1,x2) || !mgl_isrange(v1,v2))	return;
	mreal dv,d0;	x2-=1;
	if(v1*v2>0 && (v2/v1>=100 || v2/v1<=0.01))	// log scale
	{
		dv=log(v2/v1);	d0 = log(v0/v1)/log(v2/v1);
		v1*=exp(dv*x1);	v2*=exp(dv*x2);	v0=v1*exp(d0*log(v2/v1));
	}
	else
	{
		dv=v2-v1;	d0=(v0-v1)/(v2-v1);
		v1+=dv*x1;	v2+=dv*x2;	v0=v1+d0*(v2-v1);
	}
}
//-----------------------------------------------------------------------------
void mglBase::SetOrigin(mreal x0, mreal y0, mreal z0, mreal c0)
{
	Org.Set(x0,y0,z0,c0);
	if((TernAxis&3)==0)
	{
		Min = OMin;	Max = OMax;
		mglScaleAxis(Min.x, Max.x, Org.x, AMin.x, AMax.x);
		mglScaleAxis(Min.y, Max.y, Org.y, AMin.y, AMax.y);
		mglScaleAxis(Min.z, Max.z, Org.z, AMin.z, AMax.z);
		mglScaleAxis(Min.c, Max.c, Org.c, AMin.c, AMax.c);
	}
}
//-----------------------------------------------------------------------------
void mglBase::SetRanges(mglPoint m1, mglPoint m2)
{
	if(mgl_isrange(m1.x, m2.x))	{	Min.x=m1.x;	Max.x=m2.x;	}
	if(mgl_isrange(m1.y, m2.y))	{	Min.y=m1.y;	Max.y=m2.y;	}
	if(mgl_isrange(m1.z, m2.z))	{	Min.z=m1.z;	Max.z=m2.z;	}
	if(mgl_isrange(m1.c, m2.c))	{	Min.c=m1.c;	Max.c=m2.c;	}
	else	{	Min.c=Min.z;Max.c=Max.z;}

	if(Org.x<Min.x && mgl_isnum(Org.x))	Org.x = Min.x;
	if(Org.x>Max.x && mgl_isnum(Org.x))	Org.x = Max.x;
	if(Org.y<Min.y && mgl_isnum(Org.y))	Org.y = Min.y;
	if(Org.y>Max.y && mgl_isnum(Org.y))	Org.y = Max.y;
	if(Org.z<Min.z && mgl_isnum(Org.z))	Org.z = Min.z;
	if(Org.z>Max.z && mgl_isnum(Org.z))	Org.z = Max.z;

	if((TernAxis&3)==0)
	{
		OMax = Max;	OMin = Min;
		mglScaleAxis(Min.x, Max.x, Org.x, AMin.x, AMax.x);
		mglScaleAxis(Min.y, Max.y, Org.y, AMin.y, AMax.y);
		mglScaleAxis(Min.z, Max.z, Org.z, AMin.z, AMax.z);
		mglScaleAxis(Min.c, Max.c, Org.c, AMin.c, AMax.c);
	}

	CutMin.Set(0,0,0);	CutMax.Set(0,0,0);
	RecalcBorder();
}
//-----------------------------------------------------------------------------
void mglBase::CRange(HCDT a,bool add, mreal fact)
{
	mreal v1=a->Minimal(), v2=a->Maximal(), dv;
	dv=(v2-v1)*fact;	v1 -= dv;	v2 += dv;
	CRange(v1,v2,add);
}
void mglBase::CRange(mreal v1,mreal v2,bool add)
{
	if(!mgl_isrange(v1,v2) && !add)	return;
	if(!add)
	{
		if(mgl_isnum(v1))	Min.c = v1;
		if(mgl_isnum(v2))	Max.c = v2;
	}
	else if(Min.c<Max.c)
	{
		if(Min.c>v1)	Min.c=v1;
		if(Max.c<v2)	Max.c=v2;
	}
	else
	{
		mreal dv = Min.c;
		Min.c = v1<Max.c ? v1:Max.c;
		Max.c = v2>dv ? v2:dv;
	}
	if(Org.c<Min.c && mgl_isnum(Org.c))	Org.c = Min.c;
	if(Org.c>Max.c && mgl_isnum(Org.c))	Org.c = Max.c;
	if((TernAxis&3)==0)
	{
		OMax.c = Max.c;	OMin.c = Min.c;
		mglScaleAxis(Min.c, Max.c, Org.c, AMin.c, AMax.c);
	}
	RecalcCRange();
}
//-----------------------------------------------------------------------------
void mglBase::XRange(HCDT a,bool add,mreal fact)
{
	mreal v1=a->Minimal(), v2=a->Maximal(), dv;
	dv=(v2-v1)*fact;	v1 -= dv;	v2 += dv;
	XRange(v1,v2,add);
}
void mglBase::XRange(mreal v1,mreal v2,bool add)
{
	if(!mgl_isrange(v1,v2) && !add)	return;
	if(!add)
	{
		if(mgl_isnum(v1))	Min.x = v1;
		if(mgl_isnum(v2))	Max.x = v2;
	}
	else if(Min.x<Max.x)
	{
		if(Min.x>v1)	Min.x=v1;
		if(Max.x<v2)	Max.x=v2;
	}
	else
	{
		mreal dv = Min.x;
		Min.x = v1<Max.x ? v1:Max.x;
		Max.x = v2>dv ? v2:dv;
	}
	if(Org.x<Min.x && mgl_isnum(Org.x))	Org.x = Min.x;
	if(Org.x>Max.x && mgl_isnum(Org.x))	Org.x = Max.x;
	if((TernAxis&3)==0)
	{
		OMax.x = Max.x;	OMin.x = Min.x;
		mglScaleAxis(Min.x, Max.x, Org.x, AMin.x, AMax.x);
	}
	RecalcBorder();
}
//-----------------------------------------------------------------------------
void mglBase::YRange(HCDT a,bool add,mreal fact)
{
	mreal v1=a->Minimal(), v2=a->Maximal(), dv;
	dv=(v2-v1)*fact;	v1 -= dv;	v2 += dv;
	YRange(v1,v2,add);
}
void mglBase::YRange(mreal v1,mreal v2,bool add)
{
	if(!mgl_isrange(v1,v2) && !add)	return;
	if(!add)
	{
		if(mgl_isnum(v1))	Min.y = v1;
		if(mgl_isnum(v2))	Max.y = v2;
	}
	else if(Min.y<Max.y)
	{
		if(Min.y>v1)	Min.y=v1;
		if(Max.y<v2)	Max.y=v2;
	}
	else
	{
		mreal dv = Min.y;
		Min.y = v1<Max.y ? v1:Max.y;
		Max.y = v2>dv ? v2:dv;
	}
	if(Org.y<Min.y && mgl_isnum(Org.y))	Org.y = Min.y;
	if(Org.y>Max.y && mgl_isnum(Org.y))	Org.y = Max.y;
	if((TernAxis&3)==0)
	{
		OMax.y = Max.y;	OMin.y = Min.y;
		mglScaleAxis(Min.y, Max.y, Org.y, AMin.y, AMax.y);

	}
	RecalcBorder();
}
//-----------------------------------------------------------------------------
void mglBase::ZRange(HCDT a,bool add,mreal fact)
{
	mreal v1=a->Minimal(), v2=a->Maximal(), dv;
	dv=(v2-v1)*fact;	v1 -= dv;	v2 += dv;
	ZRange(v1,v2,add);
}
void mglBase::ZRange(mreal v1,mreal v2,bool add)
{
	if(!mgl_isrange(v1,v2) && !add)	return;
	if(!add)
	{
		if(mgl_isnum(v1))	Min.z = v1;
		if(mgl_isnum(v2))	Max.z = v2;
	}
	else if(Min.z<Max.z)
	{
		if(Min.z>v1)	Min.z=v1;
		if(Max.z<v2)	Max.z=v2;
	}
	else
	{
		mreal dv = Min.z;
		Min.z = v1<Max.z ? v1:Max.z;
		Max.z = v2>dv ? v2:dv;
	}
	if(Org.z<Min.z && mgl_isnum(Org.z))	Org.z = Min.z;
	if(Org.z>Max.z && mgl_isnum(Org.z))	Org.z = Max.z;
	if((TernAxis&3)==0)
	{
		OMax.z = Max.z;	OMin.z = Min.z;
		mglScaleAxis(Min.z, Max.z, Org.z, AMin.z, AMax.z);
	}
	RecalcBorder();
}
//-----------------------------------------------------------------------------
void mglBase::SetAutoRanges(mreal x1, mreal x2, mreal y1, mreal y2, mreal z1, mreal z2, mreal c1, mreal c2)
{
	if(mgl_isrange(x1,x2))	{	Min.x = x1;	Max.x = x2;	}
	if(mgl_isrange(y1,y2))	{	Min.y = y1;	Max.y = y2;	}
	if(mgl_isrange(z1,z2))	{	Min.z = z1;	Max.z = z2;	}
	if(mgl_isrange(c1,c2))	{	Min.c = c1;	Max.c = c2;	}
}
//-----------------------------------------------------------------------------
void mglBase::Ternary(int t)
{
	static mglPoint x1(-1,-1,-1),x2(1,1,1),o(NAN,NAN,NAN);
	static bool c = true;
	TernAxis = t;
	if(t&3)
	{
		if(c)	{	x1 = Min;	x2 = Max;	o = Org;	}
		SetRanges(mglPoint(0,0,0),mglPoint(1,1,(t&3)==1?0:1));
		Org.Set(0,0,(t&3)==1?NAN:0);	c = false;
	}
	else if(!c)	{	SetRanges(x1,x2);	Org=o;	c=true;	}
}
//-----------------------------------------------------------------------------
//		Transformation functions
//-----------------------------------------------------------------------------
void mglBase::SetFunc(const char *EqX,const char *EqY,const char *EqZ,const char *EqA)
{
	if(fa)	delete fa;
	if(fx)	delete fx;
	if(fy)	delete fy;
	if(fz)	delete fz;
	if(EqX && *EqX && (EqX[0]!='x' || EqX[1]!=0))
		fx = new mglFormula(EqX);
	else	fx = 0;
	if(EqY && *EqY && (EqY[0]!='y' || EqY[1]!=0))
		fy = new mglFormula(EqY);
	else	fy = 0;
	if(EqZ && *EqZ && (EqZ[0]!='z' || EqZ[1]!=0))
		fz = new mglFormula(EqZ);
	else	fz = 0;
	if(EqA && *EqA && ((EqA[0]!='c' && EqA[0]!='a') || EqA[1]!=0))
		fa = new mglFormula(EqA);
	else	fa = 0;
	RecalcBorder();
}
//-----------------------------------------------------------------------------
void mglBase::CutOff(const char *EqC)
{
#pragma omp critical(eq)
	{
		if(fc)	delete fc;
		fc = (EqC && EqC[0])?new mglFormula(EqC):0;
	}
}
//-----------------------------------------------------------------------------
void mglBase::SetCoor(int how)
{
	switch(how)
	{
	case mglCartesian:	SetFunc(0,0);	break;
	case mglPolar:
		SetFunc("x*cos(y)","x*sin(y)");	break;
	case mglSpherical:
		SetFunc("x*sin(y)*cos(z)","x*sin(y)*sin(z)","x*cos(y)");	break;
	case mglParabolic:
		SetFunc("x*y","(x*x-y*y)/2");	break;
	case mglParaboloidal:
		SetFunc("(x*x-y*y)*cos(z)/2","(x*x-y*y)*sin(z)/2","x*y");	break;
	case mglOblate:
		SetFunc("cosh(x)*cos(y)*cos(z)","cosh(x)*cos(y)*sin(z)","sinh(x)*sin(y)");	break;
//		SetFunc("x*y*cos(z)","x*y*sin(z)","(x*x-1)*(1-y*y)");	break;
	case mglProlate:
		SetFunc("sinh(x)*sin(y)*cos(z)","sinh(x)*sin(y)*sin(z)","cosh(x)*cos(y)");	break;
	case mglElliptic:
		SetFunc("cosh(x)*cos(y)","sinh(x)*sin(y)");	break;
	case mglToroidal:
		SetFunc("sinh(x)*cos(z)/(cosh(x)-cos(y))","sinh(x)*sin(z)/(cosh(x)-cos(y))",
			"sin(y)/(cosh(x)-cos(y))");	break;
	case mglBispherical:
		SetFunc("sin(y)*cos(z)/(cosh(x)-cos(y))","sin(y)*sin(z)/(cosh(x)-cos(y))",
			"sinh(x)/(cosh(x)-cos(y))");	break;
	case mglBipolar:
		SetFunc("sinh(x)/(cosh(x)-cos(y))","sin(y)/(cosh(x)-cos(y))");	break;
	case mglLogLog:	SetFunc("lg(x)","lg(y)");	break;
	case mglLogX:	SetFunc("lg(x)","");	break;
	case mglLogY:	SetFunc("","lg(y)");	break;
	default:	SetFunc(0,0);	break;
	}
}
//-----------------------------------------------------------------------------
void mglBase::ClearEq()
{
#pragma omp critical(eq)
	{
		if(fx)	delete fx;
		if(fy)	delete fy;
		if(fz)	delete fz;
		if(fa)	delete fa;
		if(fc)	delete fc;
		fx = fy = fz = fc = fa = 0;
	}
	RecalcBorder();
}
//-----------------------------------------------------------------------------
//		Colors ids
//-----------------------------------------------------------------------------
MGL_EXPORT mglColorID mglColorIds[31] = {{'k', mglColor(0,0,0)},
	{'r', mglColor(1,0,0)},		{'R', mglColor(0.5,0,0)},
	{'g', mglColor(0,1,0)},		{'G', mglColor(0,0.5,0)},
	{'b', mglColor(0,0,1)},		{'B', mglColor(0,0,0.5)},
	{'w', mglColor(1,1,1)},		{'W', mglColor(0.7,0.7,0.7)},
	{'c', mglColor(0,1,1)},		{'C', mglColor(0,0.5,0.5)},
	{'m', mglColor(1,0,1)},		{'M', mglColor(0.5,0,0.5)},
	{'y', mglColor(1,1,0)},		{'Y', mglColor(0.5,0.5,0)},
	{'h', mglColor(0.5,0.5,0.5)},	{'H', mglColor(0.3,0.3,0.3)},
	{'l', mglColor(0,1,0.5)},	{'L', mglColor(0,0.5,0.25)},
	{'e', mglColor(0.5,1,0)},	{'E', mglColor(0.25,0.5,0)},
	{'n', mglColor(0,0.5,1)},	{'N', mglColor(0,0.25,0.5)},
	{'u', mglColor(0.5,0,1)},	{'U', mglColor(0.25,0,0.5)},
	{'q', mglColor(1,0.5,0)},	{'Q', mglColor(0.5,0.25,0)},
	{'p', mglColor(1,0,0.5)},	{'P', mglColor(0.5,0,0.25)},
	{' ', mglColor(-1,-1,-1)},	{0, mglColor(-1,-1,-1)}	// the last one MUST have id=0
};
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_chrrgb(char p, float c[3])
{
	c[0]=c[1]=c[2]=-1;
	for(long i=0; mglColorIds[i].id; i++)
		if(mglColorIds[i].id==p)
		{
			c[0]=mglColorIds[i].col.r;
			c[1]=mglColorIds[i].col.g;
			c[2]=mglColorIds[i].col.b;
			break;
		}
}
//-----------------------------------------------------------------------------
size_t MGL_EXPORT_PURE mgl_get_num_color(const char *s, int smooth)
{
	if(!s || !s[0])	return 0;
	size_t l=strlen(s), n=0;	long j=0;
	for(size_t i=0;i<l;i++)		// find number of colors
	{
		if(smooth>=0 && s[i]==':' && j<1)	break;
		if(s[i]=='{' && strchr(MGL_COLORS"x",s[i+1]) && j<1)	n++;
		if(s[i]=='[' || s[i]=='{')	j++;
		if(s[i]==']' || s[i]=='}')	j--;
		if(strchr(MGL_COLORS,s[i]) && j<1)	n++;
//		if(smooth && s[i]==':')	break;	// NOTE: should use []
	}
	return n;
}
//-----------------------------------------------------------------------------
void mglTexture::Set(const char *s, int smooth, mreal alpha)
{
	// NOTE: New syntax -- colors are CCCCC or {CNCNCCCN}; options inside []
	if(!s || !s[0])	return;
	mgl_strncpy(Sch,s,259);	Smooth=smooth;	Alpha=alpha;	Clear();

	long l=strlen(s);
	bool map = smooth==2 || mglchr(s,'%'), sm = smooth>=0 && !strchr(s,'|');	// Use mapping, smoothed colors
	n = mgl_get_num_color(s,smooth);
	if(!n)
	{
		if(strchr(s,'|') && !smooth)	// sharp colors
		{	n=l=6;	s=MGL_DEF_SCH;	sm = false;	}
		else if(smooth==0)		// none colors but color scheme
		{	n=l=6;	s=MGL_DEF_SCH;	}
	}
	if(n<=0)	return;
	bool man=sm;
	c0 = new mglColor[2*n];	// Colors itself
	val = new float[n];
	for(long i=0, m=0, j=n=0;i<l;i++)	// fill colors
	{
		if(smooth>=0 && s[i]==':' && j<1)	break;
		if(s[i]=='[')	j++;
		if(s[i]==']')	j--;
		if(s[i]=='{')	m++;
		if(s[i]=='}')	m--;
		if(strchr(MGL_COLORS,s[i]) && j<1 && (m==0 || s[i-1]=='{'))	// {CN,val} format, where val in [0,1]
		{
			if(m>0 && s[i+1]>'0' && s[i+1]<='9')// ext color
			{	c0[2*n].Set(s[i],(s[i+1]-'0')/5.f);	i++;	}
			else	c0[2*n].Set(s[i]);	// usual color
			val[n]=-1;	c0[2*n].a = -1;	n++;
		}
		if(s[i]=='x' && i>0 && s[i-1]=='{' && j<1)	// {xRRGGBB,val} format, where val in [0,1]
		{
			uint32_t id = strtoul(s+1+i,0,16);
			if(memchr(s+i+1,'}',8) || memchr(s+i+1,',',8))	c0[2*n].a = -1;
			else	{	c0[2*n].a = (id%256)/255.;	id /= 256;	}
			c0[2*n].b = (id%256)/255.;	id /= 256;
			c0[2*n].g = (id%256)/255.;	id /= 256;
			c0[2*n].r = (id%256)/255.;
			while(strchr("0123456789abcdefABCDEFx",s[i]))	i++;
			val[n]=-1;	n++;	i--;
		}
		if(s[i]==',' && m>0 && j<1 && n>0)
			val[n-1] = atof(s+i+1);
		// NOTE: User can change alpha if it placed like {AN}
		if(s[i]=='A' && j<1 && m>0 && s[i+1]>'0' && s[i+1]<='9')
		{	man=false;	alpha = 0.1*(s[i+1]-'0');	i++;	}
	}
	for(long i=0;i<n;i++)	// default texture
	{
		if(c0[2*i].a<0)	c0[2*i].a=alpha;
		c0[2*i+1]=c0[2*i];
		if(man)	c0[2*i].a=0;
	}
	if(map && sm && n>1)		// map texture
	{
		if(n==2)
		{	c0[1]=c0[2];	c0[2]=c0[0];	c0[0]=BC;	c0[3]=c0[1]+c0[2];	}
		else if(n==3)
		{	c0[1]=c0[2];	c0[2]=c0[0];	c0[0]=BC;	c0[3]=c0[4];	n=2;}
		else
		{	c0[1]=c0[4];	c0[3]=c0[6];	n=2;	}
		c0[0].a = c0[1].a = c0[2].a = c0[3].a = alpha;
		val[0]=val[1]=-1;
	}
	// TODO if(!sm && n==1)	then try to find color in palette ???

	// fill missed values  of val[]
	float  v1=0,v2=1;
	std::vector <long>  def;
	val[0]=0;	val[n-1]=1;	// boundary have to be [0,1]
	for(long i=0;i<n;i++) if(val[i]>0 && val[i]<1) 	def.push_back(i);
	def.push_back(n-1);
	long i1=0;
	for(size_t j=0;j<def.size();j++)	for(long i=i1+1;i<def[j];i++)
	{
		i1 = j>0?def[j-1]:0;	long i2 = def[j];
		v1 = val[i1];	v2 = val[i2];
		v2 = i2-i1>1?(v2-v1)/(i2-i1):0;
		val[i]=v1+v2*(i-i1);
	}
	// fill texture itself
	mreal v=sm?(n-1)/255.:n/256.;
	if(!sm)	for(long i=0;i<256;i++)
	{
		long j = 2*long(v*i);	//u-=j;
		col[2*i] = c0[j];	col[2*i+1] = c0[j+1];
	}
	else	for(long i=i1=0;i<256;i++)
	{
		mreal u = v*i;	long j = long(u);	//u-=j;
		if(j<n-1)	// advanced scheme using val
		{
			for(;i1<n-1 && i>=255*val[i1];i1++);
			v2 = i1<n?1/(val[i1]-val[i1-1]):0;
			j=i1-1;	u=(i/255.-val[j])*v2;
			col[2*i] = c0[2*j]*(1-u)+c0[2*j+2]*u;
			col[2*i+1]=c0[2*j+1]*(1-u)+c0[2*j+3]*u;
		}
		else
		{	col[2*i] = c0[2*n-2];col[2*i+1] = c0[2*n-1];	}
	}
}
//-----------------------------------------------------------------------------
mglColor mglTexture::GetC(mreal u,mreal v) const
{
	u -= long(u);
	long i=long(255*u);	u = u*255-i;
	const mglColor *s=col+2*i;
	return (s[0]*(1-u)+s[2]*u)*(1-v) + (s[1]*(1-u)+s[3]*u)*v;
}
//-----------------------------------------------------------------------------
void mglTexture::GetC(mreal u,mreal v,mglPnt &p) const
{
	u -= long(u);
	long i=long(255*u);	u = u*255-i;
	const mglColor &s0=col[2*i], &s1=col[2*i+1], &s2=col[2*i+2], &s3=col[2*i+3];
	p.r = (s0.r*(1-u)+s2.r*u)*(1-v) + (s1.r*(1-u)+s3.r*u)*v;
	p.g = (s0.g*(1-u)+s2.g*u)*(1-v) + (s1.g*(1-u)+s3.g*u)*v;
	p.b = (s0.b*(1-u)+s2.b*u)*(1-v) + (s1.b*(1-u)+s3.b*u)*v;
	p.a = (s0.a*(1-u)+s2.a*u)*(1-v) + (s1.a*(1-u)+s3.a*u)*v;
//	p.a = (s0.a*(1-u)+s2.a*u)*v + (s1.a*(1-u)+s3.a*u)*(1-v);	// for alpha use inverted
}
//-----------------------------------------------------------------------------
long mglBase::AddTexture(const char *cols, int smooth)
{
	if(smooth>=0)	SetMask(cols);
	mglTexture t(cols,smooth,smooth==2?AlphaDef:1);
	if(t.n==0)	return smooth<0 ? 0:1;
	if(smooth<0)	CurrPal=0;
	// check if already exist
	for(size_t i=0;i<Txt.size();i++)
		if(!t.IsSame(Txt[i]))	continue;	else	return i;
	// create new one
	long k;
#pragma omp critical(txt)
	{k=Txt.size();	MGL_PUSH(Txt,t,mutexTxt);}	return k;
}
//-----------------------------------------------------------------------------
mreal mglBase::AddTexture(mglColor c)
{
	if(!c.Valid())	return -1;
	// first lets try an existed one
	for(size_t i=0;i<Txt.size();i++)	for(int j=0;j<255;j++)
		if(c==Txt[i].col[2*j])
			return i+j/255.;
	// add new texture
	mglTexture t;
	for(long i=0;i<MGL_TEXTURE_COLOURS;i++)	t.col[i]=c;
	long k;
#pragma omp critical(txt)
	{k=Txt.size();	MGL_PUSH(Txt,t,mutexTxt);}	return k;
}
//-----------------------------------------------------------------------------
//		Coloring and palette
//-----------------------------------------------------------------------------
mreal mglBase::NextColor(long &id)
{
	long i=labs(id)/256, n=Txt[i].n, p=labs(id)&0xff;
	if(id>=0)	{	p=(p+1)%n;	id = 256*i+p;	}
	CDef = i + (n>0 ? (p+0.5)/n : 0);	CurrPal++;
	sprintf(last_style+11,"{&%g}",CDef);
	if(!leg_str.empty())
	{	AddLegend(leg_str.c_str(),last_style);	leg_str.clear();	}
	return CDef;
}
//-----------------------------------------------------------------------------
mreal mglBase::NextColor(long id, long sh)
{
	long i=labs(id)/256, n=Txt[i].n, p=labs(id)&0xff;
	if(id>=0)	p=(p+sh)%n;
	mreal cc = i + (n>0 ? (p+0.5)/n : 0);
	sprintf(last_style+11,"{&%g}",cc);
	return cc;
}
//-----------------------------------------------------------------------------
MGL_EXPORT_PURE const char *mglchrs(const char *str, const char *chr)
{
	if(!str || !str[0] || !chr || !chr[0])	return NULL;
	size_t l=strlen(chr);
	for(size_t i=0;i<l;i++)
	{
		const char *res = mglchr(str,chr[i]);
		if(res)	return res;
	}
	return NULL;
}
//-----------------------------------------------------------------------------
MGL_EXPORT_PURE const char *mglchr(const char *str, char ch)
{
	if(!str || !str[0])	return NULL;
	size_t l=strlen(str),k=0;
	for(size_t i=0;i<l;i++)
	{
		char c = str[i];
		if(c=='{')	k++;
		if(c=='}')	k--;
		if(c==ch && k==0)	return str+i;
	}
	return NULL;
}
//-----------------------------------------------------------------------------
char mglBase::SetPenPal(const char *p, long *Id, bool pal)
{
	char mk=0;
	PDef = 0xffff;	// reset to solid line
	strcpy(last_style,"__1 {dFFFF}k\0");

	const char *s;
	Arrow1 = Arrow2 = 0;	PenWidth = 1;
	if(p && *p)
	{
//		const char *col = "wkrgbcymhRGBCYMHWlenuqpLENUQP";
		unsigned val[8] = {0x0000, 0xffff, 0x00ff, 0x0f0f, 0x1111, 0x087f, 0x2727, 0x3333};
		const char *stl = " -|;:ji=";
		const char *mrk = "*o+xsd.^v<>";
		const char *MRK = "YOPXSDCTVLR";
		const char *wdh = "123456789";
		const char *arr = "AKDTVISOX_";
		long m=0;
		size_t l=strlen(p);
		for(size_t i=0;i<l;i++)
		{
			if(p[i]=='{')	m++;
			if(p[i]=='}')	m--;
			if(m>0 && p[i]=='d')	PDef = strtol(p+i+1,0,16);
			if(m>0)	continue;
			s = mglchr(stl,p[i]);
			if(s)
			{	PDef = val[s-stl];	sprintf(last_style+6,"%04x",PDef);	last_style[10]='}';	}
			else if(mglchr(mrk,p[i]))
			{	mk = p[i];	last_style[3] = mk;	}
			else if(mglchr(wdh,p[i]))
			{	PenWidth = p[i]-'0';	last_style[2] = p[i];	}
			else if(mglchr(arr,p[i]))
			{
				if(!Arrow2)	Arrow2 = p[i];
				else	Arrow1 = p[i];
			}
		}
		if(!Arrow1)	Arrow1='_';
		if(!Arrow2)	Arrow2='_';
		if(mglchr(p,'#'))
		{
			s = mglchr(mrk,mk);
			if(s)
			{	mk = MRK[s-mrk];	last_style[3] = mk;	}
		}
		if((s=strstr(p,"{&"))!=0)
		{	mk = last_style[3] = p[3];	strcpy(last_style+11,s);	}
		else if(mk && mglchr(p,'&'))
		{	mk += 128;	last_style[3] = mk;	}
		last_style[0] = Arrow1;	last_style[1] = Arrow2;
	}
	if(pal)
	{
		if(p && (s=strstr(p,"{&"))!=0)
		{
			CDef = atof(s+2);
//			if(Id)	*Id=long(tt)*256+(n+CurrPal-1)%n;
		}
		else
		{
			long tt, n;
			tt = AddTexture(p?p:MGL_DEF_PAL,-1);	n=Txt[tt].n;
			CDef = tt+((n+CurrPal-1)%n+0.5)/n;
			if(Id)	*Id=long(tt)*256+(n+CurrPal-1)%n;
			sprintf(last_style+11,"{&%g}",CDef);
		}
	}
	if(Arrow1=='_')	Arrow1=0;
	if(Arrow2=='_')	Arrow2=0;
	return mk;
}
//-----------------------------------------------------------------------------
// keep this for restore default mask
MGL_EXPORT uint64_t mgl_mask_def[16]={
	0x000000FF00000000,	0x080808FF08080808,	0x0000FF00FF000000,	0x0000000F00000000,
	0x0000182424180000,	0x0000183C3C180000,	0x00003C24243C0000,	0x00003C3C3C3C0000,
	0x0000060990600000,	0x0060584658600000,	0x00061A621A060000,	0x0000002700000000,
	0x0008083E08080000,	0x0139010010931000,	0x0000001818000000,	0x101010FF010101FF};
MGL_EXPORT uint64_t mgl_mask_val[16]={
	0x000000FF00000000,	0x080808FF08080808,	0x0000FF00FF000000,	0x0000000F00000000,
	0x0000182424180000,	0x0000183C3C180000,	0x00003C24243C0000,	0x00003C3C3C3C0000,
	0x0000060990600000,	0x0060584658600000,	0x00061A621A060000,	0x0000002700000000,
	0x0008083E08080000,	0x0139010010931000,	0x0000001818000000,	0x101010FF010101FF};
// 	0x000000FF00000000,	0x080808FF08080808,	0x0000FF00FF000000,	0x0000007700000000,
// 	0x0000182424180000,	0x0000183C3C180000,	0x00003C24243C0000,	0x00003C3C3C3C0000,
// 	0x0000060990600000,	0x0060584658600000,	0x00061A621A060000,	0x0000005F00000000,
//	0x0008142214080000,	0x00081C3E1C080000,	0x8142241818244281,	0x0000001824420000};
void mglBase::SetMask(const char *p)
{
	mask = MGL_SOLID_MASK;	// reset to solid face
	PenWidth = 1;	MaskAn=DefMaskAn;
	if(p && *p)
	{
		const char *msk = MGL_MASK_ID, *s;
		const char *wdh = "123456789";
		long m=0, l=strlen(p);
		for(long i=0;i<l;i++)
		{
			if(p[i]=='{')	m++;
			if(p[i]=='}')	m--;
			if(m>0 && p[i]=='s')	mask = strtoull(p+i+1,0,16);
			if(m>0)	continue;
			if(p[i]==':')	break;
			s = mglchr(msk, p[i]);
			if(s)	mask = mgl_mask_val[s-msk];
			else if(mglchr(wdh,p[i]))	PenWidth = p[i]-'0';
			else if(p[i]=='I')	MaskAn=90;
			else if(p[i]=='/')	MaskAn=315;	// =360-45
			else if(p[i]=='\\')	MaskAn=45;
		}
		// use line if rotation only specified
		if(mask==MGL_SOLID_MASK && MaskAn!=0)	mask = mgl_mask_val[0];
	}
}
//-----------------------------------------------------------------------------
mreal mglBase::GetA(mreal a) const
{
	if(fa)	a = fa->Calc(0,0,0,a);
	a = (a-FMin.c)/(FMax.c-FMin.c);
	a = (a>1?1:(a<0?0:a))/MGL_FEPSILON;	// for texture a must be <1 always!!!
//	a = (a<1?(a>0?a:0):1)/MGL_FEPSILON;	// for texture a must be <1 always!!!
	return a;
}
//-----------------------------------------------------------------------------
mglPoint GetX(HCDT x, int i, int j, int k)
{
	k = k<x->GetNz() ? k : 0;
	if(x->GetNy()>1)
		return mglPoint(x->v(i,j,k),x->dvx(i,j,k),x->dvy(i,j,k));
	else
		return mglPoint(x->v(i),x->dvx(i),0);
}
//-----------------------------------------------------------------------------
mglPoint GetY(HCDT y, int i, int j, int k)
{
	k = k<y->GetNz() ? k : 0;
	if(y->GetNy()>1)
		return mglPoint(y->v(i,j,k),y->dvx(i,j,k),y->dvy(i,j,k));
	else
		return mglPoint(y->v(j),0,y->dvx(j));
}
//-----------------------------------------------------------------------------
mglPoint GetZ(HCDT z, int i, int j, int k)
{
	if(z->GetNy()>1)
		return mglPoint(z->v(i,j,k),z->dvx(i,j,k),z->dvy(i,j,k));
	else
		return mglPoint(z->v(k),0,0);
}
//-----------------------------------------------------------------------------
void mglBase::vect_plot(long p1, long p2, mreal s)
{
	if(p1<0 || p2<0)	return;
	const mglPnt &q1=Pnt[p1], &q2=Pnt[p2];
	mglPnt s1=q2,s2=q2;
	s = s<=0 ? 0.1 : s*0.1;
	s1.x=s1.xx = q2.x - 3*s*(q2.x-q1.x) + s*(q2.y-q1.y);
	s2.x=s2.xx = q2.x - 3*s*(q2.x-q1.x) - s*(q2.y-q1.y);
	s1.y=s1.yy = q2.y - 3*s*(q2.y-q1.y) - s*(q2.x-q1.x);
	s2.y=s2.yy = q2.y - 3*s*(q2.y-q1.y) + s*(q2.x-q1.x);
	s1.z=s1.zz=s2.z=s2.zz = q2.z - 3*s*(q2.z-q1.z);
	long n1,n2;
#pragma omp critical(pnt)
	{
		n1=Pnt.size();	MGL_PUSH(Pnt,s1,mutexPnt);
		n2=Pnt.size();	MGL_PUSH(Pnt,s2,mutexPnt);
	}
	line_plot(p1,p2);	line_plot(n1,p2);	line_plot(p2,n2);
}
//-----------------------------------------------------------------------------
int MGL_LOCAL_PURE mglFindArg(const char *str)
{
	long l=0,k=0,len=strlen(str);
	for(long i=0;i<len;i++)
	{
		if(str[i]=='\'') l++;
		if(str[i]=='{') k++;
		if(str[i]=='}') k--;
		if(l%2==0 && k==0)
		{
			if(str[i]=='#' || str[i]==';')	return -i;
			if(str[i]<=' ')	return i;
		}
	}
	return 0;
}
//-----------------------------------------------------------------------------
void mglBase::SetAmbient(mreal bright)
{	AmbBr=bright;	size_t n=Sub.size();	if(n>0)	Sub[n-1].AmbBr=bright;	}
void mglBase::SetDiffuse(mreal bright)
{	DifBr=bright;	size_t n=Sub.size();	if(n>0)	Sub[n-1].DifBr=bright;	}
//-----------------------------------------------------------------------------
mreal mglBase::SaveState(const char *opt)
{
	if(saved)	return PrevState;
	if(!opt || !opt[0])	return NAN;
	MSS=MarkSize;	ASS=ArrowSize;
	FSS=FontSize;	ADS=AlphaDef;
	MNS=MeshNum;	CSS=Flag;	LSS=AmbBr;
	MinS=Min;		MaxS=Max;	saved=true;
	// parse option
	char *qi=mgl_strdup(opt),*q=qi, *s;
	mgl_strtrim(q);
	// NOTE: not consider '#' inside legend entry !!!
	s=strchr(q,'#');	if(s)	*s=0;
	mreal res=NAN;
	while(q && *q)
	{
		s=q;	q=strchr(s,';');
		if(q)	{	*q=0;	q++;	}
		mgl_strtrim(s);		char *a=s;
		long n=mglFindArg(s);	if(n>0)	{	s[n]=0;		s=s+n+1;	}
		mgl_strtrim(a);		char *b=s;
		n=mglFindArg(s);	if(n>0)	{	s[n]=0;		s=s+n+1;	}
		mgl_strtrim(b);

		mreal ff=atof(b),ss;
		size_opt = NAN;
		if(!strcmp(b,"on"))	ff=1;
		if(!strcmp(a+1,"range"))
		{
			n=mglFindArg(s);	char *c=s;
			if(n>0)	{	s[n]=0;	s=s+n+1;	}
			mgl_strtrim(c);		ss = atof(c);
			if(a[0]=='x')		{	Min.x=ff;	Max.x=ss;	}
			else if(a[0]=='y')	{	Min.y=ff;	Max.y=ss;	}
			else if(a[0]=='z')	{	Min.z=ff;	Max.z=ss;	}
//			else if(a[0]=='c')	{	Min.c=ff;	Max.c=ss;	}	// Bad idea since there is formula for coloring
		}
		else if(!strcmp(a,"cut"))		SetCut(ff!=0);
		else if(!strcmp(a,"meshnum"))	SetMeshNum(ff);
		else if(!strcmp(a,"alpha"))		{Alpha(true);	SetAlphaDef(ff);}
		else if(!strcmp(a,"light"))		Light(ff!=0);
		else if(!strcmp(a,"ambient"))	SetAmbient(ff);
		else if(!strcmp(a,"diffuse"))	SetDifLight(ff);
		else if(!strcmp(a,"size"))
		{	SetMarkSize(ff);	SetFontSize(ff);	SetArrowSize(ff);	size_opt=ff;	}
		else if(!strcmp(a,"num") || !strcmp(a,"number") || !strcmp(a,"value"))	res=ff;
		else if(!strcmp(a,"legend"))
		{	if(*b=='\'')	{	b++;	b[strlen(b)-1]=0;	}	leg_str = b;	}
	}
	free(qi);	PrevState=res;	return res;
}
//-----------------------------------------------------------------------------
void mglBase::LoadState()
{
	if(!saved)	return;
	MarkSize=MSS;	ArrowSize=ASS;
	FontSize=FSS;	AlphaDef=ADS;
	MeshNum=MNS;	Flag=CSS;	AmbBr=LSS;
	Min=MinS;		Max=MaxS;	saved=false;
}
//-----------------------------------------------------------------------------
void mglBase::AddLegend(const wchar_t *text,const char *style)
{
	if(text)
#pragma omp critical(leg)
		MGL_PUSH(Leg,mglText(text,style),mutexLeg);
}
//-----------------------------------------------------------------------------
void mglBase::AddLegend(const char *str,const char *style)
{
	MGL_TO_WCS(str,AddLegend(wcs, style));
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_check_dim2(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT a, const char *name, bool less)
{
//	if(!gr || !x || !y || !z)	return true;		// if data is absent then should be segfault!!!
	long n=z->GetNx(),m=z->GetNy();
	if(n<2 || m<2)	{	gr->SetWarn(mglWarnLow,name);	return true;	}
	if(a && z->GetNN()!=a->GetNN())
	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	if(less)
	{
		if(x->GetNx()<n)
		{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(y->GetNx()<m && (x->GetNy()<m || y->GetNx()<n || y->GetNy()<m))
		{	gr->SetWarn(mglWarnDim,name);	return true;	}
	}
	else
	{
		if(x->GetNx()!=n)
		{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(y->GetNx()!=m && (x->GetNy()!=m || y->GetNx()!=n || y->GetNy()!=m))
		{	gr->SetWarn(mglWarnDim,name);	return true;	}
	}
	return false;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_check_dim0(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT r, const char *name, bool less)
{
//	if(!gr || !x || !y)	return true;		// if data is absent then should be segfault!!!
	long n=y->GetNx();
	if(less)
	{
		if(x->GetNx()<n)		{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(z && z->GetNx()<n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(r && r->GetNx()<n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	}
	else
	{
		if(x->GetNx()!=n)		{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(z && z->GetNx()!=n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(r && r->GetNx()!=n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	}
	return false;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_check_dim1(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT r, const char *name, bool less)
{
//	if(!gr || !x || !y)	return true;		// if data is absent then should be segfault!!!
	long n=y->GetNx();
	if(n<2)	{	gr->SetWarn(mglWarnLow,name);	return true;	}
	if(less)
	{
		if(x->GetNx()<n)		{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(z && z->GetNx()<n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(r && r->GetNx()<n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	}
	else
	{
		if(x->GetNx()!=n)		{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(z && z->GetNx()!=n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
		if(r && r->GetNx()!=n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	}
	return false;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_check_dim3(HMGL gr, bool both, HCDT x, HCDT y, HCDT z, HCDT a, HCDT b, const char *name)
{
// 	if(!gr || !x || !y || !z || !a)	return true;		// if data is absent then should be segfault!!!
	long n=a->GetNx(),m=a->GetNy(),l=a->GetNz();
	if(n<2 || m<2 || l<2)
	{	gr->SetWarn(mglWarnLow,name);	return true;	}
	if(!both && (x->GetNx()!=n || y->GetNx()!=m || z->GetNx()!=l))
	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	if(b && b->GetNN()!=n*m*l)
	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	return false;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_check_trig(HMGL gr, HCDT nums, HCDT x, HCDT y, HCDT z, HCDT a, const char *name, int d)
{
// 	if(!gr || !x || !y || !z || !a || !nums)	return true;		// if data is absent then should be segfault!!!
	long n = x->GetNN(), m = nums->GetNy();
	if(nums->GetNx()<d)	{	gr->SetWarn(mglWarnLow,name);	return true;	}
	if(y->GetNN()!=n || z->GetNN()!=n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	if(a->GetNN()!=m && a->GetNN()!=n)	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	return false;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_isnboth(HCDT x, HCDT y, HCDT z, HCDT a)
{
	long n=a->GetNN();
	return x->GetNN()!=n || y->GetNN()!=n || z->GetNN()!=n;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_isboth(HCDT x, HCDT y, HCDT z, HCDT a)
{
	long n=a->GetNN();
	return x->GetNN()==n && y->GetNN()==n && z->GetNN()==n;
}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_check_vec3(HMGL gr, HCDT x, HCDT y, HCDT z, HCDT ax, HCDT ay, HCDT az, const char *name)
{
// 	if(!gr || !x || !y || !z || !ax || !ay || !az)	return true;		// if data is absent then should be segfault!!!
	long n=ax->GetNx(),m=ax->GetNy(),l=ax->GetNz(), nn=n*m*l;
	if(nn!=ay->GetNN() || nn!=az->GetNN())
	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	if(n<2 || m<2 || l<2)	{	gr->SetWarn(mglWarnLow,name);	return true;	}
	bool both = x->GetNN()==nn && y->GetNN()==nn && z->GetNN()==nn;
	if(!(both || (x->GetNx()==n && y->GetNx()==m && z->GetNx()==l)))
	{	gr->SetWarn(mglWarnDim,name);	return true;	}
	return false;
}
//-----------------------------------------------------------------------------
void mglBase::ClearUnused()
{
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexPnt);	pthread_mutex_lock(&mutexPrm);
#endif
#pragma omp critical
	{
		size_t l=Prm.size();
		// find points which are actually used
		long *used = new long[Pnt.size()];	memset(used,0,Pnt.size()*sizeof(long));
		for(size_t i=0;i<l;i++)
		{
			const mglPrim &p=Prm[i];
			if(p.n1<0)	continue;
			used[p.n1] = 1;
			switch(p.type)
			{
			case 1:	case 4:	if(p.n2>=0)	used[p.n2] = 1;
				break;
			case 2:	if(p.n2>=0 && p.n3>=0)	used[p.n2] = used[p.n3] = 1;
				break;
			case 3:	if(p.n2>=0 && p.n3>=0 && p.n4>=0)
					used[p.n2] = used[p.n3] = used[p.n4] = 1;
				break;
			}
		}
		// now add proper indexes
		l=Pnt.size();
		mglStack<mglPnt> pnt;	pnt.reserve(l);
		for(size_t i=0;i<l;i++)	if(used[i])
		{	pnt.push_back(Pnt[i]);	used[i]=pnt.size()-1;	}
		Pnt = pnt;	pnt.clear();
		// now replace point id
		l=Prm.size();
		for(size_t i=0;i<l;i++)
		{
			mglPrim &p=Prm[i];	p.n1=used[p.n1];
			if(p.type==1 || p.type==4)	p.n2=used[p.n2];
			if(p.type==2)	{	p.n2=used[p.n2];	p.n3=used[p.n3];	}
			if(p.type==3)	{	p.n2=used[p.n2];	p.n3=used[p.n3];	p.n4=used[p.n4];	}
		}
		delete []used;
	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexPnt);	pthread_mutex_unlock(&mutexPrm);
#endif
}
//-----------------------------------------------------------------------------
void mglBase::ClearPrmInd()
{
#pragma omp critical(prmind)
	{	if(PrmInd)	delete []PrmInd;	PrmInd=NULL;	}
}
//-----------------------------------------------------------------------------
void mglBase::curve_plot(size_t num, size_t k0, size_t step)
{
	// exclude straight-line parts
	if(get(MGL_FULL_CURV))	for(size_t i=0;i+1<num;i++)
		line_plot(k0+i*step,k0+(i+1)*step);
	else	for(size_t i=0;i+1<num;i++)
	{
		const mglPoint p1(GetPntP(k0+i*step)), ps(GetPntP(k0+(i+1)*step));
		if(mgl_isnan(p1.x) || mgl_isnan(ps.x))	continue;
		const mglColor c1(GetPntC(k0+i*step));
		// remove duplicates
		for(;i+1<num;i++)
		{
			size_t ii = k0+(i+1)*step;
			const mglPoint pp(GetPntP(ii));
			if(p1!=pp || mgl_isnan(pp.x))	break;
		}
		if(i+1>=num)	break;

		float t1=-100, t2=100;		// XY angle boundary
		float rg1=-100, rg2=100;	// RG angle boundary
		float gb1=-100, gb2=100;	// GB angle boundary
		size_t k;
		for(k=i+1;k<num;k++)
		{
			const mglPoint p2(GetPntP(k0+k*step)-p1);
			if(mgl_isnan(p2.x))	break;
			float dd=p2.x*p2.x+p2.y*p2.y+p2.z*p2.z;
			if(dd<=0)	continue;	// the same point (micro-loop? :) )
			float t = atan2(p2.y,p2.x), d = atan(0.03/dd);
			if(t1 > t+d || t2 < t-d)	break;		// too curved
			const mglColor c2(GetPntC(k0+(k-1)*step)-c1);	dd = c2.NormS();
			if(dd>0)	// color are different
			{
				float rg = atan2(c2.r,c2.g), gb = atan2(c2.g,c2.b);	d = atan(1e-4/dd);
				if(rg1 > rg+d || rg2 < rg-d || gb1 > gb+d || gb2 < gb-d)	break;		// too curved
				rg1 = rg1<rg-d?rg-d:rg1;	rg2 = rg2>rg+d?rg+d:rg2;	// new RG range
				gb1 = gb1<gb-d?gb-d:gb1;	gb2 = gb2>gb+d?gb+d:gb2;	// new GB range
			}
			t1 = t1<t-d?t-d:t1;	t2 = t2>t+d?t+d:t2;	// new range
		}
		k--;	line_plot(k0+i*step,k0+k*step);	i = k-1;
	}
}
//-----------------------------------------------------------------------------
