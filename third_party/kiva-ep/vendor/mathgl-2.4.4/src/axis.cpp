/***************************************************************************
 * axis.cpp is part of Math Graphic Library
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
#include <time.h>
#include <ctype.h>
#include "mgl2/data.h"
#include "mgl2/canvas.h"
#include "mgl2/prim.h"
#include "mgl2/eval.h"
std::wstring MGL_EXPORT mgl_ftoa(double v, const char *fmt);
//-----------------------------------------------------------------------------
MGL_NO_EXPORT inline struct tm *mgl_localtime (const time_t *clock, tm *result, bool use_utc)
{	if (!clock || !result) return NULL;
	const tm *res = use_utc?gmtime(clock):localtime(clock);
	memcpy(result,res,sizeof(tm));	return result;	}
//-----------------------------------------------------------------------------
long MGL_EXPORT_PURE mgl_have_color(const char *stl)
{
	return mgl_get_num_color(stl,0);
// 	long j=0;
// 	if(stl)	for(long i=0;stl[i];i++)
// 	{
// 		if(strchr(MGL_COLORS,stl[i]))	j++;
// 		if(stl[i]=='{' && stl[i+1]=='x')	j++;
// 	}
// 	return j;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_wcstrim(wchar_t *str)
{
	if(!str || *str==0)	return;
	size_t n=mgl_wcslen(str), k, i;
	for(k=0;k<n;k++)	if(str[k]>' ')	break;
	for(i=n;i>k;i--)	if(str[i-1]>' ')	break;
	for(size_t j=0;j<i-k;j++)	str[j]=str[j+k];
	str[i-k]=0;
}
//-----------------------------------------------------------------------------
//		Ticks setup
//-----------------------------------------------------------------------------
void mglCanvas::SetAxisStl(const char *stl, const char *tck, const char *sub)
{
	if(!stl || !(*stl))	mgl_strncpy(AxisStl,"k",32);
	else 				mgl_strncpy(AxisStl,stl,32);
	if(!tck || !(*tck))	mgl_strncpy(TickStl,AxisStl,32);
	else 				mgl_strncpy(TickStl,tck,32);
	if(!sub || !(*sub))	mgl_strncpy(SubTStl,TickStl,32);
	else 				mgl_strncpy(SubTStl,sub,32);
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTickLen(mreal tlen, mreal stt)
{	TickLen = tlen?tlen:0.02;	st_t=stt>0?stt:1;	}
//-----------------------------------------------------------------------------
void mglCanvas::SetTicks(char dir, mreal d, int ns, mreal org, const wchar_t *lbl)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = GetAxis(dir);

	if(aa.f==1)	aa.t.clear();
	aa.d=d;	aa.f=0;	aa.ns=ns;	aa.o=org;
	aa.txt.clear();
	if(!lbl || *lbl==0)	aa.fact.clear();
	else	aa.fact = lbl;
}
//-----------------------------------------------------------------------------
void mglCanvas::AddTick(char dir, double v, const wchar_t *lbl)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = GetAxis(dir);
	bool ff = GetFormula(dir);

	UpdateAxis();	AdjustTicks(aa,ff);
	if(!v || !lbl || !lbl[0])	{	aa.f = 0;	return;	}
	aa.f = 2;	aa.ns=0;	aa.ds=0;
	aa.AddLabel(lbl,v);
}
//-----------------------------------------------------------------------------
void mglCanvas::AddTick(char dir, double v, const char *lbl)
{	MGL_TO_WCS(lbl,AddTick(dir,v,wcs));	}
//-----------------------------------------------------------------------------
void mglCanvas::SetTicksVal(char dir, HCDT v, const wchar_t *lbl, bool add)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = GetAxis(dir);
	bool ff = GetFormula(dir);

	if(add)	{	UpdateAxis();	AdjustTicks(aa,ff);	}
	else	aa.txt.clear();
	if(!v || !lbl || !lbl[0])	{	aa.f = 0;	return;	}
	aa.f = 2;	aa.ns=0;	aa.ds=0;
	long i=0,l=0,n=v->GetNx();
	for(long j=0;i<n && lbl[j];j++)
	{
		if(lbl[j]=='\n')
		{
			aa.AddLabel(std::wstring(lbl+l,j-l),v->v(i));
			i++;	l=j+1;
		}
		if(j>1 && lbl[j]=='n' && lbl[j-1]=='\\')
		{
			aa.AddLabel(std::wstring(lbl+l,j-l-1),v->v(i));
			i++;	l=j+1;
		}
	}
	if(i<n && lbl[l])	aa.AddLabel(lbl+l,v->v(i));
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTicksVal(char dir, HCDT v, const char *lbl, bool add)
{	MGL_TO_WCS(lbl,SetTicksVal(dir,v,wcs,add));	}
//-----------------------------------------------------------------------------
void mglCanvas::SetTicksVal(char dir, const wchar_t *lbl, bool add)
{
	long i=0,len=mgl_wcslen(lbl);
	for(long j=1;j<len;j++)
		if(lbl[j]=='\n' || (lbl[j]=='n' && lbl[j-1]=='\\'))	i++;
	if(i>63)	i=63;
	mglData val(i+1);	val.Fill(Min.x,Max.x);
	SetTicksVal(dir, &val, lbl, add);
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTicksVal(char dir, const char *lbl, bool add)
{
	long i=0,len=strlen(lbl);
	for(long j=1;j<len;j++)
		if(lbl[j]=='\n' || (lbl[j]=='n' && lbl[j-1]=='\\'))	i++;
	if(i>63)	i=63;
	mglData val(i+1);	val.Fill(Min.x,Max.x);
	SetTicksVal(dir, &val, lbl, add);
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTicksVal(char dir, HCDT v, const wchar_t **lbl, bool add)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = GetAxis(dir);
	bool ff = GetFormula(dir);

	if(add)	{	UpdateAxis();	AdjustTicks(aa,ff);	}
	else	aa.txt.clear();
	if(!v || !lbl)	{	aa.f = 0;	return;	}
	aa.f = 2;	aa.ns=0;	aa.ds=0;
	long n=v->GetNx();
	for(long i=0;i<n;i++)	aa.AddLabel(lbl[i],v->v(i));
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTicksVal(char dir, HCDT v, const char **lbl, bool add)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = GetAxis(dir);
	bool ff = GetFormula(dir);

	aa.txt.clear();
	if(add)	{	UpdateAxis();	AdjustTicks(aa,ff);	}
	if(!v || !lbl)	{	aa.f = 0;	return;	}
	aa.f = 2;	aa.ns=0;	aa.ds=0;
	for(long i=0;i<v->GetNx();i++)	MGL_TO_WCS(lbl[i],aa.AddLabel(wcs,v->v(i)));
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTickTempl(char dir, const wchar_t *t)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = GetAxis(dir);

	if(aa.f==1)	aa.f = 0;	// remove time ticks
	if(!t || !t[0])	aa.t.clear();	else aa.t=t;
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTickTempl(char dir, const char *t)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = GetAxis(dir);

	if(aa.f==1)	aa.f = 0;	// remove time ticks
	if(!t || !t[0])	aa.t.clear();
	else	MGL_TO_WCS(t,aa.t=wcs);
}
//-----------------------------------------------------------------------------
static double mgl_adj_val(double v,mreal *ds=0)
{
	double n = floor(log10(v)), s;
	v = floor(v*pow(10.,-n));	n = pow(10.,n);

	if(v==1)	{	v = n/5;	s=n/10;	}
	else if(v<4){	v = n/2;	s=n/10;	}
	else if(v<7){	v = n;		s=n/5;	}
	else		{	v = 2*n;	s=n/2;	}
	if(ds)	*ds=s;
	return v;
}
//-----------------------------------------------------------------------------
void mglCanvas::SetTickTime(char dir, mreal d, const char *t)
{
	if(!strchr("xyzca",dir))	return;
	mglAxis &aa = (dir=='x' ? ax : (dir=='y' ? ay : (dir=='z' ? az : ac)));
	UpdateAxis();

	time_t tt;	tm t1,t2;
	tt=(time_t)aa.v1;	mgl_localtime(&tt, &t1, get(MGL_USE_GMTIME));
	tt=(time_t)aa.v2;	mgl_localtime(&tt, &t2, get(MGL_USE_GMTIME));
	if(aa.v1<aa.v2)	// adjust periodic values
	{
		if(abs(t1.tm_year-t2.tm_year)==1)	t2.tm_yday += 365;
		if(abs(t1.tm_yday-t2.tm_yday)==1)	t2.tm_hour += 24;
		if(abs(t1.tm_hour-t2.tm_hour)==1)	t2.tm_min += 60;
		if(abs(t1.tm_min-t2.tm_min)==1)		t2.tm_sec += 60;
	}
	else
	{
		if(abs(t1.tm_year-t2.tm_year)==1)	t1.tm_yday += 365;
		if(abs(t1.tm_yday-t2.tm_yday)==1)	t1.tm_hour += 24;
		if(abs(t1.tm_hour-t2.tm_hour)==1)	t1.tm_min += 60;
		if(abs(t1.tm_min-t2.tm_min)==1)		t1.tm_sec += 60;
	}
	if(!t || !t[0])		// adjust template
	{
		t = abs(t1.tm_yday-t2.tm_yday)>1 ? "%x" : "%X";
		if(abs(t1.tm_year-t2.tm_year)>3)	t = "%Y";
	}
	mreal ds=0;
	if(d==0)	// try to select optimal step
	{
		if(abs(t1.tm_year-t2.tm_year)>1)	// number of second in year NOTE: improve it
		{	d = 365.25*24*3600*mgl_adj_val(abs(t1.tm_year-t2.tm_year),&ds);
			ds *= 365.25*24*3600;	}
		// NOTE here should be months ... but it is too unregular ... so omit it now
// 		else if(t1.tm_mon!=t2.tm_mon)	d = 30*24*3600;	// number of second in month
		else if(abs(t1.tm_yday-t2.tm_yday)>=14)	// number of second in week
		{	d = mgl_adj_val(abs(t1.tm_yday-t2.tm_yday)/7,&ds);
			ds = ((ds<1)?1./7:ds)*7*24*3600;	d = ((d>=1)?d:1)*7*24*3600;	}
		else if(abs(t1.tm_yday-t2.tm_yday)>1)	// localtime("%x") cannot print time < 1 day
		{	d = 24*3600.*mgl_adj_val(abs(t1.tm_yday-t2.tm_yday),&ds);
			ds *= 24*3600;	if(d<24*3600)	{	d=24*3600;	ds=d/2;}	}
		else if(abs(t1.tm_hour-t2.tm_hour)>1)
		{	d = 3600.*mgl_adj_val(abs(t1.tm_hour-t2.tm_hour),&ds);	ds *=3600;	}
		else if(abs(t1.tm_min-t2.tm_min)>1)
		{	d = 60*mgl_adj_val(abs(t1.tm_min-t2.tm_min),&ds);	ds *=60;	}
		else if(abs(t1.tm_sec-t2.tm_sec)>1)	// localtime("%X") cannot print time < 1 sec
		{	d = mgl_adj_val(abs(t1.tm_sec-t2.tm_sec),&ds);
			if(d<1)	{	d=1;	ds=0.5;}	}
		else	// adjust msec. NOTE: this is not supported by localtime() !!!
			d = mgl_adj_val(fabs(aa.v2-aa.v1),&ds);
	}

	aa.ds = ds;	aa.dv = d;	aa.f = 1;	aa.txt.clear();
	MGL_TO_WCS(t,aa.t=wcs);

	if(strchr("xyztuvw",aa.ch))
		aa.org.Set(GetOrgX(aa.ch,aa.inv), GetOrgY(aa.ch,aa.inv), GetOrgZ(aa.ch,aa.inv));
	if(aa.ch=='x')	aa.v0 = aa.org.x;
	if(aa.ch=='y')	aa.v0 = aa.org.y;
	if(aa.ch=='z')	aa.v0 = aa.org.z;

	mreal v, v0 = mgl_isnan(aa.o) ? aa.v0 : aa.o, v1;
	if(aa.v2>aa.v1)
	{	v1 = aa.v2;		v0 = v0 - aa.dv*floor((v0-aa.v1)/aa.dv+1e-3);	}
	else
	{	v1 = aa.v1;		v0 = v0 - aa.dv*floor((v0-aa.v2)/aa.dv+1e-3);	}
	if(v0+aa.dv!=v0 && v1+aa.dv!=v1)	for(v=v0;v<=v1;v+=aa.dv)
	{
		wchar_t buf[64];
		tt = (time_t)v;	tm tp;		mgl_localtime(&tt, &tp, get(MGL_USE_GMTIME));
		wcsftime(buf,64,aa.t.c_str(),&tp);	aa.AddLabel(buf,v);
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::AdjustTicks(const char *dir, bool force, std::string stl)
{
	if(force)	SetTuneTicks(3);
	UpdateAxis();
	if(strchr(dir,'x') || strchr(dir,'X'))	// NOTE dir have to be non-NULL here !!!
	{	if(force)	ax.d=0;	ax.stl=stl;	AdjustTicks(ax,fx!=0);	}
	if(strchr(dir,'y') || strchr(dir,'Y'))
	{	if(force)	ay.d=0;	ay.stl=stl;	AdjustTicks(ay,fy!=0);	}
	if(strchr(dir,'z') || strchr(dir,'Z'))
	{	if(force)	az.d=0;	az.stl=stl;	AdjustTicks(az,fz!=0);	}
	if(strchr(dir,'a') || strchr(dir,'c'))
	{	if(force)	ac.d=0;	ac.stl=stl;	AdjustTicks(ac,fa!=0);	}
}
//-----------------------------------------------------------------------------
void mglCanvas::AdjustTicks(mglAxis &aa, bool ff)
{
	double d = fabs(aa.v2-aa.v1);
	if(aa.f>0)	return;
	if(ff && mgl_islog(aa.v1,aa.v2))
	{	aa.dv = 0;	aa.ds=0;	}
	else if(aa.d>0)
	{	aa.dv = aa.d;	aa.ds = aa.d/(abs(aa.ns)+1);	}
	else if(aa.d>-1.5)	// like =0 or =-1
	{	aa.dv = mgl_adj_val(d,&aa.ds);	aa.o=0;	}
	else
	{
		d /= -aa.d;
		long n = lrint(floor(log10(d)));
		aa.dv = pow(10.,n)*mgl_int(d*pow(10.,-n));
		aa.o=0;	aa.ds = pow(10.,n);
	}
	LabelTicks(aa);
}
//-----------------------------------------------------------------------------
static int mgl_tick_ext(mreal a, mreal b, wchar_t s[32], mreal &v)
{
	int kind = 0;
	if(fabs(a-b)<=0.01*fabs(a))
	{
		kind = 1;
		v = fabs(a-b);
		if(v>1000.f)
		{
			int k=int(log10(v)-0.01);
			kind=3;		v=mgl_ipow(10.,k);
			mglprintf(s, 32, L" (@{\\times{}10^{%d}})", k);
		}
		else if(v<0.02f)
		{
			int k=int(log10(v)-0.01)-1;
			kind=3;		v=mgl_ipow(10.,k);
			mglprintf(s, 32, L" (@{\\times{}10^{%d}})", k);
		}
	}
	else
	{
		v = fabs(b)>fabs(a)?fabs(b):fabs(a);
		if(v>=1000.f)
		{
			kind = 2;
			int k=int(log10(v)-0.01);
			v=mgl_ipow(10.,k);
			mglprintf(s, 32, L" \\times 10^{%d}", k);
		}
		else if(v<=1e-3f)
		{
			kind = 2;
			int k=int(log10(v)-0.01)-1;
			v=mgl_ipow(10.,k);
			mglprintf(s, 32, L" \\times 10^{%d}", k);
		}
	}
	return kind;
}
//-----------------------------------------------------------------------------
/*static std::wstring mgl_format(mreal v1, mreal v2, bool zero)
{
	v1=fabs(v1);	v2=fabs(v2);	if(v1>v2)	v2=v1;
	wchar_t str[5]=L"%.3g";
	int prec=int(2-log10(v2));	if(prec<0)	prec=0;
	if(v2<=0.001 || v2>=10000)
	{	str[2] = '2';	str[3]='e';	}
	else if(zero)
	{	str[2] = '0'+prec;	str[3]='f';	}
	return str;
}*/
//-----------------------------------------------------------------------------
static std::wstring mgl_tick_text(mreal z, mreal z0, mreal d, mreal v, int kind, const std::wstring &fact, mreal step, const char *stl)
{
	std::wstring str;
	bool ff = step>0 && !fact.empty();// && mgl_wcslen(str)+fact.length()<62;
	if(ff)	z/=step;
	mreal u = fabs(z)<d ? 0:z;
	if((kind&1) && z>z0)	u = fabs(z-z0)<d ? 0:(z-z0);
	if(kind==2 || (kind==3 && z>z0))	u /= v;
	str = mgl_ftoa(u,stl?stl:"");
	if((kind&1) && z>z0)	str = L"@{(+"+str+L")}";
//	if(kind&1)	fmt = z>z0?L"@{(+"+fmt+L")}":L"%g";
//	mglprintf(str,64,fmt.c_str(), u);
	if(ff)
	{
		if(str==L"-1" || str==L"+1" || str==L"\u22121")	str = str[0] + fact;
		else if(str==L"1")	str = fact;
		else if(str!=L"0")	str += fact;
	}
	return str;
}
//-----------------------------------------------------------------------------
void mglCanvas::LabelTicks(mglAxis &aa)
{
	if(strchr("xyztuvw",aa.ch))
		aa.org.Set(GetOrgX(aa.ch,aa.inv), GetOrgY(aa.ch,aa.inv), GetOrgZ(aa.ch,aa.inv));
	if(aa.ch=='x')	aa.v0 = aa.org.x;
	if(aa.ch=='y')	aa.v0 = aa.org.y;
	if(aa.ch=='z')	aa.v0 = aa.org.z;

	wchar_t buf[64]=L"";
	if(aa.f)	return;
	aa.txt.clear();
	bool minus = mglchr(aa.stl.c_str(),'-') && !mglchr(aa.stl.c_str(),'+');
	if(aa.dv==0 && aa.v1>0)	// positive log-scale
	{
		mreal v1 = aa.v1, v2 = aa.v2;
		if(v1>v2)	{	v1 = aa.v2; v2 = aa.v1;	}
		mreal v0 = exp(M_LN10*floor(0.1+log10(v1)));
		int ds = int(floor(0.1+log10(v2/v0))/7)+1;
		for(mreal v=v0;v<=v2*MGL_EPSILON;v*=10)	if(v*MGL_EPSILON>=v1)
		{
			int d = int(floor(0.1+log10(v)));
			if(d==0)	wcscpy(buf,L"1");
			else if(d==1)	wcscpy(buf,L"10");
			else if(d>0)	mglprintf(buf,64,L"10^{%d}",d);
			else	mglprintf(buf,64,minus?L"10^{-%d}":L"10^{\u2212%d}",-d);
			if(d%ds!=0)	*buf=0;	//	remove too often log ticks
			aa.AddLabel(buf,v);
		}
	}
	else if(aa.dv==0 && aa.v2<0)	// negative log-scale
	{
		mreal v1 = aa.v1, v2 = aa.v2;
		if(v1>v2)	{	v1 = aa.v2; v2 = aa.v1;	}
		mreal v0 = -exp(M_LN10*floor(0.1+log10(-v2)));
		int ds = int(floor(0.1+log10(v1/v0))/7)+1;
		for(mreal v=v0;v>=v1*MGL_EPSILON;v*=10)	if(v*MGL_EPSILON<=v2)
		{
			int d = int(floor(0.1+log10(-v)));
			if(d==0)	wcscpy(buf,minus?L"-1":L"\u22121");
			else if(d==1)	wcscpy(buf,minus?L"-10":L"\u221210");
			else if(d>0)	mglprintf(buf,64,minus?L"-10^{%d}":L"\u221210^{%d}",d);
			else	mglprintf(buf,64,minus?L"-10^{-%d}":L"\u221210^{\u2212%d}",-d);
			if(d%ds!=0)	*buf=0;	//	remove too often log ticks
			aa.AddLabel(buf,v);
		}
	}
	else if(aa.dv)	// ticks drawing
	{
		mreal w=0;
		int kind=0;
		wchar_t s[32]=L"";
		if(aa.t.empty() && TuneTicks && !strchr(aa.stl.c_str(),'!'))
			kind = mgl_tick_ext(aa.v2, aa.v1, s, w);
		if(((TuneTicks&1)==0 && kind==2) || ((TuneTicks&2)==0 && kind!=2))
			kind=0;

		mreal v0 = mgl_isnan(aa.o) ? aa.v0 : aa.o, v1;
		if(mgl_isnan(v0))	v0=0;
		if(aa.v2>aa.v1)
		{	v1 = aa.v2;		v0 = v0 - aa.dv*floor((v0-aa.v1)/aa.dv+1e-3);	}
		else
		{	v1 = aa.v1;		v0 = v0 - aa.dv*floor((v0-aa.v2)/aa.dv+1e-3);	}

		if(v0+aa.dv!=v0 && v1+aa.dv!=v1)
		{
			if(aa.t.empty())	for(mreal v=v0;v<=v1;v+=aa.dv)
				aa.AddLabel(mgl_tick_text(v,v0,aa.dv/100,w,kind,aa.fact,aa.d,aa.stl.c_str()),v);
			else	for(mreal v=v0;v<=v1;v+=aa.dv)
			{
				if(aa.t[0]!='&')	mglprintf(buf, 64, aa.t.c_str(), fabs(v)<aa.dv/100 ? 0 : v);
				else	mglprintf(buf, 64, aa.t.c_str()+1, mgl_int(fabs(v)<aa.dv/100 ? 0 : v));
				mgl_wcstrim(buf);	aa.AddLabel(buf,v);
			}
		}
		if(kind&2)	aa.AddLabel(s,FactorPos*(aa.v2-aa.v1)+aa.v1);
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::Axis(const char *dir, const char *stl, const char *opt)
{
	int text = !(mglchr(dir,'_') || mglchr(dir,'~'))?1:0;
	if(mglchr(dir,':'))	text = text|2;
	bool inv = mglchr(dir,'^');
	bool ret = get(MGL_ENABLE_RTEXT);
	if(mglchr(dir,'U'))	clr(MGL_ENABLE_RTEXT);

	std::string Tstl;
	for(const char *s="+E0123456789-fF!";*s;s++)	if(mglchr(dir,*s))	Tstl += *s;

	const char *ar = "AKDTVISO";
	char arr=0;
	for(size_t i=0;i<strlen(ar);i++)
		if(strchr(dir,ar[i]))	{	arr=ar[i];	break;	}
	if(!mglchrs(dir,"xXyYzZ"))	dir="xyz";

	mreal angl = SaveState(opt);
	AdjustTicks(dir,mglchr(stl,'a'),Tstl);
	LoadState();

	ax.pos = strchr(dir,'X') ? 'T':'t';
	ay.pos = strchr(dir,'Y') ? 'T':'t';
	az.pos = strchr(dir,'Z') ? 'T':'t';
	ax.inv = ay.inv = az.inv = false;

	if(strchr(dir,'X') || strchr(dir,'x'))
	{	ax.inv = inv;	DrawAxis(ax, text, arr, stl, angl);	}
	if(strchr(dir,'Z') || strchr(dir,'z'))
	{	az.inv = inv;	DrawAxis(az, text, arr, stl, angl);	}
	if((TernAxis&3))
	{
		mglAxis ty(ay);		ty.pos='t';	ty.ch='T';
		ty.dir.Set(-1,1);		ty.org.Set(1,0,ay.org.z);
		DrawAxis(ty, text, arr, stl, angl);	ty.ch='t';
		ty.dir.Set(0,-1);		ty.org.Set(0,1,ay.org.z);
		DrawAxis(ty, text, arr, stl, angl);
	}
	else if(strchr(dir,'Y') || strchr(dir,'y'))
	{	ay.inv = inv;	DrawAxis(ay, text, arr, stl, angl);	}
	set(ret, MGL_ENABLE_RTEXT);
}
//-----------------------------------------------------------------------------
void mglCanvas::DrawAxis(mglAxis &aa, int text, char arr,const char *stl,mreal angl)
{
	aa.angl = angl;
	if(strchr("xyz",aa.ch))
		aa.org.Set(GetOrgX(aa.ch,aa.inv), GetOrgY(aa.ch,aa.inv), GetOrgZ(aa.ch,aa.inv));
	if(aa.ch=='x')	aa.v0 = aa.org.x;
	if(aa.ch=='y')	aa.v0 = aa.org.y;
	if(aa.ch=='z')	aa.v0 = aa.org.z;

	mglPoint d(aa.dir), o(aa.org), q(NAN);	// "transverse" org
	if(strchr("xyz",aa.ch))	o -= d*(o*d);
	mglPoint av=(Min+Max)/2, dv,da,db, p;
	dv.Set(mgl_sign(av.x-o.x), mgl_sign(av.y-o.y), mgl_sign(av.z-o.z));
	da = aa.a*(dv*aa.a);	db = aa.b*(dv*aa.b);

	static int cgid=1;	StartGroup("Axis",cgid++);

	bool have_color=mgl_have_color(stl);
	bool dif_color = !have_color && aa.dv==0 && strcmp(TickStl,SubTStl);
	long kq = AllocPnts(31);
	if(text&2)	// line throw point (0,0,0)
	{
		SetPenPal("k:");
#pragma omp parallel for
		for(long i=0;i<31;i++)
			AddPntQ(kq+i, &B, d*(aa.v1+(aa.v2-aa.v1)*i/30.), CDef,q,-1,3);
		curve_plot(31,kq);
	}
	SetPenPal(have_color ? stl:AxisStl);

	kq = AllocPnts(31);
#pragma omp parallel for
	for(long i=0;i<31;i++)
		AddPntQ(kq+i, &B, o + d*(aa.v1+(aa.v2-aa.v1)*i/30.), CDef,q,-1,3);
	curve_plot(31,kq);
	if(arr)
	{
		p = o + d*(aa.v1+(aa.v2-aa.v1)*1.05);
		long k1 = AddPnt(&B, p,CDef,q,-1,3);
		line_plot(k1,kq+30);	arrow_plot(k1,kq+30,arr);
	}

	long k2 = aa.txt.size();
	mreal v, u, v0 = mgl_isnan(aa.o) ? aa.v0 : aa.o;
	if(*TickStl && !have_color)	SetPenPal(TickStl);
	if(k2>0)	for(long i=0;i<k2;i++)
	{
		v = aa.txt[i].val;	u = fabs(v);
		if((v-aa.v2)*(v-aa.v1)<=0)	tick_draw(o+d*v, da, db, 0);
		if(dif_color)	SetPenPal(SubTStl);
		if(aa.dv==0 && aa.v2>aa.v1 && fabs(u-exp(M_LN10*floor(0.1+log10(u))))<0.01*u)
			for(long j=2;j<10 && v*j<aa.v2;j++)	tick_draw(o+d*(v*j),da,db,1);
		if(aa.dv==0 && aa.v2<aa.v1 && fabs(u-exp(M_LN10*floor(0.1+log10(u))))<0.01*u)
			for(long j=2;j<10 && v*j<aa.v1;j++)	tick_draw(o+d*(v*j),da,db,1);
		if(dif_color)	SetPenPal(TickStl);
	}
	if(aa.ds>0 && !get(MGL_NOSUBTICKS) && (fabs(aa.v1)>1e-150 || fabs(aa.v2)>1e-150))
	{
		if(aa.v2>aa.v1)	v0 = v0 - aa.ds*floor((v0-aa.v1)/aa.ds+1e-3);
		else			v0 = v0 - aa.ds*floor((v0-aa.v2)/aa.ds+1e-3);
		if(v0+aa.ds!=v0 && aa.v2+aa.ds!=aa.v2)
		{
			if(*SubTStl && !have_color)	SetPenPal(SubTStl);
			for(v=v0;(v-aa.v2)*(v-aa.v1)<=0;v+=aa.ds)
				tick_draw(o+d*v,da,db,1);
		}
	}
	if(!have_color)	SetPenPal(AxisStl);
	if(text&1)	DrawLabels(aa);
	EndGroup();
}
//-----------------------------------------------------------------------------
void mglCanvas::DrawLabels(mglAxis &aa, bool inv, const mglMatrix *M)
{
	if(M==0)	M=&B;
	if(strchr("xyz",aa.ch))
		aa.org.Set(GetOrgX(aa.ch,aa.inv), GetOrgY(aa.ch,aa.inv), GetOrgZ(aa.ch,aa.inv));
	if(aa.ch=='x')	aa.v0 = aa.org.x;
	if(aa.ch=='y')	aa.v0 = aa.org.y;
	if(aa.ch=='z')	aa.v0 = aa.org.z;

	mglPoint d(aa.dir), o(aa.org), q(NAN);	// "transverse" org
	if(strchr("xyz",aa.ch))	o -= d*(o*d);
	mglPoint s=(Min+Max)/2, dv(mgl_sign(s.x-o.x), mgl_sign(s.y-o.y), mgl_sign(s.z-o.z));
	mglPoint a = aa.a*(dv*aa.a) + aa.b*(dv*aa.b);
	if(aa.ch=='c')	a = aa.a;

	long n = aa.txt.size();
	mreal *w=new mreal[n], wsp = 2*TextWidth(" ",FontDef,-1);
	long *kk=new long[n], kq = AllocPnts(n);
#pragma omp parallel for
	for(long i=0;i<n;i++)	// fill base label properties
	{
		w[i] = TextWidth(aa.txt[i].text.c_str(),FontDef,-1);
		AddPntQ(kq+i, M, o+d*aa.txt[i].val,-1,d,0,7);
	}
	for(long i=0;i<n;i++)	kk[i] = kq+i;
	mreal c=INFINITY, l=0, h = TextHeight(FontDef,-1);	// find sizes
	for(long i=0;i<n-1;i++)
	{
		// exclude factors
		if(aa.ch!='c' && (aa.txt[i].val<aa.v1 || aa.txt[i+1].val<aa.v1 || aa.txt[i].val>aa.v2 || aa.txt[i+1].val>aa.v2))
			continue;
		if(kk[i]<0 || kk[i+1]<0)	continue;
		mreal v = (GetPntP(kk[i+1])-GetPntP(kk[i])).norm();	// distance between ticks
		mreal vv = (w[i]+w[i+1])/2-wsp;	// length of labels
		if(v>0 && l < vv/v)	l = vv/v;
		if(c>v)	c = v;
	}
	h /= c;

	mreal tet=0;
	if(mgl_isnum(aa.angl))	tet = aa.angl*M_PI/180;	// manual rotation
	else if(get(MGL_ENABLE_RTEXT) && get(MGL_TICKS_ROTATE) && l>1)	// try rotate first
	{
		mreal t1 = 1.1*h<1 ? asin(1.1*h) : M_PI/2;
		mreal r2 = l*l+h*h;
		mreal t2 = r2*1.21>1 ? asin((l*sqrt(r2-1/1.21)+h/1.1)/r2):M_PI/2;
		tet = t1<t2 ? t1:t2;
	}
	mreal sn = sin(tet), cs=cos(tet);
	if(sn)
	{
		mreal l1=h/fabs(sn), l2=fabs(l*cs+h*sn);
		l = l2>l1?l1:l2;
	}
	char *align=new char[n], *up=new char[n];
	for(long i=0;i<n;i++)	if(kk[i]>=0)	// select proper align
	{
		mglPoint p(a),r(o+d*aa.txt[i].val);
		ScalePoint(M, r, p, false);
		mglPnt &pp = Pnt[kk[i]];
		mreal ux=pp.u*cs + pp.v*sn, uy=pp.v*cs - pp.u*sn;
		bool Nrot = !get(MGL_ENABLE_RTEXT) || !get(MGL_TICKS_ROTATE);
		bool Vcnt = ux==0 && uy!=0 && Nrot;
		bool algn = tet!=0;		// TODO add proper align for arbitrary tet!
		if(ux*ux+uy*uy!=0 && Nrot)	{	ux=1;	uy=0;	algn=true;	}
		if(ux<0 || (ux==0 && uy<0))	{	ux=-ux;	uy=-uy;	pp.w=-pp.w;	}
		pp.u = ux;	pp.v = uy;
		mreal pu = p.x*ux+p.y*uy, pv = p.y*ux-p.x*uy; /*, su = ps.x*ux+ps.y*uy;*/
		if(Vcnt)	up[i]='V';
		else if(aa.ch!='c')	up[i] = ((pv>0) ^ inv) ? 'T':'t';
		else		up[i]=(aa.ns==0 || aa.ns==3)?'t':'T';
		int t=0;
		if(algn)
		{
			if(aa.ch!='c')	t= (pu==0)?0:(pu<0? -1:1);
			else	t=inv?-1:1;
		}
		char val[3]={'L','C','R'};	align[i] = val[t+1];
	}
	long k = get(MGL_TICKS_SKIP) ? 1+l : 1;

	for(long i=0;i<n;i++)	if(kk[i]>=0)
	{
		mreal v = aa.txt[i].val;
		if(get(MGL_NO_ORIGIN) && v==aa.v0)	continue;
		if(v>aa.v1 && v<aa.v2 && i%k!=0)	continue;
		char pos[4]={up[i],':',align[i],0};
		text_plot(kk[i], aa.txt[i].text.c_str(), pos, -1, aa.sh+0.05,CDef);
	}
	delete []w;	delete []kk;	delete []align;	delete []up;
}
//-----------------------------------------------------------------------------
char mglCanvas::GetLabelPos(mreal c, long kk, mglAxis &aa)
{
	if(strchr("xyz",aa.ch))
		aa.org.Set(GetOrgX(aa.ch,aa.inv), GetOrgY(aa.ch,aa.inv), GetOrgZ(aa.ch,aa.inv));
	mglPoint d = aa.dir, o = aa.org;	// "transverse" org
	if(strchr("xyz",aa.ch))	o -= d*(o*d);
	mglPoint p,q, s=(Min+Max)/2, nn;
	s = s - d*(s*d);

	int ts = 1;
	if(aa.ch=='c')	ts=(aa.ns==0 || aa.ns==3)?1:-1;
	if(aa.ch=='T')	ts=-1;

	p = o+d*c;	nn = s-o;	ScalePoint(&B,p,nn);
	mglPnt &qq = Pnt[kk];

	if(aa.ch=='c')	qq.u = qq.v = NAN;
	if(!get(MGL_DISABLE_SCALE))	ts = mgl_sign(qq.v*nn.x-qq.u*nn.y);
	if(aa.ch=='T')	ts *= -1;
	if(aa.pos=='T')	ts *= -1;
	return ts>0 ? 't':'T';
}
//-----------------------------------------------------------------------------
void mglCanvas::tick_draw(mglPoint o, mglPoint d1, mglPoint d2, int f)
{
	if(TickLen==0)	return;
	// try to exclude ticks out of axis range
	if(f && ((o.x-Min.x)*(o.x-Max.x)>0 || (o.y-Min.y)*(o.y-Max.y)>0 || (o.z-Min.z)*(o.z-Max.z)>0))
		return;
	mreal v = font_factor*TickLen/sqrt(1.f+f*st_t);
	mglPoint p=o;

	ScalePoint(&B,o, d1, false);	d1.Normalize();
	ScalePoint(&B,p, d2, false);	d2.Normalize();
	long k1 = AddPnt(&B, p+d1*v, CDef, mglPoint(NAN), 0, 0);
	long k2 = AddPnt(&B, p, CDef, mglPoint(NAN), 0, 0);
	long k3 = AddPnt(&B, p+d2*v, CDef, mglPoint(NAN), 0, 0);
	line_plot(k1,k2);	line_plot(k2,k3);
}
//-----------------------------------------------------------------------------
void mglCanvas::Grid(const char *dir, const char *pen, const char *opt)
{
	SaveState(opt);
	bool at_tick=mglchr(dir,'!');
	if(!mglchrs(dir,"xyz"))	dir="xyz";
	AdjustTicks(dir,false);
	SetPenPal(pen);

	static int cgid=1;	StartGroup("AxisGrid",cgid++);
	if(strchr(dir,'x'))	DrawGrid(ax,at_tick);
	if(strchr(dir,'y'))	DrawGrid(ay,at_tick);
	if(strchr(dir,'z'))	DrawGrid(az,at_tick);
	EndGroup();
}
//-----------------------------------------------------------------------------
static void mgl_drw_grid(HMGL gr, double val, const mglPoint &d, const mglPoint &oa, const mglPoint &ob, const mglPoint &da1, const mglPoint &db1, const mglPoint &da2, const mglPoint &db2)
{
	mglPoint q(oa+d*val);	// lines along 'a'
	long kq = gr->AllocPnts(31);
#pragma omp parallel for
	for(long i=0;i<31;i++)
	{	mreal v=i/30.;	gr->AddPntQ(kq+i,q+da1*(1-v)+da2*v);	}
	gr->curve_plot(31,kq);
	q = ob+d*val;		// lines along 'b'
	kq = gr->AllocPnts(31);
#pragma omp parallel for
	for(long i=0;i<31;i++)
	{	mreal v = i/30.;	gr->AddPntQ(kq+i,q+db1*(1-v)+db2*v);	}
	gr->curve_plot(31,kq);
}
void mglCanvas::DrawGrid(mglAxis &aa, bool at_tick)
{
	mglPoint pp[8]={Min,Min,Min,Min,Max,Max,Max,Max},nan(NAN), oo[8], org=Min;
	pp[1].x=Max.x;	pp[2].y=Max.y;	pp[3].z=Max.z;
	pp[4].x=Min.x;	pp[5].y=Min.y;	pp[6].z=Min.z;
	mreal zm=INFINITY;
	memcpy(oo,pp,8*sizeof(mglPoint));
	for(int i=0;i<8;i++)	// find deepest point
	{
		ScalePoint(&B,pp[i],nan,false);
		if(pp[i].z<zm)	{	zm=pp[i].z;	org=oo[i];	}
	}
	if(mgl_isnum(Org.x)) 	org.x = Org.x;
	if(mgl_isnum(Org.y)) 	org.y = Org.y;
	if(mgl_isnum(Org.z)) 	org.z = Org.z;
	mglPoint d=aa.dir, da1,da2,db1,db2,oa,ob, p,q;
	da1 = aa.a*(aa.a*Min);	da2 = aa.a*(aa.a*Max);
	db1 = aa.b*(aa.b*Min);	db2 = aa.b*(aa.b*Max);
	oa  = aa.b*(aa.b*org);	ob  = aa.a*(aa.a*org);

	if(at_tick && aa.ds>0 && !get(MGL_NOSUBTICKS))
	{
		mreal v0 = mgl_isnan(aa.o) ? aa.v0 : aa.o;
		if(aa.v2>aa.v1)	v0 = v0 - aa.ds*floor((v0-aa.v1)/aa.ds+1e-3);
		else			v0 = v0 - aa.ds*floor((v0-aa.v2)/aa.ds+1e-3);
		fflush(stdout);		// somehow this help to bypass bug in GCC 32bit
		if(v0+aa.ds!=v0 && aa.v2+aa.ds!=aa.v2)
			for(mreal v=v0;(v-aa.v2)*(v-aa.v1)<=0;v+=aa.ds)
				mgl_drw_grid(this, v, d, oa, ob, da1, db1, da2, db2);
	}
	if(aa.dv)	at_tick = false;
	long n=aa.txt.size();
	if(n>0)	for(long i=0;i<n;i++)
	{
		mreal v = aa.txt[i].val;
		mgl_drw_grid(this, v, d, oa, ob, da1, db1, da2, db2);
		if(at_tick)
		{
			mreal u = fabs(v);
			if(aa.v2>aa.v1 && fabs(u-exp(M_LN10*floor(0.1+log10(u))))<0.01*u)
				for(long j=2;j<10 && v*j<aa.v2;j++)
					mgl_drw_grid(this, v*j, d, oa, ob, da1, db1, da2, db2);
			if(aa.v2<aa.v1 && fabs(u-exp(M_LN10*floor(0.1+log10(u))))<0.01*u)
				for(long j=2;j<10 && v*j<aa.v1;j++)
					mgl_drw_grid(this, v*j, d, oa, ob, da1, db1, da2, db2);
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvas::Label(char dir, const char *str, mreal pos, const char *opt)
{
	MGL_TO_WCS(str,Labelw(dir, wcs, pos, opt));
}
//-----------------------------------------------------------------------------
void mglCanvas::Labelw(char dir, const wchar_t *text, mreal pos, const char *opt)
{
	mreal shift =  SaveState(opt), t=0;	if(mgl_isnan(shift))	shift=0;
	shift -= 0.1;
	mglPoint p,q;
	mglAxis *aa=0;

	mglAxis ty(ay);

	if(dir=='c')
	{
		AdjustTicks(ac,fc!=0);	aa = &ac;	// TODO
		p = ac.org+(ac.a.y?1.15:1)*ac.dir;	q = ac.dir;
		pos = ac.a.x>0?1:-1;	shift += ac.sh;
	}
	if(dir=='x')
	{
		AdjustTicks(ax,fx!=0);	aa = &ax;
		if(ax.dv)	t = (Min.x+Max.x+pos*(Max.x-Min.x))/2;
		else	t = Min.x*pow(Max.x/Min.x, (pos+1)/2);
		p.Set(t, GetOrgY(ax.ch,ax.inv), GetOrgZ(ax.ch,ax.inv));
		q.Set(1,0,0);	shift += ax.sh;
	}
	if(dir=='y' && !(TernAxis&3))
	{
		AdjustTicks(ay,fy!=0);	aa = &ay;
		if(ay.dv)	t = (Min.y+Max.y+pos*(Max.y-Min.y))/2;
		else	t = Min.y*pow(Max.y/Min.y, (pos+1)/2);
		p.Set(GetOrgX(ay.ch,ay.inv), t, GetOrgZ(ay.ch,ay.inv));
		q.Set(0,1,0);	shift += ay.sh;
		if(TernAxis&3)
		{
			q.Set(-1,1,0);	pos=-pos;
		}
	}
	if(dir=='y' && (TernAxis&3))
	{
		ty.ch='T';	ty.dir.Set(-1,1);	ty.org.Set(1,0,ay.org.z);
		AdjustTicks(ty,fy!=0);	aa = &ty;
		if(ty.dv)	t = (Min.y+Max.y+pos*(Max.y-Min.y))/2;
		else	t = Min.y*pow(Max.y/Min.y, (pos+1)/2);
		p.Set(GetOrgX(ty.ch,ty.inv), t, GetOrgZ(ty.ch,ty.inv));
		q.Set(0,1,0);	shift += ty.sh;
		if(TernAxis&3)
		{
			q.Set(-1,1,0);	pos=-pos;
		}
	}
	if(dir=='t' && (TernAxis&3))
	{
		ty.ch='t';	ty.dir.Set(0,-1);	ty.org.Set(0,1,ay.org.z);
		AdjustTicks(ty,fy!=0);	pos = -pos;	aa = &ty;
		if(ty.dv)	t = (Min.y+Max.y+pos*(Max.y-Min.y))/2;
		else	t = Min.y*pow(Max.y/Min.y, (pos+1)/2);
		p.Set(GetOrgX(ty.ch,ty.inv), t, GetOrgZ(ty.ch,ty.inv));
		q.Set(0,1,0);	shift += ty.sh;
	}
	if(dir=='z')
	{
		AdjustTicks(az,fz!=0);	aa = &az;
		if(az.dv)	t = (Min.z+Max.z+pos*(Max.z-Min.z))/2;
		else	t = Min.z*pow(Max.z/Min.z, (pos+1)/2);
		p.Set(GetOrgX(az.ch,az.inv), GetOrgY(az.ch,az.inv), t);
		q.Set(0,0,1);	shift += az.sh;
	}
	if(aa)
	{
		char font[64],ff[3]=":C";	memset(font,0,64);
		if(pos<-0.2)	ff[1]='L';
		if(pos>0.2)	ff[1]='R';
		mgl_strncpy(font,FontDef,32);	strcat(font,ff);
		long kk = AddPnt(&B, p,-1,q,0,7);	ff[1]=0;
		if(kk>=0)
		{
			mglPnt &pp = Pnt[kk];
			if(pp.u<0 || (pp.u==0 && pp.v<0))
			{	pp.u=-pp.u;	pp.v=-pp.v;	pp.w=-pp.w;	}
			if(dir=='c' &&  ac.a.y!=0)
			{
				ff[0] = ac.a.y>0?'T':'t';	strcat(font,ff);
				text_plot(kk,text,font,-1.4,ac.a.y>0?shift:0);
			}
			else
			{
				ff[0] = GetLabelPos(t, kk, *aa);	strcat(font,ff);
				text_plot(kk,text,font,-1.4,(ff[0]=='T'?0.3:0.35)+shift);
			}
		}
	}
	LoadState();
}
//-----------------------------------------------------------------------------
void mglCanvas::Box(const char *col, bool ticks)
{
	mglPoint o = Org;
	mreal tl=TickLen;
	if(!ticks)	TickLen=0;
	set(MGL_NOSUBTICKS);	Org = Min;
	static int cgid=1;	StartGroup("Box",cgid++);
	Axis("xyz_",col);
	if(TernAxis&1)
	{
		Org.x=Max.x;	Org.y=Min.y;	Org.z=Max.z;
		DrawAxis(ax, 0, 0,col);	DrawAxis(az, 0, 0,col);
		Org.x=Min.x;	Org.y=Max.y;	Org.z=Max.z;
		DrawAxis(az, 0, 0,col);

		mglAxis ty(ay);				ty.ch='T';
		ty.dir.Set(-1,1);	ty.org.Set(1,0,Max.z);
		DrawAxis(ty, 0, 0,col);	ty.ch='t';
		ty.dir.Set(0,-1);	ty.org.Set(0,1,Max.z);
		DrawAxis(ty, 0, 0,col);
	}
	else if(TernAxis&2)
	{
		mglAxis ty(az);
		ty.ch='T';	ty.a.Set(1,0);	ty.b.Set(-1,1);
		ty.dir.Set(-1,0,1);	ty.org.Set(1,0,0);
		DrawAxis(ty, 0, 0,col);
		ty.ch='t';	ty.a.Set(0,1);	ty.b.Set(-1,1);
		ty.dir.Set(0,-1,1);	ty.org.Set(0,1,0);
		DrawAxis(ty, 0, 0,col);
	}
	else
	{
		Org.z=Max.z;	Axis("xy_",col);
		Org = Max;		Axis("xyz_",col);
		Org.z=Min.z;	Axis("xy_",col);
		Org.x=Min.x;	DrawAxis(az,0,0,col);
		Org.x=Max.x;	Org.y=Min.y;	DrawAxis(az,0,0,col);
		if(mglchr(col,'@'))
		{
			// edge points
			mglPoint p[8]={Min,Min,Min,Min,Max,Max,Max,Max},nan(NAN),oo[8];
			p[1].x=Max.x;	p[2].y=Max.y;	p[3].z=Max.z;
			p[4].x=Min.x;	p[5].y=Min.y;	p[6].z=Min.z;
			mreal zm=INFINITY;	int im=0;
			memcpy(oo,p,8*sizeof(mglPoint));
			for(int i=0;i<8;i++)	// find deepest point
			{
				ScalePoint(&B,p[i],nan,false);
				if(p[i].z<zm)	{	zm=p[i].z;	im=i;	}
			}
			// now draw faces
			char color[10]="{y9}";
			for(int i=0;col[i];i++)
			{
				if(col[i]=='{' && col[i+1]=='x')
				{	memcpy(color,col+i,9);	color[9]=0;	break;	}
				if(strchr(MGL_COLORS,col[i]))
				{
					if(i>1 && col[i-1]=='{')	{	color[1]=col[i];	color[2]=col[i+1];	break;	}
					else	{	color[0]=col[i];	color[1]=0;	break;	}
				}
			}
			SetPenPal(color);
			mreal dx = (Max.x-Min.x)/30, dy = (Max.y-Min.y)/30, dz = (Max.z-Min.z)/30;
			long kq = AllocPnts(3*31*31);
#pragma omp parallel for collapse(2)
			for(long i=0;i<31;i++)	for(long j=0;j<31;j++)
			{
				long i0=kq+3*(i+31*j);
				AddPntQ(i0,  mglPoint(oo[im].x,Min.y+dy*i,Min.z+dz*j));
				AddPntQ(i0+1,mglPoint(Min.x+dx*i,oo[im].y,Min.z+dz*j));
				AddPntQ(i0+2,mglPoint(Min.x+dx*i,Min.y+dy*j,oo[im].z));
			}
			for(long i=0;i<30;i++)	for(long j=0;j<30;j++)
			{
				long i0=kq+3*(i+31*j);
				quad_plot(i0,  i0+3,i0+93,i0+96);
				quad_plot(i0+1,i0+4,i0+94,i0+97);
				quad_plot(i0+2,i0+5,i0+95,i0+98);
			}
		}
	}
	EndGroup();
	clr(MGL_NOSUBTICKS);	Org=o;	TickLen=tl;
}
//-----------------------------------------------------------------------------
void mglCanvas::Colorbar(const char *sch)
{
	bool in = mglchr(sch,'I');
	mreal sx = (fabs(B.b[0])+fabs(B.b[1])+fabs(B.b[2]))/B.pf/B1.b[0], x=1;
	mreal sy = (fabs(B.b[3])+fabs(B.b[4])+fabs(B.b[5]))/B.pf/B1.b[4], y=0;
	if(mglchr(sch,'<'))	{	x=in?(1-sx)/2:0.05;	y=0;	}
	else if(mglchr(sch,'^'))	{	x=0;	y=in?(1+sy)/2:0.95;	}
	else if(mglchr(sch,'_'))	{	x=0;	y=in?(1-sy)/2:0.05;	}
	else	{	x=in?(1+sx)/2:0.95;	y=0;	}
	Colorbar(sch, x, y, 1, 1);
}
//-----------------------------------------------------------------------------
void mglCanvas::Colorbar(const char *sch, mreal x, mreal y, mreal w, mreal h)
{
	bool in = mglchr(sch,'I');
	bool text = !mglchr(sch,'~');
	int where = 0;		// ‘0’ - right, ‘1’ - left, ‘2’ - above, ‘3’ - under
	if(mglchr(sch,'>'))	where = in?1:0;
	if(mglchr(sch,'<'))	where = in?0:1;
	if(mglchr(sch,'^'))	where = in?3:2;
	if(mglchr(sch,'_'))	where = in?2:3;
	if(mglchr(sch,'A'))	{	Push();	Identity();	}

	ac.stl.clear();
	for(const char *s="+E0123456789-fF!";*s;s++)	if(mglchr(sch,*s))	ac.stl += *s;
	AdjustTicks("c",mglchr(sch,'a'),ac.stl.c_str());

	long n=256, s = AddTexture(sch);
	mglData v(n);
	if(ac.d || fa==0 || Min.c*Max.c<=0)	v.Fill(Min.c,Max.c);
	else if(Min.c>0)
	{	v.Fill(log(Min.c), log(Max.c));		v.Modify("exp(u)");		}
	else if(Max.c<0)
	{	v.Fill(log(-Min.c), log(-Max.c));	v.Modify("-exp(u)");	}
	mreal *c=new mreal[n];
	for(long i=0;i<n;i++)	c[i] = GetC(s,v.a[i]);
	colorbar(&v, c, where, x, y, w, h, text);
	delete []c;
	if(mglchr(sch,'A'))	Pop();
}
//-----------------------------------------------------------------------------
void mglCanvas::Colorbar(HCDT v, const char *sch)
{
	bool in = mglchr(sch,'I');
	mreal sx = (fabs(B.b[0])+fabs(B.b[1])+fabs(B.b[2]))/B.pf/B1.b[0], x=1;
	mreal sy = (fabs(B.b[3])+fabs(B.b[4])+fabs(B.b[5]))/B.pf/B1.b[4], y=0;
	if(mglchr(sch,'>'))	{	x=in?(1+sx)/2:1;	y=0;	}
	if(mglchr(sch,'<'))	{	x=in?(1-sx)/2:0;	y=0;	}
	if(mglchr(sch,'^'))	{	x=0;	y=in?(1+sy)/2:1;	}
	if(mglchr(sch,'_'))	{	x=0;	y=in?(1-sy)/2:0;	}
	Colorbar(v, sch, x, y, 1, 1);
}
//-----------------------------------------------------------------------------
void mglCanvas::Colorbar(HCDT v, const char *sch, mreal x, mreal y, mreal w, mreal h)
{
	bool in = mglchr(sch,'I');
	bool text = !mglchr(sch,'~');
	int where = 0;
	if(mglchr(sch,'>'))	where = in?1:0;
	if(mglchr(sch,'<'))	where = in?0:1;
	if(mglchr(sch,'^'))	where = in?3:2;
	if(mglchr(sch,'_'))	where = in?2:3;
	if(mglchr(sch,'A'))	{	Push();	Identity();	}

	ac.stl.clear();
	for(const char *s="+E0123456789-fF!";*s;s++)	if(mglchr(sch,*s))	ac.stl += *s;
	AdjustTicks("c",mglchr(sch,'a'),ac.stl.c_str());

	mreal *c=new mreal[v->GetNx()];
	if(!mgl_have_color(sch))	sch = MGL_DEF_PAL;
	long s = AddTexture(sch);
	int nc = GetNumPal(s*256);
	mreal dc = nc>1 ? 1/(MGL_EPSILON*(nc-1)):0;
	for(long i=0;i<v->GetNx();i++)	c[i] = s+i*dc;
	colorbar(v, c, where, x, y, w, h, text);
	delete []c;
	if(mglchr(sch,'A'))	Pop();
}
//-----------------------------------------------------------------------------
void mglCanvas::colorbar(HCDT vv, const mreal *c, int where, mreal x, mreal y, mreal w, mreal h, bool text)
{
	static int cgid=1;	StartGroup("Colorbar",cgid++);
	long n=vv->GetNx();
	mreal s3=B.pf,ss=1/s3;		// NOTE: colorbar was wider ss=0.9;
	mglPoint p1,p2;
	mglMatrix M=B1;	M.pf=s3;	M.norot=true;

	set(MGL_DISABLE_SCALE);		// NOTE this make colorbar non-thread-safe!!!
	x = s3*(2*x-1);	y = s3*(2*y-1);	w *= s3;	h *= s3;
	mask = MGL_SOLID_MASK;

	const long kq = AllocPnts(n*2);
	for(long i=0;i<n;i++)
	{
		mreal d = GetA(vv->v(i))*2-1;
		p1 = p2 = mglPoint((ss*d+1)*w+x, (ss*d+1)*h+y, s3);
		switch(where)
		{
			case 1:	p1.x = x;	p2.x = x+0.1*w;	break;
			case 2:	p1.y = y-0.1*h;	p2.y = y;	break;
			case 3:	p1.y = y;	p2.y = y+0.1*h;	break;
			default:p1.x = x-0.1*w;	p2.x = x;	break;
		}
		AddPntQ(kq+i, &M, p1,c[i]);
		AddPntQ(kq+i+n, &M, p2,c[i]);
	}
	for(long i=0;i<n-1;i++)	quad_plot(kq+i, kq+i+n, kq+i+1, kq+i+1+n);

	if(n<64)
	{
		ac.txt.clear();
		if(ac.t.empty())
			for(long i=0;i<n;i++)
			{
				mreal d = vv->v(i);
				ac.AddLabel(mgl_ftoa(d,ac.stl.c_str()),d);
			}
		else
		{
			wchar_t buf[64];
			for(long i=0;i<n;i++)
			{
				mreal d = vv->v(i);
				mglprintf(buf,64,ac.t.c_str(),d);
				ac.AddLabel(buf,d);
			}
		}
	}
	else	{	UpdateAxis();	AdjustTicks(ac,fa!=0);	}
	// hint for using standard label drawing function
	SetPenPal(TickStl);
	for(size_t i=0;i<ac.txt.size();i++)
	{
		mreal d = fa?fa->Calc(0,0,0,ac.txt[i].val):ac.txt[i].val;
		ac.txt[i].val = d = 2*(d-FMin.c)/(FMax.c-FMin.c)-1;
		if(fabs(d)>1)	continue;	// this is factor
//		mreal d = ac.txt[i].val = GetA(ac.txt[i].val)*2-1;
		p1 = p2 = mglPoint((ss*d+1)*w+x, (ss*d+1)*h+y, s3);
		switch(where)
		{
			case 1:	p1.x = x;	p2.x = x+0.1*w;	break;
			case 2:	p1.y = y-0.1*h;	p2.y = y;	break;
			case 3:	p1.y = y;	p2.y = y+0.1*h;	break;
			default:p1.x = x-0.1*w;	p2.x = x;	break;
		}
		mglPoint p3(0.75*p1.x+0.25*p2.x, 0.75*p1.y+0.25*p2.y, s3);
		mglPoint p4(0.25*p1.x+0.75*p2.x, 0.25*p1.y+0.75*p2.y, s3);
//		line_plot(AddPnt(&M, p1), AddPnt(&M, p2));
		line_plot(AddPnt(&M, p1), AddPnt(&M, p3));
		line_plot(AddPnt(&M, p4), AddPnt(&M, p2));
	}
	ac.dir.Set(ss*w,ss*h,0);	ac.a.Set(0,0,0);
	ac.org.Set(w+x,h+y,s3+1);	ac.b.Set(0,0,0);
	switch(where)
	{
		case 1:	ac.dir.x = 0;	ac.a.x= 1;	ac.org.x = x+0.1*w-(get(MGL_ENABLE_RTEXT)?0:0.06);	break;
		case 2:	ac.dir.y = 0;	ac.a.y=-1;	ac.org.y = y-0.1*h;	break;
		case 3:	ac.dir.y = 0;	ac.a.y= 1;	ac.org.y = y+0.1*h;	break;
		default:ac.dir.x = 0;	ac.a.x=-1;	ac.org.x = x-0.1*w;	break;
	}
	SetPenPal(AxisStl);
	bool inv = where!=3 && where!=0;
	ac.ns = where;	ac.angl=NAN;	// NOTE ns isn't used for colorbar
	if(text)	DrawLabels(ac,inv,&M);
	clr(MGL_DISABLE_SCALE);	EndGroup();
}
//-----------------------------------------------------------------------------
