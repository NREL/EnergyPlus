/***************************************************************************
 * exec_1d.cpp is part of Math Graphic Library
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
//	1D
//-----------------------------------------------------------------------------
int static mgls_area(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Area(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Area(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Area(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Area(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Area(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Area(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_axial(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Axial(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Axial(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Axial(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Axial(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_bars(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Bars(*(a[0].d), "",opt);
	else if(!strcmp(k,"ds"))	gr->Bars(*(a[0].d), a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Bars(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Bars(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd")) 	gr->Bars(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Bars(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_barh(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Barh(*(a[0].d), "",opt);
	else if(!strcmp(k,"ds"))	gr->Barh(*(a[0].d), a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Barh(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Barh(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_bifurcation(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"nd"))	gr->Bifurcation(a[0].v,*(a[1].d),"",opt);
	else if(!strcmp(k,"nds"))	gr->Bifurcation(a[0].v,*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ns"))	gr->Bifurcation(a[0].v,a[1].s.s,"",opt);
	else if(!strcmp(k,"nss"))	gr->Bifurcation(a[0].v,a[1].s.s,a[2].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_boxplot(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->BoxPlot(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->BoxPlot(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->BoxPlot(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->BoxPlot(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_candle(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Candle(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Candle(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Candle(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Candle(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Candle(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Candle(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Candle(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Candle(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->Candle(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))gr->Candle(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_chart(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Chart(*(a[0].d), "",opt);
	else if(!strcmp(k,"ds"))	gr->Chart(*(a[0].d), a[1].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cones(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Cones(*(a[0].d), "",opt);
	else if(!strcmp(k,"ds"))	gr->Cones(*(a[0].d), a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Cones(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Cones(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd")) 	gr->Cones(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Cones(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_error(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Error(*(a[0].d),*(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Error(*(a[0].d),*(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Error(*(a[0].d),*(a[1].d),*(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Error(*(a[0].d),*(a[1].d),*(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Error(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Error(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_iris(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ds"))	gr->Iris(*(a[0].d),a[1].s.w,"",opt);
	else if(!strcmp(k,"dss"))	gr->Iris(*(a[0].d),a[1].s.w,a[2].s.s,opt);
	else if(!strcmp(k,"dds"))	gr->Iris(*(a[0].d),*(a[1].d),a[2].s.w,"",opt);
	else if(!strcmp(k,"ddss"))	gr->Iris(*(a[0].d),*(a[1].d),a[2].s.w,a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_label(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ds"))	gr->Label(*(a[0].d), a[1].s.w, "",opt);
	else if(!strcmp(k,"dss"))	gr->Label(*(a[0].d), a[1].s.w, a[2].s.s,opt);
	else if(!strcmp(k,"dds"))	gr->Label(*(a[0].d), *(a[1].d), a[2].s.w, "",opt);
	else if(!strcmp(k,"ddss"))	gr->Label(*(a[0].d), *(a[1].d), a[2].s.w, a[3].s.s,opt);
	else if(!strcmp(k,"ddds"))	gr->Label(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.w, "",opt);
	else if(!strcmp(k,"dddss"))	gr->Label(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.w, a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_lamerey(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"nd"))	gr->Lamerey(a[0].v,*(a[1].d),"",opt);
	else if(!strcmp(k,"nds"))	gr->Lamerey(a[0].v,*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ns"))	gr->Lamerey(a[0].v,a[1].s.s,"",opt);
	else if(!strcmp(k,"nss"))	gr->Lamerey(a[0].v,a[1].s.s,a[2].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_mark(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Mark(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Mark(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd")) 	gr->Mark(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Mark(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd")) 	gr->Mark(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Mark(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ohlc(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dddd"))	gr->OHLC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->OHLC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->OHLC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d), "",opt);
	else if(!strcmp(k,"ddddds"))	gr->OHLC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d), a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_plot(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Plot(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Plot(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Plot(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Plot(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Plot(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Plot(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"nns"))	gr->Mark(mglPoint(a[0].v,a[1].v,NAN),a[2].s.s);
	else if(!strcmp(k,"nnns"))	gr->Mark(mglPoint(a[0].v,a[1].v,a[2].v),a[3].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_radar(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Radar(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Radar(*(a[0].d),a[1].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_region(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))		gr->Region(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Region(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Region(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Region(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Region(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Region(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"dddddd"))	gr->Region(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"dddddds"))	gr->Region(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_stem(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Stem(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Stem(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Stem(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Stem(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Stem(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Stem(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_step(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Step(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Step(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Step(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Step(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Step(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Step(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_table(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Table(*(a[0].d), L"", "#|",opt);
	else if(!strcmp(k,"ds"))	gr->Table(*(a[0].d), a[1].s.w, "#|",opt);
	else if(!strcmp(k,"dss"))	gr->Table(*(a[0].d), a[1].s.w, a[2].s.s,opt);
	else if(!strcmp(k,"nnd"))	gr->Table(a[0].v, a[1].v, *(a[2].d), L"", "#|",opt);
	else if(!strcmp(k,"nnds"))	gr->Table(a[0].v, a[1].v, *(a[2].d), a[3].s.w, "#|",opt);
	else if(!strcmp(k,"nndss"))	gr->Table(a[0].v, a[1].v, *(a[2].d), a[3].s.w, a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tape(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Tape(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Tape(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Tape(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Tape(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Tape(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Tape(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tens(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Tens(*(a[0].d),*(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Tens(*(a[0].d),*(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Tens(*(a[0].d),*(a[1].d),*(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Tens(*(a[0].d),*(a[1].d),*(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd")) 	gr->Tens(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Tens(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), a[4].s.s,opt);
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_textmark(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ds"))	gr->TextMark(*(a[0].d),a[1].s.w,"",opt);
	else if(!strcmp(k,"dss"))	gr->TextMark(*(a[0].d),a[1].s.w,a[2].s.s,opt);
	else if(!strcmp(k,"dds"))	gr->TextMark(*(a[0].d),*(a[1].d),a[2].s.w,"",opt);
	else if(!strcmp(k,"ddss"))	gr->TextMark(*(a[0].d),*(a[1].d),a[2].s.w,a[3].s.s,opt);
	else if(!strcmp(k,"ddds"))	gr->TextMark(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.w,"",opt);
	else if(!strcmp(k,"dddss"))	gr->TextMark(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.w,a[4].s.s,opt);
	else if(!strcmp(k,"dddds"))	gr->TextMark(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.w,"",opt);
	else if(!strcmp(k,"ddddss"))gr->TextMark(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.w,a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_torus(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Torus(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Torus(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tube(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dn"))
		gr->Tube(*(a[0].d),a[1].v,"",opt);
	else if(!strcmp(k,"dns"))
		gr->Tube(*(a[0].d),a[1].v,a[2].s.s,opt);
	else if(!strcmp(k,"dd"))
		gr->Tube(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))
		gr->Tube(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddn"))
		gr->Tube(*(a[0].d),*(a[1].d),a[2].v,"",opt);
	else if(!strcmp(k,"ddns"))
		gr->Tube(*(a[0].d),*(a[1].d),a[2].v,a[3].s.s,opt);
	else if(!strcmp(k,"ddd"))
		gr->Tube(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))
		gr->Tube(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddn"))
		gr->Tube(*(a[0].d),*(a[1].d),*(a[2].d),a[3].v,"",opt);
	else if(!strcmp(k,"dddns"))
		gr->Tube(*(a[0].d),*(a[1].d),*(a[2].d),a[3].v,a[4].s.s,opt);
	else if(!strcmp(k,"dddd"))
		gr->Tube(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))
		gr->Tube(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
//	2D
//-----------------------------------------------------------------------------
int static mgls_belt(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Belt(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Belt(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Belt(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Belt(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_beltc(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->BeltC(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->BeltC(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->BeltC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->BeltC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_boxs(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Boxs(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Boxs(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Boxs(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Boxs(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cont(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Cont(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Cont(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->Cont(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Cont(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Cont(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Cont(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Cont(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Cont(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,opt);
	else if(!strcmp(k,"ndddd"))	gr->ContGen(a[0].v, *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d),"",opt);
	else if(!strcmp(k,"ndddds"))	gr->ContGen(a[0].v, *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contd(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContD(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->ContD(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->ContD(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->ContD(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->ContD(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->ContD(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->ContD(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->ContD(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contf(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContF(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->ContF(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->ContF(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->ContF(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->ContF(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->ContF(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->ContF(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->ContF(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,opt);
	else if(!strcmp(k,"nndddd"))	gr->ContFGen(a[0].v, a[1].v, *(a[2].d), *(a[3].d), *(a[4].d), *(a[5].d),"",opt);
	else if(!strcmp(k,"nndddds"))	gr->ContFGen(a[0].v, a[1].v, *(a[2].d), *(a[3].d), *(a[4].d), *(a[5].d), a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contp(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dddd"))	gr->ContP(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->ContP(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->ContP(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), "",opt);
	else if(!strcmp(k,"ddddds"))	gr->ContP(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contv(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContV(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->ContV(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dd"))	gr->ContV(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->ContV(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->ContV(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->ContV(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->ContV(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->ContV(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_dens(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Dens(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Dens(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Dens(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Dens(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fall(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Fall(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Fall(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Fall(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Fall(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_grid2(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Grid(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Grid(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Grid(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Grid(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_map(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Map(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Map(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Map(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Map(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_mesh(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Mesh(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Mesh(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Mesh(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Mesh(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_pmap(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Pmap(*(a[0].d), *(a[1].d), "",opt);
	else if(!strcmp(k,"dds"))	gr->Pmap(*(a[0].d), *(a[1].d), a[2].s.s,opt);
	else if(!strcmp(k,"ddd")) 	gr->Pmap(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Pmap(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd")) 	gr->Pmap(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Pmap(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_stfa(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddn"))
		gr->STFA(*(a[0].d),*(a[1].d), mgl_int(a[2].v), "",opt);
	else if(!strcmp(k,"ddns"))
		gr->STFA(*(a[0].d),*(a[1].d), mgl_int(a[2].v), a[3].s.s,opt);
	else if(!strcmp(k,"ddddn"))
		gr->STFA(*(a[0].d),*(a[1].d), *(a[2].d),*(a[3].d), mgl_int(a[4].v), "",opt);
	else if(!strcmp(k,"ddddns"))
		gr->STFA(*(a[0].d),*(a[1].d), *(a[2].d),*(a[3].d), mgl_int(a[4].v), a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surf(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Surf(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Surf(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Surf(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Surf(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surfc(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->SurfC(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->SurfC(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->SurfC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->SurfC(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surfa(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->SurfA(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->SurfA(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->SurfA(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->SurfA(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surfca(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))	gr->SurfCA(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->SurfCA(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->SurfCA(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))gr->SurfCA(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tile(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Tile(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Tile(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Tile(*(a[0].d), *(a[1].d), *(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Tile(*(a[0].d), *(a[1].d), *(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Tile(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Tile(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tiles(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->TileS(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->TileS(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->TileS(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->TileS(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->TileS(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))	gr->TileS(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_triplot(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))	gr->TriPlot(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->TriPlot(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->TriPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->TriPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->TriPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))gr->TriPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_quadplot(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))	gr->QuadPlot(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->QuadPlot(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->QuadPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->QuadPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->QuadPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))gr->QuadPlot(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tricont(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dddd"))
		gr->TriCont(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))
		gr->TriCont(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))
		gr->TriContV(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))
		gr->TriContV(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else if(!strcmp(k,"dddddd"))
		gr->TriCont(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"dddddds"))
		gr->TriCont(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tricontv(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dddd"))
		gr->TriContVt(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))
		gr->TriContVt(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))
		gr->TriContVt(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))
		gr->TriContVt(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else if(!strcmp(k,"dddddd"))
		gr->TriContVt(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"dddddds"))
		gr->TriContVt(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
//	3D
//-----------------------------------------------------------------------------
int static mgls_beam(mglGraph *gr, long , mglArg *a, const char *k, const char *)	// NOTE beam can be made obsolete ???
{
	int res=0;
	if(!strcmp(k,"ddddn"))
		gr->Beam(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].v,"",0, 3);
	else if(!strcmp(k,"ddddns"))
		gr->Beam(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].v,a[5].s.s,0, 3);
	else if(!strcmp(k,"ddddnsn"))
		gr->Beam(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].v,a[5].s.s,mgl_int(a[6].v), 3);
	else if(!strcmp(k,"ddddnsnn"))
		gr->Beam(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].v,a[5].s.s,mgl_int(a[6].v), mgl_int(a[7].v));
	else if(!strcmp(k,"nddddn"))
		gr->Beam(a[0].v,*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].v,"",0);
	else if(!strcmp(k,"nddddns"))
		gr->Beam(a[0].v,*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].v,a[6].s.s,0);
	else if(!strcmp(k,"nddddnsn"))
		gr->Beam(a[0].v,*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].v,a[6].s.s,mgl_int(a[7].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cloud(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Cloud(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Cloud(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Cloud(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Cloud(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cont3(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Cont3(*(a[0].d), "", -1,opt);
	else if(!strcmp(k,"ds"))	gr->Cont3(*(a[0].d), a[1].s.s, -1,opt);
	else if(!strcmp(k,"dsn"))	gr->Cont3(*(a[0].d), a[1].s.s, mgl_int(a[2].v),opt);
	else if(!strcmp(k,"dd"))	gr->Cont3(*(a[0].d), *(a[1].d), "", -1,opt);
	else if(!strcmp(k,"dds"))	gr->Cont3(*(a[0].d), *(a[1].d), a[2].s.s,-1,opt);
	else if(!strcmp(k,"ddsn"))	gr->Cont3(*(a[0].d), *(a[1].d), a[2].s.s,mgl_int(a[3].v),opt);
	else if(!strcmp(k,"dddd"))	gr->Cont3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "", -1,opt);
	else if(!strcmp(k,"dddds"))	gr->Cont3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,-1,opt);
	else if(!strcmp(k,"ddddsn"))	gr->Cont3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,mgl_int(a[5].v),opt);
	else if(!strcmp(k,"ddddd"))	gr->Cont3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), "", -1,opt);
	else if(!strcmp(k,"ddddds"))	gr->Cont3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s,-1,opt);
	else if(!strcmp(k,"dddddsn"))gr->Cont3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s,mgl_int(a[6].v),opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contf3(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContF3(*(a[0].d), "", -1,opt);
	else if(!strcmp(k,"ds"))	gr->ContF3(*(a[0].d), a[1].s.s, -1,opt);
	else if(!strcmp(k,"dsn"))	gr->ContF3(*(a[0].d), a[1].s.s, mgl_int(a[2].v),opt);
	else if(!strcmp(k,"dd"))	gr->ContF3(*(a[0].d), *(a[1].d), "", -1,opt);
	else if(!strcmp(k,"dds"))	gr->ContF3(*(a[0].d), *(a[1].d), a[2].s.s,-1,opt);
	else if(!strcmp(k,"ddsn"))	gr->ContF3(*(a[0].d), *(a[1].d), a[2].s.s,mgl_int(a[3].v),opt);
	else if(!strcmp(k,"dddd"))	gr->ContF3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), "", -1,opt);
	else if(!strcmp(k,"dddds"))	gr->ContF3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,-1,opt);
	else if(!strcmp(k,"ddddsn"))	gr->ContF3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s,mgl_int(a[5].v),opt);
	else if(!strcmp(k,"ddddd"))	gr->ContF3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), "", -1,opt);
	else if(!strcmp(k,"ddddds"))	gr->ContF3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s,-1,opt);
	else if(!strcmp(k,"dddddsn"))gr->ContF3(*(a[0].d), *(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s,mgl_int(a[6].v),opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contx(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContX(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->ContX(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->ContX(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contfx(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContFX(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->ContFX(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->ContFX(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_conty(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContY(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->ContY(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->ContY(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contfy(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContFY(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->ContFY(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->ContFY(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contz(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContZ(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->ContZ(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->ContZ(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_contfz(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->ContFZ(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->ContFZ(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->ContFZ(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_crust(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))	gr->Crust(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Crust(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_dens3(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Dens3(*(a[0].d),"",-1,opt);
	else if(!strcmp(k,"ds"))	gr->Dens3(*(a[0].d),a[1].s.s,-1,opt);
	else if(!strcmp(k,"dsn"))	gr->Dens3(*(a[0].d),a[1].s.s,mgl_int(a[2].v),opt);
	else if(!strcmp(k,"dddd"))	gr->Dens3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"", -1,opt);
	else if(!strcmp(k,"dddds"))	gr->Dens3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,-1,opt);
	else if(!strcmp(k,"ddddsn"))	gr->Dens3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,mgl_int(a[5].v),opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_densx(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->DensX(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->DensX(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->DensX(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_densy(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->DensY(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->DensY(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->DensY(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_densz(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->DensZ(*(a[0].d),"",NAN,opt);
	else if(!strcmp(k,"ds"))	gr->DensZ(*(a[0].d),a[1].s.s,NAN,opt);
	else if(!strcmp(k,"dsn"))	gr->DensZ(*(a[0].d),a[1].s.s,a[2].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_dots(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))	gr->Dots(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Dots(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Dots(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Dots(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"ddddd"))	gr->Dots(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"ddddds"))gr->Dots(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_grid3(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Grid3(*(a[0].d),"",-1,opt);
	else if(!strcmp(k,"ds"))	gr->Grid3(*(a[0].d),a[1].s.s,-1,opt);
	else if(!strcmp(k,"dsn"))	gr->Grid3(*(a[0].d),a[1].s.s,mgl_int(a[2].v),opt);
	else if(!strcmp(k,"dddd"))	gr->Grid3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",-1,opt);
	else if(!strcmp(k,"dddds"))	gr->Grid3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,-1,opt);
	else if(!strcmp(k,"ddddsn"))gr->Grid3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,mgl_int(a[5].v),opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surf3(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Surf3(*(a[0].d),"",opt);
	else if(!strcmp(k,"ds"))	gr->Surf3(*(a[0].d),a[1].s.s,opt);
	else if(!strcmp(k,"dn"))	gr->Surf3(a[1].v,*(a[0].d),"",opt);
	else if(!strcmp(k,"dns"))	gr->Surf3(a[1].v,*(a[0].d),a[2].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Surf3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Surf3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), a[4].s.s,opt);
	else if(!strcmp(k,"ddddn"))	gr->Surf3(a[4].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"ddddns"))gr->Surf3(a[4].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[5].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surf3c(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Surf3C(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Surf3C(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddn"))	gr->Surf3C(a[2].v,*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"ddns"))	gr->Surf3C(a[2].v,*(a[0].d),*(a[1].d),a[3].s.s,opt);
	else if(!strcmp(k,"ddddd"))
		gr->Surf3C(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d), "",opt);
	else if(!strcmp(k,"ddddds"))
		gr->Surf3C(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d), a[5].s.s,opt);
	else if(!strcmp(k,"dddddn"))
		gr->Surf3C(a[5].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"dddddns"))
		gr->Surf3C(a[5].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surf3a(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Surf3A(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Surf3A(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddn"))	gr->Surf3A(a[2].v,*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"ddns"))	gr->Surf3A(a[2].v,*(a[0].d),*(a[1].d),a[3].s.s,opt);
	else if(!strcmp(k,"ddddd"))
		gr->Surf3A(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d), "",opt);
	else if(!strcmp(k,"ddddds"))
		gr->Surf3A(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d), a[5].s.s,opt);
	else if(!strcmp(k,"dddddn"))
		gr->Surf3A(a[5].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),"",opt);
	else if(!strcmp(k,"dddddns"))
		gr->Surf3A(a[5].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_surf3ca(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))	gr->Surf3CA(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Surf3CA(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddn"))	gr->Surf3CA(a[4].v,*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"dddns"))	gr->Surf3CA(a[4].v,*(a[0].d),*(a[1].d),*(a[2].d),a[4].s.s,opt);
	else if(!strcmp(k,"dddddd"))
		gr->Surf3CA(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d), "",opt);
	else if(!strcmp(k,"dddddds"))
		gr->Surf3CA(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d), a[6].s.s,opt);
	else if(!strcmp(k,"ddddddn"))
		gr->Surf3CA(a[6].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"ddddddns"))
		gr->Surf3CA(a[6].v,*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[7].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
//	Vect
//-----------------------------------------------------------------------------
int static mgls_dew(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Dew(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Dew(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Dew(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Dew(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_flow(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Flow(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Flow(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Flow(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Flow(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Flow(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Flow(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"dddddd"))
		gr->Flow(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"dddddds"))
		gr->Flow(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else if(!strcmp(k,"nndd"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,NAN), *(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"nndds"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,NAN), *(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"nndddd"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,NAN), *(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"nndddds"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,NAN), *(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else if(!strcmp(k,"nnnddd"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,a[2].v), *(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"nnnddds"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,a[2].v), *(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else if(!strcmp(k,"nnndddddd"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,a[2].v), *(a[3].d),*(a[4].d),*(a[5].d),*(a[6].d),*(a[7].d),*(a[8].d),"",opt);
	else if(!strcmp(k,"nnndddddds"))
		gr->FlowP(mglPoint(a[0].v,a[1].v,a[2].v), *(a[3].d),*(a[4].d),*(a[5].d),*(a[6].d),*(a[7].d),*(a[8].d),a[9].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_flow3(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))	gr->Flow3(*(a[0].d),*(a[1].d),*(a[2].d),"",-1,opt);
	else if(!strcmp(k,"ddds"))	gr->Flow3(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,-1,opt);
	else if(!strcmp(k,"dddsn"))	gr->Flow3(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,a[4].v,opt);
	else if(!strcmp(k,"dddddd"))
		gr->Flow3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",-1,opt);
	else if(!strcmp(k,"dddddds"))
		gr->Flow3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,-1,opt);
	else if(!strcmp(k,"ddddddsn"))
		gr->Flow3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,a[7].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_grad(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->Grad(*(a[0].d), "",opt);
	else if(!strcmp(k,"ds"))	gr->Grad(*(a[0].d), a[1].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Grad(*(a[0].d),*(a[1].d),*(a[2].d), "",opt);
	else if(!strcmp(k,"ddds"))	gr->Grad(*(a[0].d),*(a[1].d),*(a[2].d), a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Grad(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), "",opt);
	else if(!strcmp(k,"dddds"))	gr->Grad(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d), a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_pipe(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Pipe(*(a[0].d),*(a[1].d),"",0.05,opt);
	else if(!strcmp(k,"dds"))	gr->Pipe(*(a[0].d),*(a[1].d),a[2].s.s,0.05,opt);
	else if(!strcmp(k,"ddsn"))	gr->Pipe(*(a[0].d),*(a[1].d),a[2].s.s,a[3].v,opt);
	else if(!strcmp(k,"dddd"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",0.05,opt);
	else if(!strcmp(k,"dddds"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,0.05,opt);
	else if(!strcmp(k,"ddddsn"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,a[5].v,opt);
	else if(!strcmp(k,"ddd"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),"",0.05,opt);
	else if(!strcmp(k,"ddds"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,0.05,opt);
	else if(!strcmp(k,"dddsn"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,a[4].v,opt);
	else if(!strcmp(k,"dddddd"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",0.05,opt);
	else if(!strcmp(k,"dddddds"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,0.05,opt);
	else if(!strcmp(k,"ddddddsn"))
		gr->Pipe(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,a[7].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_traj(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dddd"))
		gr->Traj(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))
		gr->Traj(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"dddddd"))
		gr->Traj(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"dddddds"))
		gr->Traj(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_vect(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"dd"))	gr->Vect(*(a[0].d),*(a[1].d),"",opt);
	else if(!strcmp(k,"dds"))	gr->Vect(*(a[0].d),*(a[1].d),a[2].s.s,opt);
	else if(!strcmp(k,"ddd"))	gr->Vect(*(a[0].d),*(a[1].d),*(a[2].d),"",opt);
	else if(!strcmp(k,"ddds"))	gr->Vect(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,opt);
	else if(!strcmp(k,"dddd"))	gr->Vect(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),"",opt);
	else if(!strcmp(k,"dddds"))	gr->Vect(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),a[4].s.s,opt);
	else if(!strcmp(k,"dddddd"))	gr->Vect(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",opt);
	else if(!strcmp(k,"dddddds"))	gr->Vect(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_vect3(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(!strcmp(k,"ddd"))
		gr->Vect3(*(a[0].d),*(a[1].d),*(a[2].d),"",-1,opt);
	else if(!strcmp(k,"ddds"))
		gr->Vect3(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,-1,opt);
	else if(!strcmp(k,"dddsn"))
		gr->Vect3(*(a[0].d),*(a[1].d),*(a[2].d),a[3].s.s,a[4].v,opt);
	else if(!strcmp(k,"dddddd"))
		gr->Vect3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),"",-1,opt);
	else if(!strcmp(k,"dddddds"))
		gr->Vect3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,-1,opt);
	else if(!strcmp(k,"ddddddsn"))
		gr->Vect3(*(a[0].d),*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),*(a[5].d),a[6].s.s,a[7].v,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
mglCommand mgls_grf_cmd[] = {
	{"area",_("Draw area plot for 1D data"),"area Ydat ['fmt']|Xdat Ydat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_area ,7},
	{"axial",_("Draw surfaces of contour lines rotation"),"axial Zdat ['fmt' num]|Xdat Ydat Zdat ['fmt' num]", mgls_axial ,8},
	{"barh",_("Draw horizontal bars for 1D data"), "barh Ydat ['fmt' above]|Xdat Ydat ['fmt' above]", mgls_barh ,7},
	{"bars",_("Draw bars for 1D data"),"bars Ydat ['fmt' above]|Xdat Ydat ['fmt' above]|Xdat Ydat Zdat ['fmt' above]", mgls_bars ,7},
	{"beam",_("Draw quasi-optical beam"),"beam Ray G1 G2 Adat r ['sch' flag num]|val Ray G1 G2 Adat r ['sch' flag num]", mgls_beam ,9},
	{"belt",_("Draw belts"),"belt Zdat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_belt ,8},
	{"beltc",_("Draw belts colored by other data"),"beltc Zdat Cdat ['fmt']|Xdat Ydat Zdat Cdat ['fmt']", mgls_beltc ,8},
	{"bifurcation",_("Draw Bifurcation diagram"),"bifurcation dx Func ['fmt']|dx 'func' ['fmt']", mgls_bifurcation,13},
	{"boxplot",_("Draw boxplot for 2D data"),"boxplot Ydat ['fmt']|Xdat Ydat ['fmt']", mgls_boxplot ,7},
	{"boxs",_("Draw boxes"),"boxs Zdat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_boxs ,8},
	{"candle",_("Draw candlestick chart"),"candle candle Vdat1 ['fmt']|Vdat1 Vdat2 ['fmt']|Vdat1 Ydat1 Ydat2 ['fmt']||Vdat1 Vdat2 Ydat1 Ydat2 ['fmt']|Xdat Vdat1 Vdat2 Ydat1 Ydat2 ['fmt']", mgls_candle ,7},
	{"chart",_("Draw chart"),"chart Dat ['fmt']", mgls_chart ,7},
	{"cloud",_("Draw cloud"),"cloud Adat ['fmt']|Xdat Ydat Zdat Adat ['fmt']", mgls_cloud ,9},
	{"cones",_("Draw cones for 1D data"),"cones Ydat ['fmt' above]|Xdat Ydat ['fmt' above]|Xdat Ydat Zdat ['fmt' above]", mgls_cones ,7},
	{"cont",_("Draw contour lines"),"cont Zdat ['fmt']|Vdat Zdat ['fmt']|Xdat Ydat Zdat ['fmt']|Vdat Xdat Ydat Zdat ['fmt']|val Adat Xdat Ydat Zdat ['fmt']", mgls_cont ,8},
	{"cont3",_("Draw contour lines for 3D data"),"cont3 Adat 'dir' [val 'fmt' num]|Vdat Adat 'dir' [val 'fmt']|Xdat Ydat Zdat Adat 'dir' [val 'fmt' num]|Vdat Xdat Ydat Zdar Adat 'dir' [val 'fmt']", mgls_cont3 ,9},
	{"contd",_("Draw solid contours with manual colors"),"contd Zdat ['fmt']|Vdat Zdat ['fmt']|Xdat Ydat Zdat ['fmt']|Vdat Xdat Ydat Zdat ['fmt']", mgls_contd ,8},
	{"contf",_("Draw solid contours"),"contf Zdat ['fmt']|Vdat Zdat ['fmt']|Xdat Ydat Zdat ['fmt']|Vdat Xdat Ydat Zdat ['fmt']|v1 v2 Adat Xdat Ydat Zdat ['fmt']", mgls_contf ,8},
	{"contf3",_("Draw solid contour lines for 3D data"),"contf3 Adat 'dir' [val 'fmt' num]|Vdat Adat 'dir' [val 'fmt']|Xdat Ydat Zdat Adat 'dir' [val 'fmt' num]|Vdat Xdat Ydat Zdar Adat 'dir' [val 'fmt']", mgls_contf3 ,9},
	{"contfx",_("Draw solid contour lines at x-slice (or x-plane)"),"contfx Dat ['fmt' pos num]", mgls_contfx ,0},
	{"contfy",_("Draw solid contour lines at y-slice (or y-plane)"),"contfy Dat ['fmt' pos num]", mgls_contfy ,0},
	{"contfz",_("Draw solid contour lines at z-slice (or z-plane)"),"contfz Dat ['fmt' pos num]", mgls_contfz ,0},
	{"contp",_("Draw contour lines on parametric surface"),"contp Xdat Ydat Zdat Adat ['fmt' num zpos]|Vdat Xdat Ydat Zdat Adat ['fmt' zpos]", mgls_contp ,8},
	{"contv",_("Draw contour tubes"),"contv Zdat ['fmt' num zpos]|Vdat Zdat ['fmt' zpos]|Xdat Ydat Zdat ['fmt' num zpos]|Vdat Xdat Ydat Zdat ['fmt' zpos]", mgls_contv ,0},
	{"contx",_("Draw contour lines at x-slice (or x-plane)"),"contx Dat ['fmt' pos num]", mgls_contx ,0},
	{"conty",_("Draw contour lines at y-slice (or y-plane)"),"conty Dat ['fmt' pos num]", mgls_conty ,0},
	{"contz",_("Draw contour lines at z-slice (or z-plane)"),"contz Dat ['fmt' pos num]", mgls_contz ,0},
	{"crust",_("Draw reconstructed surface for arbitrary data points"),"crust Xdat Ydat Zdat ['fmt']", mgls_crust ,0},
	{"dens",_("Draw density plot"),"dens Zdat ['fmt' zpos]|Xdat Ydat Zdat ['fmt' zpos]", mgls_dens ,8},
	{"dens3",_("Draw density plot at slices of 3D data"),"dens3 Adat 'dir' [pos 'fmt']|Xdat Ydat Zdat Adat 'dir' [pos 'fmt']", mgls_dens3 ,9},
	{"densx",_("Draw density plot at x-slice (or x-plane)"),"densx Dat ['fmt' pos]", mgls_densx ,0},
	{"densy",_("Draw density plot at y-slice (or y-plane)"),"densy Dat ['fmt' pos]", mgls_densy ,0},
	{"densz",_("Draw density plot at z-slice (or z-plane)"),"densz Dat ['fmt' pos]", mgls_densz ,0},
	{"dew",_("Draw dew plot"),"dew Udat Vdat ['fmt']|Xdat Ydat Udat Vdat ['fmt']", mgls_dew ,11},
	{"dots",_("Draw dots for arbitrary data points"),"dots Xdat Ydat Zdat ['fmt']|Xdat Ydat Zdat Adat ['fmt']|Xdat Ydat Zdat Cdat Adat ['fmt']", mgls_dots ,9},
	{"error",_("Draw error boxes"),"error Ydat Yerr ['fmt']|Xdat Ydat Yerr ['fmt']|Xdat Ydat Xerr Yerr ['fmt']", mgls_error ,7},
	{"fall",_("Draw waterfalls"),"fall Zdat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_fall ,8},
	{"flow",_("Draw flow threads for vector field"),"flow Udat Vdat ['fmt']|Xdat Ydat Udat Vdat ['fmt']|Udat Vdat Wdat ['fmt']|Xdat Ydat Zdat Udat Vdat Wdat ['fmt']|\
	x0 y0 Udat Vdat ['fmt']|x0 y0 Xdat Ydat Udat Vdat ['fmt']|x0 y0 z0 Udat Vdat Wdat ['fmt']|x0 y0 z0 Xdat Ydat Zdat Udat Vdat Wdat ['fmt']", mgls_flow ,11},
	{"flow3",_("Draw flow threads from plain for vector field"),"flow3 Udat Udat Vdat Wdat ['fmt' num]|Xdat Ydat Zdat Udat Vdat Wdat ['fmt' num]", mgls_flow3 ,11},
	{"grad",_("Draw gradient lines for scalar field"),"grad Phi ['fmt' num]|Xdat Ydat Phi ['fmt' num]|Xdat Ydat Zdat Phi ['fmt' num]", mgls_grad ,8},
	{"grid2",_("Draw grid for data array(s)"),"grid2 Zdat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_grid2 ,8},
	{"grid3",_("Draw grid at slices of 3D data"),"grid3 Adat 'dir' [pos 'fmt']|Xdat Ydat Zdat Adat 'dir' [pos 'fmt']", mgls_grid3 ,9},
	{"iris",_("Draw Iris plots"),"iris Dats 'ids' ['fmt']|Dats Ranges 'ids' ['fmt']", mgls_iris,13},
	{"label",_("Draw label at arbitrary position"),"label Ydat 'txt' ['fmt'='']|Xdat Ydat 'txt' ['fmt'='']|Xdat Ydat Zdat 'txt' ['fmt'='']", mgls_label ,7},
	{"lamerey",_("Draw Lamerey diagram"),"lamerey x0 Func ['fmt']|x0 'func' ['fmt']", mgls_lamerey ,13},
	{"map",_("Draw mapping plot"),"map Udat Vdat ['fmt']|Xdat Ydat Udat Vdat ['fmt']", mgls_map ,10},
	{"mark",_("Draw mark plot for 1D data"),"mark Ydat Rdat ['fmt']|Xdat Ydat Rdat ['fmt']|Xdat Ydat Zdat Rdat ['fmt']", mgls_mark ,7},
	{"mesh",_("Draw mesh surface"),"mesh Zdat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_mesh ,8},
	{"ohlc",_("Draw Open-High-Low-Close (OHLC) diagram"),"ohlc Odat Hdat Ldat Cdat ['fmt']|Xdat Odat Hdat Ldat Cdat ['fmt']", mgls_ohlc ,7},
	{"pipe",_("Draw flow pipes for vector field"),"pipe Udat Vdat ['fmt' rad num]|Xdat Ydat Udat Vdat ['fmt' rad num]|Udat Vdat Wdat ['fmt' rad num]|Xdat Ydat Zdat Udat Vdat Wdat ['fmt' rad num]", mgls_pipe ,11},
	{"plot",_("Draw usual plot for 1D data"),"plot Ydat ['fmt']|Xdat Ydat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_plot ,7},
	{"pmap",_("Draw Poincare map"),"pmap Ydat Rdat ['fmt']|Xdat Ydat Rdat ['fmt']|Xdat Ydat Zdat Rdat ['fmt']", mgls_pmap ,7},
	{"quadplot",_("Draw surface of quadrangles"),"quadplot Idat Xdat Ydat ['fmt']|Idat Xdat Ydat Zdat ['fmt']|Idat Xdat Ydat Zdat Cdat ['fmt'] ", mgls_quadplot ,0},
	{"radar",_("Draw radar chart"),"radar Rdat ['fmt']", mgls_radar ,7},
	{"region",_("Draw filled region (ribbon) between 2 curves"),"region Ydat1 Ydat2 ['fmt']|Xdat Ydat1 Ydat2 ['fmt']||Xdat1 Ydat1 Xdat2 Ydat2 ['fmt']|Xdat1 Ydat1 Zdat1 Xdat2 Ydat2 Zdat2 ['fmt']", mgls_region ,7},
	{"stem",_("Draw stem plot for 1D data"),"stem Ydat ['fmt']|Xdat Ydat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_stem ,7},
	{"step",_("Draw step plot for 1D data"),"step Ydat ['fmt']|Xdat Ydat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_step ,7},
	{"stfa",_("Draw STFA diagram"),"stfa Udat Vdat dn ['fmt']|Xdat Ydat Udat Vdat dn ['fmt']", mgls_stfa ,10},
	{"surf",_("Draw solid surface"),"surf Zdat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_surf ,8},
	{"surf3",_("Draw isosurface for 3D data"),"surf3 Adat ['fmt' num]|Xdat Ydat Zdat Adat ['fmt' num]|Adat val ['fmt']|Xdat Ydat Zdat Adat val ['fmt']", mgls_surf3 ,9},
	{"surf3a",_("Draw isosurface for 3D data transpared by other data"),"surf3a Adat Cdat ['fmt' num]|Xdat Ydat Zdat Adat Cdat ['fmt' num]|Adat Cdat val ['fmt']|Xdat Ydat Zdat Adat Cdat val ['fmt']", mgls_surf3a ,10},
	{"surf3c",_("Draw isosurface for 3D data colored by other data"),"surf3c Adat Cdat ['fmt' num]|Xdat Ydat Zdat Adat Cdat ['fmt' num]|Adat Cdat val ['fmt']|Xdat Ydat Zdat Adat Cdat val ['fmt']", mgls_surf3c ,10},
	{"surf3ca",_("Draw isosurface for 3D data colored and transpared by other data"),"surf3c Adat Cdat Bdat ['fmt' num]|Xdat Ydat Zdat Adat Cdat Bdat ['fmt' num]|Adat Cdat Bdat val ['fmt']|Xdat Ydat Zdat Adat Cdat Bdat val ['fmt']", mgls_surf3ca ,10},
	{"surfa",_("Draw solid surface transpared by other data"),"surfa Zdat Cdat ['fmt']|Xdat Ydat Zdat Cdat ['fmt']", mgls_surfa ,10},
	{"surfc",_("Draw solid surface colored by other data"),"surfc Zdat Cdat ['fmt']|Xdat Ydat Zdat Cdat ['fmt']", mgls_surfc ,10},
	{"surfca",_("Draw solid surface colored and transpared by other data"),"surfca Zdat Cdat Adat ['fmt']|Xdat Ydat Zdat Cdat Adat ['fmt']", mgls_surfca ,10},
	{"table",_("Draw table with data values"),"table Dat ['txt' 'fmt']|x y Dat ['txt' 'fmt']", mgls_table ,7},
	{"tape",_("Draw binormales for 1D data"),"tape Ydat ['fmt']|Xdat Ydat ['fmt']|Xdat Ydat Zdat ['fmt']", mgls_tape ,7},
	{"tens",_("Draw tension plot for 1D data"),"tens Ydat Cdat ['fmt']|Xdat Ydat Cdat ['fmt']|Xdat Ydat Zdat Cdat ['fmt']", mgls_tens ,7},
	{"textmark",_("Draw TeX mark at point position"),"textmark Ydat Rdat 'text' ['fmt']|Xdat Ydat Rdat 'text' ['fmt']|Xdat Ydat Zdat Rdat 'text' ['fmt']", mgls_textmark ,7},
	{"tile",_("Draw horizontal tiles"),"tile Zdat ['fmt']|Xdat Ydat Zdat ['fmt']|Xdat Ydat Zdat Cdat ['fmt']", mgls_tile ,8},
	{"tiles",_("Draw horizontal tiles with variable size"),"tiles Zdat Rdat ['fmt']|Xdat Ydat Zdat Rdat ['fmt']|Xdat Ydat Zdat Rdat Cdat ['fmt']", mgls_tiles ,10},
	{"torus",_("Draw surface of curve rotation"),"torus Rdat ['fmt']|Zdat Rdat ['fmt']", mgls_torus ,7},
	{"traj",_("Draw vectors along a curve"),"traj Xdat Ydat Udat Vdat ['fmt' len]|Xdat Ydat Zdat Udat Vdat Wdat ['fmt' len]", mgls_traj ,11},
	{"tricont",_("Draw contour lines for surface of triangles"),"tricont Idat Xdat Ydat Cdat ['fmt']|Idat Xdat Ydat Zdat Cdat ['fmt']|Vdat Idat Xdat Ydat Cdat ['fmt']|Vdat Idat Xdat Ydat Zdat Cdat ['fmt']", mgls_tricont ,0},
	{"tricontv",_("Draw contour tubes for surface of triangles"),"tricontv Idat Xdat Ydat Cdat ['fmt']|Idat Xdat Ydat Zdat Cdat ['fmt']|Vdat Idat Xdat Ydat Cdat ['fmt']|Vdat Idat Xdat Ydat Zdat Cdat ['fmt']", mgls_tricontv ,0},
	{"triplot",_("Draw surface of triangles"),"triplot Idat Xdat Ydat ['fmt']|Idat Xdat Ydat Zdat ['fmt']|Idat Xdat Ydat Zdat Cdat ['fmt'] ", mgls_triplot ,0},
	{"tube",_("Draw curve by tube"),"tube Ydat Rdat ['fmt']|Ydat rval ['fmt']|Xdat Ydat Rdat ['fmt']|Xdat Ydat rval ['fmt']|Xdat Ydat Zdat Rdat ['fmt']|Xdat Ydat Zdat rval ['fmt']", mgls_tube ,7},
	{"vect",_("Draw vector field"),"vect Udat Vdat ['fmt']|Xdat Ydat Udat Vdat ['fmt']|Udat Vdat Wdat ['fmt']|Xdat Ydat Zdat Udat Vdat Wdat ['fmt']", mgls_vect ,11},
	{"vect3",_("Draw vector field at slices of 3D data"),"vect Udat Vdat Wdat ['fmt' sval]|Xdat Ydat Zdat Udat Vdat Wdat ['fmt' sval]", mgls_vect3 ,11},
{"","","",NULL,0}};
//-----------------------------------------------------------------------------
