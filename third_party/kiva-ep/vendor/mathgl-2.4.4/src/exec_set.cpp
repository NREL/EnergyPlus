/***************************************************************************
 * exec_set.cpp is part of Math Graphic Library
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
//#pragma GCC diagnostic ignored "-Wmisleading-indentation"
wchar_t *mgl_str_copy(const char *s);
//-----------------------------------------------------------------------------
int static mgls_addlegend(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"ss"))	gr->AddLegend(a[0].s.w,a[1].s.s);
	else	res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_addsymbol(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"sdd"))	gr->DefineSymbol(a[0].s[0],*(a[1].d),*(a[2].d));
	else	res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_adjust(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->Adjust();
	else if(!strcmp(k,"s"))	gr->Adjust(a[0].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_alpha(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->Alpha(true);
	else if(!strcmp(k,"n"))	gr->Alpha(a[0].v!=0);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_alphadef(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetAlphaDef(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ambient(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetAmbient(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_arrowsize(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetArrowSize(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_aspect(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))	gr->Aspect(a[0].v, a[1].v, 1);
	else if(!strcmp(k,"nnn"))	gr->Aspect(a[0].v, a[1].v, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_attachlight(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->AttachLight(a[0].v!=0);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_axisstl(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->SetAxisStl();
	else if(!strcmp(k,"s"))	gr->SetAxisStl(a[0].s.s);
	else if(!strcmp(k,"ss"))	gr->SetAxisStl(a[0].s.s, a[1].s.s);
	else if(!strcmp(k,"sss"))	gr->SetAxisStl(a[0].s.s, a[1].s.s, a[2].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_barwidth(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetBarWidth(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_bbox(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn") && a[1].v>=0 && a[0].v>=0)
		gr->SetBBox(mgl_int(a[0].v), mgl_int(a[1].v));
	else if(!strcmp(k,"nnnn"))
		gr->SetBBox(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v), mgl_int(a[3].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_chdir(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))
	{	if(chdir(a[0].s.s))	gr->SetWarn(mglWarnFile,"chdir");	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_clearlegend(mglGraph *gr, long , mglArg *, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->ClearLegend();
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_clf(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->Clf();
	else if(!strcmp(k,"s"))	gr->Clf(a[0].s.s);
	else if(!strcmp(k,"nnn"))	gr->Clf(a[0].v,a[1].v,a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_columnplot(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))	gr->ColumnPlot(mgl_int(a[0].v), mgl_int(a[1].v));
	else if(!strcmp(k,"nnn"))	gr->ColumnPlot(mgl_int(a[0].v), mgl_int(a[1].v), a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_crange(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->SetRange('c',*(a[0].d));
	else if(!strcmp(k,"dn"))	gr->SetRange('c',*(a[0].d),a[1].v);
	else if(!strcmp(k,"nn"))	gr->SetRange('c', a[0].v, a[1].v);
	else if(!strcmp(k,"nnn"))
	{
		if(a[2].v)	gr->AddRange('c', a[0].v, a[1].v);
		else	gr->SetRange('c', a[0].v, a[1].v);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ctick(mglGraph *gr, long n, mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->SetTickTempl('c',a[0].s.w);
	else if(!strcmp(k,"n"))	gr->SetTicks('c',a[0].v,0,0);
	else if(!strcmp(k,"ns"))	gr->SetTicks('c',a[0].v,0,0,a[1].s.w);
	else if(!strcmp(k,"ds"))	gr->SetTicksVal('c', *(a[0].d), a[1].s.w);
	else if(!strcmp(k,"dsn"))	gr->SetTicksVal('c', *(a[0].d), a[1].s.w, a[2].v);
	else if(!strncmp(k,"ns",2))
	{
		std::wstring s;
		std::vector<mreal> v;
		for(long i=0;i<n/2;i++)
		{
			if(a[2*i].type==2 && a[2*i+1].type==1)
			{	v.push_back(a[2*i].v);	s += std::wstring(a[2*i+1].s.w)+L"\n";	}
			else	break;
		}
		gr->SetTicksVal('c',mglDataS(v),s.c_str(),v.size()==1?true:false);
	}	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cut(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetCut(a[0].v != 0);
	else if(!strcmp(k,"nnnnnn"))
		gr->SetCutBox(mglPoint(a[0].v,a[1].v,a[2].v), mglPoint(a[3].v,a[4].v,a[5].v));
	else if(!strcmp(k,"s"))	gr->CutOff(a[0].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_diffuse(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetDiffuse(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_drawreg(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->SetDrawReg();
	else if(!strcmp(k,"nnn"))	gr->SetDrawReg(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_facenum(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetFaceNum(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fog(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->Fog(a[0].v);
	else if(!strcmp(k,"nn"))	gr->Fog(a[0].v,a[1].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_font(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->SetFontDef(a[0].s.s);
	else if(!strcmp(k,"sn"))
	{	gr->SetFontDef(a[0].s.s);	gr->SetFontSize(a[1].v);	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_gray(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->Gray(true);
	else if(!strcmp(k,"n"))	gr->Gray(a[0].v!=0);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_gridplot(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnn"))	gr->GridPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v));
	else if(!strcmp(k,"nnnn"))	gr->GridPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v), a[3].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_inplot(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnn"))	gr->InPlot(a[0].v, a[1].v, a[2].v, a[3].v);
	else if(!strcmp(k,"nnnnn"))	gr->InPlot(a[0].v, a[1].v, a[2].v, a[3].v, a[4].v!=0);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_legendmarks(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetLegendMarks(mgl_int(a[0].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_light(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->Light(true);
	else if(!strcmp(k,"n"))	gr->Light(a[0].v!=0);
	else if(!strcmp(k,"nn"))	gr->Light(mgl_int(a[0].v),a[1].v!=0);
	else if(!strcmp(k,"nnnn"))	gr->AddLight(mgl_int(a[0].v),mglPoint(a[1].v,a[2].v,a[3].v));
	else if(!strcmp(k,"nnnns"))	gr->AddLight(mgl_int(a[0].v),mglPoint(a[1].v,a[2].v,a[3].v), a[4].s.s[0]);
	else if(!strcmp(k,"nnnnsn"))gr->AddLight(mgl_int(a[0].v),mglPoint(a[1].v,a[2].v,a[3].v), a[4].s.s[0],a[5].v);
	else if(!strcmp(k,"nnnnsnn"))
		gr->AddLight(mgl_int(a[0].v), mglPoint(a[1].v,a[2].v,a[3].v), a[4].s.s[0],a[5].v,a[6].v);
	else if(!strcmp(k,"nnnnnnn"))
		gr->AddLight(mgl_int(a[0].v), mglPoint(a[1].v,a[2].v,a[3].v), mglPoint(a[4].v,a[5].v,a[6].v));
	else if(!strcmp(k,"nnnnnnns"))
		gr->AddLight(mgl_int(a[0].v), mglPoint(a[1].v,a[2].v,a[3].v), mglPoint(a[4].v,a[5].v,a[6].v), a[7].s.s[0]);
	else if(!strcmp(k,"nnnnnnnsn"))
		gr->AddLight(mgl_int(a[0].v), mglPoint(a[1].v,a[2].v,a[3].v), mglPoint(a[4].v,a[5].v,a[6].v), a[7].s.s[0],a[8].v);
	else if(!strcmp(k,"nnnnnnnsnn"))
		gr->AddLight(mgl_int(a[0].v), mglPoint(a[1].v,a[2].v,a[3].v), mglPoint(a[4].v,a[5].v,a[6].v), a[7].s.s[0],a[8].v,a[9].v);
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_load(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s") && gr->pr)	mgl_parser_load(gr->pr, a[0].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_loadfont(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0 || (!strcmp(k,"s") && a[0].s[0]==0))	gr->RestoreFont();
	else if(!strcmp(k,"s"))	gr->LoadFont(a[0].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_marksize(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetMarkSize(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_mask(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"sn"))	gr->SetMask(a[0].s[0],a[1].v);
	else if(!strcmp(k,"ss"))	gr->SetMask(a[0].s[0],a[1].s.s);
	else if(!strcmp(k,"snn"))
	{	gr->SetMask(a[0].s[0],a[1].v);	gr->SetMaskAngle(mgl_int(a[2].v));	}
	else if(!strcmp(k,"ssn"))
	{	gr->SetMask(a[0].s[0],a[1].s.s);gr->SetMaskAngle(mgl_int(a[2].v));	}
	else if(!strcmp(k,"n"))		gr->SetMaskAngle(mgl_int(a[0].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_meshnum(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetMeshNum(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_multiplot(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnnn"))
		gr->MultiPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v), mgl_int(a[3].v), mgl_int(a[4].v));
	else if(!strcmp(k,"nnnnns"))
		gr->MultiPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v), mgl_int(a[3].v), mgl_int(a[4].v), a[5].s.s);
	else if(!strcmp(k,"nnnnnsnn"))
		gr->MultiPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v), mgl_int(a[3].v), mgl_int(a[4].v), a[5].s.s, a[6].v,a[7].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_origin(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))	gr->SetOrigin(a[0].v,a[1].v,NAN);
	else if(!strcmp(k,"nnn"))	gr->SetOrigin(a[0].v,a[1].v,a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_origintick(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetOriginTick(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_palette(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->SetPalette(a[0].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_pendelta(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetPenDelta(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_perspective(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->Perspective(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_plotid(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->SetPlotId(a[1].s.s);
	else  res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_quality(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->SetQuality();
	else if(!strcmp(k,"n"))	gr->SetQuality(mgl_int(a[0].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ranges(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnn"))	gr->SetRanges(a[0].v,a[1].v, a[2].v,a[3].v);
	else if(!strcmp(k,"nnnnnn"))gr->SetRanges(a[0].v,a[1].v,a[2].v, a[3].v,a[4].v,a[5].v);
	else if(!strcmp(k,"dd"))	gr->SetRanges(*(a[0].d),*(a[1].d));
	else if(!strcmp(k,"ddd"))	gr->SetRanges(*(a[0].d),*(a[1].d), *(a[2].d));
	else if(!strcmp(k,"dddd"))	gr->SetRanges(*(a[0].d),*(a[1].d), *(a[2].d),*(a[3].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_rasterize(mglGraph *gr, long , mglArg *, const char *, const char *)
{
	gr->Rasterize();	return 0;
}
//-----------------------------------------------------------------------------
int static mgls_reset(mglGraph *gr, long , mglArg *, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->DefaultPlotParam();
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_rotate(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))	gr->Rotate(a[0].v, a[1].v, 0);
	else if(!strcmp(k,"nnn"))	gr->Rotate(a[0].v, a[1].v, a[2].v);
	else if(!strcmp(k,"nnnn"))	gr->RotateN(a[0].v, a[1].v, a[2].v, a[3].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_rotatetext(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetRotatedText(a[0].v!=0);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_scaletext(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetScaleText(a[0].v!=0);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_setsize(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn") && a[1].v>1 && a[0].v>1)
		gr->SetSize(mgl_int(a[0].v), mgl_int(a[1].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_setsizescl(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetSizeScl(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_setup(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))	gr->SetFlagAdv(mgl_int(a[0].v), mgl_int(a[1].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_shear(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))	gr->Shear(a[0].v, a[1].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_shearplot(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnn"))	gr->ShearPlot(mgl_int(a[0].v), mgl_int(a[1].v), a[2].v, a[3].v);
	else if(!strcmp(k,"nnnnnn"))	gr->ShearPlot(mgl_int(a[0].v), mgl_int(a[1].v), a[2].v, a[3].v, a[4].v, a[5].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_stickplot(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnn"))	gr->StickPlot(mgl_int(a[0].v), mgl_int(a[1].v), a[2].v, a[3].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_subplot(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnn"))	gr->SubPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v));
	else if(!strcmp(k,"nnns"))	gr->SubPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v), a[3].s.s);
	else if(!strcmp(k,"nnnsnn"))	gr->SubPlot(mgl_int(a[0].v), mgl_int(a[1].v), mgl_int(a[2].v), a[3].s.s, a[4].v,a[5].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ternary(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->Ternary(int(a[0].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ticklen(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetTickLen(a[0].v);
	else if(!strcmp(k,"nn"))	gr->SetTickLen(a[0].v, a[1].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tickshift(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetTickShift(mglPoint(a[0].v));
	else if(!strcmp(k,"nn"))	gr->SetTickShift(mglPoint(a[0].v, a[1].v));
	else if(!strcmp(k,"nnn"))	gr->SetTickShift(mglPoint(a[0].v, a[1].v, a[2].v));
	else if(!strcmp(k,"nnnn"))	gr->SetTickShift(mglPoint(a[0].v, a[1].v, a[2].v, a[3].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ticktime(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))	gr->SetTicksTime(a[0].s[0]);
	else if(!strcmp(k,"sn"))	gr->SetTicksTime(a[0].s[0],a[1].v);
	else if(!strcmp(k,"sns"))	gr->SetTicksTime(a[0].s[0],a[1].v,a[2].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_transptype(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetTranspType(a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tuneticks(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetTuneTicks(mgl_int(a[0].v));
	else if(!strcmp(k,"nn"))	gr->SetTuneTicks(mgl_int(a[0].v),a[1].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_variant(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n") && gr->pr)	mgl_parser_variant(gr->pr, a[0].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_version(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	char buf[32];	sprintf(buf,"MathGL version is 2.%g",MGL_VER2);
	if(k[0]==0)	gr->SetWarn(-1,buf);
	else if(!strcmp(k,"s"))	res = mgl_check_version(a[0].s.s)?1:0;
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_view(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))		gr->View(a[0].v, a[1].v);
	else if(!strcmp(k,"nnn"))	gr->View(a[0].v, a[1].v, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_write(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]==0)	gr->WriteFrame("", "MathGL");
	else if(!strcmp(k,"s"))	gr->WriteFrame(a[0].s.s, "MathGL");
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_xrange(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->SetRange('x',*(a[0].d));
	else if(!strcmp(k,"dn"))	gr->SetRange('x',*(a[0].d),a[1].v);
	else if(!strcmp(k,"nn"))	gr->SetRange('x', a[0].v, a[1].v);
	else if(!strcmp(k,"nnn"))
	{
		if(a[2].v)	gr->AddRange('x', a[0].v, a[1].v);
		else	gr->SetRange('x', a[0].v, a[1].v);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_xtick(mglGraph *gr, long n, mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetTicks('x', a[0].v);
//	else if(!strcmp(k,"ns"))	gr->SetTicks('x', a[0].v, 0, NAN, a[1].s.w);
	else if(!strcmp(k,"nn"))	gr->SetTicks('x', a[0].v, mgl_int(a[1].v));
	else if(!strcmp(k,"nns"))	gr->SetTicks('x', a[0].v, mgl_int(a[1].v), NAN, a[2].s.w);
	else if(!strcmp(k,"nnn"))	gr->SetTicks('x', a[0].v, mgl_int(a[1].v), a[2].v);
	else if(!strcmp(k,"nnns"))	gr->SetTicks('x', a[0].v, mgl_int(a[1].v), a[2].v, a[3].s.w);
	else if(!strcmp(k,"s"))		gr->SetTickTempl('x',a[0].s.w);
	else if(!strcmp(k,"ds"))	gr->SetTicksVal('x', *(a[0].d), a[1].s.w);
	else if(!strcmp(k,"dsn"))	gr->SetTicksVal('x', *(a[0].d), a[1].s.w, a[2].v);
	else if(!strncmp(k,"ns",2))
	{
		std::wstring s;
		std::vector<mreal> v;
		for(long i=0;i<n/2;i++)
		{
			if(a[2*i].type==2 && a[2*i+1].type==1)
			{	v.push_back(a[2*i].v);	s += std::wstring(a[2*i+1].s.w)+L"\n";	}
			else	break;
		}
		gr->SetTicksVal('x',mglDataS(v),s.c_str(),v.size()==1?true:false);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_yrange(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->SetRange('y',*(a[0].d));
	else if(!strcmp(k,"dn"))	gr->SetRange('y',*(a[0].d),a[1].v);
	else if(!strcmp(k,"nn"))	gr->SetRange('y', a[0].v, a[1].v);
	else if(!strcmp(k,"nnn"))
	{
		if(a[2].v)	gr->AddRange('y', a[0].v, a[1].v);
		else	gr->SetRange('y', a[0].v, a[1].v);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ytick(mglGraph *gr, long n, mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetTicks('y', a[0].v);
//	else if(!strcmp(k,"ns"))	gr->SetTicks('y', a[0].v, 0, NAN, a[1].s.w);
	else if(!strcmp(k,"nn"))	gr->SetTicks('y', a[0].v, mgl_int(a[1].v));
	else if(!strcmp(k,"nns"))	gr->SetTicks('y', a[0].v, mgl_int(a[1].v), NAN, a[2].s.w);
	else if(!strcmp(k,"nnn"))	gr->SetTicks('y', a[0].v, mgl_int(a[1].v), a[2].v);
	else if(!strcmp(k,"nnns"))	gr->SetTicks('y', a[0].v, mgl_int(a[1].v), a[2].v, a[3].s.w);
	else if(!strcmp(k,"s"))		gr->SetTickTempl('y',a[0].s.w);
	else if(!strcmp(k,"ds"))	gr->SetTicksVal('y', *(a[0].d), a[1].s.w);
	else if(!strcmp(k,"dsn"))	gr->SetTicksVal('y', *(a[0].d), a[1].s.w, a[2].v);
	else if(!strncmp(k,"ns",2))
	{
		std::wstring s;
		std::vector<mreal> v;
		for(long i=0;i<n/2;i++)
		{
			if(a[2*i].type==2 && a[2*i+1].type==1)
			{	v.push_back(a[2*i].v);	s += std::wstring(a[2*i+1].s.w)+L"\n";	}
			else	break;
		}
		gr->SetTicksVal('y',mglDataS(v),s.c_str(),v.size()==1?true:false);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_zoom(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnn"))	gr->Zoom(a[0].v, a[1].v, a[2].v, a[3].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_zoomaxis(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn"))	gr->ZoomAxis(mglPoint(a[0].v), mglPoint(a[1].v));
	else if(!strcmp(k,"nnnn"))	gr->ZoomAxis(mglPoint(a[0].v, a[1].v), mglPoint(a[2].v, a[3].v));
	else if(!strcmp(k,"nnnnnn"))	gr->ZoomAxis(mglPoint(a[0].v, a[1].v, a[2].v), mglPoint(a[3].v, a[4].v, a[5].v));
	else if(!strcmp(k,"nnnnnnnn"))	gr->ZoomAxis(mglPoint(a[0].v, a[1].v, a[2].v, a[3].v), mglPoint(a[4].v, a[5].v, a[6].v, a[7].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_zrange(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->SetRange('z',*(a[0].d));
	else if(!strcmp(k,"dn"))	gr->SetRange('z',*(a[0].d),a[1].v);
	else if(!strcmp(k,"nn"))	gr->SetRange('z', a[0].v, a[1].v);
	else if(!strcmp(k,"nnn"))
	{
		if(a[2].v)	gr->AddRange('z', a[0].v, a[1].v);
		else	gr->SetRange('z', a[0].v, a[1].v);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ztick(mglGraph *gr, long n, mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"n"))	gr->SetTicks('z', a[0].v);
//	else if(!strcmp(k,"ns"))	gr->SetTicks('z', a[0].v, 0, NAN, a[1].s.w);
	else if(!strcmp(k,"nn"))	gr->SetTicks('z', a[0].v, mgl_int(a[1].v));
	else if(!strcmp(k,"nns"))	gr->SetTicks('z', a[0].v, mgl_int(a[1].v), NAN, a[2].s.w);
	else if(!strcmp(k,"nnn"))	gr->SetTicks('z', a[0].v, mgl_int(a[1].v), a[2].v);
	else if(!strcmp(k,"nnns"))	gr->SetTicks('z', a[0].v, mgl_int(a[1].v), a[2].v, a[3].s.w);
	else if(!strcmp(k,"s"))		gr->SetTickTempl('z',a[0].s.w);
	else if(!strcmp(k,"ds"))	gr->SetTicksVal('z', *(a[0].d), a[1].s.w);
	else if(!strcmp(k,"dsn"))	gr->SetTicksVal('z', *(a[0].d), a[1].s.w, a[2].v);
	else if(!strncmp(k,"ns",2))
	{
		std::wstring s;
		std::vector<mreal> v;
		for(long i=0;i<n/2;i++)
		{
			if(a[2*i].type==2 && a[2*i+1].type==1)
			{	v.push_back(a[2*i].v);	s += std::wstring(a[2*i+1].s.w)+L"\n";	}
			else	break;
		}
		gr->SetTicksVal('z',mglDataS(v),s.c_str(),v.size()==1?true:false);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
mglCommand mgls_set_cmd[] = {
	{"addlegend",_("Add legend entry"),"addlegend 'txt' 'fmt'", mgls_addlegend,15},
	{"addsymbol",_("Add user-defined symbol"),"addsymbol 'id' Xdat Ydat", mgls_addsymbol,15},
	{"adjust",_("Adjust ticks for best view"),"adjust ['dir']", mgls_adjust ,14},
	{"alpha",_("Switch on/off transparency"),"alpha [val]", mgls_alpha ,2},
	{"alphadef",_("Set default transparency"),"alphadef val", mgls_alphadef ,2},
	{"ambient",_("Set ambient light brightness"),"ambient val", mgls_ambient ,2},
	{"arrowsize",_("Set size of arrows"),"arrowsize val", mgls_arrowsize ,2},
	{"aspect",_("Set aspect ration"),"aspect valx valy [valz]", mgls_aspect ,5},
	{"attachlight",_("Attach light settings to inplot"),"attachlight val", mgls_attachlight ,2},
	{"axisstl",_("Set axis and tick style"),"axisstl 'stl' ['tick' 'sub']", mgls_axisstl ,14},
	{"barwidth",_("Set default bars width"),"barwidth val", mgls_barwidth ,2},
	{"bbox",_("Set bounding box for 2d export"),"bbox x1 y1 [x2 y2]", mgls_bbox ,2},
	{"chdir",_("Change current directory"),"chdir 'path'", mgls_chdir ,2},
	{"clearlegend",_("Clear legend entries"),"clearlegend", mgls_clearlegend ,15},
	{"clf",_("Clear picture"),"clf|'col'|r g b", mgls_clf ,12},
	{"columnplot",_("Set position of plot inside cell of column"), "columnplot num ind [d]", mgls_columnplot ,5},
	{"crange",_("Set color range"),"crange Dat [add]|c1 c2 [add]", mgls_crange ,14},
	{"ctick",_("Set ticks for colorbar"),"ctick 'tmpl'|dc ['factor']", mgls_ctick ,14},
	{"cut",_("Setup plot points cutting"),"cut val|x1 y1 z1 x2 y2 z2|'cond'", mgls_cut ,2},
	{"diffuse",_("Set diffusive light brightness"),"diffuse val", mgls_diffuse ,2},
	{"drawreg",_("Set draw region for quality&4"),"drawreg|nx ny m", mgls_drawreg ,2},
	{"facenum",_("Set number of visible faces"),"facenum val", mgls_facenum ,2},
	{"fog",_("Switch on/off fog"),"fog val [dz]", mgls_fog ,2},
	{"font",_("Setup font"),"font 'fmt' [size]", mgls_font ,15},
	{"gray",_("Switch on/off gray-scale mode"),"gray [val]", mgls_gray ,2},
	{"gridplot",_("Set position of plot inside cell of matrix"), "gridplot nx ny ind [d]", mgls_gridplot ,5},
	{"inplot",_("Set arbitrary position of plot in picture"),"x1 x2 y1 y2 [rel]", mgls_inplot ,5},
	{"legendmarks",_("Set number of marks in the legend"),"legendmarks val", mgls_legendmarks ,15},
	{"light",_("Setup light"),"light [val]|num val|num xpos ypos zpos ['fmt' br ap]|num xpos ypos zpos px py pz ['fmt' br ap]", mgls_light ,2},
	{"load",_("Load commands from external DLL"),"load 'fname'", mgls_load, 6},
	{"loadfont",_("Load fontfaces"),"loadfont ['face']", mgls_loadfont ,15},
	{"marksize",_("Set size of markers"),"marksize val", mgls_marksize ,2},
	{"mask",_("Set brush for given mask id"),"mask 'id' 'val'|'id' val|angle", mgls_mask ,2},
	{"meshnum",_("Set number of lines in mesh/fall/vect and so on"),"meshnum val", mgls_meshnum ,2},
	{"multiplot",_("Set position of plot block in matrix"),"multiplot m n pos dx dy 'style' [sx sy]", mgls_multiplot ,5},
	{"origin",_("Set axis origin"),"origin x0 y0 [z0]", mgls_origin ,14},
	{"origintick",_("Set tick labels drawing at origin"),"origintick val", mgls_origintick ,14},
	{"palette",_("Set palette for 1D plots"),"palette 'colors'", mgls_palette ,2},
	{"pendelta",_("Set size of semi-transparent area around line"),"pendelta val", mgls_pendelta ,2},
	{"perspective",_("Set perspective"),"perspective val", mgls_perspective ,2},
	{"plotid",_("Set default filename"),"plotid 'name'", mgls_plotid ,2},
	{"quality",_("Set plot quality"),"quality [val]", mgls_quality ,2},
	{"ranges",_("Set axis ranges"),"ranges x1 x2 y1 y2 [z1 z2]|Xdat Ydat [Zdat Cdat]", mgls_ranges ,14},
	{"rasterize",_("Rasterize plot and save to background"),"rasterize", mgls_rasterize ,12},
	{"reset",_("Reset settings and clear picture"),"reset", mgls_reset ,12},
	{"rotate",_("Rotate plot"),"rotate tetz tetx [tety]|tet x y z", mgls_rotate ,5},
	{"rotatetext",_("Set to auto rotate text or not"),"rotatetext val", mgls_rotatetext ,15},
	{"scaletext",_("Set scale text in relative subplots too"),"scaletext val", mgls_scaletext ,15},
	{"setsize",_("Set picture size"),"setsize width height", mgls_setsize ,2},
	{"setsizescl",_("Set scaling factor for further setsize"),"setsizescl val", mgls_setsizescl ,2},
	{"setup",_("Set bit-flags (for advanced users only)"),"setup val flag", mgls_setup ,2},
	{"shear",_("Shear plot"),"shear valx valy", mgls_shear ,5},
	{"shearplot",_("Set position of plot inside cell of sheared stick"), "shearplot num ind sx sy [xd yd]", mgls_shearplot ,5},
	{"stickplot",_("Set position of plot inside cell of rotated stick"), "stickplot num ind tet phi", mgls_stickplot ,5},
	{"subplot",_("Set position of plot as cell of matrix"),"subplot m n pos ['style' dx dy]", mgls_subplot ,5},
	{"ternary",_("Switch on/off to use ternary axis"),"ternary val", mgls_ternary ,14},
	{"ticklen",_("Set tick length"),"ticklen val [stt]", mgls_ticklen ,14},
	{"tickshift",_("Set additional tick and axis labels shift"),"tickshift dx [dy dz dc]", mgls_tickshift ,14},
	{"ticktime",_("Set ticks in time format"),"ticktime 'dir' [dv 'tmpl']", mgls_ticktime ,14},
	{"transptype",_("Set type transparency"),"transptype val", mgls_transptype ,2},
	{"tuneticks",_("Set ticks tuning"),"tuneticks val [pos]", mgls_tuneticks ,14},
	{"variant",_("Select variant of plot style(s)"),"variant var", mgls_variant, 6},
	{"version",_("Print MathGL version or check if it is valid"),"version ['ver']", mgls_version, 2},
	{"view",_("Change view angles - use 'rotate' for plot rotation"),"view tetz tetx [tety]", mgls_view ,5},
	{"write",_("Write current image to graphical file"),"write ['fname']", mgls_write ,2},
	{"xrange",_("Set range for x-axis"),"xrange Dat [add]|x1 x2 [add]", mgls_xrange ,14},
	{"xtick",_("Set ticks for x-axis"),"xtick dx sx ['factor']|dx sx tx ['factor']|'tmpl'|Xdat 'lbl' [add]|v1 'lbl1' ...", mgls_xtick,14},
	{"yrange",_("Set range for y-axis"),"yrange Dat [add]|y1 y2 [add]", mgls_yrange,14},
	{"ytick",_("Set ticks for y-axis"),"ytick dy sy ['factor']|dy sy ty ['factor']|'tmpl'|Ydat 'lbl' [add]|v1 'lbl1' ...", mgls_ytick,14},
	{"zoom",_("Zoom plot region"),"zoom x1 x2 y1 y2", mgls_zoom,5},
	{"zoomaxis",_("Zoom axis range"),"zoomaxis x1 x2|x1 y1 x2 y2|x1 y1 z1 x2 y2 z2|x1 y1 z1 c1 x2 y2 z2 c2", mgls_zoomaxis,14},
	{"zrange",_("Set range for z-axis"),"yrange Dat [add]|z1 z2 [add]", mgls_zrange ,14},
	{"ztick",_("Set ticks for z-axis"),"ztick dz sz ['factor']|dz sz tz ['factor']|'tmpl'|Zdat 'lbl' [add]|v1 'lbl1' ...", mgls_ztick,14},
{"","","",NULL,0}};
//-----------------------------------------------------------------------------
