/***************************************************************************
 * base.cpp is part of Math gric Library
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
#include "mgl2/base_cf.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
//
//		C interfaces
//
//-----------------------------------------------------------------------------
bool mglPrintWarn = true;
void MGL_EXPORT mgl_suppress_warn(int on)	{	mglPrintWarn = !on;	}
void MGL_EXPORT mgl_suppress_warn_(int *on)	{	mgl_suppress_warn(*on);	}
void MGL_EXPORT mgl_set_quality(HMGL gr, int qual)	{	gr->SetQuality(qual);	}
void MGL_EXPORT mgl_set_quality_(uintptr_t *gr, int *qual)	{	_GR_->SetQuality(*qual);	}
int MGL_EXPORT_PURE mgl_get_quality(HMGL gr)	{	return gr->GetQuality();	}
int MGL_EXPORT_PURE mgl_get_quality_(uintptr_t *gr)	{	return _GR_->GetQuality();	}
int MGL_EXPORT_PURE mgl_is_frames(HMGL gr)
{	return gr->get(MGL_VECT_FRAME) && !(gr->GetQuality()&MGL_DRAW_LMEM);	}
void MGL_EXPORT mgl_set_draw_reg(HMGL gr, long nx, long ny, long m)	{	gr->SetDrawReg(nx,ny,m);	}
void MGL_EXPORT mgl_set_draw_reg_(uintptr_t *gr, int *nx, int *ny, int *m)	{	_GR_->SetDrawReg(*nx,*ny,*m);	}
//-----------------------------------------------------------------------------
int MGL_EXPORT_PURE mgl_get_flag(HMGL gr, uint32_t flag)	{	return gr->get(flag);	}
int MGL_EXPORT_PURE mgl_get_flag_(uintptr_t *gr, unsigned long *flag)	{	return _GR_->get(*flag);	}
void MGL_EXPORT mgl_set_flag(HMGL gr, int val, uint32_t flag)		{	gr->set(val,flag);	}
void MGL_EXPORT mgl_set_flag_(uintptr_t *gr, int *val, unsigned long *flag)	{	_GR_->set(*val,*flag);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_color(char id, double r, double g, double b)
{
	for(long i=0;mglColorIds[i].id;i++)
		if(mglColorIds[i].id==id)	mglColorIds[i].col = mglColor(r,g,b);
}
void MGL_EXPORT mgl_set_color_(char *id, mreal *r, mreal *g, mreal *b, int)	{	mgl_set_color(*id,*r,*g,*b);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_def_sch(HMGL gr, const char *sch)	{	gr->SetDefScheme(sch);	}
void MGL_EXPORT mgl_set_def_sch_(uintptr_t *gr, const char *sch,int l)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	mgl_set_def_sch(_GR_, s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_plotid(HMGL gr, const char *id)	{	gr->PlotId = id;	}
void MGL_EXPORT mgl_set_plotid_(uintptr_t *gr, const char *id,int l)
{	char *s=new char[l+1];	memcpy(s,id,l);	s[l]=0;
	_GR_->PlotId = s;	delete []s;	}
MGL_EXPORT_PURE const char *mgl_get_plotid(HMGL gr)	{	return gr->PlotId.c_str();	}
int MGL_EXPORT mgl_get_plotid_(uintptr_t *gr, char *out, int len)
{
	const char *res = mgl_get_plotid(_GR_);
	if(out)	mgl_strncpy(out,res,len);
	return strlen(res);
}
//-----------------------------------------------------------------------------
MGL_EXPORT_PURE const char *mgl_get_mess(HMGL gr)	{	return gr->Mess.c_str();	}
int MGL_EXPORT mgl_get_mess_(uintptr_t *gr, char *out, int len)
{
	const char *res = mgl_get_mess(_GR_);
	if(out)	mgl_strncpy(out,res,len);
	return strlen(res);
}
int MGL_EXPORT_PURE mgl_get_warn(HMGL gr)	{	return gr->GetWarn();	}
void MGL_EXPORT mgl_set_warn(HMGL gr, int code, const char *txt)
{	gr->SetWarn(code,txt);	}
extern bool mglPrintWarn;
void MGL_EXPORT mgl_set_global_warn(const char *txt)
{
	if(txt && *txt)
	{
		mglGlobalMess += txt;	mglGlobalMess += '\n';
		if(mglPrintWarn)	fprintf(stderr,_("Global message - %s\n"),txt);
	}
}
void MGL_EXPORT mgl_set_global_warn_(const char *txt, int l)
{	char *s=new char[l+1];	memcpy(s,txt,l);	s[l]=0;	mgl_set_global_warn(s);	delete []s;	}
MGL_EXPORT_PURE const char *mgl_get_global_warn()	{	return mglGlobalMess.c_str();	}
int MGL_EXPORT mgl_get_global_warn_(char *out, int len)
{
	const char *res = mgl_get_global_warn();
	if(out)	mgl_strncpy(out,res,len);
	return strlen(res);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_origin(HMGL gr, double x0, double y0, double z0)
{	gr->SetOrigin(x0,y0,z0);	}
void MGL_EXPORT mgl_set_palette(HMGL gr, const char *colors)
{	gr->SetPalette(colors);	}
void MGL_EXPORT mgl_set_meshnum(HMGL gr, int num)	{	gr->SetMeshNum(num);	}
void MGL_EXPORT mgl_set_facenum(HMGL gr, int num)	{	gr->FaceNum=num;		}
void MGL_EXPORT mgl_set_alpha_default(HMGL gr, double alpha)	{	gr->SetAlphaDef(alpha);	}
void MGL_EXPORT mgl_set_light_dif(HMGL gr, int enable)		{	gr->SetDifLight(enable);	}
void MGL_EXPORT mgl_clear_unused(HMGL gr)	{	gr->ClearUnused();	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_rdc_acc(HMGL gr, int reduce)	{	gr->SetReduceAcc(reduce);	}
void MGL_EXPORT mgl_highlight(HMGL gr, int id)			{	gr->Highlight(id);	}
void MGL_EXPORT mgl_set_cut(HMGL gr, int cut)	{	gr->SetCut(cut);	}
void MGL_EXPORT mgl_set_cut_box(HMGL gr, double x1,double y1,double z1,double x2,double y2,double z2)
{	gr->SetCutBox(x1,y1,z1,x2,y2,z2);	}
void MGL_EXPORT mgl_set_cutoff(HMGL gr, const char *EqC)	{	gr->CutOff(EqC);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_ternary(HMGL gr, int enable)			{	gr->Ternary(enable);	}
void MGL_EXPORT mgl_set_range_val(HMGL gr, char dir, double v1,double v2)
{
	if(dir=='c' || dir=='a')	gr->CRange(v1,v2);
	else if(dir=='x')	gr->XRange(v1,v2);
	else if(dir=='y')	gr->YRange(v1,v2);
	else if(dir=='z')	gr->ZRange(v1,v2);
}
void MGL_EXPORT mgl_add_range_val(HMGL gr, char dir, double v1,double v2)
{
	if(dir=='c' || dir=='a')	gr->CRange(v1,v2,true);
	else if(dir=='x')	gr->XRange(v1,v2,true);
	else if(dir=='y')	gr->YRange(v1,v2,true);
	else if(dir=='z')	gr->ZRange(v1,v2,true);
}
void MGL_EXPORT mgl_set_range_dat(HMGL gr, char dir, HCDT a, int add)
{
	if(dir=='c' || dir=='a')	gr->CRange(a,add);
	else if(dir=='x')	gr->XRange(a,add);
	else if(dir=='y')	gr->YRange(a,add);
	else if(dir=='z')	gr->ZRange(a,add);
}
void MGL_EXPORT mgl_set_ranges(HMGL gr, double x1, double x2, double y1, double y2, double z1, double z2)
{	gr->SetRanges(x1,x2,y1,y2,z1,z2);	}
void MGL_EXPORT mgl_set_auto_ranges(HMGL gr, double x1, double x2, double y1, double y2, double z1, double z2, double c1, double c2)
{	gr->SetAutoRanges(x1,x2,y1,y2,z1,z2,c1,c2);	}
void MGL_EXPORT mgl_set_func(HMGL gr, const char *EqX,const char *EqY,const char *EqZ,const char *EqA)
{	gr->SetFunc(EqX,EqY,EqZ,EqA);	}
void MGL_EXPORT mgl_set_coor(HMGL gr, int how)	{	gr->SetCoor(how);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_bar_width(HMGL gr, double width)	{	gr->SetBarWidth(width);	}
//-----------------------------------------------------------------------------
//
//		Fortran interfaces
//
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_rdc_acc_(uintptr_t *gr, int *reduce)
{	_GR_->SetReduceAcc(*reduce);	}
void MGL_EXPORT mgl_highlight_(uintptr_t *gr, int *id)	{	_GR_->Highlight(*id);	}
void MGL_EXPORT mgl_set_origin_(uintptr_t *gr, mreal *x0, mreal *y0, mreal *z0)
{	_GR_->SetOrigin(*x0,*y0,*z0);	}
int MGL_EXPORT_PURE mgl_get_warn_(uintptr_t *gr)	{	return _GR_->GetWarn();	}
void MGL_EXPORT mgl_set_warn_(uintptr_t *gr, int *code, const char *txt, int l)
{	char *s=new char[l+1];	memcpy(s,txt,l);	s[l]=0;
	_GR_->SetWarn(*code, s);	delete []s;	}
void MGL_EXPORT mgl_set_palette_(uintptr_t *gr, const char *colors, int l)
{	char *s=new char[l+1];	memcpy(s,colors,l);	s[l]=0;
	_GR_->SetPalette(s);	delete []s;	}
void MGL_EXPORT mgl_set_meshnum_(uintptr_t *gr, int *num)	{	_GR_->SetMeshNum(*num);	}
void MGL_EXPORT mgl_set_facenum_(uintptr_t *gr, int *num)	{	_GR_->FaceNum=*num;		}
void MGL_EXPORT mgl_set_alpha_default_(uintptr_t *gr, mreal *alpha)	{	_GR_->SetAlphaDef(*alpha);	}
void MGL_EXPORT mgl_set_light_dif_(uintptr_t *gr, int *enable)			{	_GR_->SetDifLight(*enable);	}
void MGL_EXPORT mgl_clear_unused_(uintptr_t *gr)	{	_GR_->ClearUnused();	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_cut_box_(uintptr_t *gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2)
{	_GR_->SetCutBox(*x1,*y1,*z1,*x2,*y2,*z2);	}
void MGL_EXPORT mgl_set_cut_(uintptr_t *gr, int *cut)	{	_GR_->SetCut(*cut);	}
void MGL_EXPORT mgl_set_cutoff_(uintptr_t *gr, const char *EqC, int l)
{	char *s=new char[l+1];	memcpy(s,EqC,l);	s[l]=0;
	_GR_->CutOff(s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_ternary_(uintptr_t *gr, int *enable)	{	_GR_->Ternary(*enable);	}
void MGL_EXPORT mgl_set_range_val_(uintptr_t *gr, const char *dir, mreal *v1, mreal *v2,int)
{	mgl_set_range_val(_GR_,*dir,*v1,*v2);	}
void MGL_EXPORT mgl_add_range_val_(uintptr_t *gr, const char *dir, mreal *v1, mreal *v2,int)
{	mgl_add_range_val(_GR_,*dir,*v1,*v2);	}
void MGL_EXPORT mgl_set_range_dat_(uintptr_t *gr, const char *dir, uintptr_t *a, int *add,int)
{	mgl_set_range_dat(_GR_,*dir,_DA_(a),*add);	}
void MGL_EXPORT mgl_set_ranges_(uintptr_t *gr, mreal *x1, mreal *x2, mreal *y1, mreal *y2, mreal *z1, mreal *z2)
{	_GR_->SetRanges(*x1,*x2,*y1,*y2,*z1,*z2);	}
void MGL_EXPORT mgl_set_auto_ranges_(uintptr_t *gr, mreal *x1, mreal *x2, mreal *y1, mreal *y2, mreal *z1, mreal *z2, mreal *c1, mreal *c2)
{	_GR_->SetAutoRanges(*x1,*x2,*y1,*y2,*z1,*z2,*c1,*c2);	}
void MGL_EXPORT mgl_set_func_(uintptr_t *gr, const char *EqX,const char *EqY,const char *EqZ,const char *EqA,int lx,int ly,int lz,int la)
{
	char *sx=new char[lx+1];	memcpy(sx,EqX,lx);	sx[lx]=0;
	char *sy=new char[ly+1];	memcpy(sy,EqY,ly);	sy[ly]=0;
	char *sz=new char[lz+1];	memcpy(sz,EqZ,lz);	sz[lz]=0;
	char *sa=new char[la+1];	memcpy(sa,EqA,la);	sa[la]=0;
	_GR_->SetFunc(sx,sy,sz,sa);
	delete []sx;	delete []sy;	delete []sz;	delete []sa;
}
void MGL_EXPORT mgl_set_coor_(uintptr_t *gr, int *how)
{	_GR_->SetCoor(*how);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_tick_rotate(HMGL gr, int enable){	gr->SetTickRotate(enable);	}
void MGL_EXPORT mgl_set_tick_skip(HMGL gr, int enable)	{	gr->SetTickSkip(enable);	}
void MGL_EXPORT mgl_set_tick_rotate_(uintptr_t *gr,int *enable){	_GR_->SetTickRotate(*enable);	}
void MGL_EXPORT mgl_set_tick_skip_(uintptr_t *gr, int *enable)	{	_GR_->SetTickSkip(*enable);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_rotated_text(HMGL gr, int enable)	{	gr->SetRotatedText(enable);	}
void MGL_EXPORT mgl_set_scale_text(HMGL gr, int enable)		{	gr->set(!enable, MGL_NO_SCALE_REL);	}
void MGL_EXPORT mgl_set_mark_size(HMGL gr, double size)		{	gr->SetMarkSize(size);	}
void MGL_EXPORT mgl_set_arrow_size(HMGL gr, double size)	{	gr->SetArrowSize(size);	}
void MGL_EXPORT mgl_set_font_size(HMGL gr, double size)		{	gr->SetFontSize(size);	}
void MGL_EXPORT mgl_set_font_def(HMGL gr, const char *fnt)	{	gr->SetFontDef(fnt);	}
void MGL_EXPORT mgl_load_font(HMGL gr, const char *name, const char *path)
{	gr->LoadFont(name,path);	}
void MGL_EXPORT mgl_copy_font(HMGL gr, HMGL gr_from)	{	gr->CopyFont(gr_from);	}
void MGL_EXPORT mgl_restore_font(HMGL gr)	{	gr->RestoreFont();	}
void MGL_EXPORT mgl_define_symbol(HMGL gr, char id, HCDT x, HCDT y)	{	gr->DefineGlyph(x,y,id);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_bar_width_(uintptr_t *gr, mreal *width)	{	_GR_->SetBarWidth(*width);	}
void MGL_EXPORT mgl_set_rotated_text_(uintptr_t *gr, int *enable)	{	_GR_->SetRotatedText(*enable);	}
void MGL_EXPORT mgl_set_scale_text_(uintptr_t *gr, int *enable)	{	mgl_set_scale_text(_GR_,*enable);	}
void MGL_EXPORT mgl_set_mark_size_(uintptr_t *gr, mreal *size)		{	_GR_->SetMarkSize(*size);	}
void MGL_EXPORT mgl_set_arrow_size_(uintptr_t *gr, mreal *size)	{	_GR_->SetArrowSize(*size);	}
void MGL_EXPORT mgl_set_font_size_(uintptr_t *gr, mreal *size)		{	_GR_->SetFontSize(*size);	}
void MGL_EXPORT mgl_set_font_def_(uintptr_t *gr, const char *name, int l)
{	char *s=new char[l+1];		memcpy(s,name,l);	s[l]=0;
	_GR_->SetFontDef(s);	delete []s;	}
void MGL_EXPORT mgl_load_font_(uintptr_t *gr, char *name, char *path, int l,int n)
{	char *s=new char[l+1];		memcpy(s,name,l);	s[l]=0;
	char *d=new char[n+1];		memcpy(d,path,n);	d[n]=0;
	_GR_->LoadFont(s,d);	delete []s;		delete []d;	}
void MGL_EXPORT mgl_copy_font_(uintptr_t *gr, uintptr_t *gr_from)
{	_GR_->CopyFont((mglBase *)(*gr_from));	}
void MGL_EXPORT mgl_restore_font_(uintptr_t *gr)	{	_GR_->RestoreFont();	}
void MGL_EXPORT mgl_define_symbol_(uintptr_t *gr, char *id, uintptr_t *x, uintptr_t *y, int)
{	_GR_->DefineGlyph(_DA_(x),_DA_(y),id?*id:0);	}
//-----------------------------------------------------------------------------
extern mglFont mglDefFont;
void MGL_EXPORT mgl_def_font(const char *name, const char *path)
{	mglDefFont.Load(name,path);	}
void MGL_EXPORT mgl_def_font_(const char *name, const char *path,int l,int n)
{	char *s=new char[l+1];		memcpy(s,name,l);	s[l]=0;
	char *d=new char[n+1];		memcpy(d,path,n);	d[n]=0;
	mglDefFont.Load(name,path);	delete []s;		delete []d;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_check_version(const char *ver)
{	double v=0;	int r = sscanf(ver,"2.%lg",&v);
	return r<1 || v>MGL_VER2;	}
int MGL_EXPORT mgl_check_version_(const char *ver, int l)
{	char *s=new char[l+1];		memcpy(s,ver,l);	s[l]=0;
	int r=mgl_check_version(s);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_start_group(HMGL gr, const char *s)	{	gr->StartAutoGroup(s);	}
void MGL_EXPORT mgl_end_group(HMGL gr)	{	gr->EndGroup();	}
void MGL_EXPORT mgl_start_group_(uintptr_t *gr, const char *name,int l)
{	char *s=new char[l+1];		memcpy(s,name,l);	s[l]=0;
	_GR_->StartAutoGroup(s);	delete []s;	}
void MGL_EXPORT mgl_end_group_(uintptr_t *gr)	{	_GR_->EndGroup();	}
//-----------------------------------------------------------------------------
#include <stdarg.h>
bool mglTestMode=false;
void MGL_EXPORT mgl_test_txt(const char *str, ...)
{
	if(mglTestMode)
	{
		char buf[256];
		va_list lst;
		va_start(lst,str);
		vsnprintf(buf,256,str,lst);	buf[255]=0;
		va_end(lst);
		printf(_("TEST: %s\n"),buf);
		fflush(stdout);
	}
}
void MGL_EXPORT mgl_set_test_mode(int enable)	{	mglTestMode=enable;	}
//---------------------------------------------------------------------------
long MGL_EXPORT mgl_use_graph(HMGL gr, int inc)
{	if(!gr)	return 0;	gr->InUse+=inc;	return gr->InUse;	}
long MGL_EXPORT mgl_use_graph_(uintptr_t *gr, int *inc)
{	return mgl_use_graph(_GR_,*inc);	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_set_ambbr(HMGL gr, double i)		{	gr->SetAmbient(i);	}
void MGL_EXPORT mgl_set_ambbr_(uintptr_t *gr, mreal *i){	_GR_->SetAmbient(*i);	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_set_difbr(HMGL gr, double i)		{	gr->SetDiffuse(i);	}
void MGL_EXPORT mgl_set_difbr_(uintptr_t *gr, mreal *i){	_GR_->SetDiffuse(*i);	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_zoom_axis(HMGL gr, double x1,double y1,double z1,double c1,double x2,double y2,double z2,double c2)
{	gr->ZoomAxis(mglPoint(x1,y1,z1,c1), mglPoint(x2,y2,z2,c2));	}
void MGL_EXPORT mgl_zoom_axis_(uintptr_t *gr, mreal *x1, mreal *y1, mreal *z1, mreal *c1, mreal *x2, mreal *y2, mreal *z2, mreal *c2)
{	_GR_->ZoomAxis(mglPoint(*x1,*y1,*z1,*c1), mglPoint(*x2,*y2,*z2,*c2));	}
//---------------------------------------------------------------------------
extern uint64_t mgl_mask_def[16];
void MGL_EXPORT mgl_set_mask(char id, const char *mask)
{
	const char *msk = MGL_MASK_ID, *s = mglchr(msk, id);
	if(s)
	{
		uint64_t val = (mask && *mask) ? strtoull(mask,NULL,16) : mgl_mask_def[s-msk];
		mgl_mask_val[s-msk] = val;
	}
}
void MGL_EXPORT mgl_set_mask_(const char *id, const char *mask,int,int l)
{	char *s=new char[l+1];	memcpy(s,mask,l);	s[l]=0;	mgl_set_mask(*id,s);	delete []s;	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_set_mask_val(char id, uint64_t mask)
{
	const char *msk = MGL_MASK_ID, *s = mglchr(msk, id);
	if(s)	mgl_mask_val[s-msk]=mask;
}
void MGL_EXPORT mgl_set_mask_val_(const char *id, uint64_t *mask,int)
{	mgl_set_mask_val(*id,*mask);	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_set_mask_angle(HMGL gr, int angle)	{	gr->SetMaskAngle(angle);	}
void MGL_EXPORT mgl_set_mask_angle_(uintptr_t *gr, int *angle)	{	_GR_->SetMaskAngle(*angle);	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_ask_stop(HMGL gr, int stop)		{	gr->AskStop(stop);	}
void MGL_EXPORT mgl_ask_stop_(uintptr_t *gr, int *stop){	_GR_->AskStop(*stop);	}
int MGL_EXPORT mgl_need_stop(HMGL gr)			{	return gr->NeedStop();	}
int MGL_EXPORT mgl_need_stop_(uintptr_t *gr)	{	return _GR_->NeedStop();}
void MGL_EXPORT mgl_set_event_func(HMGL gr, void (*func)(void *), void *par)
{	gr->SetEventFunc(func,par);	}
//---------------------------------------------------------------------------
