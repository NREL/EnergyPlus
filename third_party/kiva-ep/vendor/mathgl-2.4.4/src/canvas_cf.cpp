/***************************************************************************
 * canvas_cf.cpp is part of Math Graphic Library
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
#include "mgl2/canvas.h"
#include "mgl2/canvas_cf.h"
#include "mgl2/eval.h"
#include "mgl2/evalc.h"
//-----------------------------------------------------------------------------
#undef _GR_
#define _GR_	((mglCanvas *)(*gr))
//-----------------------------------------------------------------------------
MGL_EXPORT const unsigned char *mgl_get_rgb(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetBits():0;	}
MGL_EXPORT const unsigned char *mgl_get_rgba(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetRGBA():0;	}
MGL_EXPORT_PURE const unsigned char* mgl_get_background(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetBackground():0;	}
int MGL_EXPORT mgl_get_width(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetWidth():0;	}
int MGL_EXPORT mgl_get_height(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetHeight():0;	}
void MGL_EXPORT mgl_calc_xyz(HMGL gr, int xs, int ys, mreal *x, mreal *y, mreal *z)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);
	mglPoint p = g?g->CalcXYZ(xs,ys):mglPoint(NAN,NAN,NAN);
	*x = p.x;	*y = p.y;	*z = p.z;	}
void MGL_EXPORT mgl_calc_scr(HMGL gr, double x, double y, double z, int *xs, int *ys)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->CalcScr(mglPoint(x,y,z),xs,ys);	}
void MGL_EXPORT mgl_set_obj_id(HMGL gr, int id)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetObjId(id);	}
int MGL_EXPORT_PURE mgl_get_obj_id(HMGL gr, int x, int y)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetObjId(x,y):-1;	}
int MGL_EXPORT_PURE mgl_get_spl_id(HMGL gr, int x, int y)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetSplId(x,y):-1;	}
//-----------------------------------------------------------------------------
long MGL_EXPORT_PURE mgl_is_active(HMGL gr, int xs, int ys, int d)
{
	if(d<=0) 	d=1;
	for(size_t i=0;i<gr->Act.size();i++)
	{
		const mglActivePos &p = gr->Act[i];
		if(abs(xs-p.x)<d && abs(ys-p.y)<d)	return i;
	}
	return -1;
}
long MGL_EXPORT_PURE mgl_is_active_(uintptr_t *gr, int *xs, int *ys, int *d)
{	return mgl_is_active(_GR_, *xs, *ys, *d);	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_new_frame(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->NewFrame():-1;	}
void MGL_EXPORT mgl_end_frame(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->EndFrame();	}
int MGL_EXPORT_PURE mgl_get_num_frame(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetNumFrame():0;	}
void MGL_EXPORT mgl_reset_frames(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->ResetFrames();	}
void MGL_EXPORT mgl_get_frame(HMGL gr, int i)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->GetFrame(i);	}
void MGL_EXPORT mgl_set_frame(HMGL gr, int i)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetFrame(i);	}
void MGL_EXPORT mgl_show_frame(HMGL gr, int i)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->ShowFrame(i);	}
void MGL_EXPORT mgl_del_frame(HMGL gr, int i)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->DelFrame(i);	}
void MGL_EXPORT mgl_clear_frame(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->ClearFrame();	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_transp_type(HMGL gr, int type)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTranspType(type);	}
void MGL_EXPORT mgl_set_alpha(HMGL gr, int enable)	{	gr->Alpha(enable);	}
void MGL_EXPORT mgl_set_gray(HMGL gr, int enable)	{	gr->set(enable, MGL_GRAY_MODE);	}
void MGL_EXPORT mgl_set_fog(HMGL gr, double d, double dz)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Fog(d,dz);	}
void MGL_EXPORT mgl_set_light(HMGL gr, int enable)	{	gr->Light(enable);	}
void MGL_EXPORT mgl_set_attach_light(HMGL gr, int enable)	{	gr->AttachLight(enable);	}
void MGL_EXPORT mgl_set_light_n(HMGL gr, int n, int enable)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Light(n, enable);	}
void MGL_EXPORT mgl_add_light_ext(HMGL gr, int n, double x, double y, double z, char c, double br, double ap)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AddLight(n,mglPoint(x,y,z),c,br,ap);	}
void MGL_EXPORT mgl_add_light_loc(HMGL gr, int n, double x, double y, double z, double dx, double dy, double dz, char c, double br, double ap)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AddLight(n,mglPoint(x,y,z),mglPoint(dx,dy,dz),c,br,ap);	}
void MGL_EXPORT mgl_add_light(HMGL gr, int n, double x, double y, double z)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AddLight(n,mglPoint(x,y,z));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mat_push(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Push();	}
void MGL_EXPORT mgl_mat_pop(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Pop();	}
void MGL_EXPORT mgl_clf(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Clf();	}
void MGL_EXPORT mgl_clf_chr(HMGL gr, char ch)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Clf(mglColor(ch));	}
void MGL_EXPORT mgl_clf_rgb(HMGL gr, double r, double g, double b)
{	mglCanvas *gg = dynamic_cast<mglCanvas *>(gr);	if(gg)	gg->Clf(mglColor(r,g,b));	}
void MGL_EXPORT mgl_clf_str(HMGL gr, const char *col)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Clf(col);	}
void MGL_EXPORT mgl_load_background(HMGL gr, const char *fn, double alpha)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->LoadBackground(fn,alpha);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_subplot_d(HMGL gr, int nx,int ny,int m,const char *style,double dx,double dy)
{
	double x1,x2,y1,y2;
	int mx = m%nx, my = m/nx;
	if(gr->get(MGL_AUTO_FACTOR))	{	dx /= 1.55;	dy /= 1.55;	}
	else	{	dx /= 2;	dy /= 2;	}
	x1 = (mx+dx)/nx;		x2 = (mx+1+dx)/nx;
	y2 = 1.f-(my+dy)/ny;	y1 = 1.f-(my+1+dy)/ny;
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);
	if(g)	g->InPlot(x1,x2,y1,y2,style);
}
void MGL_EXPORT mgl_subplot(HMGL gr, int nx,int ny,int m,const char *style)
{	mgl_subplot_d(gr,nx,ny,m,style,0,0);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_multiplot_d(HMGL gr, int nx,int ny,int m,int dx,int dy,const char *style,double sx,double sy)
{
	double x1,x2,y1,y2;
	int mx = m%nx, my = m/nx;
	if(gr->get(MGL_AUTO_FACTOR))	{	sx /= 1.55;	sy /= 1.55;	}
	else	{	sx /= 2;	sy /= 2;	}
	dx = (dx<1 || dx+mx>nx) ? 1 : dx;
	dy = (dy<1 || dy+my>ny) ? 1 : dy;
	x1 = double(mx+sx)/nx;		x2 = double(mx+dx+sx)/nx;
	y2 = 1-double(my+sy)/ny;	y1 = 1-double(my+dy+sy)/ny;
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->InPlot(x1,x2,y1,y2,style);
}
void MGL_EXPORT mgl_multiplot(HMGL gr, int nx,int ny,int m,int dx,int dy,const char *style)
{	mgl_multiplot_d(gr,nx,ny,m,dx,dy,style,0,0);	}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_inplot(HMGL gr, double x1,double x2,double y1,double y2)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->InPlot(x1,x2,y1,y2,false);	}
void MGL_EXPORT mgl_relplot(HMGL gr, double x1,double x2,double y1,double y2)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->InPlot(x1,x2,y1,y2,true);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_columnplot(HMGL gr, int num, int i, double dd)
{
	double w = 1./num;
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);
	if(g)	g->InPlot(0,1,1-w*(i+1-dd/2),1-(i+dd/2)*w,true);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_gridplot(HMGL gr, int nx, int ny, int i, double dd)
{
	int ix=i%nx, iy=i/nx;
	double wx = 1./nx, wy = 1./ny;
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);
	if(g)	g->InPlot((ix+dd/2)*wx,wx*(ix+1-dd/2),1-wy*(iy+1-dd/2),1-(iy+dd/2)*wy,true);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_stickplot(HMGL gr, int num, int i, double tet, double phi)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->StickPlot(num, i, tet, phi);	}
void MGL_EXPORT mgl_shearplot(HMGL gr, int num, int i, double sx, double sy, double xd, double yd)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->ShearPlot(num, i, sx, sy, xd, yd);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_aspect(HMGL gr, double Ax,double Ay,double Az)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Aspect(Ax,Ay,Az);	}
void MGL_EXPORT mgl_shear(HMGL gr, double Sx,double Sy)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Shear(Sx,Sy);	}
void MGL_EXPORT mgl_rotate(HMGL gr, double TetX,double TetZ,double TetY)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Rotate(TetX,TetZ,TetY);	}
void MGL_EXPORT mgl_view(HMGL gr, double TetX,double TetZ,double TetY)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->View(TetX,TetZ,TetY);	}
void MGL_EXPORT mgl_zoom(HMGL gr, double x1, double y1, double x2, double y2)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Zoom(x1,y1,x2,y2);	}
void MGL_EXPORT mgl_rotate_vector(HMGL gr, double Tet,double x,double y,double z)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->RotateN(Tet,x,y,z);	}
void MGL_EXPORT mgl_perspective(HMGL gr, double val)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Perspective(val);	}
void MGL_EXPORT mgl_ask_perspective(HMGL gr, double val)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Perspective(val,false);	}
void MGL_EXPORT mgl_title(HMGL gr, const char *title, const char *stl, double size)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Title(title,stl,size);	}
void MGL_EXPORT mgl_titlew(HMGL gr, const wchar_t *title, const char *stl, double size)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Title(title,stl,size);	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_new_frame_(uintptr_t *gr)	{	return _GR_->NewFrame();	}
void MGL_EXPORT mgl_end_frame_(uintptr_t *gr)	{	_GR_->EndFrame();	}
int MGL_EXPORT_PURE mgl_get_num_frame_(uintptr_t *gr)	{	return _GR_->GetNumFrame();	}
void MGL_EXPORT mgl_reset_frames_(uintptr_t *gr)	{	_GR_->ResetFrames();	}
void MGL_EXPORT mgl_get_frame_(uintptr_t *gr, int *i)	{	_GR_->GetFrame(*i);	}
void MGL_EXPORT mgl_set_frame_(uintptr_t *gr, int *i)	{	_GR_->SetFrame(*i);	}
void MGL_EXPORT mgl_show_frame_(uintptr_t *gr, int *i)	{	_GR_->ShowFrame(*i);}
void MGL_EXPORT mgl_del_frame_(uintptr_t *gr, int *i)	{	_GR_->DelFrame(*i);	}
void MGL_EXPORT mgl_clear_frame_(uintptr_t *gr)			{	_GR_->ClearFrame();	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_transp_type_(uintptr_t *gr, int *type)		{	_GR_->SetTranspType(*type);	}
void MGL_EXPORT mgl_set_alpha_(uintptr_t *gr, int *enable)			{	_GR_->Alpha(*enable);	}
void MGL_EXPORT mgl_set_gray_(uintptr_t *gr, int *enable)			{	_GR_->set(*enable, MGL_GRAY_MODE);	}
void MGL_EXPORT mgl_set_fog_(uintptr_t *gr, mreal *d, mreal *dz)	{	_GR_->Fog(*d, *dz);		}
void MGL_EXPORT mgl_set_light_(uintptr_t *gr, int *enable)			{	_GR_->Light(*enable);	}
void MGL_EXPORT mgl_set_attach_light_(uintptr_t *gr, int *enable)	{	_GR_->AttachLight(*enable);	}
void MGL_EXPORT mgl_set_light_n_(uintptr_t *gr, int *n, int *enable)
{	_GR_->Light(*n, *enable);	}
void MGL_EXPORT mgl_add_light_(uintptr_t *gr, int *n, mreal *x, mreal *y, mreal *z)
{	_GR_->AddLight(*n,mglPoint(*x,*y,*z));	}
void MGL_EXPORT mgl_add_light_ext_(uintptr_t *gr, int *n, mreal *x, mreal *y, mreal *z, char *c, mreal *br, mreal *ap, int)
{	_GR_->AddLight(*n,mglPoint(*x,*y,*z),*c,*br,*ap);	}
void MGL_EXPORT mgl_add_light_loc_(uintptr_t *gr, int *n, mreal *x, mreal *y, mreal *z, mreal *dx, mreal *dy, mreal *dz, char *c, mreal *br, mreal *ap, int)
{	_GR_->AddLight(*n,mglPoint(*x,*y,*z),mglPoint(*dx,*dy,*dz),*c,*br,*ap);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_mat_push_(uintptr_t *gr)	{	_GR_->Push();	}
void MGL_EXPORT mgl_mat_pop_(uintptr_t *gr)	{	_GR_->Pop();	}
void MGL_EXPORT mgl_clf_(uintptr_t *gr)		{	_GR_->Clf();	}
void MGL_EXPORT mgl_clf_chr_(uintptr_t *gr, const char *ch, int)
{	_GR_->Clf(mglColor(*ch));	}
void MGL_EXPORT mgl_clf_rgb_(uintptr_t *gr, mreal *r, mreal *g, mreal *b)
{	_GR_->Clf(mglColor(*r,*g,*b));	}
void MGL_EXPORT mgl_clf_str_(uintptr_t *gr, const char *col, int l)
{	char *s=new char[l+1];	memcpy(s,col,l);	s[l]=0;
	mgl_clf_str(_GR_,s);	delete []s;	}
void MGL_EXPORT mgl_load_background_(uintptr_t *gr, const char *fn, mreal *a, int l)
{	char *s=new char[l+1];	memcpy(s,fn,l);	s[l]=0;
	mgl_load_background(_GR_,s,*a);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_subplot_d_(uintptr_t *gr, int *nx,int *ny,int *m,const char *st, mreal *dx, mreal *dy,int l)
{	char *s=new char[l+1];	memcpy(s,st,l);	s[l]=0;
	mgl_subplot_d(_GR_,*nx,*ny,*m,s,*dx,*dy);	delete []s;	}
void MGL_EXPORT mgl_subplot_(uintptr_t *gr, int *nx,int *ny,int *m,const char *st,int l)
{	char *s=new char[l+1];	memcpy(s,st,l);	s[l]=0;
	mgl_subplot(_GR_,*nx,*ny,*m,s);	delete []s;	}
void MGL_EXPORT mgl_multiplot_(uintptr_t *gr, int *nx,int *ny,int *m,int *dx,int *dy,const char *st,int l)
{	char *s=new char[l+1];	memcpy(s,st,l);	s[l]=0;
	mgl_multiplot(_GR_,*nx,*ny,*m,*dx,*dy,s);	delete []s;	}
void MGL_EXPORT mgl_multiplot_d_(uintptr_t *gr, int *nx,int *ny,int *m,int *dx,int *dy,const char *st, mreal *sx, mreal *sy,int l)
{	char *s=new char[l+1];	memcpy(s,st,l);	s[l]=0;
	mgl_multiplot_d(_GR_,*nx,*ny,*m,*dx,*dy,s, *sx, *sy);	delete []s;	}
void MGL_EXPORT mgl_inplot_(uintptr_t *gr, mreal *x1, mreal *x2, mreal *y1, mreal *y2)
{	_GR_->InPlot(*x1,*x2,*y1,*y2,false);	}
void MGL_EXPORT mgl_relplot_(uintptr_t *gr, mreal *x1, mreal *x2, mreal *y1, mreal *y2)
{	_GR_->InPlot(*x1,*x2,*y1,*y2,true);	}
void MGL_EXPORT mgl_columnplot_(uintptr_t *gr, int *num, int *i, mreal *d)
{	mgl_columnplot(_GR_,*num,*i,*d);	}
void MGL_EXPORT mgl_gridplot_(uintptr_t *gr, int *nx, int *ny, int *i, mreal *d)
{	mgl_gridplot(_GR_,*nx,*ny,*i,*d);	}
void MGL_EXPORT mgl_stickplot_(uintptr_t *gr, int *num, int *i, mreal *tet, mreal *phi)
{	_GR_->StickPlot(*num, *i, *tet, *phi);	}
void MGL_EXPORT mgl_shearplot_(uintptr_t *gr, int *num, int *i, mreal *sy, mreal *sx, mreal *xd, mreal *yd)
{	_GR_->ShearPlot(*num,*i,*sx,*sy,*xd,*yd);	}

void MGL_EXPORT mgl_title_(uintptr_t *gr, const char *title, const char *stl, mreal *size, int l,int m)
{	char *t=new char[l+1];	memcpy(t,title,l);	t[l]=0;
	char *s=new char[m+1];	memcpy(s,stl,m);	s[m]=0;
	_GR_->Title(t,s,*size);	delete []s;	delete []t;	}
void MGL_EXPORT mgl_aspect_(uintptr_t *gr, mreal *Ax, mreal *Ay, mreal *Az)
{	_GR_->Aspect(*Ax,*Ay,*Az);	}
void MGL_EXPORT mgl_shear_(uintptr_t *gr, mreal *Sx, mreal *Sy)
{	_GR_->Shear(*Sx,*Sy);	}
void MGL_EXPORT mgl_rotate_(uintptr_t *gr, mreal *TetX, mreal *TetZ, mreal *TetY)
{	_GR_->Rotate(*TetX,*TetZ,*TetY);	}
void MGL_EXPORT mgl_view_(uintptr_t *gr, mreal *TetX, mreal *TetZ, mreal *TetY)
{	_GR_->View(*TetX,*TetZ,*TetY);	}
void MGL_EXPORT mgl_zoom_(uintptr_t *gr, mreal *x1, mreal *y1, mreal *x2, mreal *y2)
{	_GR_->Zoom(*x1,*y1,*x2,*y2);	}
void MGL_EXPORT mgl_rotate_vector_(uintptr_t *gr, mreal *Tet, mreal *x, mreal *y, mreal *z)
{	_GR_->RotateN(*Tet,*x,*y,*z);	}
void MGL_EXPORT mgl_perspective_(uintptr_t *gr, mreal *val)
{	_GR_->Perspective(*val);	}
void MGL_EXPORT mgl_ask_perspective_(uintptr_t *gr, mreal *val)
{	mgl_ask_perspective(_GR_,*val);	}
//-----------------------------------------------------------------------------
MGL_EXPORT const unsigned char *mgl_get_rgb_(uintptr_t *gr)	{	return gr ? _GR_->GetBits():0;	}
MGL_EXPORT const unsigned char *mgl_get_rgba_(uintptr_t *gr){	return gr ? _GR_->GetRGBA():0;	}
MGL_EXPORT_PURE const unsigned char* mgl_get_background_(uintptr_t* gr)	{	return gr ? _GR_->GetBackground():0;	}
int MGL_EXPORT mgl_get_width_(uintptr_t *gr)	{	return gr ? _GR_->GetWidth():0;	}
int MGL_EXPORT mgl_get_height_(uintptr_t *gr)	{	return gr ? _GR_->GetHeight():0;}
void MGL_EXPORT mgl_calc_xyz_(uintptr_t *gr, int *xs, int *ys, mreal *x, mreal *y, mreal *z)
{	mglPoint p = _GR_->CalcXYZ(*xs,*ys);	*x = p.x;	*y = p.y;	*z = p.z;	}
void MGL_EXPORT mgl_calc_scr_(uintptr_t *gr, mreal *x, mreal *y, mreal *z, int *xs, int *ys)
{	_GR_->CalcScr(mglPoint(*x,*y,*z),xs,ys);	}
void MGL_EXPORT mgl_set_obj_id_(uintptr_t *gr, int *id)		{	_GR_->SetObjId(*id);	}
int MGL_EXPORT_PURE mgl_get_obj_id_(uintptr_t *gr, int *x, int *y)	{	return _GR_->GetObjId(*x,*y);	}
int MGL_EXPORT_PURE mgl_get_spl_id_(uintptr_t *gr, int *x, int *y)	{	return _GR_->GetSplId(*x,*y);	}
//-----------------------------------------------------------------------------
double mgl_size_scl = 1;
HMGL MGL_EXPORT mgl_create_graph(int width, int height)
{	return new mglCanvas(width,height);	}
void MGL_EXPORT mgl_delete_graph(HMGL gr)	{	if(gr)	delete gr;	}
void MGL_EXPORT mgl_set_size_scl(double scl){	if(scl>0)	mgl_size_scl = scl;	}
void MGL_EXPORT mgl_set_size(HMGL gr, int width, int height)
{
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);
	width = int(mgl_size_scl*width);	height = int(mgl_size_scl*height);
	if(g)	g->SetSize(width, height);
}
void MGL_EXPORT mgl_scale_size(HMGL gr, int width, int height)
{
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);
	width = int(mgl_size_scl*width);	height = int(mgl_size_scl*height);
	if(g)	g->SetSize(width, height, false);
}
void MGL_EXPORT mgl_set_def_param(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->DefaultPlotParam();	}
void MGL_EXPORT mgl_combine_gr(HMGL gr, HMGL in)
{	const mglCanvas *gg = dynamic_cast<const mglCanvas *>(in);
	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g && gg)	g->Combine(gg);	}
void MGL_EXPORT mgl_set_bbox(HMGL gr, int x1, int y1, int x2, int y2)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetBBox(x1,y1,x2,y2);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_tick_len(HMGL gr, double len, double stt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTickLen(len,stt);	}
void MGL_EXPORT mgl_set_axis_stl(HMGL gr, const char *stl, const char *tck, const char *sub)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetAxisStl(stl,tck,sub);	}
void MGL_EXPORT mgl_tune_ticks(HMGL gr, int tune, double pos)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTuneTicks(tune,pos);	}
void MGL_EXPORT mgl_adjust_ticks(HMGL gr, const char *dir)
{	mgl_adjust_ticks_ext(gr,dir,"");	}
void MGL_EXPORT mgl_adjust_ticks_ext(HMGL gr, const char *dir, const char *stl)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AdjustTicks(dir,true,stl);	}
void MGL_EXPORT mgl_set_ticks(HMGL gr, char dir, double d, int ns, double org)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTicks(dir,d,ns,org);	}
void MGL_EXPORT mgl_set_ticks_factw(HMGL gr, char dir, double d, int ns, double org, const wchar_t *fact)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTicks(dir,d,ns,org,fact);	}
void MGL_EXPORT mgl_set_ticks_fact(HMGL gr, char dir, double d, int ns, double org, const char *fact)
{	MGL_TO_WCS(fact,mgl_set_ticks_factw(gr,dir,d,ns,org,wcs));	}
void MGL_EXPORT mgl_set_ticks_str(HMGL gr, char dir, const char *lbl, int add)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTicksVal(dir,lbl,add);	}
void MGL_EXPORT mgl_set_ticks_wcs(HMGL gr, char dir, const wchar_t *lbl, int add)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTicksVal(dir,lbl,add);	}
void MGL_EXPORT mgl_set_ticks_val(HMGL gr, char dir, HCDT val, const char *lbl, int add)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTicksVal(dir,val,lbl,add);	}
void MGL_EXPORT mgl_set_ticks_valw(HMGL gr, char dir, HCDT val, const wchar_t *lbl, int add)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTicksVal(dir,val,lbl,add);	}
void MGL_EXPORT mgl_add_tick(HMGL gr, char dir, double val, const char *lbl)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AddTick(dir,val,lbl);	}
void MGL_EXPORT mgl_add_tickw(HMGL gr, char dir, double val, const wchar_t *lbl)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AddTick(dir,val,lbl);	}
void MGL_EXPORT mgl_set_tick_templ(HMGL gr, char dir, const char *templ)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTickTempl(dir,templ);	}
void MGL_EXPORT mgl_set_tick_templw(HMGL gr, char dir, const wchar_t *templ)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTickTempl(dir,templ);	}
void MGL_EXPORT mgl_set_ticks_time(HMGL gr, char dir, double d, const char *t)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTickTime(dir,d,t);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_box(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Box();	}
void MGL_EXPORT mgl_box_str(HMGL gr, const char *col, int ticks)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Box(col,ticks);	}
void MGL_EXPORT mgl_axis(HMGL gr, const char *dir, const char *stl, const char *opt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Axis(dir,stl,opt);	}
void MGL_EXPORT mgl_axis_grid(HMGL gr, const char *dir,const char *pen, const char *opt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Grid(dir,pen,opt);	}
void MGL_EXPORT mgl_label(HMGL gr, char dir, const char *text, double pos, const char *opt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Label(dir,text,pos,opt);	}
void MGL_EXPORT mgl_labelw(HMGL gr, char dir, const wchar_t *text, double pos, const char *opt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Labelw(dir,text,pos,opt);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_colorbar(HMGL gr, const char *sch)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Colorbar(sch);	}
void MGL_EXPORT mgl_colorbar_ext(HMGL gr, const char *sch, double x, double y, double w, double h)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Colorbar(sch,x,y,w,h);	}
void MGL_EXPORT mgl_colorbar_val(HMGL gr, HCDT dat, const char *sch)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Colorbar(dat,sch);	}
void MGL_EXPORT mgl_colorbar_val_ext(HMGL gr, HCDT dat, const char *sch,double x, double y, double w, double h)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Colorbar(dat,sch,x,y,w,h);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_add_legend(HMGL gr, const char *text,const char *style)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AddLegend(text,style);	}
void MGL_EXPORT mgl_add_legendw(HMGL gr, const wchar_t *text,const char *style)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->AddLegend(text,style);	}
void MGL_EXPORT mgl_clear_legend(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->ClearLegend();	}
void MGL_EXPORT mgl_legend_pos(HMGL gr, double x, double y, const char *font, const char *opt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Legend(x,y,font,opt);	}
void MGL_EXPORT mgl_legend(HMGL gr, int where, const char *font, const char *opt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Legend(where,font,opt);	}
void MGL_EXPORT mgl_set_legend_marks(HMGL gr, int num)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetLegendMarks(num);	}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_create_graph_(int *width, int *height)
{	return uintptr_t(new mglCanvas(*width,*height));	}
void MGL_EXPORT mgl_delete_graph_(uintptr_t *gr)	{	delete _GR_;	}
void MGL_EXPORT mgl_set_size_scl_(double *scl)	{	mgl_set_size_scl(*scl);	}
void MGL_EXPORT mgl_set_size_(uintptr_t *gr, int *width, int *height)
{	mgl_set_size(_GR_,*width,*height);	}
void MGL_EXPORT mgl_scale_size_(uintptr_t *gr, int *width, int *height)
{	mgl_scale_size(_GR_,*width,*height);	}
void MGL_EXPORT mgl_set_def_param_(uintptr_t *gr)	{	_GR_->DefaultPlotParam();	}
void MGL_EXPORT mgl_combine_gr_(uintptr_t *gr, uintptr_t *in)
{	_GR_->Combine((mglCanvas *)in);	}
void MGL_EXPORT mgl_set_bbox_(uintptr_t *gr, int *x1, int *y1, int *x2, int *y2)
{	_GR_->SetBBox(*x1,*y1,*x2,*y2);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_ticks_fact_(uintptr_t *gr, char *dir, double *d, int *ns, double *org, const char *fact,int,int l)
{	char *s=new char[l+1];	memcpy(s,fact,l);	s[l]=0;
	mgl_set_ticks_fact(_GR_,*dir,*d,*ns,*org,s);	delete []s;	}
void MGL_EXPORT mgl_set_tick_len_(uintptr_t *gr, mreal *len, mreal *stt)
{	_GR_->SetTickLen(*len, *stt);	}
void MGL_EXPORT mgl_set_axis_stl_(uintptr_t *gr, const char *stl, const char *tck, const char *sub, int l,int m,int n)
{	char *a=new char[l+1];	memcpy(a,stl,l);	a[l]=0;
	char *t=new char[m+1];	memcpy(t,tck,m);	t[m]=0;
	char *s=new char[n+1];	memcpy(s,sub,n);	s[n]=0;
	_GR_->SetAxisStl(a,t,s);	delete []a;	delete []s;	delete []t;	}
void MGL_EXPORT mgl_adjust_ticks_(uintptr_t *gr, const char *dir, int l)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	_GR_->AdjustTicks(s,true);	delete []s;	}
void MGL_EXPORT mgl_adjust_ticks_ext_(uintptr_t *gr, const char *dir, const char *stl, int l, int m)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	char *f=new char[m+1];	memcpy(f,stl,m);	f[m]=0;
	_GR_->AdjustTicks(s,true,f);	delete []s;	delete []f;	}
void MGL_EXPORT mgl_set_ticks_(uintptr_t *gr, char *dir, mreal *d, int *ns, mreal *org, int)
{	_GR_->SetTicks(*dir, *d, *ns, *org);	}
void MGL_EXPORT mgl_set_ticks_str_(uintptr_t *gr, const char *dir, const char *lbl, int *add,int,int l)
{	char *s=new char[l+1];	memcpy(s,lbl,l);	s[l]=0;
	_GR_->SetTicksVal(*dir,s,*add);	delete []s;	}
void MGL_EXPORT mgl_set_ticks_val_(uintptr_t *gr, const char *dir, uintptr_t *val, const char *lbl, int *add,int,int l)
{	char *s=new char[l+1];	memcpy(s,lbl,l);	s[l]=0;
	_GR_->SetTicksVal(*dir,_DA_(val),s,*add);	delete []s;	}
void MGL_EXPORT mgl_add_tick_(uintptr_t *gr, const char *dir, mreal *val, const char *lbl, int,int l)
{	char *s=new char[l+1];	memcpy(s,lbl,l);	s[l]=0;
	mgl_add_tick(_GR_,*dir,*val,s);	delete []s;	}
void MGL_EXPORT mgl_tune_ticks_(uintptr_t *gr, int *tune, mreal *fact_pos)
{	_GR_->SetTuneTicks(*tune, *fact_pos);	}
void MGL_EXPORT mgl_set_tick_templ_(uintptr_t *gr, const char *dir, const char *templ,int,int l)
{	char *s=new char[l+1];	memcpy(s,templ,l);	s[l]=0;
	_GR_->SetTickTempl(*dir,s);	delete []s;	}
void MGL_EXPORT mgl_set_ticks_time_(uintptr_t *gr, const char *dir, mreal *d, const char *t,int,int l)
{	char *s=new char[l+1];	memcpy(s,t,l);	s[l]=0;
	_GR_->SetTickTime(*dir,*d,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_box_(uintptr_t *gr)	{	_GR_->Box();	}
void MGL_EXPORT mgl_box_str_(uintptr_t *gr, const char *col, int *ticks, int l)
{	char *s=new char[l+1];	memcpy(s,col,l);	s[l]=0;
	_GR_->Box(s,*ticks);	delete []s;	}
void MGL_EXPORT mgl_axis_(uintptr_t *gr, const char *dir, const char *stl, const char *opt,int l,int n,int m)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	char *p=new char[n+1];	memcpy(p,stl,l);	p[n]=0;
	char *o=new char[m+1];	memcpy(o,opt,m);	o[m]=0;
	_GR_->Axis(s,p,o);	delete []s;	delete []p;	delete []o;	}
void MGL_EXPORT mgl_axis_grid_(uintptr_t *gr, const char *dir,const char *pen, const char *opt,int l,int n,int m)
{	char *s=new char[l+1];	memcpy(s,dir,l);	s[l]=0;
	char *p=new char[n+1];	memcpy(p,pen,n);	p[n]=0;
	char *o=new char[m+1];	memcpy(o,opt,m);	o[m]=0;
	_GR_->Grid(s,p,o);	delete []s;	delete []p;	delete []o;	}
void MGL_EXPORT mgl_label_(uintptr_t *gr, const char *dir, const char *text, mreal *pos, const char *opt,int,int l,int m)
{	char *s=new char[l+1];	memcpy(s,text,l);	s[l]=0;
	char *o=new char[m+1];	memcpy(o,opt,m);	o[m]=0;
	_GR_->Label(*dir, s, *pos, o);	delete []s;	delete []o;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_colorbar_(uintptr_t *gr, const char *sch,int l)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	_GR_->Colorbar(s);	delete []s;	}
void MGL_EXPORT mgl_colorbar_ext_(uintptr_t *gr, const char *sch, mreal *x, mreal *y, mreal *w, mreal *h, int l)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	_GR_->Colorbar(s,*x,*y,*w,*h);	delete []s;	}
void MGL_EXPORT mgl_colorbar_val_(uintptr_t *gr, uintptr_t *dat, const char *sch,int l)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	_GR_->Colorbar(_DA_(dat), s);	delete []s;	}
void MGL_EXPORT mgl_colorbar_val_ext_(uintptr_t *gr, uintptr_t *dat, const char *sch, mreal *x, mreal *y, mreal *w, mreal *h, int l)
{	char *s=new char[l+1];	memcpy(s,sch,l);	s[l]=0;
	_GR_->Colorbar(_DA_(dat),s,*x,*y,*w,*h);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_add_legend_(uintptr_t *gr, const char *text,const char *style,int l,int n)
{	char *s=new char[l+1];	memcpy(s,text,l);	s[l]=0;
	char *f=new char[n+1];	memcpy(f,style,n);	f[n]=0;
	_GR_->AddLegend(s,f);	delete []s;	delete []f;	}
void MGL_EXPORT mgl_clear_legend_(uintptr_t *gr)	{	if(gr)	_GR_->ClearLegend();	}
void MGL_EXPORT mgl_legend_pos_(uintptr_t *gr, mreal *x, mreal *y, const char *font, const char *opt,int l,int m)
{	char *s=new char[l+1];	memcpy(s,font,l);	s[l]=0;
	char *o=new char[m+1];	memcpy(o,opt,m);	o[m]=0;
	_GR_->Legend(*x, *y, s, o);	delete []s;	delete []o;	}
void MGL_EXPORT mgl_legend_(uintptr_t *gr, int *where, const char *font, const char *opt,int l,int m)
{	char *s=new char[l+1];	memcpy(s,font,l);	s[l]=0;
	char *o=new char[m+1];	memcpy(o,opt,m);	o[m]=0;
	_GR_->Legend(*where, s, o);	delete []s;	delete []o;	}
void MGL_EXPORT mgl_set_legend_marks_(uintptr_t *gr, int *num)
{	_GR_->SetLegendMarks(*num);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_plotfactor(HMGL gr, double val)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetPlotFactor(val);	}
void MGL_EXPORT mgl_set_plotfactor_(uintptr_t *gr, mreal *val)
{	_GR_->SetPlotFactor(*val);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_set_tick_shift(HMGL gr, double sx, double sy, double sz, double sc)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetTickShift(mglPoint(sx,sy,sz,sc));	}
void MGL_EXPORT mgl_set_tick_shift_(uintptr_t *gr, mreal *sx, mreal *sy, mreal *sz, mreal *sc)
{	_GR_->SetTickShift(mglPoint(*sx,*sy,*sz,*sc));	}
//-----------------------------------------------------------------------------
#if !MGL_HAVE_PNG
void MGL_EXPORT mgl_write_prc(HMGL gr, const char *fname,const char *descr, int make_pdf)
{	mgl_set_global_warn(_("PNG support was disabled. Please, enable it and rebuild MathGL."));	}
void MGL_EXPORT mgl_write_prc_(uintptr_t *graph, const char *fname,const char *descr, int *make_pdf,int lf,int ld)
{	mgl_set_global_warn(_("PNG support was disabled. Please, enable it and rebuild MathGL."));	}
#endif
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_finish(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Finish();	}
void MGL_EXPORT mgl_finish_(uintptr_t *gr)	{	_GR_->Finish();	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_rasterize(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->Rasterize();	}
void MGL_EXPORT mgl_rasterize_(uintptr_t *gr)	{	_GR_->Rasterize();	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_pen_delta(HMGL gr, double d)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetPenDelta(d);	}
void MGL_EXPORT mgl_pen_delta_(uintptr_t *gr, double *d)	{	_GR_->SetPenDelta(*d);	}
//-----------------------------------------------------------------------------
