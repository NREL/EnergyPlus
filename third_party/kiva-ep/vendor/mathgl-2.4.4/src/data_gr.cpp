/***************************************************************************
 * data_gr.cpp is part of Math Graphic Library
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
#include <ctype.h>

#ifndef WIN32
#include <glob.h>
#endif

#include "mgl2/datac.h"
#include "mgl2/evalc.h"
#include "mgl2/thread.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
HMDT MGL_NO_EXPORT mglFormulaCalc(const char *str, const std::vector<mglDataA*> &head);
HADT MGL_NO_EXPORT mglFormulaCalcC(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_refill_gr(HMGL gr, HMDT dat, HCDT xdat, HCDT ydat, HCDT zdat, HCDT vdat, long sl, const char *opt)
{
	if(!vdat)	return;
	gr->SaveState(opt);
	if(!ydat && !zdat)	mgl_data_refill_x(dat,xdat,vdat,gr->Min.x,gr->Max.x,sl);
//	else if(!xdat && !zdat)	mgl_data_refill_x(dat,ydat,vdat,gr->Min.y,gr->Max.y,sl);
//	else if(!xdat && !ydat)	mgl_data_refill_x(dat,zdat,vdat,gr->Min.z,gr->Max.z,sl);
	else if(!zdat)	mgl_data_refill_xy(dat,xdat,ydat,vdat,gr->Min.x,gr->Max.x,gr->Min.y,gr->Max.y,sl);
//	else if(!ydat)	mgl_data_refill_xy(dat,xdat,zdat,vdat,gr->Min.x,gr->Max.x,gr->Min.z,gr->Max.z,sl);
//	else if(!xdat)	mgl_data_refill_xy(dat,ydat,zdat,vdat,gr->Min.y,gr->Max.y,gr->Min.z,gr->Max.z,sl);
	else	mgl_data_refill_xyz(dat,xdat,ydat,zdat,vdat,gr->Min.x,gr->Max.x,gr->Min.y,gr->Max.y,gr->Min.z,gr->Max.z);
	gr->LoadState();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_refill_gr(HMGL gr, HADT dat, HCDT xdat, HCDT ydat, HCDT zdat, HCDT vdat, long sl, const char *opt)
{
	if(!vdat)	return;
	gr->SaveState(opt);
	if(!ydat && !zdat)	mgl_datac_refill_x(dat,xdat,vdat,gr->Min.x,gr->Max.x,sl);
//	else if(!xdat && !zdat)	mgl_datac_refill_x(dat,ydat,vdat,gr->Min.y,gr->Max.y,sl);
//	else if(!xdat && !ydat)	mgl_datac_refill_x(dat,zdat,vdat,gr->Min.z,gr->Max.z,sl);
	else if(!zdat)	mgl_datac_refill_xy(dat,xdat,ydat,vdat,gr->Min.x,gr->Max.x,gr->Min.y,gr->Max.y,sl);
//	else if(!ydat)	mgl_datac_refill_xy(dat,xdat,zdat,vdat,gr->Min.x,gr->Max.x,gr->Min.z,gr->Max.z,sl);
//	else if(!xdat)	mgl_datac_refill_xy(dat,ydat,zdat,vdat,gr->Min.y,gr->Max.y,gr->Min.z,gr->Max.z,sl);
	else	mgl_datac_refill_xyz(dat,xdat,ydat,zdat,vdat,gr->Min.x,gr->Max.x,gr->Min.y,gr->Max.y,gr->Min.z,gr->Max.z);
	gr->LoadState();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_refill_x_(uintptr_t *d, uintptr_t *xdat, uintptr_t *vdat, mreal *x1, mreal *x2, long *sl)
{	mgl_data_refill_x(_DT_,_DA_(xdat),_DA_(vdat),*x1,*x2,*sl);	}
void MGL_EXPORT mgl_data_refill_xy_(uintptr_t *d, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *vdat, mreal *x1, mreal *x2, mreal *y1, mreal *y2, long *sl)
{	mgl_data_refill_xy(_DT_,_DA_(xdat),_DA_(ydat),_DA_(vdat),*x1,*x2,*y1,*y2,*sl);	}
void MGL_EXPORT mgl_data_refill_xyz_(uintptr_t *d, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, uintptr_t *vdat, mreal *x1, mreal *x2, mreal *y1, mreal *y2, mreal *z1, mreal *z2)
{	mgl_data_refill_xyz(_DT_,_DA_(xdat),_DA_(ydat),_DA_(zdat),_DA_(vdat),*x1,*x2,*y1,*y2,*z1,*z2);	}
void MGL_EXPORT mgl_data_refill_gr_(uintptr_t *gr, uintptr_t *d, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, uintptr_t *vdat, long *sl, const char *opt,int l)
{	char *s=new char[l+1];	memcpy(s,opt,l);	s[l]=0;
	mgl_data_refill_gr(_GR_,_DT_,_DA_(xdat),_DA_(ydat),_DA_(zdat),_DA_(vdat),*sl,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_refill_x_(uintptr_t *d, uintptr_t *xdat, uintptr_t *vdat, mreal *x1, mreal *x2, long *sl)
{	mgl_datac_refill_x(_DC_,_DA_(xdat),_DA_(vdat),*x1,*x2,*sl);	}
void MGL_EXPORT mgl_datac_refill_xy_(uintptr_t *d, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *vdat, mreal *x1, mreal *x2, mreal *y1, mreal *y2, long *sl)
{	mgl_datac_refill_xy(_DC_,_DA_(xdat),_DA_(ydat),_DA_(vdat),*x1,*x2,*y1,*y2,*sl);	}
void MGL_EXPORT mgl_datac_refill_xyz_(uintptr_t *d, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, uintptr_t *vdat, mreal *x1, mreal *x2, mreal *y1, mreal *y2, mreal *z1, mreal *z2)
{	mgl_datac_refill_xyz(_DC_,_DA_(xdat),_DA_(ydat),_DA_(zdat),_DA_(vdat),*x1,*x2,*y1,*y2,*z1,*z2);	}
void MGL_EXPORT mgl_datac_refill_gr_(uintptr_t *gr, uintptr_t *d, uintptr_t *xdat, uintptr_t *ydat, uintptr_t *zdat, uintptr_t *vdat, long *sl, const char *opt,int l)
{	char *s=new char[l+1];	memcpy(s,opt,l);	s[l]=0;
	mgl_datac_refill_gr(_GR_,_DC_,_DA_(xdat),_DA_(ydat),_DA_(zdat),_DA_(vdat),*sl,s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_fill_eq(HMGL gr, HMDT d, const char *eq, HCDT vdat, HCDT wdat, const char *opt)
{
	if(vdat && vdat->GetNN()!=d->GetNN())	return;	// incompatible dimensions
	if(wdat && wdat->GetNN()!=d->GetNN())	return;
	gr->SaveState(opt);
	std::wstring s = d->Name();	d->Name(L"u");
	mglDataV x(d->nx,d->ny,d->nz, gr->Min.x,gr->Max.x,'x');	x.Name(L"x");
	mglDataV y(d->nx,d->ny,d->nz, gr->Min.y,gr->Max.y,'y');	y.Name(L"y");
	mglDataV z(d->nx,d->ny,d->nz, gr->Min.z,gr->Max.z,'z');	z.Name(L"z");
	mglDataV i(d->nx,d->ny,d->nz, 0,d->nx-1,'x');	i.Name(L"i");
	mglDataV j(d->nx,d->ny,d->nz, 0,d->ny-1,'y');	j.Name(L"j");
	mglDataV k(d->nx,d->ny,d->nz, 0,d->nz-1,'z');	k.Name(L"k");
	mglDataV r(d->nx,d->ny,d->nz);	r.Name(L"#$mgl");
	mglData v(vdat), w(wdat);	v.Name(L"v");	w.Name(L"w");
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&z);	list.push_back(&r);
	list.push_back(d);	list.push_back(&v);	list.push_back(&w);
	list.push_back(&i);	list.push_back(&j);	list.push_back(&k);
	d->Move(mglFormulaCalc(eq,list));	d->Name(s.c_str());	gr->LoadState();
}
void MGL_EXPORT mgl_data_fill_eq_(uintptr_t *gr, uintptr_t *d, const char *eq, uintptr_t *v, uintptr_t *w, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_data_fill_eq(_GR_,_DT_,s,_DA_(v),_DA_(w),o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_fill_eq(HMGL gr, HADT d, const char *eq, HCDT vdat, HCDT wdat, const char *opt)
{
	if(vdat && vdat->GetNN()!=d->GetNN())	return;	// incompatible dimensions
	if(wdat && wdat->GetNN()!=d->GetNN())	return;
	gr->SaveState(opt);
	std::wstring s = d->Name();	d->Name(L"u");
	mglDataV x(d->nx,d->ny,d->nz, gr->Min.x,gr->Max.x,'x');	x.Name(L"x");
	mglDataV y(d->nx,d->ny,d->nz, gr->Min.y,gr->Max.y,'y');	y.Name(L"y");
	mglDataV z(d->nx,d->ny,d->nz, gr->Min.z,gr->Max.z,'z');	z.Name(L"z");
	mglDataV i(d->nx,d->ny,d->nz, 0,d->nx-1,'x');	i.Name(L"i");
	mglDataV j(d->nx,d->ny,d->nz, 0,d->ny-1,'y');	j.Name(L"j");
	mglDataV k(d->nx,d->ny,d->nz, 0,d->nz-1,'z');	k.Name(L"k");
	mglDataV r(d->nx,d->ny,d->nz);	r.Name(L"#$mgl");
	mglData v(vdat), w(wdat);	v.Name(L"v");	w.Name(L"w");
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&z);	list.push_back(&r);
	list.push_back(d);	list.push_back(&v);	list.push_back(&w);
	list.push_back(&i);	list.push_back(&j);	list.push_back(&k);
	d->Move(mglFormulaCalcC(eq,list));	d->Name(s.c_str());	gr->LoadState();
}
void MGL_EXPORT mgl_datac_fill_eq_(uintptr_t *gr, uintptr_t *d, const char *eq, uintptr_t *v, uintptr_t *w, const char *opt,int l,int lo)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	char *o=new char[lo+1];	memcpy(o,opt,lo);	o[lo]=0;
	mgl_datac_fill_eq(_GR_,_DC_,s,_DA_(v),_DA_(w),o);	delete []o;	delete []s;	}
//-----------------------------------------------------------------------------
