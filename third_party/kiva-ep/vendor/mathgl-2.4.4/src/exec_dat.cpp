/***************************************************************************
 * exec_dat.cpp is part of Math Graphic Library
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
int static mgls_addto(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dd"))		*d += *(a[1].d);
	else if(d && !strcmp(k,"dn"))	*d += a[1].v;
	else if(c && !strcmp(k,"dd"))	*c += *(a[1].d);
	else if(c && !strcmp(k,"dn"))	*c += a[1].c;
	else	res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_apde(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	if(k[1]=='d' && a[1].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d), *f = dynamic_cast<mglData *>(a[1].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && f)
	{
		mglDataC r;
		if(!strcmp(k,"ddsdd"))
			r = gr->APDEc(a[2].s.s, *(a[3].d), *(a[4].d), 0.1,100,opt);
		else if(!strcmp(k,"ddsddn"))
			r = gr->APDEc(a[2].s.s, *(a[3].d), *(a[4].d), a[5].v,100,opt);
		else if(!strcmp(k,"ddsddnn"))
			r = gr->APDEc(a[2].s.s, *(a[3].d), *(a[4].d), a[5].v,a[6].v,opt);
		else res = 1;
		if(res==0)	{	*d = r.Abs();	*f = r.Arg();	}
	}
	else if(d)
	{
		if(!strcmp(k,"dsdd"))
			*d = gr->APDE(a[1].s.s, *(a[2].d), *(a[3].d), 0.1,100,opt);
		else if(!strcmp(k,"dsddn"))
			*d = gr->APDE(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,100,opt);
		else if(!strcmp(k,"dsddnn"))
			*d = gr->APDE(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,a[5].v,opt);
		else res = 1;
	}
	else if(c)
	{
		if(!strcmp(k,"dsdd"))
			*c = gr->APDEc(a[1].s.s, *(a[2].d), *(a[3].d), 0.1,100,opt);
		else if(!strcmp(k,"dsddn"))
			*c = gr->APDEc(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,100,opt);
		else if(!strcmp(k,"dsddnn"))
			*c = gr->APDEc(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,a[5].v,opt);
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_clean(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dn"))	d->Clean(mgl_int(a[1].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_coil(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dnn"))		d->Coil(a[1].v, a[2].v);
	else if(d && !strcmp(k,"dnnn"))	d->Coil(a[1].v, a[2].v, a[3].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_column(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dds"))	*d = mglData(true,mgl_data_column(a[1].d,a[2].s.s));
	else if(c && !strcmp(k,"dds"))	*c = mglDataC(true,mgl_datac_column(a[1].d,a[2].s.s));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_combine(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ddd"))	*d = mglData(true,mgl_data_combine(a[1].d, a[2].d));
	else if(c && !strcmp(k,"ddd"))	*c = mglDataC(true,mgl_datac_combine(a[1].d, a[2].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_conts(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dnd"))	*d = mglData(true,mgl_data_conts(a[1].v,a[2].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_copy(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(c && !strcmp(k,"dd"))	c->Set(a[1].d);
	else if(!d)	return 1;
	else if(!strcmp(k,"dd"))	d->Set(a[1].d);
	else if(!strcmp(k,"dds"))
	{	d->Set(a[1].d);	gr->Fill(*d, a[2].s.s);	}
	else if(!strcmp(k,"ddd"))
	{
		mglData *D = dynamic_cast<mglData *>(a[1].d);
		mglDataC *C = dynamic_cast<mglDataC *>(a[2].d);
		if(D && C)	{	d->Set(C->Real());	D->Set(C->Imag());	}
		else	res = 1;
	}
	else if(!strcmp(k,"dn"))	*d = a[1].v;
	else if(!strcmp(k,"ds") && gr->pr)
		d->Set(mgl_parser_find_var(gr->pr, a[1].s.s));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_correl(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dds"))	*d = mglData(true,mgl_data_correl(a[1].d, a[1].d, a[2].s.s));
	else if(d && !strcmp(k,"ddds"))	*d = mglData(true,mgl_data_correl(a[1].d, a[2].d, a[3].s.s));
	else if(c && !strcmp(k,"dds"))	*c = mglDataC(true,mgl_datac_correl(a[1].d, a[1].d, a[2].s.s));
	else if(c && !strcmp(k,"ddds"))	*c = mglDataC(true,mgl_datac_correl(a[1].d, a[2].d, a[3].s.s));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cosfft(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->CosFFT(a[1].s.s);
	else if(c && !strcmp(k,"ds"))	d->CosFFT(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_crop(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dnns"))	d->Crop(mgl_int(a[1].v),mgl_int(a[2].v),a[3].s.s[0]);
	else if(d && !strcmp(k,"ds"))	d->Crop(a[1].s.s);
	else if(c && !strcmp(k,"dnns"))	c->Crop(mgl_int(a[1].v),mgl_int(a[2].v),a[3].s.s[0]);
	else if(c && !strcmp(k,"ds"))	c->Crop(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_cumsum(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->CumSum(a[1].s.s);
	else if(c && !strcmp(k,"ds"))	c->CumSum(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_datagrid(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!strcmp(k,"dddd") && d)	gr->DataGrid(*d, *(a[1].d), *(a[2].d), *(a[3].d),opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_datas(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s"))
	{
		char *buf=new char[1024];
		long n=mgl_datas_hdf(a[0].s.s,buf,1024);
		if(n<0)
		{
			delete []buf;	buf=new char[-n];
			mgl_datas_hdf(a[0].s.s,buf,-n);
		}
		gr->SetWarn(-1,buf);
		delete []buf;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_delete(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if((!strcmp(k,"d") || !strcmp(k,"s")) && gr->pr)
		mgl_parser_del_var(gr->pr, a[0].s.s);
	else if(d && !strcmp(k,"ds"))	d->Delete(a[1].s.s[0]);
	else if(d && !strcmp(k,"dsn"))	d->Delete(a[1].s.s[0], mgl_int(a[2].v));
	else if(d && !strcmp(k,"dsnn"))	d->Delete(a[1].s.s[0], mgl_int(a[2].v), mgl_int(a[3].v));
	else if(c && !strcmp(k,"ds"))	c->Delete(a[1].s.s[0]);
	else if(c && !strcmp(k,"dsn"))	c->Delete(a[1].s.s[0], mgl_int(a[2].v));
	else if(c && !strcmp(k,"dsnn"))	c->Delete(a[1].s.s[0], mgl_int(a[2].v), mgl_int(a[3].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_detect(mglGraph *, long, mglArg *a, const char *k, const char *)
{
	mglData *r = dynamic_cast<mglData*>(a[0].d);
	int res = 0;
	if(r && !strcmp(k, "ddnn"))	r->Set(mglDetect(*(a[1].d), a[2].v, a[3].v));
	else if(r && !strcmp(k, "ddnnn"))	r->Set(mglDetect(*(a[1].d), a[2].v, a[3].v, a[4].v));
	else if(r && !strcmp(k, "ddnnnn"))	r->Set(mglDetect(*(a[1].d), a[2].v, a[3].v, a[4].v, a[5].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_diff(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->Diff(a[1].s.s);
	else if(d && !strcmp(k,"ddd"))	d->Diff(*(a[1].d), *(a[2].d));
	else if(d && !strcmp(k,"dddd"))	d->Diff(*(a[1].d), *(a[2].d), *(a[3].d));
	else if(c && !strcmp(k,"ds"))	c->Diff(a[1].s.s);
//	else if(c && !strcmp(k,"ddd"))	c->Diff(*(a[1].d), *(a[2].d));	// TODO Add later
//	else if(c && !strcmp(k,"dddd"))	c->Diff(*(a[1].d), *(a[2].d), *(a[3].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_diff2(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->Diff2(a[1].s.s);
	else if(c && !strcmp(k,"ds"))	c->Diff2(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_diffract(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(c && !strcmp(k,"dsn"))	c->Diffraction(a[1].s.s, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_dilate(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"d"))	d->Dilate();
	else if(d && !strcmp(k,"dn"))	d->Dilate(a[1].v);
	else if(d && !strcmp(k,"dnn"))	d->Dilate(a[1].v, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_divto(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dd"))		*d /= *(a[1].d);
	else if(d && !strcmp(k,"dn"))	*d /= a[1].v;
	else if(c && !strcmp(k,"dd"))	*c /= *(a[1].d);
	else if(c && !strcmp(k,"dn"))	*c /= a[1].c;
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_echo(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->SetWarn(-1,a[0].d->Get().c_str());
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_envelop(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"d"))	d->Envelop();
	else if(d && !strcmp(k,"ds"))	d->Envelop(a[1].s.s[0]);
	else if(c && !strcmp(k,"d"))	c->Envelop();
	else if(c && !strcmp(k,"ds"))	c->Envelop(a[1].s.s[0]);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_erode(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"d"))	d->Erode();
	else if(d && !strcmp(k,"dn"))	d->Erode(a[1].v);
	else if(d && !strcmp(k,"dnn"))	d->Erode(a[1].v, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_evaluate(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && k[0]=='d' && k[1]=='d' && k[2]=='d')
	{
		if(k[3]==0)	*d = mglData(true,mgl_data_evaluate(a[1].d,a[2].d,0,0,true));
		else if(!strcmp(k+3,"n"))	*d = mglData(true,mgl_data_evaluate(a[1].d,a[2].d,0,0, a[3].v!=0));
		else if(!strcmp(k+3,"d"))	*d = mglData(true,mgl_data_evaluate(a[1].d,a[2].d,a[3].d,0,true));
		else if(!strcmp(k+3,"dn"))	*d = mglData(true,mgl_data_evaluate(a[1].d,a[2].d,a[3].d,0, a[4].v!=0));
		else if(!strcmp(k+3,"dd"))	*d = mglData(true,mgl_data_evaluate(a[1].d,a[2].d,a[3].d,a[4].d,true));
		else if(!strcmp(k+3,"ddn"))	*d = mglData(true,mgl_data_evaluate(a[1].d,a[2].d,a[3].d,a[4].d, a[5].v!=0));
		else res = 1;
	}
	else if(c && k[0]=='d' && k[1]=='d' && k[2]=='d')
	{
		if(k[3]==0)	*c = mglDataC(true,mgl_datac_evaluate(a[1].d,a[2].d,0,0,true));
		else if(!strcmp(k+3,"n"))	*c = mglDataC(true,mgl_datac_evaluate(a[1].d,a[2].d,0,0, a[3].v!=0));
		else if(!strcmp(k+3,"d"))	*c = mglDataC(true,mgl_datac_evaluate(a[1].d,a[2].d,a[3].d,0,true));
		else if(!strcmp(k+3,"dn"))	*c = mglDataC(true,mgl_datac_evaluate(a[1].d,a[2].d,a[3].d,0, a[4].v!=0));
		else if(!strcmp(k+3,"dd"))	*c = mglDataC(true,mgl_datac_evaluate(a[1].d,a[2].d,a[3].d,a[4].d,true));
		else if(!strcmp(k+3,"ddn"))	*c = mglDataC(true,mgl_datac_evaluate(a[1].d,a[2].d,a[3].d,a[4].d, a[5].v!=0));
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_export(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"dss"))	a[0].d->Export(a[1].s.s, a[2].s.s);
	else if(!strcmp(k,"dssnn"))	a[0].d->Export(a[1].s.s, a[2].s.s, a[3].v,a[4].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_extend(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dn"))	d->Extend(mgl_int(a[1].v));
	else if(d && !strcmp(k,"dnn"))	d->Extend(mgl_int(a[1].v),mgl_int(a[2].v));
	else if(c && !strcmp(k,"dn"))	c->Extend(mgl_int(a[1].v));
	else if(c && !strcmp(k,"dnn"))	c->Extend(mgl_int(a[1].v),mgl_int(a[2].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_minmax(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dd"))	*d = mglData(true,mgl_data_minmax(a[1].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_connect(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
//	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d1 = dynamic_cast<mglData *>(a[0].d);
	mglData *d2 = dynamic_cast<mglData *>(a[1].d);
	if(d1 && !strcmp(k,"ddd"))	*d1 = mglData(true,mgl_data_connect(a[1].d,a[2].d));
	else if(d1 && d2 && !strcmp(k,"dd"))	mgl_data_connect_r(d1,d2);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fill(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && k[0]=='d')
	{
		if(!strcmp(k,"dnn"))	d->Fill(a[1].v,a[2].v);
		else if(!strcmp(k,"dnns"))	d->Fill(a[1].v,a[2].v,a[3].s.s[0]);
		else if(!strcmp(k,"ds"))	gr->Fill(*d,a[1].s.s,opt);
		else if(!strcmp(k,"dsd"))	gr->Fill(*d,a[1].s.s, *(a[2].d),opt);
		else if(!strcmp(k,"dsdd"))	gr->Fill(*d,a[1].s.s, *(a[2].d), *(a[3].d),opt);
		else res = 1;
	}
	else if(c && k[0]=='d')
	{
		if(!strcmp(k,"dnn"))	c->Fill(a[1].v,a[2].v);
		else if(!strcmp(k,"dnns"))	c->Fill(a[1].v,a[2].v,a[3].s.s[0]);
		else if(!strcmp(k,"ds"))	gr->Fill(*c,a[1].s.s,opt);
		else if(!strcmp(k,"dsd"))	gr->Fill(*c,a[1].s.s, *(a[2].d),opt);
		else if(!strcmp(k,"dsdd"))	gr->Fill(*c,a[1].s.s, *(a[2].d), *(a[3].d),opt);
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fillsample(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->FillSample(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fit(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"dddddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[7].d);
		if(i)	*d = gr->Fit(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s, a[6].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"dddddss"))
		*d = gr->Fit(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s, a[6].s.s,opt);
	else if(!strcmp(k,"ddddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[6].d);
		if(i)	*d = gr->Fit(*(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s, a[5].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"ddddss"))
		*d = gr->Fit(*(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s, a[5].s.s,opt);
	else if(!strcmp(k,"dddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[5].d);
		if(i)	*d = gr->Fit(*(a[1].d), *(a[2].d), a[3].s.s, a[4].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"dddss"))
		*d = gr->Fit(*(a[1].d), *(a[2].d), a[3].s.s, a[4].s.s,opt);
	else if(!strcmp(k,"ddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[4].d);
		if(i)	*d = gr->Fit(*(a[1].d), a[2].s.s, a[3].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"ddss"))
		*d = gr->Fit(*(a[1].d), a[2].s.s, a[3].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fits(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"ddddddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[8].d);
		if(i)	*d = gr->FitS(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), *(a[5].d), a[6].s.s, a[7].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"ddddddss"))
		*d = gr->FitS(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), *(a[5].d), a[6].s.s, a[7].s.s,opt);
	else if(!strcmp(k,"dddddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[7].d);
		if(i)	*d = gr->FitS(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s, a[6].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"dddddss"))
		*d = gr->FitS(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s, a[6].s.s,opt);
	else if(!strcmp(k,"ddddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[6].d);
		if(i)	*d = gr->FitS(*(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s, a[5].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"ddddss"))
		*d = gr->FitS(*(a[1].d), *(a[2].d), *(a[3].d), a[4].s.s, a[5].s.s,opt);
	else if(!strcmp(k,"dddssd"))
	{
		mglData *i = dynamic_cast<mglData *>(a[5].d);
		if(i)	*d = gr->FitS(*(a[1].d), *(a[2].d), a[3].s.s, a[4].s.s, *i,opt);
		else	res = 1;
	}
	else if(!strcmp(k,"dddss"))
		*d = gr->FitS(*(a[1].d), *(a[2].d), a[3].s.s, a[4].s.s,opt);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_fourier(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *re = dynamic_cast<mglData *>(a[0].d), *im = dynamic_cast<mglData *>(a[1].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(!strcmp(k,"dds") && re && im)	mglFourier(*re,*im,a[2].s.s);
	else if(!strcmp(k,"ds") && c)		c->FFT(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_gspline(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ddd"))	d->RefillGS(*(a[1].d),*(a[2].d),gr->Self()->Min.x,gr->Self()->Max.x,-1);
	else if(d && !strcmp(k,"dddn"))	d->RefillGS(*(a[1].d),*(a[2].d),gr->Self()->Min.x,gr->Self()->Max.x,mgl_int(a[3].v));
	else if(c && !strcmp(k,"ddd"))	c->RefillGS(*(a[1].d),*(a[2].d),gr->Self()->Min.x,gr->Self()->Max.x,-1);
	else if(c && !strcmp(k,"dddn"))	c->RefillGS(*(a[1].d),*(a[2].d),gr->Self()->Min.x,gr->Self()->Max.x,mgl_int(a[3].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_hankel(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))		d->Hankel(a[1].s.s);
	else if(c && !strcmp(k,"ds"))	c->Hankel(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_hist(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"ddd"))		*d = gr->Hist(*(a[1].d), *(a[2].d),opt);
	else if(!strcmp(k,"dddd"))	*d = gr->Hist(*(a[1].d), *(a[2].d), *(a[3].d),opt);
	else if(!strcmp(k,"ddddd"))	*d = gr->Hist(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d),opt);
	else if(!strcmp(k,"ddnnn"))	*d = mglData(true,mgl_data_hist(a[1].d,int(a[2].v+0.5), a[3].v, a[4].v, 0));
	else if(!strcmp(k,"ddnnnn"))	*d = mglData(true,mgl_data_hist(a[1].d,mgl_int(a[2].v), a[3].v, a[4].v, mgl_int(a[5].v)));
	else if(!strcmp(k,"dddnnn"))	*d = mglData(true,mgl_data_hist_w(a[1].d,a[2].d, mgl_int(a[3].v), a[4].v, a[5].v, 0));
	else if(!strcmp(k,"dddnnnn"))	*d = mglData(true,mgl_data_hist_w(a[1].d,a[2].d, mgl_int(a[3].v), a[4].v, a[5].v, mgl_int(a[6].v)));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_idset(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	if(!strcmp(k,"ds"))	a[0].d->SetColumnId(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_import(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"dss"))	d->Import(a[1].s.s, a[2].s.s);
	else if(!strcmp(k,"dssnn"))	d->Import(a[1].s.s, a[2].s.s, a[3].v,a[4].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_info(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"d"))	gr->SetWarn(-1,a[0].d->PrintInfo());
	else if(!strcmp(k,"s"))	gr->SetWarn(-1,a[0].s.s);
	else if(!strcmp(k,"n"))	gr->SetWarn(-1,("value = "+mgl_str_num(a[0].v)).c_str());
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_insert(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->Insert(a[1].s.s[0]);
	else if(d && !strcmp(k,"dsn"))	d->Insert(a[1].s.s[0], mgl_int(a[2].v));
	else if(d && !strcmp(k,"dsnn"))	d->Insert(a[1].s.s[0], mgl_int(a[2].v), mgl_int(a[3].v));
	else if(c && !strcmp(k,"ds"))	c->Insert(a[1].s.s[0]);
	else if(c && !strcmp(k,"dsn"))	c->Insert(a[1].s.s[0], mgl_int(a[2].v));
	else if(c && !strcmp(k,"dsnn"))	c->Insert(a[1].s.s[0], mgl_int(a[2].v), mgl_int(a[3].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_integrate(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->Integral(a[1].s.s);
	else if(c && !strcmp(k,"ds"))	c->Integral(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_jacobian(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"ddd"))	*d = mglJacobian(*(a[1].d), *(a[2].d));
	else if(!strcmp(k,"dddd"))	*d = mglJacobian(*(a[1].d), *(a[2].d), *(a[3].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_join(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if((!d && !c) || k[1]!='d')	res = 1;
	else if(d)	for(long i=1;k[i]=='d';i++)	d->Join(*(a[i].d));
	else if(c)	for(long i=1;k[i]=='d';i++)	c->Join(*(a[i].d));
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_limit(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dn"))		d->Limit(a[1].v);
	else if(c && !strcmp(k,"dn"))	c->Limit(a[1].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_max(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dds"))	*d = mglData(true,mgl_data_max_dir(a[1].d,a[2].s.s));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_min(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dds"))	*d = mglData(true,mgl_data_min_dir(a[1].d,a[2].s.s));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_mirror(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))		d->Mirror(a[1].s.s);
	else if(c && !strcmp(k,"ds"))	c->Mirror(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_modify(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->Modify(a[1].s.s);
	else if(d && !strcmp(k,"dsn"))	d->Modify(a[1].s.s, mgl_int(a[2].v));
	else if(d && !strcmp(k,"dsd"))	d->Modify(a[1].s.s,*(a[2].d));
	else if(d && !strcmp(k,"dsdd"))	d->Modify(a[1].s.s,*(a[2].d),*(a[3].d));
	else if(c && !strcmp(k,"ds"))	c->Modify(a[1].s.s);
	else if(c && !strcmp(k,"dsn"))	c->Modify(a[1].s.s, mgl_int(a[2].v));
	else if(c && !strcmp(k,"dsd"))	c->Modify(a[1].s.s,*(a[2].d));
	else if(c && !strcmp(k,"dsdd"))	c->Modify(a[1].s.s,*(a[2].d),*(a[3].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_momentum(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dds"))	*d = mglData(true,mgl_data_momentum(a[1].d,'z', a[2].s.s));
	else if(d && !strcmp(k,"ddss"))	*d = mglData(true,mgl_data_momentum(a[1].d,a[3].s.s[0], a[2].s.s));
	else if(c && !strcmp(k,"dds"))	*c = mglDataC(true,mgl_datac_momentum(a[1].d,'z', a[2].s.s));
	else if(c && !strcmp(k,"ddss"))	*c = mglDataC(true,mgl_datac_momentum(a[1].d,a[3].s.s[0], a[2].s.s));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_multo(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dd"))		*d *= *(a[1].d);
	else if(d && !strcmp(k,"dn"))	*d *= a[1].v;
	else if(c && !strcmp(k,"dd"))	*c *= *(a[1].d);
	else if(c && !strcmp(k,"dn"))	*c *= a[1].c;
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_new(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dn"))	d->Create(mgl_int(a[1].v));
	else if(d && !strcmp(k,"dns"))
	{	d->Create(mgl_int(a[1].v));	d->Fill(gr->Self(),a[2].s.s,opt);	}
	else if(d && !strcmp(k,"dnn"))	d->Create(mgl_int(a[1].v),mgl_int(a[2].v));
	else if(d && !strcmp(k,"dnns"))
	{	d->Create(mgl_int(a[1].v),mgl_int(a[2].v));	d->Fill(gr->Self(),a[3].s.s,opt);	}
	else if(d && !strcmp(k,"dnnn"))	d->Create(mgl_int(a[1].v),mgl_int(a[2].v),mgl_int(a[3].v));
	else if(d && !strcmp(k,"dnnns"))
	{	d->Create(mgl_int(a[1].v),mgl_int(a[2].v),mgl_int(a[3].v));	d->Fill(gr->Self(),a[4].s.s,opt);	}
	else if(c && !strcmp(k,"dn"))	c->Create(mgl_int(a[1].v));
	else if(c && !strcmp(k,"dns"))
	{	c->Create(mgl_int(a[1].v));	c->Fill(gr->Self(),a[2].s.s,opt);	}
	else if(c && !strcmp(k,"dnn"))	c->Create(mgl_int(a[1].v),mgl_int(a[2].v));
	else if(c && !strcmp(k,"dnns"))
	{	c->Create(mgl_int(a[1].v),mgl_int(a[2].v));	c->Fill(gr->Self(),a[3].s.s,opt);	}
	else if(c && !strcmp(k,"dnnn"))	c->Create(mgl_int(a[1].v),mgl_int(a[2].v),mgl_int(a[3].v));
	else if(c && !strcmp(k,"dnnns"))
	{	c->Create(mgl_int(a[1].v),mgl_int(a[2].v),mgl_int(a[3].v));	c->Fill(gr->Self(),a[4].s.s,opt);	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_norm(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"dnn"))	d->Norm(a[1].v,a[2].v);
	else if(!strcmp(k,"dnnn"))	d->Norm(a[1].v,a[2].v,a[3].v!=0);
	else if(!strcmp(k,"dnnnn"))	d->Norm(a[1].v,a[2].v,a[3].v!=0,mgl_int(a[4].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_normsl(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"dnn"))	d->NormSl(a[1].v, a[2].v);
	else if(!strcmp(k,"dnns"))	d->NormSl(a[1].v, a[2].v, a[3].s.s[0]);
	else if(!strcmp(k,"dnnsn"))	d->NormSl(a[1].v, a[2].v, a[3].s.s[0],a[4].v);
	else if(!strcmp(k,"dnnsnn"))d->NormSl(a[1].v, a[2].v, a[3].s.s[0],a[4].v,a[5].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ode(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d)
	{
		if(!strcmp(k,"dssd"))
			*d = mglODE(a[1].s.s, a[2].s.s, *(a[3].d));
		else if(!strcmp(k,"dssdnn"))
			*d = mglODE(a[1].s.s, a[2].s.s, *(a[3].d), a[4].v, a[5].v);
	}
	else if(c)
	{
		if(!strcmp(k,"dssd"))
			*c = mglODEc(a[1].s.s, a[2].s.s, *(a[3].d));
		else if(!strcmp(k,"dssdnn"))
			*c = mglODEc(a[1].s.s, a[2].s.s, *(a[3].d), a[4].v, a[5].v);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_openhdf(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"s") && gr->pr)	mgl_parser_openhdf(gr->pr, a[0].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_pde(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	if(k[1]=='d' && a[1].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d), *f = dynamic_cast<mglData *>(a[1].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && f)
	{
		mglDataC r;
		if(!strcmp(k,"ddsdd"))
			r = gr->PDEc(a[2].s.s, *(a[3].d), *(a[4].d), 0.1,100,opt);
		else if(!strcmp(k,"ddsddn"))
			r = gr->PDEc(a[2].s.s, *(a[3].d), *(a[4].d), a[5].v,100,opt);
		else if(!strcmp(k,"ddsddnn"))
			r = gr->PDEc(a[2].s.s, *(a[3].d), *(a[4].d), a[5].v,a[6].v,opt);
		else res = 1;
		if(res==0)	{	*d = r.Abs();	*f = r.Arg();	}
	}
	else if(d)
	{
		if(!strcmp(k,"dsdd"))
			*d = gr->PDE(a[1].s.s, *(a[2].d), *(a[3].d), 0.1,100,opt);
		else if(!strcmp(k,"dsddn"))
			*d = gr->PDE(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,100,opt);
		else if(!strcmp(k,"dsddnn"))
			*d = gr->PDE(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,a[5].v,opt);
		else res = 1;
	}
	else if(c)
	{
		if(!strcmp(k,"dsdd"))
			*c = gr->PDEc(a[1].s.s, *(a[2].d), *(a[3].d), 0.1,100,opt);
		else if(!strcmp(k,"dsddn"))
			*c = gr->PDEc(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,100,opt);
		else if(!strcmp(k,"dsddnn"))
			*c = gr->PDEc(a[1].s.s, *(a[2].d), *(a[3].d), a[4].v,a[5].v,opt);
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_print(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"d"))	printf("%s\n",a[0].d->PrintInfo());
	else if(!strcmp(k,"s"))	printf("%s\n",a[0].s.s);
	else if(!strcmp(k,"n"))	printf("value = %g\n",a[0].v);
	else res = 1;
	fflush(stdout);	return res;
}
//-----------------------------------------------------------------------------
int static mgls_progress(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nn") && mgl_progress_func)
		mgl_progress_func(mgl_int(a[0].v), mgl_int(a[1].v), gr->Self());
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_pulse(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dds"))	*d = mglData(true,mgl_data_pulse(a[1].d,a[2].s[0]));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_put(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d)
	{
		if(!strcmp(k,"dn"))	d->Put(a[1].v);
		else if(!strcmp(k,"dnn"))	d->Put(a[1].v, mgl_int(a[2].v));
		else if(!strcmp(k,"dnnn"))	d->Put(a[1].v, mgl_int(a[2].v),mgl_int(a[3].v));
		else if(!strcmp(k,"dnnnn"))	d->Put(a[1].v, mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v));
		else if(!strcmp(k,"dd"))	d->Put(*(a[1].d));
		else if(!strcmp(k,"ddn"))	d->Put(*(a[1].d), mgl_int(a[2].v));
		else if(!strcmp(k,"ddnn"))	d->Put(*(a[1].d), mgl_int(a[2].v),mgl_int(a[3].v));
		else if(!strcmp(k,"ddnnn"))	d->Put(*(a[1].d), mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v));
		else res = 1;
	}
	else if(c)
	{
		if(!strcmp(k,"dn"))	c->Put(a[1].c);
		else if(!strcmp(k,"dnn"))	c->Put(a[1].c, mgl_int(a[2].v));
		else if(!strcmp(k,"dnnn"))	c->Put(a[1].c, mgl_int(a[2].v),mgl_int(a[3].v));
		else if(!strcmp(k,"dnnnn"))	c->Put(a[1].c, mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v));
		else if(!strcmp(k,"dd"))	c->Put(*(a[1].d));
		else if(!strcmp(k,"ddn"))	c->Put(*(a[1].d), mgl_int(a[2].v));
		else if(!strcmp(k,"ddnn"))	c->Put(*(a[1].d), mgl_int(a[2].v),mgl_int(a[3].v));
		else if(!strcmp(k,"ddnnn"))	c->Put(*(a[1].d), mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v));
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_putsfit(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;	gr->Self()->SaveState(opt);
	if(!strcmp(k,"nn"))		gr->PutsFit(mglPoint(a[0].v,a[1].v));
	else if(!strcmp(k,"nns"))	gr->PutsFit(mglPoint(a[0].v,a[1].v), a[2].s.s);
	else if(!strcmp(k,"nnss"))	gr->PutsFit(mglPoint(a[0].v,a[1].v), a[2].s.s,a[3].s.s);
	else if(!strcmp(k,"nnssn"))	gr->PutsFit(mglPoint(a[0].v,a[1].v), a[2].s.s,a[3].s.s,a[4].v);
	else if(!strcmp(k,"nnn"))	gr->PutsFit(mglPoint(a[0].v,a[1].v,a[2].v));
	else if(!strcmp(k,"nnns"))	gr->PutsFit(mglPoint(a[0].v,a[1].v,a[2].v), a[3].s.s);
	else if(!strcmp(k,"nnnss"))	gr->PutsFit(mglPoint(a[0].v,a[1].v,a[2].v), a[3].s.s,a[4].s.s);
	else if(!strcmp(k,"nnnssn"))gr->PutsFit(mglPoint(a[0].v,a[1].v,a[2].v), a[3].s.s,a[4].s.s,a[5].v);
	else res = 1;
	gr->Self()->LoadState();	return res;
}
//-----------------------------------------------------------------------------
int static mgls_qo2d(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	if(k[1]=='d' && a[1].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d), *f = dynamic_cast<mglData *>(a[1].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && f)
	{
		mglDataC r;
		if(!strcmp(k,"ddsddd"))
			r = mglDataC(true, mgl_qo2d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, 1,100, 0,0));
		else if(!strcmp(k,"ddsdddn"))
			r = mglDataC(true, mgl_qo2d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, a[6].v,100, 0,0));
		else if(!strcmp(k,"ddsdddnn"))
			r = mglDataC(true, mgl_qo2d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, a[6].v,a[7].v, 0,0));
		else if(!strcmp(k,"ddsdddnndd"))
			r = mglDataC(true, mgl_qo2d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, a[6].v,a[7].v, dynamic_cast<mglData *>(a[8].d),dynamic_cast<mglData *>(a[9].d)));
		else res = 1;
		if(res==0)	{	*d = r.Abs();	*f = r.Arg();	}
	}
	else if(d)
	{
		if(!strcmp(k,"dsddd"))
			*d = mglData(true, mgl_qo2d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, 1,100, 0,0));
		else if(!strcmp(k,"dsdddn"))
			*d = mglData(true, mgl_qo2d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,100, 0,0));
		else if(!strcmp(k,"dsdddnn"))
			*d = mglData(true, mgl_qo2d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, 0,0));
		else if(!strcmp(k,"dsdddnndd"))
			*d = mglData(true, mgl_qo2d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, dynamic_cast<mglData *>(a[7].d),dynamic_cast<mglData *>(a[8].d)));
		else res = 1;
	}
	else if(c)
	{
		if(!strcmp(k,"dsddd"))
			*c = mglDataC(true, mgl_qo2d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, 1,100, 0,0));
		else if(!strcmp(k,"dsdddn"))
			*c = mglDataC(true, mgl_qo2d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,100, 0,0));
		else if(!strcmp(k,"dsdddnn"))
			*c = mglDataC(true, mgl_qo2d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, 0,0));
		else if(!strcmp(k,"dsdddnndd"))
			*c = mglDataC(true, mgl_qo2d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, dynamic_cast<mglData *>(a[7].d),dynamic_cast<mglData *>(a[8].d)));
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_qo3d(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	if(k[1]=='d' && a[1].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d), *f = dynamic_cast<mglData *>(a[1].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && f)
	{
		mglDataC r;
		if(!strcmp(k,"ddsddd"))
			r = mglDataC(true, mgl_qo3d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, 1,100, 0,0,0));
		else if(!strcmp(k,"ddsdddn"))
			r = mglDataC(true, mgl_qo3d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, a[6].v,100, 0,0,0));
		else if(!strcmp(k,"ddsdddnn"))
			r = mglDataC(true, mgl_qo3d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, a[6].v,a[7].v, 0,0,0));
		else if(!strcmp(k,"ddsdddnnddd"))
			r = mglDataC(true, mgl_qo3d_solve_c(a[2].s.s, a[3].d, a[4].d, a[5].d, a[6].v,a[7].v, dynamic_cast<mglData *>(a[8].d),dynamic_cast<mglData *>(a[9].d),dynamic_cast<mglData *>(a[10].d)));
		else res = 1;
		if(res==0)	{	*d = r.Abs();	*f = r.Arg();	}
	}
	else if(d)
	{
		if(!strcmp(k,"dsddd"))
			*d = mglData(true, mgl_qo3d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, 1,100, 0,0,0));
		else if(!strcmp(k,"dsdddn"))
			*d = mglData(true, mgl_qo3d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,100, 0,0,0));
		else if(!strcmp(k,"dsdddnn"))
			*d = mglData(true, mgl_qo3d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, 0,0,0));
		else if(!strcmp(k,"dsdddnnddd"))
			*d = mglData(true, mgl_qo3d_solve(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, dynamic_cast<mglData *>(a[7].d),dynamic_cast<mglData *>(a[8].d),dynamic_cast<mglData *>(a[9].d)));
		else res = 1;
	}
	else if(c)
	{
		if(!strcmp(k,"dsddd"))
			*c = mglDataC(true, mgl_qo3d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, 1,100, 0,0,0));
		else if(!strcmp(k,"dsdddn"))
			*c = mglDataC(true, mgl_qo3d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,100, 0,0,0));
		else if(!strcmp(k,"dsdddnn"))
			*c = mglDataC(true, mgl_qo3d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, 0,0,0));
		else if(!strcmp(k,"dsdddnnddd"))
			*c = mglDataC(true, mgl_qo3d_solve_c(a[1].s.s, a[2].d, a[3].d, a[4].d, a[5].v,a[6].v, dynamic_cast<mglData *>(a[7].d),dynamic_cast<mglData *>(a[8].d),dynamic_cast<mglData *>(a[9].d)));
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_ray(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"dsnnnn"))
		*d = mglRay(a[1].s.s, mglPoint(a[2].v, a[3].v), mglPoint(a[4].v, a[5].v));
	else if(!strcmp(k,"dsnnnnnn"))
		*d = mglRay(a[1].s.s, mglPoint(a[2].v, a[3].v, a[4].v), mglPoint(a[5].v, a[6].v, a[7].v));
	else if(!strcmp(k,"dsnnnnnnn"))
		*d = mglRay(a[1].s.s, mglPoint(a[2].v, a[3].v, a[4].v), mglPoint(a[5].v, a[6].v, a[7].v), a[8].v);
	else if(!strcmp(k,"dsnnnnnnnn"))
		*d = mglRay(a[1].s.s, mglPoint(a[2].v, a[3].v, a[4].v), mglPoint(a[5].v, a[6].v, a[7].v), a[8].v,a[9].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_read(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	bool rr=true;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	if(k[1]=='d' && a[1].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglData *f = dynamic_cast<mglData *>(a[1].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(c)
	{
		if(!strcmp(k,"ds"))			rr=c->Read(a[1].s.s);
		else if(!strcmp(k,"dsn"))	rr=c->Read(a[1].s.s, mgl_int(a[2].v));
		else if(!strcmp(k,"dsnn"))	rr=c->Read(a[1].s.s, mgl_int(a[2].v),mgl_int(a[3].v));
		else if(!strcmp(k,"dsnnn"))	rr=c->Read(a[1].s.s, mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v));
		else res = 1;
	}
	else if(d && f && k[0]=='d' && k[1]=='d' && k[2]=='s')
	{
		mglDataC r;
		if(k[3]==0)	rr=r.Read(a[2].s.s);
		else if(!strcmp(k+3,"n"))	rr=r.Read(a[2].s.s, mgl_int(a[3].v));
		else if(!strcmp(k+3,"nn"))	rr=r.Read(a[2].s.s, mgl_int(a[3].v),mgl_int(a[4].v));
		else if(!strcmp(k+3,"nnn"))	rr=r.Read(a[2].s.s, mgl_int(a[3].v),mgl_int(a[4].v),mgl_int(a[5].v));
		else res = 1;
		if(res==0)	{	*d = r.Real();	*f = r.Imag();	}
	}
	else if(d)
	{
		if(!strcmp(k,"ds"))	rr=d->Read(a[1].s.s);
		else if(!strcmp(k,"dsn"))	rr=d->Read(a[1].s.s, mgl_int(a[2].v));
		else if(!strcmp(k,"dsnn"))	rr=d->Read(a[1].s.s, mgl_int(a[2].v),mgl_int(a[3].v));
		else if(!strcmp(k,"dsnnn"))	rr=d->Read(a[1].s.s, mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v));
		else res = 1;
	}
	else res = 1;
	if(!rr)	gr->SetWarn(mglWarnFile,"Read");
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_readall(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	bool rr=true;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	rr=d->ReadAll(a[1].s.s);
	else if(d && !strcmp(k,"dsn"))		rr=d->ReadAll(a[1].s.s, a[2].v);
	else if(d && !strcmp(k,"dsnn"))		rr=d->ReadRange(a[1].s.s, a[2].v, a[3].v);
	else if(d && !strcmp(k,"dsnnn"))	rr=d->ReadRange(a[1].s.s, a[2].v, a[3].v, a[4].v);
	else if(d && !strcmp(k,"dsnnnn"))	rr=d->ReadRange(a[1].s.s, a[2].v, a[3].v, a[4].v, a[5].v);
	else if(c && !strcmp(k,"ds"))		rr=c->ReadAll(a[1].s.s);
	else if(c && !strcmp(k,"dsn"))		rr=c->ReadAll(a[1].s.s, a[2].v);
	else if(c && !strcmp(k,"dsnn"))		rr=c->ReadRange(a[1].s.s, a[2].v, a[3].v);
	else if(c && !strcmp(k,"dsnnn"))	rr=c->ReadRange(a[1].s.s, a[2].v, a[3].v, a[4].v);
	else if(c && !strcmp(k,"dsnnnn"))	rr=c->ReadRange(a[1].s.s, a[2].v, a[3].v, a[4].v, a[5].v);
	else res = 1;
	if(!rr)	gr->SetWarn(mglWarnFile,"ReadAll");
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_readhdf(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dss"))	d->ReadHDF(a[1].s.s, a[2].s.s);
	else if(c && !strcmp(k,"dss"))	c->ReadHDF(a[1].s.s, a[2].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_readmat(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	bool rr=true;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))		rr=d->ReadMat(a[1].s.s);
	else if(d && !strcmp(k,"dsn"))	rr=d->ReadMat(a[1].s.s, mgl_int(a[2].v));
	else if(c && !strcmp(k,"ds"))	rr=c->ReadMat(a[1].s.s);
	else if(c && !strcmp(k,"dsn"))	rr=c->ReadMat(a[1].s.s, mgl_int(a[2].v));
	else res = 1;
	if(!rr)	gr->SetWarn(mglWarnFile,"ReadMat");
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_rearrange(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dn"))	d->Rearrange(mgl_int(a[1].v));
	else if(d && !strcmp(k,"dnn"))	d->Rearrange(mgl_int(a[1].v), mgl_int(a[2].v));
	else if(d && !strcmp(k,"dnnn"))	d->Rearrange(mgl_int(a[1].v), mgl_int(a[2].v), mgl_int(a[3].v));
	else if(c && !strcmp(k,"dn"))	c->Rearrange(mgl_int(a[1].v));
	else if(c && !strcmp(k,"dnn"))	c->Rearrange(mgl_int(a[1].v), mgl_int(a[2].v));
	else if(c && !strcmp(k,"dnnn"))	c->Rearrange(mgl_int(a[1].v), mgl_int(a[2].v), mgl_int(a[3].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_refill(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && k[0]=='d' && k[1]=='d' && k[2]=='d')
	{
		if(k[3]==0)	gr->Refill(*d,*(a[1].d),*(a[2].d),-1,opt);
		else if(!strcmp(k+3,"n"))	gr->Refill(*d,*(a[1].d),*(a[2].d),mgl_int(a[3].v),opt);
		else if(!strcmp(k+3,"d"))	gr->Refill(*d,*(a[1].d),*(a[2].d),*(a[3].d),-1,opt);
		else if(!strcmp(k+3,"dn"))	gr->Refill(*d,*(a[1].d),*(a[2].d),*(a[3].d),mgl_int(a[4].v),opt);
		else if(!strcmp(k+3,"dd"))	gr->Refill(*d,*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),opt);
		else res = 1;
	}
	else if(c && k[0]=='d' && k[1]=='d' && k[2]=='d')
	{
		if(k[3]==0)	gr->Refill(*c,*(a[1].d),*(a[2].d),-1,opt);
		else if(!strcmp(k+3,"n"))	gr->Refill(*c,*(a[1].d),*(a[2].d),mgl_int(a[3].v),opt);
		else if(!strcmp(k+3,"d"))	gr->Refill(*c,*(a[1].d),*(a[2].d),*(a[3].d),-1,opt);
		else if(!strcmp(k+3,"dn"))	gr->Refill(*c,*(a[1].d),*(a[2].d),*(a[3].d),mgl_int(a[4].v),opt);
		else if(!strcmp(k+3,"dd"))	gr->Refill(*c,*(a[1].d),*(a[2].d),*(a[3].d),*(a[4].d),opt);
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_resize(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ddn"))	*d = mglData(true,mgl_data_resize_box(a[1].d, mgl_int(a[2].v),0,0, 0,1, 0,1, 0,1));
	else if(d && !strcmp(k,"ddnn"))	*d = mglData(true,mgl_data_resize_box(a[1].d, mgl_int(a[2].v),mgl_int(a[3].v),0, 0,1, 0,1, 0,1));
	else if(d && !strcmp(k,"ddnnn"))*d = mglData(true,mgl_data_resize_box(a[1].d, mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v), 0,1, 0,1, 0,1));
	else if(c && !strcmp(k,"ddn"))	*c = mglDataC(true,mgl_datac_resize_box(a[1].d, mgl_int(a[2].v),0,0, 0,1, 0,1, 0,1));
	else if(c && !strcmp(k,"ddnn"))	*c = mglDataC(true,mgl_datac_resize_box(a[1].d, mgl_int(a[2].v),mgl_int(a[3].v),0, 0,1, 0,1, 0,1));
	else if(c && !strcmp(k,"ddnnn"))*c = mglDataC(true,mgl_datac_resize_box(a[1].d, mgl_int(a[2].v),mgl_int(a[3].v),mgl_int(a[4].v), 0,1, 0,1, 0,1));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_rkstep(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"ss") && gr->pr)
		mgl_rk_step_w(gr->pr, a[0].s.w, a[1].s.w, 1);
	else if(!strcmp(k,"ssn") && gr->pr)
		mgl_rk_step_w(gr->pr, a[0].s.w, a[1].s.w, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_roll(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dsn"))	d->Roll(a[1].s.s[0], mgl_int(a[2].v));
	else if(c && !strcmp(k,"dsn"))	c->Roll(a[1].s.s[0], mgl_int(a[2].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_roots(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"dsds"))
		*d = mglData(true,mgl_data_roots(a[1].s.s, a[2].d, a[3].s[0]));
	else if(!strcmp(k,"dsns"))
		d->a[0] = mgl_find_root_txt(a[1].s.s, a[2].v, a[3].s[0]);
	else if(!strcmp(k,"dsd"))
		*d = mglData(true,mgl_data_roots(a[1].s.s, a[2].d, 'x'));
	else if(!strcmp(k,"dsn"))
		d->a[0] = mgl_find_root_txt(a[1].s.s, a[2].v, 'x');
	else if(!strcmp(k,"dssd"))
		*d = mglData(true,mgl_find_roots_txt(a[1].s.s, a[2].s.s, a[3].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_save(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"ds"))	a[0].d->Save(a[1].s.s);
	else if(!strcmp(k,"ss"))
	{
		FILE *fp = fopen(a[1].s.s,"a");
		size_t pos;	std::string s=a[0].s.s;
		while((pos=s.find("\\n"))!=std::string::npos)
		{	s[pos]=' ';	s[pos+1]='\n';	}
		while((pos=s.find("\b\b"))!=std::string::npos)	s.erase(pos,2);
		fprintf(fp,"%s\n",s.c_str());	fclose(fp);
	}
	else if(!strcmp(k,"sss"))
	{
		FILE *fp = fopen(a[1].s.s,a[2].s.s);
		size_t pos;	std::string s=a[0].s.s;
		while((pos=s.find("\\n"))!=std::string::npos)
		{	s[pos]=' ';	s[pos+1]='\n';	}
		while((pos=s.find("\b\b"))!=std::string::npos)	s.erase(pos,2);
		fprintf(fp,"%s\n",s.c_str());	fclose(fp);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_savehdf(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"dss"))	a[0].d->SaveHDF(a[1].s.s, a[2].s.s);
	else if(!strcmp(k,"dssn"))	a[0].d->SaveHDF(a[1].s.s, a[2].s.s,mgl_int(a[3].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_scanfile(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"dss"))
	{
		mglData *d = dynamic_cast<mglData *>(a[0].d);
		if(!d)	return 1;
		d->ScanFile(a[1].s.s, a[2].s.s);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_section(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d)
	{
		if(!strcmp(k,"dddsn"))
			*d = mglData(true,mgl_data_section(a[1].d, a[2].d, a[3].s[0], a[4].v));
		else if(!strcmp(k,"ddds"))
			*d = mglData(true,mgl_data_section(a[1].d, a[2].d, a[3].s[0], NAN));
		else if(!strcmp(k,"ddd"))
			*d = mglData(true,mgl_data_section(a[1].d, a[2].d, 'y', NAN));
		else if(!strcmp(k,"ddnsn"))
			*d = mglData(true,mgl_data_section_val(a[1].d, mgl_int(a[2].v), a[3].s[0], a[4].v));
		else if(!strcmp(k,"ddns"))
			*d = mglData(true,mgl_data_section_val(a[1].d, mgl_int(a[2].v), a[3].s[0], NAN));
		else if(!strcmp(k,"ddn"))
			*d = mglData(true,mgl_data_section_val(a[1].d, mgl_int(a[2].v), 'y', NAN));
		else res = 1;
	}
	else if(c)
	{
		if(!strcmp(k,"dddsn"))
			*d = mglDataC(true,mgl_datac_section(a[1].d, a[2].d, a[3].s[0], a[4].v));
		else if(!strcmp(k,"ddds"))
			*d = mglDataC(true,mgl_datac_section(a[1].d, a[2].d, a[3].s[0], NAN));
		else if(!strcmp(k,"ddd"))
			*d = mglDataC(true,mgl_datac_section(a[1].d, a[2].d, 'y', NAN));
		else if(!strcmp(k,"ddnsn"))
			*d = mglDataC(true,mgl_datac_section_val(a[1].d, mgl_int(a[2].v), a[3].s[0], a[4].v));
		else if(!strcmp(k,"ddns"))
			*d = mglDataC(true,mgl_datac_section_val(a[1].d, mgl_int(a[2].v), a[3].s[0], NAN));
		else if(!strcmp(k,"ddn"))
			*d = mglDataC(true,mgl_datac_section_val(a[1].d, mgl_int(a[2].v), 'y', NAN));
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_sew(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"d"))	d->Sew();
	else if(!strcmp(k,"ds"))	d->Sew(a[1].s.s);
	else if(!strcmp(k,"dsn"))	d->Sew(a[1].s.s, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_sinfft(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->SinFFT(a[1].s.s);
	else if(d && !strcmp(k,"ds"))	c->SinFFT(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_smooth(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"d"))	d->Smooth();
	else if(d && !strcmp(k,"ds"))	d->Smooth(a[1].s.s);
	else if(c && !strcmp(k,"d"))	c->Smooth();
	else if(c && !strcmp(k,"ds"))	c->Smooth(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_solve(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"ddns"))	*d = mglData(true,mgl_data_solve(a[1].d, a[2].v, a[3].s[0], 0, true));
	else if(!strcmp(k,"ddnsn"))	*d = mglData(true,mgl_data_solve(a[1].d, a[2].v, a[3].s[0], 0, a[4].v!=0));
	else if(!strcmp(k,"ddnsd"))	*d = mglData(true,mgl_data_solve(a[1].d, a[2].v, a[3].s[0], a[4].d, true));
	else if(!strcmp(k,"ddnsdn"))*d = mglData(true,mgl_data_solve(a[1].d, a[2].v, a[3].s[0], a[4].d, a[5].v!=0));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_sort(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(d && !strcmp(k,"dn"))	d->Sort(a[1].v, -1);
	else if(d && !strcmp(k,"dnn"))	d->Sort(a[1].v, a[2].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_squeeze(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dn"))	d->Squeeze(mgl_int(a[1].v));
	else if(d && !strcmp(k,"dnn"))	d->Squeeze(mgl_int(a[1].v), mgl_int(a[2].v));
	else if(d && !strcmp(k,"dnnn"))	d->Squeeze(mgl_int(a[1].v), mgl_int(a[2].v),mgl_int(a[3].v));
	else if(d && !strcmp(k,"dnnnn"))d->Squeeze(mgl_int(a[1].v), mgl_int(a[2].v),mgl_int(a[3].v), a[4].v);
	else if(c && !strcmp(k,"dn"))	c->Squeeze(mgl_int(a[1].v));
	else if(c && !strcmp(k,"dnn"))	c->Squeeze(mgl_int(a[1].v), mgl_int(a[2].v));
	else if(c && !strcmp(k,"dnnn"))	c->Squeeze(mgl_int(a[1].v), mgl_int(a[2].v),mgl_int(a[3].v));
	else if(c && !strcmp(k,"dnnnn"))c->Squeeze(mgl_int(a[1].v), mgl_int(a[2].v),mgl_int(a[3].v), a[4].v);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_stfad(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"dddn"))		*d = mglSTFA(*(a[1].d),*(a[2].d), mgl_int(a[3].v));
	else if(!strcmp(k,"dddns"))	*d = mglSTFA(*(a[1].d),*(a[2].d), mgl_int(a[3].v), a[4].s.s[0]);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_subdata(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d)
	{
		if(!strcmp(k,"ddn"))		*d = mglData(true,mgl_data_subdata(a[1].d, mgl_int(a[2].v), -1, -1));
		else if(!strcmp(k,"ddnn"))	*d = mglData(true,mgl_data_subdata(a[1].d, mgl_int(a[2].v), mgl_int(a[3].v), -1));
		else if(!strcmp(k,"ddnnn"))	*d = mglData(true,mgl_data_subdata(a[1].d, mgl_int(a[2].v), mgl_int(a[3].v), mgl_int(a[4].v)));
		else if(!strcmp(k,"ddd"))	*d = mglData(true,mgl_data_subdata_ext(a[1].d, a[2].d, 0, 0));
		else if(!strcmp(k,"dddd"))	*d = mglData(true,mgl_data_subdata_ext(a[1].d, a[2].d, a[3].d, 0));
		else if(!strcmp(k,"ddddd"))	*d = mglData(true,mgl_data_subdata_ext(a[1].d, a[2].d, a[3].d, a[4].d));
		else res = 1;
	}
	else if(c)
	{
		if(!strcmp(k,"ddn"))		*c = mglDataC(true,mgl_datac_subdata(a[1].d, mgl_int(a[2].v), -1, -1));
		else if(!strcmp(k,"ddnn"))	*c = mglDataC(true,mgl_datac_subdata(a[1].d, mgl_int(a[2].v), mgl_int(a[3].v), -1));
		else if(!strcmp(k,"ddnnn"))	*c = mglDataC(true,mgl_datac_subdata(a[1].d, mgl_int(a[2].v), mgl_int(a[3].v), mgl_int(a[4].v)));
		else if(!strcmp(k,"ddd"))	*c = mglDataC(true,mgl_datac_subdata_ext(a[1].d, a[2].d, 0, 0));
		else if(!strcmp(k,"dddd"))	*c = mglDataC(true,mgl_datac_subdata_ext(a[1].d, a[2].d, a[3].d, 0));
		else if(!strcmp(k,"ddddd"))	*c = mglDataC(true,mgl_datac_subdata_ext(a[1].d, a[2].d, a[3].d, a[4].d));
		else res = 1;
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_subto(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dd"))		*d -= *(a[1].d);
	else if(d && !strcmp(k,"dn"))	*d -= a[1].v;
	else if(c && !strcmp(k,"dd"))	*c -= *(a[1].d);
	else if(c && !strcmp(k,"dn"))	*c -= a[1].c;
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_sum(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dds"))	*d = mglData(true,mgl_data_sum(a[1].d,a[2].s.s));
	else if(c && !strcmp(k,"dds"))	*c = mglDataC(true,mgl_datac_sum(a[1].d,a[2].s.s));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_swap(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"ds"))	d->Swap(a[1].s.s);
	else if(c && !strcmp(k,"ds"))	c->Swap(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_trace(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"dd"))	*d = mglData(true,mgl_data_trace(a[1].d));
	else if(c && !strcmp(k,"dd"))	*c = mglDataC(true,mgl_datac_trace(a[1].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_transform(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!strcmp(k,"dsdd") && d)	*d = mglTransform(*(a[2].d),*(a[3].d),a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_transforma(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!strcmp(k,"dsdd") && d)	*d = mglTransformA(*(a[2].d),*(a[3].d),a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_transpose(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(d && !strcmp(k,"d"))	d->Transpose();
	else if(d && !strcmp(k,"ds"))	d->Transpose(a[1].s.s);
	else if(c && !strcmp(k,"d"))	c->Transpose();
	else if(c && !strcmp(k,"ds"))	c->Transpose(a[1].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_triangulate(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	if(!d)	return 1;
	if(!strcmp(k,"ddd"))		*d = mglTriangulation(*(a[1].d), *(a[2].d));
	else if(!strcmp(k,"dddd"))	*d = mglTriangulation(*(a[1].d), *(a[2].d), *(a[3].d));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_tridmat(mglGraph *gr, long , mglArg *a, const char *k, const char *opt)
{
	int res=0;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(c && !strcmp(k,"ddddds"))
		*c = mglTridMatC(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s);
	else if(d && !strcmp(k,"ddddds"))
		*d = mglTridMat(*(a[1].d), *(a[2].d), *(a[3].d), *(a[4].d), a[5].s.s);
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_var(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);	// TODO use mglDataV here?!
	if(!d)	return 1;
	if(!strcmp(k,"dnn"))
	{	d->Create(mgl_int(a[1].v));	d->Fill(a[2].v, NAN);	}
	else if(!strcmp(k,"dnnn"))
	{	d->Create(mgl_int(a[1].v));	d->Fill(a[2].v, a[3].v);	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int static mgls_wavelet(mglGraph *, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(k[0]=='d' && a[0].d->temp)	return 5;
	mglData *d = dynamic_cast<mglData *>(a[0].d);
	mglDataC *c = dynamic_cast<mglDataC *>(a[0].d);
	if(!strcmp(k,"dsn") && d)	d->Wavelet(a[1].s.s, mgl_int(a[2].v));
	else if(!strcmp(k,"dsn") && c)	c->Wavelet(a[1].s.s, mgl_int(a[2].v));
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
mglCommand mgls_dat_cmd[] = {
	{"addto",_("Add data or number"),"addto Var Dat|Var num", mgls_addto ,3},
	{"apde",_("Solve PDE using advanced method (X-Y only)"),"apde Res 'ham' IniRe IniIm [dz k0]", mgls_apde ,4},
	{"clean",_("Remove duplicate rows"),"clean Dat id", mgls_clean ,3},
	{"coil",_("Project periodical data in [v1,v2]"),"coil Dat v1 v2 [sep]", mgls_coil ,16},
	{"column",_("Get data column filled by formula on column ids"),"column Res Dat 'eq'", mgls_column ,4},
	{"combine",_("Direct multiplication of arrays"), "combine Res Adat Bdat", mgls_combine ,4},
	{"connect",_("Get indexes or resort data for found connected surfaces dependent on j,k"),"connect Adat Bdat|Res Adat Bdat", mgls_connect ,4},
	{"conts",_("Get contour lines for dat[i,j]=val, separated by NAN"),"conts Res val Dat", mgls_conts ,4},
	{"copy",_("Copy data from another variable"),"copy Dat1 Dat2 ['eq']|ReDat ImDat Cdat|Dat val|Dat 'name'", mgls_copy ,4},
	{"correl",_("Find correlation between data arrays"), "correl Res Adat Bdat 'dir'|Res Adat 'dir'", mgls_correl ,4},
	{"cosfft",_("Cos-Fourier transform at some direction"),"cosfft Dat 'dir'", mgls_cosfft ,16},
	{"crop",_("Crop edge of data"),"crop Dat n1 n2 'dir'|Dat 'how'", mgls_crop ,16},
	{"cumsum",_("Cumulative summation along direction(s)"),"cumsum Dat 'dir'", mgls_cumsum ,16},
	{"datagrid",_("Fill data by triangulated values"),"datagrid Var Xdat Ydat Zdat", mgls_datagrid ,3},
	{"datas",_("Print list of data names in HDF file"),"datas 'fname'", mgls_datas ,3},
	{"delete",_("Delete data or slice of data"),"delete Dat|'Dat'|Dat 'dir' [pos=0 num=1]", mgls_delete ,3},
	{"detect",_("Detect curves for maximums of data array"), "detect Res Dat lvl dj [di min_len]", mgls_detect, 4},
	{"diff",_("Numerically differentiate data"),"diff Var 'dir'|Var Dir Const1 [Const2]", mgls_diff ,16},
	{"diff2",_("Numerically double differentiate data"),"diff2 Var 'dir'", mgls_diff2 ,16},
	{"diffract",_("Step for pulse diffraction"),"diffract Res 'how' q", mgls_diffract ,16},
	{"dilate",_("Dilate data larger val"),"dilate Dat [val step]", mgls_dilate ,3},
	{"divto",_("Divide by data or number"),"divto Var Dat|Var num", mgls_divto ,3},
	{"echo",_("Print content of the data"),"echo Dat", mgls_echo ,3},
	{"envelop",_("Find envelop for the data"),"envelop Dat ['dir']", mgls_envelop ,16},
	{"erode",_("Erode data larger val"),"erode Dat [val step]", mgls_erode ,3},
	{"evaluate",_("Evaluate (interpolate) values of array Dat at points i=idat,j=jdat,k=kdat"),"evaluate Res Dat Idat [norm]|Res Dat Idat Jdat [norm]|Res Dat Idat Jdat Kdat [norm]", mgls_evaluate ,4},
	{"export",_("Export data to PNG file"),"export Dat 'fname' 'sch' [v1 v2]", mgls_export ,3},
	{"extend",_("Extend data array"),"extend Dat dim1 [dim2]", mgls_extend ,3},
	{"minmax",_("Get positions of local maximums and minimums"),"minmax Res Dat", mgls_minmax ,4},
	{"fill",_("Fill data linearly in range [v1, v2]"),"fill Var v1 v2 ['dir']|Var 'eq' [Vdat Wdat]", mgls_fill ,3},
	{"fillsample",_("Fill x-,k-samples for transforms"),"fillsample Var 'how'", mgls_fillsample ,3},
	{"fit",_("Fit data to formula"),"fit Res A 'eq' 'var' [Ini]|Res X A 'eq' 'var' [Ini]|Res X Y A 'eq' 'var' [Ini]|Res X Y Z A 'eq' 'var' [Ini]", mgls_fit ,4},
	{"fits",_("Fit data to formula"),"fits Res A S 'eq' 'var' [Ini]|Res X A S 'eq' 'var' [Ini]|Res X Y A S 'eq' 'var' [Ini]|Res X Y Z A S 'eq' 'var' [Ini]", mgls_fits ,4},
	{"fourier",_("In-place Fourier transform"),"fourier ReDat ImDat 'dir'|Cmplx 'dir'", mgls_fourier , 16},
	{"gspline",_("Fill data by global spline of Vdat"),"gspline Dat Xdat Vdat [sl]", mgls_gspline ,3},
	{"hankel",_("Hankel transform at some direction"),"hankel Dat 'dir'", mgls_hankel ,16},
	{"hist",_("Create histogram (distribution) of data values"),"hist Res Dat num v1 v2 [nsub]|Res Dat Wdat num v1 v2 [nsub]|Res Xdat Dat|Res Xdat Ydat Dat|Res Xdat Ydat Zdat Dat", mgls_hist ,4},
	{"idset",_("Set column id for data"),"idset Dat 'ids'", mgls_idset ,3},
	{"import",_("Import data from PNG file"),"import Dat 'fname' 'scheme' [v1 v2]", mgls_import ,4},
	{"info",_("Print message or information about the data"),"info Dat|'message'|const", mgls_info ,3},
	{"insert",_("Insert slice of data"),"insert Dat 'dir' [pos=0 num=1]", mgls_insert ,3},
	{"integrate",_("Integrate data along direction(s)"),"integrate Dat 'dir'", mgls_integrate ,16},
	{"jacobian",_("Get Jacobian"),"jacobian Res Xdat Ydat [Zdat]", mgls_jacobian ,4},
	{"join",_("Join data arrays"),"join Dat Add1 ...", mgls_join ,3},
	{"limit",_("Limit data to be inside [-v,v]"),"limit Dat v", mgls_limit ,16},
	{"max",_("Find maximal value over direction"),"max Res Dat 'dir'", mgls_max ,4},
	{"min",_("Find minimal value over direction"),"min Res Dat 'dir'", mgls_min ,4},
	{"mirror",_("Mirror data at some direction"),"mirror Dat 'dir'", mgls_mirror ,16},
	{"modify",_("Modify data values by formula"),"modify Dat 'eq' [num]|Dat 'eq' Vdat [Wdat]", mgls_modify ,3},
	{"momentum",_("Get momentum along direction"),"momentum Res Dat 'how' ['dir']", mgls_momentum ,4},
	{"multo",_("Multiply by data or number"),"multo Var Dat|Var num", mgls_multo ,3},
	{"new",_("Create new data"),"new Dat nx ['eq']|Dat nx ny ['eq']|Dat nx ny nz ['eq']", mgls_new ,4},
	{"norm",_("Normalize data"),"norm Dat v1 v2 [sym dim]", mgls_norm ,16},
	{"normsl",_("Normalize data slice by slice"),"normsl Dat v1 v2 ['dir' keep sym] ", mgls_normsl ,16},
	{"ode",_("Solve ODE"),"ode Res 'df' 'var' Ini [dt tmax]", mgls_ode ,4},
	{"openhdf",_("Open all data arrays from HDF file"),"openhdf 'fname'", mgls_openhdf ,3},
	{"pde",_("Solve PDE"),"pde Res 'ham' IniRe IniIm [dz k0]", mgls_pde ,4},
	{"print",_("Immediately print the message"),"print 'message'|Dat|const", mgls_print ,3},
	{"progress",_("Immediately display the progress of calculation"),"progress value maximal", mgls_progress ,3},
	{"pulse",_("Get pulse properties"),"pulse Res Dat 'dir'", mgls_pulse ,4},
	{"put",_("Put value (numeric or array) to given data element"),"put Dat val [i j k]|Dat Val [i j k]", mgls_put ,3},
	{"putsfit",_("Print fitted formula"),"putsfit x y ['pre' 'font' size]|x y z ['pre' 'font' size]", mgls_putsfit ,15},
	{"qo2d",_("Solve PDE in accompanied coordinates for 2d case"),"qo2d Res 'ham' IniRe IniIm Ray [r k0 Xout Yout]", mgls_qo2d ,4},
	{"qo3d",_("Solve PDE in accompanied coordinates for 3d case"),"qo3d Res 'ham' IniRe IniIm Ray [r k0 Xout Yout Zout]", mgls_qo3d ,4},
	{"ray",_("Solve Hamiltonian ODE (find GO ray or trajectory)"),"ray Res 'ham' x0 y0 z0 px0 py0 pz0 [dt=0.1 tmax=10]", mgls_ray ,4},
	{"read",_("Read data from file"),"read Dat 'file' [nx ny nz]|ReDat ImDat 'file' [nx ny nz]", mgls_read ,4},
	{"readall",_("Read and join data from several files"),"readall Dat 'templ' [slice]|Dat 'templ' from to [step slice]", mgls_readall ,4},
	{"readhdf",_("Read data with name 'id' from HDF file"),"readhdf Dat 'file' 'id'", mgls_readhdf ,4},
	{"readmat",_("Read data from file with sizes specified in first row"),"readmat Dat 'file' [dim]", mgls_readmat ,4},
	{"rearrange",_("Rearrange data dimensions"),"rearrange Dat mx [my mz]", mgls_rearrange ,3},
	{"refill",_("Fill data by interpolation of Vdat"),"refill Dat Xdat Vdat [sl]|Dat Xdat Ydat Vdat [sl]|Dat Xdat Ydat Zdat Vdat", mgls_refill ,3},
	{"resize",_("Resize data array"),"resize Res Dat mx [my mz]", mgls_resize ,4},
	{"rkstep",_("Perform Runge-Kutta step"),"rkstep 'Diff1;Diff2;...' 'Var1;Var2;...' [dt]", mgls_rkstep, 6},
	{"roll",_("Roll data along direction(s)"),"roll Dat 'dir' num", mgls_roll ,16},
	{"roots",_("Find roots using data as initial values"), "roots Res 'func' Ini ['var']|Res 'func' ini ['var']|Res 'func' 'vars' Ini", mgls_roots ,4},
	{"save",_("Save data to file"),"save Dat 'file'|'str' 'file' ['how']", mgls_save ,3},
	{"savehdf",_("Save data to HDF5 file"),"savehdf Dat 'file' 'id' [rewrite]", mgls_savehdf ,3},
	{"scanfile",_("Get formated data from file"),"scanfile Dat 'fname 'templ'", mgls_scanfile ,4},
	{"section",_("Extract sub-array between values"),"section Res Dat id ['dir' val]|Res Dat Ids ['dir' val]", mgls_section ,4},
	{"sew",_("Remove jump into the data, like phase jumps"),"sew Dat ['dir' da]", mgls_sew ,16},
	{"sinfft",_("Sin-Fourier transform at some direction"),"sinfft Dat 'dir'", mgls_sinfft ,16},
	{"smooth",_("Smooth data"),"smooth Dat ['how']", mgls_smooth ,16},
	{"solve",_("Find root Dat[i,j,k]=val (inverse evaluate)"),"solve Res Dat val 'dir' [Idat norm]", mgls_solve ,4},
	{"sort",_("Sort data by values in column"),"sort Dat idx [idy]", mgls_sort ,3},
	{"squeeze",_("Squeeze data"),"squeeze Dat kx [ky kz smooth]", mgls_squeeze ,3},
	{"stfad",_("Do STFA transform"),"stfad Res Real Imag dn ['dir']", mgls_stfad ,4},
	{"subdata",_("Extract sub-array"),"subdata Res Dat ix [iy iz]|Res Dat Xdat [Ydat Zdat]", mgls_subdata ,4},
	{"subto",_("Subtract data or number"),"subto Var Dat|Var num", mgls_subto ,3},
	{"sum",_("Find summation over direction"),"sum Res Dat 'dir'", mgls_sum ,4},
	{"swap",_("Swap data (useful after Fourier transform)"),"swap Dat 'dir'", mgls_swap ,16},
	{"trace",_("Get trace of array"),"trace Res Dat", mgls_trace ,4},
	{"transform",_("Do integral transform of data"),"transform Res 'how' Rdat Idat", mgls_transform ,4},
	{"transforma",_("Do integral transform of data"),"transforma Res 'how' Adat Pdat", mgls_transforma ,4},
	{"transpose",_("Transpose data array"),"transpose Dat ['dir']", mgls_transpose ,16},
	{"triangulate",_("Find triangles of randomly placed points"),"triangulate Res Xdat Ydat|Res Xdat Ydat Zdat", mgls_triangulate ,4},
	{"tridmat",_("Solve tridiagonal matrix"),"tridmat Res A B C D 'how'", mgls_tridmat ,4},
	{"var",_("Create new 1D data and fill it in range"),"var Dat nx x1 [x2]", mgls_var ,4},
	{"wavelet",_("Wavelet transform at some direction"),"wavelet Dat 'dir' k", mgls_wavelet ,16},
{"","","",NULL,0}};
//-----------------------------------------------------------------------------
