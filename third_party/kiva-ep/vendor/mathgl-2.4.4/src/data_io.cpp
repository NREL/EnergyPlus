/***************************************************************************
 * data_io.cpp is part of Math Graphic Library
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

#include "mgl2/data.h"
#include "mgl2/datac.h"
#include "mgl2/eval.h"
#include "mgl2/thread.h"

#if MGL_HAVE_HDF5
//#define H5_NO_DEPRECATED_SYMBOLS
#define H5_USE_16_API
#include <hdf5.h>
#endif
#if MGL_HAVE_HDF4
#define intf hdf4_intf
#include <mfhdf.h>
#undef intf
#endif

inline bool isn(char ch)	{return ch=='\n';}
HMDT MGL_NO_EXPORT mglFormulaCalc(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_create_data()	{	return new mglData;	}
HMDT MGL_EXPORT mgl_create_data_size(long nx, long ny, long nz){	return new mglData(nx,ny,nz);	}
HMDT MGL_EXPORT mgl_create_data_file(const char *fname)		{	return new mglData(fname);	}
void MGL_EXPORT mgl_delete_data(HMDT d)	{	if(d)	delete d;	}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_create_data_()
{	return uintptr_t(new mglData());	}
uintptr_t MGL_EXPORT mgl_create_data_size_(int *nx, int *ny, int *nz)
{	return uintptr_t(new mglData(*nx,*ny,*nz));	}
uintptr_t MGL_EXPORT mgl_create_data_file_(const char *fname,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	uintptr_t r = uintptr_t(new mglData(s));	delete []s;	return r;	}
void MGL_EXPORT mgl_delete_data_(uintptr_t *d)
{	if(_DT_)	delete _DT_;	}
//-----------------------------------------------------------------------------
void mglFromStr(HMDT d,char *buf,long NX,long NY,long NZ)
{
	if(NX<1 || NY <1 || NZ<1)	return;
	mgl_data_create(d, NX,NY,NZ);
	const std::string loc = setlocale(LC_NUMERIC, "C");
	std::vector<char *> lines;
	std::vector<std::vector<mreal> > numbs;
	while(*buf && *buf<=' ')	buf++;
	lines.push_back(buf);
	for(char *s=buf; *s; s++)	if(isn(*s))
	{	lines.push_back(s+1);	*s = 0;	s++;	}
	numbs.resize(lines.size());
	long nl = long(lines.size());
#pragma omp parallel for
	for(long k=0;k<nl;k++)
	{
		char *b = lines[k];
		long nb = strlen(b);
		for(long j=0;j<nb;j++)
		{
			while(j<nb && b[j]<=' ')	j++;	// skip first spaces
			if(j>=nb)	break;
			if(b[j]=='#')
			{
				std::string id;
				if(j<nb-1 && b[j+1]=='#')	for(long i=j+2;i<nb;i++)
					if(b[i]>='a' && b[i]<='z')	id.push_back(b[i]);
				d->SetColumnId(id.c_str());
				break;
			}
			const char *s=b+j;
			while(j<nb && b[j]>' ' && b[j]!=',' && b[j]!=';')	j++;
			b[j]=0;
			numbs[k].push_back(strstr(s,"NAN")?NAN:atof(s));
		}
	}
	long i=0, n=NX*NY*NZ;
	for(long k=0;k<nl && i<n;k++)
	{
		const std::vector<mreal> &vals = numbs[k];
		long c = vals.size();
		if(c>n-i)	c = n-i;
		memcpy(d->a+i,&(vals[0]),c*sizeof(mreal));
		i += c;
	}
	setlocale(LC_NUMERIC, loc.c_str());
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set(HMDT d, HCDT a)
{
	if(!a)	return;
//	d->temp = a->temp;	d->s = a->s;	d->func = a->func;	d->o = a->o;

	mgl_data_create(d, a->GetNx(), a->GetNy(), a->GetNz());
	const mglData *dd = dynamic_cast<const mglData *>(a);	// faster for mglData
	if(dd)	// this one should be much faster
		memcpy(d->a, dd->a, d->nx*d->ny*d->nz*sizeof(mreal));
	else	// very inefficient!!!
#pragma omp parallel for collapse(3)
		for(long k=0;k<d->nz;k++)	for(long j=0;j<d->ny;j++)	for(long i=0;i<d->nx;i++)
			d->a[i+d->nx*(j+d->ny*k)] = a->v(i,j,k);
}
void MGL_EXPORT mgl_data_set_(uintptr_t *d, uintptr_t *a)	{	mgl_data_set(_DT_,_DA_(a));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_values(HMDT d, const char *v,long NX,long NY,long NZ)
{
	if(NX<1 || NY <1 || NZ<1)	return;
	long n=strlen(v)+1;
	char *buf = new char[n];
	memcpy(buf,v,n);
	mglFromStr(d,buf,NX,NY,NZ);
	delete []buf;
}
void MGL_EXPORT mgl_data_set_values_(uintptr_t *d, const char *val, int *nx, int *ny, int *nz, int l)
{	char *s=new char[l+1];	memcpy(s,val,l);	s[l]=0;
	mgl_data_set_values(_DT_,s,*nx,*ny,*nz);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_vector(HMDT d, gsl_vector *v)
{
#if MGL_HAVE_GSL
	if(!v || v->size<1)	return;
	mgl_data_create(d, v->size,1,1);
#pragma omp parallel for
	for(long i=0;i<d->nx;i++)	d->a[i] = v->data[i*v->stride];
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_matrix(HMDT d, gsl_matrix *m)
{
#if MGL_HAVE_GSL
	if(!m || m->size1<1 || m->size2<1)	return;
	mgl_data_create(d, m->size1,m->size2,1);
#pragma omp parallel for collapse(2)
	for(long j=0;j<d->ny;j++)	for(long i=0;i<d->nx;i++)
		d->a[i+j*d->nx] = m->data[i * m->tda + j];
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_float(HMDT d, const float *A,long NX,long NY,long NZ)
{
	if(NX<=0 || NY<=0 || NZ<=0)	return;
	mgl_data_create(d, NX,NY,NZ);	if(!A)	return;
#if MGL_USE_DOUBLE
#pragma omp parallel for
	for(long i=0;i<NX*NY*NZ;i++)	d->a[i] = A[i];
#else
	memcpy(d->a,A,NX*NY*NZ*sizeof(float));
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_double(HMDT d, const double *A,long NX,long NY,long NZ)
{
	if(NX<=0 || NY<=0 || NZ<=0)	return;
	mgl_data_create(d, NX,NY,NZ);	if(!A)	return;
#if MGL_USE_DOUBLE
	memcpy(d->a,A,NX*NY*NZ*sizeof(double));
#else
#pragma omp parallel for
	for(long i=0;i<NX*NY*NZ;i++)	d->a[i] = A[i];
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_float2(HMDT d, float const * const *A,long N1,long N2)
{
	if(N1<=0 || N2<=0)	return;
	mgl_data_create(d, N2,N1,1);	if(!A)	return;
#if MGL_USE_DOUBLE
#pragma omp parallel for collapse(2)
	for(long i=0;i<N1;i++)	for(long j=0;j<N2;j++)	d->a[j+i*N2] = A[i][j];
#else
#pragma omp parallel for
	for(long i=0;i<N1;i++)	memcpy(d->a+i*N2,A[i],N2*sizeof(float));
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_double2(HMDT d, double const *const *A,long N1,long N2)
{
	if(N1<=0 || N2<=0)	return;
	mgl_data_create(d, N2,N1,1);	if(!A)	return;
#if MGL_USE_DOUBLE
#pragma omp parallel for
	for(long i=0;i<N1;i++)	memcpy(d->a+i*N2,A[i],N2*sizeof(double));
#else
#pragma omp parallel for collapse(2)
	for(long i=0;i<N1;i++)	for(long j=0;j<N2;j++)	d->a[j+i*N2] = A[i][j];
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_float3(HMDT d, float const * const * const *A,long N1,long N2,long N3)
{
	if(N1<=0 || N2<=0 || N3<=0)	return;
	mgl_data_create(d, N3,N2,N1);	if(!A)	return;
#if MGL_USE_DOUBLE
#pragma omp parallel for collapse(3)
	for(long i=0;i<N1;i++)	for(long j=0;j<N2;j++)	for(long k=0;k<N3;k++)
		d->a[k+N3*(j+i*N2)] = A[i][j][k];
#else
#pragma omp parallel for collapse(2)
	for(long i=0;i<N1;i++)	for(long j=0;j<N2;j++)
		memcpy(d->a+N3*(j+i*N2),A[i][j],N3*sizeof(float));
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_double3(HMDT d, double const * const * const *A,long N1,long N2,long N3)
{
	if(N1<=0 || N2<=0 || N3<=0)	return;
	mgl_data_create(d, N3,N2,N1);	if(!A)	return;
#if MGL_USE_DOUBLE
#pragma omp parallel for collapse(2)
	for(long i=0;i<N1;i++)	for(long j=0;j<N2;j++)
		memcpy(d->a+N3*(j+i*N2),A[i][j],N3*sizeof(double));
#else
#pragma omp parallel for collapse(3)
	for(long i=0;i<N1;i++)	for(long j=0;j<N2;j++)	for(long k=0;k<N3;k++)
		d->a[k+N3*(j+i*N2)] = A[i][j][k];
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_float1_(uintptr_t *d, const float *A,int *NX)
{	mgl_data_set_float(_DT_,A,*NX,1,1);	}
void MGL_EXPORT mgl_data_set_double1_(uintptr_t *d, const double *A,int *NX)
{	mgl_data_set_double(_DT_,A,*NX,1,1);	}
void MGL_EXPORT mgl_data_set_float_(uintptr_t *d, const float *A,int *NX,int *NY,int *NZ)
{	mgl_data_set_float(_DT_,A,*NX,*NY,*NZ);	}
void MGL_EXPORT mgl_data_set_double_(uintptr_t *d, const double *A,int *NX,int *NY,int *NZ)
{	mgl_data_set_double(_DT_,A,*NX,*NY,*NZ);	}
void MGL_EXPORT mgl_data_set_float2_(uintptr_t *d, const float *A,int *N1,int *N2)
{	mgl_data_set_float(_DT_,A,*N1,*N2,1);	}
void MGL_EXPORT mgl_data_set_double2_(uintptr_t *d, const double *A,int *N1,int *N2)
{	mgl_data_set_double(_DT_,A,*N1,*N2,1);	}
void MGL_EXPORT mgl_data_set_float3_(uintptr_t *d, const float *A,int *N1,int *N2,int *N3)
{	mgl_data_set_float(_DT_,A,*N1,*N2,*N3);	}
void MGL_EXPORT mgl_data_set_double3_(uintptr_t *d, const double *A,int *N1,int *N2,int *N3)
{	mgl_data_set_double(_DT_,A,*N1,*N2,*N3);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_rearrange(HMDT d, long mx,long my,long mz)
{
	if(mx<1)	return;	// wrong mx
	if(my<1)	{	my = d->nx*d->ny*d->nz/mx;	mz = 1;	}
	else if(mz<1)	mz = (d->nx*d->ny*d->nz)/(mx*my);
	long m = mx*my*mz;
	if(m==0 || m>d->nx*d->ny*d->nz)	return;	// too high desired dimensions
	d->nx = mx;	d->ny = my;	d->nz = mz;	d->NewId();
}
void MGL_EXPORT mgl_data_rearrange_(uintptr_t *d, int *mx, int *my, int *mz)
{	mgl_data_rearrange(_DT_,*mx,*my,*mz);	}
//-----------------------------------------------------------------------------
MGL_EXPORT_PURE const char *mgl_data_get_id(HCDT d)	{	return d->id.s;	}
void MGL_EXPORT mgl_data_set_id(mglDataA *d, const char *ids)	{	d->id = ids;	}
void MGL_EXPORT mgl_data_set_id_(uintptr_t *d, const char *eq,int l)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	mgl_data_set_id(_DT_, s);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_set_name_w(mglDataA *dat, const wchar_t *name)
{	dat->s = name;	}
MGL_EXPORT_PURE const wchar_t *mgl_data_get_name_w(HCDT dat)
{	return dat->s.w;	}
void MGL_EXPORT mgl_data_set_name(mglDataA *dat, const char *name)
{	MGL_TO_WCS(name,dat->Name(wcs));	}
void MGL_EXPORT mgl_data_set_name_(uintptr_t *d, const char *name,int l)
{	char *s=new char[l+1];	memcpy(s,name,l);	s[l]=0;
	mgl_data_set_name(_DT_,s);		delete []s;	}
void MGL_EXPORT mgl_data_set_func(mglDataA *dat, void (*func)(void *), void *par)
{	dat->func = func;	dat->o = par;	}
//-----------------------------------------------------------------------------
/// Get section separated by symbol ch. This is analog of QString::section().
std::vector<std::string> MGL_EXPORT mgl_str_args(const std::string &str, char ch)
{
	std::vector<size_t> pos;	pos.push_back(0);
	for(size_t p=0; p != std::string::npos;)
	{	p=str.find(ch,p+1);	pos.push_back(p?p+1:0);	}
	std::vector<std::string> res;
	for(size_t i=0;i<pos.size()-1;i++)
		res.push_back(str.substr(pos[i],pos[i+1]-pos[i]-1));
	return res;
}
//-----------------------------------------------------------------------------
/// Get section separated by symbol ch. This is analog of QString::section().
std::string MGL_EXPORT mgl_str_arg(const std::string &str, char ch, int n1, int n2)
{
	std::vector<size_t> pos;	pos.push_back(0);
	for(size_t p=0; p != std::string::npos;)
	{	p=str.find(ch,p+1);	pos.push_back(p?p+1:0);	}
	std::string res;
	if(n2<0)	n2=n1;
	if(n1<0 || n1>=long(pos.size())-1 || n2<n1)	return res;
	if(n2>=long(pos.size()))	n2=pos.size()-1;
	res = str.substr(pos[n1],pos[n2+1]-pos[n1]-1);
	if(res.size()==1 && res[0]==ch)	res.clear();
	return res;
}
//-----------------------------------------------------------------------------
/// Get string from number.
std::string MGL_EXPORT mgl_str_num(double val)
{	char buf[32];	snprintf(buf,32,"%g",val);	return std::string(buf);	}
std::string MGL_EXPORT mgl_str_num(dual val)
{
	char buf[64];
	double re = real(val), im = imag(val);
	if(re==0 && im>0)	snprintf(buf,64,"i%g",im);
	else if(re && im<0)	snprintf(buf,64,"-i%g",-im);
	else if(im>0)	snprintf(buf,64,"%g+i%g",re,im);
	else if(im<0)	snprintf(buf,64,"%g-i%g",re,-im);
	else	snprintf(buf,64,"%g",re);
	return std::string(buf);
}
//-----------------------------------------------------------------------------
std::string MGL_EXPORT mgl_data_to_string(HCDT d, long ns)
{
	long nx=d->GetNx(), ny=d->GetNy(), nz=d->GetNz();
	const std::string loc = setlocale(LC_NUMERIC, "C");
	std::string out;
	if(ns<0 || (ns>=nz && nz>1))	for(long k=0;k<nz;k++)
	{	// save whole data
		std::string id = d->GetColumnId();
		if(!id.empty())	out += "## "+id+'\n';
		for(long i=0;i<ny;i++)
		{
			for(long j=0;j<nx-1;j++)	out += mgl_str_num(d->v(j,i,k))+'\t';
			out += mgl_str_num(d->v(nx-1,i,k))+'\n';
		}
		out += "\n";
	}
	else
	{	// save selected slice
		if(nz>1)	for(long i=0;i<ny;i++)
		{
			for(long j=0;j<nx-1;j++)	out += mgl_str_num(d->v(j,i,ns))+'\t';
			out += mgl_str_num(d->v(nx-1,i,ns))+'\n';
		}
		else if(ns<ny)	for(long j=0;j<nx;j++)	out += mgl_str_num(d->v(j,ns))+'\t';
	}
	setlocale(LC_NUMERIC, loc.c_str());
	return out;
}
void MGL_EXPORT mgl_data_save(HCDT d, const char *fname,long ns)
{
	FILE *fp = fopen(fname,"w");
	if(fp)	{	fprintf(fp,"%s",mgl_data_to_string(d,ns).c_str());	fclose(fp);	}
}
void MGL_EXPORT mgl_data_save_(uintptr_t *d, const char *fname,int *ns,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	mgl_data_save(_DT_,s,*ns);		delete []s;	}
//-----------------------------------------------------------------------------
MGL_NO_EXPORT char *mgl_read_gz(gzFile fp)
{
	long size=1024,n=0,m;
	char *buf=(char*)malloc(size);
	while((m=gzread(fp,buf+size*n,size))>0)
	{
		if(m<size)	{	buf[size*n+m]=0;	break;	}
		n++;		buf=(char*)realloc(buf,size*(n+1));
		memset(buf+size*n,0,size);
	}
	return buf;
}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_read(HMDT d, const char *fname)
{
	long l=1,m=1,k=1,i;
	gzFile fp = gzopen(fname,"r");
	if(!fp)
	{
		if(!d->a)	mgl_data_create(d, 1,1,1);
		return	0;
	}
	char *buf = mgl_read_gz(fp), *tbuf=buf;
	while(*buf && *buf<=' ')	buf++;	// remove leading spaces
	long nb = strlen(buf);	gzclose(fp);

	bool first=false;
	for(i=nb-1;i>=0;i--)	if(buf[i]>' ')	break;
	buf[i+1]=0;	nb = i+1;		// remove tailing spaces
	for(i=0;i<nb-1 && !isn(buf[i]);i++)	// determine nx
	{
		while(buf[i]=='#')	{	while(!isn(buf[i]) && i<nb)	i++;	}
		char ch = buf[i];
		if(ch>' ' && !first)	first=true;
		if(first && (ch==' ' || ch=='\t' || ch==',' || ch==';') && buf[i+1]>' ') k++;
	}
	first = false;
	for(i=0;i<nb-1;i++)					// determine ny
	{
		char ch = buf[i];
		if(ch=='#')	while(!isn(buf[i]) && i<nb)	i++;
		if(isn(ch))
		{
			while(buf[i+1]==' ' || buf[i+1]=='\t') i++;
			if(isn(buf[i+1]))	{first=true;	break;	}
			m++;
		}
		if(ch=='\f')	break;
	}
	if(first)	for(i=0;i<nb-1;i++)		// determine nz
	{
		char ch = buf[i];
		if(ch=='#')	while(!isn(buf[i]) && i<nb)	i++;
		if(isn(ch))
		{
			while(buf[i+1]==' ' || buf[i+1]=='\t') i++;
			if(isn(buf[i+1]))	l++;
		}
	}
	else	for(i=0;i<nb-1;i++)	if(buf[i]=='\f')	l++;
	mglFromStr(d,buf,k,m,l);
	free(tbuf);	return 1;
}
int MGL_EXPORT mgl_data_read_(uintptr_t *d, const char *fname,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_data_read(_DT_, s);	delete []s;		return r;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_scan_file(HMDT d,const char *fname, const char *templ)
{
	// first scan for all "%g"
	char *buf=new char[strlen(templ)+1],*s=buf;
	strcpy(buf,templ);
	std::vector<std::string> strs;
	for(size_t i=0;buf[i];i++)
	{
		if(buf[i]=='%' && buf[i+1]=='%')	i++;
		else if(buf[i]=='%' && buf[i+1]=='g')
		{	buf[i]=0;	strs.push_back(s);	s = buf+i+2;	}
	}
	delete []buf;
	if(strs.size()<1)	return 0;
	// read proper lines from file
	std::vector<const char *> bufs;
	gzFile fp = gzopen(fname,"r");
	if(!fp)
	{
		if(!d->a)	mgl_data_create(d, 1,1,1);
		return	0;
	}
	s = mgl_read_gz(fp);	gzclose(fp);
	size_t len = strs[0].length();
	const char *s0 = strs[0].c_str();
	if(!strncmp(s, s0, len))	bufs.push_back(s);
	for(long i=0;s[i];i++)	if(s[i]=='\n')
	{
		while(s[i+1]=='\n')	i++;
		s[i]=0;	i++;
		if(!strncmp(s+i, s0, len))	bufs.push_back(s+i);
		if(!s[i])	break;
	}
	// parse lines and collect data
	size_t nx=strs.size(), ny=bufs.size();
	if(ny<1)
	{
		if(!d->a)	mgl_data_create(d, 1,1,1);
		return	0;
	}
	d->Create(nx,ny);
	for(size_t j=0;j<ny;j++)
	{
		const char *c = bufs[j];
		for(size_t i=0;i<nx;i++)
		{
			const char *p = strstr(c,strs[i].c_str());
			if(!p)	break;
			p += strs[i].length();	c=p;
			d->a[i+nx*j] = atof(p);
		}
	}
	free(s);	return 1;
}
int MGL_EXPORT mgl_data_scan_file_(uintptr_t *d,const char *fname, const char *templ,int l,int m)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	char *t=new char[m+1];		memcpy(t,templ,m);	t[m]=0;
	int r = mgl_data_scan_file(_DT_, s,t);	delete []s;	delete []t;	return r;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_create(HMDT d,long mx,long my,long mz)
{
	d->nx = mx>0 ? mx:1;	d->ny = my>0 ? my:1;	d->nz = mz>0 ? mz:1;
	if(d->a && !d->link)	delete [](d->a);
	d->a = new mreal[d->nx*d->ny*d->nz];
	d->NewId();	d->link=false;
	memset(d->a,0,d->nx*d->ny*d->nz*sizeof(mreal));
}
void MGL_EXPORT mgl_data_create_(uintptr_t *d, int *nx,int *ny,int *nz)
{	mgl_data_create(_DT_,*nx,*ny,*nz);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_link(HMDT d, mreal *A, long mx,long my,long mz)
{
	if(!A)	return;
	if(!d->link && d->a)	delete [](d->a);
	d->nx = mx>0 ? mx:1;	d->ny = my>0 ? my:1;	d->nz = mz>0 ? mz:1;
	d->link=true;	d->a=A;	d->NewId();
}
void MGL_EXPORT mgl_data_link_(uintptr_t *d, mreal *A, int *nx,int *ny,int *nz)
{	mgl_data_link(_DT_,A,*nx,*ny,*nz);	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_read_dim(HMDT d, const char *fname,long mx,long my,long mz)
{
	if(mx<=0 || my<=0 || mz<=0)	return 0;
	gzFile fp = gzopen(fname,"r");
	if(!fp)	return 0;
	char *buf = mgl_read_gz(fp);
	gzclose(fp);
	mglFromStr(d,buf,mx,my,mz);
	free(buf);	return 1;
}
int MGL_EXPORT mgl_data_read_dim_(uintptr_t *d, const char *fname,int *mx,int *my,int *mz,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	int r = mgl_data_read_dim(_DT_,s,*mx,*my,*mz);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_read_mat(HMDT d, const char *fname, long dim)
{
	if(dim<=0 || dim>3)	return 0;
	gzFile fp = gzopen(fname,"r");
	if(!fp)	return 0;
	long nx=1, ny=1, nz=1;
	char *buf = mgl_read_gz(fp);
	long nb = strlen(buf);	gzclose(fp);

	long j=0;
	if(buf[j]=='#')	while(!isn(buf[j]))	j++;	// skip comment
	while(j<nb && buf[j]<=' ')	j++;
	if(dim==1)
	{
		sscanf(buf+j,"%ld",&nx);
		while(j<nb && buf[j]!='\n')	j++;
		j++;
//		while(buf[j]>' ')	j++;
	}
	else if(dim==2)
	{
		sscanf(buf+j,"%ld%ld",&nx,&ny);
		while(j<nb && buf[j]!='\n')	j++;
		j++;
		char *b=buf+j;
		long l=0;
		for(long i=0;b[i];i++)
		{
			while(b[i]=='#')	{	while(!isn(b[i]) && b[i])	i++;	}
			if(b[i]=='\n')	l++;
		}
		if(l==nx*ny || l==nx*ny+1)	// try to read 3d data (i.e. columns of matrix nx*ny)
		{
			nz=ny;	ny=nx;	nx=1;	l=0;
			bool first = false;
			for(long i=0;b[i] && !isn(b[i]);i++)	// determine nx
			{
				while(b[i]=='#')	{	while(!isn(b[i]) && b[i])	i++;	}
				char ch = b[i];
				if(ch>' ' && !first)	first=true;
				if(first && (ch==' ' || ch=='\t' || ch==',' || ch==';') && b[i+1]>' ') nx++;
			}
		}
	}
	else if(dim==3)
	{
		sscanf(buf+j,"%ld%ld%ld",&nx,&ny,&nz);
		while(j<nb && buf[j]!='\n')	j++;
		j++;
/*		while(buf[j]>' ' && j<nb)	j++;
		while(buf[j]<=' ' && j<nb)	j++;
		while(buf[j]>' ' && j<nb)	j++;
		while(buf[j]<=' ' && j<nb)	j++;
		while(buf[j]>' ' && j<nb)	j++;*/
	}
	mglFromStr(d,buf+j,nx,ny,nz);
	free(buf);	return 1;
}
int MGL_EXPORT mgl_data_read_mat_(uintptr_t *d, const char *fname,int *dim,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_data_read_mat(_DT_,s,*dim);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_max(HCDT d)
{
	mreal m1=-INFINITY;
	long nn=d->GetNN();
#pragma omp parallel
	{
		mreal m=-INFINITY;
#pragma omp for nowait
		for(long i=0;i<nn;i++)
		{	mreal v = d->vthr(i);	m = m<v ? v:m;	}
#pragma omp critical(max_dat)
		{	m1 = m1>m ? m1:m;	}
	}
	return m1;
}
mreal MGL_EXPORT mgl_data_max_(uintptr_t *d)	{	return mgl_data_max(_DT_);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_min(HCDT d)
{
	mreal m1=INFINITY;
	long nn=d->GetNN();
#pragma omp parallel
	{
		mreal m=INFINITY;
#pragma omp for nowait
		for(long i=0;i<nn;i++)
		{	mreal v = d->vthr(i);	m = m>v ? v:m;	}
#pragma omp critical(min_dat)
		{	m1 = m1<m ? m1:m;	}
	}
	return m1;
}
mreal MGL_EXPORT mgl_data_min_(uintptr_t *d)	{	return mgl_data_min(_DT_);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_neg_max(HCDT d)
{
	mreal m1=0;
	long nn=d->GetNN();
#pragma omp parallel
	{
		mreal m=0;
#pragma omp for nowait
		for(long i=0;i<nn;i++)
		{	mreal v = d->vthr(i);	m = m<v && v<0 ? v:m;	}
#pragma omp critical(nmax_dat)
		{	m1 = m1>m ? m1:m;	}
	}
	return m1;
}
mreal MGL_EXPORT mgl_data_neg_max_(uintptr_t *d)	{	return mgl_data_neg_max(_DT_);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_pos_min(HCDT d)
{
	mreal m1=INFINITY;
	long nn=d->GetNN();
#pragma omp parallel
	{
		mreal m=INFINITY;
#pragma omp for nowait
		for(long i=0;i<nn;i++)
		{	mreal v = d->vthr(i);	m = m>v && v>0 ? v:m;	}
#pragma omp critical(pmin_dat)
		{	m1 = m1<m ? m1:m;	}
	}
	return m1;
}
mreal MGL_EXPORT mgl_data_pos_min_(uintptr_t *d)	{	return mgl_data_pos_min(_DT_);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_max_int(HCDT d, long *i, long *j, long *k)
{
	mreal m1=-INFINITY;
	long nx=d->GetNx(), ny=d->GetNy(), nn=d->GetNN();
#pragma omp parallel
	{
		mreal m=-INFINITY;
		long im=-1,jm=-1,km=-1;
#pragma omp for nowait
		for(long ii=0;ii<nn;ii++)
		{
			mreal v = d->vthr(ii);
			if(m < v)
			{	m=v;	im=ii%nx;	jm=(ii/nx)%ny;	km=ii/(nx*ny);   }
		}
#pragma omp critical(max_int)
		if(m1 < m)	{	m1=m;	*i=im;	*j=jm;	*k=km;   }
	}
	return m1;
}
mreal MGL_EXPORT mgl_data_max_int_(uintptr_t *d, int *i, int *j, int *k)
{	long ii,jj,kk;	mreal res=mgl_data_max_int(_DT_,&ii,&jj,&kk);
	*i=ii;	*j=jj;	*k=kk;	return res;	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_min_int(HCDT d, long *i, long *j, long *k)
{
	mreal m1=INFINITY;
	long nx=d->GetNx(), ny=d->GetNy(), nn=d->GetNN();
#pragma omp parallel
	{
		mreal m=INFINITY;
		long im=-1,jm=-1,km=-1;
#pragma omp for nowait
		for(long ii=0;ii<nn;ii++)
		{
			mreal v = d->vthr(ii);
			if(m > v)
			{	m=v;	im=ii%nx;	jm=(ii/nx)%ny;	km=ii/(nx*ny);   }
		}
#pragma omp critical(min_int)
		if(m1 > m)	{	m1=m;	*i=im;	*j=jm;	*k=km;   }
	}
	return m1;
}
mreal MGL_EXPORT mgl_data_min_int_(uintptr_t *d, int *i, int *j, int *k)
{	long ii,jj,kk;	mreal res=mgl_data_min_int(_DT_,&ii,&jj,&kk);
	*i=ii;	*j=jj;	*k=kk;	return res;	}
//-----------------------------------------------------------------------------
long MGL_EXPORT mgl_data_max_first(HCDT d, char dir, long from, long *p1, long *p2)
{
	long n=d->GetNx(), n1=d->GetNy(), n2=d->GetNz(), d1=n, d2=n*n1, dd=1;
	if(dir=='y')	{	n=n1;	n1=dd=d1;	d1=1;	}
	if(dir=='z')	{	n=n2;	n2=n1;	n1=d1;	d1=1;	dd=d2;	d2=n2;	}
	bool find=false;
	if(from>=0)
	{
		for(long i=from+1;i<n-1;i++)
		{
#pragma omp parallel for collapse(2)
			for(long i1=0;i1<n1;i1++)	for(long i2=0;i2<n2;i2++)
			{
				long ii=i*dd+i1*d1+i2*d2;
				if(d->vthr(ii)>=d->vthr(ii+dd) && d->vthr(ii)>=d->vthr(ii-dd))
				{	find=true;	if(p1)	*p1=i1;	if(p2)	*p2=i2;	}
			}
			if(find)	return i;
		}
	}
	else
	{
		for(long i=n+from-1;i>0;i--)
		{
			for(long i1=0;i1<n1;i1++)	for(long i2=0;i2<n2;i2++)
			{
				long ii=i*dd+i1*d1+i2*d2;
				if(d->vthr(ii)>=d->vthr(ii+dd) && d->vthr(ii)>=d->vthr(ii-dd))
				{	find=true;	if(p1)	*p1=i1;	if(p2)	*p2=i2;	}
			}
			if(find)	return i;
		}
	}
	return -1;
}
long MGL_EXPORT mgl_data_max_first_(uintptr_t *d, const char *dir, long *from, long *p1, long *p2,int)
{	return mgl_data_max_first(_DT_,*dir,*from,p1,p2);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_max_real(HCDT d, mreal *x, mreal *y, mreal *z)
{
	long im=-1,jm=-1,km=-1;
	long nx=d->GetNx(), ny=d->GetNy(), nz=d->GetNz();
	mreal m=mgl_data_max_int(d,&im,&jm,&km), v, v1, v2;
	*x=im;	*y=jm;	*z=km;

	v = d->v(im,jm,km);
	if(nx>2)
	{
		if(im==0)	im=1;
		if(im==nx-1)im=nx-2;
		v1 = d->v(im+1,jm,km);	v2 = d->v(im-1,jm,km);
		*x = (v1+v2-2*v)==0 ? im : im+(v2-v1)/(v1+v2-2*v)/2;
	}
	if(ny>2)
	{
		if(jm==0)	jm=1;
		if(jm==ny-1)jm=ny-2;
		v1 = d->v(im,jm+1,km);	v2 = d->v(im,jm-1,km);
		*y = (v1+v2-2*v)==0 ? jm : jm+(v2-v1)/(v1+v2-2*v)/2;
	}
	if(nz>2)
	{
		if(km==0)	km=1;
		if(km==nz-1)km=nz-2;
		v1 = d->v(im,jm,km+1);	v2 = d->v(im,jm,km-1);
		*z = (v1+v2-2*v)==0 ? km : km+(v2-v1)/(v1+v2-2*v)/2;
	}
	return m;
}
mreal MGL_EXPORT mgl_data_max_real_(uintptr_t *d, mreal *x, mreal *y, mreal *z)
{	return mgl_data_max_real(_DT_,x,y,z);	}
//-----------------------------------------------------------------------------
mreal MGL_EXPORT mgl_data_min_real(HCDT d, mreal *x, mreal *y, mreal *z)
{
	long im=-1,jm=-1,km=-1;
	long nx=d->GetNx(), ny=d->GetNy(), nz=d->GetNz();
	mreal m=mgl_data_min_int(d,&im,&jm,&km), v, v1, v2;
	*x=im;	*y=jm;	*z=km;

	v = d->v(im,jm,km);
	if(nx>2)
	{
		if(im==0)	im=1;
		if(im==nx-1)im=nx-2;
		v1 = d->v(im+1,jm,km);	v2 = d->v(im-1,jm,km);
		*x = (v1+v2-2*v)==0 ? im : im+(v2-v1)/(v1+v2-2*v)/2;
	}
	if(ny>2)
	{
		if(jm==0)	jm=1;
		if(jm==ny-1)jm=ny-2;
		v1 = d->v(im,jm+1,km);	v2 = d->v(im,jm-1,km);
		*y = (v1+v2-2*v)==0 ? jm : jm+(v2-v1)/(v1+v2-2*v)/2;
	}
	if(nz>2)
	{
		if(km==0)	km=1;
		if(km==nz-1)km=nz-2;
		v1 = d->v(im,jm,km+1);	v2 = d->v(im,jm,km-1);
		*z = (v1+v2-2*v)==0 ? km : km+(v2-v1)/(v1+v2-2*v)/2;
	}
	return m;
}
mreal MGL_EXPORT mgl_data_min_real_(uintptr_t *d, mreal *x, mreal *y, mreal *z)
{	return mgl_data_min_real(_DT_,x,y,z);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_fill(HMDT d, mreal x1,mreal x2,char dir)
{
	if(mgl_isnan(x2))	x2=x1;
	if(dir<'x' || dir>'z')	dir='x';
	long nx=d->nx,ny=d->ny,nz=d->nz;
	if(dir=='x')
	{
		mreal dx = d->nx>1 ? (x2-x1)/(d->nx-1):0;
#pragma omp parallel for collapse(2)
		for(long j=0;j<ny*nz;j++)	for(long i=1;i<nx;i++)	d->a[i+nx*j] = x1+dx*i;
#pragma omp parallel for
		for(long j=0;j<ny*nz;j++)	d->a[nx*j] = x1;
	}
	if(dir=='y')
	{
		mreal dx = d->ny>1 ? (x2-x1)/(d->ny-1):0;
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long j=1;j<ny;j++)	for(long i=0;i<nx;i++)	d->a[i+nx*(j+ny*k)] = x1+dx*j;
#pragma omp parallel for collapse(2)
		for(long j=0;j<nz;j++)	for(long i=0;i<nx;i++)	d->a[i+nx*ny*j] = x1;
	}
	if(dir=='z')
	{
		mreal dx = d->nz>1 ? (x2-x1)/(d->nz-1):0;
#pragma omp parallel for collapse(2)
		for(long j=1;j<nz;j++)	for(long i=0;i<nx*ny;i++)	d->a[i+nx*ny*j] = x1+dx*j;
#pragma omp parallel for
		for(long j=0;j<nx*ny;j++)	d->a[j] = x1;
	}
}
void MGL_EXPORT mgl_data_fill_(uintptr_t *d, mreal *x1,mreal *x2,const char *dir,int)
{	mgl_data_fill(_DT_,*x1,*x2,*dir);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_norm(HMDT d, mreal v1,mreal v2,int sym,long dim)
{
	long s,nn=d->nx*d->ny*d->nz;
	mreal a1=INFINITY,a2=-INFINITY,v,*a=d->a;
	s = dim*d->ny*(d->nz>1 ? d->nx : 1);
	for(long i=s;i<nn;i++)	// determines borders of existing data
	{	a1 = a1>a[i] ? a[i]:a1;	a2 = a2<a[i] ? a[i]:a2;	}
	if(a1==a2)  {  if(a1!=0)	a1=0.;  else a2=1;  }
	if(v1>v2)	{	v=v1;	v1=v2;	v2=v;	}	// swap if uncorrect
	if(sym)				// use symmetric
	{
		v2 = -v1>v2 ? -v1:v2;	v1 = -v2;
		a2 = -a1>a2 ? -a1:a2;	a1 = -a2;
	}
	v2 = (v2-v1)/(a2-a1);	v1 = v1-a1*v2;
#pragma omp parallel for
	for(long i=s;i<nn;i++)	a[i] = v1 + v2*a[i];
}
void MGL_EXPORT mgl_data_norm_(uintptr_t *d, mreal *v1,mreal *v2,int *sym,int *dim)
{	mgl_data_norm(_DT_,*v1,*v2,*sym,*dim);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_squeeze(HMDT d, long rx,long ry,long rz,long smooth)
{
	long kx,ky,kz, nx=d->nx, ny=d->ny, nz=d->nz;
	mreal *b;

	// simple checking
	if(rx>=nx)	rx=nx-1;
	if(rx<1)	rx=1;
	if(ry>=ny)	ry=ny-1;
	if(ry<1)	ry=1;
	if(rz>=nz)	rz=nz-1;
	if(rz<1)	rz=1;
	// new sizes
	kx = 1+(nx-1)/rx;	ky = 1+(ny-1)/ry;	kz = 1+(nz-1)/rz;
	b = new mreal[kx*ky*kz];
	if(!smooth)
#pragma omp parallel for collapse(3)
		for(long k=0;k<kz;k++)	for(long j=0;j<ky;j++)	for(long i=0;i<kx;i++)
			b[i+kx*(j+ky*k)] = d->a[i*rx+nx*(j*ry+ny*rz*k)];
	else
#pragma omp parallel for collapse(3)
		for(long k=0;k<kz;k++)	for(long j=0;j<ky;j++)	for(long i=0;i<kx;i++)
		{
			long dx,dy,dz,i1,j1,k1;
			dx = (i+1)*rx<=nx ? rx : nx-i*rx;
			dy = (j+1)*ry<=ny ? ry : ny-j*ry;
			dz = (k+1)*rz<=nz ? rz : nz-k*rz;
			mreal s = 0;
			for(k1=k*rz;k1<k*rz+dz;k1++)	for(j1=j*ry;j1<j*ry+dz;j1++)	for(i1=i*rx;i1<i*rx+dx;i1++)
				s += d->a[i1+nx*(j1+ny*k1)];
			b[i+kx*(j+ky*k)] = s/(dx*dy*dz);
		}
	if(!d->link)	delete [](d->a);
	d->a=b;	d->nx = kx;  d->ny = ky;  d->nz = kz;	d->NewId();	d->link=false;
}
void MGL_EXPORT mgl_data_squeeze_(uintptr_t *d, int *rx,int *ry,int *rz,int *smooth)
{	mgl_data_squeeze(_DT_,*rx,*ry,*rz,*smooth);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_extend(HMDT d, long n1, long n2)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	if(nz>2 || n1==0)	return;
	long mx, my, mz;
	mreal *b=0;
	if(n1>0) // extend to higher dimension(s)
	{
		n2 = n2>0 ? n2:1;
		mx = nx;	my = ny>1?ny:n1;	mz = ny>1 ? n1 : n2;
		b = new mreal[mx*my*mz];
		if(ny>1)
#pragma omp parallel for
			for(long i=0;i<n1;i++)	memcpy(b+i*nx*ny, d->a, nx*ny*sizeof(mreal));
		else
#pragma omp parallel for
			for(long i=0;i<n1*n2;i++)	memcpy(b+i*nx, d->a, nx*sizeof(mreal));
	}
	else
	{
		mx = -n1;	my = n2<0 ? -n2 : nx;	mz = n2<0 ? nx : ny;
		if(n2>0 && ny==1)	mz = n2;
		b = new mreal[mx*my*mz];
		if(n2<0)
#pragma omp parallel for collapse(2)
			for(long j=0;j<nx;j++)	for(long i=0;i<mx*my;i++)
				b[i+mx*my*j] = d->a[j];
		else
#pragma omp parallel for collapse(2)
			for(long j=0;j<nx*ny;j++)	for(long i=0;i<mx;i++)
				b[i+mx*j] = d->a[j];
		if(n2>0 && ny==1)
#pragma omp parallel for
			for(long i=0;i<n2;i++)
				memcpy(b+i*mx*my, d->a, mx*my*sizeof(mreal));
	}
	if(!d->link)	delete [](d->a);
	d->a=b;	d->nx=mx;	d->ny=my;	d->nz=mz;
	d->NewId();		d->link=false;
}
void MGL_EXPORT mgl_data_extend_(uintptr_t *d, int *n1, int *n2)
{	mgl_data_extend(_DT_,*n1,*n2);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_transpose(HMDT d, const char *dim)
{
	long nx=d->nx, ny=d->ny, nz=d->nz, n;
	mreal *b=new mreal[nx*ny*nz], *a=d->a;
	if(!strcmp(dim,"xzy") || !strcmp(dim,"zy"))
	{
#pragma omp parallel for collapse(3)
		for(long j=0;j<ny;j++)	for(long k=0;k<nz;k++)	for(long i=0;i<nx;i++)
			b[i+nx*(k+nz*j)] = a[i+nx*(j+ny*k)];
		n=nz;	nz=ny;	ny=n;
	}
	else if(!strcmp(dim,"yxz") || !strcmp(dim,"yx"))
	{
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long i=0;i<nx;i++)	for(long j=0;j<ny;j++)
			b[j+ny*(i+nx*k)] = a[i+nx*(j+ny*k)];
		n=nx;	nx=ny;	ny=n;
	}
	else if(!strcmp(dim,"yzx"))
	{
#pragma omp parallel for collapse(3)
		for(long k=0;k<nz;k++)	for(long i=0;i<nx;i++)	for(long j=0;j<ny;j++)
			b[j+ny*(k+nz*i)] = a[i+nx*(j+ny*k)];
		n=nx;	nx=ny;	ny=nz;	nz=n;
	}
	else if(!strcmp(dim,"zxy"))
	{
#pragma omp parallel for collapse(3)
		for(long i=0;i<nx;i++)	for(long j=0;j<ny;j++)	for(long k=0;k<nz;k++)
			b[k+nz*(i+nx*j)] = a[i+nx*(j+ny*k)];
		n=nx;	nx=nz;	nz=ny;	ny=n;
	}
	else if(!strcmp(dim,"zyx") || !strcmp(dim,"zx"))
	{
#pragma omp parallel for collapse(3)
		for(long i=0;i<nx;i++)	for(long j=0;j<ny;j++)	for(long k=0;k<nz;k++)
			b[k+nz*(j+ny*i)] = a[i+nx*(j+ny*k)];
		n=nz;	nz=nx;	nx=n;
	}
	else	memcpy(b,a,nx*ny*nz*sizeof(mreal));
	memcpy(a,b,nx*ny*nz*sizeof(mreal));	delete []b;
	n=d->nx;	d->nx=nx;	d->ny=ny;	d->nz=nz;
	if(nx!=n)	d->NewId();
}
void MGL_EXPORT mgl_data_transpose_(uintptr_t *d, const char *dim,int l)
{	char *s=new char[l+1];	memcpy(s,dim,l);	s[l]=0;
	mgl_data_transpose(_DT_,s);	delete []s;	}
//-----------------------------------------------------------------------------
static void *mgl_modify(void *par)
{
	mglThreadD *t=(mglThreadD *)par;
	const mglFormula *f = (const mglFormula *)(t->v);
	long nx=t->p[0],ny=t->p[1],nz=t->p[2];
	mreal *b=t->a, dx,dy,dz;
	const mreal *v=t->b, *w=t->c;
	dx=nx>1?1/(nx-1.):0;	dy=ny>1?1/(ny-1.):0;	dz=nz>1?1/(nz-1.):0;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)
	{
		long i=i0%nx, j=((i0/nx)%ny), k=i0/(nx*ny);
		b[i0] = f->Calc(i*dx, j*dy, k*dz, b[i0], v?v[i0]:0, w?w[i0]:0);
	}
	return 0;
}
void MGL_EXPORT mgl_data_modify(HMDT d, const char *eq,long dim)
{
	long nx=d->nx, ny=d->ny, nz=d->nz, par[3]={nx,ny,nz};
	if(dim<=0)	mgl_data_modify_vw(d,eq,0,0);	// fastes variant for whole array
	else if(nz>1)	// 3D array
	{
		mglFormula f(eq);
		par[2] -= dim;	if(par[2]<0)	par[2]=0;
		mglStartThread(mgl_modify,0,nx*ny*par[2],d->a+nx*ny*dim,0,0,par,&f);
	}
	else		// 2D or 1D array
	{
		mglFormula f(eq);
		par[1] -= dim;	if(par[1]<0)	par[1]=0;
		mglStartThread(mgl_modify,0,nx*par[1],d->a+nx*dim,0,0,par,&f);
	}
}
void MGL_EXPORT mgl_data_modify_(uintptr_t *d, const char *eq,int *dim,int l)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	mgl_data_modify(_DT_,s,*dim);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_modify_vw(HMDT d, const char *eq,HCDT vdat,HCDT wdat)
{
	std::wstring s=d->Name();	d->Name(L"u");
	mglDataV x(d->nx,d->ny,d->nz, 0,1,'x');	x.Name(L"x");
	mglDataV y(d->nx,d->ny,d->nz, 0,1,'y');	y.Name(L"y");
	mglDataV z(d->nx,d->ny,d->nz, 0,1,'z');	z.Name(L"z");
	mglDataV i(d->nx,d->ny,d->nz, 0,d->nx-1,'x');	i.Name(L"i");
	mglDataV j(d->nx,d->ny,d->nz, 0,d->ny-1,'y');	j.Name(L"j");
	mglDataV k(d->nx,d->ny,d->nz, 0,d->nz-1,'z');	k.Name(L"k");
	mglDataV r(d->nx,d->ny,d->nz);	r.Name(L"#$mgl");
	mglData v(vdat), w(wdat);	v.Name(L"v");	w.Name(L"w");
	std::vector<mglDataA*> list;
	list.push_back(&x);	list.push_back(&y);	list.push_back(&z);	list.push_back(d);
	list.push_back(&v);	list.push_back(&w);	list.push_back(&r);
	list.push_back(&i);	list.push_back(&j);	list.push_back(&k);
	d->Move(mglFormulaCalc(eq,list));	d->Name(s.c_str());
}
void MGL_EXPORT mgl_data_modify_vw_(uintptr_t *d, const char *eq, uintptr_t *v, uintptr_t *w,int l)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	mgl_data_modify_vw(_DT_,s,_DA_(v),_DA_(w));	delete []s;	}
//-----------------------------------------------------------------------------
#if MGL_HAVE_HDF4
int MGL_EXPORT mgl_data_read_hdf4(HMDT d,const char *fname,const char *data)
{
	int32 sd = SDstart(fname,DFACC_READ), nn, i;
	if(sd==-1)	return 0;	// is not a HDF4 file
	char name[64];
	SDfileinfo(sd,&nn,&i);
	for(i=0;i<nn;i++)
	{
		int32 sds, rank, dims[32], type, attr, in[2]={0,0};
		sds = SDselect(sd,i);
		SDgetinfo(sds,name,&rank,dims,&type,&attr);
		if(!strcmp(name,data))	// as I understand there are possible many datas with the same name
		{
			if(rank==1)			{	dims[2]=dims[0];	dims[0]=dims[1]=1;	}
			else if(rank==2)	{	dims[2]=dims[1];	dims[1]=dims[0];	dims[0]=1;	}
//			else if(rank>3)		continue;
			long mm=dims[0]*dims[1]*dims[2];
			if(type==DFNT_FLOAT32)
			{
				float *b = new float[mm];
				SDreaddata(sds,in,0,dims,b);
				mgl_data_set_float(d,b,dims[2],dims[1],dims[0]);
				delete []b;
			}
			if(type==DFNT_FLOAT64)
			{
				double *b = new double[mm];
				SDreaddata(sds,in,0,dims,b);
				mgl_data_set_double(d,b,dims[2],dims[1],dims[0]);
				delete []b;
			}
		}
		SDendaccess(sds);
	}
	SDend(sd);
	return 1;
}
#else
int MGL_EXPORT mgl_data_read_hdf4(HMDT ,const char *,const char *)
{	mgl_set_global_warn(_("HDF4 support was disabled. Please, enable it and rebuild MathGL."));	return 0;	}
#endif
//-----------------------------------------------------------------------------
#if MGL_HAVE_HDF5
void MGL_EXPORT mgl_data_save_hdf(HCDT dat,const char *fname,const char *data,int rewrite)
{
	const mglData *d = dynamic_cast<const mglData *>(dat);	// NOTE: slow for non-mglData
	if(!d)	{	mglData d(dat);	mgl_data_save_hdf(&d,fname,data,rewrite);	return;	}
	hid_t hf,hd,hs;
	hsize_t dims[3];
	long rank = 3, res;
	H5Eset_auto(0,0);
	res=H5Fis_hdf5(fname);
	if(res>0 && !rewrite)	hf = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);
	else	hf = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if(hf<0)	return;
	if(d->nz==1 && d->ny == 1)	{	rank=1;	dims[0]=d->nx;	}
	else if(d->nz==1)	{	rank=2;	dims[0]=d->ny;	dims[1]=d->nx;	}
	else	{	rank=3;	dims[0]=d->nz;	dims[1]=d->ny;	dims[2]=d->nx;	}
	hs = H5Screate_simple(rank, dims, 0);
#if MGL_USE_DOUBLE
	hid_t mem_type_id = H5T_NATIVE_DOUBLE;
#else
	hid_t mem_type_id = H5T_NATIVE_FLOAT;
#endif
	hd = H5Dcreate(hf, data, mem_type_id, hs, H5P_DEFAULT);
	H5Dwrite(hd, mem_type_id, hs, hs, H5P_DEFAULT, d->a);
	H5Dclose(hd);	H5Sclose(hs);	H5Fclose(hf);
}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_read_hdf(HMDT d,const char *fname,const char *data)
{
	hid_t hf,hd,hs;
	long rank, res = H5Fis_hdf5(fname);
	if(res<=0)	return mgl_data_read_hdf4(d,fname,data);
	hf = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT);
	if(hf<0)	return 0;
	hd = H5Dopen(hf,data);
	if(hd<0)	return 0;
	hs = H5Dget_space(hd);
	rank = H5Sget_simple_extent_ndims(hs);
	if(rank>0 && rank<=3)
	{
		hsize_t dims[3];
		H5Sget_simple_extent_dims(hs,dims,0);
		if(rank==1)			{	dims[2]=dims[0];	dims[0]=dims[1]=1;	}
		else if(rank==2)	{	dims[2]=dims[1];	dims[1]=dims[0];	dims[0]=1;	}
//		else if(rank>3)		continue;
		mgl_data_create(d,dims[2],dims[1],dims[0]);
#if MGL_USE_DOUBLE
		H5Dread(hd, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, d->a);
#else
		H5Dread(hd, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, d->a);
#endif
	}
	H5Sclose(hs);	H5Dclose(hd);	H5Fclose(hf);	return 1;
}
//-----------------------------------------------------------------------------
MGL_EXPORT const char * const * mgl_datas_hdf_str(const char *fname)
{
	static std::vector<std::string> names;
	static const char **res=0;
	hid_t hf,hg,hd,ht;
	hf = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT);	names.clear();
	if(!hf)	return 0;
	hg = H5Gopen(hf,"/");
	hsize_t num;
	char name[256];
	H5Gget_num_objs(hg, &num);	// replace by H5G_info_t t; H5Gget_info(hg,&t); num=t.nlinks;
	for(hsize_t i=0;i<num;i++)
	{
		if(H5Gget_objtype_by_idx(hg, i)!=H5G_DATASET)	continue;
		H5Gget_objname_by_idx(hg, i, name, 256);	// replace by H5Lget_name_by_idx(hg,".",i,0,0,name,256,0) ?!
		hd = H5Dopen(hf,name);
		ht = H5Dget_type(hd);
		if(H5Tget_class(ht)==H5T_FLOAT || H5Tget_class(ht)==H5T_INTEGER)	names.push_back(name);
		H5Dclose(hd);	H5Tclose(ht);
	}
	H5Gclose(hg);	H5Fclose(hf);	names.push_back("");
	if(res)	delete []res;
	size_t nn = names.size();
	res = new const char*[nn+1];
	for(size_t i=0;i<nn;i++)	res[i]=names[i].c_str();
	res[nn]=NULL;	return res;
}

long MGL_EXPORT mgl_datas_hdf(const char *fname, char *buf, long size)
{
	const char * const *res = mgl_datas_hdf_str(fname);
	if(!res)	return 0;
	long n=0, len=1;
	while(res[n][0])	{	len += strlen(res[n])+1;	n++;	}
	if(len>size)	return -long(len);
	strcpy(buf,res[0]);
	for(long i=1;i<n;i++)	{	strcat(buf,"\t");	strcat(buf,res[i]);	}
	return n;
}
#else
void MGL_EXPORT mgl_data_save_hdf(HCDT ,const char *,const char *,int )
{	mgl_set_global_warn(_("HDF5 support was disabled. Please, enable it and rebuild MathGL."));	}
MGL_EXPORT const char * const * mgl_datas_hdf_str(const char *fname)
{	mgl_set_global_warn(_("HDF5 support was disabled. Please, enable it and rebuild MathGL."));	return 0;}
long MGL_EXPORT mgl_datas_hdf(const char *, char *, long )
{	mgl_set_global_warn(_("HDF5 support was disabled. Please, enable it and rebuild MathGL."));	return 0;}
int MGL_EXPORT mgl_data_read_hdf(HMDT ,const char *,const char *)
{	mgl_set_global_warn(_("HDF5 support was disabled. Please, enable it and rebuild MathGL."));	return 0;}
#endif
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_read_hdf_(uintptr_t *d, const char *fname, const char *data,int l,int n)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	char *t=new char[n+1];		memcpy(t,data,n);	t[n]=0;
	int r = mgl_data_read_hdf(_DT_,s,t);	delete []s;	delete []t;	return r;	}
void MGL_EXPORT mgl_data_save_hdf_(uintptr_t *d, const char *fname, const char *data, int *rewrite,int l,int n)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	char *t=new char[n+1];		memcpy(t,data,n);	t[n]=0;
	mgl_data_save_hdf(_DT_,s,t,*rewrite);	delete []s;	delete []t;	}
long MGL_EXPORT mgl_datas_hdf_(const char *fname, char *buf, int l, int size)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_datas_hdf(s,buf,size);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
bool MGL_EXPORT mgl_add_file(long &kx,long &ky, long &kz, mreal *&b, mglData *d,bool as_slice)
{
	if(as_slice && d->nz==1)
	{
		if(kx==d->nx && d->ny==1)
		{
			b = (mreal *)realloc(b,kx*(ky+1)*sizeof(mreal));
			memcpy(b+kx*ky,d->a,kx*sizeof(mreal));		ky++;
		}
		else if(kx==d->nx && ky==d->ny)
		{
			b = (mreal *)realloc(b,kx*ky*(kz+1)*sizeof(mreal));
			memcpy(b+kx*ky*kz,d->a,kx*ky*sizeof(mreal));	kz++;
		}
		else	return false;
	}
	else
	{
		if(d->ny*d->nz==1 && ky*kz==1)
		{
			b = (mreal *)realloc(b,(kx+d->nx)*sizeof(mreal));
			memcpy(b+kx,d->a,d->nx*sizeof(mreal));	kx+=d->nx;
		}
		else if(kx==d->nx && kz==1 && d->nz==1)
		{
			b = (mreal *)realloc(b,kx*(ky+d->ny)*sizeof(mreal));
			memcpy(b+kx*ky,d->a,kx*d->ny*sizeof(mreal));	ky+=d->ny;
		}
		else if(kx==d->nx && ky==d->ny)
		{
			b = (mreal *)realloc(b,kx*kx*(kz+d->nz)*sizeof(mreal));
			memcpy(b+kx*ky*kz,d->a,kx*ky*d->nz*sizeof(mreal));	kz+=d->nz;
		}
		else	return false;
	}
	return true;
}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_read_range(HMDT dat, const char *templ, double from, double to, double step, int as_slice)
{
	mglData d;
	mreal t = from, *b;
	long kx,ky,kz,n=strlen(templ)+20;
	char *fname = new char[n];

	//read first file
	do{	snprintf(fname,n,templ,t);	fname[n-1]=0;	t+= step;	} while(!mgl_data_read(&d,fname) && t<=to);

	if(t>to)	{	delete []fname;	return 0;	}
	kx = d.nx;	ky = d.ny;	kz = d.nz;
	b = (mreal *)malloc(kx*ky*kz*sizeof(mreal));
	memcpy(b,d.a,kx*ky*kz*sizeof(mreal));

	// read other files
	for(;t<=to;t+=step)
	{
		snprintf(fname,n,templ,t);	fname[n-1]=0;
		if(mgl_data_read(&d,fname))
			if(!mgl_add_file(kx,ky,kz,b,&d,as_slice))
			{	delete []fname;	free(b);	return 0;	}
	}
	dat->Set(b,kx,ky,kz);
	delete []fname;	free(b);
	return 1;
}
int MGL_EXPORT mgl_data_read_range_(uintptr_t *d, const char *fname, mreal *from, mreal *to, mreal *step, int *as_slice,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_data_read_range(_DT_,s,*from,*to,*step,*as_slice);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_data_read_all(HMDT dat, const char *templ, int as_slice)
{
#ifndef WIN32
	mglData d;
	glob_t res;
	size_t i;
	mreal *b;
	long kx,ky,kz;
	glob (templ, GLOB_TILDE, NULL, &res);

	//read first file
	for(i=0;i<res.gl_pathc;i++)
		if(mgl_data_read(&d,res.gl_pathv[i]))	break;

	if(i>=res.gl_pathc)
	{	globfree (&res);	return 0;	}
	kx = d.nx;	ky = d.ny;	kz = d.nz;
	b = (mreal *)malloc(kx*ky*kz*sizeof(mreal));
	memcpy(b,d.a,kx*ky*kz*sizeof(mreal));

	for(;i<res.gl_pathc;i++)
	{
		if(mgl_data_read(&d,res.gl_pathv[i]))
			if(!mgl_add_file(kx,ky,kz,b,&d,as_slice))
			{	globfree (&res);	free(b);	return 0;	}
	}
	dat->Set(b,kx,ky,kz);

	globfree (&res);	free(b);
	return 1;
#else
	return 0;
#endif
}
int MGL_EXPORT mgl_data_read_all_(uintptr_t *d, const char *fname, int *as_slice,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_data_read_all(_DT_,s,*as_slice);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_column(HCDT dat, const char *eq)
{
	std::vector<mglDataA*> list;
	const char *id = dat->GetColumnId();
	size_t len = strlen(id);
	for(size_t i=0;i<len;i++)
	{
		mglDataT *col = new mglDataT(*dat);
		col->SetInd(i,id[i]);
		list.push_back(col);
	}
	if(list.size()==0)	return 0;	// no named columns
	mglDataV *t = new mglDataV(dat->GetNy(),dat->GetNz());
	t->Name(L"#$mgl");	list.push_back(t);
	HMDT r = mglFormulaCalc(eq,list);
	for(size_t i=0;i<list.size();i++)	delete list[i];
	return r;
}
uintptr_t MGL_EXPORT mgl_data_column_(uintptr_t *d, const char *eq,int l)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	uintptr_t r = uintptr_t(mgl_data_column(_DT_,s));
	delete []s;	return r;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_limit(HMDT d, mreal v)
{
	long n = d->GetNN();
	mreal *a = d->a;
#pragma omp parallel for
	for(long i=0;i<n;i++)
	{	mreal b = fabs(a[i]);	if(b>v)	a[i] *= v/b;	}
}
void MGL_EXPORT mgl_data_limit_(uintptr_t *d, mreal *v)
{	mgl_data_limit(_DT_, *v);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_data_coil(HMDT d, mreal v1, mreal v2, int sep)
{
	long n = d->GetNN();
	if(mgl_isnan(v2))	v2=-v1;
	if(v2<v1)	{	mreal tmp=v1;	v1=v2;	v2=tmp;	}
	mreal *a = d->a, dv = v2-v1;
	if(dv==0)	return;
	long *kk=new long[n];
#pragma omp parallel for
	for(long i=0;i<n;i++)
	{
		kk[i] = mgl_int((a[i]-v1)/dv-0.5);
		a[i] -= kk[i]*dv;
	}
	if(sep)
	{
#pragma omp parallel for
		for(long i=1;i<n;i++)	if(kk[i]!=kk[i-1])	a[i] = NAN;
	}
	delete []kk;
}
void MGL_EXPORT mgl_data_coil_(uintptr_t *d, mreal *v1, mreal *v2, int *sep)
{	mgl_data_coil(_DT_, *v1, *v2, *sep);	}
//-----------------------------------------------------------------------------
/// Read binary data and swap big-endian to little-endian if swap=true
size_t MGL_EXPORT mgl_fread(FILE *fp, void *vals, size_t size, size_t num, int swap)
{
	char *ptr = (char*)vals;
	size_t r = fread(ptr,size,num,fp);
	if(r && swap)
	{
		char buf[8], ch;
		if(size==4)	for(size_t i=0;i<r;i++)
		{
			memcpy(buf,ptr+i*size,size);
			ch=buf[0];	buf[0]=buf[3];	buf[3]=ch;
			ch=buf[1];	buf[1]=buf[2];	buf[2]=ch;
		}
		else if(size==2)	for(size_t i=0;i<r;i++)
		{
			memcpy(buf,ptr+i*size,size);
			ch=buf[0];	buf[0]=buf[1];	buf[1]=ch;
		}
		else if(size==8)	for(size_t i=0;i<r;i++)
		{
			memcpy(buf,ptr+i*size,size);
			ch=buf[0];	buf[0]=buf[7];	buf[7]=ch;
			ch=buf[1];	buf[1]=buf[6];	buf[6]=ch;
			ch=buf[2];	buf[2]=buf[5];	buf[5]=ch;
			ch=buf[3];	buf[3]=buf[4];	buf[4]=ch;
		}
	}
	return r;
}
//-----------------------------------------------------------------------------
/// Read data array from Tektronix WFM file
/** Parse Tektronix TDS5000/B, TDS6000/B/C, TDS/CSA7000/B, MSO70000/C, DSA70000/B/C DPO70000/B/C DPO7000/ MSO/DPO5000. */
int MGL_EXPORT mgl_data_read_wfm(HMDT d,const char *fname, long num, long step/*=1*/, long start/*=0*/)
{
/*	if(step<1)	step=1;
	if(start<0)	start=0;
	FILE *fp = fopen(fname,"rb");
	if(!fp)	return 0;	// couldn't open file
	unsigned short byte_order;
	fread(&byte_order,2,1,fp);
	bool byteorder;	// TODO
*/	return 0;
}
int MGL_EXPORT mgl_data_read_wfm_(uintptr_t *d, const char *fname, long *num, long *step, long *start,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	int r = mgl_data_read_wfm(_DT_,s,*num,*step,*start);
	delete []s;	return r;	}
/// Read data array from Matlab MAT file (parse versions 4 and 5)
int MGL_EXPORT mgl_data_read_matlab(HMDT d,const char *fname,const char *data)
{
	// TODO
/**/return 0;
}
int MGL_EXPORT mgl_data_read_matlab_(uintptr_t *d, const char *fname, const char *data,int l,int n)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	char *t=new char[n+1];	memcpy(t,data,n);	t[n]=0;
	int r = mgl_data_read_matlab(_DT_,s,t);	delete []s;	delete []t;	return r;	}
//-----------------------------------------------------------------------------
