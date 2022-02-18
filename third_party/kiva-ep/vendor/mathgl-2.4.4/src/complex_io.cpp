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

#include "mgl2/datac.h"
#include "mgl2/evalc.h"
#include "mgl2/thread.h"

#if MGL_HAVE_HDF5
#define H5_USE_16_API
#include <hdf5.h>
#endif

inline bool isn(char ch)	{return ch=='\n';}
MGL_NO_EXPORT char *mgl_read_gz(gzFile fp);
HADT MGL_NO_EXPORT mglFormulaCalcC(const char *str, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
HADT MGL_EXPORT mgl_create_datac()	{	return new mglDataC;	}
HADT MGL_EXPORT mgl_create_datac_size(long nx, long ny, long nz){	return new mglDataC(nx,ny,nz);	}
HADT MGL_EXPORT mgl_create_datac_file(const char *fname)		{	return new mglDataC(fname);	}
void MGL_EXPORT mgl_delete_datac(HADT d)	{	if(d)	delete d;	}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_create_datac_()
{	return uintptr_t(new mglDataC());	}
uintptr_t MGL_EXPORT mgl_create_datac_size_(int *nx, int *ny, int *nz)
{	return uintptr_t(new mglDataC(*nx,*ny,*nz));	}
uintptr_t MGL_EXPORT mgl_create_datac_file_(const char *fname,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	uintptr_t r = uintptr_t(new mglDataC(s));	delete []s;	return r;	}
void MGL_EXPORT mgl_delete_datac_(uintptr_t *d)
{	if(_DC_)	delete _DC_;	}
//-----------------------------------------------------------------------------
cmdual MGL_EXPORT mgl_atoc(const char *s, int adv)
{
	double re=0,im=0;	size_t ll=strlen(s);
	while(s[ll]<=' ')	ll--;
	if(adv && *s=='(')		sscanf(s,"(%lg,%lg)",&re,&im);
	else if(*s=='i')		{	re=0;	im=atof(s+1);	}
	else if(adv && *s=='[')	sscanf(s,"[%lg,%lg]",&re,&im);
	else if(adv && *s=='{')	sscanf(s,"{%lg,%lg}",&re,&im);
	else if(s[ll]=='i')
	{
		double a,b;	//s[ll] = 0;
		int s1=sscanf(s,"%lg+%lg",&re,&im);
		int s2=sscanf(s,"%lg-%lg",&a,&b);
		if(s1<2)
		{
			if(s2==2)	{	re=a;	im=-b;	}
			else	{	im=atof(s);	re=0;	}
		}
	}
	else
	{
		double a,b;
		int s1=sscanf(s,"%lg+i%lg",&re,&im);
		int s2=sscanf(s,"%lg-i%lg",&a,&b);
		if(s1<2)
		{
			if(s2==2)	{	re=a;	im=-b;	}
			else	{	re=atof(s);	im=0;	}
		}
	}
	return mdual(re,im);
}
//-----------------------------------------------------------------------------
void mglFromStr(HADT d,char *buf,long NX,long NY,long NZ)
{
	if(NX<1 || NY <1 || NZ<1)	return;
	mgl_datac_create(d, NX,NY,NZ);
	const std::string loc = setlocale(LC_NUMERIC, "C");
	std::vector<char *> lines;
	std::vector<std::vector<dual> > numbs;
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
			char *s=b+j;
			long sk=0;
			while(j<nb && b[j]>' ' && ((b[j]!=',' && b[j]!=' ') || sk!=0) && b[j]!=';')
			{
				if(strchr("[{(",b[j]))	sk++;
				if(strchr("]})",b[j]))	sk--;
				j++;
			}
			b[j]=0;
			numbs[k].push_back(mgl_atoc(s,true));
		}
	}
	long i=0, n=NX*NY*NZ;
	for(long k=0;k<nl && i<n;k++)
	{
		std::vector<dual> &vals = numbs[k];
		long c = vals.size();
		if(c>n-i)	c = n-i;
		memcpy(d->a+i,&(vals[0]),c*sizeof(dual));
		i += c;
	}
	setlocale(LC_NUMERIC, loc.c_str());
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set(HADT d, HCDT a)
{
	if(!a)	return;
	const mglDataC *dd = dynamic_cast<const mglDataC *>(a);	// faster for mglData
	mgl_datac_create(d, a->GetNx(), a->GetNy(), a->GetNz());
	if(dd)	// this one should be much faster
		memcpy(d->a, dd->a, d->nx*d->ny*d->nz*sizeof(dual));
	else	// very inefficient!!!
	{
		for(long k=0;k<d->nz;k++)	for(long j=0;j<d->ny;j++)	for(long i=0;i<d->nx;i++)
			d->a[i+d->nx*(j+d->ny*k)] = a->v(i,j,k);
	}
}
void MGL_EXPORT mgl_datac_set_(uintptr_t *d, uintptr_t *a)	{	mgl_datac_set(_DC_,_DA_(a));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_values(HADT d, const char *v,long NX,long NY,long NZ)
{
	if(NX<1 || NY <1 || NZ<1)	return;
	long n=strlen(v)+1;
	char *buf = new char[n];
	memcpy(buf,v,n);
	mglFromStr(d,buf,NX,NY,NZ);
	delete []buf;
}
void MGL_EXPORT mgl_datac_set_values_(uintptr_t *d, const char *val, int *nx, int *ny, int *nz, int l)
{	char *s=new char[l+1];	memcpy(s,val,l);	s[l]=0;
	mgl_datac_set_values(_DC_,s,*nx,*ny,*nz);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_vector(HADT d, gsl_vector *v)
{
#if MGL_HAVE_GSL
	if(!v || v->size<1)	return;
	mgl_datac_create(d, v->size,1,1);
	for(long i=0;i<d->nx;i++)	d->a[i] = v->data[i*v->stride];
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_matrix(HADT d, gsl_matrix *m)
{
#if MGL_HAVE_GSL
	if(!m || m->size1<1 || m->size2<1)	return;
	mgl_datac_create(d, m->size1,m->size2,1);
	for(long j=0;j<d->ny;j++)	for(long i=0;i<d->nx;i++)
		d->a[i+j*d->nx] = m->data[i * m->tda + j];
#endif
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_float(HADT d, const float *A,long NX,long NY,long NZ)
{
	if(NX<=0 || NY<=0 || NZ<=0)	return;
	mgl_datac_create(d, NX,NY,NZ);	if(!A)	return;
#pragma omp parallel for
	for(long i=0;i<NX*NY*NZ;i++)	d->a[i] = A[i];
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_double(HADT d, const double *A,long NX,long NY,long NZ)
{
	if(NX<=0 || NY<=0 || NZ<=0)	return;
	mgl_datac_create(d, NX,NY,NZ);	if(!A)	return;
#pragma omp parallel for
	for(long i=0;i<NX*NY*NZ;i++)	d->a[i] = A[i];
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_complex(HADT d, const mdual *A,long NX,long NY,long NZ)
{
	if(NX<=0 || NY<=0 || NZ<=0)	return;
	mgl_datac_create(d, NX,NY,NZ);	if(!A)	return;
	memcpy(d->a,reinterpret_cast<const dual*>(A),NX*NY*NZ*sizeof(float));
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_float_(uintptr_t *d, const float *A,int *NX,int *NY,int *NZ)
{	mgl_datac_set_float(_DC_,A,*NX,*NY,*NZ);	}
void MGL_EXPORT mgl_datac_set_double_(uintptr_t *d, const double *A,int *NX,int *NY,int *NZ)
{	mgl_datac_set_double(_DC_,A,*NX,*NY,*NZ);	}
void MGL_EXPORT mgl_datac_set_complex_(uintptr_t *d, const mdual *A,int *NX,int *NY,int *NZ)
{	mgl_datac_set_complex(_DC_,A,*NX,*NY,*NZ);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_rearrange(HADT d, long mx,long my,long mz)
{
	if(mx<1)	return;	// wrong mx
	if(my<1)	{	my = d->nx*d->ny*d->nz/mx;	mz = 1;	}
	else if(mz<1)	mz = (d->nx*d->ny*d->nz)/(mx*my);
	long m = mx*my*mz;
	if(m==0 || m>d->nx*d->ny*d->nz)	return;	// too high desired dimensions
	d->nx = mx;	d->ny = my;	d->nz = mz;	d->NewId();
}
void MGL_EXPORT mgl_datac_rearrange_(uintptr_t *d, int *mx, int *my, int *mz)
{	mgl_datac_rearrange(_DC_,*mx,*my,*mz);	}
//-----------------------------------------------------------------------------
std::string MGL_EXPORT mgl_datac_to_string(HCDT d, long ns)
{
	std::string out;
	const mglDataC *dd = dynamic_cast<const mglDataC*>(d);
	if(!dd)	{	return	mgl_data_to_string(d,ns);	}
	long nx=dd->nx, ny=dd->ny, nz=dd->nz;
	const std::string loc = setlocale(LC_NUMERIC, "C");
	if(ns<0 || (ns>=nz && nz>1))	for(long k=0;k<nz;k++)
	{	// save whole data
		const mglDataC *dc = dynamic_cast<const mglDataC *>(d);
		if(dc)
		{
			std::string id = dc->GetColumnId();
			if(!id.empty())	out += "## "+id+'\n';
		}
		for(long i=0;i<ny;i++)
		{
			for(long j=0;j<nx-1;j++)
				out+=mgl_str_num(dd->a[j+nx*(i+ny*k)])+'\t';
			out+=mgl_str_num(dd->a[nx-1+nx*(i+ny*k)])+'\n';
		}
		out += "\n";
	}
	else
	{	// save selected slice
		if(nz>1)	for(long i=0;i<ny;i++)
		{
			for(long j=0;j<nx-1;j++)
				out+=mgl_str_num(dd->a[j+nx*(i+ny*ns)])+'\t';
			out+=mgl_str_num(dd->a[nx-1+nx*(i+ny*ns)])+'\n';
		}
		else if(ns<ny)	for(long j=0;j<nx;j++)
			out+=mgl_str_num(dd->a[j+nx*ns])+'\t';
	}
	setlocale(LC_NUMERIC, loc.c_str());
	return out;
}
void MGL_EXPORT mgl_datac_save(HCDT d, const char *fname,long ns)
{
	FILE *fp = fopen(fname,"w");
	if(fp)	{	fprintf(fp,"%s",mgl_datac_to_string(d,ns).c_str());	fclose(fp);	}
}
void MGL_EXPORT mgl_datac_save_(uintptr_t *d, const char *fname,int *ns,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	mgl_datac_save(_DC_,s,*ns);		delete []s;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_datac_read(HADT d, const char *fname)
{
	long l=1,m=1,k=1,sk=0;
	long nb,i;
	gzFile fp = gzopen(fname,"r");
	if(!fp)
	{
		if(!d->a)	mgl_datac_create(d, 1,1,1);
		return	0;
	}
	char *buf = mgl_read_gz(fp), *tbuf=buf;
	while(*buf && *buf<=' ')	buf++;	// remove leading spaces
	nb = strlen(buf);	gzclose(fp);

	bool first=false;	// space is not allowed delimiter for file with complex numbers
	for(i=nb-1;i>=0;i--)	if(buf[i]>' ')	break;
	buf[i+1]=0;	nb = i+1;		// remove tailing spaces
	for(i=0;i<nb-1 && !isn(buf[i]);i++)	// determine nx
	{
		while(buf[i]=='#')	{	while(!isn(buf[i]) && i<nb)	i++;	}
		char ch = buf[i];
		if(ch>' ' && !first)	first=true;
		if(strchr("[{(",ch))	sk++;
		if(strchr("]})",ch))	sk--;
		if(first && buf[i+1]>' ' && (ch=='\t' || ch==';' || ((ch==' '||ch==',') && sk==0) ))	k++;
	}
	first = false;
	for(i=0;i<nb-1;i++)					// determine ny
	{
		char ch = buf[i];
		if(ch=='#')	while(!isn(buf[i]) && i<nb)	i++;
		if(isn(ch))
		{
			while(buf[i+1]=='\t') i++;
			if(isn(buf[i+1]))	{first=true;	break;	}
			m++;
		}
		if(ch=='\f')	break;
	}
	if(first)	for(i=0;i<nb-1;i++)		// determine nz
	{
		char ch = buf[i];
		if(ch=='#')	while(!isn(buf[i]) && i<nb)	i++;
//		if(ch=='#')	com = true;	// comment
		if(isn(ch))
		{
//			if(com)	{	com=false;	continue;	}
			while(buf[i+1]=='\t') i++;
			if(isn(buf[i+1]))	l++;
		}
	}
	else	for(i=0;i<nb-1;i++)	if(buf[i]=='\f')	l++;
	mglFromStr(d,buf,k,m,l);
	free(tbuf);	return 1;
}
int MGL_EXPORT mgl_datac_read_(uintptr_t *d, const char *fname,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_datac_read(_DC_, s);	delete []s;		return r;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_create(HADT d,long mx,long my,long mz)
{
	d->nx = mx>0 ? mx:1;	d->ny = my>0 ? my:1;	d->nz = mz>0 ? mz:1;
	if(d->a && !d->link)	delete [](d->a);
	d->a = new dual[d->nx*d->ny*d->nz];
	d->NewId();	d->link=false;
	memset(d->a,0,d->nx*d->ny*d->nz*sizeof(dual));
}
void MGL_EXPORT mgl_datac_create_(uintptr_t *d, int *nx,int *ny,int *nz)
{	mgl_datac_create(_DC_,*nx,*ny,*nz);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_link(HADT d, mdual *A, long mx,long my,long mz)
{
	if(!A)	return;
	if(!d->link && d->a)	delete [](d->a);
	d->nx = mx>0 ? mx:1;	d->ny = my>0 ? my:1;	d->nz = mz>0 ? mz:1;
	d->link=true;	d->a=reinterpret_cast<dual*>(A);	d->NewId();
}
void MGL_EXPORT mgl_datac_link_(uintptr_t *d, mdual *A, int *nx,int *ny,int *nz)
{	mgl_datac_link(_DC_,A,*nx,*ny,*nz);	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_datac_read_dim(HADT d, const char *fname,long mx,long my,long mz)
{
	if(mx<=0 || my<=0 || mz<=0)	return 0;
	gzFile fp = gzopen(fname,"r");
	if(!fp)	return 0;
	char *buf = mgl_read_gz(fp);
	gzclose(fp);
	mglFromStr(d,buf,mx,my,mz);
	free(buf);	return 1;
}
int MGL_EXPORT mgl_datac_read_dim_(uintptr_t *d, const char *fname,int *mx,int *my,int *mz,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	int r = mgl_datac_read_dim(_DC_,s,*mx,*my,*mz);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_datac_read_mat(HADT d, const char *fname, long dim)
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
				if(first && (ch=='\t' || ch==';') && b[i+1]!='\t') nx++;
			}
		}
	}
	else if(dim==3)
	{
		sscanf(buf+j,"%ld%ld%ld",&nx,&ny,&nz);
		while(j<nb && buf[j]!='\n')	j++;
		j++;
	}
	mglFromStr(d,buf+j,nx,ny,nz);
	free(buf);	return 1;
}
int MGL_EXPORT mgl_datac_read_mat_(uintptr_t *d, const char *fname,int *dim,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_datac_read_mat(_DC_,s,*dim);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
static void *mgl_cfill_x(void *par)
{
	mglThreadC *t=(mglThreadC *)par;
	long nx=t->p[0],ny=t->p[1];
	dual *b=t->a, x1=t->b[0], dx=t->b[1];
	char dir = t->s[0];
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)
	{
		if(dir=='x') b[i0] = x1+dx*mreal(i0%nx);
		else if(dir=='y') b[i0] = x1+dx*mreal((i0/nx)%ny);
		else if(dir=='z') b[i0] = x1+dx*mreal(i0/(nx*ny));
	}
	return 0;
}
void MGL_EXPORT mgl_datac_fill(HADT d, mdual x1, mdual x2,char dir)
{
	if(mgl_isnan(x2))	x2=x1;
	if(dir<'x' || dir>'z')	dir='x';
	long par[2]={d->nx,d->ny};
	dual b[2]={x1,dual(x2)-dual(x1)};
	if(dir=='x')	b[1] *= d->nx>1 ? 1./(d->nx-1):0;
	if(dir=='y')	b[1] *= d->ny>1 ? 1./(d->ny-1):0;
	if(dir=='z')	b[1] *= d->nz>1 ? 1./(d->nz-1):0;
	mglStartThreadC(mgl_cfill_x,0,d->nx*d->ny*d->nz,d->a,b,0,par,0,0,0,&dir);
}
void MGL_EXPORT mgl_datac_fill_(uintptr_t *d, mdual *x1, mdual *x2, const char *dir,int)
{	mgl_datac_fill(_DC_,*x1,*x2,*dir);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_squeeze(HADT d, long rx,long ry,long rz,long smooth)
{
	long kx,ky,kz, nx=d->nx, ny=d->ny, nz=d->nz;
	dual *b;

	// simple checking
	if(rx>=nx)	rx=nx-1;
	if(rx<1)	rx=1;
	if(ry>=ny)	ry=ny-1;
	if(ry<1)	ry=1;
	if(rz>=nz)	rz=nz-1;
	if(rz<1)	rz=1;
	// new sizes
	kx = 1+(nx-1)/rx;	ky = 1+(ny-1)/ry;	kz = 1+(nz-1)/rz;
	b = new dual[kx*ky*kz];
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
			dual s = 0;
			for(k1=k*rz;k1<k*rz+dz;k1++)	for(j1=j*ry;j1<j*ry+dz;j1++)	for(i1=i*rx;i1<i*rx+dx;i1++)
				s += d->a[i1+nx*(j1+ny*k1)];
			b[i+kx*(j+ky*k)] = s/mreal(dx*dy*dz);
		}
	if(!d->link)	delete [](d->a);
	d->a=b;	d->nx = kx;  d->ny = ky;  d->nz = kz;	d->NewId();	d->link=false;
}
void MGL_EXPORT mgl_datac_squeeze_(uintptr_t *d, int *rx,int *ry,int *rz,int *smooth)
{	mgl_datac_squeeze(_DC_,*rx,*ry,*rz,*smooth);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_extend(HADT d, long n1, long n2)
{
	long nx=d->nx, ny=d->ny, nz=d->nz;
	if(nz>2 || n1==0)	return;
	long mx, my, mz;
	dual *b=0;
	if(n1>0) // extend to higher dimension(s)
	{
		n2 = n2>0 ? n2:1;
		mx = nx;	my = ny>1?ny:n1;	mz = ny>1 ? n1 : n2;
		b = new dual[mx*my*mz];
		if(ny>1)
#pragma omp parallel for
			for(long i=0;i<n1;i++)	memcpy(b+i*nx*ny, d->a, nx*ny*sizeof(dual));
		else
#pragma omp parallel for
			for(long i=0;i<n1*n2;i++)	memcpy(b+i*nx, d->a, nx*sizeof(dual));
	}
	else
	{
		mx = -n1;	my = n2<0 ? -n2 : nx;	mz = n2<0 ? nx : ny;
		if(n2>0 && ny==1)	mz = n2;
		b = new dual[mx*my*mz];
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
			for(long i=0;i<n2;i++)	memcpy(b+i*mx*my, d->a, mx*my*sizeof(dual));
	}
	if(!d->link)	delete [](d->a);
	d->a=b;	d->nx=mx;	d->ny=my;	d->nz=mz;
	d->NewId();		d->link=false;
}
void MGL_EXPORT mgl_datac_extend_(uintptr_t *d, int *n1, int *n2)
{	mgl_datac_extend(_DC_,*n1,*n2);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_transpose(HADT d, const char *dim)
{
	long nx=d->nx, ny=d->ny, nz=d->nz, n;
	dual *b=new dual[nx*ny*nz], *a=d->a;
	if(!strcmp(dim,"xyz"))	memcpy(b,a,nx*ny*nz*sizeof(dual));
	else if(!strcmp(dim,"xzy") || !strcmp(dim,"zy"))
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
	memcpy(a,b,nx*ny*nz*sizeof(dual));	delete []b;
	n=d->nx;	d->nx=nx;	d->ny=ny;	d->nz=nz;
	if(nx!=n)	d->NewId();
}
void MGL_EXPORT mgl_datac_transpose_(uintptr_t *d, const char *dim,int l)
{	char *s=new char[l+1];	memcpy(s,dim,l);	s[l]=0;
	mgl_datac_transpose(_DC_,s);	delete []s;	}
//-----------------------------------------------------------------------------
static void *mgl_cmodify(void *par)
{
	mglThreadC *t=(mglThreadC *)par;
	const mglFormulaC *f = (const mglFormulaC *)(t->v);
	long nx=t->p[0],ny=t->p[1],nz=t->p[2];
	dual *b=t->a;
	mreal dx,dy,dz;
	const dual *v=t->b, *w=t->c;
	dx=nx>1?1/(nx-1.):0;	dy=ny>1?1/(ny-1.):0;	dz=nz>1?1/(nz-1.):0;
#if !MGL_HAVE_PTHREAD
#pragma omp parallel for
#endif
	for(long i0=t->id;i0<t->n;i0+=mglNumThr)
	{
		long i=i0%nx, j=((i0/nx)%ny), k=i0/(nx*ny);
		b[i0] = f->Calc(i*dx, j*dy, k*dz, b[i0], v?v[i0]:dual(0,0), w?w[i0]:dual(0,0));
	}
	return 0;
}
void MGL_EXPORT mgl_datac_modify(HADT d, const char *eq,long dim)
{
	long nx=d->nx, ny=d->ny, nz=d->nz, par[3]={nx,ny,nz};
	if(dim<=0)	mgl_datac_modify_vw(d,eq,0,0);	// fastest variant for whole array
	mglFormulaC f(eq);
	if(nz>1)	// 3D array
	{
		par[2] -= dim;	if(par[2]<0)	par[2]=0;
		mglStartThreadC(mgl_cmodify,0,nx*ny*par[2],d->a+nx*ny*dim,0,0,par,&f);
	}
	else		// 2D or 1D array
	{
		par[1] -= dim;	if(par[1]<0)	par[1]=0;
		mglStartThreadC(mgl_cmodify,0,nx*par[1],d->a+nx*dim,0,0,par,&f);
	}
}
void MGL_EXPORT mgl_datac_modify_(uintptr_t *d, const char *eq,int *dim,int l)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	mgl_datac_modify(_DC_,s,*dim);	delete []s;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_modify_vw(HADT d, const char *eq,HCDT vdat,HCDT wdat)
{
	std::wstring s = d->Name();	d->Name(L"u");
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
	d->Move(mglFormulaCalcC(eq,list));	d->Name(s.c_str());
}
void MGL_EXPORT mgl_datac_modify_vw_(uintptr_t *d, const char *eq, uintptr_t *v, uintptr_t *w,int l)
{	char *s=new char[l+1];	memcpy(s,eq,l);	s[l]=0;
	mgl_datac_modify_vw(_DC_,s,_DA_(v),_DA_(w));	delete []s;	}
//-----------------------------------------------------------------------------
static bool mgl_add_file(long &kx,long &ky, long &kz, dual *&b, mglDataC *d,bool as_slice)
{
	if(as_slice && d->nz==1)
	{
		if(kx==d->nx && d->ny==1)
		{
			b = (dual *)realloc(b,kx*(ky+1)*sizeof(dual));
			memcpy(b+kx*ky,d->a,kx*sizeof(dual));		ky++;
		}
		else if(kx==d->nx && ky==d->ny)
		{
			b = (dual *)realloc(b,kx*ky*(kz+1)*sizeof(dual));
			memcpy(b+kx*ky*kz,d->a,kx*ky*sizeof(dual));	kz++;
		}
		else	return false;
	}
	else
	{
		if(d->ny*d->nz==1 && ky*kz==1)
		{
			b = (dual *)realloc(b,(kx+d->nx)*sizeof(dual));
			memcpy(b+kx,d->a,d->nx*sizeof(dual));	kx+=d->nx;
		}
		else if(kx==d->nx && kz==1 && d->nz==1)
		{
			b = (dual *)realloc(b,kx*(ky+d->ny)*sizeof(dual));
			memcpy(b+kx*ky,d->a,kx*d->ny*sizeof(dual));	ky+=d->ny;
		}
		else if(kx==d->nx && ky==d->ny)
		{
			b = (dual *)realloc(b,kx*kx*(kz+d->nz)*sizeof(dual));
			memcpy(b+kx*ky*kz,d->a,kx*ky*d->nz*sizeof(dual));	kz+=d->nz;
		}
		else	return false;
	}
	return true;
}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_datac_read_range(HADT dat, const char *templ, double from, double to, double step, int as_slice)
{
	mglDataC d;
	double t = from;
	dual *b;
	long kx,ky,kz,n=strlen(templ)+20;
	char *fname = new char[n];

	//read first file
	do{	snprintf(fname,n,templ,t);	fname[n-1]=0;	t+= step;	} while(!mgl_datac_read(&d,fname) && t<=to);

	if(t>to)	{	delete []fname;	return 0;	}
	kx = d.nx;	ky = d.ny;	kz = d.nz;
	b = (dual *)malloc(kx*ky*kz*sizeof(dual));
	memcpy(b,d.a,kx*ky*kz*sizeof(dual));

	// read other files
	for(;t<=to;t+=step)
	{
		snprintf(fname,n,templ,t);	fname[n-1]=0;
		if(mgl_datac_read(&d,fname))
			if(!mgl_add_file(kx,ky,kz,b,&d,as_slice))
			{	delete []fname;	free(b);	return 0;	}
	}
	dat->Set(b,kx,ky,kz);
	delete []fname;	free(b);
	return 1;
}
int MGL_EXPORT mgl_datac_read_range_(uintptr_t *d, const char *fname, mreal *from, mreal *to, mreal *step, int *as_slice,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_datac_read_range(_DC_,s,*from,*to,*step,*as_slice);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_datac_read_all(HADT dat, const char *templ, int as_slice)
{
#ifndef WIN32
	mglDataC d;
	glob_t res;
	size_t i;
	dual *b;
	long kx,ky,kz;
	glob (templ, GLOB_TILDE, NULL, &res);

	//read first file
	for(i=0;i<res.gl_pathc;i++)
		if(mgl_datac_read(&d,res.gl_pathv[i]))	break;

	if(i>=res.gl_pathc)	{	globfree (&res);	return 0;	}
	kx = d.nx;	ky = d.ny;	kz = d.nz;
	b = (dual *)malloc(kx*ky*kz*sizeof(dual));
	memcpy(b,d.a,kx*ky*kz*sizeof(dual));

	for(;i<res.gl_pathc;i++)
	{
		if(mgl_datac_read(&d,res.gl_pathv[i]))
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
int MGL_EXPORT mgl_datac_read_all_(uintptr_t *d, const char *fname, int *as_slice,int l)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	int r = mgl_datac_read_all(_DC_,s,*as_slice);	delete []s;	return r;	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_datac_real(HCDT d)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	mglData *r=new mglData(nx,ny,nz);
	const mglDataC *dd = dynamic_cast<const mglDataC*>(d);
	if(dd)
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	r->a[i] = real(dd->a[i]);
	else		r->Set(d);
	return r;
}
uintptr_t MGL_EXPORT mgl_datac_real_(uintptr_t *d)
{	return uintptr_t(mgl_datac_real(_DC_));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_datac_imag(HCDT d)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	mglData *r=new mglData(nx,ny,nz);
	const mglDataC *dd = dynamic_cast<const mglDataC*>(d);
	if(dd)
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	r->a[i] = imag(dd->a[i]);
	return r;
}
uintptr_t MGL_EXPORT mgl_datac_imag_(uintptr_t *d)
{	return uintptr_t(mgl_datac_imag(_DC_));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_datac_norm(HCDT d)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	mglData *r=new mglData(nx,ny,nz);
	const mglDataC *dd = dynamic_cast<const mglDataC*>(d);
	if(dd)
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	r->a[i] = norm(dd->a[i]);
	else
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	r->a[i] = mgl_ipow(d->vthr(i),2);
	return r;
}
uintptr_t MGL_EXPORT mgl_datac_norm_(uintptr_t *d)
{	return uintptr_t(mgl_datac_norm(_DC_));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_datac_abs(HCDT d)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	mglData *r=new mglData(nx,ny,nz);
	const mglDataC *dd = dynamic_cast<const mglDataC*>(d);
	if(dd)
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	r->a[i] = abs(dd->a[i]);
	else
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	r->a[i] = fabs(d->vthr(i));
	return r;
}
uintptr_t MGL_EXPORT mgl_datac_abs_(uintptr_t *d)
{	return uintptr_t(mgl_datac_abs(_DC_));	}
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_datac_arg(HCDT d)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	mglData *r=new mglData(nx,ny,nz);
	const mglDataC *dd = dynamic_cast<const mglDataC*>(d);
	if(dd)
#pragma omp parallel for
		for(long i=0;i<nx*ny*nz;i++)	r->a[i] = arg(dd->a[i]);
	return r;
}
uintptr_t MGL_EXPORT mgl_datac_arg_(uintptr_t *d)
{	return uintptr_t(mgl_datac_arg(_DC_));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_ri(HADT d, HCDT re, HCDT im)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	d->Create(nx,ny,nz);
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)	d->a[i] = dual(re->vthr(i),im->vthr(i));
}
void MGL_EXPORT mgl_datac_set_ri_(uintptr_t *d, uintptr_t *re, uintptr_t *im)
{	mgl_datac_set_ri(_DC_,_DA_(re),_DA_(im));	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_set_ap(HADT d, HCDT a, HCDT p)
{
	long nx=d->GetNx(),ny=d->GetNy(),nz=d->GetNz();
	d->Create(nx,ny,nz);
#pragma omp parallel for
	for(long i=0;i<nx*ny*nz;i++)
	{
		mreal aa=a->vthr(i), pp=p->vthr(i);
		d->a[i] = dual(aa*cos(pp), aa*sin(pp));
	}
}
void MGL_EXPORT mgl_datac_set_ap_(uintptr_t *d, uintptr_t *a, uintptr_t *p)
{	mgl_datac_set_ap(_DC_,_DA_(a),_DA_(p));	}
//-----------------------------------------------------------------------------
#if MGL_HAVE_HDF5
void MGL_EXPORT mgl_datac_save_hdf(HCDT dat,const char *fname,const char *data,int rewrite)
{
	const mglDataC *d = dynamic_cast<const mglDataC *>(dat);
	if(!d)	{	mgl_data_save_hdf(dat,fname,data,rewrite);	return;	}
	hid_t hf,hd,hs;
	hsize_t dims[4];
	long rank = 3, res;
	H5Eset_auto(0,0);
	res=H5Fis_hdf5(fname);
	if(res>0 && !rewrite)	hf = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);
	else	hf = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if(hf<0)	return;
	if(d->nz==1 && d->ny == 1)	{	rank=2;	dims[0]=d->nx;	dims[1]=2;	}
	else if(d->nz==1)	{	rank=3;	dims[0]=d->ny;	dims[1]=d->nx;	dims[2]=2;	}
	else	{	rank=4;	dims[0]=d->nz;	dims[1]=d->ny;	dims[2]=d->nx;	dims[3]=2;	}
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
int MGL_EXPORT mgl_datac_read_hdf(HADT d,const char *fname,const char *data)
{
	hid_t hf,hd,hs;
	hsize_t dims[4];
	long rank, res = H5Fis_hdf5(fname);
	if(res<=0)	return 0;
	hf = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT);
	if(hf<0)	return 0;
	hd = H5Dopen(hf,data);
	if(hd<0)	return 0;
	hs = H5Dget_space(hd);
	rank = H5Sget_simple_extent_ndims(hs);
	if(rank>0 && rank<=4)
	{
		H5Sget_simple_extent_dims(hs,dims,0);
		if(dims[rank-1]==2)
		{
			if(rank==1)			{	dims[2]=dims[0]=dims[1]=1;	}
			else if(rank==2)	{	dims[2]=dims[0];	dims[0]=dims[1]=1;	}
			else if(rank==3)	{	dims[2]=dims[1];	dims[1]=dims[0];	dims[0]=1;	}
			mgl_datac_create(d,dims[2],dims[1],dims[0]);
#if MGL_USE_DOUBLE
			H5Dread(hd, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, d->a);
#else
			H5Dread(hd, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, d->a);
#endif
		}
		else if(rank<=3)
		{
			if(rank==1)			{	dims[2]=dims[0];	dims[0]=dims[1]=1;	}
			else if(rank==2)	{	dims[2]=dims[1];	dims[1]=dims[0];	dims[0]=1;	}
			mgl_datac_create(d,dims[2],dims[1],dims[0]);
			long nn = dims[2]*dims[1]*dims[0];
			mreal *a = new mreal[nn];
#if MGL_USE_DOUBLE
			H5Dread(hd, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, a);
#else
			H5Dread(hd, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, a);
#endif
			for(long i=0;i<nn;i++)	d->a[i] = a[i];
			delete []a;
		}
	}
	H5Sclose(hs);	H5Dclose(hd);	H5Fclose(hf);	return 1;
}
//-----------------------------------------------------------------------------
#else
void MGL_EXPORT mgl_datac_save_hdf(HCDT ,const char *,const char *,int )
{	mgl_set_global_warn(_("HDF5 support was disabled. Please, enable it and rebuild MathGL."));	}
int MGL_EXPORT mgl_datac_read_hdf(HADT ,const char *,const char *)
{	mgl_set_global_warn(_("HDF5 support was disabled. Please, enable it and rebuild MathGL."));	return 0;}
#endif
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_datac_read_hdf_(uintptr_t *d, const char *fname, const char *data,int l,int n)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	char *t=new char[n+1];		memcpy(t,data,n);	t[n]=0;
	int r = mgl_datac_read_hdf(_DC_,s,t);	delete []s;	delete []t;	return r;	}
void MGL_EXPORT mgl_datac_save_hdf_(uintptr_t *d, const char *fname, const char *data, int *rewrite,int l,int n)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	char *t=new char[n+1];		memcpy(t,data,n);	t[n]=0;
	mgl_datac_save_hdf(_DC_,s,t,*rewrite);	delete []s;	delete []t;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_datac_limit(HADT d, mreal v)
{
	long n = d->GetNN();
	dual *a = d->a;
	#pragma omp parallel for
	for(long i=0;i<n;i++)
	{	mreal b = abs(a[i]);	if(b>v)	a[i] *= v/b;	}
}
void MGL_EXPORT mgl_datac_limit_(uintptr_t *d, mreal *v)
{	mgl_datac_limit(_DC_, *v);	}
//-----------------------------------------------------------------------------
