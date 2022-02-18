/***************************************************************************
 * addon.cpp is part of Math Graphic Library
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
#include <stdarg.h>
#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif
#include <string.h>
#include "mgl2/addon.h"
#include "mgl2/data.h"
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_strcls(char *str)
{
	size_t len = strlen(str),i,n;
	char *tmp = new char[len+1];	memset(tmp,0,len);
	for(i=0;i<len;i++)
	{
		if(i<len-1 && str[i]==' ' && str[i+1]==' ')
			continue;
		tmp[i] = str[i];
	}
	len = strlen(tmp);
	for(n=0;n<len;n++)		if(tmp[n]!=' ')	break;
	for(i=len-1;i>0;i--)	if(tmp[i]!=' ')	break;
	tmp[i+1]=0;	strcpy(str,&(tmp[n]));
	delete []tmp;
}
//-----------------------------------------------------------------------------
long MGL_EXPORT_PURE mgl_strpos(const char *str,char *fnd)
{
	const char *p=strstr(str,fnd);
	return p?p-str:-1L;
}
//-----------------------------------------------------------------------------
long MGL_EXPORT_PURE mgl_chrpos(const char *str,char ch)
{
	const char *p=str?strchr(str,ch):0;
	return p?p-str:-1L;
}
//-----------------------------------------------------------------------------
int mgl_fgetstr_script = 0;
void MGL_EXPORT mgl_fgetstr_mgl(int enable)
{
	mgl_fgetstr_script = enable;
}
MGL_EXPORT char *mgl_fgetstr(FILE *fp)
{
	const long size=10240;	// NOTE: this set maximal length of string to be read
	static char s[size];
	do
	{
		if(!fgets(s,size,fp))	break;
		mgl_strtrim(s);
		if(mgl_fgetstr_script && s[0]=='#' && s[1]=='M' && s[2]=='G' && s[3]=='L' && s[4]==' ')
		{
			std::string buf("mglconv -n ");	buf+= s+5;
			system(buf.c_str());
		}
		//		strlwr(s);
	} while(!feof(fp) && (s[0]==0 || s[0]=='%' || s[0]=='#'));
	for(long i=0;s[i];i++)	if(s[i]=='#')	{	s[i]=0;	break;	}
	mgl_strtrim(s);
	return s;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fgetpar(FILE *fp, const char *str, ...)
{
	if(!str || !str[0])	return;
	const size_t len=strlen(str);
	char *s, *t;
	va_list lst;
	va_start(lst,str);
	t = mgl_fgetstr(fp);
	for(size_t i=0;i<len;i++)
	{
		if(str[i]=='%')
		{
			if(str[i+1]=='s')	{	s = va_arg(lst, char*);	strcpy(s, t);	}
			if(strchr("efg",str[i+1]))	{	double *v = va_arg(lst, double*);	*v = atof(t);	}
			if(strchr("ld",str[i+1]))	{	long *n = va_arg(lst, long*); 	*n = atol(t);	}
			i++;
		}
		if(str[i]==':')
		{
			for(;*t && *t!=':';t++);
			if(*t==':')	t++;
		}
		if(str[i]<=' ')	t = mgl_fgetstr(fp);
	}
	va_end(lst);
}
//-----------------------------------------------------------------------------
int MGL_EXPORT_CONST mgl_istrue(char ch)
{	return (ch=='1' || ch=='t' || ch=='+' || ch=='v');	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_test(const char *str, ...)
{
	char buf[256];
	va_list lst;
	va_start(lst,str);
	vsnprintf(buf,256,str,lst);	buf[255]=0;
	va_end(lst);
	printf(_("TEST: %s\n"),buf);
	fflush(stdout);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_info(const char *str, ...)
{
	char buf[256];
	va_list lst;
	va_start(lst,str);
	vsnprintf(buf,256,str,lst);	buf[255]=0;
	va_end(lst);
	printf("%s",buf);
	FILE *fp = fopen("info.txt","at");
	fprintf(fp,"%s",buf);
	fclose(fp);
}
//-----------------------------------------------------------------------------
MGL_EXPORT FILE *mgl_next_data(const char *fname,int p)
{
	char *s;
	int len;
	static int pos=0;
	static char path[256];

	if(p>0)	pos = p;
	if(fname==NULL)	return NULL;
	if(pos==0)	{	if(!getcwd(path,256))	return 0;	}	// remember ini dyrectory
	else		{	if(chdir(path)==-1)		return 0;	}

	// read the initial (common) data
	FILE *fp=fopen(fname,"rt");
	if(fp==NULL)	return NULL;
	fseek(fp,0,SEEK_END);
	len = ftell(fp);
	if(pos>=len)		 // no more data
	{	fclose(fp);	return NULL;	}
	fseek(fp,pos,SEEK_SET);
	//printf("pos 1 = %d\t",pos);
	do
	{
		s = mgl_fgetstr(fp);
		fflush(stdout);
		if(s[0]=='$' || s[1]=='$' || s[3]=='$')
		{	fclose(fp);	return NULL;	}
	} while(!feof(fp) && (s[0]!='-' || s[1]!='-' || s[3]!='-'));
	if(feof(fp))	// no more data
	{	fclose(fp);	return NULL;	}
	return fp;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_difr_grid(dual *a,int n,int step,dual q,int Border,dual *tmp,int kk)
{	mgl_difr_grid_old(a,n,step,q,Border,tmp,tmp+n,kk);	}
void MGL_EXPORT mgl_difr_grid_old(dual *a,int n,int step,dual q,int Border,dual *b, dual *d,int kk)
{
	const dual adt = dual(0.,1.)*q;
	if(step==1)	memcpy(b,a,n*sizeof(dual));
	else	for(long i=0;i<n;i++)	b[i] = a[i*step];
	for(long k=kk;k>0;k--)	// 3 iterations
	{
		for(long i=1;i<n-1;i++)
			d[i] = a[i*step] + adt*(b[i-1]+b[i+1]-mreal(2)*b[i])/mreal(k);
		memcpy(b,d,n*sizeof(dual));
		switch(Border)
		{
			default:
			case 0:		// zero at border
				b[0] = 0;	b[n-1] = 0;		break;
			case 1:		// constant at border
				b[0] = b[1];	b[n-1] = b[n-2];	break;
			case 2:		// linear at border
				b[0] = mreal(2)*b[1]-b[2];
				b[n-1] = mreal(2)*b[n-2]-b[n-3];
				break;
			case 3:		// square at border
				b[0] = b[3]+mreal(3)*(b[1]-b[2]);
				b[n-1] = b[n-4]+mreal(3)*(b[n-2]-b[n-3]);
				break;
			case -1:		// exponent at border
			case 4:
				b[0] = norm(b[2])<norm(b[1]) ? b[1] : b[1]*b[1]/b[2];
				b[n-1] = norm(b[n-3])<norm(b[n-2]) ? b[n-2] : b[n-2]*b[n-2]/b[n-3];
				break;
			case -2:		// gaussian at border
			case 5:
				b[0] = norm(b[2])<norm(b[1]) ? b[3] : pow(b[1]/b[2],3)*b[3];
				b[n-1] = norm(b[n-3])<norm(b[n-2]) ? b[n-4] : pow(b[n-2]/b[n-3],3)*b[n-4];
				break;
		}
	}
	if(step==1)	memcpy(a,b,n*sizeof(dual));
	else	for(long i=0;i<n;i++)	a[i*step] = b[i];
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_difr_axial(dual *a,int n,int step,dual q,int Border,dual *tmp,int kk, double di)
{	mgl_difr_axial_old(a,n,step,q,Border,tmp,tmp+n,kk,di);	}
void MGL_EXPORT mgl_difr_axial_old(dual *a,int n,int step,dual q,int Border,dual *b,dual *d,int kk, double di)
{
	int ii = di<0 ? -int(floor(di)) : 0;
	mreal ff= di==floor(di) ? 4. : 2.;
	const dual adt = dual(0.,1.)*q;
	if(step==1)	memcpy(b,a,n*sizeof(dual));
	else	for(long i=0;i<n;i++)	b[i] = a[i*step];
	for(long k=kk;k>0;k--)	// kk iterations
	{
		d[ii] = a[ii] + adt*(b[ii+1]-b[ii])*(ff/k);
		for(long i=ii+1;i<n-1;i++)
		{
			mreal dd = i+di;
			dd = 1./(sqrt(dd*dd+1.)+dd);	// corrections for "axiality"
			mreal gg = 1+dd*dd;
			d[i] = a[i*step] + adt*( b[i-1]*((gg-dd)/k) - b[i]*(2*gg/k) + b[i+1]*((gg+dd)/k) );
		}
		memcpy(b,d,n*sizeof(dual));
		switch(Border)
		{
			case 0:		// zero at border
				b[n-1] = 0;		break;
			case 1:		// constant at border
				b[n-1] = b[n-2];	break;
			case 2:		// linear at border
				b[n-1] = -b[n-3] + mreal(2)*b[n-2];
				break;
			case 3:		// square at border
				b[n-1] = b[n-4] + mreal(3)*(b[n-2]-b[n-3]);
				break;
			case -1:		// exponent at border
				b[n-1] = norm(b[n-3])>norm(b[n-2]) ? b[n-2]*b[n-2]/b[n-3] : mreal(2)*b[n-2]-b[n-3];
				break;
			case -2:		// gaussian at border
				b[n-1] = norm(b[n-3])>norm(b[n-2]) ? pow(b[n-2]/b[n-3],3)*b[n-4] : b[n-4] + mreal(3)*(b[n-2]-b[n-3]);
				break;
		}
	}
	if(step==1)	memcpy(a,b,n*sizeof(dual));
	else	for(long i=0;i<n;i++)	a[i*step] = b[i];
}
//-----------------------------------------------------------------------------
double MGL_EXPORT mgl_gauss_rnd()
{
	double v1,v2;
	v1 = mgl_rnd();
	v2 = mgl_rnd();
	return v1!=0 ? sqrt(-2.*log(v1))*cos(2*M_PI*v2) : 0;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_fft_freq(double *freq, long nn)
{
	freq[0] = 0;
	for(long i=1;i<=nn/2;i++)
	{	freq[i] = i; freq[nn-i] = -i;	}
}
//-----------------------------------------------------------------------------
