/***************************************************************************
 * fractal.cpp is part of Math Graphic Library
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
#include "mgl2/other.h"
#include "mgl2/data.h"
MGL_NO_EXPORT char *mgl_read_gz(gzFile fp);
//-----------------------------------------------------------------------------
//
//	IFS series
//
//-----------------------------------------------------------------------------
void static mgl_ifs_2d_point(HCDT A, mreal& x, mreal& y, mreal amax)
{
	long i, n=A->GetNy();
	mreal r = amax*mgl_rnd(), sum_prob = 0, x1;
	for(i=0;i<n;i++)
	{
		sum_prob += A->v(6,i);
		if(r<sum_prob)	break;
	}
	x1= A->v(0,i)*x + A->v(1,i)*y + A->v(4,i);
	y = A->v(2,i)*x + A->v(3,i)*y + A->v(5,i);	x = x1;
}
HMDT MGL_EXPORT mgl_data_ifs_2d(HCDT A, long n, long skip)
{
	if(!A || A->GetNx()<7 || n<1)	return 0;	// incompatible dimensions
	mreal amax=0;
	for(long i=0; i<A->GetNy(); i++)	amax += A->v(6,i);
	if(amax<=0)	return 0;

	mglData *f = new mglData(2,n);
	mreal x = 0, y = 0;
	for(long i=0; i<skip; i++)	mgl_ifs_2d_point(A, x, y,amax);
	for(long i=0; i<n; i++)
	{
		mgl_ifs_2d_point(A, x, y, amax);
		f->a[2*i] = x;	f->a[2*i+1] = y;
	}
	return f;
}
uintptr_t MGL_EXPORT mgl_data_ifs_2d_(uintptr_t *d, long *n, long *skip)
{	return uintptr_t(mgl_data_ifs_2d(_DT_,*n,*skip));	}
//-----------------------------------------------------------------------------
void static mgl_ifs_3d_point(HCDT A, mreal& x, mreal& y, mreal& z, mreal amax)
{
	int i, n=A->GetNy();
	mreal r = amax*mgl_rnd(), sum_prob = 0, x1, y1;
	for (i=0; i<n; i++)
	{
		sum_prob += A->v(12,i);
		if(r<sum_prob)  break;
	}
	x1= A->v(0,i)*x + A->v(1,i)*y + A->v(2,i)*z + A->v(9,i);
	y1= A->v(3,i)*x + A->v(4,i)*y + A->v(5,i)*z + A->v(10,i);
	z = A->v(6,i)*x + A->v(7,i)*y + A->v(8,i)*z + A->v(11,i);
	x = x1;	y = y1;
}
HMDT MGL_EXPORT mgl_data_ifs_3d(HCDT A, long n, long skip)
{
	if(!A || A->GetNx()<13 || n<1)	return 0;   // incompatible dimensions
	mreal amax = 0;
	for(int i=0; i<A->GetNy(); i++)	amax += A->v(12,i);
	if(amax <= 0) return 0;

	mglData *f = new mglData(3,n);
	mreal x = 0, y = 0, z = 0;
	for(long i=0; i<skip; i++)	mgl_ifs_3d_point(A, x, y, z, amax);
	for(long i=0; i<n; i++)
	{
		mgl_ifs_3d_point(A, x, y, z, amax);
		f->a[3*i] = x;	f->a[3*i+1] = y;	f->a[3*i+2] = z;
	}
	return f;
}
uintptr_t MGL_EXPORT mgl_data_ifs_3d_(uintptr_t *d, long *n, long *skip)
{   return uintptr_t(mgl_data_ifs_3d(_DT_,*n,*skip));   }
//-----------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_data_ifs_file(const char *fname, const char *name, long n, long skip)
{
	gzFile fp = gzopen(fname,"r");
	if(!fp)	return 0;		// Couldn't open file file
	char *buf = mgl_read_gz(fp);	gzclose(fp);
	char *s = strstr(buf,name);
	if(!s)	return 0;		// No data for fractal 'name' in the file

	char *p = strchr(s,'{'), *e;
	if(!p)	return 0;		// Wrong data format for fractal 'name' in the file
	bool ext3d = false;
	e = strstr(s,"(3D)");	if(e && e<p)	ext3d = true;
	e = strstr(s,"(3d)");	if(e && e<p)	ext3d = true;
	e = strchr(p,'}');

	std::vector<mreal> nums;
	for(size_t i=0;p[i] && p+i<e;i++)
	{
		while(p[i]<=' ')	i++;
		if(p[i]==';' || p[i]=='#')	while(p[i] && p[i]!='\n')	i++;
		if(strchr("0123456789.+-",p[i]))	// this is number
		{
			nums.push_back(atof(p+i));
			while(p[i]>' ')	i++;
		}
	}
	HMDT dat = new mglData, res;
	if(ext3d)
	{
		dat->Set(&(nums[0]), 13, nums.size()/13, 1);
		res = mgl_data_ifs_3d(dat, n, skip);
	}
	else
	{
		dat->Set(&(nums[0]), 7, nums.size()/7, 1);
		res = mgl_data_ifs_2d(dat, n, skip);
	}
	delete dat;	free(buf);	return res;
}
uintptr_t mgl_data_ifs_file_(const char *fname, const char *name, long *n, long *skip,int l,int m)
{	char *s=new char[l+1];		memcpy(s,fname,l);	s[l]=0;
	char *t=new char[m+1];		memcpy(t,name,m);	t[m]=0;
	uintptr_t r = uintptr_t(mgl_data_ifs_file(s,t,*n,*skip));
	delete []s;	delete []t;		return r;	}
//-----------------------------------------------------------------------------
//
//	Functions for flame fractal
//
//-----------------------------------------------------------------------------
void static mgl_linear_var0(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	xNew += par[0]*x;	yNew += par[0]*y;	}
void static mgl_sinusoidal_var1(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	xNew += par[0]*sin(x);	yNew += par[0]*sin(y);	}
void static mgl_spherical_var2(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	mreal c1 = par[0]/(x*x+y*y);	xNew += c1*x;	yNew += c1*y;	}
void static mgl_swirl_var3(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r2=x*x+y*y, c1=sin(r2), c2=cos(r2);
	xNew += par[0]*(x*c1 - y*c2);
	yNew += par[0]*(x*c2 + y*c1);
}
void static mgl_horseshoe_var4(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]/hypot(x,y);
	xNew += c1*(x*x-y*y);
	yNew += 2*c1*x*y;
}
void static mgl_polar_var5(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*atan2(x,y)/M_PI;
	yNew += par[0]*(hypot(x,y)-1);
}
void static mgl_handkerchief_var6(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), t=atan2(x,y), c1=par[0]*r;
	xNew += c1*sin(t+r);	yNew += c1*cos(t-r);
}
void static mgl_heart_var7(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), c1=par[0]*r, c2=atan2(x,y)*r;
	xNew +=  c1*sin(c2);	yNew -= c1*cos(c2);
}
void static mgl_disc_var8(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*atan2(x,y)/M_PI, c2=M_PI*hypot(x,y);
	xNew += c1*sin(c2);		yNew += c1*cos(c2);
}
void static mgl_spiral_var9(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), t=atan2(x,y), c1=par[0]/r;
	xNew += c1*(cos(t)+sin(r));
	yNew += c1*(sin(t)-cos(r));
}
void static mgl_hyperbolic_var10(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), t=atan2(x,y);
	xNew += par[0]*sin(t)/r;
	yNew += par[0]*r*cos(t);
}
void static mgl_diamond_var11(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), t=atan2(x,y);
	xNew += par[0]*sin(t)*cos(r);
	yNew += par[0]*cos(t)*sin(r);
}
void static mgl_ex_var12(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), t=atan2(x,y), c1=par[0]*r;
	mreal c2=mgl_ipow(sin(t+r),3), c3 = mgl_ipow(cos(t-r), 3);
	xNew += c1*(c2 + c3);	yNew += c1*(c2 - c3);
}
void static mgl_julia_var13(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*sqrt(hypot(x,y)), c2=atan2(x,y)/2, c3=(rand()%2)*M_PI;
	xNew += c1*cos(c2+c3);	yNew += c1*sin(c2+c3);
}
void static mgl_bent_var14(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	if(x>=0 && y>=0)
	{	xNew += par[0]*x;	yNew += par[0]*y;	}
	else if(x<0 && y>=0)
	{	xNew += par[0]*2*x;	yNew += par[0]*y;	}
	else if(x>=0 && y<0)
	{	xNew += par[0]*x;	yNew += par[0]*y/2;	}
	else
	{	xNew += par[0]*2*x;	yNew += par[0]*y/2;	}
}
void static mgl_waves_var15(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	// NOTE: par[1]=b[i], par[2]=1/c[i]^2, par[3]=e[i], par[4]=1/f[i]^2
	xNew += par[0]*(x + par[1]*sin(y*par[2]));
	yNew += par[0]*(y + par[3]*sin(x*par[4]));
}
void static mgl_fisheye_var16(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*2/(hypot(x,y) + 1);
	xNew += c1*y;	yNew += c1*x;
}
void static mgl_popcorn_var17(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	// NOTE: par[1]=c[i], par[2]=f[i]
	xNew += par[0]*(x + par[1]*sin(tan(3*y)));
	yNew += par[0]*(y + par[2]*sin(tan(3*x)));
}
void static mgl_exponential_var18(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*exp(x-1);
	xNew += c1*cos(M_PI*y);	yNew += c1*sin(M_PI*y);
}
void static mgl_power_var19(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal t=atan2(x,y), c1=par[0]*pow(hypot(x,y), sin(t));
	xNew += c1*cos(t);	yNew += c1*sin(t);
}
void static mgl_cosine_var20(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*cos(M_PI*x)*cosh(y);
	yNew -= par[0]*sin(M_PI*x)*sinh(y);
}
void static mgl_rings_var21(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	// NOTE: par[1]=c[i]^2
	mreal t=atan2(x,y), r=hypot(x,y), c1=par[0]*(fmod(r+par[1],2*par[1])-par[1]+r*(1-par[1])); // convert to int?
	xNew += c1*cos(t);	yNew += c1*sin(t);
}
void static mgl_fan_var22(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	// NOTE: par[1]=c[i]^2, par[2]=f[i]
	mreal t=atan2(x,y), c1=par[0]*hypot(x,y), c2;
	c2 = fmod(t+par[2], M_PI*par[1]); // convert to int?
	if(c2>M_PI_2*par[1])	c2 = t - M_PI_2*par[1];
	else	c2 += t;
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_blob_var23(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal t=atan2(x,y), c1=par[0]*hypot(x,y)*(par[2]+(par[1]-par[2])/2*(sin(par[3]*t)));
	xNew += c1*cos(t);	yNew += c1*sin(t);
}
void static mgl_pdj_var24(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*(sin(par[1]*y) - cos(par[2]*x));
	yNew += par[0]*(sin(par[3]*x) - cos(par[4]*y));
}
void static mgl_fan2_var25(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal t=atan2(x,y), c1, c2;
	c1 = M_PI*par[1]*par[1];
	c2 = t + par[2] - c1*int(2*t*par[2]/c1);
	c1 /= 2;	c2 = c2>c1?t-c1:t+c1;
	c1 = par[0]*hypot(x,y);
	xNew += c1*sin(c2);	yNew += c1*cos(c2);
}
void static mgl_rings2_var26(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), t=atan2(x,y), c1=par[1]*par[1];
	c1 = par[0]*(r - 2*c1*int((r+c1)/(2*c1)) + r*(1-c1));
	xNew += c1*cos(t);	yNew += c1*sin(t);
}
void static mgl_eyefish_var27(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*2/(hypot(x,y)+1);
	xNew += c1*x;		yNew += c1*y;
}
void static mgl_bubble_var28(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*4/(x*x+y*y+4);
	xNew += c1*x;		yNew += c1*y;
}
void static mgl_cylinder_var29(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	xNew += par[0]*sin(x);	yNew += par[0]*y;	}
void static mgl_perspective_var30(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*par[2]/(par[2]-y*sin(par[1]));
	xNew += c1*x;	yNew += c1*y*cos(par[1]);
}
void static mgl_noise_var31(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*mgl_rnd(), c2=2*M_PI*mgl_rnd();
	xNew += c1*x*cos(c2);	yNew += c1*y*sin(c2);
}
void static mgl_juliaN_var32(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=int(fabs(par[1])*mgl_rnd()), c2;
	c2 = (atan2(y,x) + 2*M_PI*c1)/par[1];
	c1 = par[0]*pow(hypot(x,y), par[2]/par[1]);
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_juliaScope_var33(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=int(fabs(par[1])*mgl_rnd()), c2;
	c2 = ((2*(rand()%2)-1)*atan2(y,x) + 2*M_PI*c1)/par[1];
	c1 = par[0]*pow(hypot(x,y), par[2]/par[1]);
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_blur_var34(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*mgl_rnd(), c2=2*M_PI*mgl_rnd();
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_gaussian_var35(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*(4*mgl_rnd()-2), c2=2*M_PI*mgl_rnd();
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_radialBlur_var36(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal r=hypot(x,y), c1=par[1]*M_PI_2, c2=par[0]*(4*mgl_rnd()-2), c3;
	c3 = c2*cos(c1) - 1;	c2 = atan2(y,x) + c2 *sin(c1);
	xNew += r*cos(c2) + c3*x;	yNew += r*sin(c2) + c3*y;
}
void static mgl_pie_var37(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=int(mgl_rnd()*par[1] + 0.5), c2;
	c2 = par[2] + 2*M_PI/par[1]*(c1 + mgl_rnd()*par[3]);
	c1 = par[0]*mgl_rnd();
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_ngon_var38(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=2*M_PI/par[2], c2;
	c2 = atan2(y,x) - c1*floor(atan2(y,x)/c1);
	if(c2 <= c1/2)	c2 -= c1;
	c1 = par[0]*(par[3]*(1/cos(c2) - 1) + par[4])/pow(hypot(x,y), par[1]);
	xNew += c1*x;	yNew += c1*y;
}
void static mgl_curl_var39(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=1 + par[1]*x + par[2]*(x*x - y*y);
	mreal c2 = par[1]*y + 2*par[2]*x*y;
	mreal c3 = par[0]/(c1*c1 + c2*c2);
	xNew += c3*(c1*x + c2*y);	yNew += c3*(c1*x - c2*y);
}
void static mgl_rectangles_var40(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*((2*floor(x/par[1]) + 1)*par[1] - x);
	yNew += par[0]*((2*floor(y/par[2]) + 1)*par[2] - y);
}
void static mgl_arch_var41(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=mgl_rnd()*M_PI*par[0], c2=sin(c1);
	xNew += par[0]*c2;	yNew += par[0]*c2*c2/cos(c1);
}
void static mgl_tangent_var42(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	xNew += par[0]*sin(x)/cos(y);	yNew += par[0]*tan(y);	}
void static mgl_square_var43(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*(mgl_rnd() - 0.5);
	yNew += par[0]*(mgl_rnd() - 0.5);
}
void static mgl_blade_var44(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*x, c2=mgl_rnd()*hypot(x,y)*par[0];
	xNew += c1*(cos(c2) + sin(c2));	// TODO check use of c2
	yNew += c1*(cos(c2) - sin(c2));
}
void static mgl_secant_var45(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	xNew += par[0]*x;	yNew += 1/cos(par[0]*hypot(x,y));	}
void static mgl_rays_var46(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*par[0]*tan(mgl_rnd()*M_PI*par[0])/(x*x+y*y);
	xNew += c1*cos(x);	yNew += c1*sin(y);
}
void static mgl_twintrian_var47(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=par[0]*x, c2, c3;
	c2 = mgl_rnd()*hypot(x,y)*par[0];
	c3 = log10(sin(c2)*sin(c2)) + cos(c2);
	xNew += c1*c3;	yNew += c1*(c3 - M_PI*sin(c2));
}
void static mgl_cross_var48(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]/fabs(x*x - y*y);
	xNew += c1*x;	yNew += c1*y;
}
void static mgl_disc2_var49(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = cos(par[2])-1, c2 = sin(par[2]), c3;
	if(par[2]>2*M_PI)
	{
		c3 = 1+par[2]-2*M_PI;
		c1 *= c3;   c2 *= c3;
	}
	else if(par[2]<-2*M_PI)
	{
		c3 = 1+par[2]+2*M_PI;
		c1 *= c3;   c2 *= c3;
	}
	c3 = par[1]*M_PI*(x+y);
	mreal a = par[0]*atan2(x,y)/M_PI;
	xNew += a*(c1+sin(c3)); yNew += a*(c2+cos(c3));
}
void static mgl_supershape_var50(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[2]/4*atan2(y,x)+M_PI_4, r = hypot(x,y);
	c1 = pow(fabs(sin(c1)), par[5])+pow(fabs(cos(c1)), par[4]);
	c1 = par[0]*((par[1]*mgl_rnd()+(1-par[1])*r)-par[6])*pow(c1, -1.0/par[3])/r;
	xNew += c1*x;	yNew += c1*y;
}
void static mgl_flower_var51(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*(mgl_rnd()-par[2])*cos(par[1]*atan2(y,x))/hypot(x,y);
	xNew += c1*x;	yNew += c1*y;
}
void static mgl_conic_var52(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = x/hypot(x,y);
	c1 = par[0]*(mgl_rnd()-par[2])*par[1]/(1+par[1]*c1)/hypot(x,y);
	xNew += c1*x;	yNew += c1*y;
}
void static mgl_parabola_var53(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = hypot(x,y), c2;
	c2 = cos(c1);	c1 = sin(c1);
	xNew += par[0]*par[1]*c1*c1 *mgl_rnd();
	yNew += par[0]*par[2]*c2*mgl_rnd();
}
void static mgl_bent2_var54(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*x, c2 = par[0]*y;
	if(x<0.0)	c1 *= par[1];
	if(y<0.0)	c2 *= par[2];
	xNew += c1;		yNew += c2;
}
void static mgl_bipolar_var55(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 0.5*atan2(2*y, x*x+y*y-1)-M_PI_2*par[1], c2, c3, c4;
	if(c1>M_PI_2)	c1 = -M_PI_2+fmod(c1+M_PI_2, M_PI);
	else if(y<-M_PI_2)   c1 = M_PI_2-fmod(M_PI_2-y, M_PI);
	c2 = par[0]*M_2_PI;
	c3 = x*x+y*y+1;	c4 = 2*x;
	xNew += 0.25*c2*log((c3+c4)/(c3-c4));	yNew += c1*c2;
}
void static mgl_boarders_var56(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = round(x), c2 = round(y), c3, c4;
	c3 = x-c1;	c4 = y-c2;
	if(mgl_rnd()>=0.75)
	{
		xNew += par[0]*(c1+0.5*c3);
		yNew += par[0]*(c2+0.5*c4);
	}
	else
	{
		if(fabs(c3)>=fabs(c4))
		{
			if(c3>=0)
			{
				xNew += par[0]*(c1+0.5*c3+0.25);
				yNew += par[0]*(c2+0.5*c4+0.25*c4/c3);
			}
			else
			{
				xNew += par[0]*(c1+0.5*c3-0.25);
				yNew += par[0]*(c2+0.5*c4-0.25*c4/c3);
			}
		}
		else
		{
			if(c4>=0)
			{
				xNew += par[0]*(c2+0.5*c4+0.25);
				yNew += par[0]*(c1+0.5*c3+0.25*c3/c4);
			}
			else
			{
				xNew += par[0]*(c2+0.5*c4-0.25);
				yNew += par[0]*(c1+0.5*c3-0.25*c3/c4);
			}
		}
	}
}
void static mgl_butterfly_var57(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 4/sqrt(3*M_PI)*par[0], c2 = 2*y; // replace 4/sqrt(3*M_PI) for the result?
	c1 *= sqrt(fabs(x*y)/(x*x+c2*c2));
	xNew += c1*x; yNew += c1*c2;
}
void static mgl_cell_var58(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=floor(x/par[1]), c2=floor(y/par[1]), c3=x-c1*par[1], c4=y-c2*par[1];
	if(c2>=0)
	{
		if(c1>=0)	{	c1 *= 2;	c2 *= 2;	}
		else		{	c1=-2*c1-1;	c2 *= 2;	}
	}
	else
	{
		if(c1>=0)	{	c1 *= 2;	c2=-2*c2-1;	}
		else		{	c1=-2*c1-1;	c2=-2*c2-1;	}
	}
	xNew += par[0]*(par[1]*c1+c3);
	yNew += par[0]*(par[1]*c2+c4);
}
void static mgl_cpow_var59(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = atan2(y, x), c2, c3, c4, c5;
	c2 = par[1]/par[3];
	c3 = par[2]/par[3];
	c4 = 0.5*log(x*x+y*y);
	c5 = c1*c2+c3*c4+2*M_PI/par[3]*floor(par[3]*mgl_rnd());
	c1 = par[0]*exp(c2*c4-c1*c3);
	c2 = cos(c5);
	c3 = sin(c5);
	xNew += c1*c2;	yNew += c1*c3;
}
void static mgl_curve_var60(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*(x+par[1]*exp(-(y*y)/(par[3]*par[3])));
	yNew += par[0]*(y+par[2]*exp(-(x*x)/(par[4]*par[4])));
}
void static mgl_edisc_var61(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = x*x+y*y+1, c2,c3,c4,c5;
	c2 = 2*x;
	c3 = sqrt(c1-c2);
	c1 = sqrt(c1+c2);
	c1 = (c1+c3)*0.5;
	c2 = log(c1+sqrt(c1-1));
	c3 = -acos(x/c1);
	c1 = par[0]/11.57034632;
	c4 = cos(c2)*cosh(c3);
	c5 = sin(c2)*sinh(c3);
	xNew += c1*c4;	yNew += c1*c5;
}
void static mgl_elliptic_var62(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = x*x+y*y+1, c2, c3, c4, c5;
	c2 = 2*x;
	c2 = 0.5*(sqrt(c1+c2)+sqrt(c1-c2));
	c3 = x/c2;
	c4 = 1-c3*c3;
	c5 = c2-1;
	c1 = par[0]/M_PI_2;
	if(c4<0)	c4 = 0;
	else	c4 = sqrt(c4);
	if(c5<0)	c5 = 0;
	else	c5 = sqrt(c5);
	xNew += c1*atan2(c3, c4);
	if(y>0)	yNew += c1*log(c2+c5);
	else	yNew -= c1*log(c2+c5);
}
void static mgl_escher_var63(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 0.5*(1+cos(par[1])), c2 = 0.5*sin(par[1]), c3, c4, c5;
	c3 = 0.5*log(x*x+y*y);	c4 = atan2(y, x);
	c5 = c1*c4+c2*c3;		c1 = par[0]*exp(c1*c3-c2*c4);
	xNew += c1*cos(c5);	yNew += c1*sin(c5);
}
void static mgl_foci_var64(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1=0.5*exp(x), c2=0.25/c1, c3=par[0]/(c1+c2-cos(y));	// TODO Check this!!!
	xNew += c1*(c2-c3);	yNew += c1*sin(y);
}
void static mgl_lazySusan_var65(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = hypot(x, y), c2;
	if(c1<par[0])
	{
		c2 = atan2(y+par[5], x-par[4])+par[1]+par[3]*(par[0]-c1);
		c1 *= par[0];
		xNew += c1*cos(c2)+par[4];
		yNew += c1*sin(c2)-par[5];
	}
	else
	{
		c1 = par[0]*(1+par[2]/c1);
		xNew += c1*(x-par[4])+par[4];
		yNew += c1*(y+par[5])-par[5];
	}
}
void static mgl_loonie_var66(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = x*x+y*y, c2 = par[0]*par[0];
	if(c1<c2)
	{
		c1 = par[0]*sqrt(c2/c1-1);
		xNew += c1*x;	yNew += c1*y;
	}
	else	{	xNew += par[0]*x;	yNew += par[0]*y;	}
}
void static mgl_preBlur_var67(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*(mgl_rnd()+mgl_rnd()+mgl_rnd()+mgl_rnd()-2), c2;
	c2 = 2*mgl_rnd()*M_PI;
	x += c1*cos(c2);	y += c1*sin(c2); // NOTE: This changes the original coordinates, not the new ones
}
void static mgl_modulus_var68(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	if(x>par[1])	xNew += par[0]*(-par[1]+fmod(x-par[1], 2*par[1]));
	else if(x<par[1])	xNew += par[0]*(par[1]-fmod(par[1]-x, 2*par[1]));
	else	xNew += par[0]*x;
	if(y>par[2])	yNew += par[0]*(-par[2]+fmod(y+par[2], 2*par[2]));
	else if(y<par[2])	yNew += par[0]*(par[2]-fmod(par[2]-y, 2*par[2]));
	else	yNew += par[0]*y;
}
void static mgl_oscope_var69(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[3]*exp(-fabs(x)*par[4])*cos(2*M_PI*par[2]*x)+par[1];
	if(fabs(y) <= c1)
	{	xNew += par[0]*x;	yNew -= par[0]*y;	}
	else
	{	xNew += par[0]*x;	yNew += par[0]*y;	}
}
void static mgl_polar2_var70(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal t = atan2(x, y);
	xNew += par[0]*t*t;	yNew += par[0]*t/2*log(x*x+y*y);
}
void static mgl_popcorn2_var71(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*(x+par[1]*sin(tan(par[3]*y)));
	yNew += par[0]*(y+par[2]*sin(tan(par[3]*x)));
}
void static mgl_scry_var72(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 1/(hypot(x, y)*(x*x+y*y+1/par[0]));
	xNew += c1*x;	yNew += c1*y;
}
void static mgl_separation_var73(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	if(x>0)	xNew += par[0]*(sqrt(x*x+par[1]*par[1])-x*par[2]);
	else	xNew -= par[0]*(sqrt(x*x+par[1]*par[1])+x*par[2]);
	if(y>0)	yNew += par[0]*(sqrt(y*y+par[3]*par[3])-y*par[4]);
	else	yNew -= par[0]*(sqrt(y*y+par[3]*par[3])+y*par[4]);
}
void static mgl_split_var74(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	if(cos(M_PI*x*par[1])>=0)	xNew += par[0]*y;
	else	xNew -= par[0]*y;
	if(cos(M_PI*y*par[2])>=0)	yNew += par[0]*x;
	else	yNew -= par[0]*x;
}
void static mgl_splits_var75(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	if(x>=0)	xNew+= par[0]*(x+par[1]);
	else 	xNew += par[0]*(x-par[1]);
	if(y>=0)	yNew += par[0]*(y+par[2]);
	else	yNew += par[0]*(y-par[2]);
}
void static mgl_stripes_var76(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = floor(x+0.5), c2 = x-c1;
	xNew += par[0]*(c2*(1-par[1])+c1);
	yNew += par[0]*(y+c2*c2*par[2]);
}
void static mgl_wedge_var77(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = hypot(x, y), c2, c3, c4;
	c2 = atan2(y, x)+par[4]*c1;
	c3 = 1-0.5*M_1_PI*par[1]*par[3];
	c4 = floor(0.5*M_1_PI*(par[2]*c2+M_PI));
	c2 = c2*c3+c4*par[1];
	c1 = par[0]*(c1+par[2]);
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_wedgeJulia_var78(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = int(fabs(par[3])*mgl_rnd()), c2;
	c1 = (atan2(y, x)+2*M_PI*c1)/par[3];
	c2 = floor(0.5*M_1_PI*(par[2]*c1+M_PI));
	c2 = c1*(1-0.5*M_1_PI*par[1]*par[2]+c1*par[1]);	// TODO Check this!!!
	c1 = par[0]*pow(x*x +y*y, par[4]/(2*par[3]));
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_wedgeSph_var79(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 1/hypot(x, y), c2, c3, c4;
	c2 = atan2(y, x)+par[4]*c1;
	c3 = 1-0.5*M_1_PI*par[1]*par[2];
	c4 = floor(0.5*M_1_PI*(par[2]*c2+M_PI));
	c2 = c2*c3+c4*par[1];
	c1 = par[0]*(c1+par[3]);
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_whorl_var80(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = hypot(x, y), c2;
	if(c1<par[0])	c2 = atan2(y, x)+par[1]/(par[0]-c1);
	else	c2 = atan2(y, x)+par[2]/(par[0]-c1);
	c1 *= par[0];
	xNew += c1*cos(c2);	yNew += c1*sin(c2);
}
void static mgl_waves2_var81(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*(x+par[2]*sin(y*par[1]));
	yNew += par[0]*(y+par[4]*sin(x*par[3]));
}
void static mgl_exp_var82(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]*exp(x);
	xNew += c1*cos(y);	yNew += c1*sin(y);
}
void static mgl_log_var83(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*0.5*log(x*x+y*y);	yNew += par[0]*atan2(y, x);
}
void static mgl_sin_var84(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*sin(x)*cosh(y);	yNew += par[0]*cos(x)*sinh(y);
}
void static mgl_cos_var85(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	xNew += par[0]*cos(x)*cosh(y);	yNew -= par[0]*sin(x)*sinh(y);
}
void static mgl_tan_var86(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]/(cos(2*x)+cosh(2*y));
	xNew += c1*sin(2*x);	yNew += c1*sinh(2*y);
}
void static mgl_sec_var87(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 2*par[0]/(cos(2*x)+cosh(2*y));
	xNew += c1*cos(x)*cosh(y);	yNew += c1*sin(x)*sinh(y);
}
void static mgl_csc_var88(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 2*par[0]/(cosh(2*y)-cos(2*x));
	xNew += c1*sin(x)*cosh(y);	yNew -= c1*cos(x)*sinh(y);
}
void static mgl_cot_var89(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]/(cosh(2*y)-cos(2*x));
	xNew += c1*sin(2*x);	yNew -= c1*sinh(2*y);
}
void static mgl_sinh_var90(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	xNew += par[0]*sinh(x)*cos(y);	yNew += par[0]*cosh(y)*sin(y);	}
void static mgl_cosh_var91(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{	xNew += par[0]*cosh(x)*cos(y);	yNew += par[0]*sinh(x)*sin(y);	}
void static mgl_tanh_var92(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]/(cos(2*y)+cosh(2*x));
	xNew += c1*sinh(2*x);	yNew += c1*sin(2*y);
}
void static mgl_sech_var93(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 2*par[0]/(cos(2*y)+cosh(2*x));
	xNew += c1*cos(y)*cosh(x);	yNew -= c1*sin(y)*sinh(x);
}
void static mgl_csch_var94(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = 2*par[0]/(cosh(2*x)-cos(2*y));
	xNew += c1*sinh(x)*cos(y);	yNew -= c1*cosh(x)*sin(y);
}
void static mgl_coth_var95(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = par[0]/(cosh(2*x)-cos(2*y));
	xNew += c1*sinh(2*x);	yNew += c1*sin(2*y);
}
void static mgl_auger_var96(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = sin(par[3]*y), c2 = sin(par[3]*x);
	c1 = x+par[2]*(par[4]*c1/2+fabs(x)*c1);
	c2 = y+par[2]*(par[4]*c2/2+fabs(y)*c2);
	xNew += par[0]*(x+par[1]*(c1-x));	yNew += par[0]*c2;
}
void static mgl_flux_var97(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par)
{
	mreal c1 = x+par[0], c2 = x-par[0], c3;
	c3 = 0.5*(atan2(y, c2)-atan2(y, c1));
	c1 = par[0]*(2+par[1])*sqrt(sqrt(y*y+c1*c1)/sqrt(y*y-c2*c2));
	xNew += c1*cos(c3);	yNew += c1*sin(c3);
}
//-----------------------------------------------------------------------------
typedef void (*flame_func)(mreal &xNew, mreal &yNew, mreal x, mreal y, const mreal *par);
static flame_func ffunc[mglFlame2dLAST] = {
	mgl_linear_var0,	mgl_sinusoidal_var1,	mgl_spherical_var2,	mgl_swirl_var3,		mgl_horseshoe_var4,
	mgl_polar_var5,		mgl_handkerchief_var6,	mgl_heart_var7,		mgl_disc_var8,		mgl_spiral_var9,
	mgl_hyperbolic_var10,	mgl_diamond_var11,	mgl_ex_var12,		mgl_julia_var13,	mgl_bent_var14,
	mgl_waves_var15,		mgl_fisheye_var16,	mgl_popcorn_var17,	mgl_exponential_var18,	mgl_power_var19,
	mgl_cosine_var20,		mgl_rings_var21,	mgl_fan_var22,		mgl_blob_var23,		mgl_pdj_var24,
	mgl_fan2_var25,			mgl_rings2_var26,	mgl_eyefish_var27,	mgl_bubble_var28,	mgl_cylinder_var29,
	mgl_perspective_var30,	mgl_noise_var31,	mgl_juliaN_var32,	mgl_juliaScope_var33,	mgl_blur_var34,
	mgl_gaussian_var35,	mgl_radialBlur_var36,	mgl_pie_var37,		mgl_ngon_var38,		mgl_curl_var39,
	mgl_rectangles_var40,	mgl_arch_var41,		mgl_tangent_var42,	mgl_square_var43,	mgl_blade_var44,
	mgl_secant_var45,		mgl_rays_var46,		mgl_twintrian_var47,mgl_cross_var48,	mgl_disc2_var49,
	mgl_supershape_var50,	mgl_flower_var51,	mgl_conic_var52,	mgl_parabola_var53,	mgl_bent2_var54,
	mgl_bipolar_var55,		mgl_boarders_var56,	mgl_butterfly_var57,mgl_cell_var58,		mgl_cpow_var59,
	mgl_curve_var60,		mgl_edisc_var61,	mgl_elliptic_var62,	mgl_escher_var63,	mgl_foci_var64,
	mgl_lazySusan_var65,	mgl_loonie_var66,	mgl_preBlur_var67,	mgl_modulus_var68,	mgl_oscope_var69,
	mgl_polar2_var70,		mgl_popcorn2_var71,	mgl_scry_var72,		mgl_separation_var73,	mgl_split_var74,
	mgl_splits_var75,		mgl_stripes_var76,	mgl_wedge_var77,	mgl_wedgeJulia_var78,	mgl_wedgeSph_var79,
	mgl_whorl_var80,		mgl_waves2_var81,	mgl_exp_var82,		mgl_log_var83,		mgl_sin_var84,
	mgl_cos_var85,			mgl_tan_var86,		mgl_sec_var87,		mgl_csc_var88,		mgl_cot_var89,
	mgl_sinh_var90,			mgl_cosh_var91,		mgl_tanh_var92,		mgl_sech_var93,		mgl_csch_var94,
	mgl_coth_var95,			mgl_auger_var96,	mgl_flux_var97};
//-----------------------------------------------------------------------------
long static mgl_flame_2d_point(HCDT A, HCDT F, mreal& x, mreal& y, mreal amax)
{
	long i, n=A->GetNy(), m=F->GetNy(), last_func=0, l=F->GetNx();
	l = l>6?6:l;
	mreal r = amax*mgl_rnd(), sum_prob = 0, x1, y1;
	for(i=0;i<n;i++)
	{
		sum_prob += A->v(6,i);
		if(r<sum_prob)	break;
	}
	x1 = A->v(0,i)*x+A->v(1,i)*y+A->v(4,i);
	y1 = A->v(2,i)*x+A->v(3,i)*y+A->v(5,i);
	x = y = 0;
	for(long j=0;j<m;j++)
	{
		int v=int(F->v(0,j,i)+0.5);
		mreal par[5] = {F->v(1,j,i),0,0,0,0};
		for(int k=2;k<l;k++)	par[k-1]=F->v(k,j,i);
		if(v<0 || v>=mglFlame2dLAST)	{	v=0;	par[0]=1;	}
		ffunc[v](x,y,x1,y1,par);	last_func=v;
	}
	return last_func;
}
HMDT MGL_EXPORT mgl_data_flame_2d(HCDT A, HCDT F, long n, long skip)
{
	if(!A || A->GetNx()<7 || n<1)	return 0;	// incompatible dimensions
	if(!F || F->GetNx()<2 || F->GetNz()!=A->GetNy())	return 0;	// incompatible dimensions
	mreal amax=0;
	for(long i=0; i<A->GetNy(); i++)	amax += A->v(6,i);
	if(amax<=0)	return 0;

	mglData *f = new mglData(3,n);
	mreal x = 0, y = 0;
	for(long i=0; i<skip; i++)	mgl_flame_2d_point(A, F, x, y,amax);
	for(long i=0; i<n; i++)
	{
		f->a[3*i+2] = mgl_flame_2d_point(A, F, x, y, amax);	// TODO color information ?!!
		f->a[3*i] = x;	f->a[3*i+1] = y;
	}
	return f;
}
uintptr_t MGL_EXPORT mgl_data_flame_2d_(uintptr_t *d, uintptr_t *f, long *n, long *skip)
{	return uintptr_t(mgl_data_flame_2d(_DT_,_DA_(f),*n,*skip));	}
//-----------------------------------------------------------------------------
