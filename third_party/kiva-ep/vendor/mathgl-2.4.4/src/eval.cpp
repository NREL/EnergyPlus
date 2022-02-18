/***************************************************************************
 * eval.cpp is part of Math Graphic Library
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
#include <time.h>
#include <ctype.h>

#include "mgl2/data.h"
#include "mgl2/eval.h"

#if MGL_HAVE_GSL
#include <gsl/gsl_sf.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_errno.h>
#include <sys/stat.h>
#endif
//-----------------------------------------------------------------------------
//	constants for expression parsing
enum{
EQ_NUM=0,	// a variable substitution
EQ_RND,		// random number
EQ_A,		// numeric constant
// normal functions of 2 arguments
EQ_LT,		// comparison x<y			!!! MUST BE FIRST 2-PLACE FUNCTION
EQ_GT,		// comparison x>y
EQ_EQ,		// comparison x=y
EQ_OR,		// comparison x|y
EQ_AND,		// comparison x&y
EQ_ADD,		// addition x+y
EQ_SUB,		// substraction x-y
EQ_MUL,		// multiplication x*y
EQ_DIV,		// division x/y
EQ_IPOW,		// power x^n for integer n
EQ_POW,		// power x^y
EQ_MOD,		// x modulo y
EQ_LOG,		// logarithm of x on base a, log_a(x) = ln(x)/ln(a)
EQ_ARG,		// argument of complex number arg(x,y) = atan2(x,y)
EQ_HYPOT,	// sqrt(x^2+y^2)=hypot(x,y)
EQ_MAX,		// maximum of x and y
EQ_MIN,		// minimum of x and y
// special functions of 2 arguments
EQ_BESJ,		// regular cylindrical Bessel function of fractional order
EQ_BESY,		// irregular cylindrical Bessel function of fractional order
EQ_BESI,		// regular modified Bessel function of fractional order
EQ_BESK,		// irregular modified Bessel function of fractional order
EQ_ELE,		// elliptic integral E(\phi,k) = \int_0^\phi dt   \sqrt((1 - k^2 \sin^2(t)))
EQ_ELF,		// elliptic integral F(\phi,k) = \int_0^\phi dt 1/\sqrt((1 - k^2 \sin^2(t)))
EQ_LP,		// Legendre polynomial P_l(x), (|x|<=1, l>=0)
EQ_BETA,	// beta function B(x,y) = Gamma(x)*Gamma(y)/Gamma(x+y)
EQ_GAMMA_INC,	// incomplete gamma function Gamma(a,x) = \int_x^\infty dt t^{a-1} \exp(-t) for x>=0.
// normal functions of 1 argument
EQ_SIN,		// sine function \sin(x).			!!! MUST BE FIRST 1-PLACE FUNCTION
EQ_COS,		// cosine function \cos(x).
EQ_TAN,		// tangent function \tan(x).
EQ_ASIN,		// inverse sine function \sin(x).
EQ_ACOS,		// inverse cosine function \sin(x).
EQ_ATAN,		// inverse tangent function \tan(x).
EQ_SINH,		// hyperbolic sine function \sin(x).
EQ_COSH,		// hyperbolic cosine function \sin(x).
EQ_TANH,		// hyperbolic tangent function \tan(x).
EQ_ASINH,	// inverse hyperbolic sine function \sin(x).
EQ_ACOSH,	// inverse hyperbolic cosine function \sin(x).
EQ_ATANH,	// inverse hyperbolic tangent function \tan(x).
EQ_SQRT,		// square root function \sqrt(x)
EQ_EXP,		// exponential function \exp(x)
EQ_LN,		// logarithm of x, ln(x)
EQ_LG,		// decimal logarithm of x, lg(x) = ln(x)/ln(10)
EQ_SIGN,	// sign of number
EQ_STEP,		// step function
EQ_INT,		// integer part [x]
EQ_ABS,		// absolute value of x
// special functions of 1 argument
EQ_LI2,		// dilogarithm for a real argument Li2(x) = - \Re \int_0^x ds \log(1-s)/s.
EQ_ELLE,		// complete elliptic integral is denoted by E(k) = E(\pi/2, k).
EQ_ELLK,		// complete elliptic integral is denoted by K(k) = F(\pi/2, k).
EQ_AI,		// Airy function Ai(x)
EQ_BI,		// Airy function Bi(x)
EQ_ERF,		// error function erf(x) = (2/\sqrt(\pi)) \int_0^x dt \exp(-t^2).
EQ_EI3,		// exponential integral Ei_3(x) = \int_0^x dt \exp(-t^3) for x >= 0.
EQ_EI,		// exponential integral Ei(x),  Ei(x) := - PV(\int_{-x}^\infty dt \exp(-t)/t), where PV denotes the principal value of the integral.
EQ_E1,		// exponential integral E_1(x), E_1(x) := Re \int_1^\infty dt \exp(-xt)/t.
EQ_E2,		// exponential integral E_2(x), E_2(x) := Re \int_1^\infty dt \exp(-xt)/t^2.
EQ_SI,		// Sine integral Si(x) = \int_0^x dt \sin(t)/t.
EQ_CI,		// Cosine integral Ci(x) = \int_0^x dt \cos(t)/t.
EQ_GAMMA,	// Gamma function \Gamma(x) = \int_0^\infty dt  t^{x-1} \exp(-t)
EQ_PSI,		// digamma function \psi(x) = \Gamma'(x)/\Gamma(x) for general x, x \ne 0.
EQ_W0,		// principal branch of the Lambert W function, W_0(x). Functions W(x), are defined to be solutions of the equation W\exp(W) = x.
EQ_W1,		// secondary real-valued branch of the Lambert W function, W_{-1}(x). Functions W(x), are defined to be solutions of the equation W\exp(W) = x.
EQ_SINC,		// compute \sinc(x) = \sin(\pi x) / (\pi x) for any value of x.
EQ_ZETA,		// Riemann zeta function \zeta(s) = \sum_{k=1}^\infty k^{-s}for arbitrary s, s \ne 1.
EQ_ETA,		// eta function \eta(s) = (1-2^{1-s}) \zeta(s) for arbitrary s.
EQ_AID,		// Derivative of Airy function Ai(x)
EQ_BID,		// Derivative of Airy function Bi(x)
EQ_Z,		// Dawson function \exp(-x^2) \int_0^x dt \exp(t^2)
// Jacoby functions of 2 arguments
EQ_SN,		// Jacobian elliptic function sn(u|m)	// !!! MUST BE FIRST NON 1-PLACE FUNCTION
EQ_SC,		// Jacobian elliptic function sn(u|m)/cn(u|m)
EQ_SD,		// Jacobian elliptic function sn(u|m)/dn(u|m)
EQ_NS,		// Jacobian elliptic function 1/sn(u|m)
EQ_NC,		// Jacobian elliptic function 1/cn(u|m)
EQ_ND,		// Jacobian elliptic function 1/dn(u|m)
EQ_CN,		// Jacobian elliptic function cn(u|m)
EQ_CS,		// Jacobian elliptic function cn(u|m)/sn(u|m)
EQ_CD,		// Jacobian elliptic function cn(u|m)/dn(u|m)
EQ_DN,		// Jacobian elliptic function dn(u|m)
EQ_DS,		// Jacobian elliptic function dn(u|m)/sn(u|m)
EQ_DC,		// Jacobian elliptic function dn(u|m)/cn(u|m)
			// MUST BE LAST ELLIPTIC FUNCTION
// not-ready
EQ_EN,
EQ_CL		// Clausen function
};
//-----------------------------------------------------------------------------
#ifndef M_PI
#define M_PI       3.14159265358979323846
#endif
//-----------------------------------------------------------------------------
int mglFormula::Error=0;
bool MGL_LOCAL_PURE mglCheck(char *str,int n);
int MGL_LOCAL_PURE mglFindInText(const char *str, const char *lst);
//-----------------------------------------------------------------------------
#if MGL_HAVE_GSL
MGL_NO_EXPORT gsl_rng *mgl_rng=0;	// NOTE: should be deleted by gsl_rng_free() but I don't know where :(
#endif
void MGL_EXPORT mgl_srnd(long seed)
{
#if MGL_HAVE_GSL
	if(mgl_rng==0)
	{
		gsl_rng_env_setup();
		mgl_rng = gsl_rng_alloc(gsl_rng_default);
	}
	gsl_rng_set(mgl_rng, seed);
#else
	srand(seed);
#endif
}
void MGL_EXPORT mgl_srnd_(int *seed)	{	mgl_srnd(*seed);	}
//-----------------------------------------------------------------------------
double MGL_EXPORT_CONST mgl_hypot(double x, double y)	{	return hypot(x,y);	}
//-----------------------------------------------------------------------------
#if MGL_HAVE_PTHREAD
extern pthread_mutex_t mutexRnd;
#endif
double MGL_EXPORT mgl_rnd()
{
#if MGL_HAVE_PTHREAD
	pthread_mutex_lock(&mutexRnd);
#endif
	double res;
#pragma omp critical(rnd)
	{
#if MGL_HAVE_GSL
		if(mgl_rng==0)
		{
			gsl_rng_env_setup();
			mgl_rng = gsl_rng_alloc(gsl_rng_default);
			gsl_rng_set(mgl_rng, time(0));
		}
		res = gsl_rng_uniform(mgl_rng);
//		gsl_rng_free(r);
#else
		res = rand()/(RAND_MAX-1.);
#endif
	}
#if MGL_HAVE_PTHREAD
	pthread_mutex_unlock(&mutexRnd);
#endif
	return res;
}
double MGL_EXPORT mgl_rnd_()	{	return mgl_rnd();	}
//-----------------------------------------------------------------------------
mglFormula::~mglFormula()
{
	if(tmp)		delete tmp;
	if(Left)	delete Left;
	if(Right)	delete Right;
}
//-----------------------------------------------------------------------------
// Formula constructor (automatically parse and "compile" formula)
mglFormula::mglFormula(const char *string)
{
	dat = tmp = NULL;
	dx1=dy1=dz1=0;	dx2=dy2=dz2=1;
#if MGL_HAVE_GSL
	gsl_set_error_handler_off();
#endif
	Error=0;
	Left=Right=0;
	Res=0; Kod=0;
	if(!string)	{	Kod = EQ_NUM;	Res = 0;	return;	}
	char *str = new char[strlen(string)+1];
	strcpy(str,string);
	long n,len;
	mgl_strtrim(str);
	mgl_strlwr(str);
	len=strlen(str);
	if(str[0]==0) {	delete []str;	return;	}
	if(str[0]=='(' && mglCheck(str+1,len-2))	// remove braces
	{
		memmove(str,str+1,len);
		len-=2;	str[len]=0;
	}
	len=strlen(str);
	if(str[0]==':' && str[1]!=0)		//	this data file for interpolation
	{
		double sx1,sx2,sy1,sy2,sz1,sz2;
		char *buf = strchr(str+1,':');
		if(buf && *buf)
		{
			*buf = 0;
			int r = sscanf(buf+1,"%lg:%lg:%lg:%lg:%lg:%lg",&sx1,&sx2,&sy1,&sy2,&sz1,&sz2);
			if(r>1 && sx1!=sx2)	{	dx1=sx1;	dx2=sx2;	}
			if(r>3 && sy1!=sy2)	{	dy1=sy1;	dy2=sy2;	}
			if(r>5 && sz1!=sz2)	{	dz1=sz1;	dz2=sz2;	}
		}
		dat = tmp = new mglData(str+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"&|");				// lowest priority -- logical
	if(n>=0)
	{
		if(str[n]=='|') Kod=EQ_OR;	else Kod=EQ_AND;
		str[n]=0;
		Left=new mglFormula(str);
		Right=new mglFormula(str+n+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"<>=");				// low priority -- conditions
	if(n>=0)
	{
		if(str[n]=='<') Kod=EQ_LT;
		else if(str[n]=='>') Kod=EQ_GT;
		else Kod=EQ_EQ;
		str[n]=0;
		Left=new mglFormula(str);
		Right=new mglFormula(str+n+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"+-");				// normal priority -- additions
	if(n>=0 && (n<2 || str[n-1]!='e' || (str[n-2]!='.' && !isdigit(str[n-2]))))
	{
		if(str[n]=='+') Kod=EQ_ADD; else Kod=EQ_SUB;
		str[n]=0;
		Left=new mglFormula(str);
		Right=new mglFormula(str+n+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"*/%");				// high priority -- multiplications
	if(n>=0)
	{
		if(str[n]=='*')	Kod=EQ_MUL;
		else if(str[n]=='/') Kod=EQ_DIV;
		else	Kod=EQ_MOD;
		str[n]=0;
		Left=new mglFormula(str);
		Right=new mglFormula(str+n+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"^");				// highest priority -- power
	if(n>=0)
	{
		Kod=EQ_IPOW;		str[n]=0;
		Left=new mglFormula(str);
		Right=new mglFormula(str+n+1);
		delete []str;	return;
	}

	for(n=0;n<len;n++)	if(str[n]=='(')	break;
	if(n>=len)								// this is number or variable
	{
		Kod = EQ_NUM;
//		Left = Right = 0;
		if(str[1]==0 && str[0]>='a' && str[0]<='z')	// available variables
		{	Kod=EQ_A;	Res = str[0]-'a';	}
		else if(!strcmp(str,"rnd")) Kod=EQ_RND;
		else if(!strcmp(str,"pi")) Res=M_PI;
		else if(!strcmp(str,"inf")) Res=INFINITY;
		else Res=atof(str);				// this is number
	}
	else
	{
		char name[128];
		mgl_strncpy(name,str,128);	name[127]=name[n]=0;
		memmove(str,str+n+1,len-n);
		len=strlen(str);		str[--len]=0;
		if(!strncmp(name,"jacobi_",7))
			memmove(name,name+7,(strlen(name+7)+1)*sizeof(char));
		if(name[0]=='a')
		{
			if(!strcmp(name+1,"sin"))		Kod=EQ_ASIN;
			else if(!strcmp(name+1,"cos"))	Kod=EQ_ACOS;
			else if(!strcmp(name+1,"tan"))	Kod=EQ_ATAN;
			else if(!strcmp(name+1,"sinh"))	Kod=EQ_ASINH;
			else if(!strcmp(name+1,"cosh"))	Kod=EQ_ACOSH;
			else if(!strcmp(name+1,"tanh"))	Kod=EQ_ATANH;
			else if(!strcmp(name+1,"rg"))	Kod=EQ_ARG;
			else if(!strcmp(name+1,"bs"))	Kod=EQ_ABS;
			else if(!strcmp(name+1,"i"))	Kod=EQ_AI;
			else if(!strcmp(name+1,"iry_ai"))	Kod=EQ_AI;
			else if(!strcmp(name+1,"iry_bi"))	Kod=EQ_BI;
			else if(!strcmp(name+1,"iry_dai"))	Kod=EQ_AID;
			else if(!strcmp(name+1,"iry_dbi"))	Kod=EQ_BID;
		}
		else if(name[0]=='b')
		{
			if(!strcmp(name+1,"i"))		Kod=EQ_BI;
			else if(!strcmp(name+1,"essel_j"))	Kod=EQ_BESJ;
			else if(!strcmp(name+1,"essel_i"))	Kod=EQ_BESI;
			else if(!strcmp(name+1,"essel_k"))	Kod=EQ_BESK;
			else if(!strcmp(name+1,"essel_y"))	Kod=EQ_BESY;
			else if(!strcmp(name+1,"eta"))	Kod=EQ_BETA;
		}
		else if(name[0]=='c')
		{
			if(!strcmp(name+1,"os"))		Kod=EQ_COS;
			else if(!strcmp(name+1,"osh"))	Kod=EQ_COSH;
			else if(!strcmp(name+1,"h"))	Kod=EQ_COSH;
			else if(!strcmp(name+1,"i"))	Kod=EQ_CI;
			else if(!strcmp(name+1,"n"))	Kod=EQ_CN;
			else if(!strcmp(name+1,"s"))	Kod=EQ_CS;
			else if(!strcmp(name+1,"d"))	Kod=EQ_CD;
			else if(!strcmp(name+1,"l"))	Kod=EQ_CL;
		}
		else if(name[0]=='d')
		{
			if(!strcmp(name+1,"n"))			Kod=EQ_DN;
			else if(!strcmp(name+1,"s"))	Kod=EQ_DS;
			else if(!strcmp(name+1,"c"))	Kod=EQ_DC;
			else if(!strcmp(name+1,"ilog"))	Kod=EQ_LI2;
		}
		else if(name[0]=='e')
		{
			if(!strcmp(name+1,"xp"))		Kod=EQ_EXP;
			else if(!strcmp(name+1,"rf"))	Kod=EQ_ERF;
			else if(!strcmp(name+1,"n"))	Kod=EQ_EN;
			else if(!strcmp(name+1,"e"))	Kod=EQ_ELLE;
			else if(!strcmp(name+1,"k"))	Kod=EQ_ELLK;
			else if(name[0]==0)				Kod=EQ_ELE;
			else if(!strcmp(name+1,"i"))	Kod=EQ_EI;
			else if(!strcmp(name+1,"1"))	Kod=EQ_E1;
			else if(!strcmp(name+1,"2"))	Kod=EQ_E2;
			else if(!strcmp(name+1,"ta"))	Kod=EQ_ETA;
			else if(!strcmp(name+1,"i3"))	Kod=EQ_EI3;
			else if(!strcmp(name+1,"lliptic_e"))	Kod=EQ_ELE;
			else if(!strcmp(name+1,"lliptic_f"))	Kod=EQ_ELF;
			else if(!strcmp(name+1,"lliptic_ec"))	Kod=EQ_ELLE;
			else if(!strcmp(name+1,"lliptic_kc"))	Kod=EQ_ELLK;
		}
		else if(name[0]=='l')
		{
			if(!strcmp(name+1,"og"))		Kod=EQ_LOG;
			else if(!strcmp(name+1,"g"))	Kod=EQ_LG;
			else if(!strcmp(name+1,"n"))	Kod=EQ_LN;
			else if(!strcmp(name+1,"i2"))	Kod=EQ_LI2;
			else if(!strcmp(name+1,"egendre"))	Kod=EQ_LP;
		}
		else if(name[0]=='s')
		{
			if(!strcmp(name+1,"qrt"))		Kod=EQ_SQRT;
			else if(!strcmp(name+1,"in"))	Kod=EQ_SIN;
			else if(!strcmp(name+1,"tep"))	Kod=EQ_STEP;
			else if(!strcmp(name+1,"ign"))	Kod=EQ_SIGN;
			else if(!strcmp(name+1,"inh"))	Kod=EQ_SINH;
			else if(!strcmp(name+1,"h"))	Kod=EQ_SINH;
			else if(!strcmp(name+1,"i"))	Kod=EQ_SI;
			else if(!strcmp(name+1,"n"))	Kod=EQ_SN;
			else if(!strcmp(name+1,"c"))	Kod=EQ_SC;
			else if(!strcmp(name+1,"d"))	Kod=EQ_SD;
			else if(!strcmp(name+1,"inc"))	Kod=EQ_SINC;
		}
		else if(name[0]=='t')
		{
			if(!strcmp(name+1,"g"))			Kod=EQ_TAN;
			else if(!strcmp(name+1,"an"))	Kod=EQ_TAN;
			else if(!strcmp(name+1,"anh"))	Kod=EQ_TANH;
			else if(!strcmp(name+1,"h"))	Kod=EQ_TANH;
		}
		else if(name[0]=='m')
		{
			if(!strcmp(name+1,"od"))		Kod=EQ_MOD;
			else if(!strcmp(name+1,"ax"))	Kod=EQ_MAX;
			else if(!strcmp(name+1,"in"))	Kod=EQ_MIN;
		}
		else if(!strcmp(name,"hypot"))	Kod=EQ_HYPOT;
		else if(!strcmp(name,"pow"))	Kod=EQ_POW;
		else if(!strcmp(name,"i"))		Kod=EQ_BESI;
		else if(!strcmp(name,"int"))	Kod=EQ_INT;
		else if(!strcmp(name,"j"))		Kod=EQ_BESJ;
		else if(!strcmp(name,"k"))		Kod=EQ_BESK;
		else if(!strcmp(name,"y"))		Kod=EQ_BESY;
		else if(!strcmp(name,"f"))		Kod=EQ_ELF;
		else if(!strcmp(name,"gamma"))	Kod=EQ_GAMMA;
		else if(!strcmp(name,"gamma_inc"))	Kod=EQ_GAMMA_INC;
		else if(!strcmp(name,"ns"))		Kod=EQ_NS;
		else if(!strcmp(name,"nc"))		Kod=EQ_NC;
		else if(!strcmp(name,"nd"))		Kod=EQ_ND;
		else if(!strcmp(name,"w0"))		Kod=EQ_W0;
		else if(!strcmp(name,"w1"))		Kod=EQ_W1;
		else if(!strcmp(name,"psi"))	Kod=EQ_PSI;
		else if(!strcmp(name,"zeta"))	Kod=EQ_ZETA;
		else if(!strcmp(name,"z"))		Kod=EQ_Z;
		else {	delete []str;	return;	}	// unknown function
		n=mglFindInText(str,",");
		if(n>=0)
		{
			str[n]=0;
			Left=new mglFormula(str);
			Right=new mglFormula(str+n+1);
		}
		else	Left=new mglFormula(str);
	}
	delete []str;
}
//-----------------------------------------------------------------------------
// evaluate formula for 'x'='r', 'y'='n'='v', 't'='z', 'u'='a' variables
mreal mglFormula::Calc(mreal x,mreal y,mreal t,mreal u) const
{
	Error=0;
	mreal a1[MGL_VS];	memset(a1,0,MGL_VS*sizeof(mreal));
	a1['a'-'a'] = a1['c'-'a'] = a1['u'-'a'] = u;
	a1['x'-'a'] = a1['r'-'a'] = x;
	a1['y'-'a'] = a1['n'-'a'] = a1['v'-'a'] = y;
	a1['z'-'a'] = a1['t'-'a'] = t;
	mreal b = CalcIn(a1);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
// evaluate formula for 'x'='r', 'y'='n', 't'='z', 'u'='a', 'v'='b', 'w'='c' variables
mreal mglFormula::Calc(mreal x,mreal y,mreal t,mreal u,mreal v,mreal w) const
{
	Error=0;
	mreal a1[MGL_VS];	memset(a1,0,MGL_VS*sizeof(mreal));
	a1['c'-'a'] = a1['w'-'a'] = w;
	a1['b'-'a'] = a1['v'-'a'] = v;
	a1['a'-'a'] = a1['u'-'a'] = u;
	a1['x'-'a'] = a1['r'-'a'] = x;
	a1['y'-'a'] = a1['n'-'a'] = y;
	a1['z'-'a'] = a1['t'-'a'] = t;
	mreal b = CalcIn(a1);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
// evaluate formula for arbitrary set of variables
mreal mglFormula::Calc(const mreal var[MGL_VS]) const
{
	Error=0;
	mreal b = CalcIn(var);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
// evaluate formula for 'x'='r', 'y'='n'='v', 't'='z', 'u'='a' variables
mreal mglFormula::CalcD(char diff,mreal x,mreal y,mreal t,mreal u) const
{
	Error=0;
	mreal a1[MGL_VS];	memset(a1,0,MGL_VS*sizeof(mreal));
	a1['a'-'a'] = a1['c'-'a'] = a1['u'-'a'] = u;
	a1['x'-'a'] = a1['r'-'a'] = x;
	a1['y'-'a'] = a1['n'-'a'] = a1['v'-'a'] = y;
	a1['z'-'a'] = a1['t'-'a'] = t;
	mreal b = CalcDIn(diff-'a', a1);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
// evaluate formula for 'x'='r', 'y'='n', 't'='z', 'u'='a', 'v'='b', 'w'='c' variables
mreal mglFormula::CalcD(char diff,mreal x,mreal y,mreal t,mreal u,mreal v,mreal w) const
{
	Error=0;
	mreal a1[MGL_VS];	memset(a1,0,MGL_VS*sizeof(mreal));
	a1['c'-'a'] = a1['w'-'a'] = w;
	a1['b'-'a'] = a1['v'-'a'] = v;
	a1['a'-'a'] = a1['u'-'a'] = u;
	a1['x'-'a'] = a1['r'-'a'] = x;
	a1['y'-'a'] = a1['n'-'a'] = y;
	a1['z'-'a'] = a1['t'-'a'] = t;
	mreal b = CalcDIn(diff-'a', a1);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
// evaluate derivate of formula respect to 'diff' variable for arbitrary set of other variables
mreal mglFormula::CalcD(const mreal var[MGL_VS], char diff) const
{
	Error=0;
	mreal b = CalcDIn(diff-'a', var);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
double MGL_LOCAL_CONST cand(double a,double b)	{return a&&b?1:0;}
double MGL_LOCAL_CONST cor(double a,double b)	{return a||b?1:0;}
double MGL_LOCAL_CONST ceq(double a,double b)	{return a==b?1:0;}
double MGL_LOCAL_CONST clt(double a,double b)	{return a<b?1:0;}
double MGL_LOCAL_CONST cgt(double a,double b)	{return a>b?1:0;}
double MGL_LOCAL_CONST add(double a,double b)	{return a+b;}
double MGL_LOCAL_CONST sub(double a,double b)	{return a-b;}
double MGL_LOCAL_CONST mul(double a,double b)	{return a&&b?a*b:0;}
double MGL_LOCAL_CONST del(double a,double b)	{return b?a/b:NAN;}
double MGL_LOCAL_CONST ipw(double a,double b)	{return fabs(b-int(b))<1e-5 ? mgl_ipow(a,int(b)) : pow(a,b);}
double MGL_LOCAL_CONST llg(double a,double b)	{return log(a)/log(b);}
#if MGL_HAVE_GSL
double MGL_LOCAL_CONST gslEllE(double a,double b)	{return gsl_sf_ellint_E(a,b,GSL_PREC_SINGLE);}
double MGL_LOCAL_CONST gslEllF(double a,double b)	{return gsl_sf_ellint_F(a,b,GSL_PREC_SINGLE);}
double MGL_LOCAL_CONST gslLegP(double a,double b)	{return gsl_sf_legendre_Pl(int(a),b);}
double MGL_LOCAL_CONST gslEllEc(double a)	{return gsl_sf_ellint_Ecomp(a,GSL_PREC_SINGLE);}
double MGL_LOCAL_CONST gslEllFc(double a)	{return gsl_sf_ellint_Kcomp(a,GSL_PREC_SINGLE);}
double MGL_LOCAL_CONST gslAi(double a)	{return gsl_sf_airy_Ai(a,GSL_PREC_SINGLE);}
double MGL_LOCAL_CONST gslBi(double a)	{return gsl_sf_airy_Bi(a,GSL_PREC_SINGLE);}
double MGL_LOCAL_CONST gslAi_d(double a)	{return gsl_sf_airy_Ai_deriv(a,GSL_PREC_SINGLE);}
double MGL_LOCAL_CONST gslBi_d(double a)	{return gsl_sf_airy_Bi_deriv(a,GSL_PREC_SINGLE);}
#endif
double MGL_LOCAL_CONST sgn(double a)	{return a<0 ? -1:(a>0?1:0);}
double MGL_LOCAL_CONST stp(double a)	{return a>0 ? 1:0;}
double MGL_LOCAL_CONST arg(double a,double b)	{	return atan2(b,a);	}
double MGL_LOCAL_CONST mgz1(double)			{return NAN;}	// NOTE I think NAN value is more correct here than 0
double MGL_LOCAL_CONST mgz2(double,double)	{return NAN;}	// NOTE I think NAN value is more correct here than 0
double MGL_LOCAL_CONST mgl_asinh(double x)	{	return log(x+sqrt(x*x+1.));	}
double MGL_LOCAL_CONST mgl_acosh(double x)	{	return x>1 ? log(x+sqrt(x*x-1.)) : NAN;	}
double MGL_LOCAL_CONST mgl_atanh(double x)	{	return fabs(x)<1 ? log((1.+x)/(1.-x))/2 : NAN;	}
double MGL_LOCAL_CONST mgl_fmin(double a,double b)	{	return a > b ? b : a;	}
double MGL_LOCAL_CONST mgl_fmax(double a,double b)	{	return a > b ? a : b;	}
//-----------------------------------------------------------------------------
typedef double (*func_1)(double);
typedef double (*func_2)(double, double);
//-----------------------------------------------------------------------------
static const mreal z2[EQ_SIN-EQ_LT] = {3,3,3,3,0,3,3,0,0,0,0,0,NAN,3,3,3,3
#if MGL_HAVE_GSL
	,3,NAN, 3,NAN, 0,0,3,1,3
#else
	,0,0,0,0,0,0,0,0,0
#endif
};
static const func_2 f2[EQ_SIN-EQ_LT] = {clt,cgt,ceq,cor,cand,add,sub,mul,del,ipw,pow,fmod,llg,arg,hypot,mgl_fmax,mgl_fmin
#if MGL_HAVE_GSL
	,gsl_sf_bessel_Jnu,gsl_sf_bessel_Ynu,
	gsl_sf_bessel_Inu,gsl_sf_bessel_Knu,
	gslEllE,gslEllF,gslLegP,gsl_sf_beta,gsl_sf_gamma_inc
#else
	,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2
#endif
};
static const func_1 f1[EQ_SN-EQ_SIN] = {sin,cos,tan,asin,acos,atan,sinh,cosh,tanh,
			mgl_asinh,mgl_acosh,mgl_atanh,sqrt,exp,log,log10,sgn,stp,floor,fabs
#if MGL_HAVE_GSL
	,gsl_sf_dilog,gslEllEc,gslEllFc,gslAi,gslBi,gsl_sf_erf,
	gsl_sf_expint_3,gsl_sf_expint_Ei,gsl_sf_expint_E1,gsl_sf_expint_E2,
	gsl_sf_Si,gsl_sf_Ci,gsl_sf_gamma,gsl_sf_psi,gsl_sf_lambert_W0,
	gsl_sf_lambert_Wm1,gsl_sf_sinc,gsl_sf_zeta,gsl_sf_eta,gslAi_d,gslBi_d,
	gsl_sf_dawson
#else
	,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,
	mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1
#endif
};
//-----------------------------------------------------------------------------
// evaluation of embedded (included) expressions
mreal mglFormula::CalcIn(const mreal *a1) const
{
	if(dat)
	{
		mreal x = (a1['x'-'a']-dx1)*(dat->GetNx()-1)/(dx2-dx1);
		mreal y = (a1['y'-'a']-dy1)*(dat->GetNy()-1)/(dy2-dy1);
		mreal z = (a1['z'-'a']-dz1)*(dat->GetNz()-1)/(dz2-dz1);
		return mgl_data_spline(dat,x,y,z);
	}
	if(Kod<EQ_LT)
	{
		if(Kod==EQ_RND)	return mgl_rnd();
		else	return (Kod==EQ_A) ? a1[int(Res)] : Res;
	}
	double a = Left->CalcIn(a1);
	if(mgl_isfin(a))
	{
		if(Kod<EQ_SIN)
		{
			// try to bypass calc b if a==0
			if(a==0 && z2[Kod-EQ_LT]!=3)	return z2[Kod-EQ_LT];
			return Right?f2[Kod-EQ_LT](a, Right->CalcIn(a1)):NAN;
		}
		else if(Kod<EQ_SN)	return f1[Kod-EQ_SIN](a);
#if MGL_HAVE_GSL
		else if(Kod<=EQ_DC)
		{
			double sn=0, cn=0, dn=0;
			gsl_sf_elljac_e(a,Right->CalcIn(a1), &sn, &cn, &dn);
			switch(Kod)
			{
			case EQ_SN:		return sn;
			case EQ_SC:		return sn/cn;
			case EQ_SD:		return sn/dn;
			case EQ_CN:		return cn;
			case EQ_CS:		return cn/sn;
			case EQ_CD:		return cn/dn;
			case EQ_DN:		return dn;
			case EQ_DS:		return dn/sn;
			case EQ_DC:		return dn/cn;
			case EQ_NS:		return 1./sn;
			case EQ_NC:		return 1./cn;
			case EQ_ND:		return 1./dn;
			}
		}
#endif
	}
	return NAN;
}
//-----------------------------------------------------------------------------
double MGL_LOCAL_CONST mgzz(double,double)	{return 0;}
double MGL_LOCAL_CONST mgp(double ,double )	{return 1;}
double MGL_LOCAL_CONST mgm(double ,double )	{return -1;}
double MGL_LOCAL_CONST mul1(double ,double b)	{return b;}
double MGL_LOCAL_CONST mul2(double a,double )	{return a;}
double MGL_LOCAL_CONST div1(double ,double b)	{return b?1/b:NAN;}
double MGL_LOCAL_CONST div2(double a,double b)	{return b?-a/(b*b):NAN;}
double MGL_LOCAL_CONST ipw1(double a,double b)	{return b*(fabs(b-int(b))<1e-5 ? mgl_ipow(a,int(b-1)) : pow(a,b-1));}
double MGL_LOCAL_CONST pow1(double a,double b)	{return b*pow(a,b-1);}
double MGL_LOCAL_CONST pow2(double a,double b)	{return log(a)*pow(a,b);}
double MGL_LOCAL_CONST llg1(double a,double b)	{return 1/(a*log(b));}
double MGL_LOCAL_CONST llg2(double a,double b)	{return -log(a)/(b*log(b)*log(b));}
double MGL_LOCAL_CONST cos_d(double a)	{return -sin(a);}
double MGL_LOCAL_CONST tan_d(double a)	{return 1./(cos(a)*cos(a));}
double MGL_LOCAL_CONST asin_d(double a)	{return 1./sqrt(1.-a*a);}
double MGL_LOCAL_CONST acos_d(double a)	{return -1./sqrt(1.-a*a);}
double MGL_LOCAL_CONST atan_d(double a)	{return 1./(1.+a*a);}
double MGL_LOCAL_CONST tanh_d(double a)	{return 1./(cosh(a)*cosh(a));}
double MGL_LOCAL_CONST atanh_d(double a){return 1./(1.-a*a);}
double MGL_LOCAL_CONST asinh_d(double a){return 1./sqrt(1.+a*a);}
double MGL_LOCAL_CONST acosh_d(double a){return 1./sqrt(a*a-1.);}
double MGL_LOCAL_CONST sqrt_d(double a)	{return 0.5/sqrt(a);}
double MGL_LOCAL_CONST log10_d(double a){return M_LN10/a;}
double MGL_LOCAL_CONST log_d(double a)	{return 1./a;}
double MGL_LOCAL_CONST erf_d(double a)	{return 2*exp(-a*a)/sqrt(M_PI);}
double MGL_LOCAL_CONST dilog_d(double a){return log(a)/(1.-a);}
double MGL_LOCAL_CONST ei_d(double a)	{return exp(a)/a;}
double MGL_LOCAL_CONST si_d(double a)	{return a?sin(a)/a:1;}
double MGL_LOCAL_CONST ci_d(double a)	{return cos(a)/a;}
double MGL_LOCAL_CONST exp3_d(double a)	{return exp(-a*a*a);}
double MGL_LOCAL_CONST e1_d(double a)	{return exp(-a)/a;}
double MGL_LOCAL_CONST sinc_d(double a)	{return a ? (cos(M_PI*a)/a-sin(M_PI*a)/(M_PI*a*a)) : 0;}
#if MGL_HAVE_GSL
double MGL_LOCAL_CONST e2_d(double a)	{return -gsl_sf_expint_E1(a);}
double MGL_LOCAL_CONST gslJnuD(double a,double b)	{return 0.5*(gsl_sf_bessel_Jnu(a-1,b)-gsl_sf_bessel_Jnu(a+1,b));}
double MGL_LOCAL_CONST gslYnuD(double a,double b)	{return 0.5*(gsl_sf_bessel_Ynu(a-1,b)-gsl_sf_bessel_Ynu(a+1,b));}
double MGL_LOCAL_CONST gslKnuD(double a,double b)	{return -(a*gsl_sf_bessel_Knu(a,b)/b +gsl_sf_bessel_Knu(a-1,b));}
double MGL_LOCAL_CONST gslInuD(double a,double b)	{return -(a*gsl_sf_bessel_Inu(a,b)/b -gsl_sf_bessel_Inu(a-1,b));}
double MGL_LOCAL_CONST gslEllE1(double a,double b)	{return sqrt(1.-sin(a)*sin(a)*b);}
double MGL_LOCAL_CONST gslEllE2(double a,double b)	{return (gsl_sf_ellint_E(a,b,GSL_PREC_SINGLE) - gsl_sf_ellint_F(a,b,GSL_PREC_SINGLE))/(2.*b);}
double MGL_LOCAL_CONST gslEllF1(double a,double b)	{return 1./sqrt(1.-sin(a)*sin(a)*b);}
double MGL_LOCAL_CONST gslEllF2(double a,double b)	{return (gsl_sf_ellint_E(a,b,GSL_PREC_SINGLE) - gsl_sf_ellint_F(a,b,GSL_PREC_SINGLE)*(1.-b))/(2*b*(1.-b)) - sin(2.*a)/(sqrt(1.-sin(a)*sin(a)*b)*2.*(1.-b));}
double MGL_LOCAL_CONST gslE_d(double a)	{return (gsl_sf_ellint_Ecomp(a,GSL_PREC_SINGLE) - gsl_sf_ellint_Kcomp(a,GSL_PREC_SINGLE))/(2.*a);}
double MGL_LOCAL_CONST gslK_d(double a)	{return (gsl_sf_ellint_Ecomp(a,GSL_PREC_SINGLE) - (1.-a)*gsl_sf_ellint_Kcomp(a,GSL_PREC_SINGLE))/(2.*a*(1.-a));}
double MGL_LOCAL_CONST gamma_d(double a)	{return gsl_sf_psi(a)*gsl_sf_gamma(a);}
#endif
double MGL_LOCAL_CONST ginc_d(double a, double x)	{return -exp(-x)*pow(x,a-1);}
//-----------------------------------------------------------------------------
static const func_2 f21[EQ_SIN-EQ_LT] = {mgzz,mgzz,mgzz, mgzz,mgzz,mgp, mgp,mul1,div1, ipw1,pow1,mgp,llg1, mgz2,mgzz,mgzz
#if MGL_HAVE_GSL
	,mgz2,mgz2,mgz2, mgz2,gslEllE1,gslEllF1, mgz2,mgz2,mgz2
#else
	,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2
#endif
};
static const func_2 f22[EQ_SIN-EQ_LT] = {mgzz,mgzz,mgzz,mgzz,mgzz,mgp,mgm,mul2,div2,pow2,pow2,mgz2,llg2, mgz2,mgzz,mgzz
#if MGL_HAVE_GSL
	,gslJnuD,gslYnuD,gslInuD,gslKnuD,gslEllE2,gslEllF2,mgz2/*gslLegP*/,mgz2,ginc_d
#else
	,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2,mgz2
#endif
};
static const func_1 f11[EQ_SN-EQ_SIN] = {cos,cos_d,tan_d,asin_d,acos_d,atan_d,cosh,sinh,tanh_d,
	asinh_d,acosh_d,atanh_d,sqrt_d,exp,log_d,log10_d,mgz1,mgz1,mgz1,sgn
#if MGL_HAVE_GSL
	,dilog_d,gslE_d,gslK_d,gslAi_d,gslBi_d,erf_d,exp3_d,ei_d,e1_d,e2_d,
	si_d,ci_d,gamma_d,gsl_sf_psi_1,mgz1,mgz1,sinc_d,mgz1,mgz1,mgz1,mgz1,mgz1
#else
	,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,
	mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1,mgz1
#endif
};
//-----------------------------------------------------------------------------
// evaluation of derivative of embedded (included) expressions
mreal mglFormula::CalcDIn(int id, const mreal *a1) const
{
	if(dat)
	{
		mreal x = (a1['x'-'a']-dx1)*(dat->GetNx()-1)/(dx2-dx1);
		mreal y = (a1['y'-'a']-dy1)*(dat->GetNy()-1)/(dy2-dy1);
		mreal z = (a1['z'-'a']-dz1)*(dat->GetNz()-1)/(dz2-dz1);
		mreal dx,dy,dz, res=0;
		mgl_data_spline_ext(dat,x,y,z,&dx,&dy,&dz);
		if(id=='x'-'a')	res = dx/(dat->GetNx()-1)*(dx2-dx1);
		if(id=='y'-'a')	res = dy/(dat->GetNy()-1)*(dy2-dy1);
		if(id=='z'-'a')	res = dz/(dat->GetNz()-1)*(dz2-dz1);
		return res;
	}
	if(Kod==EQ_A && id==(int)Res)	return 1;
	else if(Kod<EQ_LT)	return 0;
	double a = Left->CalcIn(a1), d = Left->CalcDIn(id,a1);
	if(mgl_isfin(a) && mgl_isfin(d))
	{
		if(Kod<EQ_SIN)
		{
			double b = Right?Right->CalcIn(a1):NAN;
			double c = Right?Right->CalcDIn(id,a1):NAN;
//			return mgl_isfin(b) ? (f21[Kod-EQ_LT](a,b)*d + f22[Kod-EQ_LT](a,b)*c) : NAN;
			return mgl_isfin(b) ? (d?f21[Kod-EQ_LT](a,b)*d:0) + (c?f22[Kod-EQ_LT](a,b)*c:0) : NAN;
		}
//		else if(Kod<EQ_SN)	return f11[Kod-EQ_SIN](a)*d;
		else if(Kod<EQ_SN)	return d?f11[Kod-EQ_SIN](a)*d:0;
#if MGL_HAVE_GSL
		else if(Kod<=EQ_DC)
		{
			double sn=0, cn=0, dn=0, b = Right->CalcIn(a1);
			if(mgl_isbad(b))	return NAN;
			gsl_sf_elljac_e(a,b, &sn, &cn, &dn);
			switch(Kod)	// At this moment parse only differentiation or argument NOT mu !!!
			{
			case EQ_SN:		return cn*dn*d;
			case EQ_SC:		return dn*d/(cn*cn);
			case EQ_SD:		return cn*d/(dn*dn);
			case EQ_CN:		return -dn*sn*d;
			case EQ_CS:		return dn*d/(sn*sn);
			case EQ_CD:		return (b-1)*d*sn/(dn*dn);
			case EQ_DN:		return -b*d*cn*sn;
			case EQ_DS:		return -cn*d/(sn*sn);
			case EQ_DC:		return (1-b)*sn*d/(cn*cn);
			case EQ_NS:		return -cn*dn*d/(sn*sn);
			case EQ_NC:		return dn*sn*d/(cn*cn);
			case EQ_ND:		return b*cn*sn*d/(dn*dn);
			}
		}
#endif
	}
	return NAN;
}
//-----------------------------------------------------------------------------
// Check braces correctness
bool MGL_LOCAL_PURE mglCheck(char *str,int n)
{
	long s = 0;
	for(long i=0;i<n;i++)
	{
		if(str[i]=='(')	s++;
		if(str[i]==')') s--;
		if(s<0)	return false;
	}
	return (s==0) ? true : false;
}
//-----------------------------------------------------------------------------
// Try to find one of symbols lst in the string str
int MGL_LOCAL_PURE mglFindInText(const char *str, const char *lst)
{
	long l=0,r=0,len=strlen(str);
	for(long i=len-1;i>=0;i--)
	{
		if(str[i]=='(') l++;
		if(str[i]==')') r++;
		if(l==r && strchr(lst,str[i]))	return i;
	}
	return -1;
}
//-----------------------------------------------------------------------------
HMEX MGL_EXPORT mgl_create_expr(const char *expr)	{	return new mglFormula(expr);	}
void MGL_EXPORT mgl_delete_expr(HMEX ex)	{	if(ex)	delete ex;	}
double MGL_EXPORT_PURE mgl_expr_eval(HMEX ex, double x, double y,double z)
{	return ex->Calc(x,y,z);	}
double MGL_EXPORT_PURE mgl_expr_eval_v(HMEX ex, mreal *var)
{	return ex->Calc(var);	}
double MGL_EXPORT_PURE mgl_expr_diff(HMEX ex, char dir, double x, double y,double z)
{	return ex->CalcD(dir,x,y,z);	}
double MGL_EXPORT_PURE mgl_expr_diff_v(HMEX ex, char dir, mreal *var)
{	return ex->CalcD(var, dir);		}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_create_expr_(const char *expr, int l)
{	char *s=new char[l+1];	memcpy(s,expr,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_create_expr(s));
	delete []s;	return res;	}
void MGL_EXPORT mgl_delete_expr_(uintptr_t *ex)	{	mgl_delete_expr((HMEX)ex);	}
double MGL_EXPORT_PURE mgl_expr_eval_(uintptr_t *ex, mreal *x, mreal *y, mreal *z)
{	return mgl_expr_eval((HMEX) ex, *x,*y,*z);		}
double MGL_EXPORT_PURE mgl_expr_diff_(uintptr_t *ex, const char *dir, mreal *x, mreal *y, mreal *z, int)
{	return mgl_expr_diff((HMEX) ex, *dir,*x,*y,*z);	}
//-----------------------------------------------------------------------------
