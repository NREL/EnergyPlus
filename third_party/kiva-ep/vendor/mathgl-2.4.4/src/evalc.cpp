/***************************************************************************
 * evalc.cpp is part of Math Graphic Library
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
#include "mgl2/datac.h"
#include "mgl2/evalc.h"
#if MGL_HAVE_GSL
#include <gsl/gsl_sf.h>
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
EQ_ADD,		// addition x+y
EQ_SUB,		// substraction x-y
EQ_MUL,		// multiplication x*y
EQ_DIV,		// division x/y
EQ_IPOW,	// power x^n for integer n
EQ_POW,		// power x^y
EQ_LOG,		// logarithm of x on base a, log_a(x) = ln(x)/ln(a)
EQ_CMPLX,	// return a+i*b
EQ_HYPOT,	// return sqrt(a*a+b*b)
// normal functions of 1 argument
EQ_SIN,		// sine function \sin(x).			!!! MUST BE FIRST 1-PLACE FUNCTION
EQ_COS,		// cosine function \cos(x).
EQ_TAN,		// tangent function \tan(x).
EQ_ASIN,	// inverse sine function \asin(x).
EQ_ACOS,	// inverse cosine function \acos(x).
EQ_ATAN,	// inverse tangent function \atan(x).
EQ_SINH,	// hyperbolic sine function \sinh(x).
EQ_COSH,	// hyperbolic cosine function \cosh(x).
EQ_TANH,	// hyperbolic tangent function \tanh(x).
EQ_ASINH,	// inverse hyperbolic sine function \asinh(x).
EQ_ACOSH,	// inverse hyperbolic cosine function \acosh(x).
EQ_ATANH,	// inverse hyperbolic tangent function \atanh(x).
EQ_SQRT,	// square root function \sqrt(x)
EQ_EXP,		// exponential function \exp(x)
EQ_EXPI,	// exponential function \exp(i*x)
EQ_LN,		// logarithm of x, ln(x)
EQ_LG,		// decimal logarithm of x, lg(x) = ln(x)/ln(10)
EQ_ABS,		// absolute value
EQ_ARG,		// argument (or phase) of complex number
EQ_CONJ,	// complex conjugate
EQ_REAL,	// real part
EQ_IMAG,	// imaginary part
EQ_NORM,	// square of absolute value |u|^2
EQ_LAST		// id of last entry
};
//-----------------------------------------------------------------------------
int mglFormulaC::Error=0;
bool MGL_LOCAL_PURE mglCheck(char *str,int n);
int MGL_LOCAL_PURE mglFindInText(const char *str, const char *lst);
//-----------------------------------------------------------------------------
mglFormulaC::~mglFormulaC()
{
	if(tmp)		delete tmp;
	if(Left)	delete Left;
	if(Right)	delete Right;
}
//-----------------------------------------------------------------------------
// Formula constructor (automatically parse and "compile" formula)
mglFormulaC::mglFormulaC(const char *string)
{
	dat = tmp = NULL;
	dx1=dy1=dz1=0;	dx2=dy2=dz2=1;
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
	if(str[0]=='(' && mglCheck(&(str[1]),len-2))	// remove braces
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
		dat = tmp = new mglDataC(str+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"<>=");				// low priority -- conditions
	if(n>=0)
	{
		if(str[n]=='<') Kod=EQ_LT;
		else if(str[n]=='>') Kod=EQ_GT;
		else Kod=EQ_EQ;
		str[n]=0;
		Left=new mglFormulaC(str);
		Right=new mglFormulaC(str+n+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"+-");				// normal priority -- additions
	if(n>=0 && (n<2 || str[n-1]!='e' || (str[n-2]!='.' && !isdigit(str[n-2]))))
	{
		if(str[n]=='+') Kod=EQ_ADD; else Kod=EQ_SUB;
		str[n]=0;
		Left=new mglFormulaC(str);
		Right=new mglFormulaC(str+n+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"*/");				// high priority -- multiplications
	if(n>=0)
	{
		if(str[n]=='*') Kod=EQ_MUL; else Kod=EQ_DIV;
		str[n]=0;
		Left=new mglFormulaC(str);
		Right=new mglFormulaC(str+n+1);
		delete []str;	return;
	}
	n=mglFindInText(str,"^");				// highest priority -- power
	if(n>=0)
	{
		Kod=EQ_IPOW;		str[n]=0;
		Left=new mglFormulaC(str);
		Right=new mglFormulaC(str+n+1);
		delete []str;	return;
	}

	for(n=0;n<len;n++)	if(str[n]=='(')	break;
	if(n>=len)								// this is number or variable
	{
		Kod = EQ_NUM;
//		Left = Right = 0;
		if(str[1]==0 && str[0]>='a' && str[0]<='z')	// available variables
		{	Kod=EQ_A;	Res = str[0]-'a';	}
		else if(!strcmp(str,"rnd"))	Kod=EQ_RND;
		else if(!strcmp(str,"pi"))	Res=M_PI;
		else if(!strcmp(str,"inf"))	Res=INFINITY;
		else if(str[0]=='i')	Res = dual(0,atof(str+1));
		else	Res = (str[len-1]=='i') ? dual(0,atof(str)) : atof(str);
	}
	else
	{
		char name[128];
		mgl_strncpy(name,str,128);	name[127]=name[n]=0;
		memmove(str,str+n+1,len-n);
		len=strlen(str);		str[--len]=0;
		if(!strcmp(name,"sin")) Kod=EQ_SIN;
		else if(!strcmp(name,"cos")) Kod=EQ_COS;
		else if(!strcmp(name,"tg")) Kod=EQ_TAN;
		else if(!strcmp(name,"tan")) Kod=EQ_TAN;
		else if(!strcmp(name,"asin")) Kod=EQ_ASIN;
		else if(!strcmp(name,"acos")) Kod=EQ_ACOS;
		else if(!strcmp(name,"atan")) Kod=EQ_ATAN;
		else if(!strcmp(name,"sinh")) Kod=EQ_SINH;
		else if(!strcmp(name,"cosh")) Kod=EQ_COSH;
		else if(!strcmp(name,"tanh")) Kod=EQ_TANH;
		else if(!strcmp(name,"sh")) Kod=EQ_SINH;
		else if(!strcmp(name,"ch")) Kod=EQ_COSH;
		else if(!strcmp(name,"th")) Kod=EQ_TANH;
		else if(!strcmp(name,"sqrt")) Kod=EQ_SQRT;
		else if(!strcmp(name,"log")) Kod=EQ_LOG;
		else if(!strcmp(name,"pow")) Kod=EQ_POW;
		else if(!strcmp(name,"exp")) Kod=EQ_EXP;
		else if(!strcmp(name,"lg")) Kod=EQ_LG;
		else if(!strcmp(name,"ln")) Kod=EQ_LN;
		else if(!strcmp(name,"abs")) Kod=EQ_ABS;
		else if(!strcmp(name,"arg")) Kod=EQ_ARG;
		else if(!strcmp(name,"conj")) Kod=EQ_CONJ;
		else if(!strcmp(name,"real")) Kod=EQ_REAL;
		else if(!strcmp(name,"imag")) Kod=EQ_IMAG;
		else if(!strcmp(name,"norm")) Kod=EQ_NORM;
		else if(!strcmp(name,"cmplx")) Kod=EQ_CMPLX;
		else if(!strcmp(name,"hypot")) Kod=EQ_HYPOT;
		else {	delete []str;	return;	}	// unknown function
		n=mglFindInText(str,",");
		if(n>=0)
		{
			str[n]=0;
			Left=new mglFormulaC(str);
			Right=new mglFormulaC(str+n+1);
		}
		else
			Left=new mglFormulaC(str);
	}
	delete []str;
}
//-----------------------------------------------------------------------------
// evaluate formula for 'x'='r', 'y'='n'='v', 't'='z', 'u'='a' variables
dual mglFormulaC::Calc(dual x,dual y,dual t,dual u) const
{
	Error=0;
	dual a1[MGL_VS];	memset(a1,0,MGL_VS*sizeof(dual));
	a1['a'-'a'] = a1['u'-'a'] = u;
	a1['x'-'a'] = a1['r'-'a'] = x;
	a1['y'-'a'] = a1['n'-'a'] = a1['v'-'a'] = y;
	a1['z'-'a'] = a1['t'-'a'] = t;
	a1['i'-'a'] = dual(0,1);
	dual b = CalcIn(a1);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
// evaluate formula for 'x'='r', 'y'='n', 't'='z', 'u'='a', 'v'='b', 'w'='c' variables
dual mglFormulaC::Calc(dual x,dual y,dual t,dual u,dual v,dual w) const
{
	Error=0;
	dual a1[MGL_VS];	memset(a1,0,MGL_VS*sizeof(dual));
	a1['c'-'a'] = a1['w'-'a'] = w;
	a1['b'-'a'] = a1['v'-'a'] = v;
	a1['a'-'a'] = a1['u'-'a'] = u;
	a1['x'-'a'] = a1['r'-'a'] = x;
	a1['y'-'a'] = a1['n'-'a'] = y;
	a1['z'-'a'] = a1['t'-'a'] = t;
	a1['i'-'a'] = dual(0,1);
	dual b = CalcIn(a1);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
// evaluate formula for arbitrary set of variables
dual mglFormulaC::Calc(const dual var[MGL_VS]) const
{
	Error=0;
	dual b = CalcIn(var);
	return mgl_isfin(b) ? b : NAN;
}
//-----------------------------------------------------------------------------
dual MGL_LOCAL_CONST ceqc(dual a,dual b)	{return a==b?1:0;}
dual MGL_LOCAL_CONST cltc(dual a,dual b)	{return real(a-b)<0?1:0;}
dual MGL_LOCAL_CONST cgtc(dual a,dual b)	{return real(a-b)>0?1:0;}
dual MGL_LOCAL_CONST addc(dual a,dual b)	{return a+b;}
dual MGL_LOCAL_CONST subc(dual a,dual b)	{return a-b;}
dual MGL_LOCAL_CONST mulc(dual a,dual b)	{return a*b;}
dual MGL_LOCAL_CONST divc(dual a,dual b)	{return a/b;}
dual MGL_LOCAL_CONST ipwc(dual a,dual b)	{return mgl_ipowc(a,int(b.real()));}
dual MGL_LOCAL_CONST powc(dual a,dual b)	{return exp(b*log(a));	}
dual MGL_LOCAL_CONST llgc(dual a,dual b)	{return log(a)/log(b);	}
dual MGL_LOCAL_CONST cmplxc(dual a,dual b)	{return a+dual(0,1)*b;	}
dual MGL_LOCAL_CONST expi(dual a)	{	return exp(dual(0,1)*a);	}
dual MGL_LOCAL_CONST expi(double a)	{	return dual(cos(a),sin(a));	}
//-----------------------------------------------------------------------------
dual MGL_NO_EXPORT ic = dual(0,1);
dual MGL_LOCAL_CONST hypotc(dual x, dual y)	{	return sqrt(x*x+y*y);	}
dual MGL_LOCAL_CONST asinhc(dual x)	{	return log(x+sqrt(x*x+mreal(1)));	}
dual MGL_LOCAL_CONST acoshc(dual x)	{	return log(x+sqrt(x*x-mreal(1)));	}
dual MGL_LOCAL_CONST atanhc(dual x)	{	return log((mreal(1)+x)/(mreal(1)-x))/mreal(2);	}
dual MGL_LOCAL_CONST conjc(dual x)	{	return dual(real(x),-imag(x));	}
dual MGL_LOCAL_CONST sinc(dual x)	{	return sin(x);	}
dual MGL_LOCAL_CONST cosc(dual x)	{	return cos(x);	}
dual MGL_LOCAL_CONST tanc(dual x)	{	return tan(x);	}
dual MGL_LOCAL_CONST sinhc(dual x)	{	return sinh(x);	}
dual MGL_LOCAL_CONST coshc(dual x)	{	return cosh(x);	}
dual MGL_LOCAL_CONST tanhc(dual x)	{	return tanh(x);	}
dual MGL_LOCAL_CONST asinc(dual x)	{	return log(ic*x+sqrt(mreal(1)-x*x))/ic;	}
dual MGL_LOCAL_CONST acosc(dual x)	{	return log(x+sqrt(x*x-mreal(1)))/ic;	}
dual MGL_LOCAL_CONST atanc(dual x)	{	return log((ic-x)/(ic+x))/(mreal(2)*ic);	}
dual MGL_LOCAL_CONST expc(dual x)	{	return exp(x);	}
dual MGL_LOCAL_CONST sqrtc(dual x)	{	return sqrt(x);	}
dual MGL_LOCAL_CONST logc(dual x)	{	return log(x);	}
dual MGL_LOCAL_CONST absc(dual x)	{	return abs(x);	}
dual MGL_LOCAL_CONST argc(dual x)	{	return arg(x);	}
dual MGL_LOCAL_CONST lgc(dual x)	{	return log10(x);}
dual MGL_LOCAL_CONST realc(dual x)	{	return real(x);	}
dual MGL_LOCAL_CONST imagc(dual x)	{	return imag(x);	}
dual MGL_LOCAL_CONST normc(dual x)	{	return norm(x);	}
//-----------------------------------------------------------------------------
typedef dual (*func_1)(dual);
typedef dual (*func_2)(dual, dual);
static const func_2 f2[EQ_SIN-EQ_LT] = {cltc,cgtc,ceqc,addc,subc,mulc,divc,ipwc,powc,llgc,cmplxc,hypotc};
static const func_1 f1[EQ_LAST-EQ_SIN] = {sinc,cosc,tanc,asinc,acosc,atanc,sinhc,coshc,tanhc,
					asinhc,acoshc,atanhc,sqrtc,expc,expi,logc,lgc,absc,argc,conjc,realc,imagc,normc};
// evaluation of embedded (included) expressions
dual mglFormulaC::CalcIn(const dual *a1) const
{
	if(dat)
	{
		mreal x = (real(a1['x'-'a'])-dx1)*(dat->GetNx()-1)/(dx2-dx1);
		mreal y = (real(a1['y'-'a'])-dy1)*(dat->GetNy()-1)/(dy2-dy1);
		mreal z = (real(a1['z'-'a'])-dz1)*(dat->GetNz()-1)/(dz2-dz1);
		return mgl_datac_spline(dat,x,y,z);
	}
	if(Kod<EQ_LT)
	{
		if(Kod==EQ_RND)	return mgl_rnd();
		else	return (Kod==EQ_A) ? a1[int(Res.real())] : Res;
	}

	dual a = Left->CalcIn(a1);
	if(mgl_isfin(a))
	{
		if(Kod<EQ_SIN)
			return Right?f2[Kod-EQ_LT](a,Right->CalcIn(a1)):NAN;
		else
			return f1[Kod-EQ_SIN](a);
	}
	return NAN;
}
//-----------------------------------------------------------------------------
dual MGL_LOCAL_CONST mgl_ipowc_c(dual x,int n)
{
	dual t;
	if(n==2)	t = x*x;
	else if(n==1)	t = x;
	else if(n<0)	t = mreal(1)/mgl_ipowc_c(x,-n);
	else if(n==0)	t = mreal(1);
	else
	{
		t = mgl_ipowc_c(x,n/2);	t = t*t;
		if(n%2==1)	t *= x;
	}
	return t;
}
cmdual MGL_EXPORT_CONST mgl_ipowc(mdual x,int n)
{	return mdual(mgl_ipowc_c(x,n));	}
cmdual MGL_EXPORT_PURE mgl_ipowc_(mdual *x,int *n)	{	return mgl_ipowc(*x,*n);	}
//-----------------------------------------------------------------------------
HAEX MGL_EXPORT mgl_create_cexpr(const char *expr)	{	return new mglFormulaC(expr);	}
uintptr_t MGL_EXPORT mgl_create_cexpr_(const char *expr, int l)
{	char *s=new char[l+1];	memcpy(s,expr,l);	s[l]=0;
	uintptr_t res = uintptr_t(mgl_create_cexpr(s));
	delete []s;	return res;	}
void MGL_EXPORT mgl_delete_cexpr(HAEX ex)	{	if(ex)	delete ex;	}
void MGL_EXPORT mgl_delete_cexpr_(uintptr_t *ex)	{	mgl_delete_cexpr((HAEX)ex);	}
cmdual MGL_EXPORT mgl_cexpr_eval(HAEX ex, mdual x, mdual y, mdual z)
{	return mdual(ex->Calc(x,y,z));	}
cmdual MGL_EXPORT mgl_cexpr_eval_(uintptr_t *ex, mdual *x, mdual *y, mdual *z)
{	return mgl_cexpr_eval((HAEX) ex, *x,*y,*z);		}
cmdual MGL_EXPORT mgl_cexpr_eval_v(HAEX ex, mdual *var)
{	return mdual(ex->Calc(reinterpret_cast<dual*>(var)));	}
//-----------------------------------------------------------------------------
