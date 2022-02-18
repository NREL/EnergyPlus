/***************************************************************************
 * thread.h is part of Math Graphic Library
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
#ifndef _MGL_THREAD_H_
#define _MGL_THREAD_H_
#include "mgl2/define.h"
//-----------------------------------------------------------------------------
#if MGL_HAVE_PTHREAD
#include <pthread.h>
#endif
//-----------------------------------------------------------------------------
struct mglThreadD
{
	mreal *a;		// float* array with parameters or results
	const mreal *b,*c,*d,*e;	// float* arrays with parameters
	const long *p;	// long* array with parameters
	const void *v;	// pointer to data/grapher
	int id;			// thread id
	long n;			// total number of iteration
	const char *s;
};
struct mglThreadC
{
	dual *a;		// dual* array with parameters or results
	const dual *b,*c,*d,*e;	// dual* arrays with parameters
	const long *p;	// long* array with parameters
	const void *v;	// pointer to data/grapher
	int id;			// thread id
	long n;			// total number of iteration
	const char *s;
};
struct mglThreadV
{
	mreal *a;		// float* array with parameters or results
	dual *aa;		// dual* array with parameters or results
	const void *b,*c;	// float* arrays with parameters
	const mreal *d;	// float* arrays with parameters
	const long *p;	// long* array with parameters
	const void *v;	// pointer to data/grapher
	int id;			// thread id
	long n;			// total number of iteration
};
struct mglThreadT
{
	void *a; 		// dual* or mreal* array with input or results
	double *b; 		// dual* array with input or results
	const long *p;	// long* array with parameters
	const void *v;	// pointer to table/parameter
	void **w; 		// pointer to workspace
	int id;			// thread id
	long n;			// total number of iteration
	const void *re,*im;
};
/// Start several thread for the task
void MGL_EXPORT mglStartThread(void *(*func)(void *), void (*post)(mglThreadD *,mreal *), long n,
					mreal *a=0, const mreal *b=0, const mreal *c=0, const long *p=0,
					const void *v=0, const mreal *d=0, const mreal *e=0, const char *s=0);
void MGL_EXPORT mglStartThreadV(void *(*func)(void *), long n, mreal *a, const void *b=0,
					const void *c=0, const long *p=0, const void *v=0, const mreal *d=0);
void MGL_EXPORT mglStartThreadV(void *(*func)(void *), long n, dual *a, const void *b=0,
					const void *c=0, const long *p=0, const void *v=0, const mreal *d=0);
void MGL_EXPORT mglStartThreadC(void *(*func)(void *), void (*post)(mglThreadC *,dual *), long n,
					dual *a=0, const dual *b=0, const dual *c=0, const long *p=0,
					const void *v=0, const dual *d=0, const dual *e=0, const char *s=0);
void MGL_EXPORT mglStartThreadT(void *(*func)(void *), long n, void *a, double *b, const void *v=0,
					void **w=0, const long *p=0, const void *re=0, const void *im=0);
MGL_EXPORT extern int mglNumThr;		///< Number of thread for plotting and data handling
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
