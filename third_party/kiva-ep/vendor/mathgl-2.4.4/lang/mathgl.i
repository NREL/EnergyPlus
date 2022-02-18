/***************************************************************************
 * mgl.i is part of Math Graphic Library
 * Copyright (C) 2007 Alexey Balakin <balakin@appl.sci-nnov.ru>,
 *   Xavier Delacour <xavier.delacour@gmail.com>,
 *   Alexander Filov <alexander.filov@gmail.com>                           *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/


%module mathgl
#ifdef SWIGOCTAVE
%feature("autodoc", 1);
#endif // SWIGOCTAVE

%ignore operator!;
%ignore operator=;
%ignore *::operator=;
%ignore *::operator+=;
%ignore *::operator-=;
%ignore *::operator*=;
%ignore *::operator/=;
//%ignore mglDataA

%{
#define SWIG_FILE_WITH_INIT
#include "mgl2/type.h"
#include "mgl2/data.h"
#include "mgl2/mgl.h"
const double All = -1;
const double Pi = M_PI;
const double NaN = NAN;
const double Inf = INFINITY;
%}

#if MGL_USE_DOUBLE
typedef double mreal;
#else
typedef float mreal;
#endif


#ifdef SWIGOCTAVE
%rename(__add) operator+;
%rename(__sub) operator-;
%rename(__mul) operator*;
%rename(__div) operator/;
%rename(__eq) operator==;
%rename(__ne) operator!=;
%typemap(in,noblock=1) (double* d, int rows, int cols) (Matrix tmp) {
	if (!$input.is_matrix_type())	{	error("A must be a matrix");	SWIG_fail;	}
	tmp=$input.matrix_value();
	$1=tmp.data();
	$2=tmp.rows();
	$3=tmp.columns();
}
#endif

#ifdef SWIGPYTHON
%rename(__add__) *::operator+(const mglData&, const mglData&);
%rename(__sub__) *::operator-(const mglData&, const mglData &);
%rename(__mul__) *::operator*(const mglData &, float);
%rename(__div__) *::operator/(const mglData &, float);

// Get the NumPy typemaps
%include "numpy.i"
%init %{
import_array();
%}
%apply (double* IN_ARRAY1, int DIM1) {(const double* d, int size)};
%apply (int DIM1, double* IN_ARRAY1) {(int size, const double* d)};
%apply (double* IN_ARRAY2, int DIM1, int DIM2) {(const double* d, int rows, int cols)};
%apply (int DIM1, int DIM2, double* IN_ARRAY2) {(int rows, int cols, const double* d)};
%apply (double* IN_ARRAY3, int DIM1, int DIM2, int DIM3) {(const double* d, int rows, int cols, int slc)};
%apply (int DIM1, int DIM2, int DIM3, double* IN_ARRAY3) {(int rows, int cols, int slc, const double* d)};
#endif

%include "type.i"
%include "data.i"
%include "mgl.i"
%extend mglData
{
	mreal __getitem__( int i)	{	return self->GetVal(i);	};
	mreal __paren( int i)		{	return self->GetVal(i);	};
	void __setitem__( int i, mreal y)	{	self->SetVal(y,i);	};
	void __paren_asgn( int i, mreal y)	{	self->SetVal(y,i);	};
};
