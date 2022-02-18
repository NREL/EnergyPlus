/***************************************************************************
 * eval.h is part of Math Graphic Library
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
#ifndef _MGL_EVAL_H_
#define _MGL_EVAL_H_
//-----------------------------------------------------------------------------
#include "mgl2/abstract.h"
/// types of errors
#define MGL_ERR_LOG		1
#define MGL_ERR_ARC		2
#define MGL_ERR_SQRT	3
class mglData;
//-----------------------------------------------------------------------------
/// Class for evaluating formula specified by the string
class MGL_EXPORT mglFormula
{
public:
	/// Evaluates the formula for 'x','r'=\a x, 'y','n'=\a y, 'z','t'=\a z, 'u'=\a u
	mreal Calc(mreal x,mreal y=0,mreal z=0,mreal u=0) const MGL_FUNC_PURE;
	/// Evaluates the formula for 'x, y, z, u, v, w'
	mreal Calc(mreal x,mreal y,mreal z,mreal u,mreal v,mreal w) const MGL_FUNC_PURE;
	/// Evaluates the formula for variables var
	mreal Calc(const mreal var[MGL_VS]) const MGL_FUNC_PURE;
	/// Evaluates the formula for 'x','r'=\a x, 'y','n'=\a y, 'z','t'=\a z, 'u'=\a u
	mreal CalcD(char diff, mreal x,mreal y=0,mreal z=0,mreal u=0) const MGL_FUNC_PURE;
	/// Evaluates the formula for 'x, y, z, u, v, w'
	mreal CalcD(char diff, mreal x,mreal y,mreal z,mreal u,mreal v,mreal w) const MGL_FUNC_PURE;
	/// Evaluates the derivates of the formula for variables var respect to variable diff
	mreal CalcD(const mreal var[MGL_VS], char diff) const MGL_FUNC_PURE;
	/// Return error code
	inline int GetError() const	{	return Error;	}
	/// Parse the formula str and create formula-tree
	mglFormula(const char *str);
	/// Set data for the spline interpolation
	mglFormula(HCDT d, mreal x1=0, mreal x2=1, mreal y1=0, mreal y2=1, mreal z1=0, mreal z2=1) : 
		dat(d),dx1(x1),dx2(x2),dy1(y1),dy2(y2),dz1(z1),dz2(z2),tmp(NULL)	{};
	/// Clean up formula-tree
	~mglFormula();
protected:
	mreal CalcIn(const mreal *a1) const MGL_FUNC_PURE;
	mreal CalcDIn(int id, const mreal *a1) const MGL_FUNC_PURE;
	mglFormula *Left,*Right;	// first and second argument of the function
	int Kod;					// the function ID
	mreal Res;					// the number or the variable ID
	HCDT dat;				// data file for the interpolation
	mreal dx1,dx2,dy1,dy2,dz1,dz2;	// ranges of data files
	static int Error;
private:
	mglData *tmp;
};
//-----------------------------------------------------------------------------
#endif
