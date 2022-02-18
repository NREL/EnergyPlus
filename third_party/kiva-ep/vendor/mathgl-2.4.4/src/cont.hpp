/***************************************************************************
 * cont.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2014 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
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
//-----------------------------------------------------------------------------
struct mglSegment
{
	mglPoint p1,p2;	// edges
	std::list<mglPoint> pp;
	bool set(mreal u1,mreal v1,mreal u2,mreal v2,long i,long j,long k,HCDT x, HCDT y, HCDT z)
	{
		bool res=(v1>=0 && v1<=MGL_FEPSILON && u1>=0 && u1<=MGL_FEPSILON && v2>=0 && v2<=MGL_FEPSILON && u2>=0 && u2<=MGL_FEPSILON);
		if(v1==v2 && u1==u2)	res=false;	// NOTE: shouldn't be here never
		if(res)
		{
			p1.Set(mgl_data_linear(x,i+u1,j+v1,k), mgl_data_linear(y,i+u1,j+v1,k), mgl_data_linear(z,i+u1,j+v1,k));
			p2.Set(mgl_data_linear(x,i+u2,j+v2,k), mgl_data_linear(y,i+u2,j+v2,k), mgl_data_linear(z,i+u2,j+v2,k));
		}
		return res;
	}
	void before(const mglPoint &p)	{	p1 = p;	pp.push_front(p);	}
	void after(const mglPoint &p)	{	p2 = p;	pp.push_back(p);	}
};
//-----------------------------------------------------------------------------
std::vector<mglSegment> MGL_EXPORT mgl_get_curvs(HMGL gr, std::vector<mglSegment> lines);
void MGL_NO_EXPORT mgl_draw_curvs(HMGL gr, mreal val, mreal c, int text, const std::vector<mglSegment> &curvs);
//-----------------------------------------------------------------------------
