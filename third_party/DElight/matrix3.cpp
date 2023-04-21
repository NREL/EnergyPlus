// matrix3.cpp
// =================================================================================
// Copyright 1992-2009	Regents of University of California
//						Lawrence Berkeley National Laboratory

//  Authors: W.L. Carroll and R.J. Hitchcock
//           Building Technologies Department
//           Lawrence Berkeley National Laboratory

// This work was supported by the Assistant Secretary for Energy Efficiency 
// and Renewable Energy, Office of Building Technologies, 
// Building Systems and Materials Division of the 
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

// NOTICE: The Government is granted for itself and others acting on its behalf 
// a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce, 
// prepare derivative works, and perform publicly and display publicly. 
// Beginning five (5) years after (date permission to assert copyright was obtained),
// subject to two possible five year renewals, the Government is granted for itself 
// and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
// license in this data to reproduce, prepare derivative works, distribute copies to 
// the public, perform publicly and display publicly, and to permit others to do so. 
// NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
// THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL 
// LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
// INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE 
// WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
// =================================================================================

#include <cctype>
#include <iomanip>
#include <iostream>
#include <sstream>
using namespace std;

// writewndo() Error handler include
#include "DElightManagerC.h"

#include "DEF.H"
#include "CONST.H"
#include "vector2.h"
#include "point2.h"
#include "vector3.h"
#include "point3.h"
#include "matrix3.h"

namespace BldgGeomLib {

matrix3::matrix3(Double a, Double b, Double c,
           Double d, Double e, Double f,
           Double g, Double h, Double i)
{
    row[0][0] = a;  row[0][1] = b;  row[0][2] = c;
    row[1][0] = d;  row[1][1] = e;  row[1][2] = f;
    row[2][0] = g;  row[2][1] = h;  row[2][2] = i;
}

matrix3::matrix3(const matrix3 &m)
{
    row[0] = m[0];
    row[1] = m[1];
    row[2] = m[2];
}

matrix3 &matrix3::operator = (const matrix3 &m)
{
    row[0] = m[0];
    row[1] = m[1];
    row[2] = m[2];

    return(SELF);
}

matrix3 &matrix3::operator += (const matrix3 &m)
{
    row[0] += m[0];
    row[1] += m[1];
    row[2] += m[2];

    return(SELF);
}

matrix3 &matrix3::operator -= (const matrix3 &m)
{
    row[0] -= m[0];
    row[1] -= m[1];
    row[2] -= m[2];

    return(SELF);
}

matrix3 &matrix3::operator *= (const matrix3 &m)
{
    SELF = SELF * m;

    return(SELF);
}

matrix3 &matrix3::operator *= (Double s)
{
    row[0] *= s;
    row[1] *= s;
    row[2] *= s;

    return(SELF);
}

matrix3 &matrix3::operator /= (Double s)
{
    row[0] /= s;
    row[1] /= s;
    row[2] /= s;

    return(SELF);
}


Bool matrix3::operator < (const matrix3 &m) const
{
//	return(row[0] < m[0] && row[1] < m[1] && row[2] < m[2]);	//	eighth-space ordering: WRONG
    //	half-space ordering - OK	WLC
    return ( (row[0] < m[0]) || ((row[0] == m[0]) && (row[1] < m[1])) || ((row[0] == m[0]) && (row[1] == m[1]) && (row[2] < m[2])) );
}

Bool matrix3::operator == (const matrix3 &m) const
{
    return(row[0] == m[0] && row[1] == m[1] && row[2] == m[2]);
}

Bool matrix3::operator != (const matrix3 &m) const
{
    return(row[0] != m[0] || row[1] != m[1] || row[2] != m[2]);
}


matrix3 matrix3::operator + (const matrix3 &m) const
{
    matrix3 result;

    result[0] = row[0] + m[0];
    result[1] = row[1] + m[1];
    result[2] = row[2] + m[2];

    return(result);
}

matrix3 matrix3::operator - (const matrix3 &m) const
{
    matrix3 result;

    result[0] = row[0] - m[0];
    result[1] = row[1] - m[1];
    result[2] = row[2] - m[2];

    return(result);
}

matrix3 matrix3::operator - () const
{
    matrix3 result;

    result[0] = -row[0];
    result[1] = -row[1];
    result[2] = -row[2];

    return(result);
}

matrix3 matrix3::operator * (const matrix3 &m) const
{
#define N(x,y) row[x][y]
#define M(x,y) m[x][y]
#define R(x,y) result[x][y]

    matrix3 result;

    R(0,0) = N(0,0) * M(0,0) + N(0,1) * M(1,0) + N(0,2) * M(2,0);
    R(0,1) = N(0,0) * M(0,1) + N(0,1) * M(1,1) + N(0,2) * M(2,1);
    R(0,2) = N(0,0) * M(0,2) + N(0,1) * M(1,2) + N(0,2) * M(2,2);

    R(1,0) = N(1,0) * M(0,0) + N(1,1) * M(1,0) + N(1,2) * M(2,0);
    R(1,1) = N(1,0) * M(0,1) + N(1,1) * M(1,1) + N(1,2) * M(2,1);
    R(1,2) = N(1,0) * M(0,2) + N(1,1) * M(1,2) + N(1,2) * M(2,2);

    R(2,0) = N(2,0) * M(0,0) + N(2,1) * M(1,0) + N(2,2) * M(2,0);
    R(2,1) = N(2,0) * M(0,1) + N(2,1) * M(1,1) + N(2,2) * M(2,1);
    R(2,2) = N(2,0) * M(0,2) + N(2,1) * M(1,2) + N(2,2) * M(2,2);

    return(result);

#undef N
#undef M
#undef R
}

matrix3 matrix3::operator * (Double s) const
{
    matrix3 result;

    result[0] = row[0] * s;
    result[1] = row[1] * s;
    result[2] = row[2] * s;

    return(result);
}

matrix3 matrix3::operator / (Double s) const
{
    matrix3 result;

    result[0] = row[0] / s;
    result[1] = row[1] / s;
    result[2] = row[2] / s;

    return(result);
}

matrix3 trans(const matrix3 &m)
{
#define M(x,y) m[x][y]
#define R(x,y) result[x][y]

    matrix3 result;

    R(0,0) = M(0,0); R(0,1) = M(1,0); R(0,2) = M(2,0);
    R(1,0) = M(0,1); R(1,1) = M(1,1); R(1,2) = M(2,1);
    R(2,0) = M(0,2); R(2,1) = M(1,2); R(2,2) = M(2,2);

    return(result);

#undef M
#undef R
}

matrix3 adj(const matrix3 &m)
{
    matrix3    result;

    result[0] = cross(m[1], m[2]);
    result[1] = cross(m[2], m[0]);
    result[2] = cross(m[0], m[1]);

    return(result);
}


Double trace(const matrix3 &m)
{
    return(m[0][0] + m[1][1] + m[2][2]);
}

Double det(const matrix3 &m)
{
    return(dot(m[0], cross(m[1], m[2])));
}

matrix3 inv(const matrix3 &m)
{
    Double    mDet;
    matrix3    adjoint;
    matrix3    result;

    adjoint = adj(m);
    mDet = dot(adjoint[0], m[0]);

	if (mDet != 0) {
		result = trans(adjoint);
		result /= mDet;
	}
	else result.MakeZero();
	return(result);
}

matrix3 oprod(const vector3 &a, const vector3 &b)
// returns outerproduct of a and b:  a * trans(b)
{
    matrix3    result;

    result[0] = a[0] * b;
    result[1] = a[1] * b;
    result[2] = a[2] * b;

    return(result);
}

matrix3 Rot3(const vector3 &axis, Double theta)
//	NOTE:  vector{3/4} "axis" MUST BE UNIT LENGTH	//	WLC
{
	matrix3 result;
	result.MakeRot(axis, theta);
	return(result);
}


Void matrix3::MakeZero()
{
    Int     i;

    for (i = 0; i < 9; i++)
        ((Double*) row)[i] = vl_zero;
}

Void matrix3::MakeDiag(Double k)
{
    Int     i, j;

    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            if (i == j)
                row[i][j] = k;
            else
                row[i][j] = vl_zero;
}

Void matrix3::MakeBlock(Double k)
{
    Int     i;

    for (i = 0; i < 9; i++)
        ((Double *) row)[i] = k;
}

//	NOTE:  vector{3/4} "axis" MUST BE UNIT LENGTH	//	WLC
matrix3 &matrix3::MakeRot(const vector3 &axis, Double theta)
{
	//	form quaternion
    Double            s;
    double            q[4];

    theta /= 2.0;
    s = sin(theta);

    q[0] = s * axis[0];
    q[1] = s * axis[1];
    q[2] = s * axis[2];
    q[3] = cos(theta);

//	do rotation in quaternion space and load matrix3 with result
   Double    i2 =  2 * q[0],
            j2 =  2 * q[1],
            k2 =  2 * q[2],
            ij = i2 * q[1],
            ik = i2 * q[2],
            jk = j2 * q[2],
            ri = i2 * q[3],
            rj = j2 * q[3],
            rk = k2 * q[3];

    i2 *= q[0];
    j2 *= q[1];
    k2 *= q[2];

    row[0][0] = 1 - j2 - k2;  row[0][1] = ij - rk    ;  row[0][2] = ik + rj;
    row[1][0] = ij + rk    ;  row[1][1] = 1 - i2 - k2;  row[1][2] = jk - ri;
    row[2][0] = ik - rj    ;  row[2][1] = jk + ri    ;  row[2][2] = 1 - i2 - j2;

    return(*this);
}

} //	end namespace BldgGeomLib;

using namespace BldgGeomLib;

ostream &operator << (ostream &s, const matrix3 &m)
{
    Int     w = s.width();

    return(s << '[' << m[0] << endl << setw(w) << m[1] << endl << setw(w)
           << m[2] << ']' << endl);
}

istream &operator >> (istream &s, matrix3 &m)	//	WLC 06/06/2003 major rewrite
{
    matrix3	result;
    Char	c;
    std::ostringstream osstream;
	
	// Expected format: [[1 2 3] [4 5 6] [7 8 9]]
	// Each vector is a column of the matrix.
	
    while (s >> c && isspace(c)) {;} // skip through spaces
	if (s.eof()) return(s);
	if (s.fail()) {
//		cerr << "matrix3:ReadError1: unrecoverable failbit\n";
		osstream << "matrix3:ReadError1: unrecoverable failbit\n";
		writewndo(osstream.str(),"e");
		return(s);
	}
		
    if (c != '[') {
		s.putback(c);
	    s.clear(ios::failbit);
	    return(s);
	}
	//	else ...
	s >> result[0] >> result[1] >> result[2];	

	if (!s)	{
//		cerr << "matrix3:ReadError2: Expected number\n";
		osstream << "matrix3:ReadError2: Expected number\n";
		writewndo(osstream.str(),"e");
		return(s);
	}
		
	while (s >> c && isspace(c));
	if (c != ']')	{
    	s.clear(ios::failbit);
//	    cerr << "matrix3:ReadError3: Expected ']' - got \'" << c << "\'" << "\n";
		osstream << "matrix3:ReadError3: Expected ']' - got \'" << c << "\'" << "\n";
		writewndo(osstream.str(),"e");
	    return(s);
    }
	
	m = result;
    return(s);
}


