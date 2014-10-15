// vector2.h
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
#include <cmath>

namespace BldgGeomLib {

class point2;

class vector2
{
public:

    // Constructors

                vector2();
                vector2(Double x, Double y);       // (x, y)
                vector2(const vector2 &v);        // Copy constructor
				vector2(const point2 &a, const point2 &b);	// WLC			    
                vector2(ZeroOrOne k);          // v[i] = vl_zero
                vector2(Axis k);               // v[k] = 1

    // Accessor functions

    Double        &operator [] (Int i);
    const Double  &operator [] (Int i) const;

    Int         Elts() const { return(2); };
    Double        *Ref() const;                       // Return ptr to data

    // Assignment operators

    vector2        &operator =  (const vector2 &a);
    vector2        &operator =  (ZeroOrOne k);
    vector2        &operator =  (Axis k);

    vector2        &operator += (const vector2 &a);
    vector2        &operator -= (const vector2 &a);
    vector2        &operator *= (const vector2 &a);
    vector2        &operator *= (Double s);
    vector2        &operator /= (const vector2 &a);
    vector2        &operator /= (Double s);

    // Comparison operators

	Bool		operator <  (const vector2 &a) const;	// v < a?  WLC
    Bool        operator == (const vector2 &a) const;  // v == a?
    Bool        operator != (const vector2 &a) const;  // v != a?

    // Arithmetic operators

    vector2        operator + (const vector2 &a) const;   // v + a
    vector2        operator - (const vector2 &a) const;   // v - a
    vector2        operator - () const;                // -v
    vector2        operator * (const vector2 &a) const;   // v * a (vx * ax, ...)
    vector2        operator * (Double s) const;          // v * s
    vector2        operator / (const vector2 &a) const;   // v / a (vx / ax, ...)
    vector2        operator / (Double s) const;          // v / s

    vector2        &MakeZero();                        // Zero vector
    vector2        &MakeUnit(Int i, Double k = vl_one);  // I[i]
    vector2        &MakeBlock(Double k = vl_one);        // All-k vector

    // Private...

protected:

    Double            elt[2];
};

// --- Vec operators ----------------------------------------------------------

inline vector2     operator * (Double s, const vector2 &v); // s * v
inline Double     dot(const vector2 &a, const vector2 &b);  // v . a
inline Double     len(const vector2 &v);                 // || v ||
inline Double     sqrlen(const vector2 &v);              // v . v
inline vector2     norm(const vector2 &v);                // v / || v ||
inline Void     normalize(vector2 &v);                 // v = norm(v)
inline vector2     cross(const vector2 &v);               // cross prod.


// --- Inlines ----------------------------------------------------------------

inline Double &vector2::operator [] (Int i)
{
    return(elt[i]);
}

inline const Double &vector2::operator [] (Int i) const
{
    return(elt[i]);
}

inline vector2::vector2()
{
}

inline vector2::vector2(Double x, Double y)
{
    elt[0] = x;
    elt[1] = y;
}

inline vector2::vector2(const vector2 &v)
{
    elt[0] = v[0];
    elt[1] = v[1];
}

inline Double *vector2::Ref() const
{
    return((Double *) elt);
}

inline vector2 &vector2::operator = (const vector2 &v)
{
    elt[0] = v[0];
    elt[1] = v[1];

    return(SELF);
}

inline vector2 &vector2::operator += (const vector2 &v)
{
    elt[0] += v[0];
    elt[1] += v[1];

    return(SELF);
}

inline vector2 &vector2::operator -= (const vector2 &v)
{
    elt[0] -= v[0];
    elt[1] -= v[1];

    return(SELF);
}

inline vector2 &vector2::operator *= (const vector2 &v)
{
    elt[0] *= v[0];
    elt[1] *= v[1];

    return(SELF);
}

inline vector2 &vector2::operator *= (Double s)
{
    elt[0] *= s;
    elt[1] *= s;

    return(SELF);
}

inline vector2 &vector2::operator /= (const vector2 &v)
{
    elt[0] /= v[0];
    elt[1] /= v[1];

    return(SELF);
}

inline vector2 &vector2::operator /= (Double s)
{
    elt[0] /= s;
    elt[1] /= s;

    return(SELF);
}

inline vector2 vector2::operator + (const vector2 &a) const
{
    vector2 result;

    result[0] = elt[0] + a[0];
    result[1] = elt[1] + a[1];

    return(result);
}

inline vector2 vector2::operator - (const vector2 &a) const
{
    vector2 result;

    result[0] = elt[0] - a[0];
    result[1] = elt[1] - a[1];

    return(result);
}

inline vector2 vector2::operator - () const
{
    vector2 result;

    result[0] = -elt[0];
    result[1] = -elt[1];

    return(result);
}

inline vector2 vector2::operator * (const vector2 &a) const
{
    vector2 result;

    result[0] = elt[0] * a[0];
    result[1] = elt[1] * a[1];

    return(result);
}

inline vector2 vector2::operator * (Double s) const
{
    vector2 result;

    result[0] = elt[0] * s;
    result[1] = elt[1] * s;

    return(result);
}

inline vector2 operator * (Double s, const vector2 &v)
{
    return(v * s);
}

inline vector2 vector2::operator / (const vector2 &a) const
{
    vector2 result;

    result[0] = elt[0] / a[0];
    result[1] = elt[1] / a[1];

    return(result);
}

inline vector2 vector2::operator / (Double s) const
{
    vector2 result;

    result[0] = elt[0] / s;
    result[1] = elt[1] / s;

    return(result);
}

inline Double dot(const vector2 &a, const vector2 &b)
{
    return(a[0] * b[0] + a[1] * b[1]);
}

inline vector2 cross(const vector2 &a)
{
    vector2 result;

    result[0] =  a[1];
    result[1] = -a[0];

    return(result);
}

inline Double len(const vector2 &v)
{
    return(sqrt(dot(v, v)));
}

inline Double sqrlen(const vector2 &v)
{
    return(dot(v, v));
}

inline vector2 norm(const vector2 &v)
{
    if(sqrlen(v) > 0.0)    return(v / len(v));
    return(vector2(0,0));
}

inline Void normalize(vector2 &v)
{
    if(sqrlen(v) > 0.0) v /= len(v);
}

inline vector2 &vector2::MakeUnit(Int i, Double k)
{
    if (i == 0)
    { elt[0] = k; elt[1] = vl_zero; }
    else if (i == 1)
    { elt[0] = vl_zero; elt[1] = k; }
    else
	{elt[0] = 0; elt[1] = 0;}
    return(SELF);
}

inline vector2 &vector2::MakeZero()
{
    elt[0] = vl_zero; elt[1] = vl_zero;
    return(SELF);
}

inline vector2 &vector2::MakeBlock(Double k)
{
    elt[0] = k; elt[1] = k;
    return(SELF);
}

inline vector2::vector2(ZeroOrOne k)
{
    elt[0] = k;
    elt[1] = k;
}

inline vector2::vector2(Axis k)
{
    MakeUnit(k, vl_one);
}

inline vector2 &vector2::operator = (ZeroOrOne k)
{
    elt[0] = k; elt[1] = k;

    return(SELF);
}

inline vector2 &vector2::operator = (Axis k)
{
    MakeUnit(k, vl_1);

    return(SELF);
}

inline Bool vector2::operator < (const vector2 &a) const	//	WLC
{
    return( (elt[0] < a[0]) || ((elt[0] == a[0]) && (elt[1] < a[1])) );
}

inline Bool vector2::operator == (const vector2 &a) const
{
    return(elt[0] == a[0] && elt[1] == a[1]);
}

inline Bool vector2::operator != (const vector2 &a) const
{
    return(elt[0] != a[0] || elt[1] != a[1]);
}

}	//	end namespace BldgGeomLib

std::ostream &operator << (std::ostream &s, const BldgGeomLib::vector2 &v);
std::istream &operator >> (std::istream &s, BldgGeomLib::vector2 &v);

