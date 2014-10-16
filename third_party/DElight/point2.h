// point2.h
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

class vector2;

class point2
{
public:

    // Constructors

                point2();
                point2(Double x, Double y);       // (x, y)
                point2(const point2 &p);        // Copy constructor
                point2(ZeroOrOne k);          // p[i] = vl_zero
                point2(Axis k);               // p[k] = 1

    // Accessor functions

    Double        &operator [] (Int i);
    const Double  &operator [] (Int i) const;

    Int         Elts() const { return(2); };
    Double        *Ref() const;                       // Return ptr to data

	// Assignment functions
	point2		Set(Double x, Double y); // [x, y]	// WLC
	
    // Assignment operators

    point2        &operator =  (const point2 &a);
    point2        &operator =  (ZeroOrOne k);
    point2        &operator =  (Axis k);

    point2        &operator += (const vector2 &a);
    point2        &operator -= (const vector2 &a);
    point2        &operator *= (Double s);
    point2        &operator /= (Double s);

    // Comparison operators

    Bool        operator < (const point2 &a) const;  // p < a?		WLC
    Bool        operator == (const point2 &a) const;  // p == a?
    Bool        operator != (const point2 &a) const;  // p != a?

    // Arithmetic operators

    point2        operator + (const vector2 &a) const;   // p + v
    point2        operator - (const vector2 &a) const;   // p - v
    vector2       operator - (const point2 &a) const;   // v = b - a
    point2        operator - () const;                // -p
    point2        operator * (Double s) const;          // p * s
    point2        operator / (Double s) const;          // p / s

    // Initialisers

    point2        &MakeZero();                        // Zero vector
    point2        &MakeUnit(Int i, Double k = vl_one);  // I[i]
    point2        &MakeBlock(Double k = vl_one);        // All-k vector

    // Private...

protected:

    Double            elt[2];
};

// --- point2 operators ----------------------------------------------------------

inline point2     operator * (Double s, const point2 &p); // s * p
inline Double     dist(const point2 &a, const point2 &b); // || b - a ||
inline Double     sqrdist(const point2 &a, const point2 &b); // || b - a ||**2


// --- Inlines ----------------------------------------------------------------

inline Double &point2::operator [] (Int i)
{
    return(elt[i]);
}

inline const Double &point2::operator [] (Int i) const
{
    return(elt[i]);
}

inline point2::point2()
{
    elt[0] = 0;
    elt[1] = 0;
}

inline point2::point2(Double x, Double y)
{
    elt[0] = x;
    elt[1] = y;
}

inline point2::point2(const point2 &p)
{
    elt[0] = p[0];
    elt[1] = p[1];
}


inline Double *point2::Ref() const
{
    return((Double *) elt);
}

inline point2 &point2::operator = (const point2 &p)
{
    elt[0] = p[0];
    elt[1] = p[1];

    return(SELF);
}

inline point2 &point2::operator += (const vector2 &p)
{
    elt[0] += p[0];
    elt[1] += p[1];

    return(SELF);
}

inline point2 &point2::operator -= (const vector2 &p)
{
    elt[0] -= p[0];
    elt[1] -= p[1];

    return(SELF);
}

inline point2 &point2::operator *= (Double s)
{
    elt[0] *= s;
    elt[1] *= s;

    return(SELF);
}

inline point2 &point2::operator /= (Double s)
{
    elt[0] /= s;
    elt[1] /= s;

    return(SELF);
}

inline point2 point2::operator + (const vector2 &a) const
{
	point2 result;
	
	result[0] = elt[0] + a[0];
	result[1] = elt[1] + a[1];
	
	return(result);
}

inline point2 point2::operator - (const vector2 &a) const
{
	point2 result;
	
	result[0] = elt[0] - a[0];
	result[1] = elt[1] - a[1];
	
	return(result);
}

inline vector2 point2::operator - (const point2 &a) const	// vec2 b - a
{
	vector2 result;
	
	result[0] = elt[0] - a[0];
	result[1] = elt[1] - a[1];
	
	return(result);
}

inline point2 point2::operator - () const
{
    point2 result;

    result[0] = -elt[0];
    result[1] = -elt[1];

    return(result);
}

inline point2 point2::operator * (Double s) const
{
    point2 result;

    result[0] = elt[0] * s;
    result[1] = elt[1] * s;

    return(result);
}

inline point2 operator * (Double s, const point2 &p)
{
    return(p * s);
}

inline point2 point2::operator / (Double s) const
{
    point2 result;

    result[0] = elt[0] / s;
    result[1] = elt[1] / s;

    return(result);
}

inline Double dot(const point2 &a, const point2 &b)
{
    return(a[0] * b[0] + a[1] * b[1]);
}

inline point2 cross(const point2 &a)
{
    point2 result;

    result[0] =  a[1];
    result[1] = -a[0];

    return(result);
}

inline Double	sqrdist(const point2 &a, const point2 &b)				// || b - a ||**2
{
	return((b[0] - a[0])*(b[0] - a[0]) + (b[1] - a[1])*(b[1] - a[1]));
}

inline Double	dist(const point2 &a, const point2 &b)				// || b - a ||
{
	return(sqrt(sqrdist(a, b)));
}

inline point2 &point2::MakeUnit(Int i, Double k)
{
    if (i == 0)
    { elt[0] = k; elt[1] = vl_zero; }
    else if (i == 1)
    { elt[0] = vl_zero; elt[1] = k; }
    else
    { elt[0] = 0; elt[1] = 0; }
    return(SELF);
}

inline point2 &point2::MakeZero()
{
    elt[0] = vl_zero; elt[1] = vl_zero;
    return(SELF);
}

inline point2 &point2::MakeBlock(Double k)
{
    elt[0] = k; elt[1] = k;
    return(SELF);
}

inline point2::point2(ZeroOrOne k)
{
    elt[0] = k;
    elt[1] = k;
}

inline point2::point2(Axis k)
{
    MakeUnit(k, vl_one);
}

inline point2 &point2::operator = (ZeroOrOne k)
{
    elt[0] = k; elt[1] = k;

    return(SELF);
}

inline point2 &point2::operator = (Axis k)
{
    MakeUnit(k, vl_1);

    return(SELF);
}

inline Bool point2::operator < (const point2 &a) const	//	WLC
{
    return ( (elt[0] < a[0]) || ((elt[0] == a[0]) && (elt[1] < a[1])) );
}

inline Bool point2::operator == (const point2 &a) const
{
    return(elt[0] == a[0] && elt[1] == a[1]);
}

inline Bool point2::operator != (const point2 &a) const
{
    return(elt[0] != a[0] || elt[1] != a[1]);
}

}	//	end namespace BldgGeomLib

std::ostream &operator << (std::ostream &s, const BldgGeomLib::point2 &p);
std::istream &operator >> (std::istream &s, BldgGeomLib::point2 &p);

