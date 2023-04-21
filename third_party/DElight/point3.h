// point3.h
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

class vector3;
class point3
{
public:

    // Constructors

                point3();
                point3(Double x, Double y, Double z);   // [x, y, z]
                point3(const point3 &p);            // Copy constructor
                point3(ZeroOrOne k);
                point3(Axis a);

    // Accessor functions

    Int         Elts() const { return(3); };

    Double        &operator [] (Int i);
    const Double  &operator [] (Int i) const;

    Double        *Ref() const;                   // Return pointer to data

	// Assignment functions
	point3		Set(Double x, Double y, Double z); // [x, y, z]	// WLC
	
    // Assignment operators

    point3        &operator =  (const point3 &a);
    point3        &operator =  (const vector3 &a);	//	WLC
    point3        &operator =  (ZeroOrOne k);
    point3        &operator += (const vector3 &a);
    point3        &operator -= (const vector3 &a);
    point3        &operator *= (Double s);
    point3        &operator /= (Double s);

    // Comparison operators

    Bool        operator == (const point3 &a) const;  // p == a?
    Bool        operator != (const point3 &a) const;  // p != a?
    Bool        operator <  (const point3 &a) const; // p <  a?
    Bool        operator >= (const point3 &a) const; // p >= a?

    // Arithmetic operators

    point3        operator + (const vector3 &a) const;   // p + v			//	WLC
    point3        operator - (const vector3 &a) const;   // p - v			//	WLC
	vector3	  	  operator - (const point3 &a) const;		// vec3 b - a	//	WLC
    point3        operator - () const;                // -p
    point3        operator * (Double s) const;          // p * s
    point3        operator / (Double s) const;          // p / s

    // Initialisers

    point3        &MakeZero();                        // Zero point
    point3        &MakeUnit(Int i, Double k = vl_one);  // I[i]
    point3        &MakeBlock(Double k = vl_one);        // All-k point

    // Private...

protected:

    Double elt[3];
};


// --- Vec operators ----------------------------------------------------------

inline point3     operator * (Double s, const point3 &p); // s * p
inline Double	dist(const point3 &a, const point3 &b);		// || b - a ||		WLC
inline Double	sqrdist(const point3 &a, const point3 &b);	// || b - a ||**2	WLC

// --- Inlines ----------------------------------------------------------------

inline Double &point3::operator [] (Int i)
{
    return(elt[i]);
}

inline const Double &point3::operator [] (Int i) const
{
    return(elt[i]);
}

inline point3::point3()
{
}

inline point3::point3(Double x, Double y, Double z)
{
    elt[0] = x;
    elt[1] = y;
    elt[2] = z;
}

inline point3::point3(const point3 &p)
{
    elt[0] = p[0];
    elt[1] = p[1];
    elt[2] = p[2];
}

inline Double *point3::Ref() const
{
    return((Double *) elt);
}

inline point3 point3::Set(Double x, Double y, Double z)
{
	elt[0] = x;
	elt[1] = y;
	elt[2] = z;
	
	return(SELF);
}

inline point3 &point3::operator = (const point3 &p)
{
    elt[0] = p[0];
    elt[1] = p[1];
    elt[2] = p[2];

    return(SELF);
}

inline point3 &point3::operator += (const vector3 &p)
{
    elt[0] += p[0];
    elt[1] += p[1];
    elt[2] += p[2];

    return(SELF);
}

inline point3 &point3::operator -= (const vector3 &p)
{
    elt[0] -= p[0];
    elt[1] -= p[1];
    elt[2] -= p[2];

    return(SELF);
}
/*
*/

inline point3 &point3::operator *= (Double s)
{
    elt[0] *= s;
    elt[1] *= s;
    elt[2] *= s;

    return(SELF);
}

inline point3 &point3::operator /= (Double s)
{
    elt[0] /= s;
    elt[1] /= s;
    elt[2] /= s;

    return(SELF);
}

inline point3 point3::operator + (const vector3 &a) const	//	WLC
{
	point3 result;
	
	result[0] = elt[0] + a[0];
	result[1] = elt[1] + a[1];
	result[2] = elt[2] + a[2];
	
	return(result);
}

inline point3 point3::operator - (const vector3 &a) const	//	WLC
{
	point3 result;
	
	result[0] = elt[0] - a[0];
	result[1] = elt[1] - a[1];
	result[2] = elt[2] - a[2];
	
	return(result);
}

inline vector3 point3::operator - (const point3 &a) const	// vec3 b - a	WLC
{
	vector3 result;
	
	result[0] = elt[0] - a[0];
	result[1] = elt[1] - a[1];
	result[2] = elt[2] - a[2];
	
	return(result);
}

inline point3 point3::operator - () const
{
    point3 result;

    result[0] = -elt[0];
    result[1] = -elt[1];
    result[2] = -elt[2];

    return(result);
}

inline point3 point3::operator * (Double s) const
{
    point3 result;

    result[0] = elt[0] * s;
    result[1] = elt[1] * s;
    result[2] = elt[2] * s;

    return(result);
}

inline point3 point3::operator / (Double s) const
{
    point3 result;

    result[0] = elt[0] / s;
    result[1] = elt[1] / s;
    result[2] = elt[2] / s;

    return(result);
}

inline point3 operator * (Double s, const point3 &p)
{
    return(p * s);
}

inline point3 &point3::MakeUnit(Int n, Double k)
{
    if (n == 0)
    { elt[0] = k; elt[1] = vl_zero; elt[2] = vl_zero; }
    else if (n == 1)
    { elt[0] = vl_zero; elt[1] = k; elt[2] = vl_zero; }
    else if (n == 2)
    { elt[0] = vl_zero; elt[1] = vl_zero; elt[2] = k; }
    else
    { elt[0] = 0; elt[1] = 0; elt[2] = 0; }

    return(SELF);
}

inline point3 &point3::MakeZero()
{
    elt[0] = vl_zero; elt[1] = vl_zero; elt[2] = vl_zero;

    return(SELF);
}

inline point3 &point3::MakeBlock(Double k)
{
    elt[0] = k; elt[1] = k; elt[2] = k;

    return(SELF);
}

inline point3::point3(ZeroOrOne k)
{
    elt[0] = k; elt[1] = k; elt[2] = k;
}

inline point3 &point3::operator = (ZeroOrOne k)
{
    elt[0] = k; elt[1] = k; elt[2] = k;

    return(SELF);
}

inline point3::point3(Axis a)
{
    MakeUnit(a, vl_one);
}


inline Bool point3::operator == (const point3 &a) const
{
    return(elt[0] == a[0] && elt[1] == a[1] && elt[2] == a[2]);
}

inline Bool point3::operator != (const point3 &a) const
{
    return(elt[0] != a[0] || elt[1] != a[1] || elt[2] != a[2]);
}

inline Bool point3::operator < (const point3 &a) const
{
//	return(elt[0] < a[0] && elt[1] < a[1] && elt[2] < a[2]);	//	eigth-space ordering - WRONG
    //	half-space ordering - WLC
    return ( (elt[0] < a[0]) || ((elt[0] == a[0]) && (elt[1] < a[1])) || ((elt[0] == a[0]) && (elt[1] == a[1]) && (elt[2] < a[2])) );
}

inline Bool point3::operator >= (const point3 &a) const
{
    return(elt[0] >= a[0] && elt[1] >= a[1] && elt[2] >= a[2]);
}

// WLC
inline Double	sqrdist(const point3 &a, const point3 &b)				// || b - a ||**2
{
	return((b[0] - a[0])*(b[0] - a[0]) + (b[1] - a[1])*(b[1] - a[1]) + (b[2] - a[2])*(b[2] - a[2]));
}

// WLC
inline Double	dist(const point3 &a, const point3 &b)				// || b - a ||
{
	return(sqrt(sqrdist(a, b)));
}

}	//	end namespace BldgGeomLib

std::ostream &operator << (std::ostream &s, const BldgGeomLib::point3 &p);
std::istream &operator >> (std::istream &s, BldgGeomLib::point3 &p);

