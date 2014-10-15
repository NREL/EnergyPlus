// vector3.h
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

class point3;
class vector3
{
public:

    // Constructors

                vector3();
                vector3(Double x, Double y, Double z);   // [x, y, z]
                vector3(const vector3 &v);            // Copy constructor
				vector3(const point3 &a, const point3 &b);	// WLC			    
                vector3(ZeroOrOne k);
                vector3(Axis a);

    // Accessor functions

    Int         Elts() const { return(3); };

    Double        &operator [] (Int i);
    const Double  &operator [] (Int i) const;

    Double        *Ref() const;                   // Return pointer to data

    // Assignment operators

    vector3        &operator =  (const vector3 &a);
    vector3        &operator =  (ZeroOrOne k);
    vector3        &operator += (const vector3 &a);
    vector3        &operator -= (const vector3 &a);
    vector3        &operator *= (const vector3 &a);
    vector3        &operator *= (Double s);
    vector3        &operator /= (const vector3 &a);
    vector3        &operator /= (Double s);

    // Comparison operators

    Bool        operator == (const vector3 &a) const;  // v == a?
    Bool        operator != (const vector3 &a) const;  // v != a?
    Bool        operator <  (const vector3 &a) const; // v <  a?
    Bool        operator >= (const vector3 &a) const; // v >= a?

    // Arithmetic operators

    vector3        operator + (const vector3 &a) const;   // v + a
    vector3        operator - (const vector3 &a) const;   // v - a
    vector3        operator - () const;                // -v
    vector3        operator * (const vector3 &a) const;   // v * a (vx * ax, ...)
    vector3        operator * (Double s) const;          // v * s
    vector3        operator / (const vector3 &a) const;   // v / a (vx / ax, ...)
    vector3        operator / (Double s) const;          // v / s

    vector3        &MakeZero();                        // Zero vector
    vector3        &MakeUnit(Int i, Double k = vl_one);  // I[i]
    vector3        &MakeBlock(Double k = vl_one);        // All-k vector

protected:

    Double elt[3];
};


// --- Vec operators ----------------------------------------------------------

inline vector3     operator * (Double s, const vector3 &v); // s * v
inline Double     dot(const vector3 &a, const vector3 &b);  // v . a
inline Double     len(const vector3 &v);                 // || v ||
inline Double     sqrlen(const vector3 &v);              // v . v
inline vector3     norm(const vector3 &v);                // v / || v ||
inline Void     normalize(vector3 &v);                 // v = norm(v)
inline vector3     cross(const vector3 &a, const vector3 &b);// a x b
inline vector2     proj(const vector3 &v);                // hom. projection


// --- Inlines ----------------------------------------------------------------

inline Double &vector3::operator [] (Int i)
{
    return(elt[i]);
}

inline const Double &vector3::operator [] (Int i) const
{
    return(elt[i]);
}

inline vector3::vector3()
{
}

inline vector3::vector3(Double x, Double y, Double z)
{
    elt[0] = x;
    elt[1] = y;
    elt[2] = z;
}

inline vector3::vector3(const vector3 &v)
{
    elt[0] = v[0];
    elt[1] = v[1];
    elt[2] = v[2];
}

inline Double *vector3::Ref() const
{
    return((Double *) elt);
}

inline vector3 &vector3::operator = (const vector3 &v)
{
    elt[0] = v[0];
    elt[1] = v[1];
    elt[2] = v[2];

    return(SELF);
}

inline vector3 &vector3::operator += (const vector3 &v)
{
    elt[0] += v[0];
    elt[1] += v[1];
    elt[2] += v[2];

    return(SELF);
}

inline vector3 &vector3::operator -= (const vector3 &v)
{
    elt[0] -= v[0];
    elt[1] -= v[1];
    elt[2] -= v[2];

    return(SELF);
}

inline vector3 &vector3::operator *= (const vector3 &a)
{
    elt[0] *= a[0];
    elt[1] *= a[1];
    elt[2] *= a[2];

    return(SELF);
}

inline vector3 &vector3::operator *= (Double s)
{
    elt[0] *= s;
    elt[1] *= s;
    elt[2] *= s;

    return(SELF);
}

inline vector3 &vector3::operator /= (const vector3 &a)
{
    elt[0] /= a[0];
    elt[1] /= a[1];
    elt[2] /= a[2];

    return(SELF);
}

inline vector3 &vector3::operator /= (Double s)
{
    elt[0] /= s;
    elt[1] /= s;
    elt[2] /= s;

    return(SELF);
}

inline vector3 vector3::operator + (const vector3 &a) const
{
    vector3 result;

    result[0] = elt[0] + a[0];
    result[1] = elt[1] + a[1];
    result[2] = elt[2] + a[2];

    return(result);
}

inline vector3 vector3::operator - (const vector3 &a) const
{
    vector3 result;

    result[0] = elt[0] - a[0];
    result[1] = elt[1] - a[1];
    result[2] = elt[2] - a[2];

    return(result);
}

inline vector3 vector3::operator - () const
{
    vector3 result;

    result[0] = -elt[0];
    result[1] = -elt[1];
    result[2] = -elt[2];

    return(result);
}

inline vector3 vector3::operator * (const vector3 &a) const
{
    vector3 result;

    result[0] = elt[0] * a[0];
    result[1] = elt[1] * a[1];
    result[2] = elt[2] * a[2];

    return(result);
}

inline vector3 vector3::operator * (Double s) const
{
    vector3 result;

    result[0] = elt[0] * s;
    result[1] = elt[1] * s;
    result[2] = elt[2] * s;

    return(result);
}

inline vector3 vector3::operator / (const vector3 &a) const
{
    vector3 result;

    result[0] = elt[0] / a[0];
    result[1] = elt[1] / a[1];
    result[2] = elt[2] / a[2];

    return(result);
}

inline vector3 vector3::operator / (Double s) const
{
    vector3 result;

    result[0] = elt[0] / s;
    result[1] = elt[1] / s;
    result[2] = elt[2] / s;

    return(result);
}

inline vector3 operator * (Double s, const vector3 &v)
{
    return(v * s);
}

inline vector3 &vector3::MakeUnit(Int n, Double k)
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

inline vector3 &vector3::MakeZero()
{
    elt[0] = vl_zero; elt[1] = vl_zero; elt[2] = vl_zero;

    return(SELF);
}

inline vector3 &vector3::MakeBlock(Double k)
{
    elt[0] = k; elt[1] = k; elt[2] = k;

    return(SELF);
}
inline vector3::vector3(ZeroOrOne k)
{
    elt[0] = k; elt[1] = k; elt[2] = k;
}

inline vector3 &vector3::operator = (ZeroOrOne k)
{
    elt[0] = k; elt[1] = k; elt[2] = k;

    return(SELF);
}

inline vector3::vector3(Axis a)
{
    MakeUnit(a, vl_one);
}


inline Bool vector3::operator == (const vector3 &a) const
{
    return(elt[0] == a[0] && elt[1] == a[1] && elt[2] == a[2]);
}

inline Bool vector3::operator != (const vector3 &a) const
{
    return(elt[0] != a[0] || elt[1] != a[1] || elt[2] != a[2]);
}

inline Bool vector3::operator < (const vector3 &a) const
{
//	return(elt[0] < a[0] && elt[1] < a[1] && elt[2] < a[2]);	//	eigth-space ordering - WRONG
    //	half-space ordering - OK	WLC
    return ( (elt[0] < a[0]) || ((elt[0] == a[0]) && (elt[1] < a[1])) || ((elt[0] == a[0]) && (elt[1] == a[1]) && (elt[2] < a[2])) );
}

inline Bool vector3::operator >= (const vector3 &a) const
{
    return(elt[0] >= a[0] && elt[1] >= a[1] && elt[2] >= a[2]);
}


inline Double dot(const vector3 &a, const vector3 &b)
{
    return(a[0] * b[0] + a[1] * b[1] + a[2] * b[2]);
}

inline Double len(const vector3 &v)
{
    return(sqrt(dot(v, v)));
}

inline Double sqrlen(const vector3 &v)
{
    return(dot(v, v));
}

inline vector3 norm(const vector3 &v)
{
    if(sqrlen(v) > 0.0)    return(v / len(v));
    return(vector3(0,0,0));
}

inline Void normalize(vector3 &v)
{
    v /= len(v);
}

inline vector3 cross(const vector3 &a, const vector3 &b)
{
    vector3 result;

    result[0] = a[1] * b[2] - a[2] * b[1];
    result[1] = a[2] * b[0] - a[0] * b[2];
    result[2] = a[0] * b[1] - a[1] * b[0];

    return(result);
}

inline vector2 proj(const vector3 &v)
{
    vector2 result;

	if(v[2] != 0) {
		result[0] = v[0] / v[2];
		result[1] = v[1] / v[2];
	}
	else {
		result[0] = v[0];
		result[1] = v[1];
	}

    return(result);
}

}	//	end namespace BldgGeomLib

std::ostream &operator << (std::ostream &s, const BldgGeomLib::vector3 &v);
std::istream &operator >> (std::istream &s, BldgGeomLib::vector3 &v);

