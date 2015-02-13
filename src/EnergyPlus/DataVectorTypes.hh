#ifndef DataVectorTypes_hh_INCLUDED
#define DataVectorTypes_hh_INCLUDED

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Vector3.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataVectorTypes {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS

	// the following two derived types are used in triangulation (for DXF outputs)
	//'Points (Vertices)

	//'Created Triangles, vv# are the vertex pointers

	// Types

	struct Vector // This is used to specify a point in 3D space
	{
		// Members
		// Right Handed Coordinate system is used
		Real64 x;
		Real64 y;
		Real64 z;

		// Default Constructor
		Vector()
		{}

		// Member Constructor
		Vector(
			Real64 const x,
			Real64 const y,
			Real64 const z
		) :
			x( x ),
			y( y ),
			z( z )
		{}

		// Uniform Real64 Constructor
		Vector( Real64 const v ) :
			x( v ),
			y( v ),
			z( v )
		{}

		// Array Assignment
		inline
		Vector &
		operator =( Array1D< Real64 > const a )
		{
			assert( ( a.l() == 1 ) && ( a.u() == 3 ) );
			x = a( 1 );
			y = a( 2 );
			z = a( 3 );
			return *this;
		}

		// Array Assignment
		inline
		Vector &
		operator =( Array1A< Real64 > const a )
		{
			a.dim( 3 );
			x = a( 1 );
			y = a( 2 );
			z = a( 3 );
			return *this;
		}

		// Array Assignment
		inline
		Vector &
		operator =( Array1S< Real64 > const & a )
		{
			assert( ( a.l() == 1 ) && ( a.u() == 3 ) );
			x = a( 1 );
			y = a( 2 );
			z = a( 3 );
			return *this;
		}

		// Vector3 Assignment
		inline
		Vector &
		operator =( Vector3< Real64 > const & v )
		{
			x = v.x;
			y = v.y;
			z = v.z;
			return *this;
		}

		// Real64 Assignment
		inline
		Vector &
		operator =( Real64 const v )
		{
			x = v;
			y = v;
			z = v;
			return *this;
		}

		// += Vector
		inline
		Vector &
		operator +=( Vector const & a )
		{
			x += a.x;
			y += a.y;
			z += a.z;
			return *this;
		}

		// -= Vector
		inline
		Vector &
		operator -=( Vector const & a )
		{
			x -= a.x;
			y -= a.y;
			z -= a.z;
			return *this;
		}

		// += Real64
		inline
		Vector &
		operator +=( Real64 const v )
		{
			x += v;
			y += v;
			z += v;
			return *this;
		}

		// -= Real64
		inline
		Vector &
		operator -=( Real64 const v )
		{
			x -= v;
			y -= v;
			z -= v;
			return *this;
		}

		// *= Real64
		inline
		Vector &
		operator *=( Real64 const v )
		{
			x *= v;
			y *= v;
			z *= v;
			return *this;
		}

		// /= Real64
		inline
		Vector &
		operator /=( Real64 const v )
		{
			assert( v != Real64( 0.0 ) );
			x /= v;
			y /= v;
			z /= v;
			return *this;
		}

		// Array Conversion
		inline
		operator Array1D< Real64 >() const
		{
			return Array1D< Real64 >( 3, { x, y, z } );
		}

		// Length
		inline
		Real64
		length() const
		{
			return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
		}

		// Length
		inline
		Real64
		length_squared() const
		{
			return ( x * x ) + ( y * y ) + ( z * z );
		}

		// Negated
		inline
		friend
		Vector
		operator -( Vector const & a )
		{
			return Vector( -a.x, -a.y, -a.z );
		}

		// Vector + Vector
		inline
		friend
		Vector
		operator +( Vector const & a, Vector const & b )
		{
			Vector r;
			r.x = a.x + b.x;
			r.y = a.y + b.y;
			r.z = a.z + b.z;
			return r;
		}

		// Vector - Vector
		inline
		friend
		Vector
		operator -( Vector const & a, Vector const & b )
		{
			Vector r;
			r.x = a.x - b.x;
			r.y = a.y - b.y;
			r.z = a.z - b.z;
			return r;
		}

		// Vector * Vector: Cross Product //Autodesk Suggest migrating to cross function to avoid confusion
		inline
		friend
		Vector
		operator *( Vector const & a, Vector const & b )
		{
			Vector c;
			c.x = ( a.y * b.z ) - ( a.z * b.y );
			c.y = ( a.z * b.x ) - ( a.x * b.z );
			c.z = ( a.x * b.y ) - ( a.y * b.x );
			return c;
		}

		// Vector * Real64
		inline
		friend
		Vector
		operator *( Vector const & a, Real64 const b )
		{
			Vector r;
			r.x = a.x * b;
			r.y = a.y * b;
			r.z = a.z * b;
			return r;
		}

		// Real64 * Vector
		inline
		friend
		Vector
		operator *( Real64 const b, Vector const & a )
		{
			Vector r;
			r.x = a.x * b;
			r.y = a.y * b;
			r.z = a.z * b;
			return r;
		}

		// Vector * Integer
		inline
		friend
		Vector
		operator *( Vector const & a, int const b )
		{
			Vector r;
			r.x = a.x * b;
			r.y = a.y * b;
			r.z = a.z * b;
			return r;
		}

		// Integer * Vector
		inline
		friend
		Vector
		operator *( int const b, Vector const & a )
		{
			Vector r;
			r.x = a.x * b;
			r.y = a.y * b;
			r.z = a.z * b;
			return r;
		}

		// Vector / Real64
		inline
		friend
		Vector
		operator /( Vector const & a, Real64 const b )
		{
			assert( b != 0.0 );
			Vector r;
			r.x = a.x / b;
			r.y = a.y / b;
			r.z = a.z / b;
			return r;
		}

		// Vector / Integer
		inline
		friend
		Vector
		operator /( Vector const & a, int const b )
		{
			assert( b != 0 );
			Vector r;
			r.x = a.x / b;
			r.y = a.y / b;
			r.z = a.z / b;
			return r;
		}

		// Magnitude
		inline
		friend
		Real64
		magnitude( Vector const & a )
		{
			return std::sqrt( square( a.x ) + square( a.y ) + square( a.z ) );
		}

		// Magnitude Squared
		inline
		friend
		Real64
		magnitude_squared( Vector const & a )
		{
			return square( a.x ) + square( a.y ) + square( a.z );
		}

		// Distance
		inline
		friend
		Real64
		distance( Vector const & a, Vector const & b )
		{
			return std::sqrt( square( a.x - b.x ) + square( a.y - b.y ) + square( a.z - b.z ) );
		}

		// Distance Squared
		inline
		friend
		Real64
		distance_squared( Vector const & a, Vector const & b )
		{
			return square( a.x - b.x ) + square( a.y - b.y ) + square( a.z - b.z );
		}

		// Dot Product
		inline
		friend
		Real64
		dot( Vector const & a, Vector const & b )
		{
			return ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z );
		}

		// Dot Product
		inline
		friend
		Real64
		dot( Vector const & a, Vector3< Real64 > const & b )
		{
			return ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z );
		}

		// Dot Product
		inline
		friend
		Real64
		dot( Vector3< Real64 > const & a, Vector const & b )
		{
			return ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z );
		}

		// Cross Product
		inline
		friend
		Vector
		cross( Vector const & a, Vector const & b )
		{
			Vector c;
			c.x = ( a.y * b.z ) - ( a.z * b.y );
			c.y = ( a.z * b.x ) - ( a.x * b.z );
			c.z = ( a.x * b.y ) - ( a.y * b.x );
			return c;
		}

		// Cross Product
		inline
		friend
		Vector
		cross( Vector const & a, Vector3< Real64 > const & b )
		{
			Vector c;
			c.x = ( a.y * b.z ) - ( a.z * b.y );
			c.y = ( a.z * b.x ) - ( a.x * b.z );
			c.z = ( a.x * b.y ) - ( a.y * b.x );
			return c;
		}

		// Cross Product
		inline
		friend
		Vector
		cross( Vector3< Real64 > const & a, Vector const & b )
		{
			Vector c;
			c.x = ( a.y * b.z ) - ( a.z * b.y );
			c.y = ( a.z * b.x ) - ( a.x * b.z );
			c.z = ( a.x * b.y ) - ( a.y * b.x );
			return c;
		}

		// Array Generator
		inline
		Array1D< Real64 >
		Array() const
		{
			return Array1D< Real64 >( 3, { x, y, z } );
		}

		// Array Generator
		inline
		Array1D< Real64 >
		as_Array() const
		{
			return Array1D< Real64 >( 3, { x, y, z } );
		}

		// Vector3 Generator
		inline
		Vector3< Real64 >
		as_Vector3() const
		{
			return Vector3< Real64 >( x, y, z );
		}

		// Assign to an Array
		inline
		void
		assign_to( Array1D< Real64 > & a ) const
		{
			a.dimension( 3 );
			a( 1 ) = x;
			a( 2 ) = y;
			a( 3 ) = z;
		}

		// Assign to a Vector3
		inline
		void
		assign_to( Vector3< Real64 > & v ) const
		{
			v.x = x;
			v.y = y;
			v.z = z;
		}

	private: // Static Functions

		// Square
		inline
		static
		Real64
		square( Real64 const x )
		{
			return x * x;
		}

	};

	struct PlaneEq // This is used to specify a plane based on vectors in that plane
	{
		// Members
		Real64 x;
		Real64 y;
		Real64 z;
		Real64 w;

		// Default Constructor
		PlaneEq()
		{}

		// Member Constructor
		PlaneEq(
			Real64 const x,
			Real64 const y,
			Real64 const z,
			Real64 const w
		) :
			x( x ),
			y( y ),
			z( z ),
			w( w )
		{}

	};

	struct Face // Used to specify the face of a polyhedron
	{
		// Members
		int NSides; // Number of Sides for this Face
		int SurfNum; // ALLOCATABLE to actual surface number
		Array1D< Vector > FacePoints;
		Vector NewellAreaVector;

		// Default Constructor
		Face() :
			NSides( 0 )
		{}

		// Member Constructor
		Face(
			int const NSides, // Number of Sides for this Face
			int const SurfNum, // ALLOCATABLE to actual surface number
			Array1< Vector > const & FacePoints,
			Vector const & NewellAreaVector
		) :
			NSides( NSides ),
			SurfNum( SurfNum ),
			FacePoints( FacePoints ),
			NewellAreaVector( NewellAreaVector )
		{}

	};

	struct Polyhedron // This is used to specify a polyhedron based on the vectors that comprise it (a zone).
	{
		// Members
		int NumSurfaceFaces;
		Array1D< Face > SurfaceFace;

		// Default Constructor
		Polyhedron() :
			NumSurfaceFaces( 0 )
		{}

		// Member Constructor
		Polyhedron(
			int const NumSurfaceFaces,
			Array1< Face > const & SurfaceFace
		) :
			NumSurfaceFaces( NumSurfaceFaces ),
			SurfaceFace( SurfaceFace )
		{}

	};

	struct Vector_2d
	{
		// Members
		Real64 x;
		Real64 y;

		// Default Constructor
		Vector_2d()
		{}

		// Member Constructor
		Vector_2d(
			Real64 const x,
			Real64 const y
		) :
			x( x ),
			y( y )
		{}

		// Dot Product
		inline
		friend
		Real64
		dot( Vector_2d const & a, Vector_2d const & b )
		{
			return ( a.x * b.x ) + ( a.y * b.y );
		}

		// Cross Product
		inline
		friend
		Real64
		cross( Vector_2d const & a, Vector_2d const & b )
		{
			return ( a.x * b.y ) - ( a.y * b.x );
		}

	};

	struct dTriangle
	{
		// Members
		int vv0;
		int vv1;
		int vv2;

		// Default Constructor
		dTriangle()
		{}

		// Member Constructor
		dTriangle(
			int const vv0,
			int const vv1,
			int const vv2
		) :
			vv0( vv0 ),
			vv1( vv1 ),
			vv2( vv2 )
		{}

	};

} // DataVectorTypes

} // EnergyPlus

#endif
