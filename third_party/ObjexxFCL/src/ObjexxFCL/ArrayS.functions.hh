#ifndef ObjexxFCL_ArrayS_functions_hh_INCLUDED
#define ObjexxFCL_ArrayS_functions_hh_INCLUDED

// ArrayS Functions
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3S.hh>
#include <ObjexxFCL/Array4S.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Array4D.hh>
#include <ObjexxFCL/Array5D.hh>
#include <ObjexxFCL/Fmath.hh>

// C++ Headers
#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <type_traits>

namespace ObjexxFCL {

// allocation /////

template< typename T >
inline
bool
allocated( ArrayS< T > const & a )
{
	return a.allocated();
}

// all /////

inline
bool
all( Array1S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( ! a( i ) ) return false;
	}
	return true;
}

inline
bool
all( Array2S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			if ( ! a( i1, i2 ) ) return false;
		}
	}
	return true;
}

inline
bool
all( Array3S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				if ( ! a( i1, i2, i3 ) ) return false;
			}
		}
	}
	return true;
}

inline
bool
all( Array4S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					if ( ! a( i1, i2, i3, i4 ) ) return false;
				}
			}
		}
	}
	return true;
}

// any /////

inline
bool
any( Array1S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) return true;
	}
	return false;
}

inline
bool
any( Array2S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			if ( a( i1, i2 ) ) return true;
		}
	}
	return false;
}

inline
bool
any( Array3S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				if ( a( i1, i2, i3 ) ) return true;
			}
		}
	}
	return false;
}

inline
bool
any( Array4S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					if ( a( i1, i2, i3, i4 ) ) return true;
				}
			}
		}
	}
	return false;
}

// abs /////

template< typename T >
inline
Array1D< T >
abs( Array1S< T > const & a )
{
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array2D< T >
abs( Array2S< T > const & a )
{
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array3D< T >
abs( Array3S< T > const & a )
{
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array4D< T >
abs( Array4S< T > const & a )
{
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

// Negation /////

inline
Array1D< bool >
operator !( Array1S< bool > const & a )
{
	Array1D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array2D< bool >
operator !( Array2S< bool > const & a )
{
	Array2D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array3D< bool >
operator !( Array3S< bool > const & a )
{
	Array3D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array4D< bool >
operator !( Array4S< bool > const & a )
{
	Array4D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

// Bitwise Not /////

template< typename T >
inline
Array1D< T >
bit_not( Array1S< T > const & a )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< typename T >
inline
Array2D< T >
bit_not( Array2S< T > const & a )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< typename T >
inline
Array3D< T >
bit_not( Array3S< T > const & a )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< typename T >
inline
Array4D< T >
bit_not( Array4S< T > const & a )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

// Bitwise And /////

template< typename T >
inline
Array1D< T >
bit_and( Array1S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< typename T >
inline
Array2D< T >
bit_and( Array2S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< typename T >
inline
Array3D< T >
bit_and( Array3S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< typename T >
inline
Array4D< T >
bit_and( Array4S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< typename T >
inline
Array1D< T >
bit_and( T const & a, Array1S< T > const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< typename T >
inline
Array2D< T >
bit_and( T const & a, Array2S< T > const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< typename T >
inline
Array3D< T >
bit_and( T const & a, Array3S< T > const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< typename T >
inline
Array4D< T >
bit_and( T const & a, Array4S< T > const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< typename T >
inline
Array1D< T >
bit_and( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array1D< T > r( a );
	for ( BArray::size_type l = 0, e = a.size(); l < e; ++l ) {
		r[ l ] &= b[ l ];
	}
	return r;
}

template< typename T >
inline
Array2D< T >
bit_and( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array2D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] &= b( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
Array3D< T >
bit_and( Array3S< T > const & a, Array3S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array3D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				r[ l ] &= b( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
Array4D< T >
bit_and( Array4S< T > const & a, Array4S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array4D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					r[ l ] &= b( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}

// Bitwise Or /////

template< typename T >
inline
Array1D< T >
bit_or( Array1S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< typename T >
inline
Array2D< T >
bit_or( Array2S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< typename T >
inline
Array3D< T >
bit_or( Array3S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< typename T >
inline
Array4D< T >
bit_or( Array4S< T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< typename T >
inline
Array1D< T >
bit_or( T const & a, Array1S< T > const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< typename T >
inline
Array2D< T >
bit_or( T const & a, Array2S< T > const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< typename T >
inline
Array3D< T >
bit_or( T const & a, Array3S< T > const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< typename T >
inline
Array4D< T >
bit_or( T const & a, Array4S< T > const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< typename T >
inline
Array1D< T >
bit_or( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array1D< T > r( a );
	for ( BArray::size_type l = 0, e = a.size(); l < e; ++l ) {
		r[ l ] |= b[ l ];
	}
	return r;
}

template< typename T >
inline
Array2D< T >
bit_or( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array2D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] |= b( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
Array3D< T >
bit_or( Array3S< T > const & a, Array3S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array3D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				r[ l ] |= b( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
Array4D< T >
bit_or( Array4S< T > const & a, Array4S< T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array4D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					r[ l ] |= b( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}


// pow /////

template< typename T, typename X >
inline
Array1D< T >
pow( Array1S< T > const & a, X const & x )
{
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< typename T, typename X >
inline
Array2D< T >
pow( Array2S< T > const & a, X const & x )
{
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< typename T, typename X >
inline
Array3D< T >
pow( Array3S< T > const & a, X const & x )
{
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< typename T, typename X >
inline
Array4D< T >
pow( Array4S< T > const & a, X const & x )
{
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

// sign /////

template< typename T, typename X >
inline
Array1D< T >
sign( Array1S< T > const & a, X const & x )
{
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array2D< T >
sign( Array2S< T > const & a, X const & x )
{
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array3D< T >
sign( Array3S< T > const & a, X const & x )
{
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array4D< T >
sign( Array4S< T > const & a, X const & x )
{
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename X, typename T >
inline
Array1D< X >
sign( X const & x, Array1S< T > const & a )
{
	Array1D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array2D< X >
sign( X const & x, Array2S< T > const & a )
{
	Array2D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array3D< X >
sign( X const & x, Array3S< T > const & a )
{
	Array3D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array4D< X >
sign( X const & x, Array4S< T > const & a )
{
	Array4D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

// count /////

inline
BArray::size_type
count( Array1S< bool > const & a )
{
	BArray::size_type c( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) ++c;
	}
	return c;
}

inline
BArray::size_type
count( Array2S< bool > const & a )
{
	BArray::size_type c( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			if ( a( i1, i2 ) ) ++c;
		}
	}
	return c;
}

inline
BArray::size_type
count( Array3S< bool > const & a )
{
	BArray::size_type c( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				if ( a( i1, i2, i3 ) ) ++c;
			}
		}
	}
	return c;
}

inline
BArray::size_type
count( Array4S< bool > const & a )
{
	BArray::size_type c( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					if ( a( i1, i2, i3, i4 ) ) ++c;
				}
			}
		}
	}
	return c;
}

inline
BArray::size_type
count( Array1S< bool > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return count( a );
	default:
		assert( false );
		return 0;
	}
}

inline
Array1D< BArray::size_type >
count( Array2S< bool > const & a, int const dim )
{
	typedef  BArray::size_type  size_type;
	switch ( dim ) {
	case 1:
		{
		Array1D< size_type > v( a.isize2() );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			size_type c( 0u );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) ) ++c;
			}
			v( i2 ) = c;
		}
		return v;
		}
	case 2:
		{
		Array1D< size_type > v( a.isize1() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			size_type c( 0u );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				if ( a( i1, i2 ) ) ++c;
			}
			v( i1 ) = c;
		}
		return v;
		}
	default:
		assert( false );
		return Array1D< size_type >();
	}
}

// contiguous /////

template< typename T >
inline
bool
contiguous( ArrayS< T > const & a )
{
	return a.contiguous();
}

// lbound /////

template< typename T >
inline
Array1D< int >
lbound( Array1S< T > const & )
{
	return Array1D< int >( 1, 1 );
}

template< typename T >
inline
Array1D< int >
lbound( Array2S< T > const & )
{
	return Array1D< int >( 2, 1 );
}

template< typename T >
inline
Array1D< int >
lbound( Array3S< T > const & )
{
	return Array1D< int >( 3, 1 );
}

template< typename T >
inline
Array1D< int >
lbound( Array4S< T > const & )
{
	return Array1D< int >( 4, 1 );
}

template< typename T >
inline
int
lbound( Array1S< T > const &, int const dim )
{
	switch ( dim ) {
	case 1:
		return 1;
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array2S< T > const &, int const dim )
{
	switch ( dim ) {
	case 1:
		return 1;
	case 2:
		return 1;
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array3S< T > const &, int const dim )
{
	switch ( dim ) {
	case 1:
		return 1;
	case 2:
		return 1;
	case 3:
		return 1;
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array4S< T > const &, int const dim )
{
	switch ( dim ) {
	case 1:
		return 1;
	case 2:
		return 1;
	case 3:
		return 1;
	case 4:
		return 1;
	default:
		assert( false );
		return 0;
	}
}

// ubound /////

template< typename T >
inline
Array1D< int >
ubound( Array1S< T > const & a )
{
	return Array1D< int >( 1, a.u1() );
}

template< typename T >
inline
Array1D< int >
ubound( Array2S< T > const & a )
{
	return Array1D< int >( 2, { a.u1(), a.u2() } );
}

template< typename T >
inline
Array1D< int >
ubound( Array3S< T > const & a )
{
	return Array1D< int >( 3, { a.u1(), a.u2(), a.u3() } );
}

template< typename T >
inline
Array1D< int >
ubound( Array4S< T > const & a )
{
	return Array1D< int >( 4, { a.u1(), a.u2(), a.u3(), a.u4() } );
}

template< typename T >
inline
int
ubound( Array1S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.u1();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( Array2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.u1();
	case 2:
		return a.u2();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( Array3S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.u1();
	case 2:
		return a.u2();
	case 3:
		return a.u3();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( Array4S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.u1();
	case 2:
		return a.u2();
	case 3:
		return a.u3();
	case 4:
		return a.u4();
	default:
		assert( false );
		return 0;
	}
}

// shape /////

template< typename T >
inline
Array1D< int >
shape( Array1S< T > const & a )
{
	return Array1D< int >( 1, a.isize1() );
}

template< typename T >
inline
Array1D< int >
shape( Array2S< T > const & a )
{
	return Array1D< int >( 2, { a.isize1(), a.isize2() } );
}

template< typename T >
inline
Array1D< int >
shape( Array3S< T > const & a )
{
	return Array1D< int >( 3, { a.isize1(), a.isize2(), a.isize3() } );
}

template< typename T >
inline
Array1D< int >
shape( Array4S< T > const & a )
{
	return Array1D< int >( 4, { a.isize1(), a.isize2(), a.isize3(), a.isize4() } );
}

// size /////

template< typename T >
inline
BArray::size_type
size( ArrayS< T > const & a )
{
	return a.size();
}

template< typename T >
inline
BArray::size_type
size( Array1S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
BArray::size_type
size( Array2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	case 2:
		return a.size2();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
BArray::size_type
size( Array3S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	case 2:
		return a.size2();
	case 3:
		return a.size3();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
BArray::size_type
size( Array4S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	case 2:
		return a.size2();
	case 3:
		return a.size3();
	case 4:
		return a.size4();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
isize( ArrayS< T > const & a )
{
	return static_cast< int >( size( a ) );
}

template< template< typename > class A, typename T, class = typename std::enable_if< std::is_base_of< ArrayS< T >, A< T > >::value >::type >
inline
int
isize( A< T > const & a, int const dim )
{
	return static_cast< int >( size( a, dim ) );
}

// contig /////

template< typename T >
inline
Array1D< T >
contig( Array1S< T > const & a )
{
	return Array1D< T >( a );
}

// reshape /////

template< typename T >
inline
Array1D< T >
reshape( Array1S< T > const & a )
{
	return Array1D< T >( a );
}

template< typename T, typename I >
inline
Array1D< T >
reshape( Array1S< T > const & a, std::array< I, 1 > const & shape )
{
	typedef  BArray::size_type  size_type;
	Array1D< T > r( shape[ 0 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
Array2D< T >
reshape( Array1S< T > const & a, std::array< I, 2 > const & shape )
{
	typedef  BArray::size_type  size_type;
	Array2D< T > r( shape[ 0 ], shape[ 1 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
Array3D< T >
reshape( Array1S< T > const & a, std::array< I, 3 > const & shape )
{
	typedef  BArray::size_type  size_type;
	Array3D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
Array4D< T >
reshape( Array1S< T > const & a, std::array< I, 4 > const & shape )
{
	typedef  BArray::size_type  size_type;
	Array4D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
Array5D< T >
reshape( Array1S< T > const & a, std::array< I, 5 > const & shape )
{
	typedef  BArray::size_type  size_type;
	Array5D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
Array1D< T >
reshape( Array2S< T > const & a, std::array< I, 1 > const & shape )
{
	typedef  BArray::size_type  size_type;
	Array1D< T > r( shape[ 0 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); ( ( i1 <= e1 ) && ( l < s ) ); ++i1, ++l ) {
			r[ l ] = a( i1, i2 );
		}
	}
	return r;
}

template< typename T, typename I >
inline
Array2D< T >
reshape( Array2S< T > const & a, std::array< I, 2 > const & shape )
{
	typedef  BArray::size_type  size_type;
	Array2D< T > r( shape[ 0 ], shape[ 1 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); ( ( i1 <= e1 ) && ( l < s ) ); ++i1, ++l ) {
			r[ l ] = a( i1, i2 );
		}
	}
	return r;
}

// Deferred: More reshape overloads

// pack /////

template< typename T >
inline
Array1D< T >
pack( Array1S< T > const & a, Array1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	size_type n( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	size_type l( 0u ), k( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) r[ k++ ] = a( i );
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array2S< T > const & a, Array2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	size_type n( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	size_type l( 0u ), k( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			if ( mask[ l ] ) r[ k++ ] = a( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array3S< T > const & a, Array3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	size_type n( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	size_type l( 0u ), k( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				if ( mask[ l ] ) r[ k++ ] = a( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array4S< T > const & a, Array4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	size_type n( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	size_type l( 0u ), k( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					if ( mask[ l ] ) r[ k++ ] = a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}

// unpack /////

template< typename T >
inline
Array1D< T >
unpack( Array1< T > const & a, Array1< bool > const & mask, Array1S< T > const & f )
{
	assert( mask.size_bounded() );
	assert( mask.conformable( f ) );
	typedef  BArray::size_type  size_type;
	Array1D< T > r( f );
	size_type i( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) {
			r[ l ] = a[ i ];
			++i;
		}
	}
	return r;
}

template< typename T >
inline
Array2D< T >
unpack( Array1< T > const & a, Array2< bool > const & mask, Array2S< T > const & f )
{
	assert( mask.size_bounded() );
	assert( mask.conformable( f ) );
	typedef  BArray::size_type  size_type;
	Array2D< T > r( f );
	size_type i( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) {
			r[ l ] = a[ i ];
			++i;
		}
	}
	return r;
}

template< typename T >
inline
Array3D< T >
unpack( Array1< T > const & a, Array3< bool > const & mask, Array3S< T > const & f )
{
	assert( mask.size_bounded() );
	assert( mask.conformable( f ) );
	typedef  BArray::size_type  size_type;
	Array3D< T > r( f );
	size_type i( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) {
			r[ l ] = a[ i ];
			++i;
		}
	}
	return r;
}

template< typename T >
inline
Array4D< T >
unpack( Array1< T > const & a, Array4< bool > const & mask, Array4S< T > const & f )
{
	assert( mask.size_bounded() );
	assert( mask.conformable( f ) );
	typedef  BArray::size_type  size_type;
	Array4D< T > r( f );
	size_type i( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) {
			r[ l ] = a[ i ];
			++i;
		}
	}
	return r;
}

// cshift /////

template< typename T >
inline
Array1D< T >
cshift( Array1S< T > const & a, int const shift, int const dim = 1 )
{
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	Array1D< T > o( Array1D< T >::shape( a ) );
	int const s( a.u() );
	int const h( shift > 0 ? ( shift % s ) - s : shift % s );
	for ( int i = 1, j = 0; i <= s; ++i, ++j ) {
		o[ ( j - h ) % s ] = a( i );
	}
	return o;
}

template< typename T >
inline
Array2D< T >
cshift( Array2S< T > const & a, int const shift, int const dim = 1 )
{
	typedef  BArray::size_type  size_type;
	Array2D< T > o( Array2D< T >::shape( a ) );
	int const s1( a.u1() );
	int const s2( a.u2() );
	switch ( dim ) {
	case 1:
		{
		int const h( ( shift % s1 ) + ( shift > 0 ? 1 - s1 : 1 ) );
		for ( int i1 = 1; i1 <= s1; ++i1 ) {
			int const j1( 1 + ( ( i1 - h ) % s1 ) );
			size_type m( o.index( j1, 1 ) );
			for ( int i2 = 1; i2 <= s2; ++i2, ++m ) {
				o[ m ] = a( i1, i2 );
			}
		}
		break;
		}
	case 2:
		{
		int const h( ( shift % s2 ) + ( shift > 0 ? 1 - s2 : 1 ) );
		for ( int i1 = 1; i1 <= s1; ++i1 ) {
			for ( int i2 = 1; i2 <= s2; ++i2 ) {
				o( i1, 1 + ( ( i2 - h ) % s2 ) ) = a( i1, i2 );
			}
		}
		break;
		}
	default:
		assert( false );
	}
	return o;
}

template< typename T >
inline
Array2D< T >
cshift( Array2S< T > const & a, Array1< int > const & shift, int const dim = 1 )
{
	Array2D< T > o( Array2D< T >::shape( a ) );
	int const s1( a.u1() );
	int const s2( a.u2() );
	switch ( dim ) {
	case 1:
		assert( shift.size() == a.size2() );
		for ( int i2 = 1, k2 = shift.l(); i2 <= s2; ++i2, ++k2 ) {
			int const h_( shift( k2 ) );
			int const h( ( h_ % s1 ) + ( h_ > 0 ? 1 - s1 : 1 ) );
			for ( int i1 = 1; i1 <= s1; ++i1 ) {
				o( 1 + ( ( i1 - h ) % s1 ), i2 ) = a( i1, i2 );
			}
		}
		break;
	case 2:
		assert( shift.size() == a.size1() );
		for ( int i1 = 1, k1 = shift.l(); i1 <= s1; ++i1, ++k1 ) {
			int const h_( shift( k1 ) );
			int const h( ( h_ % s2 ) + ( h_ > 0 ? 1 - s2 : 1 ) );
			for ( int i2 = 1; i2 <= s2; ++i2 ) {
				o( i1, 1 + ( ( i2 - h ) % s2 ) ) = a( i1, i2 );
			}
		}
		break;
	default:
		assert( false );
	}
	return o;
}

// eoshift /////

template< typename T >
inline
Array1D< T >
eoshift( Array1S< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	Array1D< T > o( Array1D< T >::shape( a, bdy ) );
	int const b( 1 + std::max( shift, 0 ) ), e( a.u() + std::min( shift, 0 ) );
	for ( int i = b, j = std::max( 1 - shift, 1 ); i <= e; ++i, ++j ) {
		o( j ) = a( i );
	}
	return o;
}

template< typename T >
inline
Array2D< T >
eoshift( Array2S< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	typedef  BArray::size_type  size_type;
	Array2D< T > o( Array2D< T >::shape( a, bdy ) );
	switch ( dim ) {
	case 1:
		{
		int const b1( 1 + std::max( shift, 0 ) ), e1( a.u1() + std::min( shift, 0 ) );
		int const e2( a.u2() );
		size_type m( o.index( std::max( 1 - shift, 1 ), 1 ) );
		for ( int i1 = b1; i1 <= e1; ++i1 ) {
			for ( int i2 = 1; i2 <= e2; ++i2, ++m ) {
				o[ m ] = a( i1, i2 );
			}
		}
		break;
		}
	case 2:
		{
		int const e1( a.u1() );
		int const b2( 1 + std::max( shift, 0 ) ), e2( a.u2() + std::min( shift, 0 ) );
		for ( int i1 = 1; i1 <= e1; ++i1 ) {
			size_type m( o.index( i1, std::max( 1 - shift, 1 ) ) );
			for ( int i2 = b2; i2 <= e2; ++i2, ++m ) {
				o[ m ] = a( i1, i2 );
			}
		}
		break;
		}
	default:
		assert( false );
	}
	return o;
}

template< typename T >
inline
Array2D< T >
eoshift( Array2S< T > const & a, Array1< int > const & shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	typedef  BArray::size_type  size_type;
	Array2D< T > o( Array2D< T >::shape( a, bdy ) );
	switch ( dim ) {
	case 1:
		assert( shift.size() == a.size2() );
		for ( int i2 = 1, e2 = a.u2(), k2 = shift.l(); i2 <= e2; ++i2, ++k2 ) {
			int const shift1( shift( k2 ) );
			int const b1( 1 + std::max( shift1, 0 ) ), e1( a.u1() + std::min( shift1, 0 ) );
			for ( int i1 = b1, j1 = std::max( 1 - shift1, 1 ); i1 <= e1; ++i1, ++j1 ) {
				o( j1, i2 ) = a( i1, i2 );
			}
		}
		break;
	case 2:
		assert( shift.size() == a.size1() );
		for ( int i1 = 1, e1 = a.u1(), k1 = shift.l(); i1 <= e1; ++i1, ++k1 ) {
			int const shift2( shift( k1 ) );
			int const b2( 1 + std::max( shift2, 0 ) ), e2( a.u2() + std::min( shift2, 0 ) );
			size_type m( o.index( i1, std::max( 1 - shift2, 1 ) ) );
			for ( int i2 = b2; i2 <= e2; ++i2, ++m ) {
				o[ m ] = a( i1, i2 );
			}
		}
		break;
	default:
		assert( false );
	}
	return o;
}

// sum /////

template< typename T >
inline
T
sum( Array1S< T > const & a )
{
	T r( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r += a( i );
	}
	return r;
}

template< typename T >
inline
T
sum( Array2S< T > const & a )
{
	T r( 0 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			r += a( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
T
sum( Array3S< T > const & a )
{
	T r( 0 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				r += a( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
sum( Array4S< T > const & a )
{
	T r( 0 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					r += a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
sum( Array1S< T > const & a, int const dim )
{
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	T r( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r += a( i );
	}
	return r;
}

template< typename T >
inline
Array1D< T >
sum( Array2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
		Array1D< T > r( a.isize2() );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			T s( 0 );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				s += a( i1, i2 );
			}
			r( i2 ) = s;
		}
		return r;
		}
	case 2:
		{
		Array1D< T > r( a.isize1() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			T s( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				s += a( i1, i2 );
			}
			r( i1 ) = s;
		}
		return r;
		}
	default:
		assert( false );
		return Array1D< T >();
	}
}

template< typename T >
inline
T
sum( Array1S< T > const & a, Array1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 0 );
	size_type l( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) r += a( i );
	}
	return r;
}

template< typename T >
inline
T
sum( Array2S< T > const & a, Array2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 0 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			if ( mask[ l ] ) r += a( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
T
sum( Array3S< T > const & a, Array3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 0 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				if ( mask[ l ] ) r += a( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
sum( Array4S< T > const & a, Array4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 0 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					if ( mask[ l ] ) r += a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
sum_row( Array2S< T > const & a, int const i )
{
	T r( 0 );
	for ( int j = 1, e = a.u2(); j <= e; ++j ) {
		r += a( i, j );
	}
	return r;
}

template< typename T >
inline
T
sum_col( Array2S< T > const & a, int const j )
{
	T r( 0 );
	for ( int i = 1, e = a.u1(); i <= e; ++i ) {
		r += a( i, j );
	}
	return r;
}

// product /////

template< typename T >
inline
T
product( Array1S< T > const & a )
{
	T r( 1 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r *= a( i );
	}
	return r;
}

template< typename T >
inline
T
product( Array2S< T > const & a )
{
	T r( 1 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			r *= a( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
T
product( Array3S< T > const & a )
{
	T r( 1 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				r *= a( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
product( Array4S< T > const & a )
{
	T r( 1 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					r *= a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
product( Array1S< T > const & a, int const dim )
{
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	T r( 1 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r *= a( i );
	}
	return r;
}

template< typename T >
inline
Array1D< T >
product( Array2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
		Array1D< T > r( a.isize2() );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			T p( 1 );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				p *= a( i1, i2 );
			}
			r( i2 ) = p;
		}
		return r;
		}
	case 2:
		{
		Array1D< T > r( a.isize1() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			T p( 1 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				p *= a( i1, i2 );
			}
			r( i1 ) = p;
		}
		return r;
		}
	default:
		assert( false );
		return Array1D< T >();
	}
}

template< typename T >
inline
T
product( Array1S< T > const & a, Array1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 1 );
	size_type l( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) r *= a( i );
	}
	return r;
}

template< typename T >
inline
T
product( Array2S< T > const & a, Array2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 1 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			if ( mask[ l ] ) r *= a( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
T
product( Array3S< T > const & a, Array3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 1 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				if ( mask[ l ] ) r *= a( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
product( Array4S< T > const & a, Array4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 1 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					if ( mask[ l ] ) r *= a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}


template< typename T >
inline
T
product_row( Array2S< T > const & a, int const i )
{
	T r( 1 );
	for ( int j = 1, e = a.u2(); j <= e; ++j ) {
		r *= a( i, j );
	}
	return r;
}

template< typename T >
inline
T
product_col( Array2S< T > const & a, int const j )
{
	T r( 1 );
	for ( int i = 1, e = a.u1(); i <= e; ++i ) {
		r *= a( i, j );
	}
	return r;
}

// minval /////

template< typename T >
inline
T
minval( Array1S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1 ) );
	for ( int i = 2, e = a.u(); i <= e; ++i ) {
		r = std::min( r, a( i ) );
	}
	return r;
}

template< typename T >
inline
T
minval( Array2S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			r = std::min( r, a( i1, i2 ) );
		}
	}
	return r;
}

template< typename T >
inline
T
minval( Array3S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				r = std::min( r, a( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
minval( Array4S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					r = std::min( r, a( i1, i2, i3, i4 ) );
				}
			}
		}
	}
	return r;
}

// maxval /////

template< typename T >
inline
T
maxval( Array1S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1 ) );
	for ( int i = 2, e = a.u(); i <= e; ++i ) {
		r = std::max( r, a( i ) );
	}
	return r;
}

template< typename T >
inline
T
maxval( Array2S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			r = std::max( r, a( i1, i2 ) );
		}
	}
	return r;
}

template< typename T >
inline
T
maxval( Array3S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				r = std::max( r, a( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
maxval( Array4S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					r = std::max( r, a( i1, i2, i3, i4 ) );
				}
			}
		}
	}
	return r;
}

// minloc /////

template< typename T >
inline
Array1D< int >
minloc( Array1S< T > const & a )
{
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1 ) );
	for ( int i = 2, e = a.u(); i <= e; ++i ) {
		if ( a( i ) < r ) {
			r = a( i );
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array2S< T > const & a )
{
	Array1D< int > loc( 2, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			if ( a( i1, i2 ) < r ) {
				r = a( i1, i2 );
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array3S< T > const & a )
{
	Array1D< int > loc( 3, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				if ( a( i1, i2, i3 ) < r ) {
					r = a( i1, i2, i3 );
					loc = { i1, i2, i3 };
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array4S< T > const & a )
{
	Array1D< int > loc( 4, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					if ( a( i1, i2, i3, i4 ) < r ) {
						r = a( i1, i2, i3, i4 );
						loc = { i1, i2, i3, i4 };
					}
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
int
minloc( Array1S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return minloc( a )( 1 );
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
Array1D< int >
minloc( Array2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
		Array1D< int > loc( a.isize2(), a.size2() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, i2 ) );
			for ( int i1 = 2, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < r ) {
					r = a( i1, i2 );
					loc( i2 ) = i1;
				}
			}
		}
		return loc;
		}
	case 2:
		{
		Array1D< int > loc( a.isize1(), a.size1() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			T r( a.empty() ? std::numeric_limits< T >::max() : a( i1, 1 ) );
			for ( int i2 = 2, e2 = a.u2(); i2 <= e2; ++i2 ) {
				if ( a( i1, i2 ) < r ) {
					r = a( i1, i2 );
					loc( i1 ) = i2;
				}
			}
		}
		return loc;
		}
	default:
		assert( false );
		return Array1D< int >();
	}
}

template< typename T >
inline
Array1D< int >
minloc( Array1S< T > const & a, Array1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) {
			if ( first ) {
				first = false;
				r = a( i );
				loc = { i };
			} else if ( a( i ) < r ) {
				r = a( i );
				loc = { i };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array2S< T > const & a, Array2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 2, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			if ( mask[ l ] ) {
				if ( first ) {
					first = false;
					r = a( i1, i2 );
					loc = { i1, i2 };
				} else if ( a( i1, i2 ) < r ) {
					r = a( i1, i2 );
					loc = { i1, i2 };
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array3S< T > const & a, Array3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 3, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				if ( mask[ l ] ) {
					if ( first ) {
						first = false;
						r = a( i1, i2, i3 );
						loc = { i1, i2, i3 };
					} else if ( a( i1, i2, i3 ) < r ) {
						r = a( i1, i2, i3 );
						loc = { i1, i2, i3 };
					}
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array4S< T > const & a, Array4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 4, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					if ( mask[ l ] ) {
						if ( first ) {
							first = false;
							r = a( i1, i2, i3, i4 );
							loc = { i1, i2, i3, i4 };
						} else if ( a( i1, i2, i3, i4 ) < r ) {
							r = a( i1, i2, i3, i4 );
							loc = { i1, i2, i3, i4 };
						}
					}
				}
			}
		}
	}
	return loc;
}

// maxloc /////

template< typename T >
inline
Array1D< int >
maxloc( Array1S< T > const & a )
{
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1 ) );
	for ( int i = 2, e = a.u(); i <= e; ++i ) {
		if ( a( i ) > r ) {
			r = a( i );
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array2S< T > const & a )
{
	Array1D< int > loc( 2, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			if ( a( i1, i2 ) > r ) {
				r = a( i1, i2 );
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array3S< T > const & a )
{
	Array1D< int > loc( 3, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				if ( a( i1, i2, i3 ) > r ) {
					r = a( i1, i2, i3 );
					loc = { i1, i2, i3 };
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array4S< T > const & a )
{
	Array1D< int > loc( 4, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					if ( a( i1, i2, i3, i4 ) > r ) {
						r = a( i1, i2, i3, i4 );
						loc = { i1, i2, i3, i4 };
					}
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
int
maxloc( Array1S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return maxloc( a )( 1 );
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
Array1D< int >
maxloc( Array2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
		Array1D< int > loc( a.isize2(), a.size2() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, i2 ) );
			for ( int i1 = 2, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) > r ) {
					r = a( i1, i2 );
					loc( i2 ) = i1;
				}
			}
		}
		return loc;
		}
	case 2:
		{
		Array1D< int > loc( a.isize1(), a.size1() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			T r( a.empty() ? std::numeric_limits< T >::lowest() : a( i1, 1 ) );
			for ( int i2 = 2, e2 = a.u2(); i2 <= e2; ++i2 ) {
				if ( a( i1, i2 ) > r ) {
					r = a( i1, i2 );
					loc( i1 ) = i2;
				}
			}
		}
		return loc;
		}
	default:
		assert( false );
		return Array1D< int >();
	}
}

template< typename T >
inline
Array1D< int >
maxloc( Array1S< T > const & a, Array1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) {
			if ( first ) {
				first = false;
				r = a( i );
				loc = { i };
			} else if ( a( i ) > r ) {
				r = a( i );
				loc = { i };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array2S< T > const & a, Array2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 2, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			if ( mask[ l ] ) {
				if ( first ) {
					first = false;
					r = a( i1, i2 );
					loc = { i1, i2 };
				} else if ( a( i1, i2 ) > r ) {
					r = a( i1, i2 );
					loc = { i1, i2 };
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array3S< T > const & a, Array3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 3, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				if ( mask[ l ] ) {
					if ( first ) {
						first = false;
						r = a( i1, i2, i3 );
						loc = { i1, i2, i3 };
					} else if ( a( i1, i2, i3 ) > r ) {
						r = a( i1, i2, i3 );
						loc = { i1, i2, i3 };
					}
				}
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array4S< T > const & a, Array4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	Array1D< int > loc( 4, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					if ( mask[ l ] ) {
						if ( first ) {
							first = false;
							r = a( i1, i2, i3, i4 );
							loc = { i1, i2, i3, i4 };
						} else if ( a( i1, i2, i3, i4 ) > r ) {
							r = a( i1, i2, i3, i4 );
							loc = { i1, i2, i3, i4 };
						}
					}
				}
			}
		}
	}
	return loc;
}

// matmul /////

// Matrix (Outer) Product of 1D ArraySs
template< typename T >
inline
Array2D< T >
matmul( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< T > m( a.isize(), b.isize() );
	for ( int i = 1, ei = a.u(); i <= ei; ++i ) {
		T const a_i( a( i ) );
		for ( int j = 1, ej = b.u(); j <= ej; ++j ) {
			m( i, j ) = a_i * b( j );
		}
	}
	return m;
}

// Matrix (Outer) Product of 1D Boolean ArraySs
inline
Array2D< bool >
matmul( Array1S< bool > const & a, Array1S< bool > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > m( a.isize(), b.isize() );
	for ( int i = 1, ei = a.u(); i <= ei; ++i ) {
		bool const a_i( a( i ) );
		for ( int j = 1, ej = b.u(); j <= ej; ++j ) {
			m( i, j ) = a_i && b( j );
		}
	}
	return m;
}

// Matrix Product of 1D and 2D ArraySs
template< typename T >
inline
Array1D< T >
matmul( Array1S< T > const & a, Array2S< T > const & b )
{
	assert( a.size() == b.size1() );
	Array1D< T > m( b.isize2() );
	for ( int j = 1, ej = b.u2(); j <= ej; ++j ) {
		T dot( 0 );
		for ( int i = 1, ei = a.u(); i <= ei; ++i ) {
			dot += a( i ) * b( i, j );
		}
		m( j ) = dot;
	}
	return m;
}

// Matrix Product of 1D and 2D Boolean ArraySs
inline
Array1D< bool >
matmul( Array1S< bool > const & a, Array2S< bool > const & b )
{
	assert( a.size() == b.size1() );
	Array1D< bool > m( b.isize2() );
	for ( int j = 1, ej = b.u2(); j <= ej; ++j ) {
		bool dot( false );
		for ( int i = 1, ei = a.u(); i <= ei; ++i ) {
			if ( a( i ) && b( i, j ) ) {
				dot = true;
				break;
			}
		}
		m( j ) = dot;
	}
	return m;
}

// Matrix Product of 2D and 1D ArraySs
template< typename T >
inline
Array1D< T >
matmul( Array2S< T > const & a, Array1S< T > const & b )
{
	assert( a.size2() == b.size() );
	Array1D< T > m( a.isize1() );
	for ( int i = 1, ei = a.u1(); i <= ei; ++i ) {
		T dot( 0 );
		for ( int j = 1, ej = b.u(); j <= ej; ++j ) {
			dot += a( i, j ) * b( j );
		}
		m( i ) = dot;
	}
	return m;
}

// Matrix Product of 2D and 1D Boolean ArraySs
inline
Array1D< bool >
matmul( Array2S< bool > const & a, Array1S< bool > const & b )
{
	assert( a.size2() == b.size() );
	Array1D< bool > m( a.isize1() );
	for ( int i = 1, ei = a.u1(); i <= ei; ++i ) {
		bool dot( false );
		for ( int j = 1, ej = b.u(); j <= ej; ++j ) {
			if ( a( i, j ) && b( j ) ) {
				dot = true;
				break;
			}
		}
		m( i ) = dot;
	}
	return m;
}

// Matrix Product of 2D ArraySs
template< typename T >
inline
Array2D< T >
matmul( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( a.size2() == b.size1() );
	Array2D< T > m( a.isize1(), b.isize2() );
	for ( int i = 1, ei = a.u1(); i <= ei; ++i ) {
		for ( int j = 1, ej = b.u2(); j <= ej; ++j ) {
			T dot( 0 );
			for ( int k = 1, ek = a.u2(); k <= ek; ++k ) {
				dot += a( i, k ) * b( k, j );
			}
			m( i, j ) = dot;
		}
	}
	return m;
}

// Matrix Product of 2D Boolean ArraySs
inline
Array2D< bool >
matmul( Array2S< bool > const & a, Array2S< bool > const & b )
{
	assert( a.size2() == b.size1() );
	Array2D< bool > m( a.isize1(), b.isize2() );
	for ( int i = 1, ei = a.u1(); i <= ei; ++i ) {
		for ( int j = 1, ej = b.u2(); j <= ej; ++j ) {
			bool dot( false );
			for ( int k = 1, ek = a.u2(); k <= ek; ++k ) {
				if ( a( i, k ) && b( k, j ) ) {
					dot = true;
					break;
				}
			}
			m( i, j ) = dot;
		}
	}
	return m;
}

// merge /////

// Merge 1D Arrays
template< typename T >
inline
Array1D< T >
merge( Array1S< T > const & a, Array1S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array1D< T >( a.isize(), a ); // 1-based copy of a
	} else {
		return Array1D< T >( b.isize(), b ); // 1-based copy of b
	}
}

// Merge 1D Arrays
template< typename T >
inline
Array1D< T >
merge( Array1S< T > const & a, Array1< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array1D< T >( a.isize(), a ); // 1-based copy of a
	} else {
		return Array1D< T >( b.isize(), b ); // 1-based copy of b
	}
}

// Merge 1D Arrays
template< typename T >
inline
Array1D< T >
merge( Array1< T > const & a, Array1S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array1D< T >( a.isize(), a ); // 1-based copy of a
	} else {
		return Array1D< T >( b.isize(), b ); // 1-based copy of b
	}
}

// Merge 1D Array + Scalar
template< typename T >
inline
Array1D< T >
merge( Array1S< T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array1D< T >( a.isize(), a ); // 1-based copy of a
	} else {
		return Array1D< T >( a.isize(), b ); // 1-based copy of b
	}
}

// Merge 1D Array + Scalar
template< typename T >
inline
Array1D< T >
merge( T const & a, Array1S< T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array1D< T >( b.isize(), a ); // 1-based copy of a
	} else {
		return Array1D< T >( b.isize(), b ); // 1-based copy of b
	}
}

// Merge 1D Arrays
template< typename T >
inline
Array1D< T >
merge( Array1S< T > const & a, Array1S< T > const & b, Array1< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array1D< T > m( a.isize() );
	for ( size_type l = 0, e = m.size(); l < e; ++l ) {
		m[ l ] = ( mask[ l ] ? a[ l ] : b[ l ] );
	}
	return m;
}

// Merge 1D Arrays
template< typename T >
inline
Array1D< T >
merge( Array1S< T > const & a, Array1< T > const & b, Array1< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array1D< T > m( a.isize() );
	for ( size_type l = 0, e = m.size(); l < e; ++l ) {
		m[ l ] = ( mask[ l ] ? a[ l ] : b[ l ] );
	}
	return m;
}

// Merge 1D Arrays
template< typename T >
inline
Array1D< T >
merge( Array1< T > const & a, Array1S< T > const & b, Array1< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array1D< T > m( a.isize() );
	for ( size_type l = 0, e = m.size(); l < e; ++l ) {
		m[ l ] = ( mask[ l ] ? a[ l ] : b[ l ] );
	}
	return m;
}

// Merge 1D Array + Scalar
template< typename T >
inline
Array1D< T >
merge( Array1S< T > const & a, T const & b, Array1< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( mask ) );
	Array1D< T > m( a.isize() );
	for ( size_type l = 0, e = m.size(); l < e; ++l ) {
		m[ l ] = ( mask[ l ] ? a[ l ] : b );
	}
	return m;
}

// Merge 1D Array + Scalar
template< typename T >
inline
Array1D< T >
merge( T const & a, Array1S< T > const & b, Array1< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( b.size_bounded() );
	assert( b.conformable( mask ) );
	Array1D< T > m( b.isize() );
	for ( size_type l = 0, e = m.size(); l < e; ++l ) {
		m[ l ] = ( mask[ l ] ? a : b[ l ] );
	}
	return m;
}

// Merge 2D Arrays
template< typename T >
inline
Array2D< T >
merge( Array2S< T > const & a, Array2S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array2D< T >( a.isize1(), a.isize2(), a ); // 1-based copy of a
	} else {
		return Array2D< T >( b.isize1(), b.isize2(), b ); // 1-based copy of b
	}
}

// Merge 2D Arrays
template< typename T >
inline
Array2D< T >
merge( Array2S< T > const & a, Array2< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array2D< T >( a.isize1(), a.isize2(), a ); // 1-based copy of a
	} else {
		return Array2D< T >( b.isize1(), b.isize2(), b ); // 1-based copy of b
	}
}

// Merge 2D Arrays
template< typename T >
inline
Array2D< T >
merge( Array2< T > const & a, Array2S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array2D< T >( a.isize1(), a.isize2(), a ); // 1-based copy of a
	} else {
		return Array2D< T >( b.isize1(), b.isize2(), b ); // 1-based copy of b
	}
}

// Merge 2D Array + Scalar
template< typename T >
inline
Array2D< T >
merge( Array2S< T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array2D< T >( a.isize1(), a.isize2(), a ); // 1-based copy of a
	} else {
		return Array2D< T >( a.isize1(), a.isize2(), b ); // 1-based copy of b
	}
}

// Merge 2D Array + Scalar
template< typename T >
inline
Array2D< T >
merge( T const & a, Array2S< T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array2D< T >( b.isize1(), b.isize2(), a ); // 1-based copy of a
	} else {
		return Array2D< T >( b.isize1(), b.isize2(), b ); // 1-based copy of b
	}
}

// Merge 2D Arrays
template< typename T >
inline
Array2D< T >
merge( Array2S< T > const & a, Array2S< T > const & b, Array2< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array2D< T > m( a.isize1(), a.isize2() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ib1 = b.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1, ++ib1 ) {
		for ( int ia2 = a.l2(), ib2 = b.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2, ++ib2, ++l ) {
			m[ l ] = ( mask[ l ] ? a( ia1, ia2 ) : b( ib1, ib2 ) );
		}
	}
	return m;
}

// Merge 2D Arrays
template< typename T >
inline
Array2D< T >
merge( Array2S< T > const & a, Array2< T > const & b, Array2< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array2D< T > m( a.isize1(), a.isize2() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2, ++l ) {
			m[ l ] = ( mask[ l ] ? a( ia1, ia2 ) : b[ l ] );
		}
	}
	return m;
}

// Merge 2D Arrays
template< typename T >
inline
Array2D< T >
merge( Array2< T > const & a, Array2S< T > const & b, Array2< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array2D< T > m( a.isize1(), a.isize2() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eib = b.u1(); ib1 <= eib; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2, ++l ) {
			m[ l ] = ( mask[ l ] ? a[ l ] : b( ib1, ib2 ) );
		}
	}
	return m;
}

// Merge 2D Array + Scalar
template< typename T >
inline
Array2D< T >
merge( Array2S< T > const & a, T const & b, Array2< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( mask ) );
	Array2D< T > m( a.isize1(), a.isize2() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2, ++l ) {
			m[ l ] = ( mask[ l ] ? a( ia1, ia2 ) : b );
		}
	}
	return m;
}

// Merge 2D Array + Scalar
template< typename T >
inline
Array2D< T >
merge( T const & a, Array2S< T > const & b, Array2< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( b.size_bounded() );
	assert( b.conformable( mask ) );
	Array2D< T > m( b.isize1(), b.isize2() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eib = b.u1(); ib1 <= eib; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2, ++l ) {
			m[ l ] = ( mask[ l ] ? a : b( ib1, ib2 ) );
		}
	}
	return m;
}

// Merge 3D Arrays
template< typename T >
inline
Array3D< T >
merge( Array3S< T > const & a, Array3S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array3D< T >( a.isize1(), a.isize2(), a.isize3(), a ); // 1-based copy of a
	} else {
		return Array3D< T >( b.isize1(), b.isize2(), b.isize3(), b ); // 1-based copy of b
	}
}

// Merge 3D Arrays
template< typename T >
inline
Array3D< T >
merge( Array3S< T > const & a, Array3< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array3D< T >( a.isize1(), a.isize2(), a.isize3(), a ); // 1-based copy of a
	} else {
		return Array3D< T >( b.isize1(), b.isize2(), b.isize3(), b ); // 1-based copy of b
	}
}

// Merge 3D Arrays
template< typename T >
inline
Array3D< T >
merge( Array3< T > const & a, Array3S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array3D< T >( a.isize1(), a.isize2(), a.isize3(), a ); // 1-based copy of a
	} else {
		return Array3D< T >( b.isize1(), b.isize2(), b.isize3(), b ); // 1-based copy of b
	}
}

// Merge 3D Array + Scalar
template< typename T >
inline
Array3D< T >
merge( Array3S< T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array3D< T >( a.isize1(), a.isize2(), a.isize3(), a ); // 1-based copy of a
	} else {
		return Array3D< T >( a.isize1(), a.isize2(), a.isize3(), b ); // 1-based copy of b
	}
}

// Merge 3D Array + Scalar
template< typename T >
inline
Array3D< T >
merge( T const & a, Array3S< T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array3D< T >( b.isize1(), b.isize2(), b.isize3(), a ); // 1-based copy of a
	} else {
		return Array3D< T >( b.isize1(), b.isize2(), b.isize3(), b ); // 1-based copy of b
	}
}

// Merge 3D Arrays
template< typename T >
inline
Array3D< T >
merge( Array3S< T > const & a, Array3S< T > const & b, Array3< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array3D< T > m( a.isize1(), a.isize2(), a.isize3() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ib1 = b.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1, ++ib1 ) {
		for ( int ia2 = a.l2(), ib2 = b.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2, ++ib2 ) {
			for ( int ia3 = a.l3(), ib3 = b.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3, ++ib3, ++l ) {
				m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3 ) : b( ib1, ib2, ib3 ) );
			}
		}
	}
	return m;
}

// Merge 3D Arrays
template< typename T >
inline
Array3D< T >
merge( Array3S< T > const & a, Array3< T > const & b, Array3< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array3D< T > m( a.isize1(), a.isize2(), a.isize3() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3, ++l ) {
				m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3 ) : b[ l ] );
			}
		}
	}
	return m;
}

// Merge 3D Arrays
template< typename T >
inline
Array3D< T >
merge( Array3< T > const & a, Array3S< T > const & b, Array3< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array3D< T > m( a.isize1(), a.isize2(), a.isize3() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3, ++l ) {
				m[ l ] = ( mask[ l ] ? a[ l ] : b( ib1, ib2, ib3 ) );
			}
		}
	}
	return m;
}

// Merge 3D Array + Scalar
template< typename T >
inline
Array3D< T >
merge( Array3S< T > const & a, T const & b, Array3< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( mask ) );
	Array3D< T > m( a.isize1(), a.isize2(), a.isize3() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3, ++l ) {
				m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3 ) : b );
			}
		}
	}
	return m;
}

// Merge 3D Array + Scalar
template< typename T >
inline
Array3D< T >
merge( T const & a, Array3S< T > const & b, Array3< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( b.size_bounded() );
	assert( b.conformable( mask ) );
	Array3D< T > m( b.isize1(), b.isize2(), b.isize3() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3, ++l ) {
				m[ l ] = ( mask[ l ] ? a : b( ib1, ib2, ib3 ) );
			}
		}
	}
	return m;
}

// Merge 4D Arrays
template< typename T >
inline
Array4D< T >
merge( Array4S< T > const & a, Array4S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array4D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a ); // 1-based copy of a
	} else {
		return Array4D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b ); // 1-based copy of b
	}
}

// Merge 4D Arrays
template< typename T >
inline
Array4D< T >
merge( Array4S< T > const & a, Array4< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array4D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a ); // 1-based copy of a
	} else {
		return Array4D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b ); // 1-based copy of b
	}
}

// Merge 4D Arrays
template< typename T >
inline
Array4D< T >
merge( Array4< T > const & a, Array4S< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array4D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a ); // 1-based copy of a
	} else {
		return Array4D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b ); // 1-based copy of b
	}
}

// Merge 4D Array + Scalar
template< typename T >
inline
Array4D< T >
merge( Array4S< T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array4D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a ); // 1-based copy of a
	} else {
		return Array4D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), b ); // 1-based copy of b
	}
}

// Merge 4D Array + Scalar
template< typename T >
inline
Array4D< T >
merge( T const & a, Array4S< T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array4D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), a ); // 1-based copy of a
	} else {
		return Array4D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b ); // 1-based copy of b
	}
}

// Merge 4D Arrays
template< typename T >
inline
Array4D< T >
merge( Array4S< T > const & a, Array4S< T > const & b, Array4< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array4D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ib1 = b.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1, ++ib1 ) {
		for ( int ia2 = a.l2(), ib2 = b.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2, ++ib2 ) {
			for ( int ia3 = a.l3(), ib3 = b.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3, ++ib3 ) {
				for ( int ia4 = a.l4(), ib4 = b.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4, ++ib4, ++l ) {
					m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4 ) : b( ib1, ib2, ib3, ib4 ) );
				}
			}
		}
	}
	return m;
}

// Merge 4D Arrays
template< typename T >
inline
Array4D< T >
merge( Array4S< T > const & a, Array4< T > const & b, Array4< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array4D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3 ) {
				for ( int ia4 = a.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4, ++l ) {
					m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4 ) : b[ l ] );
				}
			}
		}
	}
	return m;
}

// Merge 4D Arrays
template< typename T >
inline
Array4D< T >
merge( Array4< T > const & a, Array4S< T > const & b, Array4< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array4D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3 ) {
				for ( int ib4 = b.l4(), eb4 = b.u4(); ib4 <= eb4; ++ib4, ++l ) {
					m[ l ] = ( mask[ l ] ? a[ l ] : b( ib1, ib2, ib3, ib4 ) );
				}
			}
		}
	}
	return m;
}

// Merge 4D Array + Scalar
template< typename T >
inline
Array4D< T >
merge( Array4S< T > const & a, T const & b, Array4< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( mask ) );
	Array4D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3 ) {
				for ( int ia4 = a.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4, ++l ) {
					m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4 ) : b );
				}
			}
		}
	}
	return m;
}

// Merge 4D Array + Scalar
template< typename T >
inline
Array4D< T >
merge( T const & a, Array4S< T > const & b, Array4< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( b.size_bounded() );
	assert( b.conformable( mask ) );
	Array4D< T > m( b.isize1(), b.isize2(), b.isize3(), b.isize4() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3 ) {
				for ( int ib4 = b.l4(), eb4 = b.u4(); ib4 <= eb4; ++ib4, ++l ) {
					m[ l ] = ( mask[ l ] ? a : b( ib1, ib2, ib3, ib4 ) );
				}
			}
		}
	}
	return m;
}



} // ObjexxFCL

#endif // ObjexxFCL_ArrayS_functions_hh_INCLUDED
