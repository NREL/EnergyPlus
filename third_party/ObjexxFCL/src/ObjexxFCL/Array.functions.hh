#ifndef ObjexxFCL_Array_functions_hh_INCLUDED
#define ObjexxFCL_Array_functions_hh_INCLUDED

// Array Functions
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Array4D.hh>
#include <ObjexxFCL/Array5D.hh>
#include <ObjexxFCL/Array6D.hh>
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
allocated( Array< T > const & a )
{
	return a.allocated();
}

template< typename T >
inline
void
allocate( Array1D< T > & a, IndexRange const & I )
{
	a.allocate( I );
}

template< typename T >
inline
void
allocate( Array2D< T > & a, IndexRange const & I1, IndexRange const & I2 )
{
	a.allocate( I1, I2 );
}

template< typename T >
inline
void
allocate( Array3D< T > & a, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3 )
{
	a.allocate( I1, I2, I3 );
}

template< typename T >
inline
void
allocate( Array4D< T > & a, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3, IndexRange const & I4 )
{
	a.allocate( I1, I2, I3, I4 );
}

template< typename T >
inline
void
allocate( Array5D< T > & a, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3, IndexRange const & I4, IndexRange const & I5 )
{
	a.allocate( I1, I2, I3, I4, I5 );
}

template< typename T >
inline
void
allocate( Array6D< T > & a, IndexRange const & I1, IndexRange const & I2, IndexRange const & I3, IndexRange const & I4, IndexRange const & I5, IndexRange const & I6 )
{
	a.allocate( I1, I2, I3, I4, I5, I6 );
}

template< typename T >
inline
void
deallocate( Array< T > & a )
{
	a.clear();
}

// all /////

inline
bool
all( Array< bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return true;
	for ( Array< bool >::size_type i = 0; i < a.size(); ++i ) {
		if ( ! a[ i ] ) return false;
	}
	return true;
}

// any /////

inline
bool
any( Array< bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return false;
	for ( Array< bool >::size_type i = 0; i < a.size(); ++i ) {
		if ( a[ i ] ) return true;
	}
	return false;
}

// negation /////

inline
Array1D< bool >
operator !( Array1< bool > const & a )
{
	Array1D< bool > r( a );
	for ( Array< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array2D< bool >
operator !( Array2< bool > const & a )
{
	Array2D< bool > r( a );
	for ( Array< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array3D< bool >
operator !( Array3< bool > const & a )
{
	Array3D< bool > r( a );
	for ( Array< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array4D< bool >
operator !( Array4< bool > const & a )
{
	Array4D< bool > r( a );
	for ( Array< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array5D< bool >
operator !( Array5< bool > const & a )
{
	Array5D< bool > r( a );
	for ( Array< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array6D< bool >
operator !( Array6< bool > const & a )
{
	Array6D< bool > r( a );
	for ( Array< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

// count /////

inline
Array< bool >::size_type
count( Array< bool > const & a )
{
	assert( a.size_bounded() );
	Array< bool >::size_type c( 0u );
	for ( Array< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		if ( a[ i ] ) ++c;
	}
	return c;
}

inline
Array< bool >::size_type
count( Array1< bool > const & a, int const dim )
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
Array1D< Array< bool >::size_type >
count( Array2< bool > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	switch ( dim ) {
	case 1:
		{
		Array1D< size_type > r( static_cast< int >( as2 ) );
		for ( size_type i2 = 0; i2 < as2; ++i2 ) {
			size_type c( 0u );
			for ( size_type i1 = 0, l = i2; i1 < as1; ++i1, l += as2 ) {
				if ( a[ l ] ) ++c;
			}
			r[ i2 ] = c;
		}
		return r;
		}
	case 2:
		{
		Array1D< size_type > r( static_cast< int >( as1 ) );
		for ( size_type i1 = 0, l = 0; i1 < as1; ++i1 ) {
			size_type c( 0u );
			for ( size_type i2 = 0; i2 < as2; ++i2, ++l ) {
				if ( a[ l ] ) ++c;
			}
			r[ i1 ] = c;
		}
		return r;
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
contiguous( Array< T > const & a )
{
	return a.contiguous();
}

// lbound /////

template< typename T >
inline
Array1D< int >
lbound( Array1< T > const & a )
{
	return Array1D< int >( 1, a.l1() );
}

template< typename T >
inline
Array1D< int >
lbound( Array2< T > const & a )
{
	return Array1D< int >( 2, { a.l1(), a.l2() } );
}

template< typename T >
inline
Array1D< int >
lbound( Array3< T > const & a )
{
	return Array1D< int >( 3, { a.l1(), a.l2(), a.l3() } );
}

template< typename T >
inline
Array1D< int >
lbound( Array4< T > const & a )
{
	return Array1D< int >( 4, { a.l1(), a.l2(), a.l3(), a.l4() } );
}

template< typename T >
inline
Array1D< int >
lbound( Array5< T > const & a )
{
	return Array1D< int >( 5, { a.l1(), a.l2(), a.l3(), a.l4(), a.l5() } );
}

template< typename T >
inline
Array1D< int >
lbound( Array6< T > const & a )
{
	return Array1D< int >( 6, { a.l1(), a.l2(), a.l3(), a.l4(), a.l5(), a.l6() } );
}

template< typename T >
inline
int
lbound( Array1< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.l1();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array2< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.l1();
	case 2:
		return a.l2();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array3< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.l1();
	case 2:
		return a.l2();
	case 3:
		return a.l3();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array4< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.l1();
	case 2:
		return a.l2();
	case 3:
		return a.l3();
	case 4:
		return a.l4();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array5< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.l1();
	case 2:
		return a.l2();
	case 3:
		return a.l3();
	case 4:
		return a.l4();
	case 5:
		return a.l5();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array6< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.l1();
	case 2:
		return a.l2();
	case 3:
		return a.l3();
	case 4:
		return a.l4();
	case 5:
		return a.l5();
	case 6:
		return a.l6();
	default:
		assert( false );
		return 0;
	}
}

// ubound /////

template< typename T >
inline
Array1D< int >
ubound( Array1< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 1, a.u1() );
}

template< typename T >
inline
Array1D< int >
ubound( Array2< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 2, { a.u1(), a.u2() } );
}

template< typename T >
inline
Array1D< int >
ubound( Array3< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 3, { a.u1(), a.u2(), a.u3() } );
}

template< typename T >
inline
Array1D< int >
ubound( Array4< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 4, { a.u1(), a.u2(), a.u3(), a.u4() } );
}

template< typename T >
inline
Array1D< int >
ubound( Array5< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 5, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5() } );
}

template< typename T >
inline
Array1D< int >
ubound( Array6< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 6, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5(), a.u6() } );
}

template< typename T >
inline
int
ubound( Array1< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		assert( a.I1().bounded() );
		return a.u1();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( Array2< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		assert( a.I1().bounded() );
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
ubound( Array3< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		assert( a.I1().bounded() );
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
ubound( Array4< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		assert( a.I1().bounded() );
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

template< typename T >
inline
int
ubound( Array5< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		assert( a.I1().bounded() );
		return a.u1();
	case 2:
		return a.u2();
	case 3:
		return a.u3();
	case 4:
		return a.u4();
	case 5:
		return a.u5();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( Array6< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		assert( a.I1().bounded() );
		return a.u1();
	case 2:
		return a.u2();
	case 3:
		return a.u3();
	case 4:
		return a.u4();
	case 5:
		return a.u5();
	case 6:
		return a.u6();
	default:
		assert( false );
		return 0;
	}
}

// shape /////

template< typename T >
inline
Array1D< int >
shape( Array1< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 1, a.isize1() );
}

template< typename T >
inline
Array1D< int >
shape( Array2< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 2, { a.isize1(), a.isize2() } );
}

template< typename T >
inline
Array1D< int >
shape( Array3< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 3, { a.isize1(), a.isize2(), a.isize3() } );
}

template< typename T >
inline
Array1D< int >
shape( Array4< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 4, { a.isize1(), a.isize2(), a.isize3(), a.isize4() } );
}

template< typename T >
inline
Array1D< int >
shape( Array5< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 5, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() } );
}

template< typename T >
inline
Array1D< int >
shape( Array6< T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 6, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() } );
}

// size /////

template< typename T >
inline
typename Array< T >::size_type
size( Array< T > const & a )
{
	return a.size();
}

template< typename T >
inline
typename Array< T >::size_type
size( Array1< T > const & a, int const dim )
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
typename Array< T >::size_type
size( Array2< T > const & a, int const dim )
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
typename Array< T >::size_type
size( Array3< T > const & a, int const dim )
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
typename Array< T >::size_type
size( Array4< T > const & a, int const dim )
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
typename Array< T >::size_type
size( Array5< T > const & a, int const dim )
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
	case 5:
		return a.size5();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
typename Array< T >::size_type
size( Array6< T > const & a, int const dim )
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
	case 5:
		return a.size5();
	case 6:
		return a.size6();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
isize( Array< T > const & a )
{
	return static_cast< int >( size( a ) );
}

template< template< typename > class A, typename T >
inline
typename std::enable_if< std::is_base_of< Array< T >, A< T > >::value, int >::type
isize( A< T > const & a, int const dim )
{
	return static_cast< int >( size( a, dim ) );
}

// reshape /////

template< typename T >
inline
Array1D< T >
reshape( Array< T > const & a )
{
	return Array1D< T >( a );
}

template< typename T, typename I >
inline
Array1D< T >
reshape( Array< T > const & a, std::array< I, 1 > const & shape )
{
	return Array1D< T >( shape[ 0 ], a );
}

template< typename T, typename I >
inline
Array2D< T >
reshape( Array< T > const & a, std::array< I, 2 > const & shape )
{
	return Array2D< T >( shape[ 0 ], shape[ 1 ], a );
}

template< typename T, typename I >
inline
Array3D< T >
reshape( Array< T > const & a, std::array< I, 3 > const & shape )
{
	return Array3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], a );
}

template< typename T, typename I >
inline
Array4D< T >
reshape( Array< T > const & a, std::array< I, 4 > const & shape )
{
	return Array4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], a );
}

template< typename T, typename I >
inline
Array5D< T >
reshape( Array< T > const & a, std::array< I, 5 > const & shape )
{
	return Array5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], a );
}

template< typename T, typename I >
inline
Array6D< T >
reshape( Array< T > const & a, std::array< I, 6 > const & shape )
{
	return Array6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], a );
}

template< typename T >
inline
Array1D< T >
reshape( std::initializer_list< T > const & l )
{
	return Array1D< T >( l.size(), l );
}

template< typename T, typename I >
inline
Array1D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 1 > const & shape )
{
	return Array1D< T >( shape[ 0 ], l );
}

template< typename T, typename I >
inline
Array2D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 2 > const & shape )
{
	return Array2D< T >( shape[ 0 ], shape[ 1 ], l );
}

template< typename T, typename I >
inline
Array3D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 3 > const & shape )
{
	return Array3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], l );
}

template< typename T, typename I >
inline
Array4D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 4 > const & shape )
{
	return Array4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], l );
}

template< typename T, typename I >
inline
Array5D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 5 > const & shape )
{
	return Array5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], l );
}

template< typename T, typename I >
inline
Array6D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 6 > const & shape )
{
	return Array6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], l );
}

template< typename T, typename I >
inline
Array1D< T >
reshape1( Array< T > const & a, Array1< I > const & shape )
{
	assert( shape.size() == 1 );
	return Array1D< T >( shape[ 0 ], a );
}

template< typename T, typename I >
inline
Array2D< T >
reshape2( Array< T > const & a, Array1< I > const & shape )
{
	assert( shape.size() == 2 );
	return Array2D< T >( shape[ 0 ], shape[ 1 ], a );
}

template< typename T, typename I >
inline
Array3D< T >
reshape3( Array< T > const & a, Array1< I > const & shape )
{
	assert( shape.size() == 3 );
	return Array3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], a );
}

template< typename T, typename I >
inline
Array4D< T >
reshape4( Array< T > const & a, Array1< I > const & shape )
{
	assert( shape.size() == 4 );
	return Array4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], a );
}

template< typename T, typename I >
inline
Array5D< T >
reshape5( Array< T > const & a, Array1< I > const & shape )
{
	assert( shape.size() == 5 );
	return Array5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], a );
}

template< typename T, typename I >
inline
Array6D< T >
reshape6( Array< T > const & a, Array1< I > const & shape )
{
	assert( shape.size() == 6 );
	return Array6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], a );
}

template< typename T, typename I >
inline
Array1D< T >
reshape1( std::initializer_list< T > const & l, Array1< I > const & shape )
{
	assert( shape.size() == 1 );
	return Array1D< T >( shape[ 0 ], l );
}

template< typename T, typename I >
inline
Array2D< T >
reshape2( std::initializer_list< T > const & l, Array1< I > const & shape )
{
	assert( shape.size() == 2 );
	return Array2D< T >( shape[ 0 ], shape[ 1 ], l );
}

template< typename T, typename I >
inline
Array3D< T >
reshape3( std::initializer_list< T > const & l, Array1< I > const & shape )
{
	assert( shape.size() == 3 );
	return Array3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], l );
}

template< typename T, typename I >
inline
Array4D< T >
reshape4( std::initializer_list< T > const & l, Array1< I > const & shape )
{
	assert( shape.size() == 4 );
	return Array4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], l );
}

template< typename T, typename I >
inline
Array5D< T >
reshape5( std::initializer_list< T > const & l, Array1< I > const & shape )
{
	assert( shape.size() == 5 );
	return Array5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], l );
}

template< typename T, typename I >
inline
Array6D< T >
reshape6( std::initializer_list< T > const & l, Array1< I > const & shape )
{
	assert( shape.size() == 6 );
	return Array6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], l );
}

template< typename T, typename I >
inline
Array1D< T >
reshape1( Array< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 1 );
	return Array1D< T >( shape.begin()[ 0 ], a );
}

template< typename T, typename I >
inline
Array2D< T >
reshape2( Array< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 2 );
	return Array2D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], a );
}

template< typename T, typename I >
inline
Array3D< T >
reshape3( Array< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 3 );
	return Array3D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], a );
}

template< typename T, typename I >
inline
Array4D< T >
reshape4( Array< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 4 );
	return Array4D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], a );
}

template< typename T, typename I >
inline
Array5D< T >
reshape5( Array< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 5 );
	return Array5D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], a );
}

template< typename T, typename I >
inline
Array6D< T >
reshape6( Array< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 6 );
	return Array6D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], shape.begin()[ 5 ], a );
}

template< typename T, typename I >
inline
Array1D< T >
reshape1( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 1 );
	return Array1D< T >( shape.begin()[ 0 ], l );
}

template< typename T, typename I >
inline
Array2D< T >
reshape2( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 2 );
	return Array2D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], l );
}

template< typename T, typename I >
inline
Array3D< T >
reshape3( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 3 );
	return Array3D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], l );
}

template< typename T, typename I >
inline
Array4D< T >
reshape4( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 4 );
	return Array4D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], l );
}

template< typename T, typename I >
inline
Array5D< T >
reshape5( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 5 );
	return Array5D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], l );
}

template< typename T, typename I >
inline
Array6D< T >
reshape6( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 6 );
	return Array6D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], shape.begin()[ 5 ], l );
}

// pack /////

template< typename T >
inline
Array1D< T >
pack( Array< T > const & a, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array1D< T >( a, a.isize() ); // All elements
	} else {
		return Array1D< T >( 0 ); // Empty array
	}
}

template< typename T >
inline
Array1D< T >
pack( Array1< T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array2< T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array3< T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array4< T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array5< T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array6< T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

// cshift /////

template< typename T >
inline
Array1D< T >
cshift( Array1< T > const & a, int const shift, int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	Array1D< T > o( Array1D< T >::shape( a ) );
	int const b( a.l() ), e( a.u() ), s( a.isize() );
	int const h( shift > 0 ? ( shift % s ) - s : shift % s );
	for ( int i = b, j = 0; i <= e; ++i, ++j ) {
		o[ ( j - h ) % s ] = a( i );
	}
	return o;
}

template< typename T >
inline
Array2D< T >
cshift( Array2< T > const & a, int const shift, int const dim = 1 )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< T > o( Array2D< T >::shape( a ) );
	int const s1( a.isize1() );
	int const s2( a.isize2() );
	size_type l( 0u );
	switch ( dim ) {
	case 1:
		{
		int const h( ( shift % s1 ) + ( shift > 0 ? 1 - s1 : 1 ) );
		for ( int i1 = 1; i1 <= s1; ++i1 ) {
			int const j1( 1 + ( ( i1 - h ) % s1 ) );
			size_type m( o.index( j1, 1 ) );
			for ( int i2 = 1; i2 <= s2; ++i2, ++l, ++m ) {
				o[ m ] = a[ l ];
			}
		}
		break;
		}
	case 2:
		{
		int const h( ( shift % s2 ) + ( shift > 0 ? 1 - s2 : 1 ) );
		for ( int i1 = 1; i1 <= s1; ++i1 ) {
			for ( int i2 = 1; i2 <= s2; ++i2, ++l ) {
				o( i1, 1 + ( ( i2 - h ) % s2 ) ) = a[ l ];
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
cshift( Array2< T > const & a, Array1< int > const & shift, int const dim = 1 )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< T > o( Array2D< T >::shape( a ) );
	int const s1( a.isize1() );
	int const s2( a.isize2() );
	size_type l( 0u );
	switch ( dim ) {
	case 1:
		assert( shift.size() == a.size2() );
		for ( int i1 = 1; i1 <= s1; ++i1 ) {
			for ( int i2 = 1, k2 = shift.l(); i2 <= s2; ++i2, ++k2, ++l ) {
				int const h_( shift( k2 ) );
				int const h( ( h_ % s1 ) + ( h_ > 0 ? 1 - s1 : 1 ) );
				o( 1 + ( ( i1 - h ) % s1 ), i2 ) = a[ l ];
			}
		}
		break;
	case 2:
		assert( shift.size() == a.size1() );
		for ( int i1 = 1, k1 = shift.l(); i1 <= s1; ++i1, ++k1 ) {
			int const h_( shift( k1 ) );
			int const h( ( h_ % s2 ) + ( h_ > 0 ? 1 - s2 : 1 ) );
			for ( int i2 = 1; i2 <= s2; ++i2, ++l ) {
				o( i1, 1 + ( ( i2 - h ) % s2 ) ) = a[ l ];
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
eoshift( Array1< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	Array1D< T > o( Array1D< T >::shape( a, bdy ) );
	int const b( a.l() + std::max( shift, 0 ) ), e( a.u() + std::min( shift, 0 ) );
	for ( int i = b, j = std::max( 1 - shift, 1 ); i <= e; ++i, ++j ) {
		o( j ) = a( i );
	}
	return o;
}

template< typename T >
inline
Array2D< T >
eoshift( Array2< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< T > o( Array2D< T >::shape( a, bdy ) );
	switch ( dim ) {
	case 1:
		{
		int const b1( a.l1() + std::max( shift, 0 ) ), e1( a.u1() + std::min( shift, 0 ) );
		int const b2( a.l2() ), e2( a.u2() );
		size_type l( a.index( b1, b2 ) );
		size_type m( o.index( std::max( 1 - shift, 1 ), 1 ) );
		for ( int i1 = b1; i1 <= e1; ++i1 ) {
			for ( int i2 = b2; i2 <= e2; ++i2, ++l, ++m ) {
				o[ m ] = a[ l ];
			}
		}
		break;
		}
	case 2:
		{
		int const b1( a.l1() ), e1( a.u1() );
		int const b2( a.l2() + std::max( shift, 0 ) ), e2( a.u2() + std::min( shift, 0 ) );
		for ( int i1 = b1, j1 = 1; i1 <= e1; ++i1, ++j1 ) {
			size_type l( a.index( i1, b2 ) );
			size_type m( o.index( j1, std::max( 1 - shift, 1 ) ) );
			for ( int i2 = b2; i2 <= e2; ++i2, ++l, ++m ) {
				o[ m ] = a[ l ];
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
eoshift( Array2< T > const & a, Array1< int > const & shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< T > o( Array2D< T >::shape( a, bdy ) );
	switch ( dim ) {
	case 1:
		{
		size_type const s2( a.size2() );
		assert( shift.size() == s2 );
		int const b2( a.l2() ), e2( a.u2() );
		for ( int i2 = b2, j2 = 1, k2 = shift.l(); i2 <= e2; ++i2, ++j2, ++k2 ) {
			int const shift1( shift( k2 ) );
			int const b1( a.l1() + std::max( shift1, 0 ) ), e1( a.u1() + std::min( shift1, 0 ) );
			size_type l( a.index( b1, i2 ) );
			size_type m( o.index( std::max( 1 - shift1, 1 ), j2 ) );
			for ( int i1 = b1; i1 <= e1; ++i1, l += s2, m += s2 ) {
				o[ m ] = a[ l ];
			}
		}
		break;
		}
	case 2:
		{
		assert( shift.size() == a.size1() );
		int const b1( a.l1() ), e1( a.u1() );
		for ( int i1 = b1, j1 = 1, k1 = shift.l(); i1 <= e1; ++i1, ++j1, ++k1 ) {
			int const shift2( shift( k1 ) );
			int const b2( a.l2() + std::max( shift2, 0 ) ), e2( a.u2() + std::min( shift2, 0 ) );
			size_type l( a.index( i1, b2 ) );
			size_type m( o.index( j1, std::max( 1 - shift2, 1 ) ) );
			for ( int i2 = b2; i2 <= e2; ++i2, ++l, ++m ) {
				o[ m ] = a[ l ];
			}
		}
		break;
		}
	default:
		assert( false );
	}
	return o;
}

// sum /////

template< typename T >
inline
T
sum( Array< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	T r( 0 );
	for ( size_type i = 0; i < as; ++i ) {
		r += a[ i ];
	}
	return r;
}

template< typename T >
inline
T
sum( Array1< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	T r( 0 );
	for ( size_type i = 0; i < as; ++i ) {
		r += a[ i ];
	}
	return r;
}

template< typename T >
inline
Array1D< T >
sum( Array2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	switch ( dim ) {
	case 1:
		{
		Array1D< T > r( static_cast< int >( as2 ), T( 0 ) );
		for ( size_type i1 = 0, l = 0; i1 < as1; ++i1 ) {
			for ( size_type i2 = 0; i2 < as2; ++i2, ++l ) {
				r[ i2 ] += a[ l ];
			}
		}
		return r;
		}
	case 2:
		{
		Array1D< T > r( static_cast< int >( as1 ) );
		for ( size_type i1 = 0, l = 0; i1 < as1; ++i1 ) {
			T s( 0 );
			for ( size_type i2 = 0; i2 < as2; ++i2, ++l ) {
				s += a[ l ];
			}
			r[ i1 ] = s;
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
sum( Array< T > const & a, Array< bool > const & mask )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	assert( as == mask.size() ); // Fortran compliance requires conformable so this is looser
	T r( 0 );
	for ( size_type i = 0; i < as; ++i ) {
		if ( mask[ i ] ) r += a[ i ];
	}
	return r;
}

template< typename T >
inline
T
sum_row( Array2< T > const & a, int const i )
{
	typedef  typename Array2< T >::size_type  size_type;
	size_type l( a.index( i, a.l2() ) );
	T r( 0 );
	for ( int j = a.l2(), e = a.u2(); j <= e; ++j, ++l ) {
		r += a[ l ];
	}
	return r;
}

template< typename T >
inline
T
sum_col( Array2< T > const & a, int const j )
{
	assert( a.I1().bounded() );
	typedef  typename Array2< T >::size_type  size_type;
	size_type const s2( a.size2() );
	size_type l( j - a.l2() );
	T r( 0 );
	for ( int i = a.l1(), e = a.u1(); i <= e; ++i, l += s2 ) {
		r += a[ l ];
	}
	return r;
}

// product /////

template< typename T >
inline
T
product( Array< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	T r( 1 );
	for ( size_type i = 0; i < as; ++i ) {
		r *= a[ i ];
	}
	return r;
}

template< typename T >
inline
T
product( Array1< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	T r( 1 );
	for ( size_type i = 0; i < as; ++i ) {
		r *= a[ i ];
	}
	return r;
}

template< typename T >
inline
Array1D< T >
product( Array2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	switch ( dim ) {
	case 1:
		{
		Array1D< T > r( static_cast< int >( as2 ), T( 1 ) );
		for ( size_type i1 = 0, l = 0; i1 < as1; ++i1 ) {
			for ( size_type i2 = 0; i2 < as2; ++i2, ++l ) {
				r[ i2 ] *= a[ l ];
			}
		}
		return r;
		}
	case 2:
		{
		Array1D< T > r( static_cast< int >( as1 ) );
		for ( size_type i1 = 0, l = 0; i1 < as1; ++i1 ) {
			T p( 1 );
			for ( size_type i2 = 0; i2 < as2; ++i2, ++l ) {
				p *= a[ l ];
			}
			r[ i1 ] = p;
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
product( Array< T > const & a, Array< bool > const & mask )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	assert( as == mask.size() ); // Fortran compliance requires conformable so this is looser
	T r( 1 );
	for ( size_type i = 0; i < as; ++i ) {
		if ( mask[ i ] ) r *= a[ i ];
	}
	return r;
}

template< typename T >
inline
T
product_row( Array2< T > const & a, int const i )
{
	typedef  typename Array2< T >::size_type  size_type;
	T r( 1 );
	size_type l( a.index( i, a.l2() ) );
	for ( int j = a.l2(), e = a.u2(); j <= e; ++j, ++l ) {
		r *= a[ l ];
	}
	return r;
}

template< typename T >
inline
T
product_col( Array2< T > const & a, int const j )
{
	assert( a.I1().bounded() );
	typedef  typename Array2< T >::size_type  size_type;
	size_type const s2( a.size2() );
	T r( 1 );
	size_type l( j - a.l2() );
	for ( int i = a.l1(), e = a.u1(); i <= e; ++i, l += s2 ) {
		r *= a[ l ];
	}
	return r;
}

// abs /////

template< typename T >
inline
Array1D< T >
abs( Array1< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array1D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array2D< T >
abs( Array2< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array3D< T >
abs( Array3< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array3D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array4D< T >
abs( Array4< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array4D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array5D< T >
abs( Array5< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array5D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array6D< T >
abs( Array6< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array6D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

// pow /////

template< typename T, typename X >
inline
Array1D< T >
pow( Array1< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array1D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array2D< T >
pow( Array2< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array3D< T >
pow( Array3< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array3D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array4D< T >
pow( Array4< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array4D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array5D< T >
pow( Array5< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array5D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array6D< T >
pow( Array6< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array6D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

// sign /////

template< typename T, typename X >
inline
Array1D< T >
sign( Array1< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array1D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array2D< T >
sign( Array2< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array3D< T >
sign( Array3< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array3D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array4D< T >
sign( Array4< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array4D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array5D< T >
sign( Array5< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array5D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array6D< T >
sign( Array6< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array6D< T > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename X, typename T >
inline
Array1D< X >
sign( X const & x, Array1< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array1D< X > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array2D< X >
sign( X const & x, Array2< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array2D< X > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array3D< X >
sign( X const & x, Array3< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array3D< X > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array4D< X >
sign( X const & x, Array4< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array4D< X > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array5D< X >
sign( X const & x, Array5< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array5D< X > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array6D< X >
sign( X const & x, Array6< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	Array6D< X > r( a );
	for ( size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

// minval /////

template< typename T >
inline
T
minval( Array< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
	for ( size_type i = 1; i < a.size(); ++i ) {
		r = std::min( r, a[ i ] );
	}
	return r;
}

// maxval /////

template< typename T >
inline
T
maxval( Array< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
	for ( size_type i = 1; i < a.size(); ++i ) {
		r = std::max( r, a[ i ] );
	}
	return r;
}

// minloc /////

template< typename T >
inline
Array1D< int >
minloc( Array1< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as( a.isize1() );
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
	size_type l( 1u );
	for ( int i = 2; i <= as; ++i, ++l ) {
		if ( a[ l ] < r ) {
			r = a[ l ];
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array2< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	Array1D< int > loc( 2, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
			if ( a[ l ] < r ) {
				r = a[ l ];
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array3< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	Array1D< int > loc( 3, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3, ++l ) {
				if ( a[ l ] < r ) {
					r = a[ l ];
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
minloc( Array4< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	Array1D< int > loc( 4, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4, ++l ) {
					if ( a[ l ] < r ) {
						r = a[ l ];
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
Array1D< int >
minloc( Array5< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	Array1D< int > loc( 5, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5, ++l ) {
						if ( a[ l ] < r ) {
							r = a[ l ];
							loc = { i1, i2, i3, i4, i5 };
						}
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
minloc( Array6< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	Array1D< int > loc( 6, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5 ) {
						for ( int i6 = 1; i6 <= as6; ++i6, ++l ) {
							if ( a[ l ] < r ) {
								r = a[ l ];
								loc = { i1, i2, i3, i4, i5, i6 };
							}
						}
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
minloc( Array1< T > const & a, int const dim )
{
	assert( a.size_bounded() );
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
minloc( Array2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	switch ( dim ) {
	case 1:
		{
		Array1D< int > loc( as2, as2 > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		if ( as2 <= TypeTraits< T >::loc_2_crossover ) { // Cache-unfriendly small array method
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
				size_type l( i2 - 1 );
				for ( int i1 = 1; i1 <= as1; ++i1, l += as2 ) {
					if ( a[ l ] < r ) {
						r = a[ l ];
						loc( i2 ) = i1;
					}
				}
			}
		} else { // Cache-friendly large array method
			Array1D< T > val( as2, a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
			size_type l( 0u );
			for ( int i1 = 1; i1 <= as1; ++i1 ) {
				for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
					if ( a[ l ] < val( i2 ) ) {
						val( i2 ) = a[ l ];
						loc( i2 ) = i1;
					}
				}
			}
		}
		return loc;
		}
	case 2:
		{
		Array1D< int > loc( as1, as1 > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		size_type l( 0u );
		for ( int i1 = 1; i1 <= as1; ++i1 ) {
			T r( a.empty() ? std::numeric_limits< T >::max() : a[ 0 ] );
			for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
				if ( a[ l ] < r ) {
					r = a[ l ];
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
minloc( Array1< T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as( a.isize1() );
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i = 1; i <= as; ++i, ++l ) {
		if ( mask[ l ] ) {
			if ( first ) {
				first = false;
				r = a[ l ];
				loc = { i };
			} else if ( a[ l ] < r ) {
				r = a[ l ];
				loc = { i };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
minloc( Array2< T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	Array1D< int > loc( 2, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
			if ( mask[ l ] ) {
				if ( first ) {
					first = false;
					r = a[ l ];
					loc = { i1, i2 };
				} else if ( a[ l ] < r ) {
					r = a[ l ];
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
minloc( Array3< T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	Array1D< int > loc( 3, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3, ++l ) {
				if ( mask[ l ] ) {
					if ( first ) {
						first = false;
						r = a[ l ];
						loc = { i1, i2, i3 };
					} else if ( a[ l ] < r ) {
						r = a[ l ];
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
minloc( Array4< T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	Array1D< int > loc( 4, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4, ++l ) {
					if ( mask[ l ] ) {
						if ( first ) {
							first = false;
							r = a[ l ];
							loc = { i1, i2, i3, i4 };
						} else if ( a[ l ] < r ) {
							r = a[ l ];
							loc = { i1, i2, i3, i4 };
						}
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
minloc( Array5< T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	Array1D< int > loc( 5, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5, ++l ) {
						if ( mask[ l ] ) {
							if ( first ) {
								first = false;
								r = a[ l ];
								loc = { i1, i2, i3, i4, i5 };
							} else if ( a[ l ] < r ) {
								r = a[ l ];
								loc = { i1, i2, i3, i4, i5 };
							}
						}
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
minloc( Array6< T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	Array1D< int > loc( 6, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5 ) {
						for ( int i6 = 1; i6 <= as6; ++i6, ++l ) {
							if ( mask[ l ] ) {
								if ( first ) {
									first = false;
									r = a[ l ];
									loc = { i1, i2, i3, i4, i5, i6 };
								} else if ( a[ l ] < r ) {
									r = a[ l ];
									loc = { i1, i2, i3, i4, i5, i6 };
								}
							}
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
maxloc( Array1< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as( a.isize1() );
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
	size_type l( 1u );
	for ( int i = 2; i <= as; ++i, ++l ) {
		if ( a[ l ] > r ) {
			r = a[ l ];
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array2< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	Array1D< int > loc( 2, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
			if ( a[ l ] > r ) {
				r = a[ l ];
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array3< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	Array1D< int > loc( 3, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3, ++l ) {
				if ( a[ l ] > r ) {
					r = a[ l ];
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
maxloc( Array4< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	Array1D< int > loc( 4, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4, ++l ) {
					if ( a[ l ] > r ) {
						r = a[ l ];
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
Array1D< int >
maxloc( Array5< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	Array1D< int > loc( 5, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5, ++l ) {
						if ( a[ l ] > r ) {
							r = a[ l ];
							loc = { i1, i2, i3, i4, i5 };
						}
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
maxloc( Array6< T > const & a )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	Array1D< int > loc( 6, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
	size_type l( 0u );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5 ) {
						for ( int i6 = 1; i6 <= as6; ++i6, ++l ) {
							if ( a[ l ] > r ) {
								r = a[ l ];
								loc = { i1, i2, i3, i4, i5, i6 };
							}
						}
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
maxloc( Array1< T > const & a, int const dim )
{
	assert( a.size_bounded() );
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
maxloc( Array2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	switch ( dim ) {
	case 1:
		{
		Array1D< int > loc( as2, as2 > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		if ( as2 <= TypeTraits< T >::loc_2_crossover ) { // Cache-unfriendly small array method
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
				size_type l( i2 - 1 );
				for ( int i1 = 1; i1 <= as1; ++i1, l += as2 ) {
					if ( a[ l ] > r ) {
						r = a[ l ];
						loc( i2 ) = i1;
					}
				}
			}
		} else { // Cache-friendly large array method
			Array1D< T > val( as2, a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
			size_type l( 0u );
			for ( int i1 = 1; i1 <= as1; ++i1 ) {
				for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
					if ( a[ l ] > val( i2 ) ) {
						val( i2 ) = a[ l ];
						loc( i2 ) = i1;
					}
				}
			}
		}
		return loc;
		}
	case 2:
		{
		Array1D< int > loc( as1, as1 > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
		size_type l( 0u );
		for ( int i1 = 1; i1 <= as1; ++i1 ) {
			T r( a.empty() ? std::numeric_limits< T >::lowest() : a[ 0 ] );
			for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
				if ( a[ l ] > r ) {
					r = a[ l ];
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
maxloc( Array1< T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as( a.isize1() );
	Array1D< int > loc( 1, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i = 1; i <= as; ++i, ++l ) {
		if ( mask[ l ] ) {
			if ( first ) {
				first = false;
				r = a[ l ];
				loc = { i };
			} else if ( a[ l ] > r ) {
				r = a[ l ];
				loc = { i };
			}
		}
	}
	return loc;
}

template< typename T >
inline
Array1D< int >
maxloc( Array2< T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	Array1D< int > loc( 2, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2, ++l ) {
			if ( mask[ l ] ) {
				if ( first ) {
					first = false;
					r = a[ l ];
					loc = { i1, i2 };
				} else if ( a[ l ] > r ) {
					r = a[ l ];
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
maxloc( Array3< T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	Array1D< int > loc( 3, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3, ++l ) {
				if ( mask[ l ] ) {
					if ( first ) {
						first = false;
						r = a[ l ];
						loc = { i1, i2, i3 };
					} else if ( a[ l ] > r ) {
						r = a[ l ];
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
maxloc( Array4< T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	Array1D< int > loc( 4, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4, ++l ) {
					if ( mask[ l ] ) {
						if ( first ) {
							first = false;
							r = a[ l ];
							loc = { i1, i2, i3, i4 };
						} else if ( a[ l ] > r ) {
							r = a[ l ];
							loc = { i1, i2, i3, i4 };
						}
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
maxloc( Array5< T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	Array1D< int > loc( 5, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5, ++l ) {
						if ( mask[ l ] ) {
							if ( first ) {
								first = false;
								r = a[ l ];
								loc = { i1, i2, i3, i4, i5 };
							} else if ( a[ l ] > r ) {
								r = a[ l ];
								loc = { i1, i2, i3, i4, i5 };
							}
						}
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
maxloc( Array6< T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	Array1D< int > loc( 6, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1; i1 <= as1; ++i1 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i4 = 1; i4 <= as4; ++i4 ) {
					for ( int i5 = 1; i5 <= as5; ++i5 ) {
						for ( int i6 = 1; i6 <= as6; ++i6, ++l ) {
							if ( mask[ l ] ) {
								if ( first ) {
									first = false;
									r = a[ l ];
									loc = { i1, i2, i3, i4, i5, i6 };
								} else if ( a[ l ] > r ) {
									r = a[ l ];
									loc = { i1, i2, i3, i4, i5, i6 };
								}
							}
						}
					}
				}
			}
		}
	}
	return loc;
}

// matmul /////

// Matrix (Outer) Product of 1D Arrays
template< typename T >
inline
Array2D< T >
matmul( Array1< T > const & a, Array1< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs( b.size() );
	Array2D< T > m( static_cast< int >( as ), static_cast< int >( bs ) );
	for ( size_type la = 0, l = 0; la < as; ++la ) {
		T const a_i( a[ la ] );
		for ( size_type lb = 0; lb < bs; ++lb, ++l ) {
			m[ l ] = a_i * b[ lb ];
		}
	}
	return m;
}

// Matrix (Outer) Product of 1D Boolean Arrays
inline
Array2D< bool >
matmul( Array1< bool > const & a, Array1< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs( b.size() );
	Array2D< bool > m( static_cast< int >( as ), static_cast< int >( bs ) );
	for ( size_type la = 0, l = 0; la < as; ++la ) {
		bool const a_i( a[ la ] );
		for ( size_type lb = 0; lb < bs; ++lb, ++l ) {
			m[ l ] = a_i && b[ lb ];
		}
	}
	return m;
}

// Matrix Product of 1D and 2D Arrays
template< typename T >
inline
Array1D< T >
matmul( Array1< T > const & a, Array2< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs2( b.size2() );
	assert( as == b.size1() );
	if ( as <= TypeTraits< T >::matmul_1_2_crossover ) { // Cache-unfriendly small array method
		Array1D< T > m( static_cast< int >( bs2 ) );
		for ( size_type l = 0; l < bs2; ++l ) {
			T d( 0 );
			for ( size_type la = 0, lb = l; la < as; ++la, lb += bs2 ) {
				d += a[ la ] * b[ lb ];
			}
			m[ l ] = d;
		}
		return m;
	} else { // Cache-friendly large array method
		Array1D< T > m( static_cast< int >( bs2 ), T( 0 ) );
		for ( size_type la = 0, lb = 0; la < as; ++la ) {
			T const a_la( a[ la ] );
			for ( size_type l = 0; l < bs2; ++l, ++lb ) {
				m[ l ] += a_la * b[ lb ];
			}
		}
		return m;
	}
}

// Matrix Product of 1D and 2D Boolean Arrays
inline
Array1D< bool >
matmul( Array1< bool > const & a, Array2< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs2( b.size2() );
	assert( as == b.size1() );
	if ( as <= TypeTraits< bool >::matmul_1_2_crossover ) { // Cache-unfriendly small array method
		Array1D< bool > m( static_cast< int >( bs2 ) );
		for ( size_type l = 0; l < bs2; ++l ) {
			bool d( false );
			for ( size_type la = 0, lb = l; la < as; ++la, lb += bs2 ) {
				if ( a[ la ] && b[ lb ] ) {
					d = true;
					break;
				}
			}
			m[ l ] = d;
		}
		return m;
	} else { // Cache-friendly large array method
		Array1D< bool > m( static_cast< int >( bs2 ), false );
		for ( size_type la = 0, lb = 0; la < as; ++la ) {
			if ( a[ la ] ) {
				for ( size_type l = 0; l < bs2; ++l, ++lb ) {
					if ( b[ lb ] ) {
						m[ l ] = true;
						break;
					}
				}
			}
		}
		return m;
	}
}

// Matrix Product of 2D and 1D Arrays
template< typename T >
inline
Array1D< T >
matmul( Array2< T > const & a, Array1< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const bs( b.size() );
	assert( a.size2() == bs );
	Array1D< T > m( static_cast< int >( as1 ) );
	for ( size_type lr = 0, l = 0; lr < as1; ++lr ) {
		T d( 0 );
		for ( size_type lb = 0; lb < bs; ++lb, ++l ) {
			d += a[ l ] * b[ lb ];
		}
		m[ lr ] = d;
	}
	return m;
}

// Matrix Product of 2D and 1D Boolean Arrays
inline
Array1D< bool >
matmul( Array2< bool > const & a, Array1< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	size_type const bs( b.size() );
	assert( as2 == bs );
	Array1D< bool > m( static_cast< int >( as1 ) );
	for ( size_type lr = 0; lr < as1; ++lr ) {
		bool d( false );
		for ( size_type lb = 0, l = lr * as2; lb < bs; ++lb, ++l ) {
			if ( a[ l ] && b[ lb ] ) {
				d = true;
				break;
			}
		}
		m[ lr ] = d;
	}
	return m;
}

// Matrix Product of 2D Arrays
template< typename T >
inline
Array2D< T >
matmul( Array2< T > const & a, Array2< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	size_type const bs2( b.size2() );
	assert( as2 == b.size1() );
	if ( as2 <= TypeTraits< T >::matmul_2_2_crossover ) { // Cache-unfriendly small array method
		size_type const as( a.size() );
		size_type const bs( b.size() );
		Array2D< T > m( static_cast< int >( as1 ), static_cast< int >( bs2 ) );
		for ( size_type l1 = 0, l = 0; l1 < as; l1 += as2 ) {
			for ( size_type l2 = 0; l2 < bs2; ++l2, ++l ) {
				T d( 0 );
				for ( size_type la = l1, lb = l2; lb < bs; ++la, lb += bs2 ) {
					d += a[ la ] * b[ lb ];
				}
				m[ l ] = d;
			}
		}
		return m;
	} else { // Cache-friendly large array method
		Array2D< T > m( static_cast< int >( as1 ), static_cast< int >( bs2 ), T( 0 ) );
		for ( size_type i1 = 0, la = 0, l1 = 0; i1 < as1; ++i1, l1 += bs2 ) {
			for ( size_type i2 = 0, lb = 0; i2 < as2; ++i2, ++la ) {
				T const a_la( a[ la ] );
				for ( size_type j2 = 0, l = l1; j2 < bs2; ++j2, ++lb, ++l ) {
					m[ l ] += a_la * b[ lb ];
				}
			}
		}
		return m;
	}
}

// Matrix Product of 2D Boolean Arrays
inline
Array2D< bool >
matmul( Array2< bool > const & a, Array2< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	size_type const bs2( b.size2() );
	assert( as2 == b.size1() );
	if ( as2 <= TypeTraits< bool >::matmul_2_2_crossover ) { // Cache-unfriendly small array method
		size_type const as( a.size() );
		size_type const bs( b.size() );
		Array2D< bool > m( static_cast< int >( as1 ), static_cast< int >( bs2 ) );
		for ( size_type l1 = 0, l = 0; l1 < as; l1 += as2 ) {
			for ( size_type l2 = 0; l2 < bs2; ++l2, ++l ) {
				bool d( false );
				for ( size_type la = l1, lb = l2; lb < bs; ++la, lb += bs2 ) {
					if ( a[ la ] && b[ lb ] ) {
						d = true;
						break;
					}
				}
				m[ l ] = d;
			}
		}
		return m;
	} else { // Cache-friendly large array method
		Array2D< bool > m( static_cast< int >( as1 ), static_cast< int >( bs2 ), false );
		for ( size_type i1 = 0, la = 0, l1 = 0; i1 < as1; ++i1, l1 += bs2 ) {
			for ( size_type i2 = 0, lb = 0; i2 < as2; ++i2, ++la ) {
				if ( a[ la ] ) {
					for ( size_type j2 = 0, l = l1; j2 < bs2; ++j2, ++lb, ++l ) {
						if ( b[ lb ] ) m[ l ] = true;
					}
				}
			}
		}
		return m;
	}
}

// Matrix Product of 2D Arrays with Right Array Transposed
template< typename T >
inline
Array2D< T >
matmul_T( Array2< T > const & a, Array2< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	size_type const as( a.size() );
	size_type const bs1( b.size1() );
	assert( as2 == b.size2() );
	Array2D< T > m( static_cast< int >( as1 ), static_cast< int >( bs1 ) );
	for ( size_type l1 = 0, l = 0; l1 < as; l1 += as2 ) {
		for ( size_type l2 = 0, lb = 0; l2 < bs1; ++l2, ++l ) {
			T d( 0 );
			for ( size_type la = l1, la_end = l1 + as2; la < la_end; ++la, ++lb ) {
				d += a[ la ] * b[ lb ];
			}
			m[ l ] = d;
		}
	}
	return m;
}

// Matrix Product of 2D Boolean Arrays with Right Array Transposed
inline
Array2D< bool >
matmul_T( Array2< bool > const & a, Array2< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  Array< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	size_type const as( a.size() );
	size_type const bs1( b.size1() );
	assert( as2 == b.size2() );
	Array2D< bool > m( static_cast< int >( as1 ), static_cast< int >( bs1 ) );
	for ( size_type l1 = 0, l = 0; l1 < as; l1 += as2 ) {
		for ( size_type l2 = 0, lb = 0; l2 < bs1; ++l2, ++l ) {
			bool d( false );
			for ( size_type la = l1, la_end = l1 + as2; la < la_end; ++la, ++lb ) {
				if ( a[ la ] && b[ lb ] ) {
					d = true;
					break;
				}
			}
			m[ l ] = d;
		}
	}
	return m;
}

// Subscript Array Generators

// Subscripted Array
template< typename T >
inline
Array1D< T >
array_sub( Array1< T > const & a, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	Array1D< T > r( sub.isize() );
	for ( int i = sub.l(), e = sub.u(), k = 1; i <= e; ++i, ++k ) {
		r( k ) = a( sub( i ) );
	}
	return r;
}

} // ObjexxFCL

#endif // ObjexxFCL_Array_functions_hh_INCLUDED
