#ifndef ObjexxFCL_FArray_functions_hh_INCLUDED
#define ObjexxFCL_FArray_functions_hh_INCLUDED

// FArray Functions
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/FArray3D.hh>
#include <ObjexxFCL/FArray4D.hh>
#include <ObjexxFCL/FArray5D.hh>
#include <ObjexxFCL/FArray6D.hh>
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
allocated( FArray< T > const & a )
{
	return a.allocated();
}

template< typename T >
inline
void
allocate( FArray1D< T > & a, DynamicIndexRange const & I )
{
	a.allocate( I );
}

template< typename T >
inline
void
allocate( FArray2D< T > & a, DynamicIndexRange const & I1, DynamicIndexRange const & I2 )
{
	a.allocate( I1, I2 );
}

template< typename T >
inline
void
allocate( FArray3D< T > & a, DynamicIndexRange const & I1, DynamicIndexRange const & I2, DynamicIndexRange const & I3 )
{
	a.allocate( I1, I2, I3 );
}

template< typename T >
inline
void
allocate( FArray4D< T > & a, DynamicIndexRange const & I1, DynamicIndexRange const & I2, DynamicIndexRange const & I3, DynamicIndexRange const & I4 )
{
	a.allocate( I1, I2, I3, I4 );
}

template< typename T >
inline
void
allocate( FArray5D< T > & a, DynamicIndexRange const & I1, DynamicIndexRange const & I2, DynamicIndexRange const & I3, DynamicIndexRange const & I4, DynamicIndexRange const & I5 )
{
	a.allocate( I1, I2, I3, I4, I5 );
}

template< typename T >
inline
void
allocate( FArray6D< T > & a, DynamicIndexRange const & I1, DynamicIndexRange const & I2, DynamicIndexRange const & I3, DynamicIndexRange const & I4, DynamicIndexRange const & I5, DynamicIndexRange const & I6 )
{
	a.allocate( I1, I2, I3, I4, I5, I6 );
}

template< typename T >
inline
void
deallocate( FArray< T > & a )
{
	a.clear();
}

// all /////

inline
bool
all( FArray< bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return true;
	for ( FArray< bool >::size_type i = 0; i < a.size(); ++i ) {
		if ( ! a[ i ] ) return false;
	}
	return true;
}

// any /////

inline
bool
any( FArray< bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return false;
	for ( FArray< bool >::size_type i = 0; i < a.size(); ++i ) {
		if ( a[ i ] ) return true;
	}
	return false;
}

// negation /////

inline
FArray1D< bool >
operator !( FArray1< bool > const & a )
{
	FArray1D< bool > r( a );
	for ( FArray1< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray2D< bool >
operator !( FArray2< bool > const & a )
{
	FArray2D< bool > r( a );
	for ( FArray2< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray3D< bool >
operator !( FArray3< bool > const & a )
{
	FArray3D< bool > r( a );
	for ( FArray3< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray4D< bool >
operator !( FArray4< bool > const & a )
{
	FArray4D< bool > r( a );
	for ( FArray4< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray5D< bool >
operator !( FArray5< bool > const & a )
{
	FArray5D< bool > r( a );
	for ( FArray5< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray6D< bool >
operator !( FArray6< bool > const & a )
{
	FArray6D< bool > r( a );
	for ( FArray6< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

// count /////

inline
FArray< bool >::size_type
count( FArray< bool > const & a )
{
	assert( a.size_bounded() );
	FArray< bool >::size_type c( 0u );
	for ( FArray< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		if ( a[ i ] ) ++c;
	}
	return c;
}

inline
FArray< bool >::size_type
count( FArray1< bool > const & a, int const dim )
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
FArray1D< FArray< bool >::size_type >
count( FArray2< bool > const & a, int const dim )
{
	assert( a.size_bounded() );
	switch ( dim ) {
	case 1:
		{
			FArray1D< FArray< bool >::size_type > v( a.isize2() );
			for ( int i2 = a.l2(), e2 = a.u2(), k2 = 1; i2 <= e2; ++i2, ++k2 ) {
				FArray< bool >::size_type c( 0u );
				for ( int i1 = a.l1(), e1 = a.u1(); i1 <= e1; ++i1 ) {
					if ( a( i1, i2 ) ) ++c;
				}
				v( k2 ) = c;
			}
			return v;
		}
	case 2:
		{
			FArray1D< FArray< bool >::size_type > v( a.isize1() );
			for ( int i1 = a.l1(), e1 = a.u1(), k1 = 1; i1 <= e1; ++i1, ++k1 ) {
				FArray< bool >::size_type c( 0u );
				for ( int i2 = a.l2(), e2 = a.u2(); i2 <= e2; ++i2 ) {
					if ( a( i1, i2 ) ) ++c;
				}
				v( k1 ) = c;
			}
			return v;
		}
	default:
		assert( false );
		return FArray1D< FArray< bool >::size_type >();
	}
}

// is_contiguous /////

template< typename T >
inline
bool
is_contiguous( FArray< T > const & a )
{
	return a.is_contiguous();
}

// lbound /////

template< typename T >
inline
FArray1D< int >
lbound( FArray1< T > const & a )
{
	return FArray1D< int >( 1, a.l1() );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray2< T > const & a )
{
	return FArray1D< int >( 2, { a.l1(), a.l2() } );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray3< T > const & a )
{
	return FArray1D< int >( 3, { a.l1(), a.l2(), a.l3() } );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray4< T > const & a )
{
	return FArray1D< int >( 4, { a.l1(), a.l2(), a.l3(), a.l4() } );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray5< T > const & a )
{
	return FArray1D< int >( 5, { a.l1(), a.l2(), a.l3(), a.l4(), a.l5() } );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray6< T > const & a )
{
	return FArray1D< int >( 6, { a.l1(), a.l2(), a.l3(), a.l4(), a.l5(), a.l6() } );
}

template< typename T >
inline
int
lbound( FArray1< T > const & a, int const dim )
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
lbound( FArray2< T > const & a, int const dim )
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
lbound( FArray3< T > const & a, int const dim )
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
lbound( FArray4< T > const & a, int const dim )
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
lbound( FArray5< T > const & a, int const dim )
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
lbound( FArray6< T > const & a, int const dim )
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
FArray1D< int >
ubound( FArray1< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 1, a.u1() );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray2< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 2, { a.u1(), a.u2() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray3< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 3, { a.u1(), a.u2(), a.u3() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray4< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 4, { a.u1(), a.u2(), a.u3(), a.u4() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray5< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 5, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray6< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 6, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5(), a.u6() } );
}

template< typename T >
inline
int
ubound( FArray1< T > const & a, int const dim )
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
ubound( FArray2< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.u1();
	case 2:
		assert( a.I2().bounded() );
		return a.u2();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( FArray3< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.u1();
	case 2:
		return a.u2();
	case 3:
		assert( a.I3().bounded() );
		return a.u3();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( FArray4< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.u1();
	case 2:
		return a.u2();
	case 3:
		return a.u3();
	case 4:
		assert( a.I4().bounded() );
		return a.u4();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( FArray5< T > const & a, int const dim )
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
	case 5:
		assert( a.I5().bounded() );
		return a.u5();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( FArray6< T > const & a, int const dim )
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
	case 5:
		return a.u5();
	case 6:
		assert( a.I6().bounded() );
		return a.u6();
	default:
		assert( false );
		return 0;
	}
}

// shape /////

template< typename T >
inline
FArray1D< int >
shape( FArray1< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 1, a.isize1() );
}

template< typename T >
inline
FArray1D< int >
shape( FArray2< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 2, { a.isize1(), a.isize2() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray3< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 3, { a.isize1(), a.isize2(), a.isize3() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray4< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 4, { a.isize1(), a.isize2(), a.isize3(), a.isize4() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray5< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 5, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray6< T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 6, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() } );
}

// size /////

template< typename T >
inline
typename FArray< T >::size_type
size( FArray< T > const & a )
{
	assert( a.size_bounded() );
	return a.size();
}

template< typename T >
inline
typename FArray< T >::size_type
size( FArray1< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		assert( a.I1().bounded() );
		return a.size1();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
typename FArray< T >::size_type
size( FArray2< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	case 2:
		assert( a.I2().bounded() );
		return a.size2();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
typename FArray< T >::size_type
size( FArray3< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	case 2:
		return a.size2();
	case 3:
		assert( a.I3().bounded() );
		return a.size3();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
typename FArray< T >::size_type
size( FArray4< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	case 2:
		return a.size2();
	case 3:
		return a.size3();
	case 4:
		assert( a.I4().bounded() );
		return a.size4();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
typename FArray< T >::size_type
size( FArray5< T > const & a, int const dim )
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
		assert( a.I5().bounded() );
		return a.size5();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
typename FArray< T >::size_type
size( FArray6< T > const & a, int const dim )
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
		assert( a.I6().bounded() );
		return a.size6();
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
isize( FArray< T > const & a )
{
	return static_cast< int >( size( a ) );
}

template< template< typename > class A, typename T >
inline
typename std::enable_if< std::is_base_of< FArray< T >, A< T > >::value, int >::type // Restrict to FArray
isize( A< T > const & a, int const dim )
{
	return static_cast< int >( size( a, dim ) );
}

// reshape /////

template< typename T >
inline
FArray1D< T >
reshape( FArray< T > const & a )
{
	return FArray1D< T >( a );
}

template< typename T, typename I >
inline
FArray1D< T >
reshape( FArray< T > const & a, std::array< I, 1 > const & shape )
{
	return FArray1D< T >( shape[ 0 ], a );
}

template< typename T, typename I >
inline
FArray2D< T >
reshape( FArray< T > const & a, std::array< I, 2 > const & shape )
{
	return FArray2D< T >( shape[ 0 ], shape[ 1 ], a );
}

template< typename T, typename I >
inline
FArray3D< T >
reshape( FArray< T > const & a, std::array< I, 3 > const & shape )
{
	return FArray3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], a );
}

template< typename T, typename I >
inline
FArray4D< T >
reshape( FArray< T > const & a, std::array< I, 4 > const & shape )
{
	return FArray4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], a );
}

template< typename T, typename I >
inline
FArray5D< T >
reshape( FArray< T > const & a, std::array< I, 5 > const & shape )
{
	return FArray5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], a );
}

template< typename T, typename I >
inline
FArray6D< T >
reshape( FArray< T > const & a, std::array< I, 6 > const & shape )
{
	return FArray6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], a );
}

template< typename T >
inline
FArray1D< T >
reshape( std::initializer_list< T > const & l )
{
	return FArray1D< T >( l.size(), l );
}

template< typename T, typename I >
inline
FArray1D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 1 > const & shape )
{
	return FArray1D< T >( shape[ 0 ], l );
}

template< typename T, typename I >
inline
FArray2D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 2 > const & shape )
{
	return FArray2D< T >( shape[ 0 ], shape[ 1 ], l );
}

template< typename T, typename I >
inline
FArray3D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 3 > const & shape )
{
	return FArray3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], l );
}

template< typename T, typename I >
inline
FArray4D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 4 > const & shape )
{
	return FArray4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], l );
}

template< typename T, typename I >
inline
FArray5D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 5 > const & shape )
{
	return FArray5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], l );
}

template< typename T, typename I >
inline
FArray6D< T >
reshape( std::initializer_list< T > const & l, std::array< I, 6 > const & shape )
{
	return FArray6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], l );
}

template< typename T, typename I >
inline
FArray1D< T >
reshape1( FArray< T > const & a, FArray1< I > const & shape )
{
	assert( shape.size() == 1 );
	return FArray1D< T >( shape[ 0 ], a );
}

template< typename T, typename I >
inline
FArray2D< T >
reshape2( FArray< T > const & a, FArray1< I > const & shape )
{
	assert( shape.size() == 2 );
	return FArray2D< T >( shape[ 0 ], shape[ 1 ], a );
}

template< typename T, typename I >
inline
FArray3D< T >
reshape3( FArray< T > const & a, FArray1< I > const & shape )
{
	assert( shape.size() == 3 );
	return FArray3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], a );
}

template< typename T, typename I >
inline
FArray4D< T >
reshape4( FArray< T > const & a, FArray1< I > const & shape )
{
	assert( shape.size() == 4 );
	return FArray4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], a );
}

template< typename T, typename I >
inline
FArray5D< T >
reshape5( FArray< T > const & a, FArray1< I > const & shape )
{
	assert( shape.size() == 5 );
	return FArray5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], a );
}

template< typename T, typename I >
inline
FArray6D< T >
reshape6( FArray< T > const & a, FArray1< I > const & shape )
{
	assert( shape.size() == 6 );
	return FArray6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], a );
}

template< typename T, typename I >
inline
FArray1D< T >
reshape1( std::initializer_list< T > const & l, FArray1< I > const & shape )
{
	assert( shape.size() == 1 );
	return FArray1D< T >( shape[ 0 ], l );
}

template< typename T, typename I >
inline
FArray2D< T >
reshape2( std::initializer_list< T > const & l, FArray1< I > const & shape )
{
	assert( shape.size() == 2 );
	return FArray2D< T >( shape[ 0 ], shape[ 1 ], l );
}

template< typename T, typename I >
inline
FArray3D< T >
reshape3( std::initializer_list< T > const & l, FArray1< I > const & shape )
{
	assert( shape.size() == 3 );
	return FArray3D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], l );
}

template< typename T, typename I >
inline
FArray4D< T >
reshape4( std::initializer_list< T > const & l, FArray1< I > const & shape )
{
	assert( shape.size() == 4 );
	return FArray4D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], l );
}

template< typename T, typename I >
inline
FArray5D< T >
reshape5( std::initializer_list< T > const & l, FArray1< I > const & shape )
{
	assert( shape.size() == 5 );
	return FArray5D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], l );
}

template< typename T, typename I >
inline
FArray6D< T >
reshape6( std::initializer_list< T > const & l, FArray1< I > const & shape )
{
	assert( shape.size() == 6 );
	return FArray6D< T >( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ], l );
}

template< typename T, typename I >
inline
FArray1D< T >
reshape1( FArray< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 1 );
	return FArray1D< T >( shape.begin()[ 0 ], a );
}

template< typename T, typename I >
inline
FArray2D< T >
reshape2( FArray< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 2 );
	return FArray2D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], a );
}

template< typename T, typename I >
inline
FArray3D< T >
reshape3( FArray< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 3 );
	return FArray3D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], a );
}

template< typename T, typename I >
inline
FArray4D< T >
reshape4( FArray< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 4 );
	return FArray4D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], a );
}

template< typename T, typename I >
inline
FArray5D< T >
reshape5( FArray< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 5 );
	return FArray5D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], a );
}

template< typename T, typename I >
inline
FArray6D< T >
reshape6( FArray< T > const & a, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 6 );
	return FArray6D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], shape.begin()[ 5 ], a );
}

template< typename T, typename I >
inline
FArray1D< T >
reshape1( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 1 );
	return FArray1D< T >( shape.begin()[ 0 ], l );
}

template< typename T, typename I >
inline
FArray2D< T >
reshape2( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 2 );
	return FArray2D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], l );
}

template< typename T, typename I >
inline
FArray3D< T >
reshape3( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 3 );
	return FArray3D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], l );
}

template< typename T, typename I >
inline
FArray4D< T >
reshape4( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 4 );
	return FArray4D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], l );
}

template< typename T, typename I >
inline
FArray5D< T >
reshape5( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 5 );
	return FArray5D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], l );
}

template< typename T, typename I >
inline
FArray6D< T >
reshape6( std::initializer_list< T > const & l, std::initializer_list< I > const & shape )
{
	assert( shape.size() == 6 );
	return FArray6D< T >( shape.begin()[ 0 ], shape.begin()[ 1 ], shape.begin()[ 2 ], shape.begin()[ 3 ], shape.begin()[ 4 ], shape.begin()[ 5 ], l );
}

// pack /////

template< typename T >
inline
FArray1D< T >
pack( FArray< T > const & a, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return FArray1D< T >( a, a.isize() ); // All elements
	} else {
		return FArray1D< T >( 0 ); // Empty array
	}
}

template< typename T >
inline
FArray1D< T >
pack( FArray1< T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  FArray1< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray2< T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  FArray2< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray3< T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  FArray3< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray4< T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  FArray4< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray5< T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  FArray5< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray6< T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  FArray6< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type i = 0, e = mask.size(); i < e; ++i ) {
		if ( mask[ i ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	for ( size_type i = 0, e = mask.size(), k = 0; i < e; ++i ) {
		if ( mask[ i ] ) r[ k++ ] = a[ i ];
	}
	return r;
}

// cshift /////

template< typename T >
inline
FArray1D< T >
cshift( FArray1< T > const & a, int const shift, int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	FArray1D< T > o( FArray1D< T >::shape( a ) );
	if ( a.dimensions_initialized() ) {
		if ( o.dimensions_initialized() ) {
			int const b( a.l() ), e( a.u() ), s( a.isize() );
			for ( int i = b, j = 0; i <= e; ++i, ++j ) {
				o[ ( ( ( j - shift ) % s ) + s ) % s ] = a( i );
			}
		}
	}
	return o;
}

template< typename T >
inline
FArray2D< T >
cshift( FArray2< T > const & a, int const shift, int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( ( 1 <= dim ) && ( dim <= 2 ) );
	FArray2D< T > o( FArray2D< T >::shape( a ) );
	if ( a.dimensions_initialized() ) {
		if ( o.dimensions_initialized() ) {
			int const b1( a.l1() ), e1( a.u1() );
			int const b2( a.l2() ), e2( a.u2() );
			int const sp( shift + 1 );
			if ( dim == 1 ) {
				int const s1( a.isize1() );
				for ( int i2 = b2, j2 = 1; i2 <= e2; ++i2, ++j2 ) {
					for ( int i1 = b1, j1 = 1; i1 <= e1; ++i1, ++j1 ) {
						o( 1 + ( ( ( j1 - sp ) % s1 ) + s1 ) % s1, j2 ) = a( i1, i2 );
					}
				}
			} else if ( dim == 2 ) {
				int const s2( a.isize2() );
				for ( int i2 = b2, j2 = 1; i2 <= e2; ++i2, ++j2 ) {
					for ( int i1 = b1, j1 = 1; i1 <= e1; ++i1, ++j1 ) {
						o( j1, 1 + ( ( ( j2 - sp ) % s2 ) + s2 ) % s2 ) = a( i1, i2 );
					}
				}
			}
		}
	}
	return o;
}

template< typename T >
inline
FArray2D< T >
cshift( FArray2< T > const & a, FArray1< int > const & shift, int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( ( 1 <= dim ) && ( dim <= 2 ) );
	FArray2D< T > o( FArray2D< T >::shape( a ) );
	if ( a.dimensions_initialized() ) {
		if ( o.dimensions_initialized() ) {
			int const b1( a.l1() ), e1( a.u1() );
			int const b2( a.l2() ), e2( a.u2() );
			if ( dim == 1 ) {
				assert( shift.size() == a.size2() );
				int const s1( a.isize1() );
				for ( int i2 = b2, j2 = 1, k2 = shift.l(); i2 <= e2; ++i2, ++j2, ++k2 ) {
					int const sp( shift( k2 ) + 1 );
					for ( int i1 = b1, j1 = 1; i1 <= e1; ++i1, ++j1 ) {
						o( 1 + ( ( ( j1 - sp ) % s1 ) + s1 ) % s1, j2 ) = a( i1, i2 );
					}
				}
			} else if ( dim == 2 ) {
				assert( shift.size() == a.size1() );
				int const s2( a.isize2() );
				for ( int i1 = b1, j1 = 1, k1 = shift.l(); i1 <= e1; ++i1, ++j1, ++k1 ) {
					int const sp( shift( k1 ) + 1 );
					for ( int i2 = b2, j2 = 1; i2 <= e2; ++i2, ++j2 ) {
						o( j1, 1 + ( ( ( j2 - sp ) % s2 ) + s2 ) % s2 ) = a( i1, i2 );
					}
				}
			}
		}
	}
	return o;
}

// eoshift /////

template< typename T >
inline
FArray1D< T >
eoshift( FArray1< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	FArray1D< T > o( FArray1D< T >::shape( a, bdy ) );
	if ( a.dimensions_initialized() ) {
		if ( o.dimensions_initialized() ) {
			int const b( a.l() + std::max( shift, 0 ) ), e( a.u() + std::min( shift, 0 ) );
			for ( int i = b, j = std::max( 1 - shift, 1 ); i <= e; ++i, ++j ) {
				o( j ) = a( i );
			}
		}
	}
	return o;
}

template< typename T >
inline
FArray2D< T >
eoshift( FArray2< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( ( 1 <= dim ) && ( dim <= 2 ) );
	FArray2D< T > o( FArray2D< T >::shape( a, bdy ) );
	if ( a.dimensions_initialized() ) {
		if ( o.dimensions_initialized() ) {
			int const shift1( dim == 1 ? shift : 0 );
			int const shift2( dim == 2 ? shift : 0 );
			int const b1( a.l1() + std::max( shift1, 0 ) ), e1( a.u1() + std::min( shift1, 0 ) );
			int const b2( a.l2() + std::max( shift2, 0 ) ), e2( a.u2() + std::min( shift2, 0 ) );
			for ( int i2 = b2, j2 = std::max( 1 - shift2, 1 ); i2 <= e2; ++i2, ++j2 ) {
				for ( int i1 = b1, j1 = std::max( 1 - shift1, 1 ); i1 <= e1; ++i1, ++j1 ) {
					o( j1, j2 ) = a( i1, i2 );
				}
			}
		}
	}
	return o;
}

template< typename T >
inline
FArray2D< T >
eoshift( FArray2< T > const & a, FArray1< int > const & shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
	assert( ( 1 <= dim ) && ( dim <= 2 ) );
	FArray2D< T > o( FArray2D< T >::shape( a, bdy ) );
	if ( a.dimensions_initialized() ) {
		if ( o.dimensions_initialized() ) {
			if ( dim == 1 ) {
				assert( shift.size() == a.size2() );
				int const b2( a.l2() ), e2( a.u2() );
				for ( int i2 = b2, j2 = 1, k2 = shift.l(); i2 <= e2; ++i2, ++j2, ++k2 ) {
					int const shift1( shift( k2 ) );
					int const b1( a.l1() + std::max( shift1, 0 ) ), e1( a.u1() + std::min( shift1, 0 ) );
					for ( int i1 = b1, j1 = std::max( 1 - shift1, 1 ); i1 <= e1; ++i1, ++j1 ) {
						o( j1, j2 ) = a( i1, i2 );
					}
				}
			} else if ( dim == 2 ) {
				assert( shift.size() == a.size1() );
				int const b1( a.l1() ), e1( a.u1() );
				for ( int i1 = b1, j1 = 1, k1 = shift.l(); i1 <= e1; ++i1, ++j1, ++k1 ) {
					int const shift2( shift( k1 ) );
					int const b2( a.l2() + std::max( shift2, 0 ) ), e2( a.u2() + std::min( shift2, 0 ) );
					for ( int i2 = b2, j2 = std::max( 1 - shift2, 1 ); i2 <= e2; ++i2, ++j2 ) {
						o( j1, j2 ) = a( i1, i2 );
					}
				}
			}
		}
	}
	return o;
}

// sum /////

template< typename T >
inline
T
sum( FArray< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
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
sum( FArray1< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as( a.size() );
	T r( 0 );
	for ( size_type i = 0; i < as; ++i ) {
		r += a[ i ];
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
sum( FArray2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	size_type l( 0 ); // Linear index
	switch ( dim ) {
	case 1:
		{
			FArray1D< T > res( as2, T( 0 ) );
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				T r( 0 );
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
					r += a[ l ];
				}
				res( i2 ) = r;
			}
			return res;
		}
	case 2:
		{
			FArray1D< T > res( as1, T( 0 ) );
			for ( int i1 = 1; i1 <= as1; ++i1 ) {
				T r( 0 );
				l = i1 - 1;
				for ( int i2 = 1; i2 <= as2; ++i2, l += as1 ) {
					r += a[ l ];
				}
				res( i1 ) = r;
			}
			return res;
		}
	default:
		assert( false );
		return FArray1D< T >();
	}
}

template< typename T >
inline
T
sum( FArray< T > const & a, FArray< bool > const & mask )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as( a.size() );
	assert( as == mask.size() ); // Fortran compliance requires conformable so this is looser
	T r( 0 );
	for ( size_type i = 0; i < as; ++i ) {
		if ( mask[ i ] ) r += a[ i ];
	}
	return r;
}

// product /////

template< typename T >
inline
T
product( FArray< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
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
product( FArray1< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as( a.size() );
	T r( 1 );
	for ( size_type i = 0; i < as; ++i ) {
		r *= a[ i ];
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
product( FArray2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	size_type l( 0 ); // Linear index
	switch ( dim ) {
	case 1:
		{
			FArray1D< T > res( as2, T( 1 ) );
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				T r( 1 );
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
					r *= a[ l ];
				}
				res( i2 ) = r;
			}
			return res;
		}
	case 2:
		{
			FArray1D< T > res( as1, T( 1 ) );
			for ( int i1 = 1; i1 <= as1; ++i1 ) {
				T r( 1 );
				l = i1 - 1;
				for ( int i2 = 1; i2 <= as2; ++i2, l *= as1 ) {
					r *= a[ l ];
				}
				res( i1 ) = r;
			}
			return res;
		}
	default:
		assert( false );
		return FArray1D< T >();
	}
}

template< typename T >
inline
T
product( FArray< T > const & a, FArray< bool > const & mask )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as( a.size() );
	assert( as == mask.size() ); // Fortran compliance requires conformable so this is looser
	T r( 1 );
	for ( size_type i = 0; i < as; ++i ) {
		if ( mask[ i ] ) r *= a[ i ];
	}
	return r;
}

// abs /////

template< typename T >
inline
FArray1D< T >
abs( FArray1< T > const & a )
{
	assert( a.size_bounded() );
	FArray1D< T > r( a );
	for ( typename FArray1< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray2D< T >
abs( FArray2< T > const & a )
{
	assert( a.size_bounded() );
	FArray2D< T > r( a );
	for ( typename FArray2< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray3D< T >
abs( FArray3< T > const & a )
{
	assert( a.size_bounded() );
	FArray3D< T > r( a );
	for ( typename FArray3< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray4D< T >
abs( FArray4< T > const & a )
{
	assert( a.size_bounded() );
	FArray4D< T > r( a );
	for ( typename FArray4< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray5D< T >
abs( FArray5< T > const & a )
{
	assert( a.size_bounded() );
	FArray5D< T > r( a );
	for ( typename FArray5< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray6D< T >
abs( FArray6< T > const & a )
{
	assert( a.size_bounded() );
	FArray6D< T > r( a );
	for ( typename FArray6< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

// pow /////

template< typename T, typename X >
inline
FArray1D< T >
pow( FArray1< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray1D< T > r( a );
	for ( typename FArray1< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray2D< T >
pow( FArray2< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray2D< T > r( a );
	for ( typename FArray2< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray3D< T >
pow( FArray3< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray3D< T > r( a );
	for ( typename FArray3< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray4D< T >
pow( FArray4< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray4D< T > r( a );
	for ( typename FArray4< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray5D< T >
pow( FArray5< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray5D< T > r( a );
	for ( typename FArray5< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray6D< T >
pow( FArray6< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray6D< T > r( a );
	for ( typename FArray6< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

// sign /////

template< typename T, typename X >
inline
FArray1D< T >
sign( FArray1< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray1D< T > r( a );
	for ( typename FArray1< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray2D< T >
sign( FArray2< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray2D< T > r( a );
	for ( typename FArray2< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray3D< T >
sign( FArray3< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray3D< T > r( a );
	for ( typename FArray3< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray4D< T >
sign( FArray4< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray4D< T > r( a );
	for ( typename FArray4< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray5D< T >
sign( FArray5< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray5D< T > r( a );
	for ( typename FArray5< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray6D< T >
sign( FArray6< T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray6D< T > r( a );
	for ( typename FArray6< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename X, typename T >
inline
FArray1D< X >
sign( X const & x, FArray1< T > const & a )
{
	assert( a.size_bounded() );
	FArray1D< X > r( a );
	for ( typename FArray1< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray2D< X >
sign( X const & x, FArray2< T > const & a )
{
	assert( a.size_bounded() );
	FArray2D< X > r( a );
	for ( typename FArray2< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray3D< X >
sign( X const & x, FArray3< T > const & a )
{
	assert( a.size_bounded() );
	FArray3D< X > r( a );
	for ( typename FArray3< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray4D< X >
sign( X const & x, FArray4< T > const & a )
{
	assert( a.size_bounded() );
	FArray4D< X > r( a );
	for ( typename FArray4< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray5D< X >
sign( X const & x, FArray5< T > const & a )
{
	assert( a.size_bounded() );
	FArray5D< X > r( a );
	for ( typename FArray5< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray6D< X >
sign( X const & x, FArray6< T > const & a )
{
	assert( a.size_bounded() );
	FArray6D< X > r( a );
	for ( typename FArray6< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

// minval /////

template< typename T >
inline
T
minval( FArray< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	T r( std::numeric_limits< T >::max() );
	for ( size_type i = 0; i < a.size(); ++i ) {
		r = std::min( r, a[ i ] );
	}
	return r;
}

// maxval /////

template< typename T >
inline
T
maxval( FArray< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	T r( std::numeric_limits< T >::lowest() );
	for ( size_type i = 0; i < a.size(); ++i ) {
		r = std::max( r, a[ i ] );
	}
	return r;
}

// minloc /////

template< typename T >
inline
FArray1D< int >
minloc( FArray1< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
		if ( a[ l ] < r ) {
			r = a[ l ];
			loc = { i1 };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
minloc( FArray2< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	FArray1D< int > loc( 2, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i2 = 1; i2 <= as2; ++i2 ) {
		for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
minloc( FArray3< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	FArray1D< int > loc( 3, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i3 = 1; i3 <= as3; ++i3 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
minloc( FArray4< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	FArray1D< int > loc( 4, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i4 = 1; i4 <= as4; ++i4 ) {
		for ( int i3 = 1; i3 <= as3; ++i3 ) {
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
minloc( FArray5< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	FArray1D< int > loc( 5, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i5 = 1; i5 <= as5; ++i5 ) {
		for ( int i4 = 1; i4 <= as4; ++i4 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i2 = 1; i2 <= as2; ++i2 ) {
					for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
minloc( FArray6< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	FArray1D< int > loc( 6, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i6 = 1; i6 <= as6; ++i6 ) {
		for ( int i5 = 1; i5 <= as5; ++i5 ) {
			for ( int i4 = 1; i4 <= as4; ++i4 ) {
				for ( int i3 = 1; i3 <= as3; ++i3 ) {
					for ( int i2 = 1; i2 <= as2; ++i2 ) {
						for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
minloc( FArray1< T > const & a, int const dim )
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
FArray1D< int >
minloc( FArray2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	size_type l( 0 ); // Linear index
	switch ( dim ) {
	case 1:
		{
			FArray1D< int > loc( as2, as2 > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				T r( std::numeric_limits< T >::max() );
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
					if ( a[ l ] < r ) {
						r = a[ l ];
						loc( i2 ) = i1;
					}
				}
			}
			return loc;
		}
	case 2:
		{
			FArray1D< int > loc( as1, as1 > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
			for ( int i1 = 1; i1 <= as1; ++i1 ) {
				T r( std::numeric_limits< T >::max() );
				l = i1 - 1;
				for ( int i2 = 1; i2 <= as2; ++i2, l += as1 ) {
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
		return FArray1D< int >();
	}
}

template< typename T >
inline
FArray1D< int >
minloc( FArray1< T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
		if ( mask[ l ] && ( a[ l ] < r ) ) {
			r = a[ l ];
			loc = { i1 };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
minloc( FArray2< T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	FArray1D< int > loc( 2, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i2 = 1; i2 <= as2; ++i2 ) {
		for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
			if ( mask[ l ] && ( a[ l ] < r ) ) {
				r = a[ l ];
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
minloc( FArray3< T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	FArray1D< int > loc( 3, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i3 = 1; i3 <= as3; ++i3 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
				if ( mask[ l ] && ( a[ l ] < r ) ) {
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
FArray1D< int >
minloc( FArray4< T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	FArray1D< int > loc( 4, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i4 = 1; i4 <= as4; ++i4 ) {
		for ( int i3 = 1; i3 <= as3; ++i3 ) {
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
					if ( mask[ l ] && ( a[ l ] < r ) ) {
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
FArray1D< int >
minloc( FArray5< T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	FArray1D< int > loc( 5, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i5 = 1; i5 <= as5; ++i5 ) {
		for ( int i4 = 1; i4 <= as4; ++i4 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i2 = 1; i2 <= as2; ++i2 ) {
					for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
						if ( mask[ l ] && ( a[ l ] < r ) ) {
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
FArray1D< int >
minloc( FArray6< T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	FArray1D< int > loc( 6, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0 ); // Linear index
	for ( int i6 = 1; i6 <= as6; ++i6 ) {
		for ( int i5 = 1; i5 <= as5; ++i5 ) {
			for ( int i4 = 1; i4 <= as4; ++i4 ) {
				for ( int i3 = 1; i3 <= as3; ++i3 ) {
					for ( int i2 = 1; i2 <= as2; ++i2 ) {
						for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
							if ( mask[ l ] && ( a[ l ] < r ) ) {
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

// maxloc /////

template< typename T >
inline
FArray1D< int >
maxloc( FArray1< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
		if ( a[ l ] > r ) {
			r = a[ l ];
			loc = { i1 };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray2< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	FArray1D< int > loc( 2, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i2 = 1; i2 <= as2; ++i2 ) {
		for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
maxloc( FArray3< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	FArray1D< int > loc( 3, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i3 = 1; i3 <= as3; ++i3 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
maxloc( FArray4< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	FArray1D< int > loc( 4, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i4 = 1; i4 <= as4; ++i4 ) {
		for ( int i3 = 1; i3 <= as3; ++i3 ) {
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
maxloc( FArray5< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	FArray1D< int > loc( 5, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i5 = 1; i5 <= as5; ++i5 ) {
		for ( int i4 = 1; i4 <= as4; ++i4 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i2 = 1; i2 <= as2; ++i2 ) {
					for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
FArray1D< int >
maxloc( FArray6< T > const & a )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	FArray1D< int > loc( 6, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i6 = 1; i6 <= as6; ++i6 ) {
		for ( int i5 = 1; i5 <= as5; ++i5 ) {
			for ( int i4 = 1; i4 <= as4; ++i4 ) {
				for ( int i3 = 1; i3 <= as3; ++i3 ) {
					for ( int i2 = 1; i2 <= as2; ++i2 ) {
						for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
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
maxloc( FArray1< T > const & a, int const dim )
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
FArray1D< int >
maxloc( FArray2< T > const & a, int const dim )
{
	assert( a.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	size_type l( 0 ); // Linear index
	switch ( dim ) {
	case 1:
		{
			FArray1D< int > loc( as2, as2 > 0 ? 1 : 0 );
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				T r( std::numeric_limits< T >::lowest() );
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
					if ( a[ l ] > r ) {
						r = a[ l ];
						loc( i2 ) = i1;
					}
				}
			}
			return loc;
		}
	case 2:
		{
			FArray1D< int > loc( as1, as1 > 0 ? 1 : 0 );
			for ( int i1 = 1; i1 <= as1; ++i1 ) {
				T r( std::numeric_limits< T >::lowest() );
				l = i1 - 1;
				for ( int i2 = 1; i2 <= as2; ++i2, l += as1 ) {
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
		return FArray1D< int >();
	}
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray1< T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
		if ( mask[ l ] && ( a[ l ] > r ) ) {
			r = a[ l ];
			loc = { i1 };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray2< T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	FArray1D< int > loc( 2, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i2 = 1; i2 <= as2; ++i2 ) {
		for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
			if ( mask[ l ] && ( a[ l ] > r ) ) {
				r = a[ l ];
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray3< T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	FArray1D< int > loc( 3, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i3 = 1; i3 <= as3; ++i3 ) {
		for ( int i2 = 1; i2 <= as2; ++i2 ) {
			for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
				if ( mask[ l ] && ( a[ l ] > r ) ) {
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
FArray1D< int >
maxloc( FArray4< T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	FArray1D< int > loc( 4, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i4 = 1; i4 <= as4; ++i4 ) {
		for ( int i3 = 1; i3 <= as3; ++i3 ) {
			for ( int i2 = 1; i2 <= as2; ++i2 ) {
				for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
					if ( mask[ l ] && ( a[ l ] > r ) ) {
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
FArray1D< int >
maxloc( FArray5< T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	FArray1D< int > loc( 5, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i5 = 1; i5 <= as5; ++i5 ) {
		for ( int i4 = 1; i4 <= as4; ++i4 ) {
			for ( int i3 = 1; i3 <= as3; ++i3 ) {
				for ( int i2 = 1; i2 <= as2; ++i2 ) {
					for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
						if ( mask[ l ] && ( a[ l ] > r ) ) {
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
FArray1D< int >
maxloc( FArray6< T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  typename FArray< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	int const as3( a.isize3() );
	int const as4( a.isize4() );
	int const as5( a.isize5() );
	int const as6( a.isize6() );
	FArray1D< int > loc( 6, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0 ); // Linear index
	for ( int i6 = 1; i6 <= as6; ++i6 ) {
		for ( int i5 = 1; i5 <= as5; ++i5 ) {
			for ( int i4 = 1; i4 <= as4; ++i4 ) {
				for ( int i3 = 1; i3 <= as3; ++i3 ) {
					for ( int i2 = 1; i2 <= as2; ++i2 ) {
						for ( int i1 = 1; i1 <= as1; ++i1, ++l ) {
							if ( mask[ l ] && ( a[ l ] > r ) ) {
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

// matmul /////

// Matrix (Outer) Product of 1D Arrays
template< typename T >
inline
FArray2D< T >
matmul( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs( b.size() );
	FArray2D< T > m( static_cast< int >( as ), static_cast< int >( bs ) );
	for ( size_type lb = 0, l = 0; lb < bs; ++lb ) {
		T const b_j( b[ lb ] );
		for ( size_type la = 0; la < as; ++la, ++l ) {
			m[ l ] = a[ la ] * b_j;
		}
	}
	return m;
}

// Matrix (Outer) Product of 1D Boolean Arrays
inline
FArray2D< bool >
matmul( FArray1< bool > const & a, FArray1< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  FArray< bool >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs( b.size() );
	FArray2D< bool > m( static_cast< int >( as ), static_cast< int >( bs ) );
	for ( size_type lb = 0, l = 0; lb < bs; ++lb ) {
		bool const b_j( b[ lb ] );
		for ( size_type la = 0; la < as; ++la, ++l ) {
			m[ l ] = a[ la ] && b_j;
		}
	}
	return m;
}

// Matrix Product of 1D and 2D Arrays
template< typename T >
inline
FArray1D< T >
matmul( FArray1< T > const & a, FArray2< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs2( b.size2() );
	assert( as == b.size1() );
	FArray1D< T > m( static_cast< int >( bs2 ) );
	for ( size_type l = 0, lb = 0; l < bs2; ++l ) {
		T d( 0 );
		for ( size_type la = 0; la < as; ++la, ++lb ) {
			d += a[ la ] * b[ lb ];
		}
		m[ l ] = d;
	}
	return m;
}

// Matrix Product of 1D and 2D Boolean Arrays
inline
FArray1D< bool >
matmul( FArray1< bool > const & a, FArray2< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  FArray< bool >::size_type  size_type;
	size_type const as( a.size() );
	size_type const bs2( b.size2() );
	assert( as == b.size1() );
	FArray1D< bool > m( static_cast< int >( bs2 ) );
	for ( size_type l = 0, lb = 0; l < bs2; ++l ) {
		bool d( false );
		for ( size_type la = 0; la < as; ++la, ++lb ) {
			if ( a[ la ] && b[ lb ] ) {
				d = true;
				break;
			}
		}
		m[ l ] = d;
	}
	return m;
}

// Matrix Product of 2D and 1D Arrays
template< typename T >
inline
FArray1D< T >
matmul( FArray2< T > const & a, FArray1< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const bs( b.size() );
	assert( a.size2() == bs );
	FArray1D< T > m( static_cast< int >( as1 ) );
	for ( size_type l = 0; l < as1; ++l ) {
		T d( 0 );
		for ( size_type la = l, lb = 0; lb < bs; la += as1, ++lb ) {
			d += a[ la ] * b[ lb ];
		}
		m[ l ] = d;
	}
	return m;
}

// Matrix Product of 2D and 1D Boolean Arrays
inline
FArray1D< bool >
matmul( FArray2< bool > const & a, FArray1< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  FArray< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const bs( b.size() );
	assert( a.size2() == bs );
	FArray1D< bool > m( static_cast< int >( as1 ) );
	for ( size_type l = 0; l < as1; ++l ) {
		bool d( false );
		for ( size_type la = l, lb = 0; lb < bs; la += as1, ++lb ) {
			if ( a[ la ] && b[ lb ] ) {
				d = true;
				break;
			}
		}
		m[ l ] = d;
	}
	return m;
}

// Matrix Product of 2D Arrays
template< typename T >
inline
FArray2D< T >
matmul( FArray2< T > const & a, FArray2< T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  typename FArray< T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	size_type const as( as1 * as2 );
	size_type const bs2( b.size2() );
	assert( as2 == b.size1() );
	FArray2D< T > m( static_cast< int >( as1 ), static_cast< int >( bs2 ) );
	for ( size_type i = 0; i < as1; ++i ) {
		for ( size_type j = 0, lb = 0, l = i; j < bs2; ++j, l += as1 ) {
			T d( 0 );
			for ( size_type la = i; la < as; la += as1, ++lb ) {
				d += a[ la ] * b[ lb ];
			}
			m[ l ] = d;
		}
	}
	return m;
}

// Matrix Product of 2D Boolean Arrays
inline
FArray2D< bool >
matmul( FArray2< bool > const & a, FArray2< bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
	typedef  FArray< bool >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	size_type const as( as1 * as2 );
	size_type const bs2( b.size2() );
	assert( as2 == b.size1() );
	FArray2D< bool > m( static_cast< int >( as1 ), static_cast< int >( bs2 ) );
	for ( size_type i = 0; i < as1; ++i ) {
		for ( size_type j = 0, lb = 0, l = i; j < bs2; ++j, l += as1 ) {
			bool d( false );
			for ( size_type la = i; la < as; la += as1, ++lb ) {
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

} // ObjexxFCL

#endif // ObjexxFCL_FArray_functions_hh_INCLUDED
