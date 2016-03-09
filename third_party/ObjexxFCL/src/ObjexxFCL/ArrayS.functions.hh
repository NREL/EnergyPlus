#ifndef ObjexxFCL_ArrayS_functions_hh_INCLUDED
#define ObjexxFCL_ArrayS_functions_hh_INCLUDED

// ArrayS Functions
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
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3S.hh>
#include <ObjexxFCL/Array4S.hh>
#include <ObjexxFCL/Array5S.hh>
#include <ObjexxFCL/Array6S.hh>
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

inline
bool
all( Array5S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						if ( ! a( i1, i2, i3, i4, i5 ) ) return false;
					}
				}
			}
		}
	}
	return true;
}

inline
bool
all( Array6S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							if ( ! a( i1, i2, i3, i4, i5, i6 ) ) return false;
						}
					}
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

inline
bool
any( Array5S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						if ( a( i1, i2, i3, i4, i5 ) ) return true;
					}
				}
			}
		}
	}
	return false;
}

inline
bool
any( Array6S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							if ( a( i1, i2, i3, i4, i5, i6 ) ) return true;
						}
					}
				}
			}
		}
	}
	return false;
}

// negation /////

inline
Array1D< bool >
operator !( Array1S< bool > const & a )
{
	Array1D< bool > r( a );
	for ( Array1< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array2D< bool >
operator !( Array2S< bool > const & a )
{
	Array2D< bool > r( a );
	for ( Array2< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array3D< bool >
operator !( Array3S< bool > const & a )
{
	Array3D< bool > r( a );
	for ( Array3< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array4D< bool >
operator !( Array4S< bool > const & a )
{
	Array4D< bool > r( a );
	for ( Array4< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array5D< bool >
operator !( Array5S< bool > const & a )
{
	Array5D< bool > r( a );
	for ( Array5< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
Array6D< bool >
operator !( Array6S< bool > const & a )
{
	Array6D< bool > r( a );
	for ( Array6< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

// count /////

inline
Array1S< bool >::size_type
count( Array1S< bool > const & a )
{
	Array1S< bool >::size_type c( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) ++c;
	}
	return c;
}

inline
Array2S< bool >::size_type
count( Array2S< bool > const & a )
{
	Array2S< bool >::size_type c( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			if ( a( i1, i2 ) ) ++c;
		}
	}
	return c;
}

inline
Array3S< bool >::size_type
count( Array3S< bool > const & a )
{
	Array3S< bool >::size_type c( 0u );
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
Array4S< bool >::size_type
count( Array4S< bool > const & a )
{
	Array4S< bool >::size_type c( 0u );
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
Array5S< bool >::size_type
count( Array5S< bool > const & a )
{
	Array5S< bool >::size_type c( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						if ( a( i1, i2, i3, i4, i5 ) ) ++c;
					}
				}
			}
		}
	}
	return c;
}

inline
Array6S< bool >::size_type
count( Array6S< bool > const & a )
{
	Array6S< bool >::size_type c( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							if ( a( i1, i2, i3, i4, i5, i6 ) ) ++c;
						}
					}
				}
			}
		}
	}
	return c;
}

inline
Array1S< bool >::size_type
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
Array1D< Array2S< bool >::size_type >
count( Array2S< bool > const & a, int const dim )
{
	typedef  Array2S< bool >::size_type  size_type;
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
Array1D< int >
lbound( Array5S< T > const & )
{
	return Array1D< int >( 5, 1 );
}

template< typename T >
inline
Array1D< int >
lbound( Array6S< T > const & )
{
	return Array1D< int >( 6, 1 );
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

template< typename T >
inline
int
lbound( Array5S< T > const &, int const dim )
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
	case 5:
		return 1;
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
lbound( Array6S< T > const &, int const dim )
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
	case 5:
		return 1;
	case 6:
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
Array1D< int >
ubound( Array5S< T > const & a )
{
	return Array1D< int >( 5, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5() } );
}

template< typename T >
inline
Array1D< int >
ubound( Array6S< T > const & a )
{
	return Array1D< int >( 6, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5(), a.u6() } );
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

template< typename T >
inline
int
ubound( Array5S< T > const & a, int const dim )
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
	default:
		assert( false );
		return 0;
	}
}

template< typename T >
inline
int
ubound( Array6S< T > const & a, int const dim )
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

template< typename T >
inline
Array1D< int >
shape( Array5S< T > const & a )
{
	return Array1D< int >( 5, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() } );
}

template< typename T >
inline
Array1D< int >
shape( Array6S< T > const & a )
{
	return Array1D< int >( 6, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() } );
}

// size /////

template< typename T >
inline
typename ArrayS< T >::size_type
size( ArrayS< T > const & a )
{
	return a.size();
}

template< typename T >
inline
typename ArrayS< T >::size_type
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
typename ArrayS< T >::size_type
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
typename ArrayS< T >::size_type
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
typename ArrayS< T >::size_type
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
typename ArrayS< T >::size_type
size( Array5S< T > const & a, int const dim )
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
typename ArrayS< T >::size_type
size( Array6S< T > const & a, int const dim )
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
	typedef  typename Array1S< T >::size_type  size_type;
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
	typedef  typename Array1S< T >::size_type  size_type;
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
	typedef  typename Array1S< T >::size_type  size_type;
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
	typedef  typename Array1S< T >::size_type  size_type;
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
	typedef  typename Array1S< T >::size_type  size_type;
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
Array6D< T >
reshape( Array1S< T > const & a, std::array< I, 6 > const & shape )
{
	typedef  typename Array1S< T >::size_type  size_type;
	Array6D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ] );
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
	typedef  typename Array2S< T >::size_type  size_type;
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
	typedef  typename Array2S< T >::size_type  size_type;
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
	typedef  Array1< bool >::size_type  size_type;
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
	typedef  Array2< bool >::size_type  size_type;
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
	typedef  Array3< bool >::size_type  size_type;
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
	typedef  Array4< bool >::size_type  size_type;
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

template< typename T >
inline
Array1D< T >
pack( Array5S< T > const & a, Array5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array5< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	size_type l( 0u ), k( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						if ( mask[ l ] ) r[ k++ ] = a( i1, i2, i3, i4, i5, ++l );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
Array1D< T >
pack( Array6S< T > const & a, Array6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array6< bool >::size_type  size_type;
	size_type n( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	Array1D< T > r( static_cast< int >( n ) );
	size_type l( 0u ), k( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							if ( mask[ l ] ) r[ k++ ] = a( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
sum( Array5S< T > const & a )
{
	T r( 0 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						r += a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
sum( Array6S< T > const & a )
{
	T r( 0 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							r += a( i1, i2, i3, i4, i5, i6 );
						}
					}
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
sum( Array5S< T > const & a, Array5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	T r( 0 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
						if ( mask[ l ] ) r += a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
sum( Array6S< T > const & a, Array6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	T r( 0 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							if ( mask[ l ] ) r += a( i1, i2, i3, i4, i5, i6 );
						}
					}
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
product( Array5S< T > const & a )
{
	T r( 1 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						r *= a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
product( Array6S< T > const & a )
{
	T r( 1 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							r *= a( i1, i2, i3, i4, i5, i6 );
						}
					}
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
product( Array5S< T > const & a, Array5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	T r( 1 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
						if ( mask[ l ] ) r *= a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
product( Array6S< T > const & a, Array6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	T r( 1 );
	size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							if ( mask[ l ] ) r *= a( i1, i2, i3, i4, i5, i6 );
						}
					}
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

// abs /////

template< typename T >
inline
Array1D< T >
abs( Array1S< T > const & a )
{
	Array1D< T > r( a );
	for ( typename Array1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array5D< T >
abs( Array5S< T > const & a )
{
	Array5D< T > r( a );
	for ( typename Array5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
Array6D< T >
abs( Array6S< T > const & a )
{
	Array6D< T > r( a );
	for ( typename Array6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
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
	for ( typename Array1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array2D< T >
pow( Array2S< T > const & a, X const & x )
{
	Array2D< T > r( a );
	for ( typename Array2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array3D< T >
pow( Array3S< T > const & a, X const & x )
{
	Array3D< T > r( a );
	for ( typename Array3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array4D< T >
pow( Array4S< T > const & a, X const & x )
{
	Array4D< T > r( a );
	for ( typename Array4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array5D< T >
pow( Array5S< T > const & a, X const & x )
{
	Array5D< T > r( a );
	for ( typename Array5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array6D< T >
pow( Array6S< T > const & a, X const & x )
{
	Array6D< T > r( a );
	for ( typename Array6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
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
	for ( typename Array1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array5D< T >
sign( Array5S< T > const & a, X const & x )
{
	Array5D< T > r( a );
	for ( typename Array5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
Array6D< T >
sign( Array6S< T > const & a, X const & x )
{
	Array6D< T > r( a );
	for ( typename Array6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
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
	for ( typename Array4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array5D< X >
sign( X const & x, Array5S< T > const & a )
{
	Array5D< X > r( a );
	for ( typename Array5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
Array6D< X >
sign( X const & x, Array6S< T > const & a )
{
	Array6D< X > r( a );
	for ( typename Array6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
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

template< typename T >
inline
T
minval( Array5S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						r = std::min( r, a( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
minval( Array6S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							r = std::min( r, a( i1, i2, i3, i4, i5, i6 ) );
						}
					}
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

template< typename T >
inline
T
maxval( Array5S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						r = std::max( r, a( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
T
maxval( Array6S< T > const & a )
{
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							r = std::max( r, a( i1, i2, i3, i4, i5, i6 ) );
						}
					}
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
Array1D< int >
minloc( Array5S< T > const & a )
{
	Array1D< int > loc( 5, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						if ( a( i1, i2, i3, i4, i5 ) < r ) {
							r = a( i1, i2, i3, i4, i5 );
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
minloc( Array6S< T > const & a )
{
	Array1D< int > loc( 6, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							if ( a( i1, i2, i3, i4, i5, i6 ) < r ) {
								r = a( i1, i2, i3, i4, i5, i6 );
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
			T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1 ) );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
			T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1 ) );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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

template< typename T >
inline
Array1D< int >
minloc( Array5S< T > const & a, Array5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	Array1D< int > loc( 5, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
						if ( mask[ l ] ) {
							if ( first ) {
								first = false;
								r = a( i1, i2, i3, i4, i5 );
								loc = { i1, i2, i3, i4, i5 };
							} else if ( a( i1, i2, i3, i4, i5 ) < r ) {
								r = a( i1, i2, i3, i4, i5 );
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
minloc( Array6S< T > const & a, Array6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	Array1D< int > loc( 6, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							if ( mask[ l ] ) {
								if ( first ) {
									first = false;
									r = a( i1, i2, i3, i4, i5, i6 );
									loc = { i1, i2, i3, i4, i5, i6 };
								} else if ( a( i1, i2, i3, i4, i5, i6 ) < r ) {
									r = a( i1, i2, i3, i4, i5, i6 );
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
Array1D< int >
maxloc( Array5S< T > const & a )
{
	Array1D< int > loc( 5, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						if ( a( i1, i2, i3, i4, i5 ) > r ) {
							r = a( i1, i2, i3, i4, i5 );
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
maxloc( Array6S< T > const & a )
{
	Array1D< int > loc( 6, a.empty() ? 0 : 1 ); // F2008 standard => 0 for empty arrays
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1, 1, 1, 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
							if ( a( i1, i2, i3, i4, i5, i6 ) > r ) {
								r = a( i1, i2, i3, i4, i5, i6 );
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
			T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1 ) );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
			T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1 ) );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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
	typedef  Array< bool >::size_type  size_type;
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

template< typename T >
inline
Array1D< int >
maxloc( Array5S< T > const & a, Array5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	Array1D< int > loc( 5, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
						if ( mask[ l ] ) {
							if ( first ) {
								first = false;
								r = a( i1, i2, i3, i4, i5 );
								loc = { i1, i2, i3, i4, i5 };
							} else if ( a( i1, i2, i3, i4, i5 ) > r ) {
								r = a( i1, i2, i3, i4, i5 );
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
maxloc( Array6S< T > const & a, Array6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  Array< bool >::size_type  size_type;
	Array1D< int > loc( 6, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	size_type l( 0u );
	bool first( true );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							if ( mask[ l ] ) {
								if ( first ) {
									first = false;
									r = a( i1, i2, i3, i4, i5, i6 );
									loc = { i1, i2, i3, i4, i5, i6 };
								} else if ( a( i1, i2, i3, i4, i5, i6 ) > r ) {
									r = a( i1, i2, i3, i4, i5, i6 );
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

// Subscript Array Generators

// Subscripted Array
template< typename T >
inline
Array1D< T >
array_sub( Array1S< T > const & a, Array1S< int > const & sub )
{
	Array1D< T > r( sub.isize() );
	for ( int i = 1, e = sub.u(); i <= e; ++i ) {
		r( i ) = a( sub( i ) );
	}
	return r;
}

// Subscripted Array
template< typename T >
inline
Array1D< T >
array_sub( Array1S< T > const & a, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	Array1D< T > r( sub.isize() );
	for ( int i = sub.l(), e = sub.u(), k = 1; i <= e; ++i, ++k ) {
		r( k ) = a( sub( i ) );
	}
	return r;
}

// Subscripted Array
template< typename T >
inline
Array1D< T >
array_sub( Array1< T > const & a, Array1S< int > const & sub )
{
	Array1D< T > r( sub.isize() );
	for ( int i = 1, e = sub.u(); i <= e; ++i ) {
		r( i ) = a( sub( i ) );
	}
	return r;
}

} // ObjexxFCL

#endif // ObjexxFCL_ArrayS_functions_hh_INCLUDED
