#ifndef ObjexxFCL_FArrayS_functions_hh_INCLUDED
#define ObjexxFCL_FArrayS_functions_hh_INCLUDED

// FArrayS Functions
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
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/FArray2S.hh>
#include <ObjexxFCL/FArray3S.hh>
#include <ObjexxFCL/FArray4S.hh>
#include <ObjexxFCL/FArray5S.hh>
#include <ObjexxFCL/FArray6S.hh>
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
allocated( FArrayS< T > const & a )
{
	return a.allocated();
}

// all /////

inline
bool
all( FArray1S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( ! a( i ) ) return false;
	}
	return true;
}

inline
bool
all( FArray2S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			if ( ! a( i1, i2 ) ) return false;
		}
	}
	return true;
}

inline
bool
all( FArray3S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! a( i1, i2, i3 ) ) return false;
			}
		}
	}
	return true;
}

inline
bool
all( FArray4S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					if ( ! a( i1, i2, i3, i4 ) ) return false;
				}
			}
		}
	}
	return true;
}

inline
bool
all( FArray5S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
all( FArray6S< bool > const & a )
{
	if ( a.empty() ) return true;
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
any( FArray1S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) return true;
	}
	return false;
}

inline
bool
any( FArray2S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			if ( a( i1, i2 ) ) return true;
		}
	}
	return false;
}

inline
bool
any( FArray3S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2, i3 ) ) return true;
			}
		}
	}
	return false;
}

inline
bool
any( FArray4S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					if ( a( i1, i2, i3, i4 ) ) return true;
				}
			}
		}
	}
	return false;
}

inline
bool
any( FArray5S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
any( FArray6S< bool > const & a )
{
	if ( a.empty() ) return false;
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< bool >
operator !( FArray1S< bool > const & a )
{
	FArray1D< bool > r( a );
	for ( FArray1< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray2D< bool >
operator !( FArray2S< bool > const & a )
{
	FArray2D< bool > r( a );
	for ( FArray2< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray3D< bool >
operator !( FArray3S< bool > const & a )
{
	FArray3D< bool > r( a );
	for ( FArray3< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray4D< bool >
operator !( FArray4S< bool > const & a )
{
	FArray4D< bool > r( a );
	for ( FArray4< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray5D< bool >
operator !( FArray5S< bool > const & a )
{
	FArray5D< bool > r( a );
	for ( FArray5< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

inline
FArray6D< bool >
operator !( FArray6S< bool > const & a )
{
	FArray6D< bool > r( a );
	for ( FArray6< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

// count /////

inline
FArray1S< bool >::size_type
count( FArray1S< bool > const & a )
{
	FArray1S< bool >::size_type c( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) ++c;
	}
	return c;
}

inline
FArray2S< bool >::size_type
count( FArray2S< bool > const & a )
{
	FArray2S< bool >::size_type c( 0u );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			if ( a( i1, i2 ) ) ++c;
		}
	}
	return c;
}

inline
FArray3S< bool >::size_type
count( FArray3S< bool > const & a )
{
	FArray3S< bool >::size_type c( 0u );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2, i3 ) ) ++c;
			}
		}
	}
	return c;
}

inline
FArray4S< bool >::size_type
count( FArray4S< bool > const & a )
{
	FArray4S< bool >::size_type c( 0u );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					if ( a( i1, i2, i3, i4 ) ) ++c;
				}
			}
		}
	}
	return c;
}

inline
FArray5S< bool >::size_type
count( FArray5S< bool > const & a )
{
	FArray5S< bool >::size_type c( 0u );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
						if ( a( i1, i2, i3, i4, i5 ) ) ++c;
					}
				}
			}
		}
	}
	return c;
}

inline
FArray6S< bool >::size_type
count( FArray6S< bool > const & a )
{
	FArray6S< bool >::size_type c( 0u );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1S< bool >::size_type
count( FArray1S< bool > const & a, int const dim )
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
FArray1D< FArray2S< bool >::size_type >
count( FArray2S< bool > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
			FArray1D< FArray2S< bool >::size_type > v( a.isize2() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				FArray2S< bool >::size_type c( 0u );
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					if ( a( i1, i2 ) ) ++c;
				}
				v( i2 ) = c;
			}
			return v;
		}
	case 2:
		{
			FArray1D< FArray2S< bool >::size_type > v( a.isize1() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				FArray2S< bool >::size_type c( 0u );
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					if ( a( i1, i2 ) ) ++c;
				}
				v( i1 ) = c;
			}
			return v;
		}
	default:
		assert( false );
		return FArray1D< FArray2S< bool >::size_type >();
	}
}

// is_contiguous /////

template< typename T >
inline
bool
is_contiguous( FArrayS< T > const & )
{
	return false; //Do Replace by method call once we support it
}

// lbound /////

template< typename T >
inline
FArray1D< int >
lbound( FArray1S< T > const & )
{
	return FArray1D< int >( 1, 1 );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray2S< T > const & )
{
	return FArray1D< int >( 2, 1 );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray3S< T > const & )
{
	return FArray1D< int >( 3, 1 );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray4S< T > const & )
{
	return FArray1D< int >( 4, 1 );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray5S< T > const & )
{
	return FArray1D< int >( 5, 1 );
}

template< typename T >
inline
FArray1D< int >
lbound( FArray6S< T > const & )
{
	return FArray1D< int >( 6, 1 );
}

template< typename T >
inline
int
lbound( FArray1S< T > const &, int const dim )
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
lbound( FArray2S< T > const &, int const dim )
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
lbound( FArray3S< T > const &, int const dim )
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
lbound( FArray4S< T > const &, int const dim )
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
lbound( FArray5S< T > const &, int const dim )
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
lbound( FArray6S< T > const &, int const dim )
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
FArray1D< int >
ubound( FArray1S< T > const & a )
{
	return FArray1D< int >( 1, a.u1() );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray2S< T > const & a )
{
	return FArray1D< int >( 2, { a.u1(), a.u2() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray3S< T > const & a )
{
	return FArray1D< int >( 3, { a.u1(), a.u2(), a.u3() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray4S< T > const & a )
{
	return FArray1D< int >( 4, { a.u1(), a.u2(), a.u3(), a.u4() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray5S< T > const & a )
{
	return FArray1D< int >( 5, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5() } );
}

template< typename T >
inline
FArray1D< int >
ubound( FArray6S< T > const & a )
{
	return FArray1D< int >( 6, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5(), a.u6() } );
}

template< typename T >
inline
int
ubound( FArray1S< T > const & a, int const dim )
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
ubound( FArray2S< T > const & a, int const dim )
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
ubound( FArray3S< T > const & a, int const dim )
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
ubound( FArray4S< T > const & a, int const dim )
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
ubound( FArray5S< T > const & a, int const dim )
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
ubound( FArray6S< T > const & a, int const dim )
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
FArray1D< int >
shape( FArray1S< T > const & a )
{
	return FArray1D< int >( 1, a.isize1() );
}

template< typename T >
inline
FArray1D< int >
shape( FArray2S< T > const & a )
{
	return FArray1D< int >( 2, { a.isize1(), a.isize2() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray3S< T > const & a )
{
	return FArray1D< int >( 3, { a.isize1(), a.isize2(), a.isize3() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray4S< T > const & a )
{
	return FArray1D< int >( 4, { a.isize1(), a.isize2(), a.isize3(), a.isize4() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray5S< T > const & a )
{
	return FArray1D< int >( 5, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() } );
}

template< typename T >
inline
FArray1D< int >
shape( FArray6S< T > const & a )
{
	return FArray1D< int >( 6, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() } );
}

// size /////

template< typename T >
inline
typename FArrayS< T >::size_type
size( FArrayS< T > const & a )
{
	return a.size();
}

template< typename T >
inline
typename FArrayS< T >::size_type
size( FArray1S< T > const & a, int const dim )
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
typename FArrayS< T >::size_type
size( FArray2S< T > const & a, int const dim )
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
typename FArrayS< T >::size_type
size( FArray3S< T > const & a, int const dim )
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
typename FArrayS< T >::size_type
size( FArray4S< T > const & a, int const dim )
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
typename FArrayS< T >::size_type
size( FArray5S< T > const & a, int const dim )
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
typename FArrayS< T >::size_type
size( FArray6S< T > const & a, int const dim )
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
isize( FArrayS< T > const & a )
{
	return static_cast< int >( size( a ) );
}

template< template< typename > class A, typename T, class = typename std::enable_if< std::is_base_of< FArrayS< T >, A< T > >::value >::type >
inline
int
isize( A< T > const & a, int const dim )
{
	return static_cast< int >( size( a, dim ) );
}

// contig /////

template< typename T >
inline
FArray1D< T >
contig( FArray1S< T > const & a )
{
	return FArray1D< T >( a );
}

// reshape /////

template< typename T >
inline
FArray1D< T >
reshape( FArray1S< T > const & a )
{
	return FArray1D< T >( a );
}

template< typename T, typename I >
inline
FArray1D< T >
reshape( FArray1S< T > const & a, std::array< I, 1 > const & shape )
{
	typedef  typename FArray1S< T >::size_type  size_type;
	FArray1D< T > r( shape[ 0 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
FArray2D< T >
reshape( FArray1S< T > const & a, std::array< I, 2 > const & shape )
{
	typedef  typename FArray1S< T >::size_type  size_type;
	FArray2D< T > r( shape[ 0 ], shape[ 1 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
FArray3D< T >
reshape( FArray1S< T > const & a, std::array< I, 3 > const & shape )
{
	typedef  typename FArray1S< T >::size_type  size_type;
	FArray3D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
FArray4D< T >
reshape( FArray1S< T > const & a, std::array< I, 4 > const & shape )
{
	typedef  typename FArray1S< T >::size_type  size_type;
	FArray4D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
FArray5D< T >
reshape( FArray1S< T > const & a, std::array< I, 5 > const & shape )
{
	typedef  typename FArray1S< T >::size_type  size_type;
	FArray5D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
FArray6D< T >
reshape( FArray1S< T > const & a, std::array< I, 6 > const & shape )
{
	typedef  typename FArray1S< T >::size_type  size_type;
	FArray6D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< typename T, typename I >
inline
FArray1D< T >
reshape( FArray2S< T > const & a, std::array< I, 1 > const & shape )
{
	typedef  typename FArray2S< T >::size_type  size_type;
	FArray1D< T > r( shape[ 0 ] );
	size_type l( 0 );
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
FArray2D< T >
reshape( FArray2S< T > const & a, std::array< I, 2 > const & shape )
{
	typedef  typename FArray2S< T >::size_type  size_type;
	FArray2D< T > r( shape[ 0 ], shape[ 1 ] );
	size_type l( 0 );
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
FArray1D< T >
pack( FArray1S< T > const & a, FArray1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  FArray1< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	size_type l( 0 ), k( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) r[ k++ ] = a( i );
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray2S< T > const & a, FArray2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  FArray2< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	size_type l( 0 ), k( 0 );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
			if ( mask[ l ] ) r[ k++ ] = a( i1, i2 );
		}
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray3S< T > const & a, FArray3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  FArray3< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	size_type l( 0 ), k( 0 );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				if ( mask[ l ] ) r[ k++ ] = a( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray4S< T > const & a, FArray4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  FArray4< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	size_type l( 0 ), k( 0 );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					if ( mask[ l ] ) r[ k++ ] = a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray5S< T > const & a, FArray5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  FArray5< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	size_type l( 0 ), k( 0 );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						if ( mask[ l ] ) r[ k++ ] = a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< typename T >
inline
FArray1D< T >
pack( FArray6S< T > const & a, FArray6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	typedef  FArray6< bool >::size_type  size_type;
	size_type n( 0 );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) ++n;
	}
	FArray1D< T > r( static_cast< int >( n ) );
	size_type l( 0 ), k( 0 );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
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
FArray1D< T >
cshift( FArray1S< T > const & a, int const shift, int const dim = 1 )
{
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
cshift( FArray2S< T > const & a, int const shift, int const dim = 1 )
{
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
cshift( FArray2S< T > const & a, FArray1< int > const & shift, int const dim = 1 )
{
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
eoshift( FArray1S< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
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
eoshift( FArray2S< T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
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
eoshift( FArray2S< T > const & a, FArray1< int > const & shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
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
sum( FArray1S< T > const & a )
{
	T s( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		s += a( i );
	}
	return s;
}

template< typename T >
inline
T
sum( FArray2S< T > const & a )
{
	T s( 0 );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			s += a( i1, i2 );
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray3S< T > const & a )
{
	T s( 0 );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				s += a( i1, i2, i3 );
			}
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray4S< T > const & a )
{
	T s( 0 );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					s += a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray5S< T > const & a )
{
	T s( 0 );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
						s += a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray6S< T > const & a )
{
	T s( 0 );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
							s += a( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray1S< T > const & a, int const dim )
{
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	T s( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		s += a( i );
	}
	return s;
}

template< typename T >
inline
FArray1D< T >
sum( FArray2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
			FArray1D< T > sums( a.size2(), 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				T s( 0 );
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					s += a( i1, i2 );
				}
				sums( i2 ) = s;
			}
			return sums;
		}
	case 2:
		{
			FArray1D< T > sums( a.size1(), 0 );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				T s( 0 );
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					s += a( i1, i2 );
				}
				sums( i1 ) = s;
			}
			return sums;
		}
	default:
		assert( false );
		return FArray1D< T >();
	}
}

template< typename T >
inline
T
sum( FArray1S< T > const & a, FArray1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i = 1, e = a.u(), k = mask.l(); i <= e; ++i, ++k ) {
		if ( mask( k ) ) s += a( i );
	}
	return s;
}

template< typename T >
inline
T
sum( FArray2S< T > const & a, FArray2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
		for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
			if ( mask( k1, k2 ) ) s += a( i1, i2 );
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray3S< T > const & a, FArray3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
		for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
			for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
				if ( mask( k1, k2, k3 ) ) s += a( i1, i2, i3 );
			}
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray4S< T > const & a, FArray4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
		for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
			for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
				for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
					if ( mask( k1, k2, k3, k4 ) ) s += a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray5S< T > const & a, FArray5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
		for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
			for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
				for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
					for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
						if ( mask( k1, k2, k3, k4, k5 ) ) s += a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return s;
}

template< typename T >
inline
T
sum( FArray6S< T > const & a, FArray6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i6 = 1, e6 = a.u6(), k6 = mask.l6(); i6 <= e6; ++i6, ++k6 ) {
		for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
			for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
				for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
					for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
						for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
							if ( mask( k1, k2, k3, k4, k5, k6 ) ) s += a( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
		}
	}
	return s;
}

// product /////

template< typename T >
inline
T
product( FArray1S< T > const & a )
{
	T p( 1 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		p *= a( i );
	}
	return p;
}

template< typename T >
inline
T
product( FArray2S< T > const & a )
{
	T p( 1 );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			p *= a( i1, i2 );
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray3S< T > const & a )
{
	T p( 1 );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				p *= a( i1, i2, i3 );
			}
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray4S< T > const & a )
{
	T p( 1 );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					p *= a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray5S< T > const & a )
{
	T p( 1 );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
						p *= a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray6S< T > const & a )
{
	T p( 1 );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
							p *= a( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray1S< T > const & a, int const dim )
{
	assert( dim == 1 );
#ifdef NDEBUG
	static_cast< void >( dim ); // Suppress unused warning
#endif
	T p( 1 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		p *= a( i );
	}
	return p;
}

template< typename T >
inline
FArray1D< T >
product( FArray2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
			FArray1D< T > prds( a.size2(), 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				T p( 1 );
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					p *= a( i1, i2 );
				}
				prds( i2 ) = p;
			}
			return prds;
		}
	case 2:
		{
			FArray1D< T > prds( a.size1(), 0 );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				T p( 1 );
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					p *= a( i1, i2 );
				}
				prds( i1 ) = p;
			}
			return prds;
		}
	default:
		assert( false );
		return FArray1D< T >();
	}
}

template< typename T >
inline
T
product( FArray1S< T > const & a, FArray1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i = 1, e = a.u(), k = mask.l(); i <= e; ++i, ++k ) {
		if ( mask( k ) ) p *= a( i );
	}
	return p;
}

template< typename T >
inline
T
product( FArray2S< T > const & a, FArray2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
		for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
			if ( mask( k1, k2 ) ) p *= a( i1, i2 );
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray3S< T > const & a, FArray3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
		for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
			for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
				if ( mask( k1, k2, k3 ) ) p *= a( i1, i2, i3 );
			}
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray4S< T > const & a, FArray4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
		for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
			for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
				for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
					if ( mask( k1, k2, k3, k4 ) ) p *= a( i1, i2, i3, i4 );
				}
			}
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray5S< T > const & a, FArray5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
		for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
			for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
				for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
					for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
						if ( mask( k1, k2, k3, k4, k5 ) ) p *= a( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return p;
}

template< typename T >
inline
T
product( FArray6S< T > const & a, FArray6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i6 = 1, e6 = a.u6(), k6 = mask.l6(); i6 <= e6; ++i6, ++k6 ) {
		for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
			for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
				for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
					for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
						for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
							if ( mask( k1, k2, k3, k4, k5, k6 ) ) p *= a( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
		}
	}
	return p;
}

// abs /////

template< typename T >
inline
FArray1D< T >
abs( FArray1S< T > const & a )
{
	FArray1D< T > r( a );
	for ( typename FArray1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray2D< T >
abs( FArray2S< T > const & a )
{
	FArray2D< T > r( a );
	for ( typename FArray2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray3D< T >
abs( FArray3S< T > const & a )
{
	FArray3D< T > r( a );
	for ( typename FArray3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray4D< T >
abs( FArray4S< T > const & a )
{
	FArray4D< T > r( a );
	for ( typename FArray4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray5D< T >
abs( FArray5S< T > const & a )
{
	FArray5D< T > r( a );
	for ( typename FArray5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< typename T >
inline
FArray6D< T >
abs( FArray6S< T > const & a )
{
	FArray6D< T > r( a );
	for ( typename FArray6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

// pow /////

template< typename T, typename X >
inline
FArray1D< T >
pow( FArray1S< T > const & a, X const & x )
{
	FArray1D< T > r( a );
	for ( typename FArray1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray2D< T >
pow( FArray2S< T > const & a, X const & x )
{
	FArray2D< T > r( a );
	for ( typename FArray2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray3D< T >
pow( FArray3S< T > const & a, X const & x )
{
	FArray3D< T > r( a );
	for ( typename FArray3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray4D< T >
pow( FArray4S< T > const & a, X const & x )
{
	FArray4D< T > r( a );
	for ( typename FArray4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray5D< T >
pow( FArray5S< T > const & a, X const & x )
{
	FArray5D< T > r( a );
	for ( typename FArray5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray6D< T >
pow( FArray6S< T > const & a, X const & x )
{
	FArray6D< T > r( a );
	for ( typename FArray6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

// sign /////

template< typename T, typename X >
inline
FArray1D< T >
sign( FArray1S< T > const & a, X const & x )
{
	FArray1D< T > r( a );
	for ( typename FArray1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray2D< T >
sign( FArray2S< T > const & a, X const & x )
{
	FArray2D< T > r( a );
	for ( typename FArray2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray3D< T >
sign( FArray3S< T > const & a, X const & x )
{
	FArray3D< T > r( a );
	for ( typename FArray3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray4D< T >
sign( FArray4S< T > const & a, X const & x )
{
	FArray4D< T > r( a );
	for ( typename FArray4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray5D< T >
sign( FArray5S< T > const & a, X const & x )
{
	FArray5D< T > r( a );
	for ( typename FArray5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename T, typename X >
inline
FArray6D< T >
sign( FArray6S< T > const & a, X const & x )
{
	FArray6D< T > r( a );
	for ( typename FArray6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename X, typename T >
inline
FArray1D< X >
sign( X const & x, FArray1S< T > const & a )
{
	FArray1D< X > r( a );
	for ( typename FArray1S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray2D< X >
sign( X const & x, FArray2S< T > const & a )
{
	FArray2D< X > r( a );
	for ( typename FArray2S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray3D< X >
sign( X const & x, FArray3S< T > const & a )
{
	FArray3D< X > r( a );
	for ( typename FArray3S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray4D< X >
sign( X const & x, FArray4S< T > const & a )
{
	FArray4D< X > r( a );
	for ( typename FArray4S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray5D< X >
sign( X const & x, FArray5S< T > const & a )
{
	FArray5D< X > r( a );
	for ( typename FArray5S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, typename T >
inline
FArray6D< X >
sign( X const & x, FArray6S< T > const & a )
{
	FArray6D< X > r( a );
	for ( typename FArray6S< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

// minval /////

template< typename T >
inline
T
minval( FArray1S< T > const & a )
{
	T r( std::numeric_limits< T >::max() );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r = std::min( r, a( i ) );
	}
	return r;
}

template< typename T >
inline
T
minval( FArray2S< T > const & a )
{
	T r( std::numeric_limits< T >::max() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			r = std::min( r, a( i1, i2 ) );
		}
	}
	return r;
}

template< typename T >
inline
T
minval( FArray3S< T > const & a )
{
	T r( std::numeric_limits< T >::max() );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				r = std::min( r, a( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
minval( FArray4S< T > const & a )
{
	T r( std::numeric_limits< T >::max() );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
minval( FArray5S< T > const & a )
{
	T r( std::numeric_limits< T >::max() );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
minval( FArray6S< T > const & a )
{
	T r( std::numeric_limits< T >::max() );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
maxval( FArray1S< T > const & a )
{
	T r( std::numeric_limits< T >::lowest() );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r = std::max( r, a( i ) );
	}
	return r;
}

template< typename T >
inline
T
maxval( FArray2S< T > const & a )
{
	T r( std::numeric_limits< T >::lowest() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			r = std::max( r, a( i1, i2 ) );
		}
	}
	return r;
}

template< typename T >
inline
T
maxval( FArray3S< T > const & a )
{
	T r( std::numeric_limits< T >::lowest() );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				r = std::max( r, a( i1, i2, i3 ) );
			}
		}
	}
	return r;
}

template< typename T >
inline
T
maxval( FArray4S< T > const & a )
{
	T r( std::numeric_limits< T >::lowest() );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
maxval( FArray5S< T > const & a )
{
	T r( std::numeric_limits< T >::lowest() );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
maxval( FArray6S< T > const & a )
{
	T r( std::numeric_limits< T >::lowest() );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
minloc( FArray1S< T > const & a )
{
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) < r ) {
			r = a( i );
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
minloc( FArray2S< T > const & a )
{
	FArray1D< int > loc( 2, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
minloc( FArray3S< T > const & a )
{
	FArray1D< int > loc( 3, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
minloc( FArray4S< T > const & a )
{
	FArray1D< int > loc( 4, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
minloc( FArray5S< T > const & a )
{
	FArray1D< int > loc( 5, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
minloc( FArray6S< T > const & a )
{
	FArray1D< int > loc( 6, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
minloc( FArray1S< T > const & a, int const dim )
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
FArray1D< int >
minloc( FArray2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
			FArray1D< int > loc( a.size2(), a.size2() > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				T r( std::numeric_limits< T >::max() );
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
			FArray1D< int > loc( a.size1(), a.size1() > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				T r( std::numeric_limits< T >::max() );
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
		return FArray1D< int >();
	}
}

template< typename T >
inline
FArray1D< int >
minloc( FArray1S< T > const & a, FArray1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i = 1, e = a.u(), k = mask.l(); i <= e; ++i, ++k ) {
		if ( mask( k ) && ( a( i ) < r ) ) {
			r = a( i );
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
minloc( FArray2S< T > const & a, FArray2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 2, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
		for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
			if ( mask( k1, k2 ) && ( a( i1, i2 ) < r ) ) {
				r = a( i1, i2 );
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
minloc( FArray3S< T > const & a, FArray3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 3, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
		for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
			for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
				if ( mask( k1, k2, k3 ) && ( a( i1, i2, i3 ) < r ) ) {
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
FArray1D< int >
minloc( FArray4S< T > const & a, FArray4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 4, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
		for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
			for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
				for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
					if ( mask( k1, k2, k3, k4 ) && ( a( i1, i2, i3, i4 ) < r ) ) {
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
FArray1D< int >
minloc( FArray5S< T > const & a, FArray5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 5, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
		for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
			for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
				for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
					for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
						if ( mask( k1, k2, k3, k4, k5 ) && ( a( i1, i2, i3, i4, i5 ) < r ) ) {
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
FArray1D< int >
minloc( FArray6S< T > const & a, FArray6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 6, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::max() );
	for ( int i6 = 1, e6 = a.u6(), k6 = mask.l6(); i6 <= e6; ++i6, ++k6 ) {
		for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
			for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
				for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
					for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
						for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
							if ( mask( k1, k2, k3, k4, k5, k6 ) && ( a( i1, i2, i3, i4, i5, i6 ) < r ) ) {
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

// maxloc /////

template< typename T >
inline
FArray1D< int >
maxloc( FArray1S< T > const & a )
{
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) > r ) {
			r = a( i );
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray2S< T > const & a )
{
	FArray1D< int > loc( 2, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
maxloc( FArray3S< T > const & a )
{
	FArray1D< int > loc( 3, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
maxloc( FArray4S< T > const & a )
{
	FArray1D< int > loc( 4, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
		for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
maxloc( FArray5S< T > const & a )
{
	FArray1D< int > loc( 5, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
FArray1D< int >
maxloc( FArray6S< T > const & a )
{
	FArray1D< int > loc( 6, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
		for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
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
maxloc( FArray1S< T > const & a, int const dim )
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
FArray1D< int >
maxloc( FArray2S< T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		{
			FArray1D< int > loc( a.size2(), a.size2() > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				T r( std::numeric_limits< T >::lowest() );
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
			FArray1D< int > loc( a.size1(), a.size1() > 0 ? 1 : 0 ); // F2008 standard => 0 for empty arrays
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				T r( std::numeric_limits< T >::lowest() );
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
		return FArray1D< int >();
	}
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray1S< T > const & a, FArray1< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 1, a.size() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i = 1, e = a.u(), k = mask.l(); i <= e; ++i, ++k ) {
		if ( mask( k ) && ( a( i ) > r ) ) {
			r = a( i );
			loc = { i };
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray2S< T > const & a, FArray2< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 2, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
		for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
			if ( mask( k1, k2 ) && ( a( i1, i2 ) > r ) ) {
				r = a( i1, i2 );
				loc = { i1, i2 };
			}
		}
	}
	return loc;
}

template< typename T >
inline
FArray1D< int >
maxloc( FArray3S< T > const & a, FArray3< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 3, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
		for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
			for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
				if ( mask( k1, k2, k3 ) && ( a( i1, i2, i3 ) > r ) ) {
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
FArray1D< int >
maxloc( FArray4S< T > const & a, FArray4< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 4, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
		for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
			for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
				for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
					if ( mask( k1, k2, k3, k4 ) && ( a( i1, i2, i3, i4 ) > r ) ) {
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
FArray1D< int >
maxloc( FArray5S< T > const & a, FArray5< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 5, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
		for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
			for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
				for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
					for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
						if ( mask( k1, k2, k3, k4, k5 ) && ( a( i1, i2, i3, i4, i5 ) > r ) ) {
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
FArray1D< int >
maxloc( FArray6S< T > const & a, FArray6< bool > const & mask )
{
	assert( conformable( a, mask ) );
	FArray1D< int > loc( 6, 0 ); // F2008 standard => 0 for empty arrays
	T r( std::numeric_limits< T >::lowest() );
	for ( int i6 = 1, e6 = a.u6(), k6 = mask.l6(); i6 <= e6; ++i6, ++k6 ) {
		for ( int i5 = 1, e5 = a.u5(), k5 = mask.l5(); i5 <= e5; ++i5, ++k5 ) {
			for ( int i4 = 1, e4 = a.u4(), k4 = mask.l4(); i4 <= e4; ++i4, ++k4 ) {
				for ( int i3 = 1, e3 = a.u3(), k3 = mask.l3(); i3 <= e3; ++i3, ++k3 ) {
					for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
						for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
							if ( mask( k1, k2, k3, k4, k5, k6 ) && ( a( i1, i2, i3, i4, i5, i6 ) > r ) ) {
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

// matmul /////

// Matrix (Outer) Product of 1D FArraySs
template< typename T >
inline
FArray2D< T >
matmul( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< T > m( a.isize(), b.isize() );
	for ( int j = 1, ej = b.u(); j <= ej; ++j ) {
		T const b_j( b( j ) );
		for ( int i = 1, ei = a.u(); i <= ei; ++i ) {
			m( i, j ) = a( i ) * b_j;
		}
	}
	return m;
}

// Matrix (Outer) Product of 1D Boolean FArraySs
inline
FArray2D< bool >
matmul( FArray1S< bool > const & a, FArray1S< bool > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > m( a.isize(), b.isize() );
	for ( int j = 1, ej = b.u(); j <= ej; ++j ) {
		bool const b_j( b( j ) );
		for ( int i = 1, ei = a.u(); i <= ei; ++i ) {
			m( i, j ) = a( i ) && b_j;
		}
	}
	return m;
}

// Matrix Product of 1D and 2D FArraySs
template< typename T >
inline
FArray1D< T >
matmul( FArray1S< T > const & a, FArray2S< T > const & b )
{
	assert( a.size() == b.size1() );
	FArray1D< T > m( b.isize2() );
	for ( int j = 1, ej = b.u2(); j <= ej; ++j ) {
		T dot( 0 );
		for ( int i = 1, ei = a.u(); i <= ei; ++i ) {
			dot += a( i ) * b( i, j );
		}
		m( j ) = dot;
	}
	return m;
}

// Matrix Product of 1D and 2D Boolean FArraySs
inline
FArray1D< bool >
matmul( FArray1S< bool > const & a, FArray2S< bool > const & b )
{
	assert( a.size() == b.size1() );
	FArray1D< bool > m( b.isize2() );
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

// Matrix Product of 2D and 1D FArraySs
template< typename T >
inline
FArray1D< T >
matmul( FArray2S< T > const & a, FArray1S< T > const & b )
{
	assert( a.size2() == b.size() );
	FArray1D< T > m( a.isize1() );
	for ( int i = 1, ei = a.u1(); i <= ei; ++i ) {
		T dot( 0 );
		for ( int j = 1, ej = b.u(); j <= ej; ++j ) {
			dot += a( i, j ) * b( j );
		}
		m( i ) = dot;
	}
	return m;
}

// Matrix Product of 2D and 1D Boolean FArraySs
inline
FArray1D< bool >
matmul( FArray2S< bool > const & a, FArray1S< bool > const & b )
{
	assert( a.size2() == b.size() );
	FArray1D< bool > m( a.isize1() );
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

// Matrix Product of 2D FArraySs
template< typename T >
inline
FArray2D< T >
matmul( FArray2S< T > const & a, FArray2S< T > const & b )
{
	assert( a.size2() == b.size1() );
	FArray2D< T > m( a.isize1(), b.isize2() );
	for ( int j = 1, ej = b.u2(); j <= ej; ++j ) {
		for ( int i = 1, ei = a.u1(); i <= ei; ++i ) {
			T dot( 0 );
			for ( int k = 1, ek = a.u2(); k <= ek; ++k ) {
				dot += a( i, k ) * b( k, j );
			}
			m( i, j ) = dot;
		}
	}
	return m;
}

// Matrix Product of 2D Boolean FArraySs
inline
FArray2D< bool >
matmul( FArray2S< bool > const & a, FArray2S< bool > const & b )
{
	assert( a.size2() == b.size1() );
	FArray2D< bool > m( a.isize1(), b.isize2() );
	for ( int j = 1, ej = b.u2(); j <= ej; ++j ) {
		for ( int i = 1, ei = a.u1(); i <= ei; ++i ) {
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

} // ObjexxFCL

#endif // ObjexxFCL_FArrayS_functions_hh_INCLUDED
