#ifndef ObjexxFCL_MArray_functions_hh_INCLUDED
#define ObjexxFCL_MArray_functions_hh_INCLUDED

// MArray Functions
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
#include <ObjexxFCL/MArray1.hh>
#include <ObjexxFCL/MArray2.hh>
#include <ObjexxFCL/MArray3.hh>
#include <ObjexxFCL/MArray4.hh>
#include <ObjexxFCL/MArray5.hh>
#include <ObjexxFCL/MArray6.hh>
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

template< class A, typename T >
inline
bool
allocated( MArray< A, T > const & a )
{
	return a.allocated();
}

// all /////

template< class A >
inline
bool
all( MArray1< A, bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return true;
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( ! a( i ) ) return false;
	}
	return true;
}

template< class A >
inline
bool
all( MArray2< A, bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return true;
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			if ( ! a( i1, i2 ) ) return false;
		}
	}
	return true;
}

template< class A >
inline
bool
all( MArray3< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
all( MArray4< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
all( MArray5< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
all( MArray6< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
any( MArray1< A, bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return false;
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) return true;
	}
	return false;
}

template< class A >
inline
bool
any( MArray2< A, bool > const & a )
{
	assert( a.size_bounded() );
	if ( a.empty() ) return false;
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			if ( a( i1, i2 ) ) return true;
		}
	}
	return false;
}

template< class A >
inline
bool
any( MArray3< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
any( MArray4< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
any( MArray5< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
any( MArray6< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
FArray1D< bool >
operator !( MArray1< A, bool > const & a )
{
	FArray1D< bool > r( a );
	for ( FArray1< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
FArray2D< bool >
operator !( MArray2< A, bool > const & a )
{
	FArray2D< bool > r( a );
	for ( FArray2< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
FArray3D< bool >
operator !( MArray3< A, bool > const & a )
{
	FArray3D< bool > r( a );
	for ( FArray3< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
FArray4D< bool >
operator !( MArray4< A, bool > const & a )
{
	FArray4D< bool > r( a );
	for ( FArray4< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
FArray5D< bool >
operator !( MArray5< A, bool > const & a )
{
	FArray5D< bool > r( a );
	for ( FArray5< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
FArray6D< bool >
operator !( MArray6< A, bool > const & a )
{
	FArray6D< bool > r( a );
	for ( FArray6< bool >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

// count /////

template< class A >
inline
typename MArray1< A, bool >::size_type
count( MArray1< A, bool > const & a )
{
	assert( a.size_bounded() );
	typename MArray1< A, bool >::size_type c( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) ++c;
	}
	return c;
}

template< class A >
inline
typename MArray2< A, bool >::size_type
count( MArray2< A, bool > const & a )
{
	assert( a.size_bounded() );
	typename MArray2< A, bool >::size_type c( 0u );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			if ( a( i1, i2 ) ) ++c;
		}
	}
	return c;
}

template< class A >
inline
typename MArray3< A, bool >::size_type
count( MArray3< A, bool > const & a )
{
	assert( a.size_bounded() );
	typename MArray3< A, bool >::size_type c( 0u );
	for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2, i3 ) ) ++c;
			}
		}
	}
	return c;
}

template< class A >
inline
typename MArray4< A, bool >::size_type
count( MArray4< A, bool > const & a )
{
	assert( a.size_bounded() );
	typename MArray4< A, bool >::size_type c( 0u );
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

template< class A >
inline
typename MArray5< A, bool >::size_type
count( MArray5< A, bool > const & a )
{
	assert( a.size_bounded() );
	typename MArray5< A, bool >::size_type c( 0u );
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

template< class A >
inline
typename MArray6< A, bool >::size_type
count( MArray6< A, bool > const & a )
{
	assert( a.size_bounded() );
	typename MArray6< A, bool >::size_type c( 0u );
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

template< class A >
inline
typename MArray1< A, bool >::size_type
count( MArray1< A, bool > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return count( a );
	default:
		assert( false );
		return 0;
	}
}

template< class A >
inline
FArray1D< typename MArray2< A, bool >::size_type >
count( MArray2< A, bool > const & a, int const dim )
{
	assert( a.size_bounded() );
	switch ( dim ) {
	case 1:
		{
			FArray1D< typename MArray2< A, bool >::size_type > v( a.isize2() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				typename MArray2< A, bool >::size_type c( 0u );
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
					if ( a( i1, i2 ) ) ++c;
				}
				v( i2 ) = c;
			}
			return v;
		}
	case 2:
		{
			FArray1D< typename MArray2< A, bool >::size_type > v( a.isize1() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				typename MArray2< A, bool >::size_type c( 0u );
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					if ( a( i1, i2 ) ) ++c;
				}
				v( i1 ) = c;
			}
			return v;
		}
	default:
		assert( false );
		return FArray1D< typename MArray2< A, bool >::size_type >();
	}
}

// is_contiguous /////

template< class A, typename T >
inline
bool
is_contiguous( MArray< A, T > const & )
{
	return false; // Member arrays are by definition non-contiguous
}

// lbound /////

template< class A, typename T >
inline
FArray1D< int >
lbound( MArray1< A, T > const & )
{
	return FArray1D< int >( 1, 1 );
}

template< class A, typename T >
inline
FArray1D< int >
lbound( MArray2< A, T > const & )
{
	return FArray1D< int >( 2, 1 );
}

template< class A, typename T >
inline
FArray1D< int >
lbound( MArray3< A, T > const & )
{
	return FArray1D< int >( 3, 1 );
}

template< class A, typename T >
inline
FArray1D< int >
lbound( MArray4< A, T > const & )
{
	return FArray1D< int >( 4, 1 );
}

template< class A, typename T >
inline
FArray1D< int >
lbound( MArray5< A, T > const & )
{
	return FArray1D< int >( 5, 1 );
}

template< class A, typename T >
inline
FArray1D< int >
lbound( MArray6< A, T > const & )
{
	return FArray1D< int >( 6, 1 );
}

template< class A, typename T >
inline
int
lbound( MArray1< A, T > const &, int const dim )
{
	switch ( dim ) {
	case 1:
		return 1;
	default:
		assert( false );
		return 0;
	}
}

template< class A, typename T >
inline
int
lbound( MArray2< A, T > const &, int const dim )
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

template< class A, typename T >
inline
int
lbound( MArray3< A, T > const &, int const dim )
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

template< class A, typename T >
inline
int
lbound( MArray4< A, T > const &, int const dim )
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

template< class A, typename T >
inline
int
lbound( MArray5< A, T > const &, int const dim )
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

template< class A, typename T >
inline
int
lbound( MArray6< A, T > const &, int const dim )
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

template< class A, typename T >
inline
FArray1D< int >
ubound( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 1, a.u1() );
}

template< class A, typename T >
inline
FArray1D< int >
ubound( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 2, { a.u1(), a.u2() } );
}

template< class A, typename T >
inline
FArray1D< int >
ubound( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 3, { a.u1(), a.u2(), a.u3() } );
}

template< class A, typename T >
inline
FArray1D< int >
ubound( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 4, { a.u1(), a.u2(), a.u3(), a.u4() } );
}

template< class A, typename T >
inline
FArray1D< int >
ubound( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 5, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5() } );
}

template< class A, typename T >
inline
FArray1D< int >
ubound( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 6, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5(), a.u6() } );
}

template< class A, typename T >
inline
int
ubound( MArray1< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
ubound( MArray2< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
ubound( MArray3< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
ubound( MArray4< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
ubound( MArray5< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
ubound( MArray6< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
FArray1D< int >
shape( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 1, a.isize1() );
}

template< class A, typename T >
inline
FArray1D< int >
shape( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 2, { a.isize1(), a.isize2() } );
}

template< class A, typename T >
inline
FArray1D< int >
shape( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 3, { a.isize1(), a.isize2(), a.isize3() } );
}

template< class A, typename T >
inline
FArray1D< int >
shape( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 4, { a.isize1(), a.isize2(), a.isize3(), a.isize4() } );
}

template< class A, typename T >
inline
FArray1D< int >
shape( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 5, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() } );
}

template< class A, typename T >
inline
FArray1D< int >
shape( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	return FArray1D< int >( 6, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() } );
}

// size /////

template< class A, typename T >
inline
typename MArray< A, T >::size_type
size( MArray< A, T > const & a )
{
	assert( a.size_bounded() );
	return a.size();
}

template< class A, typename T >
inline
typename MArray< A, T >::size_type
size( MArray1< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
typename MArray< A, T >::size_type
size( MArray2< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
typename MArray< A, T >::size_type
size( MArray3< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
typename MArray< A, T >::size_type
size( MArray4< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
typename MArray< A, T >::size_type
size( MArray5< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
typename MArray< A, T >::size_type
size( MArray6< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
isize( MArray< A, T > const & a )
{
	return static_cast< int >( size( a ) );
}

template< template< class, typename > class ArrayType, class A, typename T, class = typename std::enable_if< std::is_base_of< MArray< A, T >, ArrayType< A, T > >::value >::type >
inline
int
isize( ArrayType< A, T > const & a, int const dim )
{
	return static_cast< int >( size( a, dim ) );
}

// contig /////

template< class A, typename T >
inline
FArray1D< T >
contig( MArray1< A, T > const & a )
{
	return FArray1D< T >( a );
}

// reshape /////

template< class A, typename T >
inline
FArray1D< T >
reshape( MArray1< A, T > const & a )
{
	return FArray1D< T >( a );
}

template< class A, typename T, typename I >
inline
FArray1D< T >
reshape( MArray1< A, T > const & a, std::array< I, 1 > const & shape )
{
	typedef  typename MArray1< A, T >::size_type  size_type;
	FArray1D< T > r( shape[ 0 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
FArray2D< T >
reshape( MArray1< A, T > const & a, std::array< I, 2 > const & shape )
{
	typedef  typename MArray1< A, T >::size_type  size_type;
	FArray2D< T > r( shape[ 0 ], shape[ 1 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
FArray3D< T >
reshape( MArray1< A, T > const & a, std::array< I, 3 > const & shape )
{
	typedef  typename MArray1< A, T >::size_type  size_type;
	FArray3D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
FArray4D< T >
reshape( MArray1< A, T > const & a, std::array< I, 4 > const & shape )
{
	typedef  typename MArray1< A, T >::size_type  size_type;
	FArray4D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
FArray5D< T >
reshape( MArray1< A, T > const & a, std::array< I, 5 > const & shape )
{
	typedef  typename MArray1< A, T >::size_type  size_type;
	FArray5D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
FArray6D< T >
reshape( MArray1< A, T > const & a, std::array< I, 6 > const & shape )
{
	typedef  typename MArray1< A, T >::size_type  size_type;
	FArray6D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ] );
	size_type l( 0 );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
FArray1D< T >
reshape( MArray2< A, T > const & a, std::array< I, 1 > const & shape )
{
	typedef  typename MArray2< A, T >::size_type  size_type;
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

template< class A, typename T, typename I >
inline
FArray2D< T >
reshape( MArray2< A, T > const & a, std::array< I, 2 > const & shape )
{
	typedef  typename MArray2< A, T >::size_type  size_type;
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

template< class A, typename T >
inline
FArray1D< T >
pack( MArray1< A, T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
pack( MArray2< A, T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
pack( MArray3< A, T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
pack( MArray4< A, T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
pack( MArray5< A, T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
pack( MArray6< A, T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
cshift( MArray1< A, T > const & a, int const shift, int const dim = 1 )
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

template< class A, typename T >
inline
FArray2D< T >
cshift( MArray2< A, T > const & a, int const shift, int const dim = 1 )
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

template< class A, typename T >
inline
FArray2D< T >
cshift( MArray2< A, T > const & a, FArray1< int > const & shift, int const dim = 1 )
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

template< class A, typename T >
inline
FArray1D< T >
eoshift( MArray1< A, T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
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

template< class A, typename T >
inline
FArray2D< T >
eoshift( MArray2< A, T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
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

template< class A, typename T >
inline
FArray2D< T >
eoshift( MArray2< A, T > const & a, FArray1< int > const & shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
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

template< class A, typename T >
inline
T
sum( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	T s( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		s += a( i );
	}
	return s;
}

template< class A, typename T >
inline
T
sum( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	T s( 0 );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			s += a( i1, i2 );
		}
	}
	return s;
}

template< class A, typename T >
inline
T
sum( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray1< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
sum( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray1< A, T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i = 1, e = a.u(), k = mask.l(); i <= e; ++i, ++k ) {
		if ( mask( k ) ) s += a( i );
	}
	return s;
}

template< class A, typename T >
inline
T
sum( MArray2< A, T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	T s( 0 );
	for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
		for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
			if ( mask( k1, k2 ) ) s += a( i1, i2 );
		}
	}
	return s;
}

template< class A, typename T >
inline
T
sum( MArray3< A, T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray4< A, T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray5< A, T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray6< A, T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
typename T::value_type
sum_col( MArray1< A, T > const & a, int const j )
{
	assert( a.size_bounded() );
	typename T::value_type s( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		s += a( i )( j );
	}
	return s;
}

template< class A, class B, typename T >
inline
typename T::value_type
sum_product_col( MArray1< A, T > const & a, MArray1< B, T > const & b, int const j )
{
	assert( a.size_bounded() );
	assert( a.u() == b.u() );
	typename T::value_type s( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		s += a( i )( j ) * b( i )( j );
	}
	return s;
}

template< class A, typename T >
inline
T
sum_sub( MArray1< A, T > const & a, FArray1< int > const & sub )
{
	assert( sub.size_bounded() );
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		s += a( sub( i ) );
	}
	return s;
}

template< class A, typename T >
inline
T
sum_sub( MArray1< A, T > const & a, FArray1S< int > const & sub )
{
	assert( sub.size_bounded() );
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		s += a( sub( i ) );
	}
	return s;
}

template< class A, class B, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, MArray1< B, T > const & b, FArray1< int > const & sub )
{
	assert( sub.size_bounded() );
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class A, class B, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, MArray1< B, T > const & b, FArray1S< int > const & sub )
{
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class B, typename T >
inline
T
sum_product_sub( FArray1< T > const & a, MArray1< B, T > const & b, FArray1< int > const & sub )
{
	assert( sub.size_bounded() );
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class B, typename T >
inline
T
sum_product_sub( FArray1< T > const & a, MArray1< B, T > const & b, FArray1S< int > const & sub )
{
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, FArray1< T > const & b, FArray1< int > const & sub )
{
	assert( sub.size_bounded() );
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, FArray1< T > const & b, FArray1S< int > const & sub )
{
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class B, typename T >
inline
T
sum_product_sub( FArray1S< T > const & a, MArray1< B, T > const & b, FArray1< int > const & sub )
{
	assert( sub.size_bounded() );
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class B, typename T >
inline
T
sum_product_sub( FArray1S< T > const & a, MArray1< B, T > const & b, FArray1S< int > const & sub )
{
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, FArray1S< T > const & b, FArray1< int > const & sub )
{
	assert( sub.size_bounded() );
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, FArray1S< T > const & b, FArray1S< int > const & sub )
{
	T s( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		s += a( j ) * b( j );
	}
	return s;
}

// product /////

template< class A, typename T >
inline
T
product( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	T p( 1 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		p *= a( i );
	}
	return p;
}

template< class A, typename T >
inline
T
product( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	T p( 1 );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			p *= a( i1, i2 );
		}
	}
	return p;
}

template< class A, typename T >
inline
T
product( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray1< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
product( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray1< A, T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i = 1, e = a.u(), k = mask.l(); i <= e; ++i, ++k ) {
		if ( mask( k ) ) p *= a( i );
	}
	return p;
}

template< class A, typename T >
inline
T
product( MArray2< A, T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	T p( 1 );
	for ( int i2 = 1, e2 = a.u2(), k2 = mask.l2(); i2 <= e2; ++i2, ++k2 ) {
		for ( int i1 = 1, e1 = a.u1(), k1 = mask.l1(); i1 <= e1; ++i1, ++k1 ) {
			if ( mask( k1, k2 ) ) p *= a( i1, i2 );
		}
	}
	return p;
}

template< class A, typename T >
inline
T
product( MArray3< A, T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray4< A, T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray5< A, T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray6< A, T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< T >
abs( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray1D< T > r( a );
	for ( typename MArray1< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
FArray2D< T >
abs( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray2D< T > r( a );
	for ( typename MArray2< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
FArray3D< T >
abs( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray3D< T > r( a );
	for ( typename MArray3< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
FArray4D< T >
abs( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray4D< T > r( a );
	for ( typename MArray4< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
FArray5D< T >
abs( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray5D< T > r( a );
	for ( typename MArray5< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
FArray6D< T >
abs( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray6D< T > r( a );
	for ( typename MArray6< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

// pow /////

template< class A, typename T, typename X >
inline
FArray1D< T >
pow( MArray1< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray1D< T > r( a );
	for ( typename MArray1< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray2D< T >
pow( MArray2< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray2D< T > r( a );
	for ( typename MArray2< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray3D< T >
pow( MArray3< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray3D< T > r( a );
	for ( typename MArray3< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray4D< T >
pow( MArray4< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray4D< T > r( a );
	for ( typename MArray4< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray5D< T >
pow( MArray5< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray5D< T > r( a );
	for ( typename MArray5< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray6D< T >
pow( MArray6< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray6D< T > r( a );
	for ( typename MArray6< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::pow( r[ i ], x );
	}
	return r;
}

// sign /////

template< class A, typename T, typename X >
inline
FArray1D< T >
sign( MArray1< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray1D< T > r( a );
	for ( typename MArray1< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray2D< T >
sign( MArray2< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray2D< T > r( a );
	for ( typename MArray2< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray3D< T >
sign( MArray3< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray3D< T > r( a );
	for ( typename MArray3< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray4D< T >
sign( MArray4< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray4D< T > r( a );
	for ( typename MArray4< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray5D< T >
sign( MArray5< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray5D< T > r( a );
	for ( typename MArray5< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
FArray6D< T >
sign( MArray6< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	FArray6D< T > r( a );
	for ( typename MArray6< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename X, class A, typename T >
inline
FArray1D< X >
sign( X const & x, MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray1D< X > r( a );
	for ( typename MArray1< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
FArray2D< X >
sign( X const & x, MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray2D< X > r( a );
	for ( typename MArray2< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
FArray3D< X >
sign( X const & x, MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray3D< X > r( a );
	for ( typename MArray3< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
FArray4D< X >
sign( X const & x, MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray4D< X > r( a );
	for ( typename MArray4< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
FArray5D< X >
sign( X const & x, MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray5D< X > r( a );
	for ( typename MArray5< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
FArray6D< X >
sign( X const & x, MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	FArray6D< X > r( a );
	for ( typename MArray6< A, T >::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

// minval /////

template< class A, typename T >
inline
T
minval( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( std::numeric_limits< T >::max() );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r = std::min( r, a( i ) );
	}
	return r;
}

template< class A, typename T >
inline
T
minval( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( std::numeric_limits< T >::max() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			r = std::min( r, a( i1, i2 ) );
		}
	}
	return r;
}

template< class A, typename T >
inline
T
minval( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
minval( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
minval( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
minval( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
maxval( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( std::numeric_limits< T >::lowest() );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r = std::max( r, a( i ) );
	}
	return r;
}

template< class A, typename T >
inline
T
maxval( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( std::numeric_limits< T >::lowest() );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			r = std::max( r, a( i1, i2 ) );
		}
	}
	return r;
}

template< class A, typename T >
inline
T
maxval( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
maxval( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
maxval( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
maxval( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
int
minloc( MArray1< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
	switch ( dim ) {
	case 1:
		{
			FArray1D< int > loc( a.size2(), a.size2() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
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
			FArray1D< int > loc( a.size1(), a.size1() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray1< A, T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray2< A, T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray3< A, T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray4< A, T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray5< A, T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
minloc( MArray6< A, T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
int
maxloc( MArray1< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
	switch ( dim ) {
	case 1:
		{
			FArray1D< int > loc( a.size2(), a.size2() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
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
			FArray1D< int > loc( a.size1(), a.size1() > 0u ? 1 : 0 ); // F2008 standard => 0 for empty arrays
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray1< A, T > const & a, FArray1< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray2< A, T > const & a, FArray2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray3< A, T > const & a, FArray3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray4< A, T > const & a, FArray4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray5< A, T > const & a, FArray5< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
FArray1D< int >
maxloc( MArray6< A, T > const & a, FArray6< bool > const & mask )
{
	assert( a.size_bounded() );
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

// Matrix (Outer) Product of 1D MArrays
template< class A, typename T >
inline
FArray2D< T >
matmul( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix (Outer) Product of 1D Boolean MArrays
template< class A >
inline
FArray2D< bool >
matmul( MArray1< A, bool > const & a, MArray1< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 1D and 2D MArrays
template< class A, typename T >
inline
FArray1D< T >
matmul( MArray1< A, T > const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 1D and 2D Boolean MArrays
template< class A >
inline
FArray1D< bool >
matmul( MArray1< A, bool > const & a, MArray2< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D and 1D MArrays
template< class A, typename T >
inline
FArray1D< T >
matmul( MArray2< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D and 1D Boolean MArrays
template< class A >
inline
FArray1D< bool >
matmul( MArray2< A, bool > const & a, MArray1< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D MArrays
template< class A, typename T >
inline
FArray2D< T >
matmul( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D Boolean MArrays
template< class A >
inline
FArray2D< bool >
matmul( MArray2< A, bool > const & a, MArray2< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Array Generators

// Subscripted Array
template< class A, typename T >
inline
FArray1D< T >
array_sub( MArray1< A, T > const & a, FArray1< int > const & sub )
{
	FArray1D< T > r( sub.size() );
	for ( int i = sub.l(), e = sub.u(), k = 1; i <= e; ++i, ++k ) {
		r( k ) = a( sub( i ) );
	}
	return r;
}

// Subscripted Array
template< class A, typename T >
inline
FArray1D< T >
array_sub( MArray1< A, T > const & a, FArray1S< int > const & sub )
{
	FArray1D< T > r( sub.size() );
	for ( int i = sub.l(), e = sub.u(), k = 1; i <= e; ++i, ++k ) {
		r( k ) = a( sub( i ) );
	}
	return r;
}

} // ObjexxFCL

#endif // ObjexxFCL_MArray_functions_hh_INCLUDED
