#ifndef ObjexxFCL_MArray_functions_hh_INCLUDED
#define ObjexxFCL_MArray_functions_hh_INCLUDED

// MArray Functions
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
#include <ObjexxFCL/MArray1.hh>
#include <ObjexxFCL/MArray2.hh>
#include <ObjexxFCL/MArray3.hh>
#include <ObjexxFCL/MArray4.hh>
#include <ObjexxFCL/MArray5.hh>
#include <ObjexxFCL/MArray6.hh>
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
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
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
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
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

template< class A >
inline
bool
all( MArray5< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
all( MArray6< A, bool > const & a )
{
	assert( a.size_bounded() );
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
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
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
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
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

template< class A >
inline
bool
any( MArray5< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
bool
any( MArray6< A, bool > const & a )
{
	assert( a.size_bounded() );
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

// abs /////

template< class A, typename T >
inline
Array1D< T >
abs( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
abs( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
abs( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
abs( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
abs( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	Array5D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
abs( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	Array6D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = std::abs( r[ i ] );
	}
	return r;
}

// Negation /////

template< class A >
inline
Array1D< bool >
operator !( MArray1< A, bool > const & a )
{
	Array1D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
Array2D< bool >
operator !( MArray2< A, bool > const & a )
{
	Array2D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
Array3D< bool >
operator !( MArray3< A, bool > const & a )
{
	Array3D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
Array4D< bool >
operator !( MArray4< A, bool > const & a )
{
	Array4D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
Array5D< bool >
operator !( MArray5< A, bool > const & a )
{
	Array5D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

template< class A >
inline
Array6D< bool >
operator !( MArray6< A, bool > const & a )
{
	Array6D< bool > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ! r[ i ];
	}
	return r;
}

// Bitwise Not /////

template< class A, typename T >
inline
Array1D< T >
bit_not( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_not( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_not( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_not( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_not( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	Array5D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_not( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	Array6D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = ~r[ i ];
	}
	return r;
}

// Bitwise And /////

template< class A, typename T >
inline
Array1D< T >
bit_and( MArray1< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_and( MArray2< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_and( MArray3< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_and( MArray4< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_and( MArray5< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array5D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_and( MArray6< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array6D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= b;
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
bit_and( T const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_and( T const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_and( T const & a, MArray3< A, T > const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_and( T const & a, MArray4< A, T > const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_and( T const & a, MArray5< A, T > const & b )
{
	assert( a.size_bounded() );
	Array5D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_and( T const & a, MArray6< A, T > const & b )
{
	assert( a.size_bounded() );
	Array6D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] &= a;
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
bit_and( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array1D< T > r( a );
	for ( BArray::size_type l = 0, e = a.size(); l < e; ++l ) {
		r[ l ] &= b[ l ];
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_and( MArray2< A, T > const & a, MArray2< A, T > const & b )
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

template< class A, typename T >
inline
Array3D< T >
bit_and( MArray3< A, T > const & a, MArray3< A, T > const & b )
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

template< class A, typename T >
inline
Array4D< T >
bit_and( MArray4< A, T > const & a, MArray4< A, T > const & b )
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

template< class A, typename T >
inline
Array5D< T >
bit_and( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array5D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] &= b( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_and( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array6D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							r[ l ] &= b( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
		}
	}
	return r;
}

// Bitwise Or /////

template< class A, typename T >
inline
Array1D< T >
bit_or( MArray1< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_or( MArray2< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_or( MArray3< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_or( MArray4< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_or( MArray5< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array5D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_or( MArray6< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array6D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= b;
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
bit_or( T const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_or( T const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_or( T const & a, MArray3< A, T > const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_or( T const & a, MArray4< A, T > const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_or( T const & a, MArray5< A, T > const & b )
{
	assert( a.size_bounded() );
	Array5D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_or( T const & a, MArray6< A, T > const & b )
{
	assert( a.size_bounded() );
	Array6D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] |= a;
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
bit_or( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array1D< T > r( a );
	for ( BArray::size_type l = 0, e = a.size(); l < e; ++l ) {
		r[ l ] |= b[ l ];
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_or( MArray2< A, T > const & a, MArray2< A, T > const & b )
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

template< class A, typename T >
inline
Array3D< T >
bit_or( MArray3< A, T > const & a, MArray3< A, T > const & b )
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

template< class A, typename T >
inline
Array4D< T >
bit_or( MArray4< A, T > const & a, MArray4< A, T > const & b )
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

template< class A, typename T >
inline
Array5D< T >
bit_or( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array5D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] |= b( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_or( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array6D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							r[ l ] |= b( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
		}
	}
	return r;
}

// Bitwise Xor /////

template< class A, typename T >
inline
Array1D< T >
bit_xor( MArray1< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= b;
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_xor( MArray2< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= b;
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_xor( MArray3< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= b;
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_xor( MArray4< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= b;
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_xor( MArray5< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array5D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= b;
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_xor( MArray6< A, T > const & a, T const & b )
{
	assert( a.size_bounded() );
	Array6D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= b;
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
bit_xor( T const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	Array1D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= a;
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_xor( T const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	Array2D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= a;
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_xor( T const & a, MArray3< A, T > const & b )
{
	assert( a.size_bounded() );
	Array3D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= a;
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_xor( T const & a, MArray4< A, T > const & b )
{
	assert( a.size_bounded() );
	Array4D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= a;
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_xor( T const & a, MArray5< A, T > const & b )
{
	assert( a.size_bounded() );
	Array5D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= a;
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_xor( T const & a, MArray6< A, T > const & b )
{
	assert( a.size_bounded() );
	Array6D< T > r( b );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] ^= a;
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
bit_xor( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array1D< T > r( a );
	for ( BArray::size_type l = 0, e = a.size(); l < e; ++l ) {
		r[ l ] ^= b[ l ];
	}
	return r;
}

template< class A, typename T >
inline
Array2D< T >
bit_xor( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array2D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] ^= b( i1, i2 );
		}
	}
	return r;
}

template< class A, typename T >
inline
Array3D< T >
bit_xor( MArray3< A, T > const & a, MArray3< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array3D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
				r[ l ] ^= b( i1, i2, i3 );
			}
		}
	}
	return r;
}

template< class A, typename T >
inline
Array4D< T >
bit_xor( MArray4< A, T > const & a, MArray4< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array4D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
					r[ l ] ^= b( i1, i2, i3, i4 );
				}
			}
		}
	}
	return r;
}

template< class A, typename T >
inline
Array5D< T >
bit_xor( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array5D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] ^= b( i1, i2, i3, i4, i5 );
					}
				}
			}
		}
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
bit_xor( MArray6< A, T > const & a, MArray6< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( conformable( a, b ) );
	Array6D< T > r( a );
	BArray::size_type l( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
						for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6, ++l ) {
							r[ l ] ^= b( i1, i2, i3, i4, i5, i6 );
						}
					}
				}
			}
		}
	}
	return r;
}

// pow /////

template< class A, typename T, typename X >
inline
Array1D< T >
pow( MArray1< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array2D< T >
pow( MArray2< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array3D< T >
pow( MArray3< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array4D< T >
pow( MArray4< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array5D< T >
pow( MArray5< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array5D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array6D< T >
pow( MArray6< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array6D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = T( std::pow( r[ i ], x ) );
	}
	return r;
}

// sign /////

template< class A, typename T, typename X >
inline
Array1D< T >
sign( MArray1< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array1D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array2D< T >
sign( MArray2< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array2D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array3D< T >
sign( MArray3< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array3D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array4D< T >
sign( MArray4< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array4D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array5D< T >
sign( MArray5< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array5D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< class A, typename T, typename X >
inline
Array6D< T >
sign( MArray6< A, T > const & a, X const & x )
{
	assert( a.size_bounded() );
	Array6D< T > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( r[ i ], x );
	}
	return r;
}

template< typename X, class A, typename T >
inline
Array1D< X >
sign( X const & x, MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	Array1D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
Array2D< X >
sign( X const & x, MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	Array2D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
Array3D< X >
sign( X const & x, MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	Array3D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
Array4D< X >
sign( X const & x, MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	Array4D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
Array5D< X >
sign( X const & x, MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	Array5D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

template< typename X, class A, typename T >
inline
Array6D< X >
sign( X const & x, MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	Array6D< X > r( a );
	for ( BArray::size_type i = 0, e = a.size(); i < e; ++i ) {
		r[ i ] = sign( x, r[ i ] );
	}
	return r;
}

// count /////

template< class A >
inline
BArray::size_type
count( MArray1< A, bool > const & a )
{
	assert( a.size_bounded() );
	BArray::size_type c( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) ) ++c;
	}
	return c;
}

template< class A >
inline
BArray::size_type
count( MArray2< A, bool > const & a )
{
	assert( a.size_bounded() );
	BArray::size_type c( 0u );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			if ( a( i1, i2 ) ) ++c;
		}
	}
	return c;
}

template< class A >
inline
BArray::size_type
count( MArray3< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
BArray::size_type
count( MArray4< A, bool > const & a )
{
	assert( a.size_bounded() );
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

template< class A >
inline
BArray::size_type
count( MArray5< A, bool > const & a )
{
	assert( a.size_bounded() );
	BArray::size_type c( 0u );
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

template< class A >
inline
BArray::size_type
count( MArray6< A, bool > const & a )
{
	assert( a.size_bounded() );
	BArray::size_type c( 0u );
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

template< class A >
inline
BArray::size_type
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
Array1D< BArray::size_type >
count( MArray2< A, bool > const & a, int const dim )
{
	assert( a.size_bounded() );
	switch ( dim ) {
	case 1:
		{
		Array1D< BArray::size_type > v( a.isize2() );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			BArray::size_type c( 0u );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) ) ++c;
			}
			v( i2 ) = c;
		}
		return v;
		}
	case 2:
		{
		Array1D< BArray::size_type > v( a.isize1() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			BArray::size_type c( 0u );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				if ( a( i1, i2 ) ) ++c;
			}
			v( i1 ) = c;
		}
		return v;
		}
	default:
		assert( false );
		return Array1D< BArray::size_type >();
	}
}

// contiguous /////

template< class A, typename T >
inline
bool
contiguous( MArray< A, T > const & )
{
	return false; // Member arrays are by definition non-contiguous
}

// lbound /////

template< class A, typename T >
inline
Array1D< int >
lbound( MArray1< A, T > const & )
{
	return Array1D< int >( 1, 1 );
}

template< class A, typename T >
inline
Array1D< int >
lbound( MArray2< A, T > const & )
{
	return Array1D< int >( 2, 1 );
}

template< class A, typename T >
inline
Array1D< int >
lbound( MArray3< A, T > const & )
{
	return Array1D< int >( 3, 1 );
}

template< class A, typename T >
inline
Array1D< int >
lbound( MArray4< A, T > const & )
{
	return Array1D< int >( 4, 1 );
}

template< class A, typename T >
inline
Array1D< int >
lbound( MArray5< A, T > const & )
{
	return Array1D< int >( 5, 1 );
}

template< class A, typename T >
inline
Array1D< int >
lbound( MArray6< A, T > const & )
{
	return Array1D< int >( 6, 1 );
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
Array1D< int >
ubound( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 1, a.u1() );
}

template< class A, typename T >
inline
Array1D< int >
ubound( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 2, { a.u1(), a.u2() } );
}

template< class A, typename T >
inline
Array1D< int >
ubound( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 3, { a.u1(), a.u2(), a.u3() } );
}

template< class A, typename T >
inline
Array1D< int >
ubound( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 4, { a.u1(), a.u2(), a.u3(), a.u4() } );
}

template< class A, typename T >
inline
Array1D< int >
ubound( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 5, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5() } );
}

template< class A, typename T >
inline
Array1D< int >
ubound( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 6, { a.u1(), a.u2(), a.u3(), a.u4(), a.u5(), a.u6() } );
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
		assert( a.I1().bounded() );
		return a.u1();
	case 2:
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

template< class A, typename T >
inline
int
ubound( MArray4< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
ubound( MArray5< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
int
ubound( MArray6< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
Array1D< int >
shape( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 1, a.isize1() );
}

template< class A, typename T >
inline
Array1D< int >
shape( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 2, { a.isize1(), a.isize2() } );
}

template< class A, typename T >
inline
Array1D< int >
shape( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 3, { a.isize1(), a.isize2(), a.isize3() } );
}

template< class A, typename T >
inline
Array1D< int >
shape( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 4, { a.isize1(), a.isize2(), a.isize3(), a.isize4() } );
}

template< class A, typename T >
inline
Array1D< int >
shape( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 5, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() } );
}

template< class A, typename T >
inline
Array1D< int >
shape( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
	return Array1D< int >( 6, { a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() } );
}

// size /////

template< class A, typename T >
inline
BArray::size_type
size( MArray< A, T > const & a )
{
	return a.size();
}

template< class A, typename T >
inline
BArray::size_type
size( MArray1< A, T > const & a, int const dim )
{
	switch ( dim ) {
	case 1:
		return a.size1();
	default:
		assert( false );
		return 0;
	}
}

template< class A, typename T >
inline
BArray::size_type
size( MArray2< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
BArray::size_type
size( MArray3< A, T > const & a, int const dim )
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

template< class A, typename T >
inline
BArray::size_type
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
		return a.size4();
	default:
		assert( false );
		return 0;
	}
}

template< class A, typename T >
inline
BArray::size_type
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
		return a.size5();
	default:
		assert( false );
		return 0;
	}
}

template< class A, typename T >
inline
BArray::size_type
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

template< template< class, typename > class MArrayType, class A, typename T, class = typename std::enable_if< std::is_base_of< MArray< A, T >, MArrayType< A, T > >::value >::type >
inline
int
isize( MArrayType< A, T > const & a, int const dim )
{
	return static_cast< int >( size( a, dim ) );
}

// contig /////

template< class A, typename T >
inline
Array1D< T >
contig( MArray1< A, T > const & a )
{
	return Array1D< T >( a );
}

// reshape /////

template< class A, typename T >
inline
Array1D< T >
reshape( MArray1< A, T > const & a )
{
	return Array1D< T >( a );
}

template< class A, typename T, typename I >
inline
Array1D< T >
reshape( MArray1< A, T > const & a, std::array< I, 1 > const & shape )
{
	assert( a.size_bounded() );
	typedef  BArray::size_type  size_type;
	Array1D< T > r( shape[ 0 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
Array2D< T >
reshape( MArray1< A, T > const & a, std::array< I, 2 > const & shape )
{
	assert( a.size_bounded() );
	typedef  BArray::size_type  size_type;
	Array2D< T > r( shape[ 0 ], shape[ 1 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
Array3D< T >
reshape( MArray1< A, T > const & a, std::array< I, 3 > const & shape )
{
	assert( a.size_bounded() );
	typedef  BArray::size_type  size_type;
	Array3D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
Array4D< T >
reshape( MArray1< A, T > const & a, std::array< I, 4 > const & shape )
{
	assert( a.size_bounded() );
	typedef  BArray::size_type  size_type;
	Array4D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
Array5D< T >
reshape( MArray1< A, T > const & a, std::array< I, 5 > const & shape )
{
	assert( a.size_bounded() );
	typedef  BArray::size_type  size_type;
	Array5D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
Array6D< T >
reshape( MArray1< A, T > const & a, std::array< I, 6 > const & shape )
{
	assert( a.size_bounded() );
	typedef  BArray::size_type  size_type;
	Array6D< T > r( shape[ 0 ], shape[ 1 ], shape[ 2 ], shape[ 3 ], shape[ 4 ], shape[ 5 ] );
	size_type l( 0u );
	size_type const s( r.size() );
	for ( int i = 1, e = a.u(); ( ( i <= e ) && ( l < s ) ); ++i, ++l ) {
		r[ l ] = a( i );
	}
	return r;
}

template< class A, typename T, typename I >
inline
Array1D< T >
reshape( MArray2< A, T > const & a, std::array< I, 1 > const & shape )
{
	assert( a.size_bounded() );
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

template< class A, typename T, typename I >
inline
Array2D< T >
reshape( MArray2< A, T > const & a, std::array< I, 2 > const & shape )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< T >
pack( MArray1< A, T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< T >
pack( MArray2< A, T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< T >
pack( MArray3< A, T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< T >
pack( MArray4< A, T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< T >
pack( MArray5< A, T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
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
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
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
Array1D< T >
pack( MArray6< A, T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
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

// unpack /////

template< class A, typename T >
inline
Array1D< T >
unpack( Array1< T > const & a, Array1< bool > const & mask, MArray1< A, T > const & f )
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

template< class A, typename T >
inline
Array2D< T >
unpack( Array1< T > const & a, Array2< bool > const & mask, MArray2< A, T > const & f )
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

template< class A, typename T >
inline
Array3D< T >
unpack( Array1< T > const & a, Array3< bool > const & mask, MArray3< A, T > const & f )
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

template< class A, typename T >
inline
Array4D< T >
unpack( Array1< T > const & a, Array4< bool > const & mask, MArray4< A, T > const & f )
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

template< class A, typename T >
inline
Array5D< T >
unpack( Array1< T > const & a, Array5< bool > const & mask, MArray5< A, T > const & f )
{
	assert( mask.size_bounded() );
	assert( mask.conformable( f ) );
	typedef  BArray::size_type  size_type;
	Array5D< T > r( f );
	size_type i( 0u );
	for ( size_type l = 0, e = mask.size(); l < e; ++l ) {
		if ( mask[ l ] ) {
			r[ l ] = a[ i ];
			++i;
		}
	}
	return r;
}

template< class A, typename T >
inline
Array6D< T >
unpack( Array1< T > const & a, Array6< bool > const & mask, MArray6< A, T > const & f )
{
	assert( mask.size_bounded() );
	assert( mask.conformable( f ) );
	typedef  BArray::size_type  size_type;
	Array6D< T > r( f );
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

template< class A, typename T >
inline
Array1D< T >
cshift( MArray1< A, T > const & a, int const shift, int const dim = 1 )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array2D< T >
cshift( MArray2< A, T > const & a, int const shift, int const dim = 1 )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array2D< T >
cshift( MArray2< A, T > const & a, Array1< int > const & shift, int const dim = 1 )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< T >
eoshift( MArray1< A, T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array2D< T >
eoshift( MArray2< A, T > const & a, int const shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array2D< T >
eoshift( MArray2< A, T > const & a, Array1< int > const & shift, T const bdy = TypeTraits< T >::initial_value(), int const dim = 1 )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r += a( i );
	}
	return r;
}

template< class A, typename T >
inline
T
sum( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( 0 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			r += a( i1, i2 );
		}
	}
	return r;
}

template< class A, typename T >
inline
T
sum( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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
	T r( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r += a( i );
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
sum( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray1< A, T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 0 );
	size_type l( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) r += a( i );
	}
	return r;
}

template< class A, typename T >
inline
T
sum( MArray2< A, T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray3< A, T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray4< A, T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
sum( MArray5< A, T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

template< class A, typename T >
inline
T
sum( MArray6< A, T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

template< class A, typename T >
inline
T
sum_row( MArray2< A, T > const & a, int const i )
{
	T r( 0 );
	for ( int j = 1, e = a.u2(); j <= e; ++j ) {
		r += a( i, j );
	}
	return r;
}

template< class A, typename T >
inline
T
sum_col( MArray2< A, T > const & a, int const j )
{
	T r( 0 );
	for ( int i = 1, e = a.u1(); i <= e; ++i ) {
		r += a( i, j );
	}
	return r;
}

template< class A, typename T >
inline
typename T::value_type
sum_col( MArray1< A, T > const & a, int const j )
{
	assert( a.size_bounded() );
	typename T::value_type r( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r += a( i )( j );
	}
	return r;
}

template< class A, class B, typename T >
inline
typename T::value_type
sum_product_col( MArray1< A, T > const & a, MArray1< B, T > const & b, int const j )
{
	assert( a.size_bounded() );
	assert( a.u() == b.u() );
	typename T::value_type r( 0 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r += a( i )( j ) * b( i )( j );
	}
	return r;
}

template< class A, typename T >
inline
T
sum_sub( MArray1< A, T > const & a, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		r += a( sub( i ) );
	}
	return r;
}

template< class A, typename T >
inline
T
sum_sub( MArray1< A, T > const & a, Array1S< int > const & sub )
{
	assert( sub.size_bounded() );
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		r += a( sub( i ) );
	}
	return r;
}

template< class A, class B, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, MArray1< B, T > const & b, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class A, class B, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, MArray1< B, T > const & b, Array1S< int > const & sub )
{
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class B, typename T >
inline
T
sum_product_sub( Array1< T > const & a, MArray1< B, T > const & b, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class B, typename T >
inline
T
sum_product_sub( Array1< T > const & a, MArray1< B, T > const & b, Array1S< int > const & sub )
{
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, Array1< T > const & b, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, Array1< T > const & b, Array1S< int > const & sub )
{
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class B, typename T >
inline
T
sum_product_sub( Array1S< T > const & a, MArray1< B, T > const & b, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class B, typename T >
inline
T
sum_product_sub( Array1S< T > const & a, MArray1< B, T > const & b, Array1S< int > const & sub )
{
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, Array1S< T > const & b, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

template< class A, typename T >
inline
T
sum_product_sub( MArray1< A, T > const & a, Array1S< T > const & b, Array1S< int > const & sub )
{
	T r( 0 );
	for ( int i = sub.l(), e = sub.u(); i <= e; ++i ) {
		int const j( sub( i ) );
		r += a( j ) * b( j );
	}
	return r;
}

// product /////

template< class A, typename T >
inline
T
product( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( 1 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r *= a( i );
	}
	return r;
}

template< class A, typename T >
inline
T
product( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( 1 );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			r *= a( i1, i2 );
		}
	}
	return r;
}

template< class A, typename T >
inline
T
product( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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
	T r( 1 );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		r *= a( i );
	}
	return r;
}

template< class A, typename T >
inline
Array1D< T >
product( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray1< A, T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
	T r( 1 );
	size_type l( 0u );
	for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
		if ( mask[ l ] ) r *= a( i );
	}
	return r;
}

template< class A, typename T >
inline
T
product( MArray2< A, T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray3< A, T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray4< A, T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
product( MArray5< A, T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

template< class A, typename T >
inline
T
product( MArray6< A, T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

template< class A, typename T >
inline
T
product_row( MArray2< A, T > const & a, int const i )
{
	T r( 1 );
	for ( int j = 1, e = a.u2(); j <= e; ++j ) {
		r *= a( i, j );
	}
	return r;
}

template< class A, typename T >
inline
T
product_col( MArray2< A, T > const & a, int const j )
{
	T r( 1 );
	for ( int i = 1, e = a.u1(); i <= e; ++i ) {
		r *= a( i, j );
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
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1 ) );
	for ( int i = 2, e = a.u(); i <= e; ++i ) {
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
	T r( a.empty() ? std::numeric_limits< T >::max() : a( 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
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

template< class A, typename T >
inline
T
minval( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
minval( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
minval( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
maxval( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1 ) );
	for ( int i = 2, e = a.u(); i <= e; ++i ) {
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
	T r( a.empty() ? std::numeric_limits< T >::lowest() : a( 1, 1 ) );
	for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
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

template< class A, typename T >
inline
T
maxval( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
maxval( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
T
maxval( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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
Array1D< int >
minloc( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray1< A, T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray2< A, T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray3< A, T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray4< A, T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray5< A, T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

template< class A, typename T >
inline
Array1D< int >
minloc( MArray6< A, T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray1< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray2< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray3< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray4< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray5< A, T > const & a )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray6< A, T > const & a )
{
	assert( a.size_bounded() );
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
Array1D< int >
maxloc( MArray2< A, T > const & a, int const dim )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray1< A, T > const & a, Array1< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray2< A, T > const & a, Array2< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray3< A, T > const & a, Array3< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray4< A, T > const & a, Array4< bool > const & mask )
{
	assert( a.size_bounded() );
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray5< A, T > const & a, Array5< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

template< class A, typename T >
inline
Array1D< int >
maxloc( MArray6< A, T > const & a, Array6< bool > const & mask )
{
	assert( a.size_bounded() );
	assert( conformable( a, mask ) );
	typedef  BArray::size_type  size_type;
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

// Matrix (Outer) Product of 1D MArrays
template< class A, typename T >
inline
Array2D< T >
matmul( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix (Outer) Product of 1D Boolean MArrays
template< class A >
inline
Array2D< bool >
matmul( MArray1< A, bool > const & a, MArray1< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 1D and 2D MArrays
template< class A, typename T >
inline
Array1D< T >
matmul( MArray1< A, T > const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 1D and 2D Boolean MArrays
template< class A >
inline
Array1D< bool >
matmul( MArray1< A, bool > const & a, MArray2< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D and 1D MArrays
template< class A, typename T >
inline
Array1D< T >
matmul( MArray2< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D and 1D Boolean MArrays
template< class A >
inline
Array1D< bool >
matmul( MArray2< A, bool > const & a, MArray1< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D MArrays
template< class A, typename T >
inline
Array2D< T >
matmul( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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

// Matrix Product of 2D Boolean MArrays
template< class A >
inline
Array2D< bool >
matmul( MArray2< A, bool > const & a, MArray2< A, bool > const & b )
{
	assert( a.size_bounded() );
	assert( b.size_bounded() );
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
template< class A, typename T >
inline
Array1D< T >
merge( MArray1< A, T > const & a, MArray1< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array1D< T >
merge( MArray1< A, T > const & a, Array1< T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array1D< T >
merge( Array1< T > const & a, MArray1< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array1D< T >
merge( MArray1< A, T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array1D< T >( a.isize(), a ); // 1-based copy of a
	} else {
		return Array1D< T >( a.isize(), b ); // 1-based copy of b
	}
}

// Merge 1D Array + Scalar
template< class A, typename T >
inline
Array1D< T >
merge( T const & a, MArray1< A, T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array1D< T >( b.isize(), a ); // 1-based copy of a
	} else {
		return Array1D< T >( b.isize(), b ); // 1-based copy of b
	}
}

// Merge 1D Arrays
template< class A, typename T >
inline
Array1D< T >
merge( MArray1< A, T > const & a, MArray1< A, T > const & b, Array1< bool > const & mask )
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
template< class A, typename T >
inline
Array1D< T >
merge( MArray1< A, T > const & a, Array1< T > const & b, Array1< bool > const & mask )
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
template< class A, typename T >
inline
Array1D< T >
merge( Array1< T > const & a, MArray1< A, T > const & b, Array1< bool > const & mask )
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
template< class A, typename T >
inline
Array1D< T >
merge( MArray1< A, T > const & a, T const & b, Array1< bool > const & mask )
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
template< class A, typename T >
inline
Array1D< T >
merge( T const & a, MArray1< A, T > const & b, Array1< bool > const & mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( MArray2< A, T > const & a, MArray2< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( MArray2< A, T > const & a, Array2< T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( Array2< T > const & a, MArray2< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( MArray2< A, T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array2D< T >( a.isize1(), a.isize2(), a ); // 1-based copy of a
	} else {
		return Array2D< T >( a.isize1(), a.isize2(), b ); // 1-based copy of b
	}
}

// Merge 2D Array + Scalar
template< class A, typename T >
inline
Array2D< T >
merge( T const & a, MArray2< A, T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array2D< T >( b.isize1(), b.isize2(), a ); // 1-based copy of a
	} else {
		return Array2D< T >( b.isize1(), b.isize2(), b ); // 1-based copy of b
	}
}

// Merge 2D Arrays
template< class A, typename T >
inline
Array2D< T >
merge( MArray2< A, T > const & a, MArray2< A, T > const & b, Array2< bool > const & mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( MArray2< A, T > const & a, Array2< T > const & b, Array2< bool > const & mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( Array2< T > const & a, MArray2< A, T > const & b, Array2< bool > const & mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( MArray2< A, T > const & a, T const & b, Array2< bool > const & mask )
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
template< class A, typename T >
inline
Array2D< T >
merge( T const & a, MArray2< A, T > const & b, Array2< bool > const & mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( MArray3< A, T > const & a, MArray3< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( MArray3< A, T > const & a, Array3< T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( Array3< T > const & a, MArray3< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( MArray3< A, T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array3D< T >( a.isize1(), a.isize2(), a.isize3(), a ); // 1-based copy of a
	} else {
		return Array3D< T >( a.isize1(), a.isize2(), a.isize3(), b ); // 1-based copy of b
	}
}

// Merge 3D Array + Scalar
template< class A, typename T >
inline
Array3D< T >
merge( T const & a, MArray3< A, T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array3D< T >( b.isize1(), b.isize2(), b.isize3(), a ); // 1-based copy of a
	} else {
		return Array3D< T >( b.isize1(), b.isize2(), b.isize3(), b ); // 1-based copy of b
	}
}

// Merge 3D Arrays
template< class A, typename T >
inline
Array3D< T >
merge( MArray3< A, T > const & a, MArray3< A, T > const & b, Array3< bool > const & mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( MArray3< A, T > const & a, Array3< T > const & b, Array3< bool > const & mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( Array3< T > const & a, MArray3< A, T > const & b, Array3< bool > const & mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( MArray3< A, T > const & a, T const & b, Array3< bool > const & mask )
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
template< class A, typename T >
inline
Array3D< T >
merge( T const & a, MArray3< A, T > const & b, Array3< bool > const & mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( MArray4< A, T > const & a, MArray4< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( MArray4< A, T > const & a, Array4< T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( Array4< T > const & a, MArray4< A, T > const & b, bool const mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( MArray4< A, T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array4D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a ); // 1-based copy of a
	} else {
		return Array4D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), b ); // 1-based copy of b
	}
}

// Merge 4D Array + Scalar
template< class A, typename T >
inline
Array4D< T >
merge( T const & a, MArray4< A, T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array4D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), a ); // 1-based copy of a
	} else {
		return Array4D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b ); // 1-based copy of b
	}
}

// Merge 4D Arrays
template< class A, typename T >
inline
Array4D< T >
merge( MArray4< A, T > const & a, MArray4< A, T > const & b, Array4< bool > const & mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( MArray4< A, T > const & a, Array4< T > const & b, Array4< bool > const & mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( Array4< T > const & a, MArray4< A, T > const & b, Array4< bool > const & mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( MArray4< A, T > const & a, T const & b, Array4< bool > const & mask )
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
template< class A, typename T >
inline
Array4D< T >
merge( T const & a, MArray4< A, T > const & b, Array4< bool > const & mask )
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

// Merge 5D Arrays
template< class A, typename T >
inline
Array5D< T >
merge( MArray5< A, T > const & a, MArray5< A, T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array5D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a ); // 1-based copy of a
	} else {
		return Array5D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b ); // 1-based copy of b
	}
}

// Merge 5D Arrays
template< class A, typename T >
inline
Array5D< T >
merge( MArray5< A, T > const & a, Array5< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array5D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a ); // 1-based copy of a
	} else {
		return Array5D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b ); // 1-based copy of b
	}
}

// Merge 5D Arrays
template< class A, typename T >
inline
Array5D< T >
merge( Array5< T > const & a, MArray5< A, T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array5D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a ); // 1-based copy of a
	} else {
		return Array5D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b ); // 1-based copy of b
	}
}

// Merge 5D Array + Scalar
template< class A, typename T >
inline
Array5D< T >
merge( MArray5< A, T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array5D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a ); // 1-based copy of a
	} else {
		return Array5D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), b ); // 1-based copy of b
	}
}

// Merge 5D Array + Scalar
template< class A, typename T >
inline
Array5D< T >
merge( T const & a, MArray5< A, T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array5D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), a ); // 1-based copy of a
	} else {
		return Array5D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b ); // 1-based copy of b
	}
}

// Merge 5D Arrays
template< class A, typename T >
inline
Array5D< T >
merge( MArray5< A, T > const & a, MArray5< A, T > const & b, Array5< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array5D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ib1 = b.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1, ++ib1 ) {
		for ( int ia2 = a.l2(), ib2 = b.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2, ++ib2 ) {
			for ( int ia3 = a.l3(), ib3 = b.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3, ++ib3 ) {
				for ( int ia4 = a.l4(), ib4 = b.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4, ++ib4 ) {
					for ( int ia5 = a.l5(), ib5 = b.l5(), ea5 = a.u5(); ia5 <= ea5; ++ia5, ++ib5, ++l ) {
						m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4, ia5 ) : b( ib1, ib2, ib3, ib4, ib5 ) );
					}
				}
			}
		}
	}
	return m;
}

// Merge 5D Arrays
template< class A, typename T >
inline
Array5D< T >
merge( MArray5< A, T > const & a, Array5< T > const & b, Array5< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array5D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3 ) {
				for ( int ia4 = a.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4 ) {
					for ( int ia5 = a.l5(), ea5 = a.u5(); ia5 <= ea5; ++ia5, ++l ) {
						m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4, ia5 ) : b[ l ] );
					}
				}
			}
		}
	}
	return m;
}

// Merge 5D Arrays
template< class A, typename T >
inline
Array5D< T >
merge( Array5< T > const & a, MArray5< A, T > const & b, Array5< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array5D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3 ) {
				for ( int ib4 = b.l4(), eb4 = b.u4(); ib4 <= eb4; ++ib4 ) {
					for ( int ib5 = b.l5(), eb5 = b.u5(); ib5 <= eb5; ++ib5, ++l ) {
						m[ l ] = ( mask[ l ] ? a[ l ] : b( ib1, ib2, ib3, ib4, ib5 ) );
					}
				}
			}
		}
	}
	return m;
}

// Merge 5D Array + Scalar
template< class A, typename T >
inline
Array5D< T >
merge( MArray5< A, T > const & a, T const & b, Array5< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( mask ) );
	Array5D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3 ) {
				for ( int ia4 = a.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4 ) {
					for ( int ia5 = a.l5(), ea5 = a.u5(); ia5 <= ea5; ++ia5, ++l ) {
						m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4, ia5 ) : b );
					}
				}
			}
		}
	}
	return m;
}

// Merge 5D Array + Scalar
template< class A, typename T >
inline
Array5D< T >
merge( T const & a, MArray5< A, T > const & b, Array5< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( b.size_bounded() );
	assert( b.conformable( mask ) );
	Array5D< T > m( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3 ) {
				for ( int ib4 = b.l4(), eb4 = b.u4(); ib4 <= eb4; ++ib4 ) {
					for ( int ib5 = b.l5(), eb5 = b.u5(); ib5 <= eb5; ++ib5, ++l ) {
						m[ l ] = ( mask[ l ] ? a : b( ib1, ib2, ib3, ib4, ib5 ) );
					}
				}
			}
		}
	}
	return m;
}

// Merge 6D Arrays
template< class A, typename T >
inline
Array6D< T >
merge( MArray6< A, T > const & a, MArray6< A, T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array6D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), a ); // 1-based copy of a
	} else {
		return Array6D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b.isize6(), b ); // 1-based copy of b
	}
}

// Merge 6D Arrays
template< class A, typename T >
inline
Array6D< T >
merge( MArray6< A, T > const & a, Array6< T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array6D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), a ); // 1-based copy of a
	} else {
		return Array6D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b.isize6(), b ); // 1-based copy of b
	}
}

// Merge 6D Arrays
template< class A, typename T >
inline
Array6D< T >
merge( Array6< T > const & a, MArray6< A, T > const & b, bool const mask )
{
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	if ( mask ) {
		return Array6D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), a ); // 1-based copy of a
	} else {
		return Array6D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b.isize6(), b ); // 1-based copy of b
	}
}

// Merge 6D Array + Scalar
template< class A, typename T >
inline
Array6D< T >
merge( MArray6< A, T > const & a, T const & b, bool const mask )
{
	assert( a.size_bounded() );
	if ( mask ) {
		return Array6D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), a ); // 1-based copy of a
	} else {
		return Array6D< T >( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6(), b ); // 1-based copy of b
	}
}

// Merge 6D Array + Scalar
template< class A, typename T >
inline
Array6D< T >
merge( T const & a, MArray6< A, T > const & b, bool const mask )
{
	assert( b.size_bounded() );
	if ( mask ) {
		return Array6D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b.isize6(), a ); // 1-based copy of a
	} else {
		return Array6D< T >( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b.isize6(), b ); // 1-based copy of b
	}
}

// Merge 6D Arrays
template< class A, typename T >
inline
Array6D< T >
merge( MArray6< A, T > const & a, MArray6< A, T > const & b, Array6< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array6D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ib1 = b.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1, ++ib1 ) {
		for ( int ia2 = a.l2(), ib2 = b.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2, ++ib2 ) {
			for ( int ia3 = a.l3(), ib3 = b.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3, ++ib3 ) {
				for ( int ia4 = a.l4(), ib4 = b.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4, ++ib4 ) {
					for ( int ia5 = a.l5(), ib5 = b.l5(), ea5 = a.u5(); ia5 <= ea5; ++ia5, ++ib5 ) {
						for ( int ia6 = a.l6(), ib6 = b.l6(), ea6 = a.u6(); ia6 <= ea6; ++ia6, ++ib6, ++l ) {
							m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4, ia5, ia6 ) : b( ib1, ib2, ib3, ib4, ib5, ib6 ) );
						}
					}
				}
			}
		}
	}
	return m;
}

// Merge 6D Arrays
template< class A, typename T >
inline
Array6D< T >
merge( MArray6< A, T > const & a, Array6< T > const & b, Array6< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array6D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3 ) {
				for ( int ia4 = a.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4 ) {
					for ( int ia5 = a.l5(), ea5 = a.u5(); ia5 <= ea5; ++ia5 ) {
						for ( int ia6 = a.l6(), ea6 = a.u6(); ia6 <= ea6; ++ia6, ++l ) {
							m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4, ia5, ia6 ) : b[ l ] );
						}
					}
				}
			}
		}
	}
	return m;
}

// Merge 6D Arrays
template< class A, typename T >
inline
Array6D< T >
merge( Array6< T > const & a, MArray6< A, T > const & b, Array6< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( b ) );
	assert( a.conformable( mask ) );
	Array6D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3 ) {
				for ( int ib4 = b.l4(), eb4 = b.u4(); ib4 <= eb4; ++ib4 ) {
					for ( int ib5 = b.l5(), eb5 = b.u5(); ib5 <= eb5; ++ib5 ) {
						for ( int ib6 = b.l6(), eb6 = b.u6(); ib6 <= eb6; ++ib6, ++l ) {
							m[ l ] = ( mask[ l ] ? a[ l ] : b( ib1, ib2, ib3, ib4, ib5, ib6 ) );
						}
					}
				}
			}
		}
	}
	return m;
}

// Merge 6D Array + Scalar
template< class A, typename T >
inline
Array6D< T >
merge( MArray6< A, T > const & a, T const & b, Array6< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( a.size_bounded() );
	assert( a.conformable( mask ) );
	Array6D< T > m( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a.isize6() );
	size_type l( 0 );
	for ( int ia1 = a.l1(), ea1 = a.u1(); ia1 <= ea1; ++ia1 ) {
		for ( int ia2 = a.l2(), ea2 = a.u2(); ia2 <= ea2; ++ia2 ) {
			for ( int ia3 = a.l3(), ea3 = a.u3(); ia3 <= ea3; ++ia3 ) {
				for ( int ia4 = a.l4(), ea4 = a.u4(); ia4 <= ea4; ++ia4 ) {
					for ( int ia5 = a.l5(), ea5 = a.u5(); ia5 <= ea5; ++ia5 ) {
						for ( int ia6 = a.l6(), ea6 = a.u6(); ia6 <= ea6; ++ia6, ++l ) {
							m[ l ] = ( mask[ l ] ? a( ia1, ia2, ia3, ia4, ia5, ia6 ) : b );
						}
					}
				}
			}
		}
	}
	return m;
}

// Merge 6D Array + Scalar
template< class A, typename T >
inline
Array6D< T >
merge( T const & a, MArray6< A, T > const & b, Array6< bool > const & mask )
{
	typedef  BArray::size_type  size_type;
	assert( b.size_bounded() );
	assert( b.conformable( mask ) );
	Array6D< T > m( b.isize1(), b.isize2(), b.isize3(), b.isize4(), b.isize5(), b.isize6() );
	size_type l( 0 );
	for ( int ib1 = b.l1(), eb1 = b.u1(); ib1 <= eb1; ++ib1 ) {
		for ( int ib2 = b.l2(), eb2 = b.u2(); ib2 <= eb2; ++ib2 ) {
			for ( int ib3 = b.l3(), eb3 = b.u3(); ib3 <= eb3; ++ib3 ) {
				for ( int ib4 = b.l4(), eb4 = b.u4(); ib4 <= eb4; ++ib4 ) {
					for ( int ib5 = b.l5(), eb5 = b.u5(); ib5 <= eb5; ++ib5 ) {
						for ( int ib6 = b.l6(), eb6 = b.u6(); ib6 <= eb6; ++ib6, ++l ) {
							m[ l ] = ( mask[ l ] ? a : b( ib1, ib2, ib3, ib4, ib5, ib6 ) );
						}
					}
				}
			}
		}
	}
	return m;
}

// Subscript Array Generators /////

// Subscripted Array
template< class A, typename T >
inline
Array1D< T >
array_sub( MArray1< A, T > const & a, Array1< int > const & sub )
{
	assert( sub.size_bounded() );
	Array1D< T > r( sub.isize() );
	for ( int i = sub.l(), e = sub.u(), k = 1; i <= e; ++i, ++k ) {
		r( k ) = a( sub( i ) );
	}
	return r;
}

// Subscripted Array
template< class A, typename T >
inline
Array1D< T >
array_sub( MArray1< A, T > const & a, Array1S< int > const & sub )
{
	Array1D< T > r( sub.isize() );
	for ( int i = 1, e = sub.u(); i <= e; ++i ) {
		r( i ) = a( sub( i ) );
	}
	return r;
}

} // ObjexxFCL

#endif // ObjexxFCL_MArray_functions_hh_INCLUDED
