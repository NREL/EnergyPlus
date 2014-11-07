#ifndef ObjexxFCL_FArray4_hh_INCLUDED
#define ObjexxFCL_FArray4_hh_INCLUDED

// FArray4: Fortran-Compatible 4D Array Abstract Base Class
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
#include <ObjexxFCL/FArray4.fwd.hh>
#include <ObjexxFCL/FArray.hh>
#include <ObjexxFCL/FArray4S.hh>
#include <ObjexxFCL/MArray4.hh>

namespace ObjexxFCL {

// Forward
template< typename > class FArray4D;
template< typename > class FArray4P;
template< typename > class FArray4A;

// FArray4: Fortran-Compatible 4D Array Abstract Base Class
template< typename T >
class FArray4 : public FArray< T >
{

private: // Types

	typedef  FArray< T >  Super;
	typedef  FArray4D< T >  real_FArray;
	typedef  FArray4P< T >  proxy_FArray;
	typedef  FArray4A< T >  arg_FArray;

private: // Friend

	template< typename > friend class FArray4;
	template< typename > friend class FArray4D;
	template< typename > friend class FArray4P;
	template< typename > friend class FArray4A;

protected: // Types

	typedef  internal::InitializerSentinel  InitializerSentinel;
	typedef  internal::ProxySentinel  ProxySentinel;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Tail  Tail;
	typedef  typename Super::IR  IR;
	typedef  typename Super::IS  IS;
	typedef  typename Super::DS  DS;

	// STL Style
	typedef  typename Super::value_type  value_type;
	typedef  typename Super::reference  reference;
	typedef  typename Super::const_reference  const_reference;
	typedef  typename Super::pointer  pointer;
	typedef  typename Super::const_pointer  const_pointer;
	typedef  typename Super::size_type  size_type;
	typedef  typename Super::difference_type  difference_type;

	// C++ Style
	typedef  typename Super::Value  Value;
	typedef  typename Super::Reference  Reference;
	typedef  typename Super::ConstReference  ConstReference;
	typedef  typename Super::Pointer  Pointer;
	typedef  typename Super::ConstPointer  ConstPointer;
	typedef  typename Super::Size  Size;
	typedef  typename Super::Difference  Difference;

	using Super::dimensions_initialized;
	using Super::isize;
	using Super::npos;
	using Super::overlap;
	using Super::size;
	using Super::slice_k;
	using Super::swapB;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	using Super::not_const_proxy;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

protected: // Creation

	// Default Constructor
	inline
	FArray4() :
	 z1_( 0 ),
	 z2_( 0 ),
	 z3_( 0 )
	{}

	// Copy Constructor
	inline
	FArray4( FArray4 const & a ) :
	 Super( a ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray4( FArray4< U > const & a ) :
	 Super( a ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray4( FArray4S< U > const & a ) :
	 Super( a )
	{}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	FArray4( MArray4< A, M > const & a ) :
	 Super( a )
	{}

	// Size Constructor
	inline
	explicit
	FArray4( size_type const size ) :
	 Super( size )
	{}

	// Size + InitializerSentinel Constructor
	inline
	FArray4( size_type const size, InitializerSentinel const & initialized ) :
	 Super( size, initialized )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray4( std::initializer_list< U > const l ) :
	 Super( l )
	{}

	// Default Proxy Constructor
	inline
	FArray4( ProxySentinel const & proxy ) :
	 Super( proxy ),
	 z1_( 0 ),
	 z2_( 0 ),
	 z3_( 0 )
	{}

	// Copy Proxy Constructor
	inline
	FArray4( FArray4 const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Base Proxy Constructor
	inline
	FArray4( Base const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Tail Proxy Constructor
	inline
	FArray4( Tail const & s, ProxySentinel const & proxy ) :
	 Super( s, proxy )
	{}

	// Value Proxy Constructor
	inline
	FArray4( T const & t, ProxySentinel const & proxy ) :
	 Super( t, proxy )
	{}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Proxy Constructor
	inline
	FArray4( FArray4 & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Non-Const Base Proxy Constructor
	inline
	FArray4( Base & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Non-Const Tail Proxy Constructor
	inline
	FArray4( Tail & s, ProxySentinel const & proxy ) :
	 Super( s, proxy )
	{}

	// Non-Const Value Proxy Constructor
	inline
	FArray4( T & t, ProxySentinel const & proxy ) :
	 Super( t, proxy )
	{}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

public: // Creation

	// Destructor
	inline
	virtual
	~FArray4()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray4 &
	operator =( FArray4 const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3(), a.I4() );
			Super::operator =( a );
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator =( FArray4< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3(), a.I4() );
		Super::operator =( a );
		return *this;
	}

	// Slice Assignment
	inline
	FArray4 &
	operator =( FArray4S< T > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3(), a.I4() );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							c[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator =( FArray4S< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3(), a.I4() );
		size_type l( 0 );
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						data_[ l ] = a( i1, i2, i3, i4 );
					}
				}
			}
		}
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray4 &
	operator =( MArray4< A, M > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3(), a.I4() );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator =( std::initializer_list< U > const l )
	{
		Super::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator +=( FArray4< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator -=( FArray4< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator *=( FArray4< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator /=( FArray4< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator /=( a );
		return *this;
	}

	// += Slice
	inline
	FArray4 &
	operator +=( FArray4S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							c[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] += c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] += a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// -= Slice
	inline
	FArray4 &
	operator -=( FArray4S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							c[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] -= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] -= a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// *= Slice
	inline
	FArray4 &
	operator *=( FArray4S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							c[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] *= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] *= a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// /= Slice
	inline
	FArray4 &
	operator /=( FArray4S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							assert( a( i1, i2, i3, i4 ) != T( 0 ) );
							c[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] /= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							assert( a( i1, i2, i3, i4 ) != T( 0 ) );
							data_[ l ] /= a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator +=( FArray4S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						data_[ l ] += a( i1, i2, i3, i4 );
					}
				}
			}
		}
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator -=( FArray4S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						data_[ l ] -= a( i1, i2, i3, i4 );
					}
				}
			}
		}
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator *=( FArray4S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						data_[ l ] *= a( i1, i2, i3, i4 );
					}
				}
			}
		}
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	operator /=( FArray4S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						assert( T( a( i1, i2, i3, i4 ) ) != T( 0 ) );
						data_[ l ] /= a( i1, i2, i3, i4 );
					}
				}
			}
		}
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray4 &
	operator +=( MArray4< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] += a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray4 &
	operator -=( MArray4< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] -= a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray4 &
	operator *=( MArray4< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] *= a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray4 &
	operator /=( MArray4< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							assert( T( a( i1, i2, i3, i4 ) ) != T( 0 ) );
							data_[ l ] /= a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	and_equals( FArray4< U > const & a )
	{
		assert( conformable( a ) );
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	or_equals( FArray4< U > const & a )
	{
		assert( conformable( a ) );
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice
	inline
	FArray4 &
	and_equals( FArray4S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							c[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] && c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] = data_[ l ] && a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// ||= Slice
	inline
	FArray4 &
	or_equals( FArray4S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							c[ l ] = a( i1, i2, i3, i4 );
						}
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] || c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] = data_[ l ] || a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	and_equals( FArray4S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						data_[ l ] = data_[ l ] && a( i1, i2, i3, i4 );
					}
				}
			}
		}
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4 &
	or_equals( FArray4S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						data_[ l ] = data_[ l ] || a( i1, i2, i3, i4 );
					}
				}
			}
		}
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray4 &
	and_equals( MArray4< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] = data_[ l ] && a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray4 &
	or_equals( MArray4< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
						for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
							data_[ l ] = data_[ l ] || a( i1, i2, i3, i4 );
						}
					}
				}
			}
		}
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray4 &
	operator =( T const & t )
	{
		Super::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray4 &
	operator +=( T const & t )
	{
		Super::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray4 &
	operator -=( T const & t )
	{
		Super::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray4 &
	operator *=( T const & t )
	{
		Super::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray4 &
	operator /=( T const & t )
	{
		Super::operator /=( t );
		return *this;
	}

public: // Subscript

	// array( i1, i2, i3, i4 ) const
	inline
	T const &
	operator ()( int const i1, int const i2, int const i3, int const i4 ) const
	{
		assert( contains( i1, i2, i3, i4 ) );
		return sdata_[ ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ];
	}

	// array( i1, i2, i3, i4 )
	inline
	T &
	operator ()( int const i1, int const i2, int const i3, int const i4 )
	{
		proxy_const_assert( not_const_proxy() );
		assert( contains( i1, i2, i3, i4 ) );
		return sdata_[ ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ];
	}

	// Const Tail Starting at array( i1, i2, i3, i4 )
	inline
	Tail const
	a( int const i1, int const i2, int const i3, int const i4 ) const
	{
		assert( contains( i1, i2, i3, i4 ) );
		size_type const offset( ( ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Tail Starting at array( i1, i2, i3, i4 )
	inline
	Tail
	a( int const i1, int const i2, int const i3, int const i4 )
	{
		proxy_const_assert( not_const_proxy() );
		assert( contains( i1, i2, i3, i4 ) );
		size_type const offset( ( ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( data_ + offset, ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2, int const i3, int const i4 ) const
	{
		assert( dimensions_initialized() );
		return ( ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_;
	}

public: // Slice Proxy Generators

	// array( s1, s2, s3, s4 ) const
	inline
	FArray4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4 ) const
	{
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray4S< T >( data_, -shift_, d1, d2, d3, d4 );
	}

	// array( i1, s2, s3, s4 ) const
	inline
	FArray3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d2, d3, d4 );
	}

	// array( s1, i2, s3, s4 ) const
	inline
	FArray3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d1, d3, d4 );
	}

	// array( s1, s2, i3, s4 ) const
	inline
	FArray3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d1, d2, d4 );
	}

	// array( s1, s2, s3, i4 ) const
	inline
	FArray3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d1, d2, d3 );
	}

	// array( i1, i2, s3, s4 ) const
	inline
	FArray2S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d3, d4 );
	}

	// array( i1, s2, i3, s4 ) const
	inline
	FArray2S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d2, d4 );
	}

	// array( i1, s2, s3, i4 ) const
	inline
	FArray2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d2, d3 );
	}

	// array( s1, i2, i3, s4 ) const
	inline
	FArray2S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d1, d4 );
	}

	// array( s1, i2, s3, i4 ) const
	inline
	FArray2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d1, d3 );
	}

	// array( s1, s2, i3, i4 ) const
	inline
	FArray2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, i3, i4 ) const
	inline
	FArray1S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3, i4 ) const
	inline
	FArray1S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3, i4 ) const
	inline
	FArray1S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d3 );
	}

	// array( i1, i2, i3, s4 ) const
	inline
	FArray1S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		k += slice_k( I2(), i2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d4 );
	}

	// array( s1, s2, s3, s4 )
	inline
	FArray4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4 )
	{
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray4S< T >( data_, -shift_, d1, d2, d3, d4 );
	}

	// array( i1, s2, s3, s4 )
	inline
	FArray3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d2, d3, d4 );
	}

	// array( s1, i2, s3, s4 )
	inline
	FArray3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d1, d3, d4 );
	}

	// array( s1, s2, i3, s4 )
	inline
	FArray3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d1, d2, d4 );
	}

	// array( s1, s2, s3, i4 )
	inline
	FArray3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray3S< T >( data_, k, d1, d2, d3 );
	}

	// array( i1, i2, s3, s4 )
	inline
	FArray2S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d3, d4 );
	}

	// array( i1, s2, i3, s4 )
	inline
	FArray2S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d2, d4 );
	}

	// array( i1, s2, s3, i4 )
	inline
	FArray2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d2, d3 );
	}

	// array( s1, i2, i3, s4 )
	inline
	FArray2S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d1, d4 );
	}

	// array( s1, i2, s3, i4 )
	inline
	FArray2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d1, d3 );
	}

	// array( s1, s2, i3, i4 )
	inline
	FArray2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, i3, i4 )
	inline
	FArray1S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3, i4 )
	inline
	FArray1S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3, i4 )
	inline
	FArray1S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		k += slice_k( I2(), i2, z1_ );
		DS const d3( I3(), s3, z1_ * z2_ );
		k += slice_k( I4(), i4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d3 );
	}

	// array( i1, i2, i3, s4 )
	inline
	FArray1S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		k += slice_k( I2(), i2, z1_ );
		k += slice_k( I3(), i3, z1_ * z2_ );
		DS const d4( I4(), s4, z1_ * z2_ * z3_ );
		return FArray1S< T >( data_, k, d4 );
	}

public: // Predicate

	// contains( i )
	inline
	bool
	contains( int const i1, int const i2, int const i3, int const i4 ) const
	{
		return ( I1().contains( i1 ) && I2().contains( i2 ) && I3().contains( i3 ) && I4().contains( i4 ) );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( FArray4< U > const & a ) const
	{
		return ( ( size1() == a.size1() ) && ( size2() == a.size2() ) && ( size3() == a.size3() ) && ( size4() == a.size4() ) );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( FArray4S< U > const & a ) const
	{
		return ( ( size1() == a.size1() ) && ( size2() == a.size2() ) && ( size3() == a.size3() ) && ( size4() == a.size4() ) );
	}

	// Conformable?
	template< class A, typename M >
	inline
	bool
	conformable( MArray4< A, M > const & a ) const
	{
		return ( ( size1() == a.size1() ) && ( size2() == a.size2() ) && ( size3() == a.size3() ) && ( size4() == a.size4() ) );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( FArray4< U > const & a ) const
	{
		return ( ( I1() == a.I1() ) && ( I2() == a.I2() ) && ( I3() == a.I3() ) && ( I4() == a.I4() ) );
	}

public: // Inspector

	// Rank
	inline
	int
	rank() const
	{
		return 4;
	}

	// IndexRange of a Dimension
	inline
	IR const &
	I( int const d ) const
	{
		switch ( d ) {
		case 1:
			return I1();
		case 2:
			return I2();
		case 3:
			return I3();
		case 4:
			return I4();
		default:
			assert( false );
			return I1();
		}
	}

	// Lower Index of a Dimension
	inline
	int
	l( int const d ) const
	{
		switch ( d ) {
		case 1:
			return l1();
		case 2:
			return l2();
		case 3:
			return l3();
		case 4:
			return l4();
		default:
			assert( false );
			return l1();
		}
	}

	// Upper Index of a Dimension
	inline
	int
	u( int const d ) const
	{
		switch ( d ) {
		case 1:
			return u1();
		case 2:
			return u2();
		case 3:
			return u3();
		case 4:
			return u4();
		default:
			assert( false );
			return u1();
		}
	}

	// Size of a Dimension
	inline
	size_type
	size( int const d ) const
	{
		switch ( d ) {
		case 1:
			return size1();
		case 2:
			return size2();
		case 3:
			return size3();
		case 4:
			return size4();
		default:
			assert( false );
			return size1();
		}
	}

	// Size of a Dimension
	inline
	int
	isize( int const d ) const
	{
		switch ( d ) {
		case 1:
			return isize1();
		case 2:
			return isize2();
		case 3:
			return isize3();
		case 4:
			return isize4();
		default:
			assert( false );
			return isize1();
		}
	}

	// IndexRange of Dimension 1
	virtual
	IR const &
	I1() const = 0;

	// Lower Index of Dimension 1
	virtual
	int
	l1() const = 0;

	// Upper Index of Dimension 1
	virtual
	int
	u1() const = 0;

	// Size of Dimension 1
	inline
	size_type
	size1() const
	{
		return z1_;
	}

	// Size of Dimension 1
	inline
	int
	isize1() const
	{
		return static_cast< int >( z1_ );
	}

	// IndexRange of Dimension 2
	virtual
	IR const &
	I2() const = 0;

	// Lower Index of Dimension 2
	virtual
	int
	l2() const = 0;

	// Upper Index of Dimension 2
	virtual
	int
	u2() const = 0;

	// Size of Dimension 2
	inline
	size_type
	size2() const
	{
		return z2_;
	}

	// Size of Dimension 2
	inline
	int
	isize2() const
	{
		return static_cast< int >( z2_ );
	}

	// IndexRange of Dimension 3
	virtual
	IR const &
	I3() const = 0;

	// Lower Index of Dimension 3
	virtual
	int
	l3() const = 0;

	// Upper Index of Dimension 3
	virtual
	int
	u3() const = 0;

	// Size of Dimension 3
	inline
	size_type
	size3() const
	{
		return z3_;
	}

	// Size of Dimension 3
	inline
	int
	isize3() const
	{
		return static_cast< int >( z3_ );
	}

	// IndexRange of Dimension 4
	virtual
	IR const &
	I4() const = 0;

	// Lower Index of Dimension 4
	virtual
	int
	l4() const = 0;

	// Upper Index of Dimension 4
	virtual
	int
	u4() const = 0;

	// Size of Dimension 4
	virtual
	size_type
	size4() const = 0;

	// Size of Dimension 4
	virtual
	int
	isize4() const = 0;

public: // Modifier

	// Clear
	inline
	FArray4 &
	clear()
	{
		Super::clear();
		z1_ = z2_ = z3_ = 0;
		return *this;
	}

	// Assign Default Value to all Elements
	inline
	FArray4 &
	to_default()
	{
		Super::to_default();
		return *this;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	inline
	MArray4< FArray4 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray4< FArray4 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray4< FArray4, M >
	ma( M ClassT::* pmem )
	{
		return MArray4< FArray4, M >( *this, pmem );
	}

public: // Comparison: Predicate

	// FArray4 == FArray4
	inline
	friend
	bool
	eq( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 != FArray4
	inline
	friend
	bool
	ne( FArray4 const & a, FArray4 const & b )
	{
		return ! eq( a, b );
	}

	// FArray4 < FArray4
	inline
	friend
	bool
	lt( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 <= FArray4
	inline
	friend
	bool
	le( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 > FArray4
	inline
	friend
	bool
	gt( FArray4 const & a, FArray4 const & b )
	{
		return lt( b, a );
	}

	// FArray4 >= FArray4
	inline
	friend
	bool
	ge( FArray4 const & a, FArray4 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any

	// FArray4 == FArray4
	inline
	friend
	bool
	any_eq( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 != FArray4
	inline
	friend
	bool
	any_ne( FArray4 const & a, FArray4 const & b )
	{
		return ! eq( a, b );
	}

	// FArray4 < FArray4
	inline
	friend
	bool
	any_lt( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 <= FArray4
	inline
	friend
	bool
	any_le( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 > FArray4
	inline
	friend
	bool
	any_gt( FArray4 const & a, FArray4 const & b )
	{
		return any_lt( b, a );
	}

	// FArray4 >= FArray4
	inline
	friend
	bool
	any_ge( FArray4 const & a, FArray4 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All

	// FArray4 == FArray4
	inline
	friend
	bool
	all_eq( FArray4 const & a, FArray4 const & b )
	{
		return eq( a, b );
	}

	// FArray4 != FArray4
	inline
	friend
	bool
	all_ne( FArray4 const & a, FArray4 const & b )
	{
		return ! any_eq( a, b );
	}

	// FArray4 < FArray4
	inline
	friend
	bool
	all_lt( FArray4 const & a, FArray4 const & b )
	{
		return lt( a, b );
	}

	// FArray4 <= FArray4
	inline
	friend
	bool
	all_le( FArray4 const & a, FArray4 const & b )
	{
		return le( a, b );
	}

	// FArray4 > FArray4
	inline
	friend
	bool
	all_gt( FArray4 const & a, FArray4 const & b )
	{
		return gt( a, b );
	}

	// FArray4 >= FArray4
	inline
	friend
	bool
	all_ge( FArray4 const & a, FArray4 const & b )
	{
		return ge( a, b );
	}

public: // Comparison: Count

	// FArray4 == FArray4
	inline
	friend
	bool
	count_eq( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 != FArray4
	inline
	friend
	bool
	count_ne( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ne( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 < FArray4
	inline
	friend
	bool
	count_lt( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 <= FArray4
	inline
	friend
	bool
	count_le( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 > FArray4
	inline
	friend
	bool
	count_gt( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_gt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray4 >= FArray4
	inline
	friend
	bool
	count_ge( FArray4 const & a, FArray4 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ge( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

public: // Comparison: Predicate: Slice

	// FArray4 == FArray4S
	inline
	friend
	bool
	eq( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] == b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 != FArray4S
	inline
	friend
	bool
	ne( FArray4 const & a, FArray4S< T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray4 < FArray4S
	inline
	friend
	bool
	lt( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] < b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 <= FArray4S
	inline
	friend
	bool
	le( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] <= b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 > FArray4S
	inline
	friend
	bool
	gt( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] > b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 >= FArray4S
	inline
	friend
	bool
	ge( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] >= b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4S == FArray4
	inline
	friend
	bool
	eq( FArray4S< T > const & a, FArray4 const & b )
	{
		return eq( b, a );
	}

	// FArray4S != FArray4
	inline
	friend
	bool
	ne( FArray4S< T > const & a, FArray4 const & b )
	{
		return ne( b, a );
	}

	// FArray4S < FArray4
	inline
	friend
	bool
	lt( FArray4S< T > const & a, FArray4 const & b )
	{
		return gt( b, a );
	}

	// FArray4S <= FArray4
	inline
	friend
	bool
	le( FArray4S< T > const & a, FArray4 const & b )
	{
		return ge( b, a );
	}

	// FArray4S > FArray4
	inline
	friend
	bool
	gt( FArray4S< T > const & a, FArray4 const & b )
	{
		return lt( b, a );
	}

	// FArray4S >= FArray4
	inline
	friend
	bool
	ge( FArray4S< T > const & a, FArray4 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: Slice

	// Any FArray4 == FArray4S
	inline
	friend
	bool
	any_eq( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] == b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 != FArray4S
	inline
	friend
	bool
	any_ne( FArray4 const & a, FArray4S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray4 < FArray4S
	inline
	friend
	bool
	any_lt( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] < b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 <= FArray4S
	inline
	friend
	bool
	any_le( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] <= b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 > FArray4S
	inline
	friend
	bool
	any_gt( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] > b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 >= FArray4S
	inline
	friend
	bool
	any_ge( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] >= b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4S == FArray4
	inline
	friend
	bool
	any_eq( FArray4S< T > const & a, FArray4 const & b )
	{
		return any_eq( b, a );
	}

	// Any FArray4S != FArray4
	inline
	friend
	bool
	any_ne( FArray4S< T > const & a, FArray4 const & b )
	{
		return any_ne( b, a );
	}

	// Any FArray4S < FArray4
	inline
	friend
	bool
	any_lt( FArray4S< T > const & a, FArray4 const & b )
	{
		return any_gt( b, a );
	}

	// Any FArray4S <= FArray4
	inline
	friend
	bool
	any_le( FArray4S< T > const & a, FArray4 const & b )
	{
		return any_ge( b, a );
	}

	// Any FArray4S > FArray4
	inline
	friend
	bool
	any_gt( FArray4S< T > const & a, FArray4 const & b )
	{
		return any_lt( b, a );
	}

	// Any FArray4S >= FArray4
	inline
	friend
	bool
	any_ge( FArray4S< T > const & a, FArray4 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: Slice

	// All FArray4 == FArray4S
	inline
	friend
	bool
	all_eq( FArray4 const & a, FArray4S< T > const & b )
	{
		return eq( a, b );
	}

	// All FArray4 != FArray4S
	inline
	friend
	bool
	all_ne( FArray4 const & a, FArray4S< T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray4 < FArray4S
	inline
	friend
	bool
	all_lt( FArray4 const & a, FArray4S< T > const & b )
	{
		return lt( a, b );
	}

	// All FArray4 <= FArray4S
	inline
	friend
	bool
	all_le( FArray4 const & a, FArray4S< T > const & b )
	{
		return le( a, b );
	}

	// All FArray4 > FArray4S
	inline
	friend
	bool
	all_gt( FArray4 const & a, FArray4S< T > const & b )
	{
		return gt( a, b );
	}

	// All FArray4 >= FArray4S
	inline
	friend
	bool
	all_ge( FArray4 const & a, FArray4S< T > const & b )
	{
		return ge( a, b );
	}

	// All FArray4S == FArray4
	inline
	friend
	bool
	all_eq( FArray4S< T > const & a, FArray4 const & b )
	{
		return all_eq( b, a );
	}

	// All FArray4S != FArray4
	inline
	friend
	bool
	all_ne( FArray4S< T > const & a, FArray4 const & b )
	{
		return all_ne( b, a );
	}

	// All FArray4S < FArray4
	inline
	friend
	bool
	all_lt( FArray4S< T > const & a, FArray4 const & b )
	{
		return all_gt( b, a );
	}

	// All FArray4S <= FArray4
	inline
	friend
	bool
	all_le( FArray4S< T > const & a, FArray4 const & b )
	{
		return all_ge( b, a );
	}

	// All FArray4S > FArray4
	inline
	friend
	bool
	all_gt( FArray4S< T > const & a, FArray4 const & b )
	{
		return all_lt( b, a );
	}

	// All FArray4S >= FArray4
	inline
	friend
	bool
	all_ge( FArray4S< T > const & a, FArray4 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: Slice

	// Count FArray4 == FArray4S
	inline
	friend
	size_type
	count_eq( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] == b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 != FArray4S
	inline
	friend
	size_type
	count_ne( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] != b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 < FArray4S
	inline
	friend
	size_type
	count_lt( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] < b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 <= FArray4S
	inline
	friend
	size_type
	count_le( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] <= b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 > FArray4S
	inline
	friend
	size_type
	count_gt( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] > b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 >= FArray4S
	inline
	friend
	size_type
	count_ge( FArray4 const & a, FArray4S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] >= b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4S == FArray4
	inline
	friend
	size_type
	count_eq( FArray4S< T > const & a, FArray4 const & b )
	{
		return count_eq( b, a );
	}

	// Count FArray4S != FArray4
	inline
	friend
	size_type
	count_ne( FArray4S< T > const & a, FArray4 const & b )
	{
		return count_ne( b, a );
	}

	// Count FArray4S < FArray4
	inline
	friend
	size_type
	count_lt( FArray4S< T > const & a, FArray4 const & b )
	{
		return count_gt( b, a );
	}

	// Count FArray4S <= FArray4
	inline
	friend
	size_type
	count_le( FArray4S< T > const & a, FArray4 const & b )
	{
		return count_ge( b, a );
	}

	// Count FArray4S > FArray4
	inline
	friend
	size_type
	count_gt( FArray4S< T > const & a, FArray4 const & b )
	{
		return count_lt( b, a );
	}

	// Count FArray4S >= FArray4
	inline
	friend
	size_type
	count_ge( FArray4S< T > const & a, FArray4 const & b )
	{
		return count_le( b, a );
	}

public: // Comparison: Predicate: MArray

	// FArray4 == MArray4
	template< class A >
	inline
	friend
	bool
	eq( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] == b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 != MArray4
	template< class A >
	inline
	friend
	bool
	ne( FArray4 const & a, MArray4< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray4 < MArray4
	template< class A >
	inline
	friend
	bool
	lt( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] < b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 <= MArray4
	template< class A >
	inline
	friend
	bool
	le( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] <= b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 > MArray4
	template< class A >
	inline
	friend
	bool
	gt( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] > b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// FArray4 >= MArray4
	template< class A >
	inline
	friend
	bool
	ge( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( ! ( a[ l ] >= b( i1, i2, i3, i4 ) ) ) return false;
					}
				}
			}
		}
		return true;
	}

	// MArray4 == FArray4
	template< class A >
	inline
	friend
	bool
	eq( MArray4< A, T > const & a, FArray4 const & b )
	{
		return eq( b, a );
	}

	// MArray4 != FArray4
	template< class A >
	inline
	friend
	bool
	ne( MArray4< A, T > const & a, FArray4 const & b )
	{
		return ne( b, a );
	}

	// MArray4 < FArray4
	template< class A >
	inline
	friend
	bool
	lt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return gt( b, a );
	}

	// MArray4 <= FArray4
	template< class A >
	inline
	friend
	bool
	le( MArray4< A, T > const & a, FArray4 const & b )
	{
		return ge( b, a );
	}

	// MArray4 > FArray4
	template< class A >
	inline
	friend
	bool
	gt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return lt( b, a );
	}

	// MArray4 >= FArray4
	template< class A >
	inline
	friend
	bool
	ge( MArray4< A, T > const & a, FArray4 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any FArray4 == MArray4
	template< class A >
	inline
	friend
	bool
	any_eq( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] == b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 != MArray4
	template< class A >
	inline
	friend
	bool
	any_ne( FArray4 const & a, MArray4< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray4 < MArray4
	template< class A >
	inline
	friend
	bool
	any_lt( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] < b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 <= MArray4
	template< class A >
	inline
	friend
	bool
	any_le( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] <= b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 > MArray4
	template< class A >
	inline
	friend
	bool
	any_gt( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] > b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any FArray4 >= MArray4
	template< class A >
	inline
	friend
	bool
	any_ge( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] >= b( i1, i2, i3, i4 ) ) return true;
					}
				}
			}
		}
		return false;
	}

	// Any MArray4 == FArray4
	template< class A >
	inline
	friend
	bool
	any_eq( MArray4< A, T > const & a, FArray4 const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray4 != FArray4
	template< class A >
	inline
	friend
	bool
	any_ne( MArray4< A, T > const & a, FArray4 const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray4 < FArray4
	template< class A >
	inline
	friend
	bool
	any_lt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray4 <= FArray4
	template< class A >
	inline
	friend
	bool
	any_le( MArray4< A, T > const & a, FArray4 const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray4 > FArray4
	template< class A >
	inline
	friend
	bool
	any_gt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray4 >= FArray4
	template< class A >
	inline
	friend
	bool
	any_ge( MArray4< A, T > const & a, FArray4 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All FArray4 == MArray4
	template< class A >
	inline
	friend
	bool
	all_eq( FArray4 const & a, MArray4< A, T > const & b )
	{
		return eq( a, b );
	}

	// All FArray4 != MArray4
	template< class A >
	inline
	friend
	bool
	all_ne( FArray4 const & a, MArray4< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray4 < MArray4
	template< class A >
	inline
	friend
	bool
	all_lt( FArray4 const & a, MArray4< A, T > const & b )
	{
		return lt( a, b );
	}

	// All FArray4 <= MArray4
	template< class A >
	inline
	friend
	bool
	all_le( FArray4 const & a, MArray4< A, T > const & b )
	{
		return le( a, b );
	}

	// All FArray4 > MArray4
	template< class A >
	inline
	friend
	bool
	all_gt( FArray4 const & a, MArray4< A, T > const & b )
	{
		return gt( a, b );
	}

	// All FArray4 >= MArray4
	template< class A >
	inline
	friend
	bool
	all_ge( FArray4 const & a, MArray4< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray4 == FArray4
	template< class A >
	inline
	friend
	bool
	all_eq( MArray4< A, T > const & a, FArray4 const & b )
	{
		return all_eq( b, a );
	}

	// All MArray4 != FArray4
	template< class A >
	inline
	friend
	bool
	all_ne( MArray4< A, T > const & a, FArray4 const & b )
	{
		return all_ne( b, a );
	}

	// All MArray4 < FArray4
	template< class A >
	inline
	friend
	bool
	all_lt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return all_gt( b, a );
	}

	// All MArray4 <= FArray4
	template< class A >
	inline
	friend
	bool
	all_le( MArray4< A, T > const & a, FArray4 const & b )
	{
		return all_ge( b, a );
	}

	// All MArray4 > FArray4
	template< class A >
	inline
	friend
	bool
	all_gt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return all_lt( b, a );
	}

	// All MArray4 >= FArray4
	template< class A >
	inline
	friend
	bool
	all_ge( MArray4< A, T > const & a, FArray4 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count FArray4 == MArray4
	template< class A >
	inline
	friend
	size_type
	count_eq( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] == b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 != MArray4
	template< class A >
	inline
	friend
	size_type
	count_ne( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] != b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 < MArray4
	template< class A >
	inline
	friend
	size_type
	count_lt( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] < b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 <= MArray4
	template< class A >
	inline
	friend
	size_type
	count_le( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] <= b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 > MArray4
	template< class A >
	inline
	friend
	size_type
	count_gt( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] > b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count FArray4 >= MArray4
	template< class A >
	inline
	friend
	size_type
	count_ge( FArray4 const & a, MArray4< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i4 = 1, e4 = b.u4(); i4 <= e4; ++i4 ) {
			for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3 ) {
				for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
						if ( a[ l ] >= b( i1, i2, i3, i4 ) ) ++n;
					}
				}
			}
		}
		return n;
	}

	// Count MArray4 == FArray4
	template< class A >
	inline
	friend
	size_type
	count_eq( MArray4< A, T > const & a, FArray4 const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray4 != FArray4
	template< class A >
	inline
	friend
	size_type
	count_ne( MArray4< A, T > const & a, FArray4 const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray4 < FArray4
	template< class A >
	inline
	friend
	size_type
	count_lt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray4 <= FArray4
	template< class A >
	inline
	friend
	size_type
	count_le( MArray4< A, T > const & a, FArray4 const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray4 > FArray4
	template< class A >
	inline
	friend
	size_type
	count_gt( MArray4< A, T > const & a, FArray4 const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray4 >= FArray4
	template< class A >
	inline
	friend
	size_type
	count_ge( MArray4< A, T > const & a, FArray4 const & b )
	{
		return count_le( b, a );
	}

protected: // Functions

	// Dimension by IndexRange
	virtual
	void
	dimension_assign( IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) = 0;

	// Swap
	inline
	void
	swap4DB( FArray4 & v )
	{
		swapB( v );
		std::swap( z1_, v.z1_ );
		std::swap( z2_, v.z2_ );
		std::swap( z3_, v.z3_ );
	}

protected: // Data

	size_type z1_; // Size of dim 1
	size_type z2_; // Size of dim 2
	size_type z3_; // Size of dim 3

}; // FArray4

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray4< U > const & a, FArray4< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray4< U > const & a, FArray4S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray4S< U > const & a, FArray4< V > const & b )
{
	return b.conformable( a );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( FArray4< U > const & a, MArray4< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray4< A, M > const & a, FArray4< V > const & b )
{
	return b.conformable( a );
}

// Equal Dimensions?
template< typename U, typename V >
inline
bool
equal_dimensions( FArray4< U > const & a, FArray4< V > const & b )
{
	return a.equal_dimensions( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_FArray4_hh_INCLUDED
