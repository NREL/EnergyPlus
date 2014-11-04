#ifndef ObjexxFCL_FArray2_hh_INCLUDED
#define ObjexxFCL_FArray2_hh_INCLUDED

// FArray2: Fortran-Compatible 2D Array Abstract Base Class
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
#include <ObjexxFCL/FArray2.fwd.hh>
#include <ObjexxFCL/FArray.hh>
#include <ObjexxFCL/FArray2S.hh>
#include <ObjexxFCL/MArray2.hh>

namespace ObjexxFCL {

// Forward
template< typename > class FArray1D; // For project-specific member array methods
template< typename > class FArray2D;
template< typename > class FArray2P;
template< typename > class FArray2A;

// FArray2: Fortran-Compatible 2D Array Abstract Base Class
template< typename T >
class FArray2 : public FArray< T >
{

private: // Types

	typedef  FArray< T >  Super;
	typedef  FArray2D< T >  real_FArray;
	typedef  FArray2P< T >  proxy_FArray;
	typedef  FArray2A< T >  arg_FArray;

private: // Friend

	template< typename > friend class FArray2;
	template< typename > friend class FArray2D;
	template< typename > friend class FArray2P;
	template< typename > friend class FArray2A;

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
	FArray2() :
	 z1_( 0 )
	{}

	// Copy Constructor
	inline
	FArray2( FArray2 const & a ) :
	 Super( a ),
	 z1_( a.z1_ )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray2( FArray2< U > const & a ) :
	 Super( a ),
	 z1_( a.z1_ )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray2( FArray2S< U > const & a ) :
	 Super( a )
	{}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	FArray2( MArray2< A, M > const & a ) :
	 Super( a )
	{}

	// Size Constructor
	inline
	explicit
	FArray2( size_type const size ) :
	 Super( size )
	{}

	// Size + InitializerSentinel Constructor
	inline
	FArray2( size_type const size, InitializerSentinel const & initialized ) :
	 Super( size, initialized )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2( std::initializer_list< U > const l ) :
	 Super( l )
	{}

	// Default Proxy Constructor
	inline
	FArray2( ProxySentinel const & proxy ) :
	 Super( proxy ),
	 z1_( 0 )
	{}

	// Copy Proxy Constructor
	inline
	FArray2( FArray2 const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Base Proxy Constructor
	inline
	FArray2( Base const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Tail Proxy Constructor
	inline
	FArray2( Tail const & s, ProxySentinel const & proxy ) :
	 Super( s, proxy )
	{}

	// Value Proxy Constructor
	inline
	FArray2( T const & t, ProxySentinel const & proxy ) :
	 Super( t, proxy )
	{}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Proxy Constructor
	inline
	FArray2( FArray2 & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Non-Const Base Proxy Constructor
	inline
	FArray2( Base & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Non-Const Tail Proxy Constructor
	inline
	FArray2( Tail & s, ProxySentinel const & proxy ) :
	 Super( s, proxy )
	{}

	// Non-Const Value Proxy Constructor
	inline
	FArray2( T & t, ProxySentinel const & proxy ) :
	 Super( t, proxy )
	{}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

public: // Creation

	// Destructor
	inline
	virtual
	~FArray2()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray2 &
	operator =( FArray2 const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2() );
			Super::operator =( a );
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator =( FArray2< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2() );
		Super::operator =( a );
		return *this;
	}

	// Slice Assignment
	inline
	FArray2 &
	operator =( FArray2S< T > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2() );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] = a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator =( FArray2S< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2() );
		size_type l( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				data_[ l ] = a( i1, i2 );
			}
		}
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray2 &
	operator =( MArray2< A, M > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2() );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] = a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator =( std::initializer_list< U > const l )
	{
		Super::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator +=( FArray2< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator -=( FArray2< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator *=( FArray2< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator /=( FArray2< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator /=( a );
		return *this;
	}

	// += Slice
	inline
	FArray2 &
	operator +=( FArray2S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] += c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] += a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// -= Slice
	inline
	FArray2 &
	operator -=( FArray2S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] -= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] -= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// *= Slice
	inline
	FArray2 &
	operator *=( FArray2S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] *= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] *= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// /= Slice
	inline
	FArray2 &
	operator /=( FArray2S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					assert( a( i1, i2 ) != T( 0 ) );
					c[ l ] = a( i1, i2 );
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] /= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					assert( a( i1, i2 ) != T( 0 ) );
					data_[ l ] /= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator +=( FArray2S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				data_[ l ] += a( i1, i2 );
			}
		}
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator -=( FArray2S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				data_[ l ] -= a( i1, i2 );
			}
		}
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator *=( FArray2S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				data_[ l ] *= a( i1, i2 );
			}
		}
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	operator /=( FArray2S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				assert( T( a( i1, i2 ) ) != T( 0 ) );
				data_[ l ] /= a( i1, i2 );
			}
		}
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray2 &
	operator +=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] += a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray2 &
	operator -=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] -= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray2 &
	operator *=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] *= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray2 &
	operator /=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					assert( T( a( i1, i2 ) ) != T( 0 ) );
					data_[ l ] /= a( i1, i2 );
				}
			}
		}
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	and_equals( FArray2< U > const & a )
	{
		assert( conformable( a ) );
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	or_equals( FArray2< U > const & a )
	{
		assert( conformable( a ) );
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice
	inline
	FArray2 &
	and_equals( FArray2S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] && c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] = data_[ l ] && a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// ||= Slice
	inline
	FArray2 &
	or_equals( FArray2S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] || c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] = data_[ l ] || a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	and_equals( FArray2S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				data_[ l ] = data_[ l ] && a( i1, i2 );
			}
		}
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	or_equals( FArray2S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
				data_[ l ] = data_[ l ] || a( i1, i2 );
			}
		}
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray2 &
	and_equals( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] = data_[ l ] && a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray2 &
	or_equals( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					data_[ l ] = data_[ l ] || a( i1, i2 );
				}
			}
		}
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray2 &
	operator =( T const & t )
	{
		Super::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray2 &
	operator +=( T const & t )
	{
		Super::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray2 &
	operator -=( T const & t )
	{
		Super::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray2 &
	operator *=( T const & t )
	{
		Super::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray2 &
	operator /=( T const & t )
	{
		Super::operator /=( t );
		return *this;
	}

public: // Subscript

	// array( i1, i2 ) const
	inline
	T const &
	operator ()( int const i1, int const i2 ) const
	{
		assert( contains( i1, i2 ) );
		return sdata_[ ( i2 * z1_ ) + i1 ];
	}

	// array( i1, i2 )
	inline
	T &
	operator ()( int const i1, int const i2 )
	{
		proxy_const_assert( not_const_proxy() );
		assert( contains( i1, i2 ) );
		return sdata_[ ( i2 * z1_ ) + i1 ];
	}

	// Const Tail Starting at array( i1, i2 )
	inline
	Tail const
	a( int const i1, int const i2 ) const
	{
		assert( contains( i1, i2 ) );
		size_type const offset( ( ( i2 * z1_ ) + i1 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Tail Starting at array( i1, i2 )
	inline
	Tail
	a( int const i1, int const i2 )
	{
		proxy_const_assert( not_const_proxy() );
		assert( contains( i1, i2 ) );
		size_type const offset( ( ( i2 * z1_ ) + i1 ) - shift_ );
		return Tail( data_ + offset, ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2 ) const
	{
		assert( dimensions_initialized() );
		return ( ( i2 * z1_ ) + i1 ) - shift_;
	}

public: // Slice Proxy Generators

	// array( s1, s2 ) const
	inline
	FArray2S< T >
	operator ()( IS const & s1, IS const & s2 ) const
	{
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		return FArray2S< T >( data_, -shift_, d1, d2 );
	}

	// array( i1, s2 ) const
	inline
	FArray1S< T >
	operator ()( int const i1, IS const & s2 ) const
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		return FArray1S< T >( data_, k, d2 );
	}

	// array( s1, i2 ) const
	inline
	FArray1S< T >
	operator ()( IS const & s1, int const i2 ) const
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		return FArray1S< T >( data_, k, d1 );
	}

	// array( s1, s2 )
	inline
	FArray2S< T >
	operator ()( IS const & s1, IS const & s2 )
	{
		DS const d1( I1(), s1 );
		DS const d2( I2(), s2, z1_ );
		return FArray2S< T >( data_, -shift_, d1, d2 );
	}

	// array( i1, s2 )
	inline
	FArray1S< T >
	operator ()( int const i1, IS const & s2 )
	{
		int k( -shift_ );
		k += slice_k( I1(), i1 );
		DS const d2( I2(), s2, z1_ );
		return FArray1S< T >( data_, k, d2 );
	}

	// array( s1, i2 )
	inline
	FArray1S< T >
	operator ()( IS const & s1, int const i2 )
	{
		int k( -shift_ );
		DS const d1( I1(), s1 );
		k += slice_k( I2(), i2, z1_ );
		return FArray1S< T >( data_, k, d1 );
	}

public: // Predicate

	// contains( i )
	inline
	bool
	contains( int const i1, int const i2 ) const
	{
		return ( I1().contains( i1 ) && I2().contains( i2 ) );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( FArray2< U > const & a ) const
	{
		return ( ( size1() == a.size1() ) && ( size2() == a.size2() ) );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( FArray2S< U > const & a ) const
	{
		return ( ( size1() == a.size1() ) && ( size2() == a.size2() ) );
	}

	// Conformable?
	template< class A, typename M >
	inline
	bool
	conformable( MArray2< A, M > const & a ) const
	{
		return ( ( size1() == a.size1() ) && ( size2() == a.size2() ) );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( FArray2< U > const & a ) const
	{
		return ( ( I1() == a.I1() ) && ( I2() == a.I2() ) );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( FArray2S< U > const & a ) const
	{
		return ( ( l1() == 1 ) && ( u1() == a.u1() ) && ( l2() == 1 ) && ( u2() == a.u2() ) );
	}

	// Equal Dimensions?
	template< class A, typename M >
	inline
	bool
	equal_dimensions( MArray2< A, M > const & a ) const
	{
		return ( ( l1() == 1 ) && ( u1() == a.u1() ) && ( l2() == 1 ) && ( u2() == a.u2() ) );
	}

	// Is Identity?
	inline
	bool
	is_identity() const
	{
		static T const ZERO( 0 );
		static T const ONE( 1 );
		FArray2 const & A( *this ); // Shorthand name
		if ( ! square() ) { // Non-square
			return false;
		} else { // Square
			for ( int j = l2(), je = u2(); j <= je; ++j ) {
				int const id( l1() + ( j - l2() ) ); // Row index of diagonal
				for ( int i = l1(), ie = u1(); i <= ie; ++i ) {
					if ( A( i, j ) != ( i == id ? ONE : ZERO ) ) return false;
				}
			}
			return true;
		}
	}

	// Square?
	inline
	bool
	square() const
	{
		return ( ( dimensions_initialized() ) && ( z1_ == size2() ) );
	}

	// Symmetric?
	inline
	bool
	symmetric() const
	{
		FArray2 const & A( *this ); // Shorthand name
		if ( I1() != I2() ) { // Unequal index ranges
			return false;
		} else { // Equal index ranges
			for ( int i = l1(), ie = u1(); i <= ie; ++i ) {
				for ( int j = l2(), je = i - 1; j <= je; ++j ) {
					if ( A( i, j ) != A( j, i ) ) return false;
				}
			}
			return true;
		}
	}

public: // Inspector

	// Rank
	inline
	int
	rank() const
	{
		return 2;
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
	virtual
	size_type
	size2() const = 0;

	// Size of Dimension 2
	virtual
	int
	isize2() const = 0;

public: // Modifier

	// Clear
	inline
	FArray2 &
	clear()
	{
		Super::clear();
		z1_ = 0;
		return *this;
	}

	// Assign Default Value to all Elements
	inline
	FArray2 &
	to_default()
	{
		Super::to_default();
		return *this;
	}

	// Set to the Identity Matrix
	inline
	FArray2 &
	to_identity()
	{
		proxy_const_assert( not_const_proxy() );
		assert( square() );
		FArray2 & A( *this ); // Shorthand name
		A = T( 0 ); // Zero the array
		T const One( T( 1 ) );
		for ( int i = l1(), j = l2(), e = u1(); i <= e; ++i, ++j ) { // Set diagonal to unity
			A( i, j ) = One;
		}
		return *this;
	}

	// Set to Diagonal Matrix with Uniform Value
	inline
	FArray2 &
	to_diag( T const & d )
	{
		proxy_const_assert( not_const_proxy() );
		assert( square() );
		FArray2 & A( *this ); // Shorthand name
		A = T( 0 ); // Zero the array
		for ( int i = l1(), j = l2(), e = u1(); i <= e; ++i, ++j ) { // Set diagonal value
			A( i, j ) = d;
		}
		return *this;
	}

	// Set Diagonal of Matrix to a Uniform Value
	inline
	FArray2 &
	set_diagonal( T const & d )
	{
		proxy_const_assert( not_const_proxy() );
		assert( square() );
		FArray2 & A( *this ); // Shorthand name
		for ( int i = l1(), j = l2(), e = u1(); i <= e; ++i, ++j ) { // Set diagonal value
			A( i, j ) = d;
		}
		return *this;
	}

	// Transpose
	inline
	FArray2 &
	transpose()
	{
		proxy_const_assert( not_const_proxy() );
		assert( square() ); // So dimensions aren't changed
		FArray2 & A( *this ); // Shorthand name
		int const l_off( l2() - l1() );
		for ( int i = l1(), ie = u1(); i <= ie; ++i ) {
			for ( int j = l2(), je = i + l_off; j < je; ++j ) {
				T const A_ij( A( i, j ) );
				A( i, j ) = A( j, i );
				A( j, i ) = A_ij;
			}
		}
		return *this;
	}

	// Right Multiply By Array
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	right_multiply_by( FArray2< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		size_type const s1( z1_ );
		size_type const s2( size2() );
		size_type const s( s1 * s2 );
		size_type const as1( a.size1() );
		size_type const as2( a.size2() );
		assert( s2 == as1 );
		assert( as1 == as2 ); // Square so that this array's dimensions aren't changed
		FArray2 & t( *this ); // Shorthand name for this array
		T * const r( new T[ s2 ] ); // Temporary row
		for ( size_type i = 0; i < s1; ++i ) {
			for ( size_type j = 0, la = 0, l = 0; j < as2; ++j, ++l ) {
				T d( 0 );
				for ( size_type lt = i; lt < s; lt += s1, ++la ) {
					d += t[ lt ] * a[ la ];
				}
				r[ l ] = d;
			}
			for ( size_type l = 0, lt = i; l < s2; ++l, lt += s1 ) { // Copy in the new row
				t[ lt ] = r[ l ];
			}
		}
		delete[] r;
		return *this;
	}

	// Right Multiply By Transpose of Array
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2 &
	right_multiply_by_transpose( FArray2< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		size_type const s1( z1_ );
		size_type const s2( size2() );
		size_type const s( s1 * s2 );
		size_type const as1( a.size1() );
		size_type const as2( a.size2() );
		assert( s2 == as2 );
		assert( as1 == as2 ); // Square so that this array's dimensions aren't changed
		FArray2 & t( *this ); // Shorthand name for this array
		T * const r( new T[ s2 ] ); // Temporary row
		for ( size_type i = 0; i < s1; ++i ) {
			for ( size_type j = 0, l = 0; j < as1; ++j, ++l ) {
				T d( 0 );
				for ( size_type lt = i, la = j; lt < s; lt += s1, la += as1 ) {
					d += t[ lt ] * a[ la ];
				}
				r[ l ] = d;
			}
			for ( size_type l = 0, lt = i; l < s2; ++l, lt += s1 ) { // Copy in the new row
				t[ lt ] = r[ l ];
			}
		}
		delete[] r;
		return *this;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	inline
	MArray2< FArray2 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray2< FArray2 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray2< FArray2, M >
	ma( M ClassT::* pmem )
	{
		return MArray2< FArray2, M >( *this, pmem );
	}

#include <ObjexxFCL/FArray2.Project.MArray.hh> // Inject project-specific MArray generators

public: // Comparison: Predicate

	// FArray2 == FArray2
	inline
	friend
	bool
	eq( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 != FArray2
	inline
	friend
	bool
	ne( FArray2 const & a, FArray2 const & b )
	{
		return ! eq( a, b );
	}

	// FArray2 < FArray2
	inline
	friend
	bool
	lt( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 <= FArray2
	inline
	friend
	bool
	le( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 > FArray2
	inline
	friend
	bool
	gt( FArray2 const & a, FArray2 const & b )
	{
		return lt( b, a );
	}

	// FArray2 >= FArray2
	inline
	friend
	bool
	ge( FArray2 const & a, FArray2 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any

	// FArray2 == FArray2
	inline
	friend
	bool
	any_eq( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 != FArray2
	inline
	friend
	bool
	any_ne( FArray2 const & a, FArray2 const & b )
	{
		return ! eq( a, b );
	}

	// FArray2 < FArray2
	inline
	friend
	bool
	any_lt( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 <= FArray2
	inline
	friend
	bool
	any_le( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 > FArray2
	inline
	friend
	bool
	any_gt( FArray2 const & a, FArray2 const & b )
	{
		return any_lt( b, a );
	}

	// FArray2 >= FArray2
	inline
	friend
	bool
	any_ge( FArray2 const & a, FArray2 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All

	// FArray2 == FArray2
	inline
	friend
	bool
	all_eq( FArray2 const & a, FArray2 const & b )
	{
		return eq( a, b );
	}

	// FArray2 != FArray2
	inline
	friend
	bool
	all_ne( FArray2 const & a, FArray2 const & b )
	{
		return ! any_eq( a, b );
	}

	// FArray2 < FArray2
	inline
	friend
	bool
	all_lt( FArray2 const & a, FArray2 const & b )
	{
		return lt( a, b );
	}

	// FArray2 <= FArray2
	inline
	friend
	bool
	all_le( FArray2 const & a, FArray2 const & b )
	{
		return le( a, b );
	}

	// FArray2 > FArray2
	inline
	friend
	bool
	all_gt( FArray2 const & a, FArray2 const & b )
	{
		return gt( a, b );
	}

	// FArray2 >= FArray2
	inline
	friend
	bool
	all_ge( FArray2 const & a, FArray2 const & b )
	{
		return ge( a, b );
	}

public: // Comparison: Count

	// FArray2 == FArray2
	inline
	friend
	bool
	count_eq( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 != FArray2
	inline
	friend
	bool
	count_ne( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ne( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 < FArray2
	inline
	friend
	bool
	count_lt( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 <= FArray2
	inline
	friend
	bool
	count_le( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 > FArray2
	inline
	friend
	bool
	count_gt( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_gt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray2 >= FArray2
	inline
	friend
	bool
	count_ge( FArray2 const & a, FArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ge( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

public: // Comparison: Predicate: Slice

	// FArray2 == FArray2S
	inline
	friend
	bool
	eq( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] == b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 != FArray2S
	inline
	friend
	bool
	ne( FArray2 const & a, FArray2S< T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray2 < FArray2S
	inline
	friend
	bool
	lt( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] < b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 <= FArray2S
	inline
	friend
	bool
	le( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] <= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 > FArray2S
	inline
	friend
	bool
	gt( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] > b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 >= FArray2S
	inline
	friend
	bool
	ge( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] >= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2S == FArray2
	inline
	friend
	bool
	eq( FArray2S< T > const & a, FArray2 const & b )
	{
		return eq( b, a );
	}

	// FArray2S != FArray2
	inline
	friend
	bool
	ne( FArray2S< T > const & a, FArray2 const & b )
	{
		return ne( b, a );
	}

	// FArray2S < FArray2
	inline
	friend
	bool
	lt( FArray2S< T > const & a, FArray2 const & b )
	{
		return gt( b, a );
	}

	// FArray2S <= FArray2
	inline
	friend
	bool
	le( FArray2S< T > const & a, FArray2 const & b )
	{
		return ge( b, a );
	}

	// FArray2S > FArray2
	inline
	friend
	bool
	gt( FArray2S< T > const & a, FArray2 const & b )
	{
		return lt( b, a );
	}

	// FArray2S >= FArray2
	inline
	friend
	bool
	ge( FArray2S< T > const & a, FArray2 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: Slice

	// Any FArray2 == FArray2S
	inline
	friend
	bool
	any_eq( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] == b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 != FArray2S
	inline
	friend
	bool
	any_ne( FArray2 const & a, FArray2S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray2 < FArray2S
	inline
	friend
	bool
	any_lt( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] < b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 <= FArray2S
	inline
	friend
	bool
	any_le( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] <= b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 > FArray2S
	inline
	friend
	bool
	any_gt( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] > b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 >= FArray2S
	inline
	friend
	bool
	any_ge( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] >= b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2S == FArray2
	inline
	friend
	bool
	any_eq( FArray2S< T > const & a, FArray2 const & b )
	{
		return any_eq( b, a );
	}

	// Any FArray2S != FArray2
	inline
	friend
	bool
	any_ne( FArray2S< T > const & a, FArray2 const & b )
	{
		return any_ne( b, a );
	}

	// Any FArray2S < FArray2
	inline
	friend
	bool
	any_lt( FArray2S< T > const & a, FArray2 const & b )
	{
		return any_gt( b, a );
	}

	// Any FArray2S <= FArray2
	inline
	friend
	bool
	any_le( FArray2S< T > const & a, FArray2 const & b )
	{
		return any_ge( b, a );
	}

	// Any FArray2S > FArray2
	inline
	friend
	bool
	any_gt( FArray2S< T > const & a, FArray2 const & b )
	{
		return any_lt( b, a );
	}

	// Any FArray2S >= FArray2
	inline
	friend
	bool
	any_ge( FArray2S< T > const & a, FArray2 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: Slice

	// All FArray2 == FArray2S
	inline
	friend
	bool
	all_eq( FArray2 const & a, FArray2S< T > const & b )
	{
		return eq( a, b );
	}

	// All FArray2 != FArray2S
	inline
	friend
	bool
	all_ne( FArray2 const & a, FArray2S< T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray2 < FArray2S
	inline
	friend
	bool
	all_lt( FArray2 const & a, FArray2S< T > const & b )
	{
		return lt( a, b );
	}

	// All FArray2 <= FArray2S
	inline
	friend
	bool
	all_le( FArray2 const & a, FArray2S< T > const & b )
	{
		return le( a, b );
	}

	// All FArray2 > FArray2S
	inline
	friend
	bool
	all_gt( FArray2 const & a, FArray2S< T > const & b )
	{
		return gt( a, b );
	}

	// All FArray2 >= FArray2S
	inline
	friend
	bool
	all_ge( FArray2 const & a, FArray2S< T > const & b )
	{
		return ge( a, b );
	}

	// All FArray2S == FArray2
	inline
	friend
	bool
	all_eq( FArray2S< T > const & a, FArray2 const & b )
	{
		return all_eq( b, a );
	}

	// All FArray2S != FArray2
	inline
	friend
	bool
	all_ne( FArray2S< T > const & a, FArray2 const & b )
	{
		return all_ne( b, a );
	}

	// All FArray2S < FArray2
	inline
	friend
	bool
	all_lt( FArray2S< T > const & a, FArray2 const & b )
	{
		return all_gt( b, a );
	}

	// All FArray2S <= FArray2
	inline
	friend
	bool
	all_le( FArray2S< T > const & a, FArray2 const & b )
	{
		return all_ge( b, a );
	}

	// All FArray2S > FArray2
	inline
	friend
	bool
	all_gt( FArray2S< T > const & a, FArray2 const & b )
	{
		return all_lt( b, a );
	}

	// All FArray2S >= FArray2
	inline
	friend
	bool
	all_ge( FArray2S< T > const & a, FArray2 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: Slice

	// Count FArray2 == FArray2S
	inline
	friend
	size_type
	count_eq( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] == b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 != FArray2S
	inline
	friend
	size_type
	count_ne( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] != b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 < FArray2S
	inline
	friend
	size_type
	count_lt( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] < b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 <= FArray2S
	inline
	friend
	size_type
	count_le( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] <= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 > FArray2S
	inline
	friend
	size_type
	count_gt( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] > b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 >= FArray2S
	inline
	friend
	size_type
	count_ge( FArray2 const & a, FArray2S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] >= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2S == FArray2
	inline
	friend
	size_type
	count_eq( FArray2S< T > const & a, FArray2 const & b )
	{
		return count_eq( b, a );
	}

	// Count FArray2S != FArray2
	inline
	friend
	size_type
	count_ne( FArray2S< T > const & a, FArray2 const & b )
	{
		return count_ne( b, a );
	}

	// Count FArray2S < FArray2
	inline
	friend
	size_type
	count_lt( FArray2S< T > const & a, FArray2 const & b )
	{
		return count_gt( b, a );
	}

	// Count FArray2S <= FArray2
	inline
	friend
	size_type
	count_le( FArray2S< T > const & a, FArray2 const & b )
	{
		return count_ge( b, a );
	}

	// Count FArray2S > FArray2
	inline
	friend
	size_type
	count_gt( FArray2S< T > const & a, FArray2 const & b )
	{
		return count_lt( b, a );
	}

	// Count FArray2S >= FArray2
	inline
	friend
	size_type
	count_ge( FArray2S< T > const & a, FArray2 const & b )
	{
		return count_le( b, a );
	}

public: // Comparison: Predicate: MArray

	// FArray2 == MArray2
	template< class A >
	inline
	friend
	bool
	eq( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] == b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 != MArray2
	template< class A >
	inline
	friend
	bool
	ne( FArray2 const & a, MArray2< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray2 < MArray2
	template< class A >
	inline
	friend
	bool
	lt( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] < b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 <= MArray2
	template< class A >
	inline
	friend
	bool
	le( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] <= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 > MArray2
	template< class A >
	inline
	friend
	bool
	gt( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] > b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2 >= MArray2
	template< class A >
	inline
	friend
	bool
	ge( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( ! ( a[ l ] >= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// MArray2 == FArray2
	template< class A >
	inline
	friend
	bool
	eq( MArray2< A, T > const & a, FArray2 const & b )
	{
		return eq( b, a );
	}

	// MArray2 != FArray2
	template< class A >
	inline
	friend
	bool
	ne( MArray2< A, T > const & a, FArray2 const & b )
	{
		return ne( b, a );
	}

	// MArray2 < FArray2
	template< class A >
	inline
	friend
	bool
	lt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return gt( b, a );
	}

	// MArray2 <= FArray2
	template< class A >
	inline
	friend
	bool
	le( MArray2< A, T > const & a, FArray2 const & b )
	{
		return ge( b, a );
	}

	// MArray2 > FArray2
	template< class A >
	inline
	friend
	bool
	gt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return lt( b, a );
	}

	// MArray2 >= FArray2
	template< class A >
	inline
	friend
	bool
	ge( MArray2< A, T > const & a, FArray2 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any FArray2 == MArray2
	template< class A >
	inline
	friend
	bool
	any_eq( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] == b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 != MArray2
	template< class A >
	inline
	friend
	bool
	any_ne( FArray2 const & a, MArray2< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray2 < MArray2
	template< class A >
	inline
	friend
	bool
	any_lt( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] < b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 <= MArray2
	template< class A >
	inline
	friend
	bool
	any_le( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] <= b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 > MArray2
	template< class A >
	inline
	friend
	bool
	any_gt( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] > b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2 >= MArray2
	template< class A >
	inline
	friend
	bool
	any_ge( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] >= b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any MArray2 == FArray2
	template< class A >
	inline
	friend
	bool
	any_eq( MArray2< A, T > const & a, FArray2 const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray2 != FArray2
	template< class A >
	inline
	friend
	bool
	any_ne( MArray2< A, T > const & a, FArray2 const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray2 < FArray2
	template< class A >
	inline
	friend
	bool
	any_lt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray2 <= FArray2
	template< class A >
	inline
	friend
	bool
	any_le( MArray2< A, T > const & a, FArray2 const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray2 > FArray2
	template< class A >
	inline
	friend
	bool
	any_gt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray2 >= FArray2
	template< class A >
	inline
	friend
	bool
	any_ge( MArray2< A, T > const & a, FArray2 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All FArray2 == MArray2
	template< class A >
	inline
	friend
	bool
	all_eq( FArray2 const & a, MArray2< A, T > const & b )
	{
		return eq( a, b );
	}

	// All FArray2 != MArray2
	template< class A >
	inline
	friend
	bool
	all_ne( FArray2 const & a, MArray2< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray2 < MArray2
	template< class A >
	inline
	friend
	bool
	all_lt( FArray2 const & a, MArray2< A, T > const & b )
	{
		return lt( a, b );
	}

	// All FArray2 <= MArray2
	template< class A >
	inline
	friend
	bool
	all_le( FArray2 const & a, MArray2< A, T > const & b )
	{
		return le( a, b );
	}

	// All FArray2 > MArray2
	template< class A >
	inline
	friend
	bool
	all_gt( FArray2 const & a, MArray2< A, T > const & b )
	{
		return gt( a, b );
	}

	// All FArray2 >= MArray2
	template< class A >
	inline
	friend
	bool
	all_ge( FArray2 const & a, MArray2< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray2 == FArray2
	template< class A >
	inline
	friend
	bool
	all_eq( MArray2< A, T > const & a, FArray2 const & b )
	{
		return all_eq( b, a );
	}

	// All MArray2 != FArray2
	template< class A >
	inline
	friend
	bool
	all_ne( MArray2< A, T > const & a, FArray2 const & b )
	{
		return all_ne( b, a );
	}

	// All MArray2 < FArray2
	template< class A >
	inline
	friend
	bool
	all_lt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return all_gt( b, a );
	}

	// All MArray2 <= FArray2
	template< class A >
	inline
	friend
	bool
	all_le( MArray2< A, T > const & a, FArray2 const & b )
	{
		return all_ge( b, a );
	}

	// All MArray2 > FArray2
	template< class A >
	inline
	friend
	bool
	all_gt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return all_lt( b, a );
	}

	// All MArray2 >= FArray2
	template< class A >
	inline
	friend
	bool
	all_ge( MArray2< A, T > const & a, FArray2 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count FArray2 == MArray2
	template< class A >
	inline
	friend
	size_type
	count_eq( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] == b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 != MArray2
	template< class A >
	inline
	friend
	size_type
	count_ne( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] != b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 < MArray2
	template< class A >
	inline
	friend
	size_type
	count_lt( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] < b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 <= MArray2
	template< class A >
	inline
	friend
	size_type
	count_le( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] <= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 > MArray2
	template< class A >
	inline
	friend
	size_type
	count_gt( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] > b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2 >= MArray2
	template< class A >
	inline
	friend
	size_type
	count_ge( FArray2 const & a, MArray2< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1, ++l ) {
				if ( a[ l ] >= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count MArray2 == FArray2
	template< class A >
	inline
	friend
	size_type
	count_eq( MArray2< A, T > const & a, FArray2 const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray2 != FArray2
	template< class A >
	inline
	friend
	size_type
	count_ne( MArray2< A, T > const & a, FArray2 const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray2 < FArray2
	template< class A >
	inline
	friend
	size_type
	count_lt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray2 <= FArray2
	template< class A >
	inline
	friend
	size_type
	count_le( MArray2< A, T > const & a, FArray2 const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray2 > FArray2
	template< class A >
	inline
	friend
	size_type
	count_gt( MArray2< A, T > const & a, FArray2 const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray2 >= FArray2
	template< class A >
	inline
	friend
	size_type
	count_ge( MArray2< A, T > const & a, FArray2 const & b )
	{
		return count_le( b, a );
	}

protected: // Functions

	// Dimension by IndexRange
	virtual
	void
	dimension_assign( IR const & I1, IR const & I2 ) = 0;

	// Swap
	inline
	void
	swap2DB( FArray2 & v )
	{
		swapB( v );
		std::swap( z1_, v.z1_ );
	}

protected: // Data

	size_type z1_; // Size of dim 1

}; // FArray2

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray2< U > const & a, FArray2< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray2< U > const & a, FArray2S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray2S< U > const & a, FArray2< V > const & b )
{
	return b.conformable( a );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( FArray2< U > const & a, MArray2< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray2< A, M > const & a, FArray2< V > const & b )
{
	return b.conformable( a );
}

// Equal Dimensions?
template< typename U, typename V >
inline
bool
equal_dimensions( FArray2< U > const & a, FArray2< V > const & b )
{
	return a.equal_dimensions( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_FArray2_hh_INCLUDED
