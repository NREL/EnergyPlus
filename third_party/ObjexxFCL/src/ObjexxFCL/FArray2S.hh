#ifndef ObjexxFCL_FArray2S_hh_INCLUDED
#define ObjexxFCL_FArray2S_hh_INCLUDED

// FArray2S: 2D Slice Array Proxy
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
#include <ObjexxFCL/FArray2S.fwd.hh>
#include <ObjexxFCL/FArrayRS.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/MArray2.hh>

namespace ObjexxFCL {

// FArray2S: 2D Slice Array Proxy
template< typename T >
class FArray2S : public FArrayRS< T, 2 >
{

private: // Types

	typedef  FArrayRS< T, 2 >  Super;

private: // Friend

	template< typename > friend class FArray2S;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Traits  Traits;
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

	// Using
	using Super::in_range;
	using Super::isize;
	using Super::overlap;
	using Super::size;
	using Super::slice_k;
	using Super::data_;
	using Super::data_beg_;
	using Super::data_end_;
	using Super::size_;

public: // Creation

	// Default Constructor
	inline
	FArray2S() :
	 m1_( 1 ),
	 m2_( 1 ),
	 k_( 0 ),
	 u1_( 0 ),
	 u2_( 0 )
	{}

	// Copy Constructor
	inline
	FArray2S( FArray2S const & a ) :
	 Super( a ),
	 m1_( a.m1_ ),
	 m2_( a.m2_ ),
	 k_( a.k_ ),
	 u1_( a.u1_ ),
	 u2_( a.u2_ )
	{
		data_set();
	}

	// Data Constructor
	inline
	FArray2S( T const * data, std::int64_t const k, DS const & d1, DS const & d2 ) :
	 Super( data, d1.z() * d2.z() ),
	 m1_( d1.m() ),
	 m2_( d2.m() ),
	 k_( k + d1.k() + d2.k() ),
	 u1_( d1.u() ),
	 u2_( d2.u() )
	{
		data_set();
	}

	// Non-Const Data Constructor
	inline
	FArray2S( T * data, std::int64_t const k, DS const & d1, DS const & d2 ) :
	 Super( data, d1.z() * d2.z() ),
	 m1_( d1.m() ),
	 m2_( d2.m() ),
	 k_( k + d1.k() + d2.k() ),
	 u1_( d1.u() ),
	 u2_( d2.u() )
	{
		data_set();
	}

	// Array Constructor
	template< template< typename > class Array >
	inline
	FArray2S( Array< T > const & a ) :
	 Super( a.data(), a.size() ),
	 m1_( 1 ),
	 m2_( a.size1() ),
	 k_( -( m1_ + m2_ ) ),
	 u1_( a.isize1() ),
	 u2_( a.isize2() )
	{
		data_set();
	}

	// Destructor
	inline
	virtual
	~FArray2S()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray2S &
	operator =( FArray2S const & a )
	{
		if ( this != &a ) {
			assert( conformable( a ) );
			if ( overlap( a ) ) { // Overlap-safe
				CArray< T > c( size_ );
				size_type l( 0 );
				for ( int i2 = 1; i2 <= u2_; ++i2 ) {
					for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
						c[ l ] = a( i1, i2 );
					}
				}
				l = 0;
				for ( int i2 = 1; i2 <= u2_; ++i2 ) {
					for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
						operator ()( i1, i2 ) = c[ l ];
					}
				}
			} else { // Not overlap-safe
				for ( int i2 = 1; i2 <= u2_; ++i2 ) {
					for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
						operator ()( i1, i2 ) = a( i1, i2 );
					}
				}
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator =( FArray2S< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = a( i1, i2 );
			}
		}
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray2S &
	operator =( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
					operator () ( i1, i2 ) = a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// Array Assignment Template
	template< template< typename > class ArrayType >
	inline
	FArray2S &
	operator =( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			size_type l( 0 );
			for ( int j2 = a.l2(), e2 = a.u2(); j2 <= e2; ++j2 ) {
				for ( int j1 = a.l1(), e1 = a.u1(); j1 <= e1; ++j1, ++l ) {
					c[ l ] = a( j1, j2 );
				}
			}
			l = 0;
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					operator ()( i1, i2 ) = c[ l ];
				}
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
				for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
					operator ()( i1, i2 ) = a( j1, j2 );
				}
			}
		}
		return *this;
	}

	// Array Assignment Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator =( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) = a( j1, j2 );
			}
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator =( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++r ) {
				operator ()( i1, i2 ) = *r;
			}
		}
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray2S &
	operator +=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
					operator () ( i1, i2 ) += a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray2S &
	operator -=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
					operator () ( i1, i2 ) -= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray2S &
	operator *=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
					operator () ( i1, i2 ) *= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray2S &
	operator /=( MArray2< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
					assert( T( a( i1, i2 ) ) != T( 0 ) );
					operator () ( i1, i2 ) /= a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// += Array Template
	template< template< typename > class ArrayType >
	inline
	FArray2S &
	operator +=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			size_type l( 0 );
			for ( int j2 = a.l2(), e2 = a.u2(); j2 <= e2; ++j2 ) {
				for ( int j1 = a.l1(), e1 = a.u1(); j1 <= e1; ++j1, ++l ) {
					c[ l ] = a( j1, j2 );
				}
			}
			l = 0;
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					operator ()( i1, i2 ) += c[ l ];
				}
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
				for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
					operator ()( i1, i2 ) += a( j1, j2 );
				}
			}
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class ArrayType >
	inline
	FArray2S &
	operator -=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			size_type l( 0 );
			for ( int j2 = a.l2(), e2 = a.u2(); j2 <= e2; ++j2 ) {
				for ( int j1 = a.l1(), e1 = a.u1(); j1 <= e1; ++j1, ++l ) {
					c[ l ] = a( j1, j2 );
				}
			}
			l = 0;
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					operator ()( i1, i2 ) -= c[ l ];
				}
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
				for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
					operator ()( i1, i2 ) -= a( j1, j2 );
				}
			}
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class ArrayType >
	inline
	FArray2S &
	operator *=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			size_type l( 0 );
			for ( int j2 = a.l2(), e2 = a.u2(); j2 <= e2; ++j2 ) {
				for ( int j1 = a.l1(), e1 = a.u1(); j1 <= e1; ++j1, ++l ) {
					c[ l ] = a( j1, j2 );
				}
			}
			l = 0;
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					operator ()( i1, i2 ) *= c[ l ];
				}
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
				for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
					operator ()( i1, i2 ) *= a( j1, j2 );
				}
			}
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class ArrayType >
	inline
	FArray2S &
	operator /=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			size_type l( 0 );
			for ( int j2 = a.l2(), e2 = a.u2(); j2 <= e2; ++j2 ) {
				for ( int j1 = a.l1(), e1 = a.u1(); j1 <= e1; ++j1, ++l ) {
					assert( T( a( j1, j2 ) ) != T( 0 ) );
					c[ l ] = a( j1, j2 );
				}
			}
			l = 0;
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					operator ()( i1, i2 ) /= c[ l ];
				}
			}
		} else { // Not overlap-safe
			for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
				for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
					assert( T( a( j1, j2 ) ) != T( 0 ) );
					operator ()( i1, i2 ) /= a( j1, j2 );
				}
			}
		}
		return *this;
	}

	// += Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator +=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) += a( j1, j2 );
			}
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator -=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) -= a( j1, j2 );
			}
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator *=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) *= a( j1, j2 );
			}
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator /=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(); i2 <= u2_; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1_; i1 <= e1; ++i1, ++j1 ) {
				assert( T( a( j1, j2 ) ) != T( 0 ) );
				operator ()( i1, i2 ) /= a( j1, j2 );
			}
		}
		return *this;
	}

public: // Assignment: Logical

	// &&= Array
	inline
	FArray2S &
	and_equals( FArray2S const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			size_type l( 0 );
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			l = 0;
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					operator ()( i1, i2 ) = operator ()( i1, i2 ) && c[ l ];
				}
			}
		} else { // Not overlap-safe
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
					operator ()( i1, i2 ) = operator ()( i1, i2 ) && a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// ||= Array
	inline
	FArray2S &
	or_equals( FArray2S const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			size_type l( 0 );
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					c[ l ] = a( i1, i2 );
				}
			}
			l = 0;
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1, ++l ) {
					operator ()( i1, i2 ) = operator ()( i1, i2 ) || c[ l ];
				}
			}
		} else { // Not overlap-safe
			for ( int i2 = 1; i2 <= u2_; ++i2 ) {
				for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
					operator ()( i1, i2 ) = operator ()( i1, i2 ) || a( i1, i2 );
				}
			}
		}
		return *this;
	}

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	and_equals( FArray2S const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = operator ()( i1, i2 ) && a( i1, i2 );
			}
		}
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	or_equals( FArray2S const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = operator ()( i1, i2 ) || a( i1, i2 );
			}
		}
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray2S &
	operator =( T const & t )
	{
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = t;
			}
		}
		return *this;
	}

	// += Value
	inline
	FArray2S &
	operator +=( T const & t )
	{
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) += t;
			}
		}
		return *this;
	}

	// -= Value
	inline
	FArray2S &
	operator -=( T const & t )
	{
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) -= t;
			}
		}
		return *this;
	}

	// *= Value
	inline
	FArray2S &
	operator *=( T const & t )
	{
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) *= t;
			}
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	inline
	FArray2S &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) *= inv_u;
			}
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< !std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	inline
	FArray2S &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		for ( int i2 = 1; i2 <= u2_; ++i2 ) {
			for ( int i1 = 1, e1 = u1_; i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) /= u;
			}
		}
		return *this;
	}

public: // Subscript

	// array( i1, i2 ) const
	inline
	T const &
	operator ()( int const i1, int const i2 ) const
	{
		assert( contains( i1, i2 ) );
		return data_[ k_ + ( m1_ * i1 ) + ( m2_ * i2 ) ];
	}

	// array( i1, i2 )
	inline
	T &
	operator ()( int const i1, int const i2 )
	{
		assert( contains( i1, i2 ) );
		return data_[ k_ + ( m1_ * i1 ) + ( m2_ * i2 ) ];
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2 ) const
	{
		return k_ + ( m1_ * i1 ) + ( m2_ * i2 );
	}

public: // Slice Proxy Generators

	// array( s1, s2 ) const
	inline
	FArray2S
	operator ()( IS const & s1, IS const & s2 ) const
	{
		DS const d1( u1_, s1, m1_ );
		DS const d2( u2_, s2, m2_ );
		return FArray2S( data_, k_, d1, d2 );
	}

	// array( i1, s2 ) const
	inline
	FArray1S< T >
	operator ()( int const i1, IS const & s2 ) const
	{
		std::int64_t k( k_ );
		k += slice_k( u1_, i1, m1_ );
		DS const d2( u2_, s2, m2_ );
		return FArray1S< T >( data_, k, d2 );
	}

	// array( s1, i2 ) const
	inline
	FArray1S< T >
	operator ()( IS const & s1, int const i2 ) const
	{
		std::int64_t k( k_ );
		DS const d1( u1_, s1, m1_ );
		k += slice_k( u2_, i2, m2_ );
		return FArray1S< T >( data_, k, d1 );
	}

	// array( s1, s2 )
	inline
	FArray2S
	operator ()( IS const & s1, IS const & s2 )
	{
		DS const d1( u1_, s1, m1_ );
		DS const d2( u2_, s2, m2_ );
		return FArray2S( data_, k_, d1, d2 );
	}

	// array( i1, s2 )
	inline
	FArray1S< T >
	operator ()( int const i1, IS const & s2 )
	{
		std::int64_t k( k_ );
		k += slice_k( u1_, i1, m1_ );
		DS const d2( u2_, s2, m2_ );
		return FArray1S< T >( data_, k, d2 );
	}

	// array( s1, i2 )
	inline
	FArray1S< T >
	operator ()( IS const & s1, int const i2 )
	{
		std::int64_t k( k_ );
		DS const d1( u1_, s1, m1_ );
		k += slice_k( u2_, i2, m2_ );
		return FArray1S< T >( data_, k, d1 );
	}

public: // Predicate

	// contains( i1, i2 )
	inline
	bool
	contains( int const i1, int const i2 ) const
	{
		if ( ! in_range( u1(), i1 ) ) return false;
		if ( ! in_range( u2(), i2 ) ) return false;
		return true;
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

	// Conformable?
	template< class ArrayType >
	inline
	bool
	conformable( ArrayType const & a ) const
	{
		return ( ( a.rank() == 2 ) && ( size1() == a.size1() ) && ( size2() == a.size2() ) );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( FArray2S< U > const & a ) const
	{
		return conformable( a );
	}

	// Equal Dimensions?
	template< class A, typename M >
	inline
	bool
	equal_dimensions( MArray2< A, M > const & a ) const
	{
		return conformable( a );
	}

	// Equal Dimensions?
	template< class ArrayType >
	inline
	bool
	equal_dimensions( ArrayType const & a ) const
	{
		return conformable( a );
	}

public: // Inspector

	// IndexRange of a Dimension
	inline
	IR
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

	// Upper Index of a Dimension
	inline
	int
	u( int const d ) const
	{
		switch ( d ) {
		case 1:
			return u1_;
		case 2:
			return u2_;
		default:
			assert( false );
			return u1_;
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
	inline
	IR
	I1() const
	{
		return IR( 1, u1_ );
	}

	// Lower Index of Dimension 1
	inline
	int
	l1() const
	{
		return 1;
	}

	// Upper Index of Dimension 1
	inline
	int
	u1() const
	{
		return u1_;
	}

	// Size of Dimension 1
	inline
	size_type
	size1() const
	{
		return u1_;
	}

	// Size of Dimension 1
	inline
	int
	isize1() const
	{
		return u1_;
	}

	// IndexRange of Dimension 2
	inline
	IR
	I2() const
	{
		return IR( 1, u2_ );
	}

	// Lower Index of Dimension 2
	inline
	int
	l2() const
	{
		return 1;
	}

	// Upper Index of Dimension 2
	inline
	int
	u2() const
	{
		return u2_;
	}

	// Size of Dimension 2
	inline
	size_type
	size2() const
	{
		return u2_;
	}

	// Size of Dimension 2
	inline
	int
	isize2() const
	{
		return u2_;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	inline
	MArray2< FArray2S const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray2< FArray2S const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray2< FArray2S, M >
	ma( M ClassT::* pmem )
	{
		return MArray2< FArray2S, M >( *this, pmem );
	}

#include <ObjexxFCL/FArray2S.Project.MArray.hh> // Inject project-specific MArray generators

public: // Comparison: Predicate

	// Slice == Slice
	inline
	friend
	bool
	eq( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) == b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// Slice != Slice
	inline
	friend
	bool
	ne( FArray2S const & a, FArray2S const & b )
	{
		return ! eq( a, b );
	}

	// Slice < Slice
	inline
	friend
	bool
	lt( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) < b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// Slice <= Slice
	inline
	friend
	bool
	le( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) <= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// Slice > Slice
	inline
	friend
	bool
	gt( FArray2S const & a, FArray2S const & b )
	{
		return lt( b, a );
	}

	// Slice >= Slice
	inline
	friend
	bool
	ge( FArray2S const & a, FArray2S const & b )
	{
		return le( b, a );
	}

	// Slice == Value
	inline
	friend
	bool
	eq( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) == t ) ) return false;
			}
		}
		return true;
	}

	// Slice != Value
	inline
	friend
	bool
	ne( FArray2S const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Slice < Value
	inline
	friend
	bool
	lt( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) < t ) ) return false;
			}
		}
		return true;
	}

	// Slice <= Value
	inline
	friend
	bool
	le( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) <= t ) ) return false;
			}
		}
		return true;
	}

	// Slice > Value
	inline
	friend
	bool
	gt( FArray2S const & a, T const & t )
	{
		return lt( t, a );
	}

	// Slice >= Value
	inline
	friend
	bool
	ge( FArray2S const & a, T const & t )
	{
		return le( t, a );
	}

	// Value == Slice
	inline
	friend
	bool
	eq( T const & t, FArray2S const & a )
	{
		return eq( a, t );
	}

	// Value != Slice
	inline
	friend
	bool
	ne( T const & t, FArray2S const & a )
	{
		return ! eq( a, t );
	}

	// Value < Slice
	inline
	friend
	bool
	lt( T const & t, FArray2S const & a )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( t < a( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// Value <= Slice
	inline
	friend
	bool
	le( T const & t, FArray2S const & a )
	{
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( t <= a( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// Value > Slice
	inline
	friend
	bool
	gt( T const & t, FArray2S const & a )
	{
		return lt( a, t );
	}

	// Value >= Slice
	inline
	friend
	bool
	ge( T const & t, FArray2S const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any Slice == Slice
	inline
	friend
	bool
	any_eq( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any Slice != Slice
	inline
	friend
	bool
	any_ne( FArray2S const & a, FArray2S const & b )
	{
		return ! eq( a, b );
	}

	// Any Slice < Slice
	inline
	friend
	bool
	any_lt( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any Slice <= Slice
	inline
	friend
	bool
	any_le( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any Slice > Slice
	inline
	friend
	bool
	any_gt( FArray2S const & a, FArray2S const & b )
	{
		return any_lt( b, a );
	}

	// Any Slice >= Slice
	inline
	friend
	bool
	any_ge( FArray2S const & a, FArray2S const & b )
	{
		return any_le( b, a );
	}

	// Any Slice == Value
	inline
	friend
	bool
	any_eq( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == t ) return true;
			}
		}
		return false;
	}

	// Any Slice != Value
	inline
	friend
	bool
	any_ne( FArray2S const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any Slice < Value
	inline
	friend
	bool
	any_lt( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < t ) return true;
			}
		}
		return false;
	}

	// Any Slice <= Value
	inline
	friend
	bool
	any_le( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= t ) return true;
			}
		}
		return false;
	}

	// Any Slice > Value
	inline
	friend
	bool
	any_gt( FArray2S const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any Slice >= Value
	inline
	friend
	bool
	any_ge( FArray2S const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value == Slice
	inline
	friend
	bool
	any_eq( T const & t, FArray2S const & a )
	{
		return any_eq( a, t );
	}

	// Any Value != Slice
	inline
	friend
	bool
	any_ne( T const & t, FArray2S const & a )
	{
		return ! eq( a, t );
	}

	// Any Value < Slice
	inline
	friend
	bool
	any_lt( T const & t, FArray2S const & a )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( t < a( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any Value <= Slice
	inline
	friend
	bool
	any_le( T const & t, FArray2S const & a )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( t <= a( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any Value > Slice
	inline
	friend
	bool
	any_gt( T const & t, FArray2S const & a )
	{
		return any_lt( a, t );
	}

	// Any Value >= Slice
	inline
	friend
	bool
	any_ge( T const & t, FArray2S const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All Slice == Slice
	inline
	friend
	bool
	all_eq( FArray2S const & a, FArray2S const & b )
	{
		return eq( a, b );
	}

	// All Slice != Slice
	inline
	friend
	bool
	all_ne( FArray2S const & a, FArray2S const & b )
	{
		return ! any_eq( a, b );
	}

	// All Slice < Slice
	inline
	friend
	bool
	all_lt( FArray2S const & a, FArray2S const & b )
	{
		return lt( a, b );
	}

	// All Slice <= Slice
	inline
	friend
	bool
	all_le( FArray2S const & a, FArray2S const & b )
	{
		return le( a, b );
	}

	// All Slice > Slice
	inline
	friend
	bool
	all_gt( FArray2S const & a, FArray2S const & b )
	{
		return gt( a, b );
	}

	// All Slice >= Slice
	inline
	friend
	bool
	all_ge( FArray2S const & a, FArray2S const & b )
	{
		return ge( a, b );
	}

	// All Slice == Value
	inline
	friend
	bool
	all_eq( FArray2S const & a, T const & t )
	{
		return eq( a, t );
	}

	// All Slice != Value
	inline
	friend
	bool
	all_ne( FArray2S const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All Slice < Value
	inline
	friend
	bool
	all_lt( FArray2S const & a, T const & t )
	{
		return lt( a, t );
	}

	// All Slice <= Value
	inline
	friend
	bool
	all_le( FArray2S const & a, T const & t )
	{
		return le( a, t );
	}

	// All Slice > Value
	inline
	friend
	bool
	all_gt( FArray2S const & a, T const & t )
	{
		return gt( a, t );
	}

	// All Slice >= Value
	inline
	friend
	bool
	all_ge( FArray2S const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value == Slice
	inline
	friend
	bool
	all_eq( T const & t, FArray2S const & a )
	{
		return eq( t, a );
	}

	// All Value != Slice
	inline
	friend
	bool
	all_ne( T const & t, FArray2S const & a )
	{
		return ! any_eq( t, a );
	}

	// All Value < Slice
	inline
	friend
	bool
	all_lt( T const & t, FArray2S const & a )
	{
		return lt( t, a );
	}

	// All Value <= Slice
	inline
	friend
	bool
	all_le( T const & t, FArray2S const & a )
	{
		return le( t, a );
	}

	// All Value > Slice
	inline
	friend
	bool
	all_gt( T const & t, FArray2S const & a )
	{
		return gt( t, a );
	}

	// All Value >= Slice
	inline
	friend
	bool
	all_ge( T const & t, FArray2S const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count Slice == Slice
	inline
	friend
	size_type
	count_eq( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count Slice != Slice
	inline
	friend
	size_type
	count_ne( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) != b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count Slice < Slice
	inline
	friend
	size_type
	count_lt( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count Slice <= Slice
	inline
	friend
	size_type
	count_le( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count Slice > Slice
	inline
	friend
	size_type
	count_gt( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) > b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count Slice >= Slice
	inline
	friend
	size_type
	count_ge( FArray2S const & a, FArray2S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) >= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count Slice == Value
	inline
	friend
	size_type
	count_eq( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == t ) ++n;
			}
		}
		return n;
	}

	// Count Value == Slice
	inline
	friend
	size_type
	count_eq( T const & t, FArray2S const & a )
	{
		return count_eq( a, t );
	}

	// Count Slice != Value
	inline
	friend
	size_type
	count_ne( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) != t ) ++n;
			}
		}
		return n;
	}

	// Count Value != Slice
	inline
	friend
	size_type
	count_ne( T const & t, FArray2S const & a )
	{
		return count_ne( a, t );
	}

	// Count Slice < Value
	inline
	friend
	size_type
	count_lt( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < t ) ++n;
			}
		}
		return n;
	}

	// Count Value < Slice
	inline
	friend
	size_type
	count_lt( T const & t, FArray2S const & a )
	{
		return count_gt( a, t );
	}

	// Count Slice <= Value
	inline
	friend
	size_type
	count_le( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= t ) ++n;
			}
		}
		return n;
	}

	// Count Value <= Slice
	inline
	friend
	size_type
	count_le( T const & t, FArray2S const & a )
	{
		return count_ge( a, t );
	}

	// Count Slice > Value
	inline
	friend
	size_type
	count_gt( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) > t ) ++n;
			}
		}
		return n;
	}

	// Count Value > Slice
	inline
	friend
	size_type
	count_gt( T const & t, FArray2S const & a )
	{
		return count_lt( a, t );
	}

	// Count Slice >= Value
	inline
	friend
	size_type
	count_ge( FArray2S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) >= t ) ++n;
			}
		}
		return n;
	}

	// Count Value >= Slice
	inline
	friend
	size_type
	count_ge( T const & t, FArray2S const & a )
	{
		return count_le( a, t );
	}

public: // Comparison: Predicate: MArray

	// FArray2S == MArray2
	template< class A >
	inline
	friend
	bool
	eq( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) == b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2S != MArray2
	template< class A >
	inline
	friend
	bool
	ne( FArray2S const & a, MArray2< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray2S < MArray2
	template< class A >
	inline
	friend
	bool
	lt( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) < b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2S <= MArray2
	template< class A >
	inline
	friend
	bool
	le( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) <= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2S > MArray2
	template< class A >
	inline
	friend
	bool
	gt( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) > b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// FArray2S >= MArray2
	template< class A >
	inline
	friend
	bool
	ge( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) >= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// MArray2 == FArray2S
	template< class A >
	inline
	friend
	bool
	eq( MArray2< A, T > const & a, FArray2S const & b )
	{
		return eq( b, a );
	}

	// MArray2 != FArray2S
	template< class A >
	inline
	friend
	bool
	ne( MArray2< A, T > const & a, FArray2S const & b )
	{
		return ne( b, a );
	}

	// MArray2 < FArray2S
	template< class A >
	inline
	friend
	bool
	lt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return gt( b, a );
	}

	// MArray2 <= FArray2S
	template< class A >
	inline
	friend
	bool
	le( MArray2< A, T > const & a, FArray2S const & b )
	{
		return ge( b, a );
	}

	// MArray2 > FArray2S
	template< class A >
	inline
	friend
	bool
	gt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return lt( b, a );
	}

	// MArray2 >= FArray2S
	template< class A >
	inline
	friend
	bool
	ge( MArray2< A, T > const & a, FArray2S const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any FArray2S == MArray2
	template< class A >
	inline
	friend
	bool
	any_eq( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2S != MArray2
	template< class A >
	inline
	friend
	bool
	any_ne( FArray2S const & a, MArray2< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray2S < MArray2
	template< class A >
	inline
	friend
	bool
	any_lt( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2S <= MArray2
	template< class A >
	inline
	friend
	bool
	any_le( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2S > MArray2
	template< class A >
	inline
	friend
	bool
	any_gt( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) > b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any FArray2S >= MArray2
	template< class A >
	inline
	friend
	bool
	any_ge( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) >= b( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any MArray2 == FArray2S
	template< class A >
	inline
	friend
	bool
	any_eq( MArray2< A, T > const & a, FArray2S const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray2 != FArray2S
	template< class A >
	inline
	friend
	bool
	any_ne( MArray2< A, T > const & a, FArray2S const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray2 < FArray2S
	template< class A >
	inline
	friend
	bool
	any_lt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray2 <= FArray2S
	template< class A >
	inline
	friend
	bool
	any_le( MArray2< A, T > const & a, FArray2S const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray2 > FArray2S
	template< class A >
	inline
	friend
	bool
	any_gt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray2 >= FArray2S
	template< class A >
	inline
	friend
	bool
	any_ge( MArray2< A, T > const & a, FArray2S const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All FArray2S == MArray2
	template< class A >
	inline
	friend
	bool
	all_eq( FArray2S const & a, MArray2< A, T > const & b )
	{
		return eq( a, b );
	}

	// All FArray2S != MArray2
	template< class A >
	inline
	friend
	bool
	all_ne( FArray2S const & a, MArray2< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray2S < MArray2
	template< class A >
	inline
	friend
	bool
	all_lt( FArray2S const & a, MArray2< A, T > const & b )
	{
		return lt( a, b );
	}

	// All FArray2S <= MArray2
	template< class A >
	inline
	friend
	bool
	all_le( FArray2S const & a, MArray2< A, T > const & b )
	{
		return le( a, b );
	}

	// All FArray2S > MArray2
	template< class A >
	inline
	friend
	bool
	all_gt( FArray2S const & a, MArray2< A, T > const & b )
	{
		return gt( a, b );
	}

	// All FArray2S >= MArray2
	template< class A >
	inline
	friend
	bool
	all_ge( FArray2S const & a, MArray2< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray2 == FArray2S
	template< class A >
	inline
	friend
	bool
	all_eq( MArray2< A, T > const & a, FArray2S const & b )
	{
		return all_eq( b, a );
	}

	// All MArray2 != FArray2S
	template< class A >
	inline
	friend
	bool
	all_ne( MArray2< A, T > const & a, FArray2S const & b )
	{
		return all_ne( b, a );
	}

	// All MArray2 < FArray2S
	template< class A >
	inline
	friend
	bool
	all_lt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return all_gt( b, a );
	}

	// All MArray2 <= FArray2S
	template< class A >
	inline
	friend
	bool
	all_le( MArray2< A, T > const & a, FArray2S const & b )
	{
		return all_ge( b, a );
	}

	// All MArray2 > FArray2S
	template< class A >
	inline
	friend
	bool
	all_gt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return all_lt( b, a );
	}

	// All MArray2 >= FArray2S
	template< class A >
	inline
	friend
	bool
	all_ge( MArray2< A, T > const & a, FArray2S const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count FArray2S == MArray2
	template< class A >
	inline
	friend
	size_type
	count_eq( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2S != MArray2
	template< class A >
	inline
	friend
	size_type
	count_ne( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) != b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2S < MArray2
	template< class A >
	inline
	friend
	size_type
	count_lt( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2S <= MArray2
	template< class A >
	inline
	friend
	size_type
	count_le( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2S > MArray2
	template< class A >
	inline
	friend
	size_type
	count_gt( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) > b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count FArray2S >= MArray2
	template< class A >
	inline
	friend
	size_type
	count_ge( FArray2S const & a, MArray2< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) >= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count MArray2 == FArray2S
	template< class A >
	inline
	friend
	size_type
	count_eq( MArray2< A, T > const & a, FArray2S const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray2 != FArray2S
	template< class A >
	inline
	friend
	size_type
	count_ne( MArray2< A, T > const & a, FArray2S const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray2 < FArray2S
	template< class A >
	inline
	friend
	size_type
	count_lt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray2 <= FArray2S
	template< class A >
	inline
	friend
	size_type
	count_le( MArray2< A, T > const & a, FArray2S const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray2 > FArray2S
	template< class A >
	inline
	friend
	size_type
	count_gt( MArray2< A, T > const & a, FArray2S const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray2 >= FArray2S
	template< class A >
	inline
	friend
	size_type
	count_ge( MArray2< A, T > const & a, FArray2S const & b )
	{
		return count_le( b, a );
	}

private: // Methods

	// Memory Range Set
	inline
	void
	data_set()
	{
		if ( size_ > 0u ) { // Non-empty slice
			data_beg_ = data_end_ = data_ + k_;
			data_beg_ += m1_ * ( m1_ >= 0 ? 1 : u1_ );
			data_end_ += m1_ * ( m1_ <= 0 ? 1 : u1_ );
			data_beg_ += m2_ * ( m2_ >= 0 ? 1 : u2_ );
			data_end_ += m2_ * ( m2_ <= 0 ? 1 : u2_ );
		} else {
			data_ = data_beg_ = data_end_ = nullptr;
		}
	}

private: // Data

	std::int64_t m1_; // Multiplier of dim 1
	std::int64_t m2_; // Multiplier of dim 2
	std::int64_t k_; // Constant
	int u1_; // Upper index of dim 1
	int u2_; // Upper index of dim 2

}; // FArray2S

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray2S< U > const & a, FArray2S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( FArray2S< U > const & a, MArray2< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray2< A, M > const & a, FArray2S< V > const & b )
{
	return b.conformable( a );
}

} // ObjexxFCL

#endif // ObjexxFCL_FArray2S_hh_INCLUDED
