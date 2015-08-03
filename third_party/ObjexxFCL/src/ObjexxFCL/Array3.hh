#ifndef ObjexxFCL_Array3_hh_INCLUDED
#define ObjexxFCL_Array3_hh_INCLUDED

// Array3: Row-Major 3D Array Abstract Base Class
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array3.fwd.hh>
#include <ObjexxFCL/Array.hh>
#include <ObjexxFCL/Array3S.hh>
#include <ObjexxFCL/MArray3.hh>

namespace ObjexxFCL {

// Forward
template< typename > class Array3D;
template< typename > class Array3A;

// Array3: Row-Major 3D Array Abstract Base Class
template< typename T >
class Array3 : public Array< T >
{

private: // Types

	typedef  Array< T >  Super;

private: // Friend

	template< typename > friend class Array3;
	template< typename > friend class Array3D;
	template< typename > friend class Array3A;

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
	typedef  typename Super::iterator  iterator;
	typedef  typename Super::const_iterator  const_iterator;
	typedef  typename Super::reverse_iterator  reverse_iterator;
	typedef  typename Super::const_reverse_iterator  const_reverse_iterator;
	typedef  typename Super::size_type  size_type;
	typedef  typename Super::difference_type  difference_type;

	// C++ Style
	typedef  typename Super::Value  Value;
	typedef  typename Super::Reference  Reference;
	typedef  typename Super::ConstReference  ConstReference;
	typedef  typename Super::Pointer  Pointer;
	typedef  typename Super::ConstPointer  ConstPointer;
	typedef  typename Super::Iterator  Iterator;
	typedef  typename Super::ConstIterator  ConstIterator;
	typedef  typename Super::ReverseIterator  ReverseIterator;
	typedef  typename Super::ConstReverseIterator  ConstReverseIterator;
	typedef  typename Super::Size  Size;
	typedef  typename Super::Difference  Difference;

	using Super::isize;
	using Super::npos;
	using Super::overlap;
	using Super::size;
	using Super::size_of;
	using Super::slice_k;
	using Super::swapB;
	using Super::data_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;

protected: // Creation

	// Default Constructor
	inline
	Array3() :
	 z1_( 0u ),
	 z2_( 0u ),
	 z3_( 0u )
	{}

	// Copy Constructor
	inline
	Array3( Array3 const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ )
	{}

	// Move Constructor
	inline
	Array3( Array3 && a ) NOEXCEPT :
	 Super( std::move( a ) ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ )
	{
		a.clear_move();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array3( Array3< U > const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array3( Array3S< U > const & a ) :
	 Super( a ),
	 I1_( a.u1() ),
	 I2_( a.u2() ),
	 I3_( a.u3() ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	Array3( MArray3< A, M > const & a ) :
	 Super( a ),
	 I1_( a.u1() ),
	 I2_( a.u2() ),
	 I3_( a.u3() ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// IndexRange Constructor
	inline
	Array3( IR const & I1, IR const & I2, IR const & I3 ) :
	 Super( size_of( I1, I2, I3 ) ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// IndexRange + InitializerSentinel Constructor
	inline
	Array3( IR const & I1, IR const & I2, IR const & I3, InitializerSentinel const & initialized ) :
	 Super( size_of( I1, I2, I3 ), initialized ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array3( IR const & I1, IR const & I2, IR const & I3, std::initializer_list< U > const l ) :
	 Super( l ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{
		assert( size_of( I1, I2, I3 ) == l.size() );
	}

	// Default Proxy Constructor
	inline
	Array3( ProxySentinel const & proxy ) :
	 Super( proxy ),
	 z1_( 0u ),
	 z2_( 0u ),
	 z3_( 0u )
	{}

	// Copy Proxy Constructor
	inline
	Array3( Array3 const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ )
	{}

	// Slice Proxy Constructor
	inline
	Array3( Array3S< T > const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( a.u1() ),
	 I2_( a.u2() ),
	 I3_( a.u3() ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// Base Proxy Constructor
	inline
	Array3( Base const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( a.isize() ),
	 I2_( 1 ),
	 I3_( 1 ),
	 z1_( I1_.size() ),
	 z2_( 1u ),
	 z3_( 1u )
	{}

	// Tail Proxy Constructor
	inline
	Array3( Tail const & s, ProxySentinel const & proxy ) :
	 Super( s, proxy ),
	 I1_( s.isize() ),
	 I2_( 1 ),
	 I3_( 1 ),
	 z1_( I1_.size() ),
	 z2_( 1u ),
	 z3_( 1u )
	{}

	// Value Proxy Constructor
	inline
	Array3( T const & t, ProxySentinel const & proxy ) :
	 Super( t, proxy ),
	 I1_( _ ),
	 I2_( 1 ),
	 I3_( 1 ),
	 z1_( I1_.size() ),
	 z2_( 1u ),
	 z3_( 1u )
	{}

	// Copy + IndexRange Proxy Constructor
	inline
	Array3( Array3 const & a, IR const & I1, IR const & I2, IR const & I3, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// Slice + IndexRange Proxy Constructor
	inline
	Array3( Array3S< T > const & a, IR const & I1, IR const & I2, IR const & I3, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// Base + IndexRange Proxy Constructor
	inline
	Array3( Base const & a, IR const & I1, IR const & I2, IR const & I3, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// Tail + IndexRange Proxy Constructor
	inline
	Array3( Tail const & s, IR const & I1, IR const & I2, IR const & I3, ProxySentinel const & proxy ) :
	 Super( s, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

	// Value + IndexRange Proxy Constructor
	inline
	Array3( T const & t, IR const & I1, IR const & I2, IR const & I3, ProxySentinel const & proxy ) :
	 Super( t, proxy ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 z1_( I1_.size() ),
	 z2_( I2_.size() ),
	 z3_( I3_.size() )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~Array3()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	Array3 &
	operator =( Array3 const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension_assign( a.I1_, a.I2_, a.I3_ );
			Super::operator =( a );
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator =( Array3< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1_, a.I2_, a.I3_ );
		Super::operator =( a );
		return *this;
	}

	// Slice Assignment
	inline
	Array3 &
	operator =( Array3S< T > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3() );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						c[ l ] = a( i1, i2, i3 );
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						data_[ l ] = a( i1, i2, i3 );
					}
				}
			}
		}
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator =( Array3S< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3() );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] = a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	Array3 &
	operator =( MArray3< A, M > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I1(), a.I2(), a.I3() );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] = a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator =( std::initializer_list< U > const l )
	{
		Super::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator +=( Array3< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator -=( Array3< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator *=( Array3< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator /=( Array3< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator /=( a );
		return *this;
	}

	// += Slice
	inline
	Array3 &
	operator +=( Array3S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						c[ l ] = a( i1, i2, i3 );
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] += c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						data_[ l ] += a( i1, i2, i3 );
					}
				}
			}
		}
		return *this;
	}

	// -= Slice
	inline
	Array3 &
	operator -=( Array3S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						c[ l ] = a( i1, i2, i3 );
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] -= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						data_[ l ] -= a( i1, i2, i3 );
					}
				}
			}
		}
		return *this;
	}

	// *= Slice
	inline
	Array3 &
	operator *=( Array3S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						c[ l ] = a( i1, i2, i3 );
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] *= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						data_[ l ] *= a( i1, i2, i3 );
					}
				}
			}
		}
		return *this;
	}

	// /= Slice
	inline
	Array3 &
	operator /=( Array3S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						assert( a( i1, i2, i3 ) != T( 0 ) );
						c[ l ] = a( i1, i2, i3 );
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] /= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						assert( a( i1, i2, i3 ) != T( 0 ) );
						data_[ l ] /= a( i1, i2, i3 );
					}
				}
			}
		}
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator +=( Array3S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] += a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator -=( Array3S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] -= a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator *=( Array3S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] *= a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	operator /=( Array3S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					assert( a( i1, i2, i3 ) != T( 0 ) );
					data_[ l ] /= a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	Array3 &
	operator +=( MArray3< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] += a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	Array3 &
	operator -=( MArray3< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] -= a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	Array3 &
	operator *=( MArray3< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] *= a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	Array3 &
	operator /=( MArray3< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					assert( a( i1, i2, i3 ) != T( 0 ) );
					data_[ l ] /= a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	and_equals( Array3< U > const & a )
	{
		assert( conformable( a ) );
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	or_equals( Array3< U > const & a )
	{
		assert( conformable( a ) );
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice
	inline
	Array3 &
	and_equals( Array3S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						c[ l ] = a( i1, i2, i3 );
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] && c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						data_[ l ] = data_[ l ] && a( i1, i2, i3 );
					}
				}
			}
		}
		return *this;
	}

	// ||= Slice
	inline
	Array3 &
	or_equals( Array3S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						c[ l ] = a( i1, i2, i3 );
					}
				}
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] || c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
						data_[ l ] = data_[ l ] || a( i1, i2, i3 );
					}
				}
			}
		}
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	and_equals( Array3S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] = data_[ l ] && a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array3 &
	or_equals( Array3S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] = data_[ l ] || a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	Array3 &
	and_equals( MArray3< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] = data_[ l ] && a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	Array3 &
	or_equals( MArray3< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3, ++l ) {
					data_[ l ] = data_[ l ] || a( i1, i2, i3 );
				}
			}
		}
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	Array3 &
	operator =( T const & t )
	{
		Super::operator =( t );
		return *this;
	}

	// += Value
	inline
	Array3 &
	operator +=( T const & t )
	{
		Super::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	Array3 &
	operator -=( T const & t )
	{
		Super::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	Array3 &
	operator *=( T const & t )
	{
		Super::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	Array3 &
	operator /=( T const & t )
	{
		Super::operator /=( t );
		return *this;
	}

public: // Subscript

	// array( i1, i2, i3 ) const
	inline
	T const &
	operator ()( int const i1, int const i2, int const i3 ) const
	{
		assert( contains( i1, i2, i3 ) );
		return sdata_[ ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ];
	}

	// array( i1, i2, i3 )
	inline
	T &
	operator ()( int const i1, int const i2, int const i3 )
	{
		assert( contains( i1, i2, i3 ) );
		return sdata_[ ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ];
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2, int const i3 ) const
	{
		return ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) - shift_;
	}

	// Const Tail Starting at array( i1, i2, i3 )
	inline
	Tail const
	a( int const i1, int const i2, int const i3 ) const
	{
		assert( contains( i1, i2, i3 ) );
		size_type const offset( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), ( size_ != npos ? size_ - offset : npos ) );
	}

	// Tail Starting at array( i1, i2, i3 )
	inline
	Tail
	a( int const i1, int const i2, int const i3 )
	{
		assert( contains( i1, i2, i3 ) );
		size_type const offset( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) - shift_ );
		return Tail( data_ + offset, ( size_ != npos ? size_ - offset : npos ) );
	}

public: // Slice Proxy Generators

	// array( s1, s2, s3 ) const
	inline
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3 ) const
	{
		DS const d1( I1_, s1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		DS const d3( I3_, s3 );
		return Array3S< T >( data_, -shift_, d1, d2, d3 );
	}

	// array( i1, s2, s3 ) const
	inline
	Array2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3 ) const
	{
		int k( -shift_ );
		k += slice_k( I1_, i1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		DS const d3( I3_, s3 );
		return Array2S< T >( data_, k, d2, d3 );
	}

	// array( s1, i2, s3 ) const
	inline
	Array2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3 ) const
	{
		int k( -shift_ );
		DS const d1( I1_, s1, z2_ * z3_ );
		k += slice_k( I2_, i2, z3_ );
		DS const d3( I3_, s3 );
		return Array2S< T >( data_, k, d1, d3 );
	}

	// array( s1, s2, i3 ) const
	inline
	Array2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3 ) const
	{
		int k( -shift_ );
		DS const d1( I1_, s1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		k += slice_k( I3_, i3 );
		return Array2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, i3 ) const
	inline
	Array1S< T >
	operator ()( IS const & s1, int const i2, int const i3 ) const
	{
		int k( -shift_ );
		DS const d1( I1_, s1, z2_ * z3_ );
		k += slice_k( I2_, i2, z3_ );
		k += slice_k( I3_, i3 );
		return Array1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3 ) const
	inline
	Array1S< T >
	operator ()( int const i1, IS const & s2, int const i3 ) const
	{
		int k( -shift_ );
		k += slice_k( I1_, i1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		k += slice_k( I3_, i3 );
		return Array1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3 ) const
	inline
	Array1S< T >
	operator ()( int const i1, int const i2, IS const & s3 ) const
	{
		int k( -shift_ );
		k += slice_k( I1_, i1, z2_ * z3_ );
		k += slice_k( I2_, i2, z3_ );
		DS const d3( I3_, s3 );
		return Array1S< T >( data_, k, d3 );
	}

	// array( s1, s2, s3 )
	inline
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3 )
	{
		DS const d1( I1_, s1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		DS const d3( I3_, s3 );
		return Array3S< T >( data_, -shift_, d1, d2, d3 );
	}

	// array( i1, s2, s3 )
	inline
	Array2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3 )
	{
		int k( -shift_ );
		k += slice_k( I1_, i1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		DS const d3( I3_, s3 );
		return Array2S< T >( data_, k, d2, d3 );
	}

	// array( s1, i2, s3 )
	inline
	Array2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3 )
	{
		int k( -shift_ );
		DS const d1( I1_, s1, z2_ * z3_ );
		k += slice_k( I2_, i2, z3_ );
		DS const d3( I3_, s3 );
		return Array2S< T >( data_, k, d1, d3 );
	}

	// array( s1, s2, i3 )
	inline
	Array2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3 )
	{
		int k( -shift_ );
		DS const d1( I1_, s1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		k += slice_k( I3_, i3 );
		return Array2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, i3 )
	inline
	Array1S< T >
	operator ()( IS const & s1, int const i2, int const i3 )
	{
		int k( -shift_ );
		DS const d1( I1_, s1, z2_ * z3_ );
		k += slice_k( I2_, i2, z3_ );
		k += slice_k( I3_, i3 );
		return Array1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3 )
	inline
	Array1S< T >
	operator ()( int const i1, IS const & s2, int const i3 )
	{
		int k( -shift_ );
		k += slice_k( I1_, i1, z2_ * z3_ );
		DS const d2( I2_, s2, z3_ );
		k += slice_k( I3_, i3 );
		return Array1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3 )
	inline
	Array1S< T >
	operator ()( int const i1, int const i2, IS const & s3 )
	{
		int k( -shift_ );
		k += slice_k( I1_, i1, z2_ * z3_ );
		k += slice_k( I2_, i2, z3_ );
		DS const d3( I3_, s3 );
		return Array1S< T >( data_, k, d3 );
	}

public: // Predicate

	// Contains Indexed Element?
	inline
	bool
	contains( int const i1, int const i2, int const i3 ) const
	{
		return ( I1_.contains( i1 ) && I2_.contains( i2 ) && I3_.contains( i3 ) );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( Array3< U > const & a ) const
	{
		return ( ( z1_ == a.z1_ ) && ( z2_ == a.z2_ ) && ( z3_ == a.z3_ ) );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( Array3S< U > const & a ) const
	{
		return ( ( z1_ == a.size1() ) && ( z2_ == a.size2() ) && ( z3_ == a.size3() ) );
	}

	// Conformable?
	template< class A, typename M >
	inline
	bool
	conformable( MArray3< A, M > const & a ) const
	{
		return ( ( z1_ == a.size1() ) && ( z2_ == a.size2() ) && ( z3_ == a.size3() ) );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( Array3< U > const & a ) const
	{
		return ( ( I1_ == a.I1_ ) && ( I2_ == a.I2_ ) && ( I3_ == a.I3_ ) );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( Array3S< U > const & a ) const
	{
		return ( ( l1() == 1 ) && ( u1() == a.u1() ) && ( l2() == 1 ) && ( u2() == a.u2() ) && ( l3() == 1 ) && ( u3() == a.u3() ) );
	}

	// Equal Dimensions?
	template< class A, typename M >
	inline
	bool
	equal_dimensions( MArray3< A, M > const & a ) const
	{
		return ( ( l1() == 1 ) && ( u1() == a.u1() ) && ( l2() == 1 ) && ( u2() == a.u2() ) && ( l3() == 1 ) && ( u3() == a.u3() ) );
	}

public: // Inspector

	// Rank
	inline
	int
	rank() const
	{
		return 3;
	}

	// IndexRange of a Dimension
	inline
	IR const &
	I( int const d ) const
	{
		switch ( d ) {
		case 1:
			return I1_;
		case 2:
			return I2_;
		case 3:
			return I3_;
		default:
			assert( false );
			return I1_;
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
			return z1_;
		case 2:
			return z2_;
		case 3:
			return z3_;
		default:
			assert( false );
			return z1_;
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
		default:
			assert( false );
			return isize1();
		}
	}

	// IndexRange of Dimension 1
	inline
	IR const &
	I1() const
	{
		return I1_;
	}

	// Lower Index of Dimension 1
	inline
	int
	l1() const
	{
		return I1_.l();
	}

	// Upper Index of Dimension 1
	inline
	int
	u1() const
	{
		return I1_.u();
	}

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
	inline
	IR const &
	I2() const
	{
		return I2_;
	}

	// Lower Index of Dimension 2
	inline
	int
	l2() const
	{
		return I2_.l();
	}

	// Upper Index of Dimension 2
	inline
	int
	u2() const
	{
		return I2_.u();
	}

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
	inline
	IR const &
	I3() const
	{
		return I3_;
	}

	// Lower Index of Dimension 3
	inline
	int
	l3() const
	{
		return I3_.l();
	}

	// Upper Index of Dimension 3
	inline
	int
	u3() const
	{
		return I3_.u();
	}

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

public: // Modifier

	// Clear
	inline
	Array3 &
	clear()
	{
		Super::clear();
		I1_.clear();
		I2_.clear();
		I3_.clear();
		z1_ = z2_ = z3_ = 0u;
		return *this;
	}

	// Assign Default Value to all Elements
	inline
	Array3 &
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
	MArray3< Array3 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray3< Array3 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray3< Array3, M >
	ma( M ClassT::* pmem )
	{
		return MArray3< Array3, M >( *this, pmem );
	}

public: // Comparison: Predicate

	// Array3 == Array3
	friend
	inline
	bool
	eq( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 != Array3
	friend
	inline
	bool
	ne( Array3 const & a, Array3 const & b )
	{
		return ! eq( a, b );
	}

	// Array3 < Array3
	friend
	inline
	bool
	lt( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 <= Array3
	friend
	inline
	bool
	le( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 > Array3
	friend
	inline
	bool
	gt( Array3 const & a, Array3 const & b )
	{
		return lt( b, a );
	}

	// Array3 >= Array3
	friend
	inline
	bool
	ge( Array3 const & a, Array3 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any

	// Array3 == Array3
	friend
	inline
	bool
	any_eq( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 != Array3
	friend
	inline
	bool
	any_ne( Array3 const & a, Array3 const & b )
	{
		return ! eq( a, b );
	}

	// Array3 < Array3
	friend
	inline
	bool
	any_lt( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 <= Array3
	friend
	inline
	bool
	any_le( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 > Array3
	friend
	inline
	bool
	any_gt( Array3 const & a, Array3 const & b )
	{
		return any_lt( b, a );
	}

	// Array3 >= Array3
	friend
	inline
	bool
	any_ge( Array3 const & a, Array3 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All

	// Array3 == Array3
	friend
	inline
	bool
	all_eq( Array3 const & a, Array3 const & b )
	{
		return eq( a, b );
	}

	// Array3 != Array3
	friend
	inline
	bool
	all_ne( Array3 const & a, Array3 const & b )
	{
		return ! any_eq( a, b );
	}

	// Array3 < Array3
	friend
	inline
	bool
	all_lt( Array3 const & a, Array3 const & b )
	{
		return lt( a, b );
	}

	// Array3 <= Array3
	friend
	inline
	bool
	all_le( Array3 const & a, Array3 const & b )
	{
		return le( a, b );
	}

	// Array3 > Array3
	friend
	inline
	bool
	all_gt( Array3 const & a, Array3 const & b )
	{
		return gt( a, b );
	}

	// Array3 >= Array3
	friend
	inline
	bool
	all_ge( Array3 const & a, Array3 const & b )
	{
		return ge( a, b );
	}

public: // Comparison: Count

	// Array3 == Array3
	friend
	inline
	bool
	count_eq( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 != Array3
	friend
	inline
	bool
	count_ne( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ne( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 < Array3
	friend
	inline
	bool
	count_lt( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 <= Array3
	friend
	inline
	bool
	count_le( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 > Array3
	friend
	inline
	bool
	count_gt( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_gt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array3 >= Array3
	friend
	inline
	bool
	count_ge( Array3 const & a, Array3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ge( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

public: // Comparison: Predicate: Slice

	// Array3 == Array3S
	friend
	inline
	bool
	eq( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] == b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 != Array3S
	friend
	inline
	bool
	ne( Array3 const & a, Array3S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Array3 < Array3S
	friend
	inline
	bool
	lt( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] < b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 <= Array3S
	friend
	inline
	bool
	le( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] <= b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 > Array3S
	friend
	inline
	bool
	gt( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] > b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 >= Array3S
	friend
	inline
	bool
	ge( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] >= b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3S == Array3
	friend
	inline
	bool
	eq( Array3S< T > const & a, Array3 const & b )
	{
		return eq( b, a );
	}

	// Array3S != Array3
	friend
	inline
	bool
	ne( Array3S< T > const & a, Array3 const & b )
	{
		return ne( b, a );
	}

	// Array3S < Array3
	friend
	inline
	bool
	lt( Array3S< T > const & a, Array3 const & b )
	{
		return gt( b, a );
	}

	// Array3S <= Array3
	friend
	inline
	bool
	le( Array3S< T > const & a, Array3 const & b )
	{
		return ge( b, a );
	}

	// Array3S > Array3
	friend
	inline
	bool
	gt( Array3S< T > const & a, Array3 const & b )
	{
		return lt( b, a );
	}

	// Array3S >= Array3
	friend
	inline
	bool
	ge( Array3S< T > const & a, Array3 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: Slice

	// Any Array3 == Array3S
	friend
	inline
	bool
	any_eq( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] == b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 != Array3S
	friend
	inline
	bool
	any_ne( Array3 const & a, Array3S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Any Array3 < Array3S
	friend
	inline
	bool
	any_lt( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] < b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 <= Array3S
	friend
	inline
	bool
	any_le( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] <= b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 > Array3S
	friend
	inline
	bool
	any_gt( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] > b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 >= Array3S
	friend
	inline
	bool
	any_ge( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] >= b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3S == Array3
	friend
	inline
	bool
	any_eq( Array3S< T > const & a, Array3 const & b )
	{
		return any_eq( b, a );
	}

	// Any Array3S != Array3
	friend
	inline
	bool
	any_ne( Array3S< T > const & a, Array3 const & b )
	{
		return any_ne( b, a );
	}

	// Any Array3S < Array3
	friend
	inline
	bool
	any_lt( Array3S< T > const & a, Array3 const & b )
	{
		return any_gt( b, a );
	}

	// Any Array3S <= Array3
	friend
	inline
	bool
	any_le( Array3S< T > const & a, Array3 const & b )
	{
		return any_ge( b, a );
	}

	// Any Array3S > Array3
	friend
	inline
	bool
	any_gt( Array3S< T > const & a, Array3 const & b )
	{
		return any_lt( b, a );
	}

	// Any Array3S >= Array3
	friend
	inline
	bool
	any_ge( Array3S< T > const & a, Array3 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: Slice

	// All Array3 == Array3S
	friend
	inline
	bool
	all_eq( Array3 const & a, Array3S< T > const & b )
	{
		return eq( a, b );
	}

	// All Array3 != Array3S
	friend
	inline
	bool
	all_ne( Array3 const & a, Array3S< T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All Array3 < Array3S
	friend
	inline
	bool
	all_lt( Array3 const & a, Array3S< T > const & b )
	{
		return lt( a, b );
	}

	// All Array3 <= Array3S
	friend
	inline
	bool
	all_le( Array3 const & a, Array3S< T > const & b )
	{
		return le( a, b );
	}

	// All Array3 > Array3S
	friend
	inline
	bool
	all_gt( Array3 const & a, Array3S< T > const & b )
	{
		return gt( a, b );
	}

	// All Array3 >= Array3S
	friend
	inline
	bool
	all_ge( Array3 const & a, Array3S< T > const & b )
	{
		return ge( a, b );
	}

	// All Array3S == Array3
	friend
	inline
	bool
	all_eq( Array3S< T > const & a, Array3 const & b )
	{
		return all_eq( b, a );
	}

	// All Array3S != Array3
	friend
	inline
	bool
	all_ne( Array3S< T > const & a, Array3 const & b )
	{
		return all_ne( b, a );
	}

	// All Array3S < Array3
	friend
	inline
	bool
	all_lt( Array3S< T > const & a, Array3 const & b )
	{
		return all_gt( b, a );
	}

	// All Array3S <= Array3
	friend
	inline
	bool
	all_le( Array3S< T > const & a, Array3 const & b )
	{
		return all_ge( b, a );
	}

	// All Array3S > Array3
	friend
	inline
	bool
	all_gt( Array3S< T > const & a, Array3 const & b )
	{
		return all_lt( b, a );
	}

	// All Array3S >= Array3
	friend
	inline
	bool
	all_ge( Array3S< T > const & a, Array3 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: Slice

	// Count Array3 == Array3S
	friend
	inline
	size_type
	count_eq( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] == b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 != Array3S
	friend
	inline
	size_type
	count_ne( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] != b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 < Array3S
	friend
	inline
	size_type
	count_lt( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] < b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 <= Array3S
	friend
	inline
	size_type
	count_le( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] <= b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 > Array3S
	friend
	inline
	size_type
	count_gt( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] > b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 >= Array3S
	friend
	inline
	size_type
	count_ge( Array3 const & a, Array3S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] >= b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3S == Array3
	friend
	inline
	size_type
	count_eq( Array3S< T > const & a, Array3 const & b )
	{
		return count_eq( b, a );
	}

	// Count Array3S != Array3
	friend
	inline
	size_type
	count_ne( Array3S< T > const & a, Array3 const & b )
	{
		return count_ne( b, a );
	}

	// Count Array3S < Array3
	friend
	inline
	size_type
	count_lt( Array3S< T > const & a, Array3 const & b )
	{
		return count_gt( b, a );
	}

	// Count Array3S <= Array3
	friend
	inline
	size_type
	count_le( Array3S< T > const & a, Array3 const & b )
	{
		return count_ge( b, a );
	}

	// Count Array3S > Array3
	friend
	inline
	size_type
	count_gt( Array3S< T > const & a, Array3 const & b )
	{
		return count_lt( b, a );
	}

	// Count Array3S >= Array3
	friend
	inline
	size_type
	count_ge( Array3S< T > const & a, Array3 const & b )
	{
		return count_le( b, a );
	}

public: // Comparison: Predicate: MArray

	// Array3 == MArray3
	template< class A >
	friend
	inline
	bool
	eq( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] == b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 != MArray3
	template< class A >
	friend
	inline
	bool
	ne( Array3 const & a, MArray3< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Array3 < MArray3
	template< class A >
	friend
	inline
	bool
	lt( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] < b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 <= MArray3
	template< class A >
	friend
	inline
	bool
	le( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] <= b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 > MArray3
	template< class A >
	friend
	inline
	bool
	gt( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] > b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Array3 >= MArray3
	template< class A >
	friend
	inline
	bool
	ge( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( ! ( a[ l ] >= b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// MArray3 == Array3
	template< class A >
	friend
	inline
	bool
	eq( MArray3< A, T > const & a, Array3 const & b )
	{
		return eq( b, a );
	}

	// MArray3 != Array3
	template< class A >
	friend
	inline
	bool
	ne( MArray3< A, T > const & a, Array3 const & b )
	{
		return ne( b, a );
	}

	// MArray3 < Array3
	template< class A >
	friend
	inline
	bool
	lt( MArray3< A, T > const & a, Array3 const & b )
	{
		return gt( b, a );
	}

	// MArray3 <= Array3
	template< class A >
	friend
	inline
	bool
	le( MArray3< A, T > const & a, Array3 const & b )
	{
		return ge( b, a );
	}

	// MArray3 > Array3
	template< class A >
	friend
	inline
	bool
	gt( MArray3< A, T > const & a, Array3 const & b )
	{
		return lt( b, a );
	}

	// MArray3 >= Array3
	template< class A >
	friend
	inline
	bool
	ge( MArray3< A, T > const & a, Array3 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any Array3 == MArray3
	template< class A >
	friend
	inline
	bool
	any_eq( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] == b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 != MArray3
	template< class A >
	friend
	inline
	bool
	any_ne( Array3 const & a, MArray3< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any Array3 < MArray3
	template< class A >
	friend
	inline
	bool
	any_lt( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] < b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 <= MArray3
	template< class A >
	friend
	inline
	bool
	any_le( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] <= b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 > MArray3
	template< class A >
	friend
	inline
	bool
	any_gt( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] > b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Array3 >= MArray3
	template< class A >
	friend
	inline
	bool
	any_ge( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] >= b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any MArray3 == Array3
	template< class A >
	friend
	inline
	bool
	any_eq( MArray3< A, T > const & a, Array3 const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray3 != Array3
	template< class A >
	friend
	inline
	bool
	any_ne( MArray3< A, T > const & a, Array3 const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray3 < Array3
	template< class A >
	friend
	inline
	bool
	any_lt( MArray3< A, T > const & a, Array3 const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray3 <= Array3
	template< class A >
	friend
	inline
	bool
	any_le( MArray3< A, T > const & a, Array3 const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray3 > Array3
	template< class A >
	friend
	inline
	bool
	any_gt( MArray3< A, T > const & a, Array3 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray3 >= Array3
	template< class A >
	friend
	inline
	bool
	any_ge( MArray3< A, T > const & a, Array3 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All Array3 == MArray3
	template< class A >
	friend
	inline
	bool
	all_eq( Array3 const & a, MArray3< A, T > const & b )
	{
		return eq( a, b );
	}

	// All Array3 != MArray3
	template< class A >
	friend
	inline
	bool
	all_ne( Array3 const & a, MArray3< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All Array3 < MArray3
	template< class A >
	friend
	inline
	bool
	all_lt( Array3 const & a, MArray3< A, T > const & b )
	{
		return lt( a, b );
	}

	// All Array3 <= MArray3
	template< class A >
	friend
	inline
	bool
	all_le( Array3 const & a, MArray3< A, T > const & b )
	{
		return le( a, b );
	}

	// All Array3 > MArray3
	template< class A >
	friend
	inline
	bool
	all_gt( Array3 const & a, MArray3< A, T > const & b )
	{
		return gt( a, b );
	}

	// All Array3 >= MArray3
	template< class A >
	friend
	inline
	bool
	all_ge( Array3 const & a, MArray3< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray3 == Array3
	template< class A >
	friend
	inline
	bool
	all_eq( MArray3< A, T > const & a, Array3 const & b )
	{
		return all_eq( b, a );
	}

	// All MArray3 != Array3
	template< class A >
	friend
	inline
	bool
	all_ne( MArray3< A, T > const & a, Array3 const & b )
	{
		return all_ne( b, a );
	}

	// All MArray3 < Array3
	template< class A >
	friend
	inline
	bool
	all_lt( MArray3< A, T > const & a, Array3 const & b )
	{
		return all_gt( b, a );
	}

	// All MArray3 <= Array3
	template< class A >
	friend
	inline
	bool
	all_le( MArray3< A, T > const & a, Array3 const & b )
	{
		return all_ge( b, a );
	}

	// All MArray3 > Array3
	template< class A >
	friend
	inline
	bool
	all_gt( MArray3< A, T > const & a, Array3 const & b )
	{
		return all_lt( b, a );
	}

	// All MArray3 >= Array3
	template< class A >
	friend
	inline
	bool
	all_ge( MArray3< A, T > const & a, Array3 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count Array3 == MArray3
	template< class A >
	friend
	inline
	size_type
	count_eq( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] == b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 != MArray3
	template< class A >
	friend
	inline
	size_type
	count_ne( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] != b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 < MArray3
	template< class A >
	friend
	inline
	size_type
	count_lt( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] < b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 <= MArray3
	template< class A >
	friend
	inline
	size_type
	count_le( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] <= b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 > MArray3
	template< class A >
	friend
	inline
	size_type
	count_gt( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] > b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count Array3 >= MArray3
	template< class A >
	friend
	inline
	size_type
	count_ge( Array3 const & a, MArray3< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i1 = 1, e1 = b.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = b.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = b.u3(); i3 <= e3; ++i3, ++l ) {
					if ( a[ l ] >= b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count MArray3 == Array3
	template< class A >
	friend
	inline
	size_type
	count_eq( MArray3< A, T > const & a, Array3 const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray3 != Array3
	template< class A >
	friend
	inline
	size_type
	count_ne( MArray3< A, T > const & a, Array3 const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray3 < Array3
	template< class A >
	friend
	inline
	size_type
	count_lt( MArray3< A, T > const & a, Array3 const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray3 <= Array3
	template< class A >
	friend
	inline
	size_type
	count_le( MArray3< A, T > const & a, Array3 const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray3 > Array3
	template< class A >
	friend
	inline
	size_type
	count_gt( MArray3< A, T > const & a, Array3 const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray3 >= Array3
	template< class A >
	friend
	inline
	size_type
	count_ge( MArray3< A, T > const & a, Array3 const & b )
	{
		return count_le( b, a );
	}

protected: // Functions

	// Dimension by IndexRange
	virtual
	void
	dimension_assign( IR const & I1, IR const & I2, IR const & I3 ) = 0;

	// Clear on Move
	inline
	void
	clear_move()
	{
		I1_.clear();
		I2_.clear();
		I3_.clear();
		z1_ = z2_ = z3_ = 0u;
	}

	// Swap
	inline
	void
	swap3( Array3 & v )
	{
		swapB( v );
		I1_.swap( v.I1_ );
		I2_.swap( v.I2_ );
		I3_.swap( v.I3_ );
		std::swap( z1_, v.z1_ );
		std::swap( z2_, v.z2_ );
		std::swap( z3_, v.z3_ );
	}

protected: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2
	IR I3_; // Index range of dim 3

	size_type z1_; // Size of dim 1
	size_type z2_; // Size of dim 2
	size_type z3_; // Size of dim 3

}; // Array3

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array3< U > const & a, Array3< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array3< U > const & a, Array3S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array3S< U > const & a, Array3< V > const & b )
{
	return b.conformable( a );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( Array3< U > const & a, MArray3< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray3< A, M > const & a, Array3< V > const & b )
{
	return b.conformable( a );
}

// Equal Dimensions?
template< typename U, typename V >
inline
bool
equal_dimensions( Array3< U > const & a, Array3< V > const & b )
{
	return a.equal_dimensions( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_Array3_hh_INCLUDED
