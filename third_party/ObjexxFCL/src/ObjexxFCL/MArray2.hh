#ifndef ObjexxFCL_MArray2_hh_INCLUDED
#define ObjexxFCL_MArray2_hh_INCLUDED

// MArray2: 2D Member Array Proxy
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
#include <ObjexxFCL/MArrayR.hh>

namespace ObjexxFCL {

// MArray2: 2D Member Array Proxy
template< class A, typename T >
class MArray2 : public MArrayR< A, T, 2 >
{

private: // Types

	typedef  MArrayR< A, T, 2 >  Super;

private: // Friend

	template< typename, typename > friend class MArray2;

public: // Types

	typedef  typename Super::Array  Array;
	typedef  typename Super::Class  Class;
	typedef  typename Super::MPtr  MPtr;
	typedef  typename Super::Traits  Traits;
	typedef  typename Super::IR  IR;

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
	using Super::l;
	using Super::u;
	using Super::size;
	using Super::j1;
	using Super::j2;
	using Super::array_;
	using Super::pmem_;

public: // Creation

	// Copy Constructor
	inline
	MArray2( MArray2 const & a ) :
	 Super( a )
	{}

	// Constructor
	inline
	MArray2( A & a, T Class::* pmem ) :
	 Super( a, pmem )
	{}

	// Destructor
	inline
	virtual
	~MArray2()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	MArray2 &
	operator =( MArray2 const & a )
	{
		if ( this != &a ) {
			assert( conformable( a ) );
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
					operator ()( i1, i2 ) = a( i1, i2 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename Aa, typename Ta >
	inline
	MArray2 &
	operator =( MArray2< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = a( i1, i2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// Array Assignment Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	operator =( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) = a( j1, j2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// += MArray2 Template
	template< typename Aa, typename Ta >
	inline
	MArray2 &
	operator +=( MArray2< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) += a( i1, i2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// -= MArray2 Template
	template< typename Aa, typename Ta >
	inline
	MArray2 &
	operator -=( MArray2< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) -= a( i1, i2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// *= MArray2 Template
	template< typename Aa, typename Ta >
	inline
	MArray2 &
	operator *=( MArray2< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) *= a( i1, i2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// /= MArray2 Template
	template< typename Aa, typename Ta >
	inline
	MArray2 &
	operator /=( MArray2< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				assert( a( i1, i2 ) != T( 0 ) );
				operator ()( i1, i2 ) /= a( i1, i2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// += Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	operator +=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) += a( j1, j2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	operator -=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) -= a( j1, j2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	operator *=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
				operator ()( i1, i2 ) *= a( j1, j2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	operator /=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
			for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
				assert( a( j1, j2 ) != T( 0 ) );
				operator ()( i1, i2 ) /= a( j1, j2 ); // Not overlap-safe
			}
		}
		return *this;
	}

public: // Assignment: Logical

	// &&= MArray2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	and_equals( MArray2 const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = operator ()( i1, i2 ) && a( i1, i2 ); // Not overlap-safe
			}
		}
		return *this;
	}

	// ||= MArray2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	or_equals( MArray2 const & a )
	{
		assert( conformable( a ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = operator ()( i1, i2 ) || a( i1, i2 ); // Not overlap-safe
			}
		}
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	MArray2 &
	operator =( T const & t )
	{
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = t;
			}
		}
		return *this;
	}

	// = Value Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	operator =( U const & t )
	{
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = t;
			}
		}
		return *this;
	}

	// += Value
	inline
	MArray2 &
	operator +=( T const & t )
	{
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) += t;
			}
		}
		return *this;
	}

	// -= Value
	inline
	MArray2 &
	operator -=( T const & t )
	{
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) -= t;
			}
		}
		return *this;
	}

	// *= Value
	inline
	MArray2 &
	operator *=( T const & t )
	{
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) *= t;
			}
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	inline
	MArray2 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) *= inv_u;
			}
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< !std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	inline
	MArray2 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
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
		return array_( j1( i1 ), j2( i2 ) ).*pmem_;
	}

	// array( i1, i2 )
	inline
	T &
	operator ()( int const i1, int const i2 )
	{
		assert( contains( i1, i2 ) );
		return array_( j1( i1 ), j2( i2 ) ).*pmem_;
	}

public: // Predicate

	// Contains Indexed Element?
	inline
	bool
	contains( int const i1, int const i2 ) const
	{
		if ( ! in_range( u1(), i1 ) ) return false;
		if ( ! in_range( u2(), i2 ) ) return false;
		return true;
	}

	// Conformable?
	template< typename Aa, typename Ta >
	inline
	bool
	conformable( MArray2< Aa, Ta > const & a ) const
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
	template< typename Aa, typename Ta >
	inline
	bool
	equal_dimensions( MArray2< Aa, Ta > const & a ) const
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

	// IndexRange of Dimension 1
	inline
	IR
	I1() const
	{
		return IR( 1, u1() );
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
		return array_.isize1();
	}

	// Size of Dimension 1
	inline
	size_type
	size1() const
	{
		return array_.size1();
	}

	// Size of Dimension 1
	inline
	int
	isize1() const
	{
		return array_.isize1();
	}

	// IndexRange of Dimension 2
	inline
	IR
	I2() const
	{
		return IR( 1, u2() );
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
		return array_.isize2();
	}

	// Size of Dimension 2
	inline
	size_type
	size2() const
	{
		return array_.size2();
	}

	// Size of Dimension 2
	inline
	int
	isize2() const
	{
		return array_.isize2();
	}

public: // Modifier

	// Assign Default Value to all Elements
	inline
	MArray2 &
	to_default()
	{
		for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				operator ()( i1, i2 ) = Traits::initial_value();
			}
		}
		return *this;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	inline
	MArray2< MArray2 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray2< MArray2 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray2< MArray2, M >
	ma( M ClassT::* pmem )
	{
		return MArray2< MArray2, M >( *this, pmem );
	}

public: // Comparison: Predicate

	// MArray2 == MArray2
	inline
	friend
	bool
	eq( MArray2 const & a, MArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) == b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// MArray2 != MArray2
	inline
	friend
	bool
	ne( MArray2 const & a, MArray2 const & b )
	{
		return ! eq( a, b );
	}

	// MArray2 < MArray2
	inline
	friend
	bool
	lt( MArray2 const & a, MArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) < b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// MArray2 <= MArray2
	inline
	friend
	bool
	le( MArray2 const & a, MArray2 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) <= b( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// MArray2 > MArray2
	inline
	friend
	bool
	gt( MArray2 const & a, MArray2 const & b )
	{
		return lt( b, a );
	}

	// MArray2 >= MArray2
	inline
	friend
	bool
	ge( MArray2 const & a, MArray2 const & b )
	{
		return le( b, a );
	}

	// MArray2 == Value
	inline
	friend
	bool
	eq( MArray2 const & a, T const & t )
	{
		assert( a.size_bounded() );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) == t ) ) return false;
			}
		}
		return true;
	}

	// MArray2 != Value
	inline
	friend
	bool
	ne( MArray2 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// MArray2 < Value
	inline
	friend
	bool
	lt( MArray2 const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) < t ) ) return false;
			}
		}
		return true;
	}

	// MArray2 <= Value
	inline
	friend
	bool
	le( MArray2 const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( a( i1, i2 ) <= t ) ) return false;
			}
		}
		return true;
	}

	// MArray2 > Value
	inline
	friend
	bool
	gt( MArray2 const & a, T const & t )
	{
		return lt( t, a );
	}

	// MArray2 >= Value
	inline
	friend
	bool
	ge( MArray2 const & a, T const & t )
	{
		return le( t, a );
	}

	// Value == MArray2
	inline
	friend
	bool
	eq( T const & t, MArray2 const & a )
	{
		return eq( a, t );
	}

	// Value != MArray2
	inline
	friend
	bool
	ne( T const & t, MArray2 const & a )
	{
		return ! eq( a, t );
	}

	// Value < MArray2
	inline
	friend
	bool
	lt( T const & t, MArray2 const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( t < a( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// Value <= MArray2
	inline
	friend
	bool
	le( T const & t, MArray2 const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( ! ( t <= a( i1, i2 ) ) ) return false;
			}
		}
		return true;
	}

	// Value > MArray2
	inline
	friend
	bool
	gt( T const & t, MArray2 const & a )
	{
		return lt( a, t );
	}

	// Value >= MArray2
	inline
	friend
	bool
	ge( T const & t, MArray2 const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any MArray2 == MArray2
	inline
	friend
	bool
	any_eq( MArray2 const & a, MArray2 const & b )
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

	// Any MArray2 != MArray2
	inline
	friend
	bool
	any_ne( MArray2 const & a, MArray2 const & b )
	{
		return ! eq( a, b );
	}

	// Any MArray2 < MArray2
	inline
	friend
	bool
	any_lt( MArray2 const & a, MArray2 const & b )
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

	// Any MArray2 <= MArray2
	inline
	friend
	bool
	any_le( MArray2 const & a, MArray2 const & b )
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

	// Any MArray2 > MArray2
	inline
	friend
	bool
	any_gt( MArray2 const & a, MArray2 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray2 >= MArray2
	inline
	friend
	bool
	any_ge( MArray2 const & a, MArray2 const & b )
	{
		return any_le( b, a );
	}

	// Any MArray2 == Value
	inline
	friend
	bool
	any_eq( MArray2 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == t ) return true;
			}
		}
		return false;
	}

	// Any MArray2 != Value
	inline
	friend
	bool
	any_ne( MArray2 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any MArray2 < Value
	inline
	friend
	bool
	any_lt( MArray2 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) < t ) return true;
			}
		}
		return false;
	}

	// Any MArray2 <= Value
	inline
	friend
	bool
	any_le( MArray2 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= t ) return true;
			}
		}
		return false;
	}

	// Any MArray2 > Value
	inline
	friend
	bool
	any_gt( MArray2 const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any MArray2 >= Value
	inline
	friend
	bool
	any_ge( MArray2 const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value == MArray2
	inline
	friend
	bool
	any_eq( T const & t, MArray2 const & a )
	{
		return any_eq( a, t );
	}

	// Any Value != MArray2
	inline
	friend
	bool
	any_ne( T const & t, MArray2 const & a )
	{
		return ! eq( a, t );
	}

	// Any Value < MArray2
	inline
	friend
	bool
	any_lt( T const & t, MArray2 const & a )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( t < a( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any Value <= MArray2
	inline
	friend
	bool
	any_le( T const & t, MArray2 const & a )
	{
		if ( a.empty() ) return false;
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( t <= a( i1, i2 ) ) return true;
			}
		}
		return false;
	}

	// Any Value > MArray2
	inline
	friend
	bool
	any_gt( T const & t, MArray2 const & a )
	{
		return any_lt( a, t );
	}

	// Any Value >= MArray2
	inline
	friend
	bool
	any_ge( T const & t, MArray2 const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All MArray2 == MArray2
	inline
	friend
	bool
	all_eq( MArray2 const & a, MArray2 const & b )
	{
		return eq( a, b );
	}

	// All MArray2 != MArray2
	inline
	friend
	bool
	all_ne( MArray2 const & a, MArray2 const & b )
	{
		return ! any_eq( a, b );
	}

	// All MArray2 < MArray2
	inline
	friend
	bool
	all_lt( MArray2 const & a, MArray2 const & b )
	{
		return lt( a, b );
	}

	// All MArray2 <= MArray2
	inline
	friend
	bool
	all_le( MArray2 const & a, MArray2 const & b )
	{
		return le( a, b );
	}

	// All MArray2 > MArray2
	inline
	friend
	bool
	all_gt( MArray2 const & a, MArray2 const & b )
	{
		return gt( a, b );
	}

	// All MArray2 >= MArray2
	inline
	friend
	bool
	all_ge( MArray2 const & a, MArray2 const & b )
	{
		return ge( a, b );
	}

	// All MArray2 == Value
	inline
	friend
	bool
	all_eq( MArray2 const & a, T const & t )
	{
		return eq( a, t );
	}

	// All MArray2 != Value
	inline
	friend
	bool
	all_ne( MArray2 const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All MArray2 < Value
	inline
	friend
	bool
	all_lt( MArray2 const & a, T const & t )
	{
		return lt( a, t );
	}

	// All MArray2 <= Value
	inline
	friend
	bool
	all_le( MArray2 const & a, T const & t )
	{
		return le( a, t );
	}

	// All MArray2 > Value
	inline
	friend
	bool
	all_gt( MArray2 const & a, T const & t )
	{
		return gt( a, t );
	}

	// All MArray2 >= Value
	inline
	friend
	bool
	all_ge( MArray2 const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value == MArray2
	inline
	friend
	bool
	all_eq( T const & t, MArray2 const & a )
	{
		return eq( t, a );
	}

	// All Value != MArray2
	inline
	friend
	bool
	all_ne( T const & t, MArray2 const & a )
	{
		return ! any_eq( t, a );
	}

	// All Value < MArray2
	inline
	friend
	bool
	all_lt( T const & t, MArray2 const & a )
	{
		return lt( t, a );
	}

	// All Value <= MArray2
	inline
	friend
	bool
	all_le( T const & t, MArray2 const & a )
	{
		return le( t, a );
	}

	// All Value > MArray2
	inline
	friend
	bool
	all_gt( T const & t, MArray2 const & a )
	{
		return gt( t, a );
	}

	// All Value >= MArray2
	inline
	friend
	bool
	all_ge( T const & t, MArray2 const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count MArray2 == MArray2
	inline
	friend
	size_type
	count_eq( MArray2 const & a, MArray2 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) == b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count MArray2 != MArray2
	inline
	friend
	size_type
	count_ne( MArray2 const & a, MArray2 const & b )
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

	// Count MArray2 < MArray2
	inline
	friend
	size_type
	count_lt( MArray2 const & a, MArray2 const & b )
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

	// Count MArray2 <= MArray2
	inline
	friend
	size_type
	count_le( MArray2 const & a, MArray2 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) <= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count MArray2 > MArray2
	inline
	friend
	size_type
	count_gt( MArray2 const & a, MArray2 const & b )
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

	// Count MArray2 >= MArray2
	inline
	friend
	size_type
	count_ge( MArray2 const & a, MArray2 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0 );
		for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
			for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
				if ( a( i1, i2 ) >= b( i1, i2 ) ) ++n;
			}
		}
		return n;
	}

	// Count MArray2 == Value
	inline
	friend
	size_type
	count_eq( MArray2 const & a, T const & t )
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

	// Count Value == MArray2
	inline
	friend
	size_type
	count_eq( T const & t, MArray2 const & a )
	{
		return count_eq( a, t );
	}

	// Count MArray2 != Value
	inline
	friend
	size_type
	count_ne( MArray2 const & a, T const & t )
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

	// Count Value != MArray2
	inline
	friend
	size_type
	count_ne( T const & t, MArray2 const & a )
	{
		return count_ne( a, t );
	}

	// Count MArray2 < Value
	inline
	friend
	size_type
	count_lt( MArray2 const & a, T const & t )
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

	// Count Value < MArray2
	inline
	friend
	size_type
	count_lt( T const & t, MArray2 const & a )
	{
		return count_gt( a, t );
	}

	// Count MArray2 <= Value
	inline
	friend
	size_type
	count_le( MArray2 const & a, T const & t )
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

	// Count Value <= MArray2
	inline
	friend
	size_type
	count_le( T const & t, MArray2 const & a )
	{
		return count_ge( a, t );
	}

	// Count MArray2 > Value
	inline
	friend
	size_type
	count_gt( MArray2 const & a, T const & t )
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

	// Count Value > MArray2
	inline
	friend
	size_type
	count_gt( T const & t, MArray2 const & a )
	{
		return count_lt( a, t );
	}

	// Count MArray2 >= Value
	inline
	friend
	size_type
	count_ge( MArray2 const & a, T const & t )
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

	// Count Value >= MArray2
	inline
	friend
	size_type
	count_ge( T const & t, MArray2 const & a )
	{
		return count_le( a, t );
	}

}; // MArray2

// Functions

// Make a MArray2
template< class A, typename T >
inline
MArray2< A, T >
make_MArray2( A & array, T A::value_type::* pmem )
{
	return MArray2< A, T >( array, pmem );
}

// Make a MArray2
template< class A, typename T >
inline
MArray2< A, T >
MA2( A & array, T A::value_type::* pmem )
{
	return MArray2< A, T >( array, pmem );
}

// Conformable?
template< typename Aa, typename Ta, typename Ab, typename Tb >
inline
bool
conformable( MArray2< Aa, Ta > const & a, MArray2< Ab, Tb > const & b )
{
	return a.conformable( b );
}

// Equal Dimensions?
template< typename Aa, typename Ta, typename Ab, typename Tb >
inline
bool
equal_dimensions( MArray2< Aa, Ta > const & a, MArray2< Ab, Tb > const & b )
{
	return a.equal_dimensions( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_MArray2_hh_INCLUDED
