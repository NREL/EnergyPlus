#ifndef ObjexxFCL_MArray3_hh_INCLUDED
#define ObjexxFCL_MArray3_hh_INCLUDED

// MArray3: 3D Member Array Proxy
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
#include <ObjexxFCL/MArrayR.hh>

namespace ObjexxFCL {

// MArray3: 3D Member Array Proxy
template< class A, typename T >
class MArray3 : public MArrayR< A, T, 3 >
{

private: // Types

	typedef  MArrayR< A, T, 3 >  Super;

private: // Friend

	template< typename, typename > friend class MArray3;

public: // Types

	typedef  typename Super::ArrayType  ArrayType;
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
	using Super::j3;
	using Super::array_;
	using Super::pmem_;

public: // Creation

	// Copy Constructor
	MArray3( MArray3 const & a ) :
	 Super( a )
	{}

	// Constructor
	MArray3( A & a, T Class::* pmem ) :
	 Super( a, pmem )
	{}

	// Destructor
	virtual
	~MArray3()
	{}

public: // Assignment: Array

	// Copy Assignment
	MArray3 &
	operator =( MArray3 const & a )
	{
		if ( this != &a ) {
			assert( conformable( a ) );
			for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
						operator ()( i1, i2, i3 ) = a( i1, i2, i3 ); // Not overlap-safe
					}
				}
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename Aa, typename Ta >
	MArray3 &
	operator =( MArray3< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) = a( i1, i2, i3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// Array Assignment Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	operator =( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
			for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
				for ( int i3 = 1, j3 = a.l3(), e3 = u3(); i3 <= e3; ++i3, ++j3 ) {
					operator ()( i1, i2, i3 ) = a( j1, j2, j3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// += MArray3 Template
	template< typename Aa, typename Ta >
	MArray3 &
	operator +=( MArray3< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) += a( i1, i2, i3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// -= MArray3 Template
	template< typename Aa, typename Ta >
	MArray3 &
	operator -=( MArray3< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) -= a( i1, i2, i3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// *= MArray3 Template
	template< typename Aa, typename Ta >
	MArray3 &
	operator *=( MArray3< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) *= a( i1, i2, i3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// /= MArray3 Template
	template< typename Aa, typename Ta >
	MArray3 &
	operator /=( MArray3< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					assert( a( i1, i2, i3 ) != T( 0 ) );
					operator ()( i1, i2, i3 ) /= a( i1, i2, i3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// += Array Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	operator +=( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
			for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
				for ( int i3 = 1, j3 = a.l3(), e3 = u3(); i3 <= e3; ++i3, ++j3 ) {
					operator ()( i1, i2, i3 ) += a( j1, j2, j3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	operator -=( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
			for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
				for ( int i3 = 1, j3 = a.l3(), e3 = u3(); i3 <= e3; ++i3, ++j3 ) {
					operator ()( i1, i2, i3 ) -= a( j1, j2, j3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	operator *=( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
			for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
				for ( int i3 = 1, j3 = a.l3(), e3 = u3(); i3 <= e3; ++i3, ++j3 ) {
					operator ()( i1, i2, i3 ) *= a( j1, j2, j3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	operator /=( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, j1 = a.l1(), e1 = u1(); i1 <= e1; ++i1, ++j1 ) {
			for ( int i2 = 1, j2 = a.l2(), e2 = u2(); i2 <= e2; ++i2, ++j2 ) {
				for ( int i3 = 1, j3 = a.l3(), e3 = u3(); i3 <= e3; ++i3, ++j3 ) {
					assert( a( j1, j2, j3 ) != T( 0 ) );
					operator ()( i1, i2, i3 ) /= a( j1, j2, j3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

public: // Assignment: Logical

	// &&= MArray3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	and_equals( MArray3 const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					auto & v( operator ()( i1, i2, i3 ) );
					v = v && a( i1, i2, i3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

	// ||= MArray3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	or_equals( MArray3 const & a )
	{
		assert( conformable( a ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					auto & v( operator ()( i1, i2, i3 ) );
					v = v || a( i1, i2, i3 ); // Not overlap-safe
				}
			}
		}
		return *this;
	}

public: // Assignment: Value

	// = Value
	MArray3 &
	operator =( T const & t )
	{
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) = t;
				}
			}
		}
		return *this;
	}

	// = Value Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray3 &
	operator =( U const & t )
	{
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) = t;
				}
			}
		}
		return *this;
	}

	// += Value
	MArray3 &
	operator +=( T const & t )
	{
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) += t;
				}
			}
		}
		return *this;
	}

	// -= Value
	MArray3 &
	operator -=( T const & t )
	{
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) -= t;
				}
			}
		}
		return *this;
	}

	// *= Value
	MArray3 &
	operator *=( T const & t )
	{
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) *= t;
				}
			}
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	MArray3 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) *= inv_u;
				}
			}
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	MArray3 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		for ( int i1 = 1, e1 = u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = u3(); i3 <= e3; ++i3 ) {
					operator ()( i1, i2, i3 ) /= u;
				}
			}
		}
		return *this;
	}

public: // Subscript

	// array( i1, i2, i3 ) const
	T const &
	operator ()( int const i1, int const i2, int const i3 ) const
	{
		assert( contains( i1, i2, i3 ) );
		return array_( j1( i1 ), j2( i2 ), j2( i3 ) ).*pmem_;
	}

	// array( i1, i2, i3 )
	T &
	operator ()( int const i1, int const i2, int const i3 )
	{
		assert( contains( i1, i2, i3 ) );
		return array_( j1( i1 ), j2( i2 ), j2( i3 ) ).*pmem_;
	}

public: // Predicate

	// Contains Indexed Element?
	bool
	contains( int const i1, int const i2, int const i3 ) const
	{
		if ( ! in_range( u1(), i1 ) ) return false;
		if ( ! in_range( u2(), i2 ) ) return false;
		if ( ! in_range( u3(), i3 ) ) return false;
		return true;
	}

	// Conformable?
	template< typename Aa, typename Ta >
	bool
	conformable( MArray3< Aa, Ta > const & a ) const
	{
		return ( ( size1() == a.size1() ) && ( size2() == a.size2() ) && ( size3() == a.size3() ) );
	}

	// Conformable?
	template< class Ar >
	bool
	conformable( Ar const & a ) const
	{
		return ( ( a.rank() == 3 ) && ( size1() == a.size1() ) && ( size2() == a.size2() ) && ( size3() == a.size3() ) );
	}

	// Equal Dimensions?
	template< typename Aa, typename Ta >
	bool
	equal_dimensions( MArray3< Aa, Ta > const & a ) const
	{
		return conformable( a );
	}

	// Equal Dimensions?
	template< class Ar >
	bool
	equal_dimensions( Ar const & a ) const
	{
		return conformable( a );
	}

public: // Inspector

	// IndexRange of Dimension 1
	IR
	I1() const
	{
		return IR( 1, u1() );
	}

	// Lower Index of Dimension 1
	int
	l1() const
	{
		return 1;
	}

	// Upper Index of Dimension 1
	int
	u1() const
	{
		return array_.isize1();
	}

	// Size of Dimension 1
	size_type
	size1() const
	{
		return array_.size1();
	}

	// Size of Dimension 1
	int
	isize1() const
	{
		return array_.isize1();
	}

	// IndexRange of Dimension 2
	IR
	I2() const
	{
		return IR( 1, u2() );
	}

	// Lower Index of Dimension 2
	int
	l2() const
	{
		return 1;
	}

	// Upper Index of Dimension 2
	int
	u2() const
	{
		return array_.isize2();
	}

	// Size of Dimension 2
	size_type
	size2() const
	{
		return array_.size2();
	}

	// Size of Dimension 2
	int
	isize2() const
	{
		return array_.isize2();
	}

	// IndexRange of Dimension 3
	IR
	I3() const
	{
		return IR( 1, u3() );
	}

	// Lower Index of Dimension 3
	int
	l3() const
	{
		return 1;
	}

	// Upper Index of Dimension 3
	int
	u3() const
	{
		return array_.isize3();
	}

	// Size of Dimension 3
	size_type
	size3() const
	{
		return array_.size3();
	}

	// Size of Dimension 3
	int
	isize3() const
	{
		return array_.isize3();
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	MArray3< MArray3 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray3< MArray3 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	MArray3< MArray3, M >
	ma( M ClassT::* pmem )
	{
		return MArray3< MArray3, M >( *this, pmem );
	}

public: // Comparison: Predicate

	// MArray3 == MArray3
	friend
	bool
	eq( MArray3 const & a, MArray3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( a( i1, i2, i3 ) == b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// MArray3 != MArray3
	friend
	bool
	ne( MArray3 const & a, MArray3 const & b )
	{
		return ! eq( a, b );
	}

	// MArray3 < MArray3
	friend
	bool
	lt( MArray3 const & a, MArray3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( a( i1, i2, i3 ) < b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// MArray3 <= MArray3
	friend
	bool
	le( MArray3 const & a, MArray3 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( a( i1, i2, i3 ) <= b( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// MArray3 > MArray3
	friend
	bool
	gt( MArray3 const & a, MArray3 const & b )
	{
		return lt( b, a );
	}

	// MArray3 >= MArray3
	friend
	bool
	ge( MArray3 const & a, MArray3 const & b )
	{
		return le( b, a );
	}

	// MArray3 == Value
	friend
	bool
	eq( MArray3 const & a, T const & t )
	{
		assert( a.size_bounded() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( a( i1, i2, i3 ) == t ) ) return false;
				}
			}
		}
		return true;
	}

	// MArray3 != Value
	friend
	bool
	ne( MArray3 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// MArray3 < Value
	friend
	bool
	lt( MArray3 const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( a( i1, i2, i3 ) < t ) ) return false;
				}
			}
		}
		return true;
	}

	// MArray3 <= Value
	friend
	bool
	le( MArray3 const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( a( i1, i2, i3 ) <= t ) ) return false;
				}
			}
		}
		return true;
	}

	// MArray3 > Value
	friend
	bool
	gt( MArray3 const & a, T const & t )
	{
		return lt( t, a );
	}

	// MArray3 >= Value
	friend
	bool
	ge( MArray3 const & a, T const & t )
	{
		return le( t, a );
	}

	// Value == MArray3
	friend
	bool
	eq( T const & t, MArray3 const & a )
	{
		return eq( a, t );
	}

	// Value != MArray3
	friend
	bool
	ne( T const & t, MArray3 const & a )
	{
		return ! eq( a, t );
	}

	// Value < MArray3
	friend
	bool
	lt( T const & t, MArray3 const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( t < a( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Value <= MArray3
	friend
	bool
	le( T const & t, MArray3 const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( ! ( t <= a( i1, i2, i3 ) ) ) return false;
				}
			}
		}
		return true;
	}

	// Value > MArray3
	friend
	bool
	gt( T const & t, MArray3 const & a )
	{
		return lt( a, t );
	}

	// Value >= MArray3
	friend
	bool
	ge( T const & t, MArray3 const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any MArray3 == MArray3
	friend
	bool
	any_eq( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) == b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any MArray3 != MArray3
	friend
	bool
	any_ne( MArray3 const & a, MArray3 const & b )
	{
		return ! eq( a, b );
	}

	// Any MArray3 < MArray3
	friend
	bool
	any_lt( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) < b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any MArray3 <= MArray3
	friend
	bool
	any_le( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) <= b( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any MArray3 > MArray3
	friend
	bool
	any_gt( MArray3 const & a, MArray3 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray3 >= MArray3
	friend
	bool
	any_ge( MArray3 const & a, MArray3 const & b )
	{
		return any_le( b, a );
	}

	// Any MArray3 == Value
	friend
	bool
	any_eq( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) == t ) return true;
				}
			}
		}
		return false;
	}

	// Any MArray3 != Value
	friend
	bool
	any_ne( MArray3 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any MArray3 < Value
	friend
	bool
	any_lt( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) < t ) return true;
				}
			}
		}
		return false;
	}

	// Any MArray3 <= Value
	friend
	bool
	any_le( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) <= t ) return true;
				}
			}
		}
		return false;
	}

	// Any MArray3 > Value
	friend
	bool
	any_gt( MArray3 const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any MArray3 >= Value
	friend
	bool
	any_ge( MArray3 const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value == MArray3
	friend
	bool
	any_eq( T const & t, MArray3 const & a )
	{
		return any_eq( a, t );
	}

	// Any Value != MArray3
	friend
	bool
	any_ne( T const & t, MArray3 const & a )
	{
		return ! eq( a, t );
	}

	// Any Value < MArray3
	friend
	bool
	any_lt( T const & t, MArray3 const & a )
	{
		if ( a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( t < a( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Value <= MArray3
	friend
	bool
	any_le( T const & t, MArray3 const & a )
	{
		if ( a.empty() ) return false;
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( t <= a( i1, i2, i3 ) ) return true;
				}
			}
		}
		return false;
	}

	// Any Value > MArray3
	friend
	bool
	any_gt( T const & t, MArray3 const & a )
	{
		return any_lt( a, t );
	}

	// Any Value >= MArray3
	friend
	bool
	any_ge( T const & t, MArray3 const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All MArray3 == MArray3
	friend
	bool
	all_eq( MArray3 const & a, MArray3 const & b )
	{
		return eq( a, b );
	}

	// All MArray3 != MArray3
	friend
	bool
	all_ne( MArray3 const & a, MArray3 const & b )
	{
		return ! any_eq( a, b );
	}

	// All MArray3 < MArray3
	friend
	bool
	all_lt( MArray3 const & a, MArray3 const & b )
	{
		return lt( a, b );
	}

	// All MArray3 <= MArray3
	friend
	bool
	all_le( MArray3 const & a, MArray3 const & b )
	{
		return le( a, b );
	}

	// All MArray3 > MArray3
	friend
	bool
	all_gt( MArray3 const & a, MArray3 const & b )
	{
		return gt( a, b );
	}

	// All MArray3 >= MArray3
	friend
	bool
	all_ge( MArray3 const & a, MArray3 const & b )
	{
		return ge( a, b );
	}

	// All MArray3 == Value
	friend
	bool
	all_eq( MArray3 const & a, T const & t )
	{
		return eq( a, t );
	}

	// All MArray3 != Value
	friend
	bool
	all_ne( MArray3 const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All MArray3 < Value
	friend
	bool
	all_lt( MArray3 const & a, T const & t )
	{
		return lt( a, t );
	}

	// All MArray3 <= Value
	friend
	bool
	all_le( MArray3 const & a, T const & t )
	{
		return le( a, t );
	}

	// All MArray3 > Value
	friend
	bool
	all_gt( MArray3 const & a, T const & t )
	{
		return gt( a, t );
	}

	// All MArray3 >= Value
	friend
	bool
	all_ge( MArray3 const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value == MArray3
	friend
	bool
	all_eq( T const & t, MArray3 const & a )
	{
		return eq( t, a );
	}

	// All Value != MArray3
	friend
	bool
	all_ne( T const & t, MArray3 const & a )
	{
		return ! any_eq( t, a );
	}

	// All Value < MArray3
	friend
	bool
	all_lt( T const & t, MArray3 const & a )
	{
		return lt( t, a );
	}

	// All Value <= MArray3
	friend
	bool
	all_le( T const & t, MArray3 const & a )
	{
		return le( t, a );
	}

	// All Value > MArray3
	friend
	bool
	all_gt( T const & t, MArray3 const & a )
	{
		return gt( t, a );
	}

	// All Value >= MArray3
	friend
	bool
	all_ge( T const & t, MArray3 const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count MArray3 == MArray3
	friend
	size_type
	count_eq( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) == b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count MArray3 != MArray3
	friend
	size_type
	count_ne( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) != b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count MArray3 < MArray3
	friend
	size_type
	count_lt( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) < b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count MArray3 <= MArray3
	friend
	size_type
	count_le( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) <= b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count MArray3 > MArray3
	friend
	size_type
	count_gt( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) > b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count MArray3 >= MArray3
	friend
	size_type
	count_ge( MArray3 const & a, MArray3 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) >= b( i1, i2, i3 ) ) ++n;
				}
			}
		}
		return n;
	}

	// Count MArray3 == Value
	friend
	size_type
	count_eq( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) == t ) ++n;
				}
			}
		}
		return n;
	}

	// Count Value == MArray3
	friend
	size_type
	count_eq( T const & t, MArray3 const & a )
	{
		return count_eq( a, t );
	}

	// Count MArray3 != Value
	friend
	size_type
	count_ne( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) != t ) ++n;
				}
			}
		}
		return n;
	}

	// Count Value != MArray3
	friend
	size_type
	count_ne( T const & t, MArray3 const & a )
	{
		return count_ne( a, t );
	}

	// Count MArray3 < Value
	friend
	size_type
	count_lt( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) < t ) ++n;
				}
			}
		}
		return n;
	}

	// Count Value < MArray3
	friend
	size_type
	count_lt( T const & t, MArray3 const & a )
	{
		return count_gt( a, t );
	}

	// Count MArray3 <= Value
	friend
	size_type
	count_le( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) <= t ) ++n;
				}
			}
		}
		return n;
	}

	// Count Value <= MArray3
	friend
	size_type
	count_le( T const & t, MArray3 const & a )
	{
		return count_ge( a, t );
	}

	// Count MArray3 > Value
	friend
	size_type
	count_gt( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) > t ) ++n;
				}
			}
		}
		return n;
	}

	// Count Value > MArray3
	friend
	size_type
	count_gt( T const & t, MArray3 const & a )
	{
		return count_lt( a, t );
	}

	// Count MArray3 >= Value
	friend
	size_type
	count_ge( MArray3 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					if ( a( i1, i2, i3 ) >= t ) ++n;
				}
			}
		}
		return n;
	}

	// Count Value >= MArray3
	friend
	size_type
	count_ge( T const & t, MArray3 const & a )
	{
		return count_le( a, t );
	}

}; // MArray3

// Functions

// Make an MArray3
template< class A, typename T >
inline
MArray3< A, T >
make_MArray3( A & a, T A::value_type::* pmem )
{
	return MArray3< A, T >( a, pmem );
}

// Make an MArray3
template< class A, typename T >
inline
MArray3< A, T >
MA3( A & a, T A::value_type::* pmem )
{
	return MArray3< A, T >( a, pmem );
}

// Conformable?
template< typename Aa, typename Ta, typename Ab, typename Tb >
inline
bool
conformable( MArray3< Aa, Ta > const & a, MArray3< Ab, Tb > const & b )
{
	return a.conformable( b );
}

// Equal Dimensions?
template< typename Aa, typename Ta, typename Ab, typename Tb >
inline
bool
equal_dimensions( MArray3< Aa, Ta > const & a, MArray3< Ab, Tb > const & b )
{
	return a.equal_dimensions( b );
}

// Stream >> MArray3
template< class A, typename T >
inline
std::istream &
operator >>( std::istream & stream, MArray3< A, T > & a )
{
	if ( stream && ( a.size() > 0u ) ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					stream >> a( i1, i2, i3 );
					if ( ! stream ) break;
				} if ( ! stream ) break;
			} if ( ! stream ) break;
		}
	}
	return stream;
}

// Stream << MArray3
template< class A, typename T >
inline
std::ostream &
operator <<( std::ostream & stream, MArray3< A, T > const & a )
{
	typedef  TypeTraits< T >  Traits;
	if ( stream && ( a.size() > 0u ) ) {
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision ) );
		stream << std::right << std::showpoint << std::uppercase;
		int const w( Traits::iwidth );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					stream << std::setw( w ) << a( i1, i2, i3 ) << ' ';
					if ( ! stream ) break;
				} if ( ! stream ) break;
			} if ( ! stream ) break;
		}
		stream.precision( old_precision );
		stream.flags( old_flags );
	}
	return stream;
}

// Read an MArray3 from a Binary File
template< class A, typename T >
inline
std::istream &
read_binary( std::istream & stream, MArray3< A, T > & a )
{
	std::size_t const n( a.size() );
	if ( stream && ( n > 0u ) ) {
		std::size_t const type_size( sizeof( T ) / sizeof( std::istream::char_type ) );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					stream.read( ( std::istream::char_type * )&a( i1, i2, i3 ), type_size );
					if ( ! stream ) break;
				} if ( ! stream ) break;
			} if ( ! stream ) break;
		}
	}
	return stream;
}

// Write an MArray3 to a Binary File
template< class A, typename T >
inline
std::ostream &
write_binary( std::ostream & stream, MArray3< A, T > const & a )
{
	std::size_t const n( a.size() );
	if ( stream && ( n > 0u ) ) {
		std::size_t const type_size( sizeof( T ) / sizeof( std::ostream::char_type ) );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					stream.write( ( std::ostream::char_type const * )&a( i1, i2, i3 ), type_size );
					if ( ! stream ) break;
				} if ( ! stream ) break;
			} if ( ! stream ) break;
		}
	}
	return stream;
}

namespace fmt {

// List-Directed Format: MArray3
template< class A, typename T >
inline
std::string
LD( MArray3< A, T > const & a )
{
	std::string s;
	std::size_t const n( a.size() );
	if ( n > 0u ) {
		s.reserve( n * TypeTraits< T >::width );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					s.append( fmt::LD( a( i1, i2, i3 ) ) );
				}
			}
		}
	}
	return s;
}

} // fmt

} // ObjexxFCL

#endif // ObjexxFCL_MArray3_hh_INCLUDED
