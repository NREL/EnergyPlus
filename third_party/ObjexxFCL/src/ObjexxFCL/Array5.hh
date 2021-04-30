#ifndef ObjexxFCL_Array5_hh_INCLUDED
#define ObjexxFCL_Array5_hh_INCLUDED

// Array5: Row-Major 5D Array Abstract Base Class
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
#include <ObjexxFCL/Array5.fwd.hh>
#include <ObjexxFCL/Array3S.hh>
#include <ObjexxFCL/Array4S.hh>
#include <ObjexxFCL/Array.hh>

namespace ObjexxFCL {

// Forward
template< typename > class Array5D;

// Array5: Row-Major 5D Array Abstract Base Class
template< typename T >
class Array5 : public Array< T >
{

private: // Types

	typedef  Array< T >  Super;

private: // Friend

	template< typename > friend class Array5;
	template< typename > friend class Array5D;

protected: // Types

	typedef  internal::InitializerSentinel  InitializerSentinel;

public: // Types

	typedef  typename Super::Base  Base;
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

protected: // Types

	using Super::size_of;
	using Super::slice_k;
	using Super::swapB;

	using Super::data_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;

protected: // Creation

	// Default Constructor
	Array5() :
	 z1_( 0u ),
	 z2_( 0u ),
	 z3_( 0u ),
	 z4_( 0u ),
	 z5_( 0u )
	{}

	// Copy Constructor
	Array5( Array5 const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ ),
	 z4_( a.z4_ ),
	 z5_( a.z5_ )
	{}

	// Move Constructor
	Array5( Array5 && a ) noexcept :
	 Super( std::move( a ) ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 I5_( a.I5_ ),
	 z1_( a.z1_ ),
	 z2_( a.z2_ ),
	 z3_( a.z3_ ),
	 z4_( a.z4_ ),
	 z5_( a.z5_ )
	{
		a.clear_move();
	}


public: // Creation

	// Destructor
	virtual
	~Array5() = default;

public: // Assignment: Array

	// Copy Assignment
	Array5 &
	operator =( Array5 const & a )
	{
		if ( this != &a ) {
			if ( ( conformable( a ) ) || ( ! dimension_assign( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ ) ) ) {
				Super::operator =( a );
			} else {
				Super::initialize( a );
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5 &
	operator =( Array5< U > const & a )
	{
		if ( ( conformable( a ) ) || ( ! dimension_assign( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ ) ) ) {
			Super::operator =( a );
		} else {
			Super::initialize( a );
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5 &
	operator =( std::initializer_list< U > const l )
	{
		Super::operator =( l );
		return *this;
	}


public: // Assignment: Value

	// = Value
	Array5 &
	operator =( T const & t )
	{
		Super::operator =( t );
		return *this;
	}


public: // Subscript

	// array( i1, i2, i3, i4, i5 ) const
	T const &
	operator ()( int const i1, int const i2, int const i3, int const i4, int const i5 ) const
	{
		assert( contains( i1, i2, i3, i4, i5 ) );
		return sdata_[ ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ];
	}

	// array( i1, i2, i3, i4, i5 )
	T &
	operator ()( int const i1, int const i2, int const i3, int const i4, int const i5 )
	{
		assert( contains( i1, i2, i3, i4, i5 ) );
		return sdata_[ ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ];
	}

	// Linear Index
	size_type
	index( int const i1, int const i2, int const i3, int const i4, int const i5 ) const
	{
		return ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) - shift_;
	}

public: // Slice Proxy Generators

	// array( i1, s2, s3, s4, s5 ) const
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d4, d5 );
	}

	// array( s1, i2, s3, s4, s5 ) const
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d4, d5 );
	}

	// array( s1, s2, i3, s4, s5 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d4, d5 );
	}

	// array( s1, s2, s3, i4, s5 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d5 );
	}

	// array( s1, s2, s3, s4, i5 ) const
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d4 );
	}

	// array( i1, i2, s3, s4, s5 ) const
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d4, d5 );
	}

	// array( i1, s2, i3, s4, s5 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d4, d5 );
	}

	// array( i1, s2, s3, i4, s5 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d5 );
	}

	// array( i1, s2, s3, s4, i5 ) const
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d4 );
	}

	// array( s1, i2, i3, s4, s5 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d4, d5 );
	}

	// array( s1, i2, s3, i4, s5 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d5 );
	}

	// array( s1, i2, s3, s4, i5 ) const
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d4 );
	}

	// array( s1, s2, i3, i4, s5 ) const
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d5 );
	}

	// array( s1, s2, i3, s4, i5 ) const
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d4 );
	}

	// array( s1, s2, s3, i4, i5 ) const
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d3 );
	}

	// array( s1, s2, i3, i4, i5 ) const
	Array2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, s3, i4, i5 ) const
	Array2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d3 );
	}

	// array( s1, i2, i3, s4, i5 ) const
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d4 );
	}

	// array( s1, i2, i3, i4, s5 ) const
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d5 );
	}

	// array( i1, s2, s3, i4, i5 ) const
	Array2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d3 );
	}

	// array( i1, s2, i3, s4, i5 ) const
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d4 );
	}

	// array( i1, s2, i3, i4, s5 ) const
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d5 );
	}

	// array( i1, i2, s3, s4, i5 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d4 );
	}

	// array( i1, i2, s3, i4, s5 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d5 );
	}

	// array( i1, i2, i3, s4, s5 ) const
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d4, d5 );
	}

	// array( s1, i2, i3, i4, i5 ) const
	Array1S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3, i4, i5 ) const
	Array1S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3, i4, i5 ) const
	Array1S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d3 );
	}

	// array( i1, i2, i3, s4, i5 ) const
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, int const i5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d4 );
	}

	// array( i1, i2, i3, i4, s5 ) const
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, IS const & s5 ) const
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d5 );
	}

	// array( i1, s2, s3, s4, s5 )
	Array4S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array4S< T >( data_, k, d2, d3, d4, d5 );
	}

	// array( s1, i2, s3, s4, s5 )
	Array4S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d3, d4, d5 );
	}

	// array( s1, s2, i3, s4, s5 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d4, d5 );
	}

	// array( s1, s2, s3, i4, s5 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d5 );
	}

	// array( s1, s2, s3, s4, i5 )
	Array4S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array4S< T >( data_, k, d1, d2, d3, d4 );
	}

	// array( i1, i2, s3, s4, s5 )
	Array3S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d3, d4, d5 );
	}

	// array( i1, s2, i3, s4, s5 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d4, d5 );
	}

	// array( i1, s2, s3, i4, s5 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d5 );
	}

	// array( i1, s2, s3, s4, i5 )
	Array3S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array3S< T >( data_, k, d2, d3, d4 );
	}

	// array( s1, i2, i3, s4, s5 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d4, d5 );
	}

	// array( s1, i2, s3, i4, s5 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d5 );
	}

	// array( s1, i2, s3, s4, i5 )
	Array3S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d3, d4 );
	}

	// array( s1, s2, i3, i4, s5 )
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d5 );
	}

	// array( s1, s2, i3, s4, i5 )
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d4 );
	}

	// array( s1, s2, s3, i4, i5 )
	Array3S< T >
	operator ()( IS const & s1, IS const & s2, IS const & s3, int const i4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array3S< T >( data_, k, d1, d2, d3 );
	}

	// array( s1, s2, i3, i4, i5 )
	Array2S< T >
	operator ()( IS const & s1, IS const & s2, int const i3, int const i4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d2 );
	}

	// array( s1, i2, s3, i4, i5 )
	Array2S< T >
	operator ()( IS const & s1, int const i2, IS const & s3, int const i4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d3 );
	}

	// array( s1, i2, i3, s4, i5 )
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d4 );
	}

	// array( s1, i2, i3, i4, s5 )
	Array2S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array2S< T >( data_, k, d1, d5 );
	}

	// array( i1, s2, s3, i4, i5 )
	Array2S< T >
	operator ()( int const i1, IS const & s2, IS const & s3, int const i4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d3 );
	}

	// array( i1, s2, i3, s4, i5 )
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d4 );
	}

	// array( i1, s2, i3, i4, s5 )
	Array2S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d2, d5 );
	}

	// array( i1, i2, s3, s4, i5 )
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d4 );
	}

	// array( i1, i2, s3, i4, s5 )
	Array2S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d3, d5 );
	}

	// array( i1, i2, i3, s4, s5 )
	Array2S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array2S< T >( data_, k, d4, d5 );
	}

	// array( s1, i2, i3, i4, i5 )
	Array1S< T >
	operator ()( IS const & s1, int const i2, int const i3, int const i4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		DS const d1( I1_, s1, z *= z2_ );
		return Array1S< T >( data_, k, d1 );
	}

	// array( i1, s2, i3, i4, i5 )
	Array1S< T >
	operator ()( int const i1, IS const & s2, int const i3, int const i4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		DS const d2( I2_, s2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d2 );
	}

	// array( i1, i2, s3, i4, i5 )
	Array1S< T >
	operator ()( int const i1, int const i2, IS const & s3, int const i4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		k += slice_k( I4_, i4, z );
		DS const d3( I3_, s3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d3 );
	}

	// array( i1, i2, i3, s4, i5 )
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, IS const & s4, int const i5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		k += slice_k( I5_, i5 );
		DS const d4( I4_, s4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d4 );
	}

	// array( i1, i2, i3, i4, s5 )
	Array1S< T >
	operator ()( int const i1, int const i2, int const i3, int const i4, IS const & s5 )
	{
		std::int64_t k( -shift_ );
		size_type z( z5_ );
		DS const d5( I5_, s5 );
		k += slice_k( I4_, i4, z );
		k += slice_k( I3_, i3, z *= z4_ );
		k += slice_k( I2_, i2, z *= z3_ );
		k += slice_k( I1_, i1, z *= z2_ );
		return Array1S< T >( data_, k, d5 );
	}

public: // Predicate

	// Contains Indexed Element?
	bool
	contains( int const i1, int const i2, int const i3, int const i4, int const i5 ) const
	{
		return ( I1_.contains( i1 ) && I2_.contains( i2 ) && I3_.contains( i3 ) && I4_.contains( i4 ) && I5_.contains( i5 ) );
	}

	// Conformable?
	template< typename U >
	bool
	conformable( Array5< U > const & a ) const
	{
		return ( ( z1_ == a.z1_ ) && ( z2_ == a.z2_ ) && ( z3_ == a.z3_ ) && ( z4_ == a.z4_ ) && ( z5_ == a.z5_ ) );
	}

	// Equal Dimensions?
	template< typename U >
	bool
	equal_dimensions( Array5< U > const & a ) const
	{
		return ( ( I1_ == a.I1_ ) && ( I2_ == a.I2_ ) && ( I3_ == a.I3_ ) && ( I4_ == a.I4_ ) && ( I5_ == a.I5_ ) );
	}

public: // Inspector

	// Rank
	int
	rank() const
	{
		return 5;
	}

	// IndexRange of a Dimension
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
		case 4:
			return I4_;
		case 5:
			return I5_;
		default:
			assert( false );
			return I1_;
		}
	}

	// Lower Index of a Dimension
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
		case 5:
			return l5();
		default:
			assert( false );
			return l1();
		}
	}

	// Upper Index of a Dimension
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
		case 5:
			return u5();
		default:
			assert( false );
			return u1();
		}
	}

	// Size of a Dimension
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
		case 4:
			return z4_;
		case 5:
			return z5_;
		default:
			assert( false );
			return z1_;
		}
	}

	// Size of a Dimension
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
		case 5:
			return isize5();
		default:
			assert( false );
			return isize1();
		}
	}

	// IndexRange of Dimension 1
	IR const &
	I1() const
	{
		return I1_;
	}

	// Lower Index of Dimension 1
	int
	l1() const
	{
		return I1_.l();
	}

	// Upper Index of Dimension 1
	int
	u1() const
	{
		return I1_.u();
	}

	// Size of Dimension 1
	size_type
	size1() const
	{
		return z1_;
	}

	// Size of Dimension 1
	int
	isize1() const
	{
		return static_cast< int >( z1_ );
	}

	// IndexRange of Dimension 2
	IR const &
	I2() const
	{
		return I2_;
	}

	// Lower Index of Dimension 2
	int
	l2() const
	{
		return I2_.l();
	}

	// Upper Index of Dimension 2
	int
	u2() const
	{
		return I2_.u();
	}

	// Size of Dimension 2
	size_type
	size2() const
	{
		return z2_;
	}

	// Size of Dimension 2
	int
	isize2() const
	{
		return static_cast< int >( z2_ );
	}

	// IndexRange of Dimension 3
	IR const &
	I3() const
	{
		return I3_;
	}

	// Lower Index of Dimension 3
	int
	l3() const
	{
		return I3_.l();
	}

	// Upper Index of Dimension 3
	int
	u3() const
	{
		return I3_.u();
	}

	// Size of Dimension 3
	size_type
	size3() const
	{
		return z3_;
	}

	// Size of Dimension 3
	int
	isize3() const
	{
		return static_cast< int >( z3_ );
	}

	// IndexRange of Dimension 4
	IR const &
	I4() const
	{
		return I4_;
	}

	// Lower Index of Dimension 4
	int
	l4() const
	{
		return I4_.l();
	}

	// Upper Index of Dimension 4
	int
	u4() const
	{
		return I4_.u();
	}

	// Size of Dimension 4
	size_type
	size4() const
	{
		return z4_;
	}

	// Size of Dimension 4
	int
	isize4() const
	{
		return static_cast< int >( z4_ );
	}

	// IndexRange of Dimension 5
	IR const &
	I5() const
	{
		return I5_;
	}

	// Lower Index of Dimension 5
	int
	l5() const
	{
		return I5_.l();
	}

	// Upper Index of Dimension 5
	int
	u5() const
	{
		return I5_.u();
	}

	// Size of Dimension 5
	size_type
	size5() const
	{
		return z5_;
	}

	// Size of Dimension 5
	int
	isize5() const
	{
		return static_cast< int >( z5_ );
	}

public: // Modifier

	// Clear
	Array5 &
	clear()
	{
		Super::clear();
		I1_.clear();
		I2_.clear();
		I3_.clear();
		I4_.clear();
		I5_.clear();
		z1_ = z2_ = z3_ = z4_ = z5_ = 0u;
		return *this;
	}


protected: // Functions

	// Dimension by IndexRange
	virtual
	bool
	dimension_assign( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) = 0;

	// Clear on Move
	void
	clear_move()
	{
		I1_.clear();
		I2_.clear();
		I3_.clear();
		I4_.clear();
		I5_.clear();
		z1_ = z2_ = z3_ = z4_ = z5_ = 0u;
	}

	// Swap
	void
	swap5( Array5 & v )
	{
		swapB( v );
		I1_.swap( v.I1_ );
		I2_.swap( v.I2_ );
		I3_.swap( v.I3_ );
		I4_.swap( v.I4_ );
		I5_.swap( v.I5_ );
		std::swap( z1_, v.z1_ );
		std::swap( z2_, v.z2_ );
		std::swap( z3_, v.z3_ );
		std::swap( z4_, v.z4_ );
		std::swap( z5_, v.z5_ );
	}

protected: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2
	IR I3_; // Index range of dim 3
	IR I4_; // Index range of dim 4
	IR I5_; // Index range of dim 5

	size_type z1_; // Size of dim 1
	size_type z2_; // Size of dim 2
	size_type z3_; // Size of dim 3
	size_type z4_; // Size of dim 4
	size_type z5_; // Size of dim 5

}; // Array5

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array5< U > const & a, Array5< V > const & b )
{
	return a.conformable( b );
}

// Equal Dimensions?
template< typename U, typename V >
inline
bool
equal_dimensions( Array5< U > const & a, Array5< V > const & b )
{
	return a.equal_dimensions( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_Array5_hh_INCLUDED
