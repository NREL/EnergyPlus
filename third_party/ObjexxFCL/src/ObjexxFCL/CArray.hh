#ifndef ObjexxFCL_CArray_hh_INCLUDED
#define ObjexxFCL_CArray_hh_INCLUDED

// CArray: Memory-Managed C Array Wrapper
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
#include <ObjexxFCL/CArray.fwd.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <initializer_list>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// CArray: Memory-Managed C Array Wrapper
template< typename T >
class CArray
{

private: // Friend

	template< typename > friend class CArray; // Friendship across value types

public: // Types

	// STL Style
	typedef  T  value_type;
	typedef  T &  reference;
	typedef  T const &  const_reference;
	typedef  T *  pointer;
	typedef  T const *  const_pointer;
	typedef  T *  iterator;
	typedef  T const *  const_iterator;
	typedef  std::size_t  size_type;
	typedef  std::ptrdiff_t  difference_type;

	// C++ Style
	typedef  T  Value;
	typedef  T &  Reference;
	typedef  T const &  ConstReference;
	typedef  T *  Pointer;
	typedef  T const *  ConstPointer;
	typedef  T *  Iterator;
	typedef  T const *  ConstIterator;
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;

	// Types to prevent compile failure when std::distance is in scope
	typedef  void  iterator_category;

public: // Creation

	// Default Constructor
	inline
	CArray() :
	 size_( 0 ),
	 data_( nullptr )
	{}

	// Copy Constructor
	inline
	CArray( CArray const & a ) :
	 size_( a.size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = a.data_[ i ];
		}
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	CArray( CArray< U > const & a ) :
	 size_( a.size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( a.data_[ i ] );
		}
	}

	// Pointer + Size Constructor
	inline
	CArray(
	 T const * const p,
	 size_type const size
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = p[ i ];
		}
	}

	// Pointer + Size Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	CArray(
	 U const * const p,
	 size_type const size
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( p[ i ] );
		}
	}

	// Iterator Range Constructor Template
	template< typename InputIterator >
	inline
	CArray(
	 InputIterator const beg,
	 InputIterator const end
	) :
	 size_( end - beg ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		if ( size_ > 0u ) {
			InputIterator k( beg );
			for ( size_type i = 0; i < size_; ++i, ++k ) {
				data_[ i ] = T( *k );
			}
		}
	}

	// Size Constructor
	//  Built-in value types are not initialized
	inline
	explicit
	CArray( size_type const size ) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{}

	// Size + Uniform Value Constructor
	inline
	CArray(
	 size_type const size,
	 T const & t
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = t;
		}
	}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	CArray( std::initializer_list< U > const l ) :
	 size_( l.size() ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		std::copy( l.begin(), l.end(), data_ );
	}

	// Destructor
	inline
	~CArray()
	{
		delete[] data_;
	}

public: // Conversion

	// Active?
	inline
	operator bool() const
	{
		return ( data_ != nullptr );
	}

public: // Assignment

	// Copy Assignment
	inline
	CArray &
	operator =( CArray const & a )
	{
		if ( this != &a ) {
			if ( size_ != a.size_ ) {
				size_ = a.size_;
				delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = a.data_[ i ];
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArray &
	operator =( CArray< U > const & a )
	{
		if ( size_ != a.size_ ) {
			size_ = a.size_;
			delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
		}
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( a.data_[ i ] );
		}
		return *this;
	}

	// Uniform Value Assignment
	inline
	CArray &
	operator =( T const & t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = t;
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArray &
	operator =( std::initializer_list< U > const l )
	{
		assert( l.size() == size_ );
		std::copy( l.begin(), l.end(), data_ );
		return *this;
	}

	// Pointer + Size Assignment
	inline
	CArray &
	assign(
	 T const * const p,
	 size_type const size
	)
	{
		if ( size_ != size ) {
			size_ = size;
			delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
		}
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = p[ i ];
		}
		return *this;
	}

	// Pointer + Size Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArray &
	assign(
	 U const * const p,
	 size_type const size
	)
	{
		if ( size_ != size ) {
			size_ = size;
			delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
		}
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( p[ i ] );
		}
		return *this;
	}

	// Iterator Range Assignment Template
	template< typename InputIterator >
	inline
	CArray &
	assign(
	 InputIterator const beg,
	 InputIterator const end
	)
	{
		size_type const size( end - beg );
		if ( size_ != size ) {
			size_ = size;
			delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
		}
		if ( size_ > 0u ) {
			InputIterator k( beg );
			for ( size_type i = 0; i < size_; ++i, ++k ) {
				data_[ i ] = T( *k );
			}
		}
		return *this;
	}

	// Size + Value Assignment
	inline
	CArray &
	assign(
	 size_type const size,
	 T const & value
	)
	{
		if ( size_ != size ) { // Set to new array with uniform values
			CArray( size, value ).swap( *this );
		} else { // Set to uniform value
			(*this) = value;
		}
		return *this;
	}

	// += CArray
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArray &
	operator +=( CArray< U > const & a )
	{
		assert( size_ == a.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += T( a.data_[ i ] );
		}
		return *this;
	}

	// -= CArray
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArray &
	operator -=( CArray< U > const & a )
	{
		assert( size_ == a.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= T( a.data_[ i ] );
		}
		return *this;
	}

	// += Value
	inline
	CArray &
	operator +=( T const & t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += t;
		}
		return *this;
	}

	// -= Value
	inline
	CArray &
	operator -=( T const & t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= t;
		}
		return *this;
	}

	// *= Value
	inline
	CArray &
	operator *=( T const & t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= t;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	inline
	CArray &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= inv_u;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< !std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	inline
	CArray &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] /= u;
		}
		return *this;
	}

public: // Predicate

	// Active?
	inline
	bool
	active() const
	{
		return ( data_ != nullptr );
	}

	// Empty?
	inline
	bool
	empty() const
	{
		return ( size_ == 0u );
	}

public: // Inspector

	// Size
	inline
	size_type
	size() const
	{
		return size_;
	}

	// First Element
	inline
	T const &
	front() const
	{
		assert( size_ > 0u );
		return data_[ 0 ];
	}

	// Last Element
	inline
	T const &
	back() const
	{
		assert( size_ > 0u );
		return data_[ size_ - 1 ];
	}

	// Length
	inline
	T
	length() const
	{
		T length_sq( T( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			T const data_i( data_[ i ] );
			length_sq += data_i * data_i;
		}
		return std::sqrt( length_sq );
	}

	// Length Squared
	inline
	T
	length_squared() const
	{
		T length_sq( T( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			T const data_i( data_[ i ] );
			length_sq += data_i * data_i;
		}
		return length_sq;
	}

public: // Modifier

	// First Element
	inline
	T &
	front()
	{
		assert( size_ > 0u );
		return data_[ 0 ];
	}

	// Last Element
	inline
	T &
	back()
	{
		assert( size_ > 0u );
		return data_[ size_ - 1 ];
	}

	// Resize: Values not Preserved
	// Built-in values are uninitialized if size changes
	inline
	CArray &
	size( size_type const size )
	{
		if ( size_ != size ) { // Set to new array
			CArray( size ).swap( *this );
		}
		return *this;
	}

	// Resize to Size With Fill Value: Values Preserved
	inline
	CArray &
	resize(
	 size_type const size,
	 T const & fill = T()
	)
	{
		if ( size_ < size ) {
			CArray a( size, fill ); // New array: Elements set to fill fill
			for ( size_type i = 0; i < size_; ++i ) { // Copy current values
				a.data_[ i ] = data_[ i ];
			}
			swap( a ); // Swap in new array
		} else if ( size_ > size ) {
			CArray a( size ); // New array
			for ( size_type i = 0; i < size; ++i ) { // Copy current values within new range
				a.data_[ i ] = data_[ i ];
			}
			swap( a ); // Swap in new array
		}
		return *this;
	}

	// Swap
	inline
	void
	swap( CArray & a )
	{
		std::swap( size_, a.size_ );
		std::swap( data_, a.data_ );
	}

	// Clear
	inline
	CArray &
	clear()
	{
		size_ = 0u;
		delete[] data_; data_ = nullptr;
		return *this;
	}

	// Normalize to Unit Length
	inline
	CArray &
	normalize()
	{
		T const length_( length() );
		assert( length_ > T( 0 ) );
		operator /=( length_ );
		return *this;
	}

public: // Subscript

	// CArray[ i ] const: 0-Based Indexing
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// CArray[ i ]: 0-Based Indexing
	inline
	T &
	operator []( size_type const i )
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// CArray( i ) const: 1-Based Indexing
	inline
	T const &
	operator ()( size_type const i ) const
	{
		assert( ( i > 0u ) && ( i <= size_ ) );
		return data_[ i - 1 ];
	}

	// CArray( i ): 1-Based Indexing
	inline
	T &
	operator ()( size_type const i )
	{
		assert( ( i > 0u ) && ( i <= size_ ) );
		return data_[ i - 1 ];
	}

public: // Iterator

	// const_iterator to Beginning of Array
	inline
	const_iterator
	begin() const
	{
		return data_;
	}

	// iterator to Beginning of Array
	inline
	iterator
	begin()
	{
		return data_;
	}

	// const_iterator to Element Past End of Array
	inline
	const_iterator
	end() const
	{
		return data_ + size_;
	}

	// iterator to Element Past End of Array
	inline
	iterator
	end()
	{
		return data_ + size_;
	}

public: // Array Accessor

	// C Array const Accessor
	inline
	T const &
	operator ()() const
	{
		return data_;
	}

	// C Array Non-const Accessor
	inline
	T &
	operator ()()
	{
		return data_;
	}

private: // Data

	size_type size_; // Number of array elements

	T * data_; // C array

}; // CArray

// Functions

// Magnitude
template< typename T >
inline
T
magnitude( CArray< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const a_i( a[ i ] );
		mag_sq += a_i * a_i;
	}
	return std::sqrt( mag_sq );
}

// Magnitude Squared
template< typename T >
inline
T
magnitude_squared( CArray< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const a_i( a[ i ] );
		mag_sq += a_i * a_i;
	}
	return mag_sq;
}

// Distance
template< typename T >
inline
T
distance( CArray< T > const & a, CArray< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const distance_i( a[ i ] - b[ i ] );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance Squared
template< typename T >
inline
T
distance_squared( CArray< T > const & a, CArray< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const distance_i( a[ i ] - b[ i ] );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Dot Product
template< typename T >
inline
T
dot( CArray< T > const & a, CArray< T > const & b )
{
	assert( a.size() == b.size() );
	T sum( T( 0 ) );
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		sum += a[ i ] * b[ i ];
	}
	return sum;
}

// Dot Product
template< typename T >
inline
T
dot_product( CArray< T > const & a, CArray< T > const & b )
{
	assert( a.size() == b.size() );
	T sum( T( 0 ) );
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		sum += a[ i ] * b[ i ];
	}
	return sum;
}

// Swap
template< typename T >
inline
void
swap( CArray< T > & a, CArray< T > & b )
{
	a.swap( b );
}

// Comparison

// Are Two CArrays Comparable?
template< typename T >
inline
bool
comparable( CArray< T > const & a, CArray< T > const & b )
{
	return ( a.size() == b.size() );
}

// CArray == CArray
template< typename T >
inline
bool
operator ==( CArray< T > const & a, CArray< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] == b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArray != CArray
template< typename T >
inline
bool
operator !=( CArray< T > const & a, CArray< T > const & b )
{
	return !( a == b );
}

// CArray < CArray
template< typename T >
inline
bool
operator <( CArray< T > const & a, CArray< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return false;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] < b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArray <= CArray
template< typename T >
inline
bool
operator <=( CArray< T > const & a, CArray< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] <= b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArray >= CArray
template< typename T >
inline
bool
operator >=( CArray< T > const & a, CArray< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] >= b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArray > CArray
template< typename T >
inline
bool
operator >( CArray< T > const & a, CArray< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return false;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] > b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArray == Value
template< typename T >
inline
bool
operator ==( CArray< T > const & a, T const & t )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( a[ i ] != t ) return false;
	}
	return true;
}

// CArray != Value
template< typename T >
inline
bool
operator !=( CArray< T > const & a, T const & t )
{
	return !( a == t );
}

// CArray < Value
template< typename T >
inline
bool
operator <( CArray< T > const & a, T const & t )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] < t ) ) return false;
	}
	return true;
}

// CArray <= Value
template< typename T >
inline
bool
operator <=( CArray< T > const & a, T const & t )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] <= t ) ) return false;
	}
	return true;
}

// CArray >= Value
template< typename T >
inline
bool
operator >=( CArray< T > const & a, T const & t )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] >= t ) ) return false;
	}
	return true;
}

// CArray > Value
template< typename T >
inline
bool
operator >( CArray< T > const & a, T const & t )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] > t ) ) return false;
	}
	return true;
}

// Value == CArray
template< typename T >
inline
bool
operator ==( T const & t, CArray< T > const & a )
{
	return ( a == t );
}

// Value != CArray
template< typename T >
inline
bool
operator !=( T const & t, CArray< T > const & a )
{
	return !( t == a );
}

// Value < CArray
template< typename T >
inline
bool
operator <( T const & t, CArray< T > const & a )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t < a[ i ] ) ) return false;
	}
	return true;
}

// Value <= CArray
template< typename T >
inline
bool
operator <=( T const & t, CArray< T > const & a )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t <= a[ i ] ) ) return false;
	}
	return true;
}

// Value >= CArray
template< typename T >
inline
bool
operator >=( T const & t, CArray< T > const & a )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t >= a[ i ] ) ) return false;
	}
	return true;
}

// Value > CArray
template< typename T >
inline
bool
operator >( T const & t, CArray< T > const & a )
{
	for ( typename CArray< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t > a[ i ] ) ) return false;
	}
	return true;
}

// Generator

// -CArray
template< typename T >
inline
CArray< T >
operator -( CArray< T > const & a )
{
	CArray< T > r( a );
	r *= T( -1 );
	return r;
}

// CArray + CArray
template< typename T >
inline
CArray< T >
operator +( CArray< T > const & a, CArray< T > const & b )
{
	CArray< T > r( a );
	r += b;
	return r;
}

// CArray - CArray
template< typename T >
inline
CArray< T >
operator -( CArray< T > const & a, CArray< T > const & b )
{
	CArray< T > r( a );
	r -= b;
	return r;
}

// CArray + Value
template< typename T >
inline
CArray< T >
operator +( CArray< T > const & a, T const & t )
{
	CArray< T > r( a );
	r += t;
	return r;
}

// Value + CArray
template< typename T >
inline
CArray< T >
operator +( T const & t, CArray< T > const & a )
{
	CArray< T > r( a );
	r += t;
	return r;
}

// CArray - Value
template< typename T >
inline
CArray< T >
operator -( CArray< T > const & a, T const & t )
{
	CArray< T > r( a );
	r -= t;
	return r;
}

// Value - CArray
template< typename T >
inline
CArray< T >
operator -( T const & t, CArray< T > const & a )
{
	CArray< T > r( -a );
	r += t;
	return r;
}

// CArray * Value
template< typename T >
inline
CArray< T >
operator *( CArray< T > const & a, T const & t )
{
	CArray< T > r( a );
	r *= t;
	return r;
}

// Value * CArray
template< typename T >
inline
CArray< T >
operator *( T const & t, CArray< T > const & a )
{
	CArray< T > r( a );
	r *= t;
	return r;
}

// CArray / Value
template< typename T >
inline
CArray< T >
operator /( CArray< T > const & a, T const & t )
{
	CArray< T > r( a );
	r /= t;
	return r;
}

} // ObjexxFCL

#endif // ObjexxFCL_CArray_hh_INCLUDED
