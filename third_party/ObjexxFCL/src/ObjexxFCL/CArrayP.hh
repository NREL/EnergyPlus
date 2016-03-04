#ifndef ObjexxFCL_CArrayP_hh_INCLUDED
#define ObjexxFCL_CArrayP_hh_INCLUDED

// CArrayP: Memory-Managed C Array Wrapper Supporting Proxies
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
#include <ObjexxFCL/CArrayP.fwd.hh>
#include <ObjexxFCL/noexcept.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <initializer_list>
#include <iomanip>
#include <istream>
#include <iterator>
#include <ostream>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// CArrayP: Memory-Managed C Array Wrapper Supporting Proxies
//
//  Proxy CArrayPs are invalidated if the underlying (owning) array data is deleted
//  Proxy CArrayPs can be created at construction with the Proxy named constructors
//  CArrayPs can become proxies with the attach() member function
//  CArrayPs can stop being proxies with the detach() member function
template< typename T >
class CArrayP
{

private: // Friend

	template< typename > friend class CArrayP; // Friendship across value types

public: // Types

	typedef  TypeTraits< T >  Traits;
	typedef  typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type  Tc;
	typedef  typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type  Tr;

	// STL Style
	typedef  T  value_type;
	typedef  T &  reference;
	typedef  T const &  const_reference;
	typedef  T *  pointer;
	typedef  T const *  const_pointer;
	typedef  T *  iterator;
	typedef  T const *  const_iterator;
	typedef  std::reverse_iterator< T * >  reverse_iterator;
	typedef  std::reverse_iterator< T const * >  const_reverse_iterator;
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
	typedef  std::reverse_iterator< T * >  ReverseIterator;
	typedef  std::reverse_iterator< T const * >  ConstReverseIterator;
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;

	// Types to prevent compile failure when std::distance is in scope
	typedef  void  iterator_category;

public: // Creation

	// Default Constructor
	CArrayP() :
	 size_( 0u ),
	 data_( nullptr ),
	 owner_( true )
	{}

	// Copy Constructor
	CArrayP( CArrayP const & a ) :
	 size_( a.size_ ),
	 data_( a.owner_ ? ( size_ > 0u ? new T[ size_ ] : nullptr ) : a.data_ ),
	 owner_( a.owner_ )
	{
		if ( owner_ ) {
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = a.data_[ i ];
			}
		}
	}

	// Move Constructor
	CArrayP( CArrayP && a ) NOEXCEPT :
	 size_( a.size_ ),
	 data_( a.data_ ),
	 owner_( a.owner_ )
	{
		a.size_ = 0u;
		a.data_ = nullptr;
		a.owner_ = true;
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	CArrayP( CArrayP< U > const & a ) :
	 size_( a.size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( a.data_[ i ] );
		}
	}

	// Pointer + Size Constructor
	CArrayP(
	 T const * const p,
	 size_type const size
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = p[ i ];
		}
	}

	// Pointer + Size Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	CArrayP(
	 U const * const p,
	 size_type const size
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( p[ i ] );
		}
	}

	// Iterator Range Constructor Template
	template< typename InputIterator >
	CArrayP(
	 InputIterator const beg,
	 InputIterator const end
	) :
	 size_( end - beg ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
	{
		if ( size_ > 0u ) {
			InputIterator k( beg );
			for ( size_type i = 0; i < size_; ++i, ++k ) {
				data_[ i ] = T( *k );
			}
		}
	}

	// Size Constructor: Built-in types are default, not zero, initialized for performance
	explicit
	CArrayP( size_type const size ) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
	{}

	// Size + Uniform Value Constructor
	CArrayP(
	 size_type const size,
	 Tc t
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = t;
		}
	}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	CArrayP( std::initializer_list< U > const l ) :
	 size_( l.size() ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
	{
		std::copy( l.begin(), l.end(), data_ );
	}

	// Proxy Copy Named Constructor
	static
	CArrayP
	Proxy( CArrayP const & a )
	{
		CArrayP p;
		p.size_ = a.size_;
		p.data_ = a.data_;
		p.owner_ = false;
		return p;
	}

	// Proxy Copy + Size Named Constructor
	static
	CArrayP
	Proxy(
	 CArrayP const & a,
	 size_type const size
	)
	{
		assert( size <= a.size_ );
		CArrayP p;
		p.size_ = size;
		p.data_ = a.data_;
		p.owner_ = false;
		return p;
	}

	// Destructor
	~CArrayP()
	{
		if ( owner_ ) delete[] data_;
	}

public: // Conversion

	// Active?
	operator bool() const
	{
		return ( data_ != nullptr );
	}

	// Data
	operator T const *() const
	{
		return data_;
	}

	// Data
	operator T *()
	{
		return data_;
	}

public: // Assignment

	// Copy Assignment
	CArrayP &
	operator =( CArrayP const & a )
	{
		if ( this != &a ) {
			if ( size_ != a.size_ ) {
				assert( owner_ );
				size_ = a.size_;
				delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = a.data_[ i ];
			}
		}
		return *this;
	}

	// Move Assignment
	CArrayP &
	operator =( CArrayP && a ) NOEXCEPT
	{
		if ( owner_ && a.owner_ ) {
			assert( this != &a );
			size_ = a.size_;
			delete[] data_; data_ = a.data_;
			a.size_ = 0u;
			a.data_ = nullptr;
			return *this;
		} else {
			return operator =( a );
		}
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	CArrayP &
	operator =( CArrayP< U > const & a )
	{
		if ( size_ != a.size_ ) {
			assert( owner_ );
			size_ = a.size_;
			delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
		}
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( a.data_[ i ] );
		}
		return *this;
	}

	// Uniform Value Assignment
	CArrayP &
	operator =( Tc t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = t;
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	CArrayP &
	operator =( std::initializer_list< U > const l )
	{
		assert( l.size() == size_ );
		std::copy( l.begin(), l.end(), data_ );
		return *this;
	}

	// Pointer + Size Assignment
	CArrayP &
	assign(
	 T const * const p,
	 size_type const size
	)
	{
		if ( size_ != size ) {
			assert( owner_ );
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
	CArrayP &
	assign(
	 U const * const p,
	 size_type const size
	)
	{
		if ( size_ != size ) {
			assert( owner_ );
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
	CArrayP &
	assign(
	 InputIterator const beg,
	 InputIterator const end
	)
	{
		size_type const size( end - beg );
		if ( size_ != size ) {
			assert( owner_ );
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
	CArrayP &
	assign(
	 size_type const size,
	 Tc t
	)
	{
		if ( size_ != size ) { // Set to new array with uniform values
			assert( owner_ );
			CArrayP( size, t ).swap( *this );
		} else { // Set to uniform value
			(*this) = t;
		}
		return *this;
	}

	// += CArrayP
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	CArrayP &
	operator +=( CArrayP< U > const & a )
	{
		assert( size_ == a.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += T( a.data_[ i ] );
		}
		return *this;
	}

	// -= CArrayP
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	CArrayP &
	operator -=( CArrayP< U > const & a )
	{
		assert( size_ == a.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= T( a.data_[ i ] );
		}
		return *this;
	}

	// += Value
	CArrayP &
	operator +=( Tc t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += t;
		}
		return *this;
	}

	// -= Value
	CArrayP &
	operator -=( Tc t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= t;
		}
		return *this;
	}

	// *= Value
	CArrayP &
	operator *=( Tc t )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= t;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	CArrayP &
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
	template< typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	CArrayP &
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
	bool
	active() const
	{
		return ( data_ != nullptr );
	}

	// Empty?
	bool
	empty() const
	{
		return ( size_ == 0u );
	}

	// Owner?
	bool
	owner() const
	{
		return owner_;
	}

	// Proxy?
	bool
	proxy() const
	{
		return ! owner_;
	}

public: // Inspector

	// Size
	size_type
	size() const
	{
		return size_;
	}

	// Lower Index
	size_type
	l() const
	{
		return 0u;
	}

	// Upper index
	size_type
	u() const
	{
		return size_ - 1u; // npos if size_ == 0
	}

	// First Element
	Tr
	front() const
	{
		assert( size_ > 0u );
		return data_[ 0 ];
	}

	// Last Element
	Tr
	back() const
	{
		assert( size_ > 0u );
		return data_[ size_ - 1 ];
	}

	// Length
	T
	length() const
	{
		T length_sq( T( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			length_sq += data_[ i ] * data_[ i ];
		}
		return std::sqrt( length_sq );
	}

	// Length Squared
	T
	length_squared() const
	{
		T length_sq( T( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			length_sq += data_[ i ] * data_[ i ];
		}
		return length_sq;
	}

public: // Modifier

	// First Element
	T &
	front()
	{
		assert( size_ > 0u );
		return data_[ 0 ];
	}

	// Last Element
	T &
	back()
	{
		assert( size_ > 0u );
		return data_[ size_ - 1 ];
	}

	// Resize: Values not Preserved
	// Built-in values are uninitialized if size changes
	CArrayP &
	size( size_type const size )
	{
		assert( owner_ );
		if ( size_ != size ) { // Set to new array
			CArrayP( size ).swap( *this );
		}
		return *this;
	}

	// Resize to Size With Fill Value: Values Preserved
	CArrayP &
	resize(
	 size_type const size,
	 Tc fill = T()
	)
	{
		assert( owner_ );
		if ( size_ < size ) {
			CArrayP a( size, fill ); // New array: Elements set to fill fill
			for ( size_type i = 0; i < size_; ++i ) { // Copy current values
				a.data_[ i ] = data_[ i ];
			}
			swap( a ); // Swap in new array
		} else if ( size_ > size ) {
			CArrayP a( size ); // New array
			for ( size_type i = 0; i < size; ++i ) { // Copy current values within new range
				a.data_[ i ] = data_[ i ];
			}
			swap( a ); // Swap in new array
		}
		return *this;
	}

	// Swap
	void
	swap( CArrayP & a )
	{
		std::swap( size_, a.size_ );
		std::swap( data_, a.data_ );
		std::swap( owner_, a.owner_ );
	}

	// Clear
	CArrayP &
	clear()
	{
		size_ = 0u;
		if ( owner_ ) delete[] data_; data_ = nullptr;
		owner_ = true;
		return *this;
	}

	// Normalize to Unit Length
	CArrayP &
	normalize()
	{
		T const length_( length() );
		assert( length_ > T( 0 ) );
		operator /=( length_ );
		return *this;
	}

	// Attach as Proxy to a const CArrayP
	CArrayP &
	attach( CArrayP const & a )
	{
		size_ = a.size_;
		if ( owner_ ) delete[] data_; data_ = a.data_;
		owner_ = false;
		return *this;
	}

	// Attach as Proxy to a CArrayP
	CArrayP &
	attach( CArrayP & a )
	{
		size_ = a.size_;
		if ( owner_ ) delete[] data_; data_ = a.data_;
		owner_ = false;
		return *this;
	}

	// Detach as Proxy to a CArrayP
	CArrayP &
	detach()
	{
		if ( ! owner_ ) clear(); // Proxy: Clear fields
		return *this;
	}

public: // Subscript

	// CArrayP[ i ] const: 0-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_unsigned< I >::value && std::is_const< T >::value >::type >
	Tr
	operator []( I const i ) const
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// CArrayP[ i ] const: 0-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_signed< I >::value && std::is_const< T >::value >::type, typename = void >
	Tr
	operator []( I const i ) const
	{
		assert( ( i >= 0 ) && ( static_cast< size_type >( i ) < size_ ) );
		return data_[ i ];
	}

	// CArrayP[ i ]: 0-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_unsigned< I >::value >::type >
	T &
	operator []( I const i )
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// CArrayP[ i ]: 0-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_signed< I >::value >::type, typename = void >
	T &
	operator []( I const i )
	{
		assert( ( i >= 0 ) && ( static_cast< size_type >( i ) < size_ ) );
		return data_[ i ];
	}

	// CArrayP( i ) const: 1-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_unsigned< I >::value && std::is_const< T >::value >::type >
	Tr
	operator ()( I const i ) const
	{
		assert( ( i > 0u ) && ( i <= size_ ) );
		return data_[ i - 1 ];
	}

	// CArrayP( i ) const: 1-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_signed< I >::value && std::is_const< T >::value >::type, typename = void >
	Tr
	operator ()( I const i ) const
	{
		assert( ( i > 0 ) && ( static_cast< size_type >( i ) <= size_ ) );
		return data_[ i - 1 ];
	}

	// CArrayP( i ): 1-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_unsigned< I >::value >::type >
	T &
	operator ()( I const i )
	{
		assert( ( i > 0u ) && ( i <= size_ ) );
		return data_[ i - 1 ];
	}

	// CArrayP( i ): 1-Based Indexing
	template< typename I, class = typename std::enable_if< std::is_integral< I >::value && std::is_signed< I >::value >::type, typename = void >
	T &
	operator ()( I const i )
	{
		assert( ( i > 0 ) && ( static_cast< size_type >( i ) <= size_ ) );
		return data_[ i - 1 ];
	}

public: // Iterator

	// Begin Iterator
	const_iterator
	begin() const
	{
		return data_;
	}

	// Begin Iterator
	iterator
	begin()
	{
		return data_;
	}

	// End Iterator
	const_iterator
	end() const
	{
		return ( data_ != nullptr ? data_ + size_ : nullptr );
	}

	// End Iterator
	iterator
	end()
	{
		return ( data_ != nullptr ? data_ + size_ : nullptr );
	}

	// Reverse Begin Iterator
	const_reverse_iterator
	rbegin() const
	{
		return const_reverse_iterator( data_ != nullptr ? data_ + size_ : nullptr );
	}

	// Reverse Begin Iterator
	reverse_iterator
	rbegin()
	{
		return reverse_iterator( data_ != nullptr ? data_ + size_ : nullptr );
	}

	// Reverse End Iterator
	const_reverse_iterator
	rend() const
	{
		return const_reverse_iterator( data_ );
	}

	// Reverse End Iterator
	reverse_iterator
	rend()
	{
		return reverse_iterator( data_ );
	}

public: // Array Accessor

	// C Array const Accessor
	T const *
	operator ()() const
	{
		return data_;
	}

	// C Array Non-const Accessor
	T *
	operator ()()
	{
		return data_;
	}

private: // Data

	size_type size_; // Number of array elements
	T * data_; // C array
	bool owner_; // Owner of the data array or proxy?

}; // CArrayP

// Functions

// Magnitude
template< typename T >
inline
T
magnitude( CArrayP< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		mag_sq += a[ i ] * a[ i ];
	}
	return std::sqrt( mag_sq );
}

// Magnitude Squared
template< typename T >
inline
T
magnitude_squared( CArrayP< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		mag_sq += a[ i ] * a[ i ];
	}
	return mag_sq;
}

// Distance
template< typename T >
inline
T
distance( CArrayP< T > const & a, CArrayP< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const distance_i( a[ i ] - b[ i ] );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance Squared
template< typename T >
inline
T
distance_squared( CArrayP< T > const & a, CArrayP< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const distance_i( a[ i ] - b[ i ] );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Dot Product
template< typename T >
inline
T
dot( CArrayP< T > const & a, CArrayP< T > const & b )
{
	assert( a.size() == b.size() );
	T sum( T( 0 ) );
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		sum += a[ i ] * b[ i ];
	}
	return sum;
}

// Dot Product
template< typename T >
inline
T
dot_product( CArrayP< T > const & a, CArrayP< T > const & b )
{
	assert( a.size() == b.size() );
	T sum( T( 0 ) );
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		sum += a[ i ] * b[ i ];
	}
	return sum;
}

// Swap
template< typename T >
inline
void
swap( CArrayP< T > & a, CArrayP< T > & b )
{
	a.swap( b );
}

// Comparison

// Are Two CArrays Comparable?
template< typename T >
inline
bool
comparable( CArrayP< T > const & a, CArrayP< T > const & b )
{
	return ( a.size() == b.size() );
}

// CArrayP == CArrayP
template< typename T >
inline
bool
operator ==( CArrayP< T > const & a, CArrayP< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] == b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArrayP != CArrayP
template< typename T >
inline
bool
operator !=( CArrayP< T > const & a, CArrayP< T > const & b )
{
	return !( a == b );
}

// CArrayP < CArrayP
template< typename T >
inline
bool
operator <( CArrayP< T > const & a, CArrayP< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return false;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] < b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArrayP <= CArrayP
template< typename T >
inline
bool
operator <=( CArrayP< T > const & a, CArrayP< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] <= b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArrayP >= CArrayP
template< typename T >
inline
bool
operator >=( CArrayP< T > const & a, CArrayP< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] >= b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArrayP > CArrayP
template< typename T >
inline
bool
operator >( CArrayP< T > const & a, CArrayP< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return false;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] > b[ i ] ) ) return false;
		}
		return true;
	}
}

// CArrayP == Value
template< typename T >
inline
bool
operator ==( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( a[ i ] != t ) return false;
	}
	return true;
}

// CArrayP != Value
template< typename T >
inline
bool
operator !=( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	return !( a == t );
}

// CArrayP < Value
template< typename T >
inline
bool
operator <( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] < t ) ) return false;
	}
	return true;
}

// CArrayP <= Value
template< typename T >
inline
bool
operator <=( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] <= t ) ) return false;
	}
	return true;
}

// CArrayP >= Value
template< typename T >
inline
bool
operator >=( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] >= t ) ) return false;
	}
	return true;
}

// CArrayP > Value
template< typename T >
inline
bool
operator >( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] > t ) ) return false;
	}
	return true;
}

// Value == CArrayP
template< typename T >
inline
bool
operator ==( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	return ( a == t );
}

// Value != CArrayP
template< typename T >
inline
bool
operator !=( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	return !( t == a );
}

// Value < CArrayP
template< typename T >
inline
bool
operator <( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t < a[ i ] ) ) return false;
	}
	return true;
}

// Value <= CArrayP
template< typename T >
inline
bool
operator <=( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t <= a[ i ] ) ) return false;
	}
	return true;
}

// Value >= CArrayP
template< typename T >
inline
bool
operator >=( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t >= a[ i ] ) ) return false;
	}
	return true;
}

// Value > CArrayP
template< typename T >
inline
bool
operator >( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	for ( typename CArrayP< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t > a[ i ] ) ) return false;
	}
	return true;
}

// Generator

// -CArrayP
template< typename T >
inline
CArrayP< T >
operator -( CArrayP< T > const & a )
{
	CArrayP< T > r( a );
	r *= T( -1 );
	return r;
}

// CArrayP + CArrayP
template< typename T >
inline
CArrayP< T >
operator +( CArrayP< T > const & a, CArrayP< T > const & b )
{
	CArrayP< T > r( a );
	r += b;
	return r;
}

// CArrayP - CArrayP
template< typename T >
inline
CArrayP< T >
operator -( CArrayP< T > const & a, CArrayP< T > const & b )
{
	CArrayP< T > r( a );
	r -= b;
	return r;
}

// CArrayP + Value
template< typename T >
inline
CArrayP< T >
operator +( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	CArrayP< T > r( a );
	r += t;
	return r;
}

// Value + CArrayP
template< typename T >
inline
CArrayP< T >
operator +( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	CArrayP< T > r( a );
	r += t;
	return r;
}

// CArrayP - Value
template< typename T >
inline
CArrayP< T >
operator -( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	CArrayP< T > r( a );
	r -= t;
	return r;
}

// Value - CArrayP
template< typename T >
inline
CArrayP< T >
operator -( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	CArrayP< T > r( -a );
	r += t;
	return r;
}

// CArrayP * Value
template< typename T >
inline
CArrayP< T >
operator *( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	CArrayP< T > r( a );
	r *= t;
	return r;
}

// Value * CArrayP
template< typename T >
inline
CArrayP< T >
operator *( typename CArrayP< T >::Tc t, CArrayP< T > const & a )
{
	CArrayP< T > r( a );
	r *= t;
	return r;
}

// CArrayP / Value
template< typename T >
inline
CArrayP< T >
operator /( CArrayP< T > const & a, typename CArrayP< T >::Tc t )
{
	CArrayP< T > r( a );
	r /= t;
	return r;
}

// Stream >> CArrayP
template< typename T >
inline
std::istream &
operator >>( std::istream & stream, CArrayP< T > & a )
{
	typedef  typename CArrayP< T >::size_type  size_type;
	if ( stream && ( ! a.emtpy() ) ) {
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			stream >> a[ i ];
			if ( ! stream ) break;
		}
	}
	return stream;
}

// Stream << CArrayP
template< typename T >
inline
std::ostream &
operator <<( std::ostream & stream, CArrayP< T > const & a )
{
	using std::setw;
	typedef  TypeTraits< T >  Traits;
	typedef  typename CArrayP< T >::size_type  size_type;
	if ( stream && ( ! a.emtpy() ) ) {
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision ) );
		stream << std::right << std::showpoint << std::uppercase;
		size_type const e( a.size() - 1 );
		int const w( Traits::iwidth );
		for ( size_type i = 0; i < e; ++i ) {
			stream << setw( w ) << a[ i ] << ' ';
		} stream << setw( w ) << a[ e ];
		stream.precision( old_precision );
		stream.flags( old_flags );
	}
	return stream;
}

} // ObjexxFCL

#endif // ObjexxFCL_CArrayP_hh_INCLUDED
