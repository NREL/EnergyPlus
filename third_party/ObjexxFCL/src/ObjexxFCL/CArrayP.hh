#ifndef ObjexxFCL_CArrayP_hh_INCLUDED
#define ObjexxFCL_CArrayP_hh_INCLUDED

// CArrayP: Memory-Managed C Array Wrapper Supporting Proxies
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
#include <ObjexxFCL/CArrayP.fwd.hh>
#include <ObjexxFCL/proxy_const_assert.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <initializer_list>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// CArrayP: Memory-Managed C Array Wrapper Supporting Proxies
//
//  Proxy CArrayPs are invalidated if the underlying (owning) array data is deleted
//  Proxy CPArras can be created at construction with the Proxy named constructors
//  CArrayPs can become proxies with the attach() member function
//  CArrayPs can stop being proxies with the detach() member function
template< typename T >
class CArrayP
{

private: // Friend

	template< typename > friend class CArrayP; // Friendship across value types

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
	CArrayP() :
	 size_( 0 ),
	 data_( nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{}

	// Copy Constructor
	inline
	CArrayP( CArrayP const & a ) :
	 size_( a.size_ ),
	 data_( a.owner_ ? ( size_ > 0u ? new T[ size_ ] : nullptr ) : a.data_ ),
	 owner_( a.owner_ )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( ! a.owner_ )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{
		if ( owner_ ) {
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = a.data_[ i ];
			}
		}
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	// Non-Const Copy Constructor
	inline
	CArrayP( CArrayP & a ) :
	 size_( a.size_ ),
	 data_( a.owner_ ? ( size_ > 0u ? new T[ size_ ] : nullptr ) : a.data_ ),
	 owner_( a.owner_ ),
	 const_proxy_( a.const_proxy_ )
	{
		if ( owner_ ) {
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = a.data_[ i ];
			}
		}
	}
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	CArrayP( CArrayP< U > const & a ) :
	 size_( a.size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( a.data_[ i ] );
		}
	}

	// Pointer + Size Constructor
	inline
	CArrayP(
	 T const * const p,
	 size_type const size
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = p[ i ];
		}
	}

	// Pointer + Size Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	CArrayP(
	 U const * const p,
	 size_type const size
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( p[ i ] );
		}
	}

	// Iterator Range Constructor Template
	template< typename InputIterator >
	inline
	CArrayP(
	 InputIterator const beg,
	 InputIterator const end
	) :
	 size_( end - beg ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
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
	CArrayP( size_type const size ) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{}

	// Size + Uniform Value Constructor
	inline
	CArrayP(
	 size_type const size,
	 T const & t
	) :
	 size_( size ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = t;
		}
	}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	CArrayP( std::initializer_list< U > const l ) :
	 size_( l.size() ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr ),
	 owner_( true )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( false )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{
		std::copy( l.begin(), l.end(), data_ );
	}

	// Proxy Copy Named Constructor
	static
	inline
	CArrayP
	Proxy( CArrayP const & a )
	{
		CArrayP p;
		p.size_ = a.size_;
		p.data_ = a.data_;
		p.owner_ = false;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		p.const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		return p;
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	// Non-Const Proxy Copy Named Constructor
	static
	inline
	CArrayP
	Proxy( CArrayP & a )
	{
		CArrayP p;
		p.size_ = a.size_;
		p.data_ = a.data_;
		p.owner_ = false;
		p.const_proxy_ = a.const_proxy_;
		return p;
	}
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Proxy Copy + Size Named Constructor
	static
	inline
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
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		p.const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		return p;
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	// Non-Const Proxy Copy + Size Named Constructor
	static
	inline
	CArrayP
	Proxy(
	 CArrayP & a,
	 size_type const size
	)
	{
		assert( size <= a.size_ );
		CArrayP p;
		p.size_ = size;
		p.data_ = a.data_;
		p.owner_ = false;
		p.const_proxy_ = a.const_proxy_;
		return p;
	}
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Destructor
	inline
	~CArrayP()
	{
		if ( owner_ ) delete[] data_;
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
	CArrayP &
	operator =( CArrayP const & a )
	{
		proxy_const_assert( not_const_proxy() );
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

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArrayP &
	operator =( CArrayP< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
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
	inline
	CArrayP &
	operator =( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = t;
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArrayP &
	operator =( std::initializer_list< U > const l )
	{
		assert( l.size() == size_ );
		std::copy( l.begin(), l.end(), data_ );
		return *this;
	}

	// Pointer + Size Assignment
	inline
	CArrayP &
	assign(
	 T const * const p,
	 size_type const size
	)
	{
		proxy_const_assert( not_const_proxy() );
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
	inline
	CArrayP &
	assign(
	 U const * const p,
	 size_type const size
	)
	{
		proxy_const_assert( not_const_proxy() );
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
	inline
	CArrayP &
	assign(
	 InputIterator const beg,
	 InputIterator const end
	)
	{
		proxy_const_assert( not_const_proxy() );
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
	inline
	CArrayP &
	assign(
	 size_type const size,
	 T const & value
	)
	{
		proxy_const_assert( not_const_proxy() );
		if ( size_ != size ) { // Set to new array with uniform values
			assert( owner_ );
			CArrayP( size, value ).swap( *this );
		} else { // Set to uniform value
			(*this) = value;
		}
		return *this;
	}

	// += CArrayP
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArrayP &
	operator +=( CArrayP< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_ == a.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += T( a.data_[ i ] );
		}
		return *this;
	}

	// -= CArrayP
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	CArrayP &
	operator -=( CArrayP< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_ == a.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= T( a.data_[ i ] );
		}
		return *this;
	}

	// += Value
	inline
	CArrayP &
	operator +=( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += t;
		}
		return *this;
	}

	// -= Value
	inline
	CArrayP &
	operator -=( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= t;
		}
		return *this;
	}

	// *= Value
	inline
	CArrayP &
	operator *=( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= t;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	inline
	CArrayP &
	operator /=( U const & u )
	{
		proxy_const_assert( not_const_proxy() );
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
	CArrayP &
	operator /=( U const & u )
	{
		proxy_const_assert( not_const_proxy() );
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

	// Owner?
	inline
	bool
	owner() const
	{
		return owner_;
	}

	// Proxy?
	inline
	bool
	proxy() const
	{
		return ! owner_;
	}

public: // Inspector

	// Size
	inline
	size_type
	size() const
	{
		return size_;
	}

	// Lower Index
	inline
	size_type
	l() const
	{
		return 0u;
	}

	// Upper index
	inline
	size_type
	u() const
	{
		return size_ - 1u; // npos if size_ == 0
	}

	// First element
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
		proxy_const_assert( not_const_proxy() );
		assert( size_ > 0u );
		return data_[ 0 ];
	}

	// Last Element
	inline
	T &
	back()
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_ > 0u );
		return data_[ size_ - 1 ];
	}

	// Resize: Values not Preserved
	// Built-in values are uninitialized if size changes
	inline
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
	inline
	CArrayP &
	resize(
	 size_type const size,
	 T const & fill = T()
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
	inline
	void
	swap( CArrayP & a )
	{
		std::swap( size_, a.size_ );
		std::swap( data_, a.data_ );
		std::swap( owner_, a.owner_ );
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		std::swap( const_proxy_, a.const_proxy_ );
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	}

	// Clear
	inline
	CArrayP &
	clear()
	{
		size_ = 0u;
		if ( owner_ ) delete[] data_; data_ = nullptr;
		owner_ = true;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = false;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		return *this;
	}

	// Normalize to Unit Length
	inline
	CArrayP &
	normalize()
	{
		T const length_( length() );
		assert( length_ > T( 0 ) );
		operator /=( length_ );
		return *this;
	}

	// Attach as Proxy to a const CArrayP
	inline
	CArrayP &
	attach( CArrayP const & a )
	{
		size_ = a.size_;
		if ( owner_ ) delete[] data_; data_ = a.data_;
		owner_ = false;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		return *this;
	}

	// Attach as Proxy to a CArrayP
	inline
	CArrayP &
	attach( CArrayP & a )
	{
		size_ = a.size_;
		if ( owner_ ) delete[] data_; data_ = a.data_;
		owner_ = false;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = a.const_proxy_;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		return *this;
	}

	// Detach as Proxy to a CArrayP
	inline
	CArrayP &
	detach()
	{
		if ( ! owner_ ) clear(); // Proxy: Clear fields
		return *this;
	}

public: // Subscript

	// CArrayP[ i ] const: 0-Based Indexing
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// CArrayP[ i ]: 0-Based Indexing
	inline
	T &
	operator []( size_type const i )
	{
		proxy_const_assert( not_const_proxy() );
		assert( i < size_ );
		return data_[ i ];
	}

	// CArrayP( i ) const: 1-Based Indexing
	inline
	T const &
	operator ()( size_type const i ) const
	{
		assert( ( i > 0u ) && ( i <= size_ ) );
		return data_[ i - 1 ];
	}

	// CArrayP( i ): 1-Based Indexing
	inline
	T &
	operator ()( size_type const i )
	{
		proxy_const_assert( not_const_proxy() );
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
		proxy_const_assert( not_const_proxy() );
		return data_;
	}

	// const_iterator to Element Past End of Array
	inline
	const_iterator
	end() const
	{
		return data_ + size_;
	}

	// iterator to element past end of array
	inline
	iterator
	end()
	{
		proxy_const_assert( not_const_proxy() );
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
		proxy_const_assert( not_const_proxy() );
		return data_;
	}

private: // Functions

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Const Proxy?
	inline
	bool
	const_proxy() const
	{
		return const_proxy_;
	}

	// Not a Const Proxy Under Strict Const-Correctness?
	inline
	bool
	not_const_proxy() const
	{
		return ! const_proxy_;
	}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

private: // Data

	size_type size_; // Number of array elements

	T * data_; // C array

	bool owner_; // Owner of the data array or proxy?

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	bool const_proxy_; // Proxy for const data array?
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

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
		T const a_i( a[ i ] );
		mag_sq += a_i * a_i;
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
		T const a_i( a[ i ] );
		mag_sq += a_i * a_i;
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
operator ==( CArrayP< T > const & a, T const & t )
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
operator !=( CArrayP< T > const & a, T const & t )
{
	return !( a == t );
}

// CArrayP < Value
template< typename T >
inline
bool
operator <( CArrayP< T > const & a, T const & t )
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
operator <=( CArrayP< T > const & a, T const & t )
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
operator >=( CArrayP< T > const & a, T const & t )
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
operator >( CArrayP< T > const & a, T const & t )
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
operator ==( T const & t, CArrayP< T > const & a )
{
	return ( a == t );
}

// Value != CArrayP
template< typename T >
inline
bool
operator !=( T const & t, CArrayP< T > const & a )
{
	return !( t == a );
}

// Value < CArrayP
template< typename T >
inline
bool
operator <( T const & t, CArrayP< T > const & a )
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
operator <=( T const & t, CArrayP< T > const & a )
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
operator >=( T const & t, CArrayP< T > const & a )
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
operator >( T const & t, CArrayP< T > const & a )
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
operator +( CArrayP< T > const & a, T const & t )
{
	CArrayP< T > r( a );
	r += t;
	return r;
}

// Value + CArrayP
template< typename T >
inline
CArrayP< T >
operator +( T const & t, CArrayP< T > const & a )
{
	CArrayP< T > r( a );
	r += t;
	return r;
}

// CArrayP - Value
template< typename T >
inline
CArrayP< T >
operator -( CArrayP< T > const & a, T const & t )
{
	CArrayP< T > r( a );
	r -= t;
	return r;
}

// Value - CArrayP
template< typename T >
inline
CArrayP< T >
operator -( T const & t, CArrayP< T > const & a )
{
	CArrayP< T > r( -a );
	r += t;
	return r;
}

// CArrayP * Value
template< typename T >
inline
CArrayP< T >
operator *( CArrayP< T > const & a, T const & t )
{
	CArrayP< T > r( a );
	r *= t;
	return r;
}

// Value * CArrayP
template< typename T >
inline
CArrayP< T >
operator *( T const & t, CArrayP< T > const & a )
{
	CArrayP< T > r( a );
	r *= t;
	return r;
}

// CArrayP / Value
template< typename T >
inline
CArrayP< T >
operator /( CArrayP< T > const & a, T const & t )
{
	CArrayP< T > r( a );
	r /= t;
	return r;
}

} // ObjexxFCL

#ifndef NO_STD_SWAP_OVERLOADS

// std::swap Overloads for Efficiency
//
// Technically you cannot add template functions overloads to namespace std
// but this works with most compilers and makes it much faster if someone uses
// std::swap instead of swap or ObjexxFCL::swap.  The legal alternative would be
// to add specializations of swap for each anticipated instantiation.

namespace std {

// std::swap( CArrayP, CArrayP )
template< typename T >
inline
void
swap( ObjexxFCL::CArrayP< T > & a, ObjexxFCL::CArrayP< T > & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_CArrayP_hh_INCLUDED
