#ifndef ObjexxFCL_Chunk_hh_INCLUDED
#define ObjexxFCL_Chunk_hh_INCLUDED

// Chunk: Contiguous Array for Use in ChunkVector
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
#include <ObjexxFCL/noexcept.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// Chunk: Contiguous Array for Use in ChunkVector
//
// Note:
//  size <= capacity
//  capacity == size after construction
//  capacity == size after assignment if reallocation required
template< typename T >
class Chunk
{

private: // Friend

	template< typename > friend class Chunk;

public: // Types

	typedef  typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type  Tc;
	typedef  typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type  Tr;

	// STL style
	typedef  T  value_type;
	typedef  T &  reference;
	typedef  T const &  const_reference;
	typedef  T *  pointer;
	typedef  T const *  const_pointer;
	typedef  std::size_t  size_type;

	// C++ style
	typedef  T  Value;
	typedef  T &  Reference;
	typedef  T const &  ConstReference;
	typedef  T *  Pointer;
	typedef  T const *  ConstPointer;
	typedef  std::size_t  Size;

public: // Creation

	// Default Constructor
	Chunk() :
	 size_( 0u ),
	 capacity_( 0u ),
	 data_( nullptr )
	{}

	// Copy Constructor
	Chunk( Chunk const & c ) :
	 size_( c.size_ ),
	 capacity_( size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = c.data_[ i ];
		}
	}

	// Move Constructor
	Chunk( Chunk && c ) NOEXCEPT :
	 size_( c.size_ ),
	 capacity_( c.capacity_ ),
	 data_( c.data_ )
	{
		c.size_ = 0u;
		c.capacity_ = 0u;
		c.data_ = nullptr;
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Chunk( Chunk< U > const & c ) :
	 size_( c.size_ ),
	 capacity_( size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( c.data_[ i ] );
		}
	}

	// Size Constructor: Built-In Types are Not Initialized!
	explicit
	Chunk( size_type const size ) :
	 size_( size ),
	 capacity_( size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{}

	// Size + Uniform Value Constructor
	Chunk(
	 size_type const size,
	 Tc value
	) :
	 size_( size ),
	 capacity_( size_ ),
	 data_( size_ > 0u ? new T[ size_ ] : nullptr )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = value;
		}
	}

	// Destructor
	~Chunk()
	{
		delete[] data_;
	}

public: // Assignment

	// Copy Assignment
	Chunk &
	operator =( Chunk const & c )
	{
		if ( this != &c ) {
			if ( size_ != c.size_ ) {
				size_ = c.size_;
				capacity_ = size_;
				delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = c.data_[ i ];
			}
		}
		return *this;
	}

	// Move Assignment
	Chunk &
	operator =( Chunk && c ) NOEXCEPT
	{
		assert( this != &c );
		size_ = c.size_;
		capacity_ = c.capacity_;
		delete[] data_; data_ = c.data_;
		c.size_ = 0u;
		c.capacity_ = 0u;
		c.data_ = nullptr;
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Chunk &
	operator =( Chunk< U > const & c )
	{
		if ( size_ != c.size_ ) {
			size_ = c.size_;
			capacity_ = size_;
			delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
		}
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = T( c.data_[ i ] );
		}
		return *this;
	}

	// Size + Value Assignment
	Chunk &
	assign(
	 size_type const size,
	 Tc value
	)
	{
		if ( size_ != size ) {
			size_ = size;
			capacity_ = size_;
			delete[] data_; data_ = ( size_ > 0u ? new T[ size_ ] : nullptr );
		}
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = value;
		}
		return *this;
	}

	// += Chunk
	Chunk &
	operator +=( Chunk const & c )
	{
		assert( size_ == c.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += c.data_[ i ];
		}
		return *this;
	}

	// -= Chunk
	Chunk &
	operator -=( Chunk const & c )
	{
		assert( size_ == c.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= c.data_[ i ];
		}
		return *this;
	}

	// += Chunk Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Chunk &
	operator +=( Chunk< U > const & c )
	{
		assert( size_ == c.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += T( c.data_[ i ] );
		}
		return *this;
	}

	// -= Chunk Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Chunk &
	operator -=( Chunk< U > const & c )
	{
		assert( size_ == c.size_ );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= T( c.data_[ i ] );
		}
		return *this;
	}

	// = Value
	Chunk &
	operator =( Tc value )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = value;
		}
		return *this;
	}

	// += Value
	Chunk &
	operator +=( Tc value )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += value;
		}
		return *this;
	}

	// -= Value
	Chunk &
	operator -=( Tc value )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= value;
		}
		return *this;
	}

	// *= Value
	Chunk &
	operator *=( Tc value )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= value;
		}
		return *this;
	}

	// /= Value
	Chunk &
	operator /=( Tc value )
	{
		assert( value != T( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] /= value;
		}
		return *this;
	}

public: // Subscript

	// Chunk[ i ] const: 0-Based Indexing
	Tr
	operator []( size_type const i ) const
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// Chunk[ i ]: 0-Based Indexing
	T &
	operator []( size_type const i )
	{
		assert( i < size_ );
		return data_[ i ];
	}

public: // Inspector

	// Size
	size_type
	size() const
	{
		return size_;
	}

	// Capacity
	size_type
	capacity() const
	{
		return capacity_;
	}

	// Empty?
	bool
	empty() const
	{
		return ( size_ == 0u );
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

	// Append an Element
	Chunk &
	push_back( Tc value )
	{
		assert( size_ < max_size );
		if ( size_ == capacity_ ) reserve( 2 * capacity_ );
		data_[ size_ ] = value;
		++size_;
		return *this;
	}

	// Remove the Last Element
	Chunk &
	pop_back()
	{
		assert( size_ > 0u );
		--size_;
		return *this;
	}

	// Resize: Values Preserved: Added Built-In Values are Not Initialized!
	Chunk &
	resize( size_type const size )
	{
		if ( size_ != size ) {
			if ( size > capacity_ ) {
				size_type const size_c( std::min( size_, size ) );
				T * const new_array( new T[ size ] );
				for ( size_type i = 0; i < size_c; ++i ) {
					new_array[ i ] = data_[ i ];
				}
				delete[] data_; data_ = new_array;
				capacity_ = size;
			}
			size_ = size;
		}
		return *this;
	}

	// Resize + Fill Value: Values Preserved
	Chunk &
	resize(
	 size_type const size,
	 Tc value
	)
	{
		if ( size_ != size ) {
			size_type const size_c( std::min( size_, size ) );
			if ( size > capacity_ ) {
				T * const new_array( new T[ size ] );
				for ( size_type i = 0; i < size_c; ++i ) {
					new_array[ i ] = data_[ i ];
				}
				delete[] data_; data_ = new_array;
				capacity_ = size;
			}
			for ( size_type i = size_c; i < size; ++i ) {
				data_[ i ] = value;
			}
			size_ = size;
		}
		return *this;
	}

	// Resize: Values Not Preserved: Built-In Values are Not Initialized!
	Chunk &
	non_preserving_resize( size_type const size )
	{
		if ( size_ != size ) {
			if ( size > capacity_ ) {
				delete[] data_; data_ = new T[ size ];
				capacity_ = size;
			}
			size_ = size;
		}
		return *this;
	}

	// Resize + Fill Value: Values Not Preserved
	Chunk &
	non_preserving_resize(
	 size_type const size,
	 Tc value
	)
	{
		if ( size_ != size ) {
			if ( size > capacity_ ) {
				delete[] data_; data_ = new T[ size ];
				capacity_ = size;
			}
			size_ = size;
		}
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = value;
		}
		return *this;
	}

	// Reserve: Values Preserved: Added Built-In Values are Not Initialized!
	Chunk &
	reserve( size_type const capacity )
	{
		if ( capacity_ < capacity ) {
			T * const new_array( new T[ capacity ] );
			for ( size_type i = 0; i < size_; ++i ) {
				new_array[ i ] = data_[ i ];
			}
			delete[] data_; data_ = new_array;
			capacity_ = capacity;
		}
		return *this;
	}

	// Shrink Capacity to Size
	Chunk &
	shrink()
	{
		if ( size_ < capacity_ ) {
			T * const new_array( size_ > 0u ? new T[ size_ ] : nullptr );
			for ( size_type i = 0; i < size_; ++i ) {
				new_array[ i ] = data_[ i ];
			}
			delete[] data_; data_ = new_array;
			capacity_ = size_;
		}
		return *this;
	}

	// Shrink Capacity to Size
	Chunk &
	shrink_to_fit()
	{
		if ( size_ < capacity_ ) {
			T * const new_array( size_ > 0u ? new T[ size_ ] : nullptr );
			for ( size_type i = 0; i < size_; ++i ) {
				new_array[ i ] = data_[ i ];
			}
			delete[] data_; data_ = new_array;
			capacity_ = size_;
		}
		return *this;
	}

	// Swap
	void
	swap( Chunk & c )
	{
		std::swap( size_, c.size_ );
		std::swap( capacity_, c.capacity_ );
		std::swap( data_, c.data_ );
	}

	// Clear
	Chunk &
	clear()
	{
		size_ = 0u;
		capacity_ = 0u;
		delete[] data_; data_ = nullptr;
		return *this;
	}

public: // Static Data

	static size_type const max_size; // Max size

private: // Data

	size_type size_; // Number of elements in use
	size_type capacity_; // Number of elements Chunk can hold without resizing
	T * data_; // Data array

}; // Chunk

	// Static Data Member Template Definitions
	template< typename T > typename Chunk< T >::size_type const Chunk< T >::max_size = static_cast< size_type >( -1 );

// Swap
template< typename T >
inline
void
swap( Chunk< T > & a, Chunk< T > & b )
{
	a.swap( b );
}

// Chunk == Chunk
template< typename T >
inline
bool
operator ==( Chunk< T > const & a, Chunk< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename Chunk< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( a[ i ] != b[ i ] ) return false; // Elements differ
		}
		return true; // No elements differ
	}
}

// Chunk != Chunk
template< typename T >
inline
bool
operator !=( Chunk< T > const & a, Chunk< T > const & b )
{
	return !( a == b );
}

} // ObjexxFCL

#endif // ObjexxFCL_Chunk_hh_INCLUDED
