#ifndef ObjexxFCL_ChunkVector_hh_INCLUDED
#define ObjexxFCL_ChunkVector_hh_INCLUDED

// ChunkVector: Chunk-Contiguous Vector for Fast Very Large Vectors
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
#include <ObjexxFCL/ChunkVector.fwd.hh>
#include <ObjexxFCL/Chunk.hh>
#include <ObjexxFCL/ChunkExponent.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <iomanip>
#include <istream>
#include <ostream>
#include <limits>
#include <type_traits>
#include <utility>
#include <vector>

namespace ObjexxFCL {

// ChunkVector: Chunk-Contiguous Vector for Fast Very Large Vectors
//
// Note:
//  Chunk allocation avoids large contiguous allocation failures with fragmented memory
//  Similar to std::deque but faster accessor performance
//  Construction and assignment give right-sized ChunkVector with no reserve capacity
//  Exponent is limited to one less than the number of bits in the Chunk size_type
//  Resize and push_back can add reserve capacity
//  Generators such as ChunkVector + ChunkVector are not provided: Can't specify the chunk exponent
//  Double loop operations are used instead of linear indexing for slight efficiency benefit
//
// Invariants:
//  chunk_size_ > 0
//  Chunks have size == chunk_size_ except last Chunk has size in [1,chunk_size_]
//  Chunks have capacity == chunk_size_ except last Chunk has capacity in [chunk.size(),chunk_size_]
template< typename T >
class ChunkVector
{

private: // Friend

	template< typename > friend class ChunkVector;

public: // Types

	typedef  std::vector< Chunk< T > >  Chunks;
	typedef  TypeTraits< T >  Traits;
	typedef  typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type  Tc;
	typedef  typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type  Tr;

	// STL style
	typedef  Chunk< T >  Chunk_type;
	typedef  T  value_type;
	typedef  T &  reference;
	typedef  T const &  const_reference;
	typedef  T *  pointer;
	typedef  T const *  const_pointer;
	typedef  std::size_t  size_type;
	typedef  std::ptrdiff_t  difference_type;
	typedef  typename Chunks::size_type  Chunks_size_type;

	// C++ style
	typedef  Chunk< T >  ChunkType;
	typedef  T  Value;
	typedef  T &  Reference;
	typedef  T const &  ConstReference;
	typedef  T *  Pointer;
	typedef  T const *  ConstPointer;
	typedef  T *  Iterator;
	typedef  T const *  ConstIterator;
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;
	typedef  typename Chunks::size_type  ChunksSize;

public: // Creation

	// Default Constructor
	ChunkVector() :
	 size_( 0u ),
	 chunk_exponent_( 0u ),
	 chunk_size_( 1u ),
	 chunk_mask_( 0u )
	{}

	// Copy Constructor
	ChunkVector( ChunkVector const & v ) :
	 size_( v.size_ ),
	 chunk_exponent_( v.chunk_exponent_ ),
	 chunk_size_( v.chunk_size_ ),
	 chunk_mask_( v.chunk_mask_ ),
	 chunks_( v.chunks_ )
	{
		assert( v.n_chunk() == computed_n_chunk() );
	}

	// Move Constructor
	ChunkVector( ChunkVector && v ) NOEXCEPT :
	 size_( v.size_ ),
	 chunk_exponent_( v.chunk_exponent_ ),
	 chunk_size_( v.chunk_size_ ),
	 chunk_mask_( v.chunk_mask_ ),
	 chunks_( std::move( v.chunks_ ) )
	{
		assert( n_chunk() == computed_n_chunk() );
		v.size_ = 0u;
		v.chunk_exponent_ = 0u;
		v.chunk_size_ = 1u;
		v.chunk_mask_ = 0u;
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	ChunkVector( ChunkVector< U > const & v ) :
	 size_( v.size_ ),
	 chunk_exponent_( v.chunk_exponent_ ),
	 chunk_size_( v.chunk_size_ ),
	 chunk_mask_( v.chunk_mask_ ),
	 chunks_( v.n_chunk() ) // std::vector doesn't have a copy constructor template
	{
		// Size and assign Chunks
		assert( v.n_chunk() == computed_n_chunk() );
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
				Chunk_type & chunk( chunks_[ i ] );
				typename ChunkVector< U >::Chunk_type const & v_chunk( v.chunks_[ i ] );
				assert( v_chunk.size() == ( i < i_last ? chunk_size_ : computed_last_chunk_size() ) );
				chunk = v_chunk;
			}
		}
	}

	// std::vector + Exponent Constructor Template
	template< typename U, typename L >
	ChunkVector(
	 std::vector< U, L > const & v,
	 ChunkExponent const & exponent
	) :
	 size_( v.size() ),
	 chunk_exponent_( exponent ),
	 chunk_size_( size_type( 1u ) << chunk_exponent_ ),
	 chunk_mask_( chunk_size_ - size_type( 1u ) ),
	 chunks_( computed_n_chunk() )
	{
		// Size and assign Chunks
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			typename std::vector< U, L >::const_iterator k( v.begin() );
			for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
				Chunk_type & chunk( chunks_[ i ] );
				chunk.non_preserving_resize( i < i_last ? chunk_size_ : computed_last_chunk_size() );
				for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
					chunk[ j ] = T( *k );
				}
			}
		}
	}

	// Iterator Range + Exponent Constructor Template
	template< typename InputIterator >
	ChunkVector(
	 InputIterator const beg,
	 InputIterator const end,
	 ChunkExponent const & exponent
	) :
	 size_( end - beg ),
	 chunk_exponent_( exponent ),
	 chunk_size_( size_type( 1u ) << chunk_exponent_ ),
	 chunk_mask_( chunk_size_ - size_type( 1u ) ),
	 chunks_( computed_n_chunk() )
	{
		// Size and assign Chunks
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			InputIterator k( beg );
			for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
				Chunk_type & chunk( chunks_[ i ] );
				chunk.non_preserving_resize( i < i_last ? chunk_size_ : computed_last_chunk_size() );
				for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
					chunk[ j ] = T( *k );
				}
			}
		}
	}

	// Size + Exponent Constructor: Built-In Values are Not Initialized!
	ChunkVector(
	 size_type const size,
	 ChunkExponent const & exponent
	) :
	 size_( size ),
	 chunk_exponent_( exponent ),
	 chunk_size_( size_type( 1u ) << chunk_exponent_ ),
	 chunk_mask_( chunk_size_ - size_type( 1u ) ),
	 chunks_( computed_n_chunk() )
	{
		// Size and assign Chunks
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			for ( Chunks_size_type i = 0; i < i_last; ++i ) {
				chunks_[ i ].non_preserving_resize( chunk_size_ );
			}
			chunks_[ i_last ].non_preserving_resize( computed_last_chunk_size() );
		}
	}

	// Size + Exponent + Uniform Value Constructor
	ChunkVector(
	 size_type const size,
	 ChunkExponent const & exponent,
	 Tc value
	) :
	 size_( size ),
	 chunk_exponent_( exponent ),
	 chunk_size_( size_type( 1u ) << chunk_exponent_ ),
	 chunk_mask_( chunk_size_ - size_type( 1u ) ),
	 chunks_( computed_n_chunk() )
	{
		// Size and assign Chunks
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			for ( Chunks_size_type i = 0; i < i_last; ++i ) {
				chunks_[ i ].non_preserving_resize( chunk_size_, value );
			}
			chunks_[ i_last ].non_preserving_resize( computed_last_chunk_size(), value );
		}
	}

	// Destructor
	~ChunkVector()
	{}

public: // Assignment

	// Copy Assignment
	ChunkVector &
	operator =( ChunkVector const & v )
	{
		if ( this != &v ) {
			if ( chunk_exponent_ == v.chunk_exponent_ ) { // Resize for efficiency
				non_preserving_resize( v.size_ );
				if ( size_ > 0u ) {
					Chunks_size_type const i_last( i_last_chunk() );
					for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
						Chunk_type & chunk( chunks_[ i ] );
						Chunk_type const & v_chunk( v.chunks_[ i ] );
						assert( ( v_chunk.size() == chunk_size_ ) || ( i == i_last ) );
						assert( v_chunk.size() == chunk.size() );
						chunk = v_chunk;
					}
				}
			} else { // Must reallocate so use member assignment
				size_ = v.size_;
				chunk_exponent( v.chunk_exponent_ );
				chunks_ = v.chunks_;
			}
			assert( n_chunk() == computed_n_chunk() );
		}
		return *this;
	}

	// Move Assignment
	ChunkVector &
	operator =( ChunkVector && v ) NOEXCEPT
	{
		assert( this != &v );
		size_ = v.size_;
		chunk_exponent_ = v.chunk_exponent_;
		chunk_size_ = v.chunk_size_;
		chunk_mask_ = v.chunk_mask_;
		chunks_ = std::move( v.chunks_ );
		assert( n_chunk() == computed_n_chunk() );
		v.chunks_.clear();
		v.size_ = 0u;
		v.chunk_exponent_ = 0u;
		v.chunk_size_ = 1u;
		v.chunk_mask_ = 0u;
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	ChunkVector &
	operator =( ChunkVector< U > const & v )
	{
		if ( chunk_exponent_ == v.chunk_exponent_ ) { // Resize for efficiency
			non_preserving_resize( v.size_ );
			if ( size_ > 0u ) {
				Chunks_size_type const i_last( i_last_chunk() );
				for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
					Chunk_type & chunk( chunks_[ i ] );
					typename ChunkVector< U >::Chunk_type const & v_chunk( v.chunks_[ i ] );
					assert( v_chunk.size() == ( i < i_last ? chunk_size_ : computed_last_chunk_size() ) );
					assert( v_chunk.size() == chunk.size() );
					chunk = v_chunk;
				}
			}
		} else { // Must reallocate so use member assignment
			size_ = v.size_;
			chunk_exponent( v.chunk_exponent_ );
			chunks_.clear();
			chunks_.resize( computed_n_chunk() );
			if ( size_ > 0u ) {
				Chunks_size_type const i_last( i_last_chunk() );
				for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
					Chunk_type & chunk( chunks_[ i ] );
					typename ChunkVector< U >::Chunk_type const & v_chunk( v.chunks_[ i ] );
					assert( v_chunk.size() == ( i < i_last ? chunk_size_ : computed_last_chunk_size() ) );
					chunk = v_chunk;
				}
			}
		}
		assert( n_chunk() == computed_n_chunk() );

		return *this;
	}

	// std::vector Assignment Template
	template< typename U, typename L >
	ChunkVector &
	operator =( std::vector< U, L > const & v )
	{
		non_preserving_resize( v.size() );
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			typename std::vector< U, L >::const_iterator k( v.begin() );
			for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
				Chunk_type & chunk( chunks_[ i ] );
				assert( ( chunk.size() == chunk_size_ ) || ( i == i_last ) );
				for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
					chunk[ j ] = T( *k );
				}
			}
		}
		return *this;
	}

	// std::vector + Exponent Assignment Template
	template< typename U, typename L >
	ChunkVector &
	assign(
	 std::vector< U, L > const & v,
	 ChunkExponent const & exponent
	)
	{
		if ( chunk_exponent_ == exponent ) { // Call other assign function for efficiency
			return operator =( v );
		} else { // Must reallocate so use member assignment
			size_ = v.size();
			chunk_exponent( exponent );
			chunks_.clear();
			chunks_.resize( computed_n_chunk() );
			if ( size_ > 0u ) {
				Chunks_size_type const i_last( i_last_chunk() );
				typename std::vector< U, L >::const_iterator k( v.begin() );
				for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
					Chunk_type & chunk( chunks_[ i ] );
					chunk.resize( i < i_last ? chunk_size_ : computed_last_chunk_size() );
					for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
						chunk[ j ] = T( *k );
					}
				}
			}
		}
		return *this;
	}

	// Iterator Range Assignment Template
	template< typename InputIterator >
	ChunkVector &
	assign(
	 InputIterator const beg,
	 InputIterator const end
	)
	{
		non_preserving_resize( end - beg );
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			InputIterator k( beg );
			for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
				Chunk_type & chunk( chunks_[ i ] );
				for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
					chunk[ j ] = T( *k );
				}
			}
		}
		return *this;
	}

	// Iterator Range + Exponent Assignment Template
	template< typename InputIterator >
	ChunkVector &
	assign(
	 InputIterator const beg,
	 InputIterator const end,
	 ChunkExponent const & exponent
	)
	{
		if ( chunk_exponent_ == exponent ) { // Call other assign function for efficiency
			return assign( beg, end );
		} else { // Must reallocate so use member assignment
			size_ = end - beg;
			chunk_exponent( exponent );
			chunks_.clear();
			chunks_.resize( computed_n_chunk() );
			if ( size_ > 0u ) {
				Chunks_size_type const i_last( i_last_chunk() );
				InputIterator k( beg );
				for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
					Chunk_type & chunk( chunks_[ i ] );
					chunk.resize( i < i_last ? chunk_size_ : computed_last_chunk_size() );
					for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
						chunk[ j ] = T( *k );
					}
				}
			}
		}
		return *this;
	}

	// Size + Value Assignment
	ChunkVector &
	assign(
	 size_type const size,
	 Tc value
	)
	{
		non_preserving_resize( size );
		if ( size_ > 0u ) {
			Chunks_size_type const i_last( i_last_chunk() );
			for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
				chunks_[ i ] = value;
			}
		}
		return *this;
	}

	// Size + Exponent + Value Assignment
	ChunkVector &
	assign(
	 size_type const size,
	 ChunkExponent const & exponent,
	 Tc value
	)
	{
		if ( chunk_exponent_ == exponent ) { // Call other assign function for efficiency
			return assign( size, value );
		} else { // Must reallocate so use member assignment
			size_ = size;
			chunk_exponent( exponent );
			chunks_.clear();
			chunks_.resize( computed_n_chunk() );
			if ( size_ > 0u ) {
				Chunks_size_type const i_last( i_last_chunk() );
				for ( Chunks_size_type i = 0; i <= i_last; ++i ) {
					chunks_[ i ].assign( ( i < i_last ? chunk_size_ : computed_last_chunk_size() ), value );
				}
			}
		}
		return *this;
	}

	// += ChunkVector
	ChunkVector &
	operator +=( ChunkVector const & v )
	{
		assert( size_ == v.size_ );
		size_type k( 0u );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
				chunk[ j ] += v[ k ];
			}
		}
		return *this;
	}

	// -= ChunkVector
	ChunkVector &
	operator -=( ChunkVector const & v )
	{
		assert( size_ == v.size_ );
		size_type k( 0u );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
				chunk[ j ] -= v[ k ];
			}
		}
		return *this;
	}

	// += ChunkVector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	ChunkVector &
	operator +=( ChunkVector< U > const & v )
	{
		assert( size_ == v.size_ );
		typename ChunkVector< U >::size_type k( 0u );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
				chunk[ j ] += T( v[ k ] );
			}
		}
		return *this;
	}

	// -= ChunkVector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	ChunkVector &
	operator -=( ChunkVector< U > const & v )
	{
		assert( size_ == v.size_ );
		typename ChunkVector< U >::size_type k( 0u );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
				chunk[ j ] -= T( v[ k ] );
			}
		}
		return *this;
	}

	// += std::vector Template
	template< typename U, typename L >
	ChunkVector &
	operator +=( std::vector< U, L > const & v )
	{
		assert( size_ == v.size() );
		typename std::vector< U, L >::size_type k( 0u );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
				chunk[ j ] += T( v[ k ] );
			}
		}
		return *this;
	}

	// -= std::vector Template
	template< typename U, typename L >
	ChunkVector &
	operator -=( std::vector< U, L > const & v )
	{
		assert( size_ == v.size() );
		typename std::vector< U, L >::size_type k( 0u );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j, ++k ) {
				chunk[ j ] -= T( v[ k ] );
			}
		}
		return *this;
	}

	// = Value
	ChunkVector &
	operator =( Tc value )
	{
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			chunks_[ i ] = value;
		}
		return *this;
	}

	// += Value
	ChunkVector &
	operator +=( Tc value )
	{
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			chunks_[ i ] += value;
		}
		return *this;
	}

	// -= Value
	ChunkVector &
	operator -=( Tc value )
	{
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			chunks_[ i ] -= value;
		}
		return *this;
	}

	// *= Value
	ChunkVector &
	operator *=( Tc value )
	{
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			chunks_[ i ] *= value;
		}
		return *this;
	}

	// /= Value
	ChunkVector &
	operator /=( Tc value )
	{
		assert( value != T( 0 ) );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			chunks_[ i ] /= value;
		}
		return *this;
	}

public: // Subscript

	// ChunkVector[ i ] const: 0-Based Indexing
	Tr
	operator []( size_type const i ) const
	{
		assert( i < size_ );
		return chunks_[ i >> chunk_exponent_ ][ i & chunk_mask_ ];
	}

	// ChunkVector[ i ]: 0-Based Indexing
	T &
	operator []( size_type const i )
	{
		assert( i < size_ );
		return chunks_[ i >> chunk_exponent_ ][ i & chunk_mask_ ];
	}

	// ChunkVector( i ) const: 1-Based Indexing
	Tr
	operator ()( size_type const i ) const
	{
		assert( ( i > 0u ) && ( i <= size_ ) );
		return chunks_[ ( i - 1 ) >> chunk_exponent_ ][ ( i - 1 ) & chunk_mask_ ];
	}

	// ChunkVector( i ): 1-Based Indexing
	T &
	operator ()( size_type const i )
	{
		assert( ( i > 0u ) && ( i <= size_ ) );
		return chunks_[ ( i - 1 ) >> chunk_exponent_ ][ ( i - 1 ) & chunk_mask_ ];
	}

public: // Inspector

	// Size
	size_type
	size() const
	{
		return size_;
	}

	// Empty?
	bool
	empty() const
	{
		return ( size_ == 0 );
	}

	// Chunk Exponent
	size_type
	chunk_exponent() const
	{
		return chunk_exponent_;
	}

	// Chunk Size
	size_type
	chunk_size() const
	{
		return chunk_size_;
	}

	// Number of Chunks
	Chunks_size_type
	n_chunk() const
	{
		return chunks_.size();
	}

	// First Element
	Tr
	front() const
	{
		assert( size_ > 0u );
		return chunks_[ 0 ][ 0 ];
	}

	// Last Element
	Tr
	back() const
	{
		assert( size_ > 0u );
		return operator ()( size_ );
	}

	// Length
	T
	length() const
	{
		T length_sq( T( 0 ) );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j ) {
				T const chunk_j( chunk[ j ] );
				length_sq += chunk_j * chunk_j;
			}
		}
		return std::sqrt( length_sq );
	}

	// Length Squared
	T
	length_squared() const
	{
		T length_sq( T( 0 ) );
		for ( Chunks_size_type i = 0, ie = chunks_.size(); i < ie; ++i ) {
			Chunk_type & chunk( chunks_[ i ] );
			for ( size_type j = 0, je = chunk.size(); j < je; ++j ) {
				T const chunk_j( chunk[ j ] );
				length_sq += chunk_j * chunk_j;
			}
		}
		return length_sq;
	}

public: // Modifier

	// First Element
	T &
	front()
	{
		assert( size_ > 0u );
		return chunks_[ 0 ][ 0 ];
	}

	// Last Element
	T &
	back()
	{
		assert( size_ > 0u );
		return operator ()( size_ );
	}

	// Append an Element
	ChunkVector &
	push_back( Tc value )
	{
		assert( size_ < max_size );
		if ( ( size_ == 0 ) || ( last_chunk().size() == chunk_size_ ) ) { // No Chunks or last Chunk is full
			chunks_.push_back( Chunk_type() ); // Add a new Chunk
		}
		Chunk_type & chunk( last_chunk() );
		chunk.reserve( chunk_size_ ); // Reserve full size for efficient appends
		chunk.push_back( value ); // Append the new element
		++size_;
		return *this;
	}

	// Remove the Last Element
	ChunkVector &
	pop_back()
	{
		assert( size_ > 0u );
		Chunk_type & chunk( last_chunk() );
		chunk.pop_back();
		if ( chunk.empty() ) chunks_.pop_back();
		--size_;
		return *this;
	}

	// Append ChunkVector
	ChunkVector &
	append( ChunkVector const & v )
	{
		if ( v.size_ > 0u ) {
			assert( size_ <= max_size - v.size_ );
			size_type const size_o( size_ );
			resize( size_ + v.size_ );
			size_type k( 0u );
			for ( size_type i = size_o; i < size_; ++i, ++k ) {
				(*this)[ i ] = v[ k ];
			}
		}
		return *this;
	}

	// Append ChunkVector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	ChunkVector &
	append( ChunkVector< U > const & v )
	{
		if ( v.size_ > 0u ) {
			assert( size_ <= max_size - v.size_ );
			size_type const size_o( size_ );
			resize( size_ + v.size_ );
			typename ChunkVector< U >::size_type k( 0u );
			for ( size_type i = size_o; i < size_; ++i, ++k ) {
				(*this)[ i ] = T( v[ k ] );
			}
		}
		return *this;
	}

	// Append std::vector Template
	template< typename U, typename L >
	ChunkVector &
	append( std::vector< U, L > const & v )
	{
		if ( v.size() > 0u ) {
			assert( size_ <= max_size - v.size() );
			size_type const size_o( size_ );
			resize( size_ + v.size() );
			typename std::vector< U, L >::size_type k( 0u );
			for ( size_type i = size_o; i < size_; ++i, ++k ) {
				(*this)[ i ] = T( v[ k ] );
			}
		}
		return *this;
	}

	// Resize with Same Chunk Size + Fill Value: Values Preserved
	ChunkVector &
	resize(
	 size_type const size,
	 Tc value = T()
	)
	{
		Chunks_size_type const n_chunk_o( n_chunk() );
		Chunks_size_type const n_chunk( ( size + chunk_size_ - 1 ) / chunk_size_ );
		Chunks_size_type const i_last_chunk( n_chunk - 1 );
		if ( size > size_ ) { // Add more values and maybe Chunks
			if ( n_chunk > n_chunk_o ) { // Add more Chunks: Use outer copy + inner swap for speed
				Chunks chunks( n_chunk ); // Create temporary outer vector with empty Chunks
				for ( Chunks_size_type i = 0; i < n_chunk_o; ++i ) { // Swap the old Chunks to get values
					chunks_[ i ].swap( chunks[ i ] );
				}
				chunks_.swap( chunks ); // Swap the outer vector
				for ( Chunks_size_type i = n_chunk_o - 1; i < i_last_chunk; ++i ) { // Fill out the Chunks
					chunks_[ i ].resize( chunk_size_, value );
				}
			}
		} else if ( size < size_ ) { // Remove values and maybe Chunks
			if ( n_chunk < n_chunk_o ) { // Remove tail Chunks
				chunks_.resize( n_chunk );
			}
		}
		if ( size > 0u ) { // Size the last Chunk
			chunks_[ i_last_chunk ].resize( size - ( i_last_chunk * chunk_size_ ), value );
		}
		size_ = size;
		return *this;
	}

	// Resize with Same Chunk Size: Values Not Preserved
	ChunkVector &
	non_preserving_resize( size_type const size )
	{
		Chunks_size_type const n_chunk_o( n_chunk() );
		Chunks_size_type const n_chunk( ( size + chunk_size_ - 1 ) / chunk_size_ );
		Chunks_size_type const i_last_chunk( n_chunk - 1 );
		if ( size > size_ ) { // Add more values and maybe Chunks
			if ( n_chunk > n_chunk_o ) { // Add more Chunks: Use outer copy + inner swap for speed
				Chunks chunks( n_chunk ); // Create temporary outer vector with empty Chunks
				for ( Chunks_size_type i = 0; i < n_chunk_o; ++i ) { // Swap the old Chunks to save allocation cost
					chunks_[ i ].swap( chunks[ i ] );
				}
				chunks_.swap( chunks ); // Swap the outer vector
				for ( Chunks_size_type i = n_chunk_o - 1; i < i_last_chunk; ++i ) { // Fill out the Chunks
					chunks_[ i ].non_preserving_resize( chunk_size_ );
				}
			}
		} else if ( size < size_ ) { // Remove values and maybe Chunks
			if ( n_chunk < n_chunk_o ) { // Remove tail Chunks
				chunks_.resize( n_chunk );
			}
		}
		if ( size > 0u ) { // Size the last Chunk
			chunks_[ i_last_chunk ].non_preserving_resize( size - ( i_last_chunk * chunk_size_ ) );
		}
		size_ = size;
		return *this;
	}

	// Reshape + Fill Value: Values Preserved
	ChunkVector &
	reshape(
	 size_type const size,
	 ChunkExponent const & exponent,
	 Tc value = T()
	)
	{
		ChunkVector v( size, exponent, value ); // Temporary with desired shape
		for ( size_type k = 0, ke = std::min( size_, size ); k < ke; ++k ) { // Copy values
			v[ k ] = (*this)[ k ];
		}
		swap( v ); // Swap with temporary
		return *this;
	}

	// Reshape: Values Not Preserved
	ChunkVector &
	non_preserving_reshape(
	 size_type const size,
	 ChunkExponent const & exponent
	)
	{
		ChunkVector( size, exponent ).swap( *this ); // Set to new array
		return *this;
	}

	// Shrink to Right-Sized
	ChunkVector &
	shrink()
	{
		if ( size_ > 0u ) {
			Chunk_type & chunk( last_chunk() ); // Only last Chunk can have excess capacity
			if ( chunk.size() < chunk.capacity() ) { // Last Chunk has excess capacity
				chunk.shrink(); // Shrink last chunk
			}
		}
		return *this;
	}

	// Shrink to Right-Sized
	ChunkVector &
	shrink_to_fit()
	{
		if ( size_ > 0u ) {
			Chunk_type & chunk( last_chunk() ); // Only last Chunk can have excess capacity
			if ( chunk.size() < chunk.capacity() ) { // Last Chunk has excess capacity
				chunk.shrink_to_fit(); // Shrink last chunk
			}
		}
		return *this;
	}

	// Swap
	void
	swap( ChunkVector & v )
	{
		std::swap( size_, v.size_ );
		std::swap( chunk_exponent_, v.chunk_exponent_ );
		std::swap( chunk_size_, v.chunk_size_ );
		std::swap( chunk_mask_, v.chunk_mask_ );
		chunks_.swap( v.chunks_ );
	}

	// Clear
	ChunkVector &
	clear()
	{
		size_ = 0u;
		chunk_exponent_ = 0u;
		chunk_size_ = 1u;
		chunk_mask_ = 0u;
		chunks_.clear();
		return *this;
	}

	// Normalize to Unit Length
	ChunkVector &
	normalize()
	{
		T const length_( length() );
		assert( length_ > T( 0 ) );
		operator /=( length_ );
		return *this;
	}

private: // Functions

	// Index of Last Chunk
	Chunks_size_type
	i_last_chunk() const
	{
		assert( ! chunks_.empty() );
		return chunks_.size() - 1;
	}

	// Last Chunk
	Chunk_type const &
	last_chunk() const
	{
		assert( ! chunks_.empty() );
		return chunks_[ chunks_.size() - 1 ];
	}

	// Last Chunk
	Chunk_type &
	last_chunk()
	{
		assert( ! chunks_.empty() );
		return chunks_[ chunks_.size() - 1 ];
	}

	// Computed Number of Chunks
	Chunks_size_type
	computed_n_chunk() const
	{
		return ( size_ + chunk_size_ - 1 ) / chunk_size_;
	}

	// Computed Last Chunk Size
	size_type
	computed_last_chunk_size() const
	{
		assert( size_ > 0u );
		return size_ - ( ( ( size_ - 1 ) / chunk_size_ ) * chunk_size_ );
	}

	// Exponent
	ChunkVector &
	chunk_exponent( ChunkExponent const & exponent )
	{
		chunk_exponent_ = exponent;
		chunk_size_ = size_type( 1u ) << chunk_exponent_;
		chunk_mask_ = chunk_size_ - size_type( 1u );
		return *this;
	}

public: // Static Data

	static size_type const max_size; // Max size

private: // Data

	size_type size_; // Number of elements
	size_type chunk_exponent_; // Chunk size exponent (< number of bits in size_type)
	size_type chunk_size_; // Chunk size (a power of 2) (last Chunk can be smaller)
	size_type chunk_mask_; // Chunk index identification mask
	Chunks chunks_; // Vector of Chunks

}; // ChunkVector

	// Static Data Member Template Definitions
	template< typename T > typename ChunkVector< T >::size_type const ChunkVector< T >::max_size = static_cast< size_type >( -1 );

// Functions

// Magnitude
template< typename T >
inline
T
magnitude( ChunkVector< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const a_i( a[ i ] );
		mag_sq += a_i * a_i;
	}
	return std::sqrt( mag_sq );
}

// Magnitude Squared
template< typename T >
inline
T
magnitude_squared( ChunkVector< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const a_i( a[ i ] );
		mag_sq += a_i * a_i;
	}
	return mag_sq;
}

// Distance
template< typename T >
inline
T
distance( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const distance_i( a[ i ] - b[ i ] );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance Squared
template< typename T >
inline
T
distance_squared( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		T const distance_i( a[ i ] - b[ i ] );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Dot Product
template< typename T >
inline
T
dot( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	assert( a.size() == b.size() );
	T sum( T( 0 ) );
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		sum += a[ i ] * b[ i ];
	}
	return sum;
}

// Dot Product
template< typename T >
inline
T
dot_product( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	assert( a.size() == b.size() );
	T sum( T( 0 ) );
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		sum += a[ i ] * b[ i ];
	}
	return sum;
}

// Swap
template< typename T >
inline
void
swap( ChunkVector< T > & a, ChunkVector< T > & b )
{
	a.swap( b );
}

// Comparison: ChunkVectors

// Are two ChunkVectors comparable?
template< typename T >
inline
bool
comparable( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	return ( a.size() == b.size() );
}

// ChunkVector == ChunkVector
template< typename T >
inline
bool
operator ==( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values: Chunk size ignored
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] == b[ i ] ) ) return false;
		}
		return true; // No elements differ
	}
}

// ChunkVector != ChunkVector
template< typename T >
inline
bool
operator !=( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	return !( a == b );
}

// ChunkVector < ChunkVector
template< typename T >
inline
bool
operator <( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return false;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values: Chunk size ignored
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] < b[ i ] ) ) return false;
		}
		return true;
	}
}

// ChunkVector <= ChunkVector
template< typename T >
inline
bool
operator <=( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values: Chunk size ignored
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] <= b[ i ] ) ) return false;
		}
		return true;
	}
}

// ChunkVector >= ChunkVector
template< typename T >
inline
bool
operator >=( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return true;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values: Chunk size ignored
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] >= b[ i ] ) ) return false;
		}
		return true;
	}
}

// ChunkVector > ChunkVector
template< typename T >
inline
bool
operator >( ChunkVector< T > const & a, ChunkVector< T > const & b )
{
	if ( &a == &b ) { // Same objects
		return false;
	} else if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values: Chunk size ignored
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] > b[ i ] ) ) return false;
		}
		return true;
	}
}

// Comparison: std::vector

// Is a ChunkVector comparable to a std::vector?
template< typename T, typename L >
inline
bool
comparable( ChunkVector< T > const & a, std::vector< T, L > const & b )
{
	return ( a.size() == b.size() );
}

// ChunkVector == std::vector Template
template< typename T, typename L >
inline
bool
operator ==( ChunkVector< T > const & a, std::vector< T, L > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] == b[ i ] ) ) return false;
		}
		return true; // No elements differ
	}
}

// ChunkVector != std::vector Template
template< typename T, typename L >
inline
bool
operator !=( ChunkVector< T > const & a, std::vector< T, L > const & b )
{
	return !( a == b );
}

// ChunkVector < std::vector
template< typename T, typename L >
inline
bool
operator <( ChunkVector< T > const & a, std::vector< T, L > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] < b[ i ] ) ) return false;
		}
		return true;
	}
}

// ChunkVector <= std::vector
template< typename T, typename L >
inline
bool
operator <=( ChunkVector< T > const & a, std::vector< T, L > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] <= b[ i ] ) ) return false;
		}
		return true;
	}
}

// ChunkVector >= std::vector
template< typename T, typename L >
inline
bool
operator >=( ChunkVector< T > const & a, std::vector< T, L > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] >= b[ i ] ) ) return false;
		}
		return true;
	}
}

// ChunkVector > std::vector
template< typename T, typename L >
inline
bool
operator >( ChunkVector< T > const & a, std::vector< T, L > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] > b[ i ] ) ) return false;
		}
		return true;
	}
}

// Is a std::vector comparable to a ChunkVector?
template< typename T, typename L >
inline
bool
comparable( std::vector< T, L > const & a, ChunkVector< T > const & b )
{
	return ( a.size() == b.size() );
}

// std::vector == ChunkVector Template
template< typename T, typename L >
inline
bool
operator ==( std::vector< T, L > const & a, ChunkVector< T > const & b )
{
	return ( b == a );
}

// std::vector != ChunkVector Template
template< typename T, typename L >
inline
bool
operator !=( std::vector< T, L > const & a, ChunkVector< T > const & b )
{
	return !( b == a );
}

// std::vector < ChunkVector
template< typename T, typename L >
inline
bool
operator <( std::vector< T, L > const & a, ChunkVector< T > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] < b[ i ] ) ) return false;
		}
		return true;
	}
}

// std::vector <= ChunkVector
template< typename T, typename L >
inline
bool
operator <=( std::vector< T, L > const & a, ChunkVector< T > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] <= b[ i ] ) ) return false;
		}
		return true;
	}
}

// std::vector >= ChunkVector
template< typename T, typename L >
inline
bool
operator >=( std::vector< T, L > const & a, ChunkVector< T > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] >= b[ i ] ) ) return false;
		}
		return true;
	}
}

// std::vector > ChunkVector
template< typename T, typename L >
inline
bool
operator >( std::vector< T, L > const & a, ChunkVector< T > const & b )
{
	if ( a.size() != b.size() ) { // Sizes differ
		return false;
	} else { // Compare values
		for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
			if ( !( a[ i ] > b[ i ] ) ) return false;
		}
		return true;
	}
}

// Comparison: Value

// ChunkVector == Value
template< typename T >
inline
bool
operator ==( ChunkVector< T > const & a, typename ChunkVector< T >::Tc t )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( a[ i ] != t ) return false;
	}
	return true;
}

// ChunkVector != Value
template< typename T >
inline
bool
operator !=( ChunkVector< T > const & a, typename ChunkVector< T >::Tc t )
{
	return !( a == t );
}

// ChunkVector < Value
template< typename T >
inline
bool
operator <( ChunkVector< T > const & a, typename ChunkVector< T >::Tc t )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] < t ) ) return false;
	}
	return true;
}

// ChunkVector <= Value
template< typename T >
inline
bool
operator <=( ChunkVector< T > const & a, typename ChunkVector< T >::Tc t )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] <= t ) ) return false;
	}
	return true;
}

// ChunkVector >= T
template< typename T >
inline
bool
operator >=( ChunkVector< T > const & a, typename ChunkVector< T >::Tc t )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] >= t ) ) return false;
	}
	return true;
}

// ChunkVector > Value
template< typename T >
inline
bool
operator >( ChunkVector< T > const & a, typename ChunkVector< T >::Tc t )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( a[ i ] > t ) ) return false;
	}
	return true;
}

// Value == ChunkVector
template< typename T >
inline
bool
operator ==( typename ChunkVector< T >::Tc t, ChunkVector< T > const & a )
{
	return ( a == t );
}

// Value != ChunkVector
template< typename T >
inline
bool
operator !=( typename ChunkVector< T >::Tc t, ChunkVector< T > const & a )
{
	return !( a == t );
}

// Value < ChunkVector
template< typename T >
inline
bool
operator <( typename ChunkVector< T >::Tc t, ChunkVector< T > const & a )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t < a[ i ] ) ) return false;
	}
	return true;
}

// Value <= ChunkVector
template< typename T >
inline
bool
operator <=( typename ChunkVector< T >::Tc t, ChunkVector< T > const & a )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t <= a[ i ] ) ) return false;
	}
	return true;
}

// Value >= ChunkVector
template< typename T >
inline
bool
operator >=( typename ChunkVector< T >::Tc t, ChunkVector< T > const & a )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t >= a[ i ] ) ) return false;
	}
	return true;
}

// Value > ChunkVector
template< typename T >
inline
bool
operator >( typename ChunkVector< T >::Tc t, ChunkVector< T > const & a )
{
	for ( typename ChunkVector< T >::size_type i = 0, ie = a.size(); i < ie; ++i ) {
		if ( !( t > a[ i ] ) ) return false;
	}
	return true;
}

// Stream >> ChunkVector
template< typename T >
inline
std::istream &
operator >>( std::istream & stream, ChunkVector< T > & v )
{
	typedef  typename ChunkVector< T >::size_type  size_type;
	if ( stream && ( ! v.emtpy() ) ) {
		for ( size_type i = 0, e = v.size(); i < e; ++i ) {
			stream >> v[ i ];
			if ( ! stream ) break;
		}
	}
	return stream;
}

// Stream << ChunkVector
template< typename T >
inline
std::ostream &
operator <<( std::ostream & stream, ChunkVector< T > const & v )
{
	using std::setw;
	typedef  TypeTraits< T >  Traits;
	typedef  typename ChunkVector< T >::size_type  size_type;
	if ( stream && ( ! v.empty() ) ) {
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision ) );
		stream << std::right << std::showpoint << std::uppercase;
		size_type const e( v.size() - 1 );
		int const w( Traits::iwidth );
		for ( size_type i = 0; i < e; ++i ) {
			stream << setw( w ) << v[ i ] << ' ';
		} stream << setw( w ) << v[ e ];
		stream.precision( old_precision );
		stream.flags( old_flags );
	}
	return stream;
}

} // ObjexxFCL

#endif // ObjexxFCL_ChunkVector_hh_INCLUDED
