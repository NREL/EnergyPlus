#ifndef ObjexxFCL_IndexRange_hh_INCLUDED
#define ObjexxFCL_IndexRange_hh_INCLUDED

// IndexRange: Index Range Abstract Base Class
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
#include <ObjexxFCL/Index.hh>
#include <ObjexxFCL/Star.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <initializer_list>
#include <iosfwd>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// Forward
class Dimension;

// IndexRange: Index Range Abstract Base Class
//
// Note:
//  Zero-size range is indicated by ( l - 1 == u ) and ( size == 0 )
//  Upper-unbounded range is indicated by ( l - 2 == u ) and ( size == npos )
//  Legal ranges have ( l - 2 <= u ) with l and u in their allowed ranges
class IndexRange
{

private: // Friend

	friend class StaticIndexRange;
	friend class DynamicIndexRange;

public: // Types

	// STL style
	typedef  std::size_t  size_type;

	// C++ style
	typedef  std::size_t  Size;

protected: // Creation

	// Default Constructor
	inline
	IndexRange() :
	 l_( 1 ),
	 u_( 0 ),
	 size_( 0 )
	{}

	// Copy Constructor
	inline
	IndexRange( IndexRange const & I ) :
	 l_( I.l_ ),
	 u_( I.u_ ),
	 size_( I.size_ )
	{}

	// Upper Index Constructor
	inline
	IndexRange( int const u ) :
	 l_( 1 ),
	 u_( clean_u( u ) ),
	 size_( u_ )
	{}

	// Star Constructor
	inline
	IndexRange( Star const ) :
	 l_( 1 ),
	 u_( -1 ),
	 size_( npos )
	{}

	// Index Range Constructor
	inline
	IndexRange( int const l, int const u ) :
	 l_( l ),
	 u_( clean_u( u ) ),
	 size_( computed_size() )
	{}

	// Lower Index + Star Constructor
	inline
	IndexRange( int const l, Star const ) :
	 l_( l ),
	 u_( l_ - 2 ),
	 size_( npos )
	{}

	// Star + Upper Index Constructor
	inline
	IndexRange( Star const, int const u ) :
	 l_( u + 2 ),
	 u_( u ),
	 size_( npos )
	{}

	// Initializer List of Integer Constructor
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	IndexRange( std::initializer_list< U > const lu ) :
	 l_( 1 ),
	 u_( 0 ),
	 size_( 0 )
	{
		size_type const n( lu.size() );
		assert( n <= 2 );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			break;
		case 1: // {l}
			l_ = *i;
			u_ = l_ - 2; // Unbounded
			break;
		case 2: // {l,u}
			l_ = *i;
			u_ = clean_u( *(++i) );
			break;
		}
		size_ = computed_size();
	}

	// Initializer List of Index Constructor
	inline
	IndexRange( std::initializer_list< Index > const lu ) :
	 l_( 1 ),
	 u_( 0 ),
	 size_( 0 )
	{
		size_type const n( lu.size() );
		assert( n <= 2 );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			break;
		case 1: // {l}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = l_ - 2; // Unbounded
			break;
		case 2: // {l,u}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = (++i)->initialized() ? clean_u( int( *i ) ) : l_ - 2; // Omit => Unbounded
			break;
		}
		size_ = computed_size();
	}

	// Omit Constructor
	inline
	IndexRange( Omit const ) :
	 l_( 1 ),
	 u_( -1 ),
	 size_( npos )
	{}

	// Lower Index + Omit Constructor
	inline
	IndexRange( int const l, Omit const ) :
	 l_( l ),
	 u_( l_ - 2 ),
	 size_( npos )
	{}

	// Omit + Upper Index Constructor
	inline
	IndexRange( Omit const, int const u ) :
	 l_( u + 2 ),
	 u_( u ),
	 size_( npos )
	{}

	// Omit + Omit Constructor
	inline
	IndexRange( Omit const, Omit const ) :
	 l_( 1 ),
	 u_( -1 ),
	 size_( npos )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~IndexRange()
	{}

protected: // Assignment

	// Copy Assignment
	inline
	void
	operator =( IndexRange const & I )
	{
		l_ = I.l_;
		u_ = I.u_;
		size_ = I.size_;
	}

public: // Assignment

	// Upper Index Assignment
	inline
	virtual
	IndexRange &
	operator =( int const u )
	{
		l_ = 1;
		u_ = clean_u( u );
		size_ = u_;
		return *this;
	}

	// Unbounded Upper Index Assignment
	inline
	virtual
	IndexRange &
	operator =( Star const )
	{
		l_ = 1;
		u_ = -1;
		size_ = npos;
		return *this;
	}

	// Initializer List of int Assignment
	template< typename U, class = typename std::enable_if< std::is_assignable< int&, U >::value >::type >
	inline
	IndexRange &
	operator =( std::initializer_list< U > const lu )
	{
		size_type const n( lu.size() );
		assert( n <= 2 );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			break;
		case 1: // {l}
			l_ = *i;
			u_ = l_ - 2; // Unbounded
			break;
		case 2: // {l,u}
			l_ = *i;
			u_ = clean_u( *(++i) );
			break;
		}
		size_ = computed_size();
		return *this;
	}

	// Initializer List of Index Assignment
	inline
	IndexRange &
	operator =( std::initializer_list< Index > const lu )
	{
		size_type const n( lu.size() );
		assert( n <= 2 );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			break;
		case 1: // {l}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = l_ - 2; // Unbounded
			break;
		case 2: // {l,u}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = (++i)->initialized() ? clean_u( int( *i ) ) : l_ - 2; // Omit => Unbounded
			break;
		}
		size_ = computed_size();
		return *this;
	}

	// Upper Index Assignment
	inline
	virtual
	IndexRange &
	assign( int const u )
	{
		l_ = 1;
		u_ = clean_u( u );
		size_ = u_;
		return *this;
	}

	// Unbounded Upper Index Assignment
	inline
	virtual
	IndexRange &
	assign( Star const )
	{
		l_ = 1;
		u_ = -1;
		size_ = npos;
		return *this;
	}

	// Index Range Assignment
	inline
	virtual
	IndexRange &
	assign( int const l, int const u )
	{
		l_ = l;
		u_ = clean_u( u );
		size_ = computed_size();
		return *this;
	}

	// Index and Unbounded Upper Index Assignment
	inline
	virtual
	IndexRange &
	assign( int const l, Star const )
	{
		l_ = l;
		u_ = l_ - 2;
		size_ = npos;
		return *this;
	}

public: // Subscript

	// IndexRange( i ) const: Internal Index for i
	inline
	int
	operator ()( int const i ) const
	{
		return l_ + ( i - 1 );
	}

public: // Predicate

	// Initialized?
	inline
	virtual
	bool
	initialized() const
	{
		return true;
	}

	// Lower Initialized?
	inline
	virtual
	bool
	l_initialized() const
	{
		return true;
	}

	// Upper Initialized?
	inline
	virtual
	bool
	u_initialized() const
	{
		return true;
	}

	// Legal?
	inline
	virtual
	bool
	legal() const
	{
		return ( ( l_ >= l_min ) && ( u_ <= u_max ) && ( l_ - 2 <= u_ ) );
	}

	// Bounded?
	inline
	virtual
	bool
	bounded() const
	{
		return ( l_ - 1 <= u_ );
	}

	// Unbounded?
	inline
	virtual
	bool
	unbounded() const
	{
		return ( l_ - 2 == u_ );
	}

	// Not Unbounded?
	inline
	virtual
	bool
	not_unbounded() const
	{
		return ( l_ - 1 <= u_ );
	}

	// Empty?
	inline
	virtual
	bool
	empty() const
	{
		return ( l_ - 1 == u_ );
	}

	// Non-Empty?
	inline
	virtual
	bool
	non_empty() const
	{
		return ( l_ <= u_ );
	}

	// Bounded with Positive Size?
	inline
	virtual
	bool
	positive() const
	{
		return ( l_ <= u_ );
	}

	// Contains an Index?
	inline
	virtual
	bool
	contains( int const i ) const
	{
		return ( ( l_ <= i ) && ( ( i <= u_ ) || ( size_ == npos ) ) );
	}

	// Contains Two Indexes?
	inline
	virtual
	bool
	contains( int const i, int const j ) const
	{
		return ( contains( i ) && contains( j ) );
	}

	// Contains Another IndexRange?
	virtual
	bool
	contains( IndexRange const & I ) const;

	// Intersects Another IndexRange?
	virtual
	bool
	intersects( IndexRange const & I ) const;

public: // Inspector

	// Lower Index
	inline
	int
	l() const
	{
		assert( l_initialized() );
		return l_;
	}

	// Lower Index (1 if Uninitialized)
	inline
	int
	lz() const
	{
		return l_;
	}

	// Upper Index
	inline
	int
	u() const
	{
		assert( u_initialized() );
		return u_;
	}

	// Size
	inline
	size_type
	size() const
	{
		return size_; // Unbounded => npos
	}

	// Size
	inline
	int
	isize() const
	{
		assert( size_ != npos );
		return static_cast< int >( size_ );
	}

	// Offset of an Index
	inline
	int
	offset( int const i ) const
	{
		assert( l_initialized() );
		return ( i - l_ ); // Doesn't check/require that IndexRange includes i
	}

	// Last Index
	inline
	int
	last() const
	{
		return std::max( l_, u_ );
	}

	// Next Index
	inline
	int
	next( int const i ) const
	{
		return i + 1; // Doesn't check that this is a valid index
	}

public: // Modifier

	// Lower Index Set
	inline
	virtual
	IndexRange &
	l( int const l )
	{
		if ( l_ - 2 == u_ ) { // Unbounded range
			l_ = l;
			u_ = l_ - 2; // Reset u_ to maintain unbounded state
		} else { // Bounded
			l_ = l;
			size_ = computed_size();
			clean();
		}
		return *this;
	}

	// Upper Index Set
	inline
	virtual
	IndexRange &
	u( int const u )
	{
		u_ = clean_u( u );
		size_ = computed_size();
		return *this;
	}

	// Unbounded Upper Index Set
	inline
	virtual
	IndexRange &
	u( Star const )
	{
		u_ = l_ - 2;
		size_ = npos;
		return *this;
	}

	// Expand to Contain an Index
	inline
	virtual
	IndexRange &
	contain( int const i )
	{
		if ( l_ - 1 <= u_ ) { // Bounded
			if ( l_ > i ) l_ = i;
			if ( u_ < i ) u_ = i;
			size_ = computed_size();
		} else { // Unbounded
			if ( l_ > i ) {
				l_ = i;
				u_ = l_ - 2; // Reset u_ to maintain unbounded state
			}
		}
		return *this;
	}

	// Expand to Contain Another IndexRange
	virtual
	IndexRange &
	contain( IndexRange const & I );

	// Intersect with Another IndexRange
	virtual
	IndexRange &
	intersect( IndexRange const & I );

	// Clear
	inline
	virtual
	IndexRange &
	clear()
	{
		l_ = 1;
		u_ = 0;
		size_ = 0u;
		return *this;
	}

protected: // Inspector

	// Legal Static Range?
	inline
	bool
	legal_static() const
	{
		return ( ( l_ >= l_min ) && ( u_ <= u_max ) && ( l_ - 2 <= u_ ) );
	}

	// Lower Dimension Clone
	inline
	virtual
	Dimension *
	l_dim_clone() const
	{
		return 0;
	}

	// Upper Dimension Clone
	inline
	virtual
	Dimension *
	u_dim_clone() const
	{
		return 0;
	}

protected: // Modifier

	// Clean
	inline
	void
	clean()
	{
		if ( initialized() && ( l_ > u_ ) ) {
			l_ = 1;
			u_ = 0;
			size_ = 0u;
		}
	}

	// Clean Upper Index Value After Lower Index Set
	inline
	int
	clean_u( int const u )
	{
		if ( initialized() && ( l_ > u ) ) {
			l_ = 1; // Changes lower index: Side effect
			return 0;
		} else {
			return u;
		}
	}

	// Swap
	inline
	void
	swap( IndexRange & I )
	{
		if ( this != &I ) {
			std::swap( l_, I.l_ );
			std::swap( u_, I.u_ );
			std::swap( size_, I.size_ );
		}
	}

private: // Inspector

	// Computed Size
	inline
	size_type
	computed_size() const
	{
		return std::max( u_ - l_ + 1, -1 );
	}

public: // Data

	static size_type const npos = static_cast< size_type >( -1 ); // Unbounded "size"

	static int const l_min = -( static_cast< int >( ( static_cast< unsigned int >( -1 ) / 2u ) ) - 1 ); // Min lower index

	static int const u_max = static_cast< int >( ( static_cast< unsigned int >( -1 ) / 2u ) ); // Max upper index

private: // Data

	int l_; // Lower index
	int u_; // Upper index

	size_type size_; // Size (npos iff unbounded)

}; // IndexRange

// Types
typedef  IndexRange  IRange;

// Comparison

// IndexRange == IndexRange
inline
bool
operator ==( IndexRange const & I, IndexRange const & J )
{
	return ( I.initialized() && J.initialized() && ( I.l() == J.l() ) && ( I.u() == J.u() ) );
}

// IndexRange != IndexRange
inline
bool
operator !=( IndexRange const & I, IndexRange const & J )
{
	return !( I == J );
}

// IndexRange < IndexRange
inline
bool
operator <( IndexRange const & I, IndexRange const & J )
{
	return ( I.positive() && J.positive() && ( I.u() < J.l() ) );
}

// IndexRange <= IndexRange
inline
bool
operator <=( IndexRange const & I, IndexRange const & J )
{
	return ( I.positive() && J.positive() && ( I.u() <= J.l() ) );
}

// IndexRange > IndexRange
inline
bool
operator >( IndexRange const & I, IndexRange const & J )
{
	return ( I.positive() && J.positive() && ( I.l() > J.u() ) );
}

// IndexRange >= IndexRange
inline
bool
operator >=( IndexRange const & I, IndexRange const & J )
{
	return ( I.positive() && J.positive() && ( I.l() >= J.u() ) );
}

// I/O

// Stream Input
std::istream &
operator >>( std::istream & stream, IndexRange & I );

// Stream Output
std::ostream &
operator <<( std::ostream & stream, IndexRange const & I );

} // ObjexxFCL

#endif // ObjexxFCL_IndexRange_hh_INCLUDED
