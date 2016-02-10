#ifndef ObjexxFCL_IndexSlice_hh_INCLUDED
#define ObjexxFCL_IndexSlice_hh_INCLUDED

// IndexSlice: Index Slice Class
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
#include <ObjexxFCL/Index.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <initializer_list>
#include <iosfwd>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// IndexSlice: Index Slice Class
class IndexSlice
{

public: // Types

	// STL style
	typedef  std::size_t  size_type;

	// C++ style
	typedef  std::size_t  Size;

public: // Creation

	// Default Constructor
	IndexSlice() :
	 l_init_( false ),
	 u_init_( false ),
	 scalar_( false ),
	 l_( 1 ),
	 u_( 0 ),
	 s_( 1 ),
	 size_( 0u )
	{}

	// Copy Constructor
	IndexSlice( IndexSlice const & I ) :
	 l_init_( I.l_init_ ),
	 u_init_( I.u_init_ ),
	 scalar_( I.scalar_ ),
	 l_( I.l_ ),
	 u_( I.u_ ),
	 s_( I.s_ ),
	 size_( I.size_ )
	{
		assert( size_ == computed_size() );
	}

	// Scalar Constructor
	IndexSlice( int const i ) :
	 l_init_( true ),
	 u_init_( true ),
	 scalar_( true ),
	 l_( i ),
	 u_( i ),
	 s_( 1 ),
	 size_( 1u )
	{}

	// Index Slice Constructor
	IndexSlice( int const l, int const u, int const s = 1 ) :
	 l_init_( true ),
	 u_init_( true ),
	 scalar_( false ),
	 l_( l ),
	 u_( u ),
	 s_( s ),
	 size_( computed_size() )
	{
		assert( s_ != 0 );
	}

	// Initializer List of Integer Constructor
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	IndexSlice( std::initializer_list< U > const lus ) :
	 l_init_( lus.size() > 0u ),
	 u_init_( lus.size() > 1u ),
	 scalar_( false ),
	 l_( 1 ),
	 u_( 0 ),
	 s_( 1 ),
	 size_( 0u )
	{
		size_type const n( lus.size() );
		assert( n <= 3 );
		auto i( lus.begin() );
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			s_ = 1;
			break;
		case 1: // {l}
			l_ = *i;
			u_ = 0;
			s_ = 1;
			break;
		case 2: // {l,u}
			l_ = *i;
			u_ = *(++i);
			s_ = 1;
			break;
		case 3: // {l,u,s}
			l_ = *i;
			u_ = *(++i);
			s_ = *(++i);
			break;
		}
		assert( s_ != 0 );
		size_ = computed_size();
	}

	// Initializer List of Index Constructor
	IndexSlice( std::initializer_list< Index > const lus ) :
	 l_init_( ( lus.size() > 0u ) && ( lus.begin()->initialized() ) ),
	 u_init_( ( lus.size() > 1u ) && ( ( lus.begin() + 1 )->initialized() ) ),
	 scalar_( false ),
	 l_( 1 ),
	 u_( 0 ),
	 s_( 1 ),
	 size_( 0u )
	{
		size_type const n( lus.size() );
		assert( n <= 3 );
		auto i( lus.begin() );
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			s_ = 1;
			break;
		case 1: // {l}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = 0;
			s_ = 1;
			break;
		case 2: // {l,u}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = (++i)->initialized() ? int( *i ) : 0;
			s_ = 1;
			break;
		case 3: // {l,u,s}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = (++i)->initialized() ? int( *i ) : 0;
			s_ = (++i)->initialized() ? int( *i ) : 1;
			break;
		}
		assert( s_ != 0 );
		size_ = computed_size();
	}

	// Omit Constructor
	IndexSlice( Omit ) :
	 l_init_( false ),
	 u_init_( false ),
	 scalar_( false ),
	 l_( 1 ),
	 u_( 0 ),
	 s_( 1 ),
	 size_( 0u )
	{
		assert( s_ != 0 );
	}

	// Lower Index + Omit Constructor
	IndexSlice( int const l, Omit, int const s = 1 ) :
	 l_init_( true ),
	 u_init_( false ),
	 scalar_( false ),
	 l_( l ),
	 u_( 0 ),
	 s_( s ),
	 size_( 0u )
	{
		assert( s_ != 0 );
	}

	// Omit + Upper Index Constructor
	IndexSlice( Omit, int const u, int const s = 1 ) :
	 l_init_( false ),
	 u_init_( true ),
	 scalar_( false ),
	 l_( 1 ),
	 u_( u ),
	 s_( s ),
	 size_( 0u )
	{
		assert( s_ != 0 );
	}

	// Omit + Omit Constructor
	IndexSlice( Omit, Omit, int const s = 1 ) :
	 l_init_( false ),
	 u_init_( false ),
	 scalar_( false ),
	 l_( 1 ),
	 u_( 0 ),
	 s_( s ),
	 size_( 0u )
	{
		assert( s_ != 0 );
	}

	// Destructor
	~IndexSlice()
	{}

public: // Assignment

	// Scalar Assignment
	IndexSlice &
	operator =( int const i )
	{
		l_init_ = true;
		u_init_ = true;
		scalar_ = true;
		l_ = i;
		u_ = i;
		s_ = 1;
		size_ = 1u;
		return *this;
	}

	// Initializer List of int Assignment
	IndexSlice &
	operator =( std::initializer_list< int > const lus )
	{
		size_type const n( lus.size() );
		assert( n <= 3 );
		l_init_ = n > 0u;
		u_init_ = n > 1u;
		scalar_ = false;
		auto i( lus.begin() );
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			s_ = 1;
			break;
		case 1: // {l}
			l_ = *i;
			u_ = 0;
			s_ = 1;
			break;
		case 2: // {l,u}
			l_ = *i;
			u_ = *(++i);
			s_ = 1;
			break;
		case 3: // {l,u,s}
			l_ = *i;
			u_ = *(++i);
			s_ = *(++i);
			break;
		}
		assert( s_ != 0 );
		size_ = computed_size();
		return *this;
	}

	// Initializer List of Index Assignment
	IndexSlice &
	operator =( std::initializer_list< Index > const lus )
	{
		size_type const n( lus.size() );
		assert( n <= 3 );
		auto i( lus.begin() );
		l_init_ = ( n > 0u ) && ( i->initialized() );
		u_init_ = ( n > 1u ) && ( ( i + 1 )->initialized() );
		scalar_ = false;
		switch ( n ) {
		case 0: // {}
			l_ = 1;
			u_ = 0;
			s_ = 1;
			break;
		case 1: // {l}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = 0;
			s_ = 1;
			break;
		case 2: // {l,u}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = (++i)->initialized() ? int( *i ) : 0;
			s_ = 1;
			break;
		case 3: // {l,u,s}
			l_ = i->initialized() ? int( *i ) : 1;
			u_ = (++i)->initialized() ? int( *i ) : 0;
			s_ = (++i)->initialized() ? int( *i ) : 1;
			break;
		}
		assert( s_ != 0 );
		size_ = computed_size();
		return *this;
	}

	// Index Slice Assignment
	IndexSlice &
	assign( int const l, int const u, int const s = 1 )
	{
		l_init_ = true;
		u_init_ = true;
		scalar_ = false;
		l_ = l;
		u_ = u;
		s_ = s;
		size_ = computed_size();
		return *this;
	}

public: // Subscript

	// IndexSlice( i ) const: Internal Index for i
	int
	operator ()( int const i ) const
	{
		assert( l_init_ );
		return ( scalar_ ? l_ : l_ + ( i - 1 ) * s_ ); // Doesn't check that i <= size of slice
	}

public: // Predicate

	// Initialized?
	bool
	initialized() const
	{
		return ( l_init_ && u_init_ );
	}

	// Lower Initialized?
	bool
	l_initialized() const
	{
		return l_init_;
	}

	// Upper Initialized?
	bool
	u_initialized() const
	{
		return u_init_;
	}

	// Scalar?
	bool
	scalar() const
	{
		return scalar_;
	}

	// Contains an Index?
	bool
	contains( int const i ) const
	{
		assert( l_init_ && u_init_ );
		return ( scalar_ ? i == l_ : ( std::min( l_, u_ ) <= i ) && ( i <= std::max( l_, u_ ) ) && ( ( ( i - l_ ) % s_ ) == 0 ) );
	}

public: // Inspector

	// Lower (Begin) Index
	int
	l() const
	{
		assert( l_initialized() );
		return l_;
	}

	// Upper (End) Index
	int
	u() const
	{
		assert( u_initialized() );
		return u_;
	}

	// Step
	int
	s() const
	{
		return s_;
	}

	// Size
	size_type
	size() const
	{
		return size_;
	}

	// Size
	int
	isize() const
	{
		return static_cast< int >( size_ );
	}

	// Last Index
	int
	last() const
	{
		assert( initialized() );
		assert( s_ != 0 );
		return l_ + ( s_ * ( size_ > 0u ? int( size_ - 1u ) : 0 ) ); // Get l_ if size==0
	}

	// Next Index
	int
	next( int const i ) const
	{
		assert( l_init_ );
		assert( contains( i ) );
		return ( scalar_ ? l_ : i + s_ ); // Doesn't check that this is a valid index
	}

	// Min Index
	int
	min() const
	{
		assert( initialized() );
		return std::min( l_, last() );
	}

	// Max Index
	int
	max() const
	{
		assert( initialized() );
		return std::max( l_, last() );
	}

	// Empty or Undefined?
	bool
	empty() const
	{
		return ( size_ == 0u );
	}

	// Non-Empty?
	bool
	non_empty() const
	{
		return ( size_ > 0u );
	}

public: // Modifier

	// Clear
	void
	clear()
	{
		l_init_ = false;
		u_init_ = false;
		scalar_ = false;
		l_ = 1;
		u_ = 0;
		s_ = 1;
		size_ = 0u;
	}

	// Scalar Index Set
	IndexSlice &
	i( int const i )
	{
		l_init_ = true;
		u_init_ = true;
		scalar_ = true;
		l_ = i;
		u_ = i;
		s_ = 1;
		size_ = 1u;
		return *this;
	}

	// Lower Index Set
	IndexSlice &
	l( int const l )
	{
		l_init_ = true;
		scalar_ = false;
		l_ = l;
		size_ = computed_size();
		return *this;
	}

	// Upper Index Set
	IndexSlice &
	u( int const u )
	{
		u_init_ = true;
		scalar_ = false;
		u_ = u;
		size_ = computed_size();
		return *this;
	}

	// Index Set if Uninitialized
	void
	lud( int const l, int const u )
	{
		if ( ! l_init_ ) {
			l_ = l;
			l_init_ = true;
		}
		if ( ( ! u_init_ ) && ( l - 1 <= u ) ) {
			u_ = u;
			u_init_ = true;
		}
		scalar_ = false;
		size_ = computed_size();
	}

	// Swap
	void
	swap( IndexSlice & I )
	{
		if ( this != &I ) {
			std::swap( l_init_, I.l_init_ );
			std::swap( u_init_, I.u_init_ );
			std::swap( scalar_, I.scalar_ );
			std::swap( l_, I.l_ );
			std::swap( u_, I.u_ );
			std::swap( s_, I.s_ );
			std::swap( size_, I.size_ );
		}
	}

public: // Friend

	// Swap
	friend
	void
	swap( IndexSlice & a, IndexSlice & b )
	{
		a.swap( b );
	}

public: // Comparison

	// IndexSlice == IndexSlice
	friend
	bool
	operator ==( IndexSlice const & I, IndexSlice const & J )
	{
		return ( ( I.l_init_ == J.l_init_ ) && ( I.u_init_ == J.u_init_ ) && ( I.scalar_ == J.scalar_ ) && ( I.l_ == J.l_ ) && ( I.u_ == J.u_ ) && ( I.s_ == J.s_ ) );
	}

	// IndexSlice != IndexSlice
	friend
	bool
	operator !=( IndexSlice const & I, IndexSlice const & J )
	{
		return !( I == J );
	}

public: // I/O

	// Stream >> IndexSlice
	friend
	std::istream &
	operator >>( std::istream & stream, IndexSlice & I );

	// Stream << IndexSlice
	friend
	std::ostream &
	operator <<( std::ostream & stream, IndexSlice const & I );

private: // Inspector

	// Computed Size
	size_type
	computed_size() const
	{
		assert( s_ != 0 || ! l_init_ || ! u_init_ );
		return ( l_init_ && u_init_ ? std::max( ( u_ - l_ + s_ ) / s_, 0 ) : 0 );
	}

private: // Data

	bool l_init_; // Lower index initialized?
	bool u_init_; // Upper index initialized?
	bool scalar_; // Scalar slice?

	int l_; // Lower index
	int u_; // Upper index
	int s_; // Step

	size_type size_; // Size (0 if unknown)

}; // IndexSlice

// Types
typedef  IndexSlice  ISlice;

// IndexSlice == IndexSlice
bool
operator ==( IndexSlice const & I, IndexSlice const & J );

// IndexSlice != IndexSlice
bool
operator !=( IndexSlice const & I, IndexSlice const & J );

// Stream >> IndexSlice
std::istream &
operator >>( std::istream & stream, IndexSlice & I );

// Stream << IndexSlice
std::ostream &
operator <<( std::ostream & stream, IndexSlice const & I );

// Swap
void
swap( IndexSlice & a, IndexSlice & b );

} // ObjexxFCL

#endif // ObjexxFCL_IndexSlice_hh_INCLUDED
