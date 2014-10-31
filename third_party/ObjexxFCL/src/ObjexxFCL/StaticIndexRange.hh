#ifndef ObjexxFCL_StaticIndexRange_hh_INCLUDED
#define ObjexxFCL_StaticIndexRange_hh_INCLUDED

// StaticIndexRange: Static Index Range
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
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/Dimension.hh>

namespace ObjexxFCL {

// StaticIndexRange: Static Index Range
//
// Note:
//  Zero-size range is indicated by ( l - 1 == u ) and ( size == 0 )
//  Upper-unbounded range is indicated by ( l - 2 == u ) and ( size == npos )
//  Legal ranges have ( l - 2 <= u ) with l and u in their allowed ranges
class StaticIndexRange : public IndexRange
{

private: // Types

	typedef  IndexRange  Super;

public: // Types

	using Super::l;
	using Super::u;

	typedef  DimensionExpression  Expression;

public: // Creation

	// Default Constructor
	inline
	StaticIndexRange()
	{}

	// Copy Constructor
	inline
	StaticIndexRange( StaticIndexRange const & I ) :
	 Super( I )
	{}

	// IndexRange Constructor
	inline
	StaticIndexRange( IndexRange const & I ) :
	 Super( I )
	{
		assert( I.initialized() );
	}

	// Upper Index Constructor
	inline
	StaticIndexRange( int const u ) :
	 Super( u )
	{
		assert( legal_static() );
	}

	// Star Constructor
	inline
	StaticIndexRange( Star const s ) :
	 Super( s )
	{}

	// Upper Dimension Constructor
	inline
	StaticIndexRange( Dimension const & u_dim ) :
	 Super( u_dim.value() )
	{
		assert( legal_static() );
	}

	// Upper Expression Constructor
	inline
	StaticIndexRange( Expression const & u_exp ) :
	 Super( u_exp.ivalue() )
	{
		assert( legal_static() );
	}

	// Index Range Constructor
	inline
	StaticIndexRange( int const l, int const u ) :
	 Super( l, u )
	{
		assert( legal_static() );
	}

	// Lower Index + Star Constructor
	inline
	StaticIndexRange( int const l, Star const s ) :
	 Super( l, s )
	{
		assert( legal_static() );
	}

	// Star + Upper Index Constructor
	inline
	StaticIndexRange( Star const s, int const u ) :
	 Super( s, u )
	{
		assert( legal_static() );
	}

	// Initializer List of Integer Constructor
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	StaticIndexRange( std::initializer_list< U > const lu ) :
	 Super( lu )
	{
		assert( legal_static() );
	}

	// Initializer List of Index Constructor
	inline
	StaticIndexRange( std::initializer_list< Index > const lu ) :
	 Super( lu )
	{
		assert( legal_static() );
	}

	// Omit Constructor
	inline
	StaticIndexRange( Omit const o ) :
	 Super( o )
	{
		assert( legal_static() );
	}

	// Lower Index + Omit Constructor
	inline
	StaticIndexRange( int const l, Omit const o ) :
	 Super( l, o )
	{
		assert( legal_static() );
	}

	// Omit + Upper Index Constructor
	inline
	StaticIndexRange( Omit const o, int const u ) :
	 Super( o, u )
	{
		assert( legal_static() );
	}

	// Omit + Omit Constructor
	inline
	StaticIndexRange( Omit const ol, Omit const ou ) :
	 Super( ol, ou )
	{
		assert( legal_static() );
	}

	// Dimension Range Constructor
	inline
	StaticIndexRange( Dimension const & l_dim, Dimension const & u_dim ) :
	 Super( l_dim.value(), u_dim.value() )
	{
		assert( legal_static() );
	}

	// Expression Range Constructor
	inline
	StaticIndexRange( Expression const & l_exp, Expression const & u_exp ) :
	 Super( l_exp.ivalue(), u_exp.ivalue() )
	{
		assert( legal_static() );
	}

	// Index and Dimension Constructor
	inline
	StaticIndexRange( int const l, Dimension const & u_dim ) :
	 Super( l, u_dim.value() )
	{
		assert( legal_static() );
	}

	// Dimension and Index Constructor
	inline
	StaticIndexRange( Dimension const & l_dim, int const u ) :
	 Super( l_dim.value(), u )
	{
		assert( legal_static() );
	}

	// Index and Expression Constructor
	inline
	StaticIndexRange( int const l, Expression const & u_exp ) :
	 Super( l, u_exp.ivalue() )
	{
		assert( legal_static() );
	}

	// Expression and Index Constructor
	inline
	StaticIndexRange( Expression const & l_exp, int const u ) :
	 Super( l_exp.ivalue(), u )
	{
		assert( legal_static() );
	}

	// Dimension and Expression Constructor
	inline
	StaticIndexRange( Dimension const & l_dim, Expression const & u_exp ) :
	 Super( l_dim.value(), u_exp.ivalue() )
	{
		assert( legal_static() );
	}

	// Expression and Dimension Constructor
	inline
	StaticIndexRange( Expression const & l_exp, Dimension const & u_dim ) :
	 Super( l_exp.ivalue(), u_dim.value() )
	{
		assert( legal_static() );
	}

	// Dimension and Unbounded Upper Index Constructor
	inline
	StaticIndexRange( Dimension const & l_dim, Star const s ) :
	 Super( l_dim.value(), s )
	{
		assert( legal_static() );
	}

	// Expression and Unbounded Upper Index Constructor
	inline
	StaticIndexRange( Expression const & l_exp, Star const s ) :
	 Super( l_exp.ivalue(), s )
	{
		assert( legal_static() );
	}

	// Destructor
	inline
	virtual
	~StaticIndexRange()
	{}

public: // Assignment

	// Copy Assignment
	inline
	StaticIndexRange &
	operator =( StaticIndexRange const & I )
	{
		if ( this != &I ) {
			Super::operator =( I );
			assert( legal_static() );
		}
		return *this;
	}

	// IndexRange Assignment
	inline
	StaticIndexRange &
	operator =( IndexRange const & I )
	{
		if ( this != &I ) {
			assert( I.initialized() );
			Super::operator =( I );
			assert( legal_static() );
		}
		return *this;
	}

	// Upper Index Assignment
	inline
	StaticIndexRange &
	operator =( int const u )
	{
		Super::operator =( u );
		assert( legal_static() );
		return *this;
	}

	// Unbounded Upper Index Assignment
	inline
	StaticIndexRange &
	operator =( Star const s )
	{
		Super::operator =( s );
		return *this;
	}

	// Upper Dimension Assignment
	inline
	StaticIndexRange &
	operator =( Dimension const & u_dim )
	{
		Super::operator =( u_dim.value() );
		assert( legal_static() );
		return *this;
	}

	// Upper Expression Assignment
	inline
	StaticIndexRange &
	operator =( Expression const & u_exp )
	{
		Super::operator =( u_exp.ivalue() );
		assert( legal_static() );
		return *this;
	}

	// Initializer List of int Assignment
	template< typename U, class = typename std::enable_if< std::is_assignable< int&, U >::value >::type >
	inline
	StaticIndexRange &
	operator =( std::initializer_list< U > const lu )
	{
		Super::operator =( lu );
		assert( legal_static() );
		return *this;
	}

	// Initializer List of Index Assignment
	inline
	StaticIndexRange &
	operator =( std::initializer_list< Index > const lu )
	{
		Super::operator =( lu );
		assert( legal_static() );
		return *this;
	}

	// StaticIndexRange Assignment
	inline
	StaticIndexRange &
	assign( StaticIndexRange const & I )
	{
		if ( this != &I ) {
			Super::operator =( I );
			assert( legal_static() );
		}
		return *this;
	}

	// IndexRange Assignment
	inline
	StaticIndexRange &
	assign( IndexRange const & I )
	{
		if ( this != &I ) {
			assert( I.initialized() );
			Super::operator =( I );
			assert( legal_static() );
		}
		return *this;
	}

	// Upper Index Assignment
	inline
	StaticIndexRange &
	assign( int const u )
	{
		Super::operator =( u );
		assert( legal_static() );
		return *this;
	}

	// Unbounded Upper Index Assignment
	inline
	StaticIndexRange &
	assign( Star const s )
	{
		Super::operator =( s );
		return *this;
	}

	// Upper Dimension Assignment
	inline
	StaticIndexRange &
	assign( Dimension const & u_dim )
	{
		Super::operator =( u_dim.value() );
		assert( legal_static() );
		return *this;
	}

	// Upper Expression Assignment
	inline
	StaticIndexRange &
	assign( Expression const & u_exp )
	{
		Super::operator =( u_exp.ivalue() );
		assert( legal_static() );
		return *this;
	}

	// Index Range Assignment
	inline
	StaticIndexRange &
	assign( int const l, int const u )
	{
		Super::assign( l, u );
		assert( legal_static() );
		return *this;
	}

	// Dimension Range Assignment
	inline
	StaticIndexRange &
	assign( Dimension const & l_dim, Dimension const & u_dim )
	{
		Super::assign( l_dim.value(), u_dim.value() );
		assert( legal_static() );
		return *this;
	}

	// Expression Range Assignment
	inline
	StaticIndexRange &
	assign( Expression const & l_exp, Expression const & u_exp )
	{
		Super::assign( l_exp.ivalue(), u_exp.ivalue() );
		assert( legal_static() );
		return *this;
	}

	// Index and Dimension Assignment
	inline
	StaticIndexRange &
	assign( int const l, Dimension const & u_dim )
	{
		Super::assign( l, u_dim.value() );
		assert( legal_static() );
		return *this;
	}

	// Dimension and Index Assignment
	inline
	StaticIndexRange &
	assign( Dimension const & l_dim, int const u )
	{
		Super::assign( l_dim.value(), u );
		assert( legal_static() );
		return *this;
	}

	// Index and Expression Assignment
	inline
	StaticIndexRange &
	assign( int const l, Expression const & u_exp )
	{
		Super::assign( l, u_exp.ivalue() );
		assert( legal_static() );
		return *this;
	}

	// Expression and Index Assignment
	inline
	StaticIndexRange &
	assign( Expression const & l_exp, int const u )
	{
		Super::assign( l_exp.ivalue(), u );
		assert( legal_static() );
		return *this;
	}

	// Dimension and Expression Assignment
	inline
	StaticIndexRange &
	assign( Dimension const & l_dim, Expression const & u_exp )
	{
		Super::assign( l_dim.value(), u_exp.ivalue() );
		assert( legal_static() );
		return *this;
	}

	// Expression and Dimension Assignment
	inline
	StaticIndexRange &
	assign( Expression const & l_exp, Dimension const & u_dim )
	{
		Super::assign( l_exp.ivalue(), u_dim.value() );
		assert( legal_static() );
		return *this;
	}

	// Index and Unbounded Upper Index Assignment
	inline
	StaticIndexRange &
	assign( int const l, Star const s )
	{
		Super::assign( l, s );
		assert( legal_static() );
		return *this;
	}

	// Dimension and Unbounded Upper Index Assignment
	inline
	StaticIndexRange &
	assign( Dimension const & l_dim, Star const s )
	{
		Super::assign( l_dim.value(), s );
		assert( legal_static() );
		return *this;
	}

	// Expression and Unbounded Upper Index Assignment
	inline
	StaticIndexRange &
	assign( Expression const & l_exp, Star const s )
	{
		Super::assign( l_exp.ivalue(), s );
		assert( legal_static() );
		return *this;
	}

	// Assign Static Value of Another IndexRange: Faster Than operator =( I )
	inline
	void
	assign_value_of( IndexRange const & I )
	{ // Skips self-assignment check for speed
		l_ = I.l_;
		u_ = I.u_;
		size_ = I.size_;
		assert( legal_static() );
	}

public: // Inspector

	// Lower Index
	inline
	int
	l() const
	{
		return l_;
	}

	// Upper Index
	inline
	int
	u() const
	{
		return u_;
	}

	// Offset of an Index
	inline
	int
	offset( int const i ) const
	{
		return ( i - l_ ); // Doesn't check/require that IndexRange includes i
	}

public: // Modifier

	// Lower Index Set
	inline
	StaticIndexRange &
	l( int const l )
	{
		Super::l( l );
		assert( legal_static() );
		return *this;
	}

	// Upper Index Set
	inline
	StaticIndexRange &
	u( int const u )
	{
		Super::u( u );
		assert( legal_static() );
		return *this;
	}

	// Unbounded Upper Index Set
	inline
	StaticIndexRange &
	u( Star const s )
	{
		Super::u( s );
		assert( legal_static() );
		return *this;
	}

	// Shift Indexes
	inline
	StaticIndexRange &
	shift( int const s )
	{
		l_ += s;
		u_ += s;
		clean();
		assert( legal_static() );
		return *this;
	}

	// Shift Lower Index Preserving Size
	inline
	StaticIndexRange &
	shift_l( int const l )
	{
		u_ += l - l_;
		l_ = l;
		clean();
		assert( legal_static() );
		return *this;
	}

	// Expand to Contain an Index
	inline
	StaticIndexRange &
	contain( int const i )
	{
		Super::contain( i );
		assert( legal_static() );
		return *this;
	}

	// Expand to Contain Another IndexRange
	inline
	StaticIndexRange &
	contain( IndexRange const & I )
	{
		Super::contain( I );
		assert( legal_static() );
		return *this;
	}

	// Intersect with Another IndexRange
	inline
	StaticIndexRange &
	intersect( IndexRange const & I )
	{
		Super::intersect( I );
		assert( legal_static() );
		return *this;
	}

	// Clear
	inline
	StaticIndexRange &
	clear()
	{
		Super::clear();
		return *this;
	}

	// Swap
	inline
	StaticIndexRange &
	swap( StaticIndexRange & I )
	{
		if ( this != &I ) {
			Super::swap( I );
			assert( legal_static() );
		}
		return *this;
	}

}; // StaticIndexRange

// Types
typedef  StaticIndexRange  SRange;

// Functions

// Swap
inline
void
swap( StaticIndexRange & a, StaticIndexRange & b )
{
	a.swap( b );
}

} // ObjexxFCL

#ifndef NO_STD_SWAP_OVERLOADS

// std::swap Overloads for Efficiency
//
// Technically you cannot add functions overloads to namespace std
// but this works with most compilers and makes it much faster if someone uses
// std::swap instead of swap or ObjexxFCL::swap.

namespace std {

// std::swap( StaticIndexRange, StaticIndexRange )
inline
void
swap( ObjexxFCL::StaticIndexRange & a, ObjexxFCL::StaticIndexRange & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_StaticIndexRange_hh_INCLUDED
