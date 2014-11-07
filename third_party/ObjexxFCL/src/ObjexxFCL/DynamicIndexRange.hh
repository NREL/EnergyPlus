#ifndef ObjexxFCL_DynamicIndexRange_hh_INCLUDED
#define ObjexxFCL_DynamicIndexRange_hh_INCLUDED

// DynamicIndexRange: Dynamic Index Range
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
#include <ObjexxFCL/ObserverSingle.hh>
#include <ObjexxFCL/Dimension.hh>

// C++ Headers
#include <algorithm>

namespace ObjexxFCL {

// DynamicIndexRange: Dynamic Index Range
//
// Note:
//  Initialized unless an active Dimension is uninitialized
//  Uninitialized range has ( size == 0 ) and its range values should not be accessed
//  Zero-size range is indicated by ( l - 1 == u ) and ( size == 0 )
//  Upper-unbounded range is indicated by ( l - 2 == u ) and ( size == npos )
//  Legal ranges have ( l - 2 <= u ) with l and u in their allowed ranges
class DynamicIndexRange : public IndexRange, public ObserverSingle // DynamicIndexRange only observed at most by the FArray that contains it
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
	DynamicIndexRange() :
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{}

	// Copy Constructor
	inline
	DynamicIndexRange( DynamicIndexRange const & I ) :
	 Super( I ),
	 ObserverSingle( I ),
	 l_dim_p_( I.l_dim_clone() ),
	 u_dim_p_( I.u_dim_clone() )
	{
		assert( legal_dynamic() );
		insert_as_observer();
	}

	// IndexRange Constructor
	inline
	DynamicIndexRange( IndexRange const & I ) :
	 Super( I ),
	 l_dim_p_( I.l_dim_clone() ),
	 u_dim_p_( I.u_dim_clone() )
	{
		assert( legal_dynamic() );
		insert_as_observer();
	}

	// Upper Index Constructor
	inline
	DynamicIndexRange( int const u ) :
	 Super( u ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Star Constructor
	inline
	DynamicIndexRange( Star const s ) :
	 Super( s ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{}

	// Upper Dimension Constructor
	inline
	DynamicIndexRange( Dimension const & u_dim ) :
	 Super( u_dim.zvalue() ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( u_dim.reference_copy() )
	{
		assert( legal_dynamic() );
		size_dynamic();
		u_insert_as_observer();
	}

	// Upper Expression Constructor
	inline
	DynamicIndexRange( Expression const & u_exp ) :
	 Super( u_exp.zvalue() ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( new Dimension( u_exp ) )
	{
		assert( legal_dynamic() );
		size_dynamic();
		u_insert_as_observer();
	}

	// Index Range Constructor
	inline
	DynamicIndexRange( int const l, int const u ) :
	 Super( l, u ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Lower Index + Star Constructor
	inline
	DynamicIndexRange( int const l, Star const s ) :
	 Super( l, s ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Star + Upper Index Constructor
	inline
	DynamicIndexRange( Star const s, int const u ) :
	 Super( s, u ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Initializer List of Integer Constructor
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	DynamicIndexRange( std::initializer_list< U > const lu ) :
	 Super( lu ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Initializer List of Index Constructor
	inline
	DynamicIndexRange( std::initializer_list< Index > const lu ) :
	 Super( lu ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Omit Constructor
	inline
	DynamicIndexRange( Omit const o ) :
	 Super( o ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Lower Index + Omit Constructor
	inline
	DynamicIndexRange( int const l, Omit const o ) :
	 Super( l, o ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Omit + Upper Index Constructor
	inline
	DynamicIndexRange( Omit const o, int const u ) :
	 Super( o, u ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Omit + Omit Constructor
	inline
	DynamicIndexRange( Omit const ol, Omit const ou ) :
	 Super( ol, ou ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( nullptr )
	{
		assert( legal_static() );
	}

	// Dimension Range Constructor
	inline
	DynamicIndexRange( Dimension const & l_dim, Dimension const & u_dim ) :
	 Super( l_dim.zvalue(), u_dim.zvalue() ),
	 l_dim_p_( l_dim.reference_copy() ),
	 u_dim_p_( u_dim.reference_copy() )
	{
		assert( legal_dynamic() );
		size_dynamic();
		insert_as_observer();
	}

	// Expression Range Constructor
	inline
	DynamicIndexRange( Expression const & l_exp, Expression const & u_exp ) :
	 Super( l_exp.zvalue(), u_exp.zvalue() ),
	 l_dim_p_( new Dimension( l_exp ) ),
	 u_dim_p_( new Dimension( u_exp ) )
	{
		assert( legal_dynamic() );
		size_dynamic();
		insert_as_observer();
	}

	// Index and Dimension Constructor
	inline
	DynamicIndexRange( int const l, Dimension const & u_dim ) :
	 Super( l, u_dim.zvalue() ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( u_dim.reference_copy() )
	{
		assert( legal_dynamic() );
		size_dynamic();
		u_insert_as_observer();
	}

	// Dimension and Index Constructor
	inline
	DynamicIndexRange( Dimension const & l_dim, int const u ) :
	 Super( l_dim.zvalue(), u ),
	 l_dim_p_( l_dim.reference_copy() ),
	 u_dim_p_( nullptr )
	{
		assert( legal_dynamic() );
		size_dynamic();
		l_insert_as_observer();
	}

	// Index and Expression Constructor
	inline
	DynamicIndexRange( int const l, Expression const & u_exp ) :
	 Super( l, u_exp.zvalue() ),
	 l_dim_p_( nullptr ),
	 u_dim_p_( new Dimension( u_exp ) )
	{
		assert( legal_dynamic() );
		size_dynamic();
		u_insert_as_observer();
	}

	// Expression and Index Constructor
	inline
	DynamicIndexRange( Expression const & l_exp, int const u ) :
	 Super( l_exp.zvalue(), u ),
	 l_dim_p_( new Dimension( l_exp ) ),
	 u_dim_p_( nullptr )
	{
		assert( legal_dynamic() );
		size_dynamic();
		l_insert_as_observer();
	}

	// Dimension and Expression Constructor
	inline
	DynamicIndexRange( Dimension const & l_dim, Expression const & u_exp ) :
	 Super( l_dim.zvalue(), u_exp.zvalue() ),
	 l_dim_p_( l_dim.reference_copy() ),
	 u_dim_p_( new Dimension( u_exp ) )
	{
		assert( legal_dynamic() );
		size_dynamic();
		insert_as_observer();
	}

	// Expression and Dimension Constructor
	inline
	DynamicIndexRange( Expression const & l_exp, Dimension const & u_dim ) :
	 Super( l_exp.zvalue(), u_dim.zvalue() ),
	 l_dim_p_( new Dimension( l_exp ) ),
	 u_dim_p_( u_dim.reference_copy() )
	{
		assert( legal_dynamic() );
		size_dynamic();
		insert_as_observer();
	}

	// Dimension and Unbounded Upper Index Constructor
	DynamicIndexRange( Dimension const & l_dim, Star const s );

	// Expression and Unbounded Upper Index Constructor
	DynamicIndexRange( Expression const & l_exp, Star const s );

	// Destructor
	inline
	virtual
	~DynamicIndexRange()
	{
		delete l_dim_p_;
		delete u_dim_p_;
	}

public: // Assignment

	// Copy Assignment
	inline
	DynamicIndexRange &
	operator =( DynamicIndexRange const & I )
	{
		if ( this != &I ) {
			delete l_dim_p_; l_dim_p_ = I.l_dim_clone(); l_insert_as_observer();
			delete u_dim_p_; u_dim_p_ = I.u_dim_clone(); u_insert_as_observer();
			Super::operator =( I );
			assert( legal_dynamic() );
		}
		notify();
		return *this;
	}

	// IndexRange Assignment
	inline
	DynamicIndexRange &
	operator =( IndexRange const & I )
	{
		if ( this != &I ) {
			delete l_dim_p_; l_dim_p_ = I.l_dim_clone(); l_insert_as_observer();
			delete u_dim_p_; u_dim_p_ = I.u_dim_clone(); u_insert_as_observer();
			Super::operator =( I );
			assert( legal_dynamic() );
		}
		notify();
		return *this;
	}

	// Upper Index Assignment
	inline
	DynamicIndexRange &
	operator =( int const u )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::operator =( u );
		assert( legal_static() );
		notify();
		return *this;
	}

	// Unbounded Upper Index Assignment
	inline
	DynamicIndexRange &
	operator =( Star const s )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::operator =( s );
		notify();
		return *this;
	}

	// Upper Dimension Assignment
	inline
	DynamicIndexRange &
	operator =( Dimension const & u_dim )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = u_dim.reference_copy(); u_insert_as_observer();
		Super::operator =( u_dim.zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Upper Expression Assignment
	inline
	DynamicIndexRange &
	operator =( Expression const & u_exp )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = new Dimension( u_exp ); u_insert_as_observer();
		Super::operator =( u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Initializer List of int Assignment
	template< typename U, class = typename std::enable_if< std::is_assignable< int&, U >::value >::type >
	inline
	DynamicIndexRange &
	operator =( std::initializer_list< U > const lu )
	{
		Super::operator =( lu );
		assert( legal_static() );
		return *this;
	}

	// Initializer List of Index Assignment
	inline
	DynamicIndexRange &
	operator =( std::initializer_list< Index > const lu )
	{
		Super::operator =( lu );
		assert( legal_static() );
		return *this;
	}

	// DynamicIndexRange Assignment
	inline
	DynamicIndexRange &
	assign( DynamicIndexRange const & I )
	{
		if ( this != &I ) {
			delete l_dim_p_; l_dim_p_ = I.l_dim_clone(); l_insert_as_observer();
			delete u_dim_p_; u_dim_p_ = I.u_dim_clone(); u_insert_as_observer();
			Super::operator =( I );
			assert( legal_dynamic() );
		}
		notify();
		return *this;
	}

	// IndexRange Assignment
	inline
	DynamicIndexRange &
	assign( IndexRange const & I )
	{
		if ( this != &I ) {
			delete l_dim_p_; l_dim_p_ = I.l_dim_clone(); l_insert_as_observer();
			delete u_dim_p_; u_dim_p_ = I.u_dim_clone(); u_insert_as_observer();
			Super::operator =( I );
			assert( legal_dynamic() );
		}
		notify();
		return *this;
	}

	// Upper Index Assignment
	inline
	DynamicIndexRange &
	assign( int const u )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::operator =( u );
		assert( legal_static() );
		notify();
		return *this;
	}

	// Unbounded Upper Index Assignment
	inline
	DynamicIndexRange &
	assign( Star const s )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::operator =( s );
		notify();
		return *this;
	}

	// Upper Dimension Assignment
	inline
	DynamicIndexRange &
	assign( Dimension const & u_dim )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = u_dim.reference_copy(); u_insert_as_observer();
		Super::operator =( u_dim.zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Upper Expression Assignment
	inline
	DynamicIndexRange &
	assign( Expression const & u_exp )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = new Dimension( u_exp ); u_insert_as_observer();
		Super::operator =( u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Index Range Assignment
	inline
	DynamicIndexRange &
	assign( int const l, int const u )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::assign( l, u );
		assert( legal_static() );
		notify();
		return *this;
	}

	// Dimension Range Assignment
	inline
	DynamicIndexRange &
	assign( Dimension const & l_dim, Dimension const & u_dim )
	{
		delete l_dim_p_; l_dim_p_ = l_dim.reference_copy(); l_insert_as_observer();
		delete u_dim_p_; u_dim_p_ = u_dim.reference_copy(); u_insert_as_observer();
		Super::assign( l_dim.zvalue(), u_dim.zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Expression Range Assignment
	inline
	DynamicIndexRange &
	assign( Expression const & l_exp, Expression const & u_exp )
	{
		delete l_dim_p_; l_dim_p_ = new Dimension( l_exp ); l_insert_as_observer();
		delete u_dim_p_; u_dim_p_ = new Dimension( u_exp ); u_insert_as_observer();
		Super::assign( l_dim_p_->zvalue(), u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Index and Dimension Assignment
	inline
	DynamicIndexRange &
	assign( int const l, Dimension const & u_dim )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = u_dim.reference_copy(); u_insert_as_observer();
		Super::assign( l, u_dim.zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Dimension and Index Assignment
	inline
	DynamicIndexRange &
	assign( Dimension const & l_dim, int const u )
	{
		delete l_dim_p_; l_dim_p_ = l_dim.reference_copy(); l_insert_as_observer();
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::assign( l_dim.zvalue(), u );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Index and Expression Assignment
	inline
	DynamicIndexRange &
	assign( int const l, Expression const & u_exp )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = new Dimension( u_exp ); u_insert_as_observer();
		Super::assign( l, u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Expression and Index Assignment
	inline
	DynamicIndexRange &
	assign( Expression const & l_exp, int const u )
	{
		delete l_dim_p_; l_dim_p_ = new Dimension( l_exp ); l_insert_as_observer();
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::assign( l_dim_p_->zvalue(), u );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Dimension and Expression Assignment
	inline
	DynamicIndexRange &
	assign( Dimension const & l_dim, Expression const & u_exp )
	{
		delete l_dim_p_; l_dim_p_ = l_dim.reference_copy(); l_insert_as_observer();
		delete u_dim_p_; u_dim_p_ = new Dimension( u_exp ); u_insert_as_observer();
		Super::assign( l_dim.zvalue(), u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Expression and Dimension Assignment
	inline
	DynamicIndexRange &
	assign( Expression const & l_exp, Dimension const & u_dim )
	{
		delete l_dim_p_; l_dim_p_ = new Dimension( l_exp ); l_insert_as_observer();
		delete u_dim_p_; u_dim_p_ = u_dim.reference_copy(); u_insert_as_observer();
		Super::assign( l_dim_p_->zvalue(), u_dim.zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Index and Unbounded Upper Index Assignment
	inline
	DynamicIndexRange &
	assign( int const l, Star const s )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::assign( l, s );
		assert( legal_static() );
		notify();
		return *this;
	}

	// Dimension and Unbounded Upper Index Assignment
	DynamicIndexRange &
	assign( Dimension const & l_dim, Star const s );

	// Expression and Unbounded Upper Index Assignment
	DynamicIndexRange &
	assign( Expression const & l_exp, Star const s );

	// DynamicIndexRange Assignment without Notification
	inline
	DynamicIndexRange &
	assign_no_notify( DynamicIndexRange const & I )
	{
		if ( this != &I ) {
			delete l_dim_p_; l_dim_p_ = I.l_dim_clone(); l_insert_as_observer();
			delete u_dim_p_; u_dim_p_ = I.u_dim_clone(); u_insert_as_observer();
			Super::operator =( I );
			assert( legal_dynamic() );
		}
		return *this;
	}

	// IndexRange Assignment without Notification
	inline
	DynamicIndexRange &
	assign_no_notify( IndexRange const & I )
	{
		if ( this != &I ) {
			delete l_dim_p_; l_dim_p_ = I.l_dim_clone(); l_insert_as_observer();
			delete u_dim_p_; u_dim_p_ = I.u_dim_clone(); u_insert_as_observer();
			Super::operator =( I );
			assert( legal_dynamic() );
		}
		return *this;
	}

	// Index and Unbounded Upper Index Assignment without Notification
	inline
	DynamicIndexRange &
	assign_no_notify( int const l, Star const s )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::assign( l, s );
		assert( legal_static() );
		return *this;
	}

public: // Predicate

	// Initialized?
	inline
	bool
	initialized() const
	{
		return ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) );
	}

	// Lower Initialized?
	inline
	bool
	l_initialized() const
	{
		return ( l_dim_p_ ? l_dim_p_->initialized_ : true );
	}

	// Upper Initialized?
	inline
	bool
	u_initialized() const
	{
		return ( u_dim_p_ ? u_dim_p_->initialized_ : true );
	}

	// Legal?
	inline
	bool
	legal() const
	{
		return ( ( ( l_ >= l_min ) && ( u_ <= u_max ) && ( l_ - 2 <= u_ ) ) ||
		 ( ! ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) ) );
	}

	// Bounded?
	inline
	bool
	bounded() const
	{
		return ( ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) && ( Super::bounded() ) );
	}

	// Unbounded?
	inline
	bool
	unbounded() const
	{
		return ( ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) && ( Super::unbounded() ) );
	}

	// Not Unbounded?
	inline
	bool
	not_unbounded() const
	{
		return ( ( ! ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) ) || ( Super::not_unbounded() ) );
	}

	// Bounded with Positive Size?
	inline
	bool
	positive() const
	{
		return ( ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) && ( Super::positive() ) );
	}

	// Contains an Index?
	inline
	bool
	contains( int const i ) const
	{
		return ( ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) && ( ( l_ <= i ) && ( ( i <= u_ ) || ( size_ == npos ) ) ) );
	}

	// Contains Another IndexRange?
	inline
	bool
	contains( IndexRange const & I ) const
	{
		return ( ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) && ( Super::contains( I ) ) );
	}

	// Intersects Another IndexRange?
	inline
	bool
	intersects( IndexRange const & I ) const
	{
		return ( ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) && ( Super::intersects( I ) ) );
	}

public: // Modifier

	// Lower Index Set
	inline
	DynamicIndexRange &
	l( int const l )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		Super::l( l );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Lower Dimension Set
	inline
	DynamicIndexRange &
	l( Dimension const & l_dim )
	{
		delete l_dim_p_; l_dim_p_ = l_dim.reference_copy(); l_insert_as_observer();
		Super::l( l_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Lower Expression Set
	inline
	DynamicIndexRange &
	l( Expression const & l_exp )
	{
		delete l_dim_p_; l_dim_p_ = new Dimension( l_exp ); l_insert_as_observer();
		Super::l( l_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Lower Index Set without Notification
	inline
	DynamicIndexRange &
	l_no_notify( int const l )
	{
		delete l_dim_p_; l_dim_p_ = nullptr;
		Super::l( l );
		assert( legal_dynamic() );
		size_dynamic();
		return *this;
	}

	// Lower Dimension Set without Notification
	inline
	DynamicIndexRange &
	l_no_notify( Dimension const & l_dim )
	{
		delete l_dim_p_; l_dim_p_ = l_dim.reference_copy(); l_insert_as_observer();
		Super::l( l_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		return *this;
	}

	// Lower Expression Set without Notification
	inline
	DynamicIndexRange &
	l_no_notify( Expression const & l_exp )
	{
		delete l_dim_p_; l_dim_p_ = new Dimension( l_exp ); l_insert_as_observer();
		Super::l( l_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		return *this;
	}

	// Upper Index Set
	inline
	DynamicIndexRange &
	u( int const u )
	{
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::u( u );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Unbounded Upper Index Set
	DynamicIndexRange &
	u( Star const s );

	// Upper Dimension Set
	inline
	DynamicIndexRange &
	u( Dimension const & u_dim )
	{
		delete u_dim_p_; u_dim_p_ = u_dim.reference_copy(); u_insert_as_observer();
		Super::u( u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Upper Expression Set
	inline
	DynamicIndexRange &
	u( Expression const & u_exp )
	{
		delete u_dim_p_; u_dim_p_ = new Dimension( u_exp ); u_insert_as_observer();
		Super::u( u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		notify();
		return *this;
	}

	// Upper Index Set without Notification
	inline
	DynamicIndexRange &
	u_no_notify( int const u )
	{
		delete u_dim_p_; u_dim_p_ = nullptr;
		Super::u( u );
		assert( legal_dynamic() );
		size_dynamic();
		return *this;
	}

	// Unbounded Upper Index Set without Notification
	DynamicIndexRange &
	u_no_notify( Star const s );

	// Upper Dimension Set without Notification
	inline
	DynamicIndexRange &
	u_no_notify( Dimension const & u_dim )
	{
		delete u_dim_p_; u_dim_p_ = u_dim.reference_copy(); u_insert_as_observer();
		Super::u( u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		return *this;
	}

	// Upper Expression Set without Notification
	inline
	DynamicIndexRange &
	u_no_notify( Expression const & u_exp )
	{
		delete u_dim_p_; u_dim_p_ = new Dimension( u_exp ); u_insert_as_observer();
		Super::u( u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
		return *this;
	}

	// Expand to Contain an Index
	DynamicIndexRange &
	contain( int const i );

	// Expand to Contain an Index and Notify If Changed
	DynamicIndexRange &
	contain_nic( int const i );

	// Expand to Contain Another IndexRange
	DynamicIndexRange &
	contain( IndexRange const & I );

	// Expand to Contain Another IndexRange and Notify If Changed
	DynamicIndexRange &
	contain_nic( IndexRange const & I );

	// Intersect with Another IndexRange
	DynamicIndexRange &
	intersect( IndexRange const & I );

	// Intersect with Another IndexRange and Notify If Changed
	DynamicIndexRange &
	intersect_nic( IndexRange const & I );

	// Clear
	inline
	DynamicIndexRange &
	clear()
	{
		Super::clear();
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		notify();
		return *this;
	}

	// Clear without Notification
	inline
	DynamicIndexRange &
	clear_no_notify()
	{
		Super::clear();
		delete l_dim_p_; l_dim_p_ = nullptr;
		delete u_dim_p_; u_dim_p_ = nullptr;
		return *this;
	}

	// Swap
	inline
	DynamicIndexRange &
	swap( DynamicIndexRange & I )
	{
		if ( this != &I ) {
			remove_as_observer();
			I.remove_as_observer();
			std::swap( l_dim_p_, I.l_dim_p_ );
			std::swap( u_dim_p_, I.u_dim_p_ );
			insert_as_observer();
			I.insert_as_observer();
			Super::swap( I );
			assert( legal_dynamic() );
			notify();
		}
		return *this;
	}

	// Swap without Notification
	inline
	DynamicIndexRange &
	swap_no_notify( DynamicIndexRange & I )
	{
		if ( this != &I ) {
			remove_as_observer();
			I.remove_as_observer();
			std::swap( l_dim_p_, I.l_dim_p_ );
			std::swap( u_dim_p_, I.u_dim_p_ );
			insert_as_observer();
			I.insert_as_observer();
			Super::swap( I );
			assert( legal_dynamic() );
		}
		return *this;
	}

public: // Observer Modifier

	// Update
	inline
	void
	update()
	{
		if ( l_dim_p_ ) Super::l( l_dim_p_->zvalue() );
		if ( u_dim_p_ ) Super::u( u_dim_p_->zvalue() );
		assert( legal_dynamic() );
		size_dynamic();
	}

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & )
	{}

private: // Functions

	// Legal DynamicIndexRange?
	inline
	bool
	legal_dynamic() const
	{
		return ( ( ( l_ >= l_min ) && ( u_ <= u_max ) && ( l_ - 2 <= u_ ) ) ||
		 ( ! ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) ) );
	}

	// Set Size to Zero if Uninitialized
	inline
	void
	size_dynamic()
	{
		if ( ! ( ( l_dim_p_ ? l_dim_p_->initialized_ : true ) && ( u_dim_p_ ? u_dim_p_->initialized_ : true ) ) ) size_ = 0u;
	}

	// Lower Dimension Clone
	inline
	Dimension *
	l_dim_clone() const
	{
		return ( l_dim_p_ ? l_dim_p_->clone() : static_cast< Dimension * >( nullptr ) );
	}

	// Upper Dimension Clone
	inline
	Dimension *
	u_dim_clone() const
	{
		return ( u_dim_p_ ? u_dim_p_->clone() : static_cast< Dimension * >( nullptr ) );
	}

	// Insert as Observer of the Dimensions
	inline
	void
	insert_as_observer()
	{
		if ( l_dim_p_ ) l_dim_p_->insert_observer( *this );
		if ( u_dim_p_ ) u_dim_p_->insert_observer( *this );
	}

	// Insert as Observer of the Lower Dimension
	inline
	void
	l_insert_as_observer()
	{
		if ( l_dim_p_ ) l_dim_p_->insert_observer( *this );
	}

	// Insert as Observer of the Upper Dimension
	inline
	void
	u_insert_as_observer()
	{
		if ( u_dim_p_ ) u_dim_p_->insert_observer( *this );
	}

	// Remove as Observer of the Dimensions
	inline
	void
	remove_as_observer()
	{
		if ( l_dim_p_ ) l_dim_p_->remove_observer( *this );
		if ( u_dim_p_ ) u_dim_p_->remove_observer( *this );
	}

private: // Data

	Dimension * l_dim_p_; // Lower Dimension pointer (0 iff no Dimension)
	Dimension * u_dim_p_; // Upper Dimension pointer (0 iff no Dimension)

}; // DynamicIndexRange

// Types
typedef  DynamicIndexRange  DRange;

// Functions

// Swap
inline
void
swap( DynamicIndexRange & a, DynamicIndexRange & b )
{
	a.swap( b );
}

// Swap without Notification
inline
void
swap_no_notify( DynamicIndexRange & a, DynamicIndexRange & b )
{
	a.swap_no_notify( b );
}

} // ObjexxFCL

#ifndef NO_STD_SWAP_OVERLOADS

// std::swap Overloads for Efficiency
//
// Technically you cannot add functions overloads to namespace std
// but this works with most compilers and makes it much faster if someone uses
// std::swap instead of swap or ObjexxFCL::swap.

namespace std {

// std::swap( DynamicIndexRange, DynamicIndexRange )
inline
void
swap( ObjexxFCL::DynamicIndexRange & a, ObjexxFCL::DynamicIndexRange & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_DynamicIndexRange_hh_INCLUDED
