#ifndef ObjexxFCL_Dimension_hh_INCLUDED
#define ObjexxFCL_Dimension_hh_INCLUDED

// Dimension: Dynamic Dimension
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
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DimensionExpression.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <iosfwd>
#include <utility>

namespace ObjexxFCL {

// Dimension: Dynamic Dimension
class Dimension : public ObserverMulti
{

private: // Friend

	friend class DynamicIndexRange;

public: // Types

	typedef  DimensionExpression  Expression;

public: // Creation

	// Default Constructor
	inline
	Dimension() :
	 exp_p_( nullptr ),
	 initialized_( false ),
	 value_( 0 )
	{}

	// Copy Constructor
	//  Copy creates a reference to the passed Dimension: Not intended for pass-by-value
	explicit
	Dimension( Dimension const & dim );

	// int Constructor
	explicit
	Dimension( int const i );

	// double Constructor
	explicit
	Dimension( double const d );

	// Expression Constructor
	inline
	explicit
	Dimension( Expression const & exp ) :
	 exp_p_( exp.clone() ),
	 initialized_( exp_p_->initialized() ),
	 value_( initialized_ ? exp_p_->ivalue() : 0 )
	{
		insert_as_observer();
	}

	// Expression Pointer Constructor (Ownership Transfer)
	inline
	explicit
	Dimension( Expression * exp_p ) :
	 exp_p_( exp_p ),
	 initialized_( exp_p_ ? exp_p_->initialized() : false ),
	 value_( initialized_ ? exp_p_->ivalue() : 0 )
	{
		reduce_expression();
		insert_as_observer();
	}

	// Clone
	inline
	Dimension *
	clone() const
	{
		return new Dimension( exp_clone() );
	}

	// Reference Copy
	inline
	Dimension *
	reference_copy() const
	{
		return new Dimension( *this );
	}

	// Destructor
	inline
	virtual
	~Dimension()
	{
		remove_as_observer();
		delete exp_p_;
	}

public: // Conversion

	// int Conversion
	inline
	operator int() const
	{
		assert( initialized_ );
		return value_;
	}

	// double Conversion
	inline
	operator double() const
	{
		assert( initialized_ );
		return static_cast< double >( value_ );
	}

public: // Assignment

	// Copy Assignment: Creates a reference to the passed Dimension
	Dimension &
	operator =( Dimension const & dim );

	// Expression Assignment
	inline
	Dimension &
	operator =( Expression const & exp )
	{
		assert( exp_p_ != &exp );
		remove_as_observer();
		delete exp_p_; exp_p_ = exp.clone( *this );
		insert_as_observer();
		update_notify();
		return *this;
	}

	// int Assignment
	Dimension &
	operator =( int const i );

	// double Assignment
	Dimension &
	operator =( double const d );

	// += Dimension
	Dimension &
	operator +=( Dimension const & dim );

	// += Expression
	Dimension &
	operator +=( Expression const & exp );

	// += int
	Dimension &
	operator +=( int const i );

	// += double
	Dimension &
	operator +=( double const d );

	// -= Dimension
	Dimension &
	operator -=( Dimension const & dim );

	// -= Expression
	Dimension &
	operator -=( Expression const & exp );

	// -= int
	Dimension &
	operator -=( int const i );

	// -= double
	Dimension &
	operator -=( double const d );

	// *= Dimension
	Dimension &
	operator *=( Dimension const & dim );

	// *= Expression
	Dimension &
	operator *=( Expression const & exp );

	// *= int
	Dimension &
	operator *=( int const i );

	// *= double
	Dimension &
	operator *=( double const d );

	// /= Dimension
	Dimension &
	operator /=( Dimension const & dim );

	// /= Expression
	Dimension &
	operator /=( Expression const & exp );

	// /= int
	Dimension &
	operator /=( int const i );

	// /= double
	Dimension &
	operator /=( double const d );

	// Dimension Value-Semantics Assignment
	inline
	Dimension &
	assign_value_of( Dimension const & dim )
	{
		if ( this != &dim ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = dim.exp_clone();
			insert_as_observer();
			update();
		}
		notify();
		return *this;
	}

	// int Assignment if Bigger than Value or Smaller than Multiplier * Value
	Dimension &
	assign_if( int const i, double const m = 1.0 );

	// double Assignment if Bigger than Value or Smaller than Multiplier * Value
	Dimension &
	assign_if( double const d, double const m = 1.0 );

	// int Assignment if Bigger than Value or Smaller than Half Value
	Dimension &
	assign_if_half( int const i );

	// double Assignment if Bigger than Value or Smaller than Half Value
	Dimension &
	assign_if_half( double const d );

	// int Assignment if Bigger than Value
	Dimension &
	assign_if_bigger( int const i );

	// double Assignment if Bigger than Value
	Dimension &
	assign_if_bigger( double const d );

	// int Assignment if Bigger than Value or Smaller than Multiplier * Value: Notify if Changed
	Dimension &
	assign_if_nic( int const i, double const m = 1.0 );

	// double Assignment if Bigger than Value or Smaller than Multiplier * Value: Notify if Changed
	Dimension &
	assign_if_nic( double const d, double const m = 1.0 );

	// int Assignment if Bigger than Value or Smaller than Half Value: Notify if Changed
	Dimension &
	assign_if_half_nic( int const i );

	// double Assignment if Bigger than Value or Smaller than Half Value: Notify if Changed
	Dimension &
	assign_if_half_nic( double const d );

	// int Assignment if Bigger than Value: Notify if Changed
	Dimension &
	assign_if_bigger_nic( int const i );

	// double Assignment if Bigger than Value: Notify if Changed
	Dimension &
	assign_if_bigger_nic( double const d );

public: // Incrememt/Decrement

	// ++Dimension
	Dimension &
	operator ++();

	// Dimension++
	Dimension const
	operator ++( int );

	// --Dimension
	Dimension &
	operator --();

	// Dimension--
	Dimension const
	operator --( int );

public: // Inspector

	// Initialized?
	inline
	bool
	initialized() const
	{
		return initialized_;
	}

	// Constant?
	inline
	bool
	constant() const
	{
		return ( exp_p_ ? exp_p_->constant() : false );
	}

	// Reference?
	inline
	bool
	reference() const
	{
		return ( exp_p_ ? exp_p_->reference() : false );
	}

	// Reducible?
	inline
	bool
	reducible() const
	{
		return ( exp_p_ ? exp_p_->reducible() : false );
	}

	// Value
	inline
	int
	operator ()() const
	{
		assert( initialized_ );
		return value_;
	}

	// Value
	inline
	int
	value() const
	{
		assert( initialized_ );
		return value_;
	}

	// Value: Zero if Uninitialized
	inline
	int
	zvalue() const
	{
		return value_;
	}

	// Expression Pointer
	inline
	Expression const *
	exp_p() const
	{
		return exp_p_;
	}

	// Expression
	inline
	Expression const &
	exp() const
	{
		assert( exp_p_ );
		return *exp_p_;
	}

	// Expression Clone
	inline
	Expression *
	exp_clone() const
	{
		return ( exp_p_ ? exp_p_->clone() : static_cast< Expression * >( 0 ) );
	}

public: // Modifier

	// Clear the Dimension
	inline
	Dimension &
	clear()
	{
		if ( exp_p_ ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = 0;
			initialized_ = false;
			value_ = 0;
		}
		notify();
		return *this;
	}

	// Clear the Dimension without Notification
	inline
	Dimension &
	clear_no_notify()
	{
		if ( exp_p_ ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = 0;
			initialized_ = false;
			value_ = 0;
		}
		return *this;
	}

	// Swap
	inline
	Dimension &
	swap( Dimension & dim )
	{
		if ( this != &dim ) {
			remove_as_observer();
			dim.remove_as_observer();
			std::swap( exp_p_, dim.exp_p_ );
			std::swap( initialized_, dim.initialized_ );
			std::swap( value_, dim.value_ );
			insert_as_observer();
			dim.insert_as_observer();
			notify();
		}
		return *this;
	}

	// Swap without Notification
	inline
	Dimension &
	swap_no_notify( Dimension & dim )
	{
		if ( this != &dim ) {
			remove_as_observer();
			dim.remove_as_observer();
			std::swap( exp_p_, dim.exp_p_ );
			std::swap( initialized_, dim.initialized_ );
			std::swap( value_, dim.value_ );
			insert_as_observer();
			dim.insert_as_observer();
		}
		return *this;
	}

public: // Observer Modifier

	// Update
	inline
	void
	update()
	{
		initialized_ = ( exp_p_ ? exp_p_->initialized() : false );
		value_ = ( initialized_ ? exp_p_->ivalue() : 0 );
	}

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & subject )
	{
		if ( exp_p_ ) exp_p_->destructed( subject );
	}

public: // Friend

	// Swap
	inline
	friend
	void
	swap( Dimension & a, Dimension & b )
	{
		a.swap( b );
	}

	// Swap
	inline
	friend
	void
	swap_no_notify( Dimension & a, Dimension & b )
	{
		a.swap_no_notify( b );
	}

private: // Functions

	// Reduce Expression
	inline
	void
	reduce_expression()
	{
		if ( reducible() ) {
			Expression * reduced_exp_p = exp_p_->clone();
			delete exp_p_; exp_p_ = reduced_exp_p;
		}
	}

	// Insert as Observer of an Expression's Referenced Dimensions
	inline
	void
	insert_as_observer_of( Dimension const & dim )
	{
		dim.insert_observer( *this );
	}

	// Insert as Observer of an Expression's Referenced Dimensions
	inline
	void
	insert_as_observer_of( Expression const & exp )
	{
		exp.insert_observer( *this );
	}

	// Insert as Observer of the Expression's Referenced Dimensions
	inline
	void
	insert_as_observer()
	{
		if ( exp_p_ ) exp_p_->insert_observer( *this );
	}

	// Remove as Observer of the Expression's Referenced Dimensions
	inline
	void
	remove_as_observer()
	{
		if ( exp_p_ ) exp_p_->remove_observer( *this );
	}

	// Update and Notify
	inline
	void
	update_notify()
	{
		initialized_ = ( exp_p_ ? exp_p_->initialized() : false );
		value_ = ( initialized_ ? exp_p_->ivalue() : 0 );
		notify();
	}

	// Update and Notify if External State Changed
	inline
	void
	update_notify_if_changed()
	{
		bool const now_initialized = ( exp_p_ ? exp_p_->initialized() : false );
		int const now_value = ( now_initialized ? exp_p_->ivalue() : 0 );
		if ( ( initialized_ != now_initialized ) || ( value_ != now_value ) ) {
			initialized_ = now_initialized;
			value_ = now_value;
			notify();
		}
	}

private: // Data

	Expression * exp_p_; // Expression pointer (owned)

	bool initialized_; // Cached initialization state

	int value_; // Cached value: Kept in synch with expression value (0 if uninitialized)

}; // Dimension

// Swap
void
swap( Dimension & a, Dimension & b );

// Swap
void
swap_no_notify( Dimension & a, Dimension & b );

// Dimension == Dimension
inline
bool
operator ==( Dimension const & dim1, Dimension const & dim2 )
{
	return ( ( dim1.initialized() ) && ( dim2.initialized() ) && ( dim1.value() == dim2.value() ) );
}

// Dimension != Dimension
inline
bool
operator !=( Dimension const & dim1, Dimension const & dim2 )
{
	return !( dim1 == dim2 );
}

// Dimension < Dimension
inline
bool
operator <( Dimension const & dim1, Dimension const & dim2 )
{
	return ( ( dim1.initialized() ) && ( dim2.initialized() ) && ( dim1.value() < dim2.value() ) );
}

// Dimension <= Dimension
inline
bool
operator <=( Dimension const & dim1, Dimension const & dim2 )
{
	return ( ( dim1.initialized() ) && ( dim2.initialized() ) && ( dim1.value() <= dim2.value() ) );
}

// Dimension > Dimension
inline
bool
operator >( Dimension const & dim1, Dimension const & dim2 )
{
	return ( ( dim1.initialized() ) && ( dim2.initialized() ) && ( dim1.value() > dim2.value() ) );
}

// Dimension >= Dimension
inline
bool
operator >=( Dimension const & dim1, Dimension const & dim2 )
{
	return ( ( dim1.initialized() ) && ( dim2.initialized() ) && ( dim1.value() >= dim2.value() ) );
}

// int == Dimension
inline
bool
operator ==( int const i, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( i == dim.value() ) );
}

// int != Dimension
inline
bool
operator !=( int const i, Dimension const & dim )
{
	return !( i == dim );
}

// int < Dimension
inline
bool
operator <( int const i, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( i < dim.value() ) );
}

// int <= Dimension
inline
bool
operator <=( int const i, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( i <= dim.value() ) );
}

// int > Dimension
inline
bool
operator >( int const i, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( i > dim.value() ) );
}

// int >= Dimension
inline
bool
operator >=( int const i, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( i >= dim.value() ) );
}

// Dimension == int
inline
bool
operator ==( Dimension const & dim, int const i )
{
	return ( ( dim.initialized() ) && ( dim.value() == i ) );
}

// Dimension != int
inline
bool
operator !=( Dimension const & dim, int const i )
{
	return !( dim == i );
}

// Dimension < int
inline
bool
operator <( Dimension const & dim, int const i )
{
	return ( ( dim.initialized() ) && ( dim.value() < i ) );
}

// Dimension <= int
inline
bool
operator <=( Dimension const & dim, int const i )
{
	return ( ( dim.initialized() ) && ( dim.value() <= i ) );
}

// Dimension > int
inline
bool
operator >( Dimension const & dim, int const i )
{
	return ( ( dim.initialized() ) && ( dim.value() > i ) );
}

// Dimension >= int
inline
bool
operator >=( Dimension const & dim, int const i )
{
	return ( ( dim.initialized() ) && ( dim.value() >= i ) );
}

// double == Dimension
inline
bool
operator ==( double const d, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( d == dim.value() ) );
}

// double != Dimension
inline
bool
operator !=( double const d, Dimension const & dim )
{
	return !( d == dim );
}

// double < Dimension
inline
bool
operator <( double const d, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( d < dim.value() ) );
}

// double <= Dimension
inline
bool
operator <=( double const d, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( d <= dim.value() ) );
}

// double > Dimension
inline
bool
operator >( double const d, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( d > dim.value() ) );
}

// double >= Dimension
inline
bool
operator >=( double const d, Dimension const & dim )
{
	return ( ( dim.initialized() ) && ( d >= dim.value() ) );
}

// Dimension == double
inline
bool
operator ==( Dimension const & dim, double const d )
{
	return ( ( dim.initialized() ) && ( dim.value() == d ) );
}

// Dimension != double
inline
bool
operator !=( Dimension const & dim, double const d )
{
	return !( dim == d );
}

// Dimension < double
inline
bool
operator <( Dimension const & dim, double const d )
{
	return ( ( dim.initialized() ) && ( dim.value() < d ) );
}

// Dimension <= double
inline
bool
operator <=( Dimension const & dim, double const d )
{
	return ( ( dim.initialized() ) && ( dim.value() <= d ) );
}

// Dimension > double
inline
bool
operator >( Dimension const & dim, double const d )
{
	return ( ( dim.initialized() ) && ( dim.value() > d ) );
}

// Dimension >= double
inline
bool
operator >=( Dimension const & dim, double const d )
{
	return ( ( dim.initialized() ) && ( dim.value() >= d ) );
}

// Stream Input
std::istream &
operator >>( std::istream & stream, Dimension & dim );

// Stream Output
std::ostream &
operator <<( std::ostream & stream, Dimension const & dim );

} // ObjexxFCL

#ifndef NO_STD_SWAP_OVERLOADS

// std::swap Overloads for Efficiency
//
// Technically you cannot add functions overloads to namespace std
// but this works with most compilers and makes it much faster if someone uses
// std::swap instead of swap or ObjexxFCL::swap.

namespace std {

// std::swap( Dimension, Dimension )
inline
void
swap( ObjexxFCL::Dimension & a, ObjexxFCL::Dimension & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_Dimension_hh_INCLUDED
