#ifndef ObjexxFCL_ArrayInitializer_hh_INCLUDED
#define ObjexxFCL_ArrayInitializer_hh_INCLUDED

// ArrayInitializer: Array Initializer Class Template
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Sticky.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <cassert>
#include <functional>

namespace ObjexxFCL {

// ArrayInitializer: Array Initializer Class Template
template< typename T, template< typename > class A >
class ArrayInitializer
{

public: // Types

	typedef  TypeTraits< T >  Traits;

	// STL style
	typedef  T  value_type;

	// C++ style
	typedef  T  Value;

	typedef  std::function< void( A< T > & ) >  Function;

private: // Types

	template< typename, template< typename > class > friend class ArrayInitializer;
	enum State { INACTIVE, VALUE, FUNCTION };

public: // Creation

	// Default Constructor
	ArrayInitializer() :
	 state_( INACTIVE ),
	 sticky_( false ),
	 value_( Traits::initial_value() )
	{}

	// Copy Constructor
	ArrayInitializer( ArrayInitializer const & a ) :
	 state_( a.state_ ),
	 sticky_( a.sticky_ ),
	 value_( a.value_ ),
	 function_( a.function_ )
	{}

	// Copy Constructor Template
	template< typename U, template< typename > class C >
	ArrayInitializer( ArrayInitializer< U, C > const & a ) :
	 state_( a.state_ == ArrayInitializer< U, C >::FUNCTION ? INACTIVE : ( a.state_ == ArrayInitializer< U, C >::VALUE ? VALUE : INACTIVE ) ),
	 sticky_( a.sticky_ ),
	 value_( a.value_ )
	{}

	// Value Constructor
	explicit
	ArrayInitializer( T const & value ) :
	 state_( VALUE ),
	 sticky_( false ),
	 value_( value )
	{}

	// Sticky Value Constructor
	explicit
	ArrayInitializer( Sticky< T > const & value ) :
	 state_( VALUE ),
	 sticky_( true ),
	 value_( value() )
	{}

	// Value Constructor Template
	template< typename U >
	explicit
	ArrayInitializer( U const & value ) :
	 state_( VALUE ),
	 sticky_( false ),
	 value_( value )
	{}

	// Sticky Value Constructor Template
	template< typename U >
	explicit
	ArrayInitializer( Sticky< U > const & value ) :
	 state_( VALUE ),
	 sticky_( true ),
	 value_( value() )
	{}

	// Function Constructor
	explicit
	ArrayInitializer( Function const & fxn ) :
	 state_( fxn ? FUNCTION : INACTIVE ),
	 sticky_( false ),
	 value_( Traits::initial_value() ),
	 function_( fxn ? fxn : Function() )
	{}

public: // Assignment

	// Copy Assignment
	ArrayInitializer &
	operator =( ArrayInitializer const & a )
	{
		if ( this != &a ) {
			state_ = a.state_;
			sticky_ = a.sticky_;
			value_ = a.value_;
			function_ = a.function_;
		}
		return *this;
	}

	// Value Assignment
	ArrayInitializer &
	operator =( T const & value )
	{
		state_ = VALUE;
		sticky_ = false;
		value_ = value;
		function_ = Function();
		return *this;
	}

	// Sticky Value Assignment
	ArrayInitializer &
	operator =( Sticky< T > const & value )
	{
		state_ = VALUE;
		sticky_ = true;
		value_ = value();
		function_ = Function();
		return *this;
	}

	// Function Assignment
	ArrayInitializer &
	operator =( Function const & fxn )
	{
		state_ = ( fxn ? FUNCTION : INACTIVE );
		sticky_ = false;
		value_ = Traits::initial_value();
		function_ = ( fxn ? fxn : Function() );
		return *this;
	}

public: // Inspector

	// Active?
	bool
	is_active() const
	{
		return ( state_ != INACTIVE );
	}

	// Value?
	bool
	is_value() const
	{
		return ( state_ == VALUE );
	}

	// Sticky?
	bool
	is_sticky() const
	{
		return sticky_;
	}

	// Sticky?
	bool
	sticky() const
	{
		return sticky_;
	}

	// Function?
	bool
	is_function() const
	{
		return ( state_ == FUNCTION );
	}

	// Value
	T const &
	value() const
	{
		assert( state_ == VALUE );
		return value_;
	}

	// Function
	Function const &
	function() const
	{
		assert( state_ == FUNCTION );
		return function_;
	}

public: // Modifier

	// Clear
	void
	clear()
	{
		state_ = INACTIVE;
		sticky_ = false;
		value_ = Traits::initial_value();
		function_ = Function();
	}

	// Clear Non-Sticky
	void
	clear_nonsticky()
	{
		if ( ! sticky_ ) {
			state_ = INACTIVE;
			value_ = Traits::initial_value();
			function_ = Function();
		}
	}

	// Swap
	void
	swap( ArrayInitializer & o )
	{
		using std::swap;
		swap( state_, o.state_ );
		swap( sticky_, o.sticky_ );
		swap( value_, o.value_ );
		swap( function_, o.function_ );
	}

private: // Data

	State state_; // State
	bool sticky_; // Sticky value?
	T value_; // Value
	Function function_; // Function

}; // ArrayInitializer

// Swap
template< typename T, template< typename > class A >
inline
void
swap( ArrayInitializer< T, A > & a, ArrayInitializer< T, A > & b )
{
	a.swap( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_ArrayInitializer_hh_INCLUDED
