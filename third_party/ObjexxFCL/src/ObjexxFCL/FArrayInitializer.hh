#ifndef ObjexxFCL_FArrayInitializer_hh_INCLUDED
#define ObjexxFCL_FArrayInitializer_hh_INCLUDED

// FArrayInitializer: FArray Initializer Class Template
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
#include <ObjexxFCL/Sticky.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <cassert>
#include <functional>

namespace ObjexxFCL {

// FArrayInitializer: FArray Initializer Class Template
template< typename T, template< typename > class Array >
class FArrayInitializer
{

public: // Types

	typedef  TypeTraits< T >  Traits;

	// STL style
	typedef  T  value_type;

	// C++ style
	typedef  T  Value;

	typedef  std::function< void( Array< T > & ) >  Function;

private: // Types

	enum State { INACTIVE, VALUE, FUNCTION };

public: // Creation

	// Default Constructor
	inline
	FArrayInitializer() :
	 state_( INACTIVE ),
	 sticky_( false ),
	 value_( Traits::initial_value() )
	{}

	// Value Constructor
	inline
	explicit
	FArrayInitializer( T const & value ) :
	 state_( VALUE ),
	 sticky_( false ),
	 value_( value )
	{}

	// Sticky Value Constructor
	inline
	explicit
	FArrayInitializer( Sticky< T > const & value ) :
	 state_( VALUE ),
	 sticky_( true ),
	 value_( value() )
	{}

	// Function Constructor
	inline
	explicit
	FArrayInitializer( Function const & fxn ) :
	 state_( fxn ? FUNCTION : INACTIVE ),
	 sticky_( false ),
	 value_( Traits::initial_value() ),
	 function_( fxn ? fxn : Function() )
	{}

public: // Assignment

	// Value Assignment
	inline
	FArrayInitializer &
	operator =( T const & value )
	{
		state_ = VALUE;
		// Don't alter stickyness
		value_ = value;
		function_ = Function();
		return *this;
	}

	// Sticky Value Assignment
	inline
	FArrayInitializer &
	operator =( Sticky< T > const & value )
	{
		state_ = VALUE;
		sticky_ = true;
		value_ = value();
		function_ = Function();
		return *this;
	}

	// Function Assignment
	inline
	FArrayInitializer &
	operator =( Function const & fxn )
	{
		state_ = ( fxn ? FUNCTION : INACTIVE );
		value_ = Traits::initial_value();
		function_ = ( fxn ? fxn : Function() );
		return *this;
	}

public: // Inspector

	// Active?
	inline
	bool
	is_active() const
	{
		return ( state_ != INACTIVE );
	}

	// Value?
	inline
	bool
	is_value() const
	{
		return ( state_ == VALUE );
	}

	// Sticky?
	inline
	bool
	is_sticky() const
	{
		return sticky_;
	}

	// Function?
	inline
	bool
	is_function() const
	{
		return ( state_ == FUNCTION );
	}

	// Value
	inline
	T const &
	value() const
	{
		assert( state_ == VALUE );
		return value_;
	}

	// Function
	inline
	Function const &
	function() const
	{
		assert( state_ == FUNCTION );
		return function_;
	}

public: // Modifier

	// Clear
	inline
	void
	clear()
	{
		state_ = INACTIVE;
		sticky_ = false;
		value_ = Traits::initial_value();
		function_ = Function();
	}

	// Clear Non-Sticky
	inline
	void
	clear_nonsticky()
	{
		if ( ! sticky_ ) {
			state_ = INACTIVE;
			value_ = Traits::initial_value();
			function_ = Function();
		}
	}

private: // Data

	State state_; // State
	bool sticky_; // Sticky?
	T value_; // Value
	Function function_; // Function

}; // FArrayInitializer

} // ObjexxFCL

#endif // ObjexxFCL_FArrayInitializer_hh_INCLUDED
