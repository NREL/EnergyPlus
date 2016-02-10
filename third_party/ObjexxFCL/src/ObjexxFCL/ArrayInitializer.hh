#ifndef ObjexxFCL_ArrayInitializer_hh_INCLUDED
#define ObjexxFCL_ArrayInitializer_hh_INCLUDED

// ArrayInitializer: Array Initializer Class Template
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
#include <ObjexxFCL/ArrayInitializer.fwd.hh>
#include <ObjexxFCL/noexcept.hh>

// C++ Headers
#include <cassert>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// ArrayInitializer: Array Initializer Class Template
template< typename T >
class ArrayInitializer
{

public: // Types

	// STL style
	typedef  T  value_type;

	// C++ style
	typedef  T  Value;

private: // Types

	template< typename > friend class ArrayInitializer;

public: // Creation

	// Default Constructor
	ArrayInitializer() :
	 value_p_( nullptr )
	{}

	// Copy Constructor
	ArrayInitializer( ArrayInitializer const & a ) :
	 value_p_( a.value_p_ != nullptr ? new T( *a.value_p_ ) : nullptr )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	ArrayInitializer( ArrayInitializer< U > const & a ) :
	 value_p_( a.value_p_ != nullptr ? new T( *a.value_p_ ) : nullptr )
	{}

	// Move Constructor
	ArrayInitializer( ArrayInitializer && a ) NOEXCEPT :
	 value_p_( a.value_p_ )
	{
		a.value_p_ = nullptr;
	}

	// Value Copy Constructor
	explicit
	ArrayInitializer( T const & value ) :
	 value_p_( new T( value ) )
	{}

	// Value Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	ArrayInitializer( U const & value ) :
	 value_p_( new T( value ) )
	{}

	// Value Move Constructor
	explicit
	ArrayInitializer( T && value ) :
	 value_p_( new T( std::move( value ) ) )
	{}

	// Destructor
	~ArrayInitializer()
	{
		if ( value_p_ != nullptr ) delete value_p_;
	}

public: // Operators

	// Value
	T const &
	operator ()() const
	{
		assert( value_p_ != nullptr );
		return *value_p_;
	}

	// Value
	T &
	operator ()()
	{
		assert( value_p_ != nullptr );
		return *value_p_;
	}

public: // Assignment

	// Copy Assignment
	ArrayInitializer &
	operator =( ArrayInitializer const & a )
	{
		if ( this != &a ) {
			if ( value_p_ != nullptr ) delete value_p_;
			value_p_ = ( a.value_p_ != nullptr ? new T( *a.value_p_ ) : nullptr );
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	ArrayInitializer &
	operator =( ArrayInitializer< U > const & a )
	{
		if ( value_p_ != nullptr ) delete value_p_;
		value_p_ = ( a.value_p_ != nullptr ? new T( *a.value_p_ ) : nullptr );
		return *this;
	}

	// Move Assignment
	ArrayInitializer &
	operator =( ArrayInitializer && a ) NOEXCEPT
	{
		if ( value_p_ != nullptr ) delete value_p_;
		value_p_ = a.value_p_;
		a.value_p_ = nullptr;
		return *this;
	}

	// Value Copy Assignment
	ArrayInitializer &
	operator =( T const & value )
	{
		if ( value_p_ != nullptr ) delete value_p_;
		value_p_ = new T( value );
		return *this;
	}

	// Value Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	ArrayInitializer &
	operator =( U const & value )
	{
		if ( value_p_ != nullptr ) delete value_p_;
		value_p_ = new T( value );
		return *this;
	}

	// Value Move Assignment
	ArrayInitializer &
	operator =( T && value )
	{
		if ( value_p_ != nullptr ) delete value_p_;
		value_p_ = new T( std::move( value ) );
		return *this;
	}

public: // Predicate

	// Active?
	bool
	active() const
	{
		return value_p_ != nullptr;
	}

public: // Property

	// Value
	T const &
	value() const
	{
		assert( value_p_ != nullptr );
		return *value_p_;
	}

	// Value
	T &
	value()
	{
		assert( value_p_ != nullptr );
		return *value_p_;
	}

public: // Modifier

	// Clear
	void
	clear()
	{
		if ( value_p_ != nullptr ) delete value_p_;
		value_p_ = nullptr;
	}

	// Swap
	void
	swap( ArrayInitializer & o )
	{
		using std::swap;
		swap( value_p_, o.value_p_ );
	}

private: // Data

	T * value_p_; // Value pointer

}; // ArrayInitializer

// Swap
template< typename T >
inline
void
swap( ArrayInitializer< T > & a, ArrayInitializer< T > & b )
{
	a.swap( b );
}

} // ObjexxFCL

#endif // ObjexxFCL_ArrayInitializer_hh_INCLUDED
