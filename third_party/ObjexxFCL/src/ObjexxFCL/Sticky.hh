#ifndef ObjexxFCL_Sticky_hh_INCLUDED
#define ObjexxFCL_Sticky_hh_INCLUDED

// Sticky Value Wrapper
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
#include <ObjexxFCL/Sticky.fwd.hh>
#include <ObjexxFCL/noexcept.hh>

// C++ Headers
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// Sticky: Sticky Argument Wrapper
template< typename T >
class Sticky
{

public: // Types

	// STL style
	typedef  T  value_type;

	// C++ style
	typedef  T  Value;

private: // Types

	template< typename > friend class Sticky;

public: // Creation

	// Default Constructor
	Sticky()
	{}

	// Copy Constructor
	Sticky( Sticky const & s ) :
	 value_( s.value_ )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Sticky( Sticky< U > const & s ) :
	 value_( s.value_ )
	{}

	// Move Constructor
	Sticky( Sticky && s ) NOEXCEPT :
	 value_( std::move( s.value_ ) )
	{}

	// Value Copy Constructor
	explicit
	Sticky( T const & value ) :
	 value_( value )
	{}

	// Value Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Sticky( U const & value ) :
	 value_( value )
	{}

	// Value Move Constructor
	Sticky( T && value ) NOEXCEPT :
	 value_( std::move( value ) )
	{}

public: // Conversion

	// Value Conversion
	operator T const &() const
	{
		return value_;
	}

	// Value Conversion
	operator T &()
	{
		return value_;
	}

public: // Operators

	// Value
	T const &
	operator ()() const
	{
		return value_;
	}

	// Value
	T &
	operator ()()
	{
		return value_;
	}

public: // Assignment

	// Copy Assignment
	Sticky &
	operator =( Sticky const & s )
	{
		if ( this != &s ) {
			value_ = s.value_;
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Sticky &
	operator =( Sticky< U > const & s )
	{
		value_ = s.value_;
		return *this;
	}

	// Move Assignment
	Sticky &
	operator =( Sticky && s ) NOEXCEPT
	{
		value_ = std::move( s.value_ );
		return *this;
	}

	// Value Copy Assignment
	Sticky &
	operator =( T const & value )
	{
		value_ = value;
		return *this;
	}

	// Value Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Sticky &
	operator =( U const & value )
	{
		value_ = value;
		return *this;
	}

	// Value Move Assignment
	Sticky &
	operator =( T && value )
	{
		value_ = std::move( value );
		return *this;
	}

public: // Property

	// Value
	T const &
	value() const
	{
		return value_;
	}

	// Value
	T &
	value()
	{
		return value_;
	}

public: // Modifier

	// Clear
	void
	clear()
	{
		value_ = T();
	}

	// Swap
	void
	swap( Sticky & o )
	{
		using std::swap;
		swap( value_, o.value_ );
	}

private: // Data

	T value_; // Object

}; // Sticky

// Swap
template< typename T >
inline
void
swap( Sticky< T > & a, Sticky< T > & b )
{
	a.swap( b );
}

// Makers

// Sticky Copy Maker
template< typename T >
inline
Sticky< T >
make_Sticky( T const & t )
{
	return Sticky< T >( t );
}

// Sticky Copy Maker
template< typename T >
inline
Sticky< T >
sticky( T const & t )
{
	return Sticky< T >( t );
}

// Sticky Move Maker
template< typename T >
inline
Sticky< T >
make_Sticky( T && t )
{
	return Sticky< T >( std::move( t ) );
}

// Sticky Move Maker
template< typename T >
inline
Sticky< T >
sticky( T && t )
{
	return Sticky< T >( std::move( t ) );
}

// Comparison

// Sticky == Sticky
template< typename T >
inline
bool
operator ==( Sticky< T > const & a, Sticky< T > const & b )
{
	return ( a() == b() );
}

// Sticky != Sticky
template< typename T >
inline
bool
operator !=( Sticky< T > const & a, Sticky< T > const & b )
{
	return ( a() != b() );
}

// Sticky == Value
template< typename T >
inline
bool
operator ==( Sticky< T > const & a, T const & b )
{
	return ( a() == b );
}

// Sticky != Value
template< typename T >
inline
bool
operator !=( Sticky< T > const & a, T const & b )
{
	return ( a() != b );
}

// Value == Sticky
template< typename T >
inline
bool
operator ==( T const & a, Sticky< T > const & b )
{
	return ( a == b() );
}

// Value != Sticky
template< typename T >
inline
bool
operator !=( T const & a, Sticky< T > const & b )
{
	return ( a != b() );
}

} // ObjexxFCL

#endif // ObjexxFCL_Sticky_hh_INCLUDED
