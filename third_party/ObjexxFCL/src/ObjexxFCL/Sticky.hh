#ifndef ObjexxFCL_Sticky_hh_INCLUDED
#define ObjexxFCL_Sticky_hh_INCLUDED

// Sticky Value Wrapper
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
#include <ObjexxFCL/Sticky.fwd.hh>

// C++ Headers
#include <cassert>
#include <type_traits>

namespace ObjexxFCL {

// Sticky Argument Wrapper
template< typename T >
class Sticky
{

public: // Types

	typedef  T  Value; // Type: Includes const attribute for const argument

public: // Creation

	// Default Constructor
	inline
	Sticky()
	{}

	// Value Constructor
	inline
	explicit
	Sticky( T const & val ) :
	 val_( val )
	{}

	// Value Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Sticky( U const & val ) :
	 val_( val )
	{}

public: // Assignment

	// Value Assignment
	inline
	Sticky &
	operator =( T const & val )
	{
		val_ = val;
		return *this;
	}

	// Value Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Sticky &
	operator =( U const & val )
	{
		val_ = val;
		return *this;
	}

public: // Conversion

	// Value Conversion
	inline
	operator T const &() const
	{
		return val_;
	}

	// Value Conversion
	inline
	operator T &()
	{
		return val_;
	}

public: // Operators

	// Value
	inline
	T const &
	operator ()() const
	{
		return val_;
	}

	// Value
	inline
	T &
	operator ()()
	{
		return val_;
	}

private: // Data

	T val_; // Object

}; // Sticky

// Functions

// Sticky Maker
template< typename T >
inline
Sticky< T >
make_Sticky( T const & t )
{
	return Sticky< T >( t );
}

// Sticky Maker
template< typename T >
inline
Sticky< T >
sticky( T const & t )
{
	return Sticky< T >( t );
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
