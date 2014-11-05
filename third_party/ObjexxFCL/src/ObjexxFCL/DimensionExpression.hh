#ifndef ObjexxFCL_DimensionExpression_hh_INCLUDED
#define ObjexxFCL_DimensionExpression_hh_INCLUDED

// DimensionExpression: DimensionExpression Interface Class
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

// C++ Headers
#include <cassert>
#include <iosfwd>

namespace ObjexxFCL {

// Forward
class Dimension;
class Observer;

// Types
typedef  Observer  Subject;

// DimensionExpression: DimensionExpression Interface Class
class DimensionExpression
{

protected: // Creation

	// Default Constructor
	inline
	DimensionExpression()
	{}

	// Copy Constructor
	inline
	DimensionExpression( DimensionExpression const & )
	{}

public: // Creation

	// Clone
	virtual
	DimensionExpression *
	clone() const = 0;

	// Clone with Dimension Substitution
	virtual
	DimensionExpression *
	clone( Dimension const & ) const = 0;

	// Destructor
	inline
	virtual
	~DimensionExpression()
	{}

public: // Conversion

	// int Conversion
	inline
	operator int() const
	{
		assert( initialized() );
		return static_cast< int >( value() );
	}

	// double Conversion
	inline
	operator double() const
	{
		assert( initialized() );
		return value();
	}

private: // Assignment

	// Copy Assignment
	DimensionExpression &
	operator =( DimensionExpression const & ); // Unimplemented: Dimension handles assignment

public: // Inspector

	// Initialized?
	virtual
	bool
	initialized() const = 0;

	// Integer?
	virtual
	bool
	integer() const = 0;

	// Constant?
	virtual
	bool
	constant() const = 0;

	// Reference?
	virtual
	bool
	reference() const = 0;

	// Reducible?
	virtual
	bool
	reducible() const = 0;

	// Value
	virtual
	double
	operator ()() const = 0;

	// Value
	virtual
	double
	value() const = 0;

	// Integer Value
	inline
	virtual
	int
	ivalue() const
	{
		return static_cast< int >( value() );
	}

	// Integer Value: Zero if Uninitialized
	inline
	virtual
	int
	zvalue() const
	{
		return ( initialized() ? static_cast< int >( value() ) : 0 );
	}

	// Insert an Observer
	virtual
	void
	insert_observer( Observer & ) const = 0;

	// Remove an Observer
	virtual
	void
	remove_observer( Observer & ) const = 0;

public: // Modifier

	// Update for Destruction of a Subject
	virtual
	void
	destructed( Subject const & ) = 0;

}; // DimensionExpression

// Comparison

// DimensionExpression == DimensionExpression
inline
bool
operator ==( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return ( ( exp1.initialized() ) && ( exp2.initialized() ) && ( exp1.value() == exp2.value() ) );
}

// DimensionExpression != DimensionExpression
inline
bool
operator !=( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return !( exp1 == exp2 );
}

// DimensionExpression < DimensionExpression
inline
bool
operator <( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return ( ( exp1.initialized() ) && ( exp2.initialized() ) && ( exp1.value() < exp2.value() ) );
}

// DimensionExpression <= DimensionExpression
inline
bool
operator <=( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return ( ( exp1.initialized() ) && ( exp2.initialized() ) && ( exp1.value() <= exp2.value() ) );
}

// DimensionExpression > DimensionExpression
inline
bool
operator >( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return ( ( exp1.initialized() ) && ( exp2.initialized() ) && ( exp1.value() > exp2.value() ) );
}

// DimensionExpression >= DimensionExpression
inline
bool
operator >=( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return ( ( exp1.initialized() ) && ( exp2.initialized() ) && ( exp1.value() >= exp2.value() ) );
}

// int == DimensionExpression
inline
bool
operator ==( int const i, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( i == exp.value() ) );
}

// int != DimensionExpression
inline
bool
operator !=( int const i, DimensionExpression const & exp )
{
	return !( i == exp );
}

// int < DimensionExpression
inline
bool
operator <( int const i, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( i < exp.value() ) );
}

// int <= DimensionExpression
inline
bool
operator <=( int const i, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( i <= exp.value() ) );
}

// int > DimensionExpression
inline
bool
operator >( int const i, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( i > exp.value() ) );
}

// int >= DimensionExpression
inline
bool
operator >=( int const i, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( i >= exp.value() ) );
}

// DimensionExpression == int
inline
bool
operator ==( DimensionExpression const & exp, int const i )
{
	return ( ( exp.initialized() ) && ( exp.value() == i ) );
}

// DimensionExpression != int
inline
bool
operator !=( DimensionExpression const & exp, int const i )
{
	return !( exp == i );
}

// DimensionExpression < int
inline
bool
operator <( DimensionExpression const & exp, int const i )
{
	return ( ( exp.initialized() ) && ( exp.value() < i ) );
}

// DimensionExpression <= int
inline
bool
operator <=( DimensionExpression const & exp, int const i )
{
	return ( ( exp.initialized() ) && ( exp.value() <= i ) );
}

// DimensionExpression > int
inline
bool
operator >( DimensionExpression const & exp, int const i )
{
	return ( ( exp.initialized() ) && ( exp.value() > i ) );
}

// DimensionExpression >= int
inline
bool
operator >=( DimensionExpression const & exp, int const i )
{
	return ( ( exp.initialized() ) && ( exp.value() >= i ) );
}

// double == DimensionExpression
inline
bool
operator ==( double const d, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( d == exp.value() ) );
}

// double != DimensionExpression
inline
bool
operator !=( double const d, DimensionExpression const & exp )
{
	return !( d == exp );
}

// double < DimensionExpression
inline
bool
operator <( double const d, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( d < exp.value() ) );
}

// double <= DimensionExpression
inline
bool
operator <=( double const d, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( d <= exp.value() ) );
}

// double > DimensionExpression
inline
bool
operator >( double const d, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( d > exp.value() ) );
}

// double >= DimensionExpression
inline
bool
operator >=( double const d, DimensionExpression const & exp )
{
	return ( ( exp.initialized() ) && ( d >= exp.value() ) );
}

// DimensionExpression == double
inline
bool
operator ==( DimensionExpression const & exp, double const d )
{
	return ( ( exp.initialized() ) && ( exp.value() == d ) );
}

// DimensionExpression != double
inline
bool
operator !=( DimensionExpression const & exp, double const d )
{
	return !( exp == d );
}

// DimensionExpression < double
inline
bool
operator <( DimensionExpression const & exp, double const d )
{
	return ( ( exp.initialized() ) && ( exp.value() < d ) );
}

// DimensionExpression <= double
inline
bool
operator <=( DimensionExpression const & exp, double const d )
{
	return ( ( exp.initialized() ) && ( exp.value() <= d ) );
}

// DimensionExpression > double
inline
bool
operator >( DimensionExpression const & exp, double const d )
{
	return ( ( exp.initialized() ) && ( exp.value() > d ) );
}

// DimensionExpression >= double
inline
bool
operator >=( DimensionExpression const & exp, double const d )
{
	return ( ( exp.initialized() ) && ( exp.value() >= d ) );
}

// I/O

// Stream Output
std::ostream &
operator <<( std::ostream & stream, DimensionExpression const & exp );

} // ObjexxFCL

#endif // ObjexxFCL_DimensionExpression_hh_INCLUDED
