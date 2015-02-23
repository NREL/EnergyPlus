#ifndef ObjexxFCL_DimensionExpressionCon_hh_INCLUDED
#define ObjexxFCL_DimensionExpressionCon_hh_INCLUDED

// DimensionExpressionCon: Constant-Valued DimensionExpression
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
#include <ObjexxFCL/DimensionExpression.hh>

namespace ObjexxFCL {

// DimensionExpressionCon: Constant-Valued DimensionExpression
class DimensionExpressionCon : public DimensionExpression
{

private: // Types

	typedef  DimensionExpression  Super;

public: // Creation

	// Copy Constructor
	inline
	DimensionExpressionCon( DimensionExpressionCon const & exp ) :
	 Super(),
	 value_( exp.value_ ),
	 integer_( exp.integer_ )
	{}

	// int Constructor
	inline
	explicit
	DimensionExpressionCon( int const value ) :
	 value_( static_cast< double >( value ) ),
	 integer_( true )
	{}

	// double Constructor
	inline
	explicit
	DimensionExpressionCon( double const value ) :
	 value_( value ),
	 integer_( false )
	{}

	// Clone
	inline
	DimensionExpressionCon *
	clone() const
	{
		return new DimensionExpressionCon( *this );
	}

	// Clone with Dimension Substitution
	inline
	DimensionExpressionCon *
	clone( Dimension const & ) const
	{
		return new DimensionExpressionCon( *this );
	}

	// Destructor
	inline
	virtual
	~DimensionExpressionCon()
	{}

public: // Inspector

	// Initialized?
	inline
	bool
	initialized() const
	{
		return true;
	}

	// Integer?
	inline
	bool
	integer() const
	{
		return integer_;
	}

	// Constant?
	inline
	bool
	constant() const
	{
		return true;
	}

	// Reference?
	inline
	bool
	reference() const
	{
		return false;
	}

	// Reducible?
	inline
	bool
	reducible() const
	{
		return false;
	}

	// Value
	inline
	double
	operator ()() const
	{
		return value_;
	}

	// Value
	inline
	double
	value() const
	{
		return value_;
	}

	// Insert an Observer
	inline
	void
	insert_observer( Observer & ) const
	{}

	// Remove an Observer
	inline
	void
	remove_observer( Observer & ) const
	{}

public: // Modifier

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & )
	{}

private: // Data

	double value_; // Value

	bool integer_; // Integer-valued?

}; // DimensionExpressionCon

} // ObjexxFCL

#endif // ObjexxFCL_DimensionExpressionCon_hh_INCLUDED
