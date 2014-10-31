#ifndef ObjexxFCL_DimensionExpressionRef_hh_INCLUDED
#define ObjexxFCL_DimensionExpressionRef_hh_INCLUDED

// DimensionExpressionRef: Dimension Reference DimensionExpression
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

// C++ Headers
#include <cassert>

namespace ObjexxFCL {

// DimensionExpressionRef: Dimension Reference DimensionExpression
class DimensionExpressionRef : public DimensionExpression
{

private: // Types

	typedef  DimensionExpression  Super;

public: // Creation

	// Copy Constructor
	inline
	DimensionExpressionRef( DimensionExpressionRef const & exp ) :
	 Super(),
	 dim_p_( exp.dim_p_ )
	{
		assert( dim_p_ );
	}

	// Dimension Constructor
	inline
	explicit
	DimensionExpressionRef( Dimension const & dim ) :
	 dim_p_( &dim )
	{}

	// Clone
	inline
	DimensionExpressionRef *
	clone() const
	{
		return new DimensionExpressionRef( *this );
	}

	// Clone with Dimension Substitution
	DimensionExpression *
	clone( Dimension const & dim ) const;

	// Destructor
	inline
	virtual
	~DimensionExpressionRef()
	{}

public: // Inspector

	// Initialized?
	bool
	initialized() const;

	// Integer?
	inline
	bool
	integer() const
	{
		return true;
	}

	// Constant?
	inline
	bool
	constant() const
	{
		return false;
	}

	// Reference?
	inline
	bool
	reference() const
	{
		return true;
	}

	// Reducible?
	inline
	bool
	reducible() const
	{
		return false;
	}

	// Value
	double
	operator ()() const;

	// Value
	double
	value() const;

	// Insert an Observer
	void
	insert_observer( Observer & observer ) const;

	// Remove an Observer
	void
	remove_observer( Observer & observer ) const;

public: // Modifier

	// Update for Destruction of a Subject
	void
	destructed( Subject const & subject );

private: // Data

	Dimension const * dim_p_; // Pointer (non-owning) to Dimension referenced

}; // DimensionExpressionRef

} // ObjexxFCL

#endif // ObjexxFCL_DimensionExpressionRef_hh_INCLUDED
