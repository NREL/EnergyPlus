#ifndef ObjexxFCL_DimensionExpressionCube_hh_INCLUDED
#define ObjexxFCL_DimensionExpressionCube_hh_INCLUDED

// DimensionExpressionCube: DimensionExpression Cube Function
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
#include <ObjexxFCL/DimensionExpressionCon.hh>

namespace ObjexxFCL {

// DimensionExpressionCube: DimensionExpression Cube Function
class DimensionExpressionCube : public DimensionExpression
{

private: // Types

	typedef  DimensionExpression  Super;

public: // Creation

	// Copy Constructor
	inline
	DimensionExpressionCube( DimensionExpressionCube const & exp ) :
	 Super(),
	 exp_p_( exp.exp_p_ ? exp.exp_p_->clone() : static_cast< DimensionExpression * >( 0 ) )
	{
		assert( exp_p_ );
	}

	// Expression Constructor
	inline
	DimensionExpressionCube( DimensionExpression const & exp ) :
	 exp_p_( exp.clone() )
	{
		assert( exp_p_ );
	}

	// Expression Pointer Constructor (Ownership Transfer)
	inline
	DimensionExpressionCube( DimensionExpression * exp_p ) :
	 exp_p_( exp_p )
	{
		assert( exp_p_ );
	}

	// Clone
	inline
	DimensionExpression *
	clone() const
	{
		assert( exp_p_ );
		if ( constant() ) {
			if ( integer() ) {
				return new DimensionExpressionCon( exp_p_->ivalue() * exp_p_->ivalue() * exp_p_->ivalue() );
			} else {
				return new DimensionExpressionCon( exp_p_->value() * exp_p_->value() * exp_p_->value() );
			}
		} else {
			return new DimensionExpressionCube( exp_p_->clone() );
		}
	}

	// Clone with Dimension Substitution
	inline
	DimensionExpression *
	clone( Dimension const & dim ) const
	{
		assert( exp_p_ );
		if ( constant() ) {
			if ( integer() ) {
				return new DimensionExpressionCon( exp_p_->ivalue() * exp_p_->ivalue() * exp_p_->ivalue() );
			} else {
				return new DimensionExpressionCon( exp_p_->value() * exp_p_->value() * exp_p_->value() );
			}
		} else {
			return new DimensionExpressionCube( exp_p_->clone( dim ) );
		}
	}

	// Destructor
	inline
	virtual
	~DimensionExpressionCube()
	{
		assert( exp_p_ );
		delete exp_p_;
	}

public: // Inspector

	// Initialized?
	inline
	bool
	initialized() const
	{
		assert( exp_p_ );
		return ( exp_p_->initialized() );
	}

	// Integer?
	inline
	bool
	integer() const
	{
		assert( exp_p_ );
		return ( exp_p_->integer() );
	}

	// Constant?
	inline
	bool
	constant() const
	{
		assert( exp_p_ );
		return ( exp_p_->constant() );
	}

	// Reference?
	inline
	bool
	reference() const
	{
		assert( exp_p_ );
		return ( exp_p_->reference() );
	}

	// Reducible?
	inline
	bool
	reducible() const
	{
		assert( exp_p_ );
		return ( ( constant() ) || ( exp_p_->reducible() ) );
	}

	// Value
	inline
	double
	operator ()() const
	{
		assert( exp_p_ );
		return ( exp_p_->operator ()() * exp_p_->operator ()() * exp_p_->operator ()() );
	}

	// Value
	inline
	double
	value() const
	{
		assert( exp_p_ );
		return ( exp_p_->value() * exp_p_->value() * exp_p_->value() );
	}

	// Insert an Observer
	inline
	void
	insert_observer( Observer & observer ) const
	{
		assert( exp_p_ );
		exp_p_->insert_observer( observer );
	}

	// Remove an Observer
	inline
	void
	remove_observer( Observer & observer ) const
	{
		assert( exp_p_ );
		exp_p_->remove_observer( observer );
	}

public: // Modifier

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & subject )
	{
		assert( exp_p_ );
		exp_p_->destructed( subject );
	}

private: // Data

	DimensionExpression * exp_p_; // Pointer to expression

}; // DimensionExpressionCube

} // ObjexxFCL

#endif // ObjexxFCL_DimensionExpressionCube_hh_INCLUDED
