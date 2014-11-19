#ifndef ObjexxFCL_DimensionExpressionMax_hh_INCLUDED
#define ObjexxFCL_DimensionExpressionMax_hh_INCLUDED

// DimensionExpressionMax: DimensionExpression Binary Max Function
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

// C++ Headers
#include <algorithm>

namespace ObjexxFCL {

// DimensionExpressionMax: DimensionExpression Binary Max Function
class DimensionExpressionMax : public DimensionExpression
{

private: // Types

	typedef  DimensionExpression  Super;

public: // Creation

	// Copy Constructor
	inline
	DimensionExpressionMax( DimensionExpressionMax const & exp ) :
	 Super(),
	 exp1_p_( exp.exp1_p_ ? exp.exp1_p_->clone() : static_cast< DimensionExpression * >( 0 ) ),
	 exp2_p_( exp.exp2_p_ ? exp.exp2_p_->clone() : static_cast< DimensionExpression * >( 0 ) )
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
	}

	// Expression Constructor
	inline
	DimensionExpressionMax( DimensionExpression const & exp1, DimensionExpression const & exp2 ) :
	 exp1_p_( exp1.clone() ),
	 exp2_p_( exp2.clone() )
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
	}

	// Expression Pointer Constructor (Ownership Transfer)
	inline
	DimensionExpressionMax( DimensionExpression * exp1_p, DimensionExpression * exp2_p ) :
	 exp1_p_( exp1_p ),
	 exp2_p_( exp2_p )
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
	}

	// Clone
	inline
	DimensionExpression *
	clone() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		if ( constant() ) {
			if ( integer() ) {
				return new DimensionExpressionCon( std::max( exp1_p_->ivalue(), exp2_p_->ivalue() ) );
			} else {
				return new DimensionExpressionCon( std::max( exp1_p_->value(), exp2_p_->value() ) );
			}
		} else {
			return new DimensionExpressionMax( exp1_p_->clone(), exp2_p_->clone() );
		}
	}

	// Clone with Dimension Substitution
	inline
	DimensionExpression *
	clone( Dimension const & dim ) const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		if ( constant() ) {
			if ( integer() ) {
				return new DimensionExpressionCon( std::max( exp1_p_->ivalue(), exp2_p_->ivalue() ) );
			} else {
				return new DimensionExpressionCon( std::max( exp1_p_->value(), exp2_p_->value() ) );
			}
		} else {
			return new DimensionExpressionMax( exp1_p_->clone( dim ), exp2_p_->clone( dim ) );
		}
	}

	// Destructor
	inline
	virtual
	~DimensionExpressionMax()
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		delete exp1_p_;
		delete exp2_p_;
	}

public: // Inspector

	// Initialized?
	inline
	bool
	initialized() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		return ( ( exp1_p_->initialized() ) && ( exp2_p_->initialized() ) );
	}

	// Integer?
	inline
	bool
	integer() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		return ( ( exp1_p_->integer() ) && ( exp2_p_->integer() ) );
	}

	// Constant?
	inline
	bool
	constant() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		return ( ( exp1_p_->constant() ) && ( exp2_p_->constant() ) );
	}

	// Reference?
	inline
	bool
	reference() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		return ( ( exp1_p_->reference() ) || ( exp2_p_->reference() ) );
	}

	// Reducible?
	inline
	bool
	reducible() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		return ( ( constant() ) || ( exp1_p_->reducible() ) || ( exp2_p_->reducible() ) );
	}

	// Value
	inline
	double
	operator ()() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		return ( std::max( exp1_p_->operator ()(), exp2_p_->operator ()() ) );
	}

	// Value
	inline
	double
	value() const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		return ( std::max( exp1_p_->value(), exp2_p_->value() ) );
	}

	// Insert an Observer
	inline
	void
	insert_observer( Observer & observer ) const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		exp1_p_->insert_observer( observer );
		exp2_p_->insert_observer( observer );
	}

	// Remove an Observer
	inline
	void
	remove_observer( Observer & observer ) const
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		exp1_p_->remove_observer( observer );
		exp2_p_->remove_observer( observer );
	}

public: // Modifier

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & subject )
	{
		assert( exp1_p_ );
		assert( exp2_p_ );
		exp1_p_->destructed( subject );
		exp2_p_->destructed( subject );
	}

private: // Data

	DimensionExpression * exp1_p_; // Pointer to expression 1
	DimensionExpression * exp2_p_; // Pointer to expression 2

}; // DimensionExpressionMax

} // ObjexxFCL

#endif // ObjexxFCL_DimensionExpressionMax_hh_INCLUDED
