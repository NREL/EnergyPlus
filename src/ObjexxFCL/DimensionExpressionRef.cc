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
#include <ObjexxFCL/DimensionExpressionRef.hh>
#include <ObjexxFCL/Dimension.hh>

namespace ObjexxFCL {

	// Clone with Dimension Substitution
	DimensionExpression *
	DimensionExpressionRef::clone( Dimension const & dim ) const
	{
		if ( ( dim_p_ != &dim ) || ( dim.exp_p() == 0 ) ) { // Copy of this reference
			return new DimensionExpressionRef( *this );
		} else { // Clone of the current expression
			return dim.exp().clone();
		}
	}

	// Initialized?
	bool
	DimensionExpressionRef::initialized() const
	{
		assert( dim_p_ );
		return dim_p_->initialized();
	}

	// Value
	double
	DimensionExpressionRef::operator ()() const
	{
		assert( dim_p_ );
		return static_cast< double >( dim_p_->operator ()() );
	}

	// Value
	double
	DimensionExpressionRef::value() const
	{
		assert( dim_p_ );
		return static_cast< double >( dim_p_->value() );
	}

	// Insert an Observer
	void
	DimensionExpressionRef::insert_observer( Observer & observer ) const
	{
		assert( dim_p_ );
		dim_p_->insert_observer( observer );
	}

	// Remove an Observer
	void
	DimensionExpressionRef::remove_observer( Observer & observer ) const
	{
		if ( dim_p_ ) dim_p_->remove_observer( observer );
	}

	// Update for Destruction of a Subject
	void
	DimensionExpressionRef::destructed( Subject const & subject )
	{
		if ( &subject == static_cast< Subject const * >( dim_p_ ) ) { // Referenced Dimension is being destructed
			dim_p_ = 0; // Zero the pointer to trigger assertion failure if it is ever used
		}
	}

} // ObjexxFCL
