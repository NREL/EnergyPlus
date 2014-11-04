// Dimension: Dynamic Dimension
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
#include <ObjexxFCL/Dimension.hh>
#include <ObjexxFCL/DimensionExpressionCon.hh>
#include <ObjexxFCL/DimensionExpressionRef.hh>
#include <ObjexxFCL/DimensionExpressionSum.hh>
#include <ObjexxFCL/DimensionExpressionSub.hh>
#include <ObjexxFCL/DimensionExpressionMul.hh>
#include <ObjexxFCL/DimensionExpressionDiv.hh>

// C++ Headers
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>

namespace ObjexxFCL {

	// Copy Constructor
	Dimension::Dimension( Dimension const & dim ) :
	 ObserverMulti(),
	 exp_p_( new DimensionExpressionRef( dim ) ),
	 initialized_( exp_p_->initialized() ),
	 value_( dim.value_ )
	{
		insert_as_observer_of( dim );
	}

	// int Constructor
	Dimension::Dimension( int const i ) :
	 exp_p_( new DimensionExpressionCon( i ) ),
	 initialized_( true ),
	 value_( i )
	{}

	// double Constructor
	Dimension::Dimension( double const d ) :
	 exp_p_( new DimensionExpressionCon( d ) ),
	 initialized_( true ),
	 value_( static_cast< int >( d ) )
	{}

	// Copy Assignment
	Dimension &
	Dimension::operator =( Dimension const & dim )
	{
		if ( this != &dim ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionRef( dim );
			insert_as_observer_of( dim );
			update();
		}
		notify();
		return *this;
	}

	// int Assignment
	Dimension &
	Dimension::operator =( int const i )
	{
		remove_as_observer();
		delete exp_p_; exp_p_ = new DimensionExpressionCon( i );
		update_notify();
		return *this;
	}

	// double Assignment
	Dimension &
	Dimension::operator =( double const d )
	{
		remove_as_observer();
		delete exp_p_; exp_p_ = new DimensionExpressionCon( d );
		update_notify();
		return *this;
	}

	// += Dimension
	Dimension &
	Dimension::operator +=( Dimension const & dim )
	{
		assert( exp_p_ );
		if ( this != &dim ) {
			exp_p_ = new DimensionExpressionSum( exp_p_, new DimensionExpressionRef( dim ) );
			insert_as_observer_of( dim );
		} else {
			exp_p_ = new DimensionExpressionSum( exp_p_, exp_p_->clone() );
			reduce_expression();
		}
		update_notify();
		return *this;
	}

	// += Expression
	Dimension &
	Dimension::operator +=( Expression const & exp )
	{
		assert( exp_p_ );
		exp_p_ = new DimensionExpressionSum( exp_p_, exp.clone( *this ) );
		reduce_expression();
		insert_as_observer_of( exp );
		update_notify();
		return *this;
	}

	// += int
	Dimension &
	Dimension::operator +=( int const i )
	{
		assert( exp_p_ );
		if ( i != 0 ) {
			exp_p_ = new DimensionExpressionSum( exp_p_, new DimensionExpressionCon( i ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// += double
	Dimension &
	Dimension::operator +=( double const d )
	{
		assert( exp_p_ );
		if ( d != 0.0 ) {
			exp_p_ = new DimensionExpressionSum( exp_p_, new DimensionExpressionCon( d ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// -= Dimension
	Dimension &
	Dimension::operator -=( Dimension const & dim )
	{
		assert( exp_p_ );
		if ( this != &dim ) {
			exp_p_ = new DimensionExpressionSub( exp_p_, new DimensionExpressionRef( dim ) );
			insert_as_observer_of( dim );
		} else {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( 0 );
		}
		update_notify();
		return *this;
	}

	// -= Expression
	Dimension &
	Dimension::operator -=( Expression const & exp )
	{
		assert( exp_p_ );
		exp_p_ = new DimensionExpressionSub( exp_p_, exp.clone( *this ) );
		reduce_expression();
		insert_as_observer_of( exp );
		update_notify();
		return *this;
	}

	// -= int
	Dimension &
	Dimension::operator -=( int const i )
	{
		assert( exp_p_ );
		if ( i != 0 ) {
			exp_p_ = new DimensionExpressionSub( exp_p_, new DimensionExpressionCon( i ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// -= double
	Dimension &
	Dimension::operator -=( double const d )
	{
		assert( exp_p_ );
		if ( d != 0.0 ) {
			exp_p_ = new DimensionExpressionSub( exp_p_, new DimensionExpressionCon( d ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// *= Dimension
	Dimension &
	Dimension::operator *=( Dimension const & dim )
	{
		assert( exp_p_ );
		if ( this != &dim ) {
			exp_p_ = new DimensionExpressionMul( exp_p_, new DimensionExpressionRef( dim ) );
			insert_as_observer_of( dim );
		} else {
			exp_p_ = new DimensionExpressionMul( exp_p_, exp_p_->clone() );
			reduce_expression();
		}
		update_notify();
		return *this;
	}

	// *= Expression
	Dimension &
	Dimension::operator *=( Expression const & exp )
	{
		assert( exp_p_ );
		exp_p_ = new DimensionExpressionMul( exp_p_, exp.clone( *this ) );
		reduce_expression();
		insert_as_observer_of( exp );
		update_notify();
		return *this;
	}

	// *= int
	Dimension &
	Dimension::operator *=( int const i )
	{
		assert( exp_p_ );
		if ( i != 1 ) {
			exp_p_ = new DimensionExpressionMul( exp_p_, new DimensionExpressionCon( i ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// *= double
	Dimension &
	Dimension::operator *=( double const d )
	{
		assert( exp_p_ );
		if ( d != 1.0 ) {
			exp_p_ = new DimensionExpressionMul( exp_p_, new DimensionExpressionCon( d ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// /= Dimension
	Dimension &
	Dimension::operator /=( Dimension const & dim )
	{
		assert( exp_p_ );
		if ( this != &dim ) {
			exp_p_ = new DimensionExpressionDiv( exp_p_, new DimensionExpressionRef( dim ) );
			insert_as_observer_of( dim );
		} else { // Keep as a ratio to catch 0/0 cases
			exp_p_ = new DimensionExpressionDiv( exp_p_, exp_p_->clone() );
			reduce_expression();
		}
		update_notify();
		return *this;
	}

	// /= Expression
	Dimension &
	Dimension::operator /=( Expression const & exp )
	{
		assert( exp_p_ );
		exp_p_ = new DimensionExpressionDiv( exp_p_, exp.clone( *this ) );
		reduce_expression();
		insert_as_observer_of( exp );
		update_notify();
		return *this;
	}

	// /= int
	Dimension &
	Dimension::operator /=( int const i )
	{
		assert( exp_p_ );
		assert( i != 0 );
		if ( i != 1 ) {
			exp_p_ = new DimensionExpressionDiv( exp_p_, new DimensionExpressionCon( i ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// /= double
	Dimension &
	Dimension::operator /=( double const d )
	{
		assert( exp_p_ );
		assert( d != 0.0 );
		if ( d != 1.0 ) {
			exp_p_ = new DimensionExpressionDiv( exp_p_, new DimensionExpressionCon( d ) );
			reduce_expression();
			update();
		}
		notify();
		return *this;
	}

	// int Assignment if Bigger than Value or Smaller than Multiplier * Value
	Dimension &
	Dimension::assign_if( int const i, double const m )
	{
		if ( ( ! initialized_ ) || ( i > value_ ) || ( i < m * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( i );
			update();
		}
		notify();
		return *this;
	}

	// double Assignment if Bigger than Value or Smaller than Multiplier * Value
	Dimension &
	Dimension::assign_if( double const d, double const m )
	{
		if ( ( ! initialized_ ) || ( d > value_ ) || ( d < m * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( d );
			update();
		}
		notify();
		return *this;
	}

	// int Assignment if Bigger than Value or Smaller than Half Value
	Dimension &
	Dimension::assign_if_half( int const i )
	{
		if ( ( ! initialized_ ) || ( i > value_ ) || ( i < 0.5 * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( i );
			update();
		}
		notify();
		return *this;
	}

	// double Assignment if Bigger than Value or Smaller than Half Value
	Dimension &
	Dimension::assign_if_half( double const d )
	{
		if ( ( ! initialized_ ) || ( d > value_ ) || ( d < 0.5 * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( d );
			update();
		}
		notify();
		return *this;
	}

	// int Assignment if Bigger than Value
	Dimension &
	Dimension::assign_if_bigger( int const i )
	{
		if ( ( ! initialized_ ) || ( i > value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( i );
			update();
		}
		notify();
		return *this;
	}

	// double Assignment if Bigger than Value
	Dimension &
	Dimension::assign_if_bigger( double const d )
	{
		if ( ( ! initialized_ ) || ( d > value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( d );
			update();
		}
		notify();
		return *this;
	}

	// int Assignment if Bigger than Value or Smaller than Multiplier * Value: Notify if Changed
	Dimension &
	Dimension::assign_if_nic( int const i, double const m )
	{
		if ( ( ! initialized_ ) || ( i > value_ ) || ( i < m * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( i );
			update_notify_if_changed();
		}
		return *this;
	}

	// double Assignment if Bigger than Value or Smaller than Multiplier * Value: Notify if Changed
	Dimension &
	Dimension::assign_if_nic( double const d, double const m )
	{
		if ( ( ! initialized_ ) || ( d > value_ ) || ( d < m * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( d );
			update_notify_if_changed();
		}
		return *this;
	}

	// int Assignment if Bigger than Value or Smaller than Half Value: Notify if Changed
	Dimension &
	Dimension::assign_if_half_nic( int const i )
	{
		if ( ( ! initialized_ ) || ( i > value_ ) || ( i < 0.5 * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( i );
			update_notify_if_changed();
		}
		return *this;
	}

	// double Assignment if Bigger than Value or Smaller than Half Value: Notify if Changed
	Dimension &
	Dimension::assign_if_half_nic( double const d )
	{
		if ( ( ! initialized_ ) || ( d > value_ ) || ( d < 0.5 * value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( d );
			update_notify_if_changed();
		}
		return *this;
	}

	// int Assignment if Bigger than Value: Notify if Changed
	Dimension &
	Dimension::assign_if_bigger_nic( int const i )
	{
		if ( ( ! initialized_ ) || ( i > value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( i );
			update_notify_if_changed();
		}
		return *this;
	}

	// double Assignment if Bigger than Value: Notify if Changed
	Dimension &
	Dimension::assign_if_bigger_nic( double const d )
	{
		if ( ( ! initialized_ ) || ( d > value_ ) ) {
			remove_as_observer();
			delete exp_p_; exp_p_ = new DimensionExpressionCon( d );
			update_notify_if_changed();
		}
		return *this;
	}

	// ++Dimension
	Dimension &
	Dimension::operator ++()
	{
		assert( exp_p_ );
		exp_p_ = new DimensionExpressionSum( exp_p_, new DimensionExpressionCon( 1 ) );
		reduce_expression();
		update();
		notify();
		return *this;
	}

	// --Dimension
	Dimension &
	Dimension::operator --()
	{
		assert( exp_p_ );
		exp_p_ = new DimensionExpressionSub( exp_p_, new DimensionExpressionCon( 1 ) );
		reduce_expression();
		update();
		notify();
		return *this;
	}

// Stream Input
std::istream &
operator >>( std::istream & stream, Dimension & dim )
{
	std::string input_string;
	stream >> input_string;
	std::istringstream num_stream( input_string );
	int ival;
	num_stream >> ival;
	if ( ( num_stream ) && ( num_stream.eof() ) ) { // OK as int
		dim = ival;
	} else {
		num_stream.clear();
		num_stream.seekg( std::ios::beg );
		double dval;
		num_stream >> dval;
		if ( ( num_stream ) && ( num_stream.eof() ) ) { // OK as double
			dim = dval;
		} else { // Invalid
#ifndef OBJEXXFCL_ERROR_SUPPRESS
			std::cerr << "\nObjexxFCL Error: Invalid stream input to Dimension: " << input_string << std::endl;
			std::exit( EXIT_FAILURE );
#endif
			assert( false ); // Invalid stream input to Dimension
		}
	}
	return stream;
}

// Stream Output
std::ostream &
operator <<( std::ostream & stream, Dimension const & dim )
{
	stream << dim.value();
	return stream;
}

} // ObjexxFCL
