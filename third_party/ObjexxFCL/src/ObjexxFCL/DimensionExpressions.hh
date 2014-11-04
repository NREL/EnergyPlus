#ifndef ObjexxFCL_DimensionExpressions_hh_INCLUDED
#define ObjexxFCL_DimensionExpressions_hh_INCLUDED

// DimensionExpressions: DimensionExpression Headers for Sources that Use Dimension Expressions
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
#include <ObjexxFCL/DimensionExpressionCon.hh>
#include <ObjexxFCL/DimensionExpressionRef.hh>
#include <ObjexxFCL/DimensionExpressionSum.hh>
#include <ObjexxFCL/DimensionExpressionSub.hh>
#include <ObjexxFCL/DimensionExpressionMul.hh>
#include <ObjexxFCL/DimensionExpressionDiv.hh>
#include <ObjexxFCL/DimensionExpressionMin.hh>
#include <ObjexxFCL/DimensionExpressionMax.hh>
#include <ObjexxFCL/DimensionExpressionPow.hh>
#include <ObjexxFCL/DimensionExpressionSquare.hh>
#include <ObjexxFCL/DimensionExpressionCube.hh>

namespace ObjexxFCL {

// Forward
class Dimension;

// Dimension Math

// +Dimension
inline
DimensionExpressionRef
operator +( Dimension const & dim )
{
	return DimensionExpressionRef( dim );
}

// -Dimension
inline
DimensionExpressionMul
operator -( Dimension const & dim )
{
	return DimensionExpressionMul( new DimensionExpressionCon( -1 ), new DimensionExpressionRef( dim ) );
}

// Dimension + Dimension
inline
DimensionExpressionSum
operator +( Dimension const & dim1, Dimension const & dim2 )
{
	return DimensionExpressionSum( new DimensionExpressionRef( dim1 ), new DimensionExpressionRef( dim2 ) );
}

// Dimension + int
inline
DimensionExpressionSum
operator +( Dimension const & dim, int const value )
{
	return DimensionExpressionSum( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// int + Dimension
inline
DimensionExpressionSum
operator +( int const value, Dimension const & dim )
{
	return DimensionExpressionSum( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// Dimension + double
inline
DimensionExpressionSum
operator +( Dimension const & dim, double const value )
{
	return DimensionExpressionSum( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// double + Dimension
inline
DimensionExpressionSum
operator +( double const value, Dimension const & dim )
{
	return DimensionExpressionSum( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// Dimension - Dimension
inline
DimensionExpressionSub
operator -( Dimension const & dim1, Dimension const & dim2 )
{
	return DimensionExpressionSub( new DimensionExpressionRef( dim1 ), new DimensionExpressionRef( dim2 ) );
}

// Dimension - int
inline
DimensionExpressionSub
operator -( Dimension const & dim, int const value )
{
	return DimensionExpressionSub( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// int - Dimension
inline
DimensionExpressionSub
operator -( int const value, Dimension const & dim )
{
	return DimensionExpressionSub( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// Dimension - double
inline
DimensionExpressionSub
operator -( Dimension const & dim, double const value )
{
	return DimensionExpressionSub( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// double - Dimension
inline
DimensionExpressionSub
operator -( double const value, Dimension const & dim )
{
	return DimensionExpressionSub( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// Dimension * Dimension
inline
DimensionExpressionMul
operator *( Dimension const & dim1, Dimension const & dim2 )
{
	return DimensionExpressionMul( new DimensionExpressionRef( dim1 ), new DimensionExpressionRef( dim2 ) );
}

// Dimension * int
inline
DimensionExpressionMul
operator *( Dimension const & dim, int const value )
{
	return DimensionExpressionMul( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// int * Dimension
inline
DimensionExpressionMul
operator *( int const value, Dimension const & dim )
{
	return DimensionExpressionMul( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// Dimension * double
inline
DimensionExpressionMul
operator *( Dimension const & dim, double const value )
{
	return DimensionExpressionMul( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// double * Dimension
inline
DimensionExpressionMul
operator *( double const value, Dimension const & dim )
{
	return DimensionExpressionMul( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// Dimension / Dimension
inline
DimensionExpressionDiv
operator /( Dimension const & dim1, Dimension const & dim2 )
{
	return DimensionExpressionDiv( new DimensionExpressionRef( dim1 ), new DimensionExpressionRef( dim2 ) );
}

// Dimension / int
inline
DimensionExpressionDiv
operator /( Dimension const & dim, int const value )
{
	return DimensionExpressionDiv( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// int / Dimension
inline
DimensionExpressionDiv
operator /( int const value, Dimension const & dim )
{
	return DimensionExpressionDiv( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// Dimension / double
inline
DimensionExpressionDiv
operator /( Dimension const & dim, double const value )
{
	return DimensionExpressionDiv( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// double / Dimension
inline
DimensionExpressionDiv
operator /( double const value, Dimension const & dim )
{
	return DimensionExpressionDiv( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// DimensionExpression Math

// +DimensionExpression
inline
DimensionExpressionMul
operator +( DimensionExpression const & exp )
{
	return DimensionExpressionMul( new DimensionExpressionCon( 1 ), exp.clone() );
}

// -DimensionExpression
inline
DimensionExpressionMul
operator -( DimensionExpression const & exp )
{
	return DimensionExpressionMul( new DimensionExpressionCon( -1 ), exp.clone() );
}

// DimensionExpression + DimensionExpression
inline
DimensionExpressionSum
operator +( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return DimensionExpressionSum( exp1.clone(), exp2.clone() );
}

// DimensionExpression + Dimension
inline
DimensionExpressionSum
operator +( DimensionExpression const & exp, Dimension const & dim )
{
	return DimensionExpressionSum( exp.clone(), new DimensionExpressionRef( dim ) );
}

// Dimension + DimensionExpression
inline
DimensionExpressionSum
operator +( Dimension const & dim, DimensionExpression const & exp )
{
	return DimensionExpressionSum( new DimensionExpressionRef( dim ), exp.clone() );
}

// DimensionExpression + int
inline
DimensionExpressionSum
operator +( DimensionExpression const & exp, int const value )
{
	return DimensionExpressionSum( exp.clone(), new DimensionExpressionCon( value ) );
}

// int + DimensionExpression
inline
DimensionExpressionSum
operator +( int const value, DimensionExpression const & exp )
{
	return DimensionExpressionSum( new DimensionExpressionCon( value ), exp.clone() );
}

// DimensionExpression + double
inline
DimensionExpressionSum
operator +( DimensionExpression const & exp, double const value )
{
	return DimensionExpressionSum( exp.clone(), new DimensionExpressionCon( value ) );
}

// double + DimensionExpression
inline
DimensionExpressionSum
operator +( double const value, DimensionExpression const & exp )
{
	return DimensionExpressionSum( new DimensionExpressionCon( value ), exp.clone() );
}

// DimensionExpression - DimensionExpression
inline
DimensionExpressionSub
operator -( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return DimensionExpressionSub( exp1.clone(), exp2.clone() );
}

// DimensionExpression - Dimension
inline
DimensionExpressionSub
operator -( DimensionExpression const & exp, Dimension const & dim )
{
	return DimensionExpressionSub( exp.clone(), new DimensionExpressionRef( dim ) );
}

// Dimension - DimensionExpression
inline
DimensionExpressionSub
operator -( Dimension const & dim, DimensionExpression const & exp )
{
	return DimensionExpressionSub( new DimensionExpressionRef( dim ), exp.clone() );
}

// DimensionExpression - int
inline
DimensionExpressionSub
operator -( DimensionExpression const & exp, int const value )
{
	return DimensionExpressionSub( exp.clone(), new DimensionExpressionCon( value ) );
}

// int - DimensionExpression
inline
DimensionExpressionSub
operator -( int const value, DimensionExpression const & exp )
{
	return DimensionExpressionSub( new DimensionExpressionCon( value ), exp.clone() );
}

// DimensionExpression - double
inline
DimensionExpressionSub
operator -( DimensionExpression const & exp, double const value )
{
	return DimensionExpressionSub( exp.clone(), new DimensionExpressionCon( value ) );
}

// double - DimensionExpression
inline
DimensionExpressionSub
operator -( double const value, DimensionExpression const & exp )
{
	return DimensionExpressionSub( new DimensionExpressionCon( value ), exp.clone() );
}

// DimensionExpression * DimensionExpression
inline
DimensionExpressionMul
operator *( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return DimensionExpressionMul( exp1.clone(), exp2.clone() );
}

// DimensionExpression * Dimension
inline
DimensionExpressionMul
operator *( DimensionExpression const & exp, Dimension const & dim )
{
	return DimensionExpressionMul( exp.clone(), new DimensionExpressionRef( dim ) );
}

// Dimension * DimensionExpression
inline
DimensionExpressionMul
operator *( Dimension const & dim, DimensionExpression const & exp )
{
	return DimensionExpressionMul( new DimensionExpressionRef( dim ), exp.clone() );
}

// DimensionExpression * int
inline
DimensionExpressionMul
operator *( DimensionExpression const & exp, int const value )
{
	return DimensionExpressionMul( exp.clone(), new DimensionExpressionCon( value ) );
}

// int * DimensionExpression
inline
DimensionExpressionMul
operator *( int const value, DimensionExpression const & exp )
{
	return DimensionExpressionMul( new DimensionExpressionCon( value ), exp.clone() );
}

// DimensionExpression * double
inline
DimensionExpressionMul
operator *( DimensionExpression const & exp, double const value )
{
	return DimensionExpressionMul( exp.clone(), new DimensionExpressionCon( value ) );
}

// double * DimensionExpression
inline
DimensionExpressionMul
operator *( double const value, DimensionExpression const & exp )
{
	return DimensionExpressionMul( new DimensionExpressionCon( value ), exp.clone() );
}

// DimensionExpression / DimensionExpression
inline
DimensionExpressionDiv
operator /( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return DimensionExpressionDiv( exp1.clone(), exp2.clone() );
}

// DimensionExpression / Dimension
inline
DimensionExpressionDiv
operator /( DimensionExpression const & exp, Dimension const & dim )
{
	return DimensionExpressionDiv( exp.clone(), new DimensionExpressionRef( dim ) );
}

// Dimension / DimensionExpression
inline
DimensionExpressionDiv
operator /( Dimension const & dim, DimensionExpression const & exp )
{
	return DimensionExpressionDiv( new DimensionExpressionRef( dim ), exp.clone() );
}

// DimensionExpression / int
inline
DimensionExpressionDiv
operator /( DimensionExpression const & exp, int const value )
{
	return DimensionExpressionDiv( exp.clone(), new DimensionExpressionCon( value ) );
}

// int / DimensionExpression
inline
DimensionExpressionDiv
operator /( int const value, DimensionExpression const & exp )
{
	return DimensionExpressionDiv( new DimensionExpressionCon( value ), exp.clone() );
}

// DimensionExpression / double
inline
DimensionExpressionDiv
operator /( DimensionExpression const & exp, double const value )
{
	return DimensionExpressionDiv( exp.clone(), new DimensionExpressionCon( value ) );
}

// double / DimensionExpression
inline
DimensionExpressionDiv
operator /( double const value, DimensionExpression const & exp )
{
	return DimensionExpressionDiv( new DimensionExpressionCon( value ), exp.clone() );
}

// Min

// min( Dimension, Dimension )
inline
DimensionExpressionMin
min( Dimension const & dim1, Dimension const & dim2 )
{
	return DimensionExpressionMin( new DimensionExpressionRef( dim1 ), new DimensionExpressionRef( dim2 ) );
}

// min( Dimension, int )
inline
DimensionExpressionMin
min( Dimension const & dim, int const value )
{
	return DimensionExpressionMin( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// min( int, Dimension )
inline
DimensionExpressionMin
min( int const value, Dimension const & dim )
{
	return DimensionExpressionMin( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// min( Dimension, double )
inline
DimensionExpressionMin
min( Dimension const & dim, double const value )
{
	return DimensionExpressionMin( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// min( double, Dimension )
inline
DimensionExpressionMin
min( double const value, Dimension const & dim )
{
	return DimensionExpressionMin( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// min( Dimension, DimensionExpression )
inline
DimensionExpressionMin
min( Dimension const & dim, DimensionExpression const & exp )
{
	return DimensionExpressionMin( new DimensionExpressionRef( dim ), exp.clone() );
}

// min( DimensionExpression, Dimension )
inline
DimensionExpressionMin
min( DimensionExpression const & exp, Dimension const & dim )
{
	return DimensionExpressionMin( exp.clone(), new DimensionExpressionRef( dim ) );
}

// min( DimensionExpression, DimensionExpression )
inline
DimensionExpressionMin
min( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return DimensionExpressionMin( exp1.clone(), exp2.clone() );
}

// min( DimensionExpression, int )
inline
DimensionExpressionMin
min( DimensionExpression const & exp, int const value )
{
	return DimensionExpressionMin( exp.clone(), new DimensionExpressionCon( value ) );
}

// min( int, DimensionExpression )
inline
DimensionExpressionMin
min( int const value, DimensionExpression const & exp )
{
	return DimensionExpressionMin( new DimensionExpressionCon( value ), exp.clone() );
}

// min( DimensionExpression, double )
inline
DimensionExpressionMin
min( DimensionExpression const & exp, double const value )
{
	return DimensionExpressionMin( exp.clone(), new DimensionExpressionCon( value ) );
}

// min( double, DimensionExpression )
inline
DimensionExpressionMin
min( double const value, DimensionExpression const & exp )
{
	return DimensionExpressionMin( new DimensionExpressionCon( value ), exp.clone() );
}

// Max

// max( Dimension, Dimension )
inline
DimensionExpressionMax
max( Dimension const & dim1, Dimension const & dim2 )
{
	return DimensionExpressionMax( new DimensionExpressionRef( dim1 ), new DimensionExpressionRef( dim2 ) );
}

// max( Dimension, int )
inline
DimensionExpressionMax
max( Dimension const & dim, int const value )
{
	return DimensionExpressionMax( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// max( int, Dimension )
inline
DimensionExpressionMax
max( int const value, Dimension const & dim )
{
	return DimensionExpressionMax( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// max( Dimension, double )
inline
DimensionExpressionMax
max( Dimension const & dim, double const value )
{
	return DimensionExpressionMax( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// max( double, Dimension )
inline
DimensionExpressionMax
max( double const value, Dimension const & dim )
{
	return DimensionExpressionMax( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// max( Dimension, DimensionExpression )
inline
DimensionExpressionMax
max( Dimension const & dim, DimensionExpression const & exp )
{
	return DimensionExpressionMax( new DimensionExpressionRef( dim ), exp.clone() );
}

// max( DimensionExpression, Dimension )
inline
DimensionExpressionMax
max( DimensionExpression const & exp, Dimension const & dim )
{
	return DimensionExpressionMax( exp.clone(), new DimensionExpressionRef( dim ) );
}

// max( DimensionExpression, DimensionExpression )
inline
DimensionExpressionMax
max( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return DimensionExpressionMax( exp1.clone(), exp2.clone() );
}

// max( DimensionExpression, int )
inline
DimensionExpressionMax
max( DimensionExpression const & exp, int const value )
{
	return DimensionExpressionMax( exp.clone(), new DimensionExpressionCon( value ) );
}

// max( int, DimensionExpression )
inline
DimensionExpressionMax
max( int const value, DimensionExpression const & exp )
{
	return DimensionExpressionMax( new DimensionExpressionCon( value ), exp.clone() );
}

// max( DimensionExpression, double )
inline
DimensionExpressionMax
max( DimensionExpression const & exp, double const value )
{
	return DimensionExpressionMax( exp.clone(), new DimensionExpressionCon( value ) );
}

// max( double, DimensionExpression )
inline
DimensionExpressionMax
max( double const value, DimensionExpression const & exp )
{
	return DimensionExpressionMax( new DimensionExpressionCon( value ), exp.clone() );
}

// Pow

// pow( Dimension, Dimension )
inline
DimensionExpressionPow
pow( Dimension const & dim1, Dimension const & dim2 )
{
	return DimensionExpressionPow( new DimensionExpressionRef( dim1 ), new DimensionExpressionRef( dim2 ) );
}

// pow( Dimension, int )
inline
DimensionExpressionPow
pow( Dimension const & dim, int const value )
{
	return DimensionExpressionPow( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// pow( int, Dimension )
inline
DimensionExpressionPow
pow( int const value, Dimension const & dim )
{
	return DimensionExpressionPow( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// pow( Dimension, double )
inline
DimensionExpressionPow
pow( Dimension const & dim, double const value )
{
	return DimensionExpressionPow( new DimensionExpressionRef( dim ), new DimensionExpressionCon( value ) );
}

// pow( double, Dimension )
inline
DimensionExpressionPow
pow( double const value, Dimension const & dim )
{
	return DimensionExpressionPow( new DimensionExpressionCon( value ), new DimensionExpressionRef( dim ) );
}

// pow( Dimension, DimensionExpression )
inline
DimensionExpressionPow
pow( Dimension const & dim, DimensionExpression const & exp )
{
	return DimensionExpressionPow( new DimensionExpressionRef( dim ), exp.clone() );
}

// pow( DimensionExpression, Dimension )
inline
DimensionExpressionPow
pow( DimensionExpression const & exp, Dimension const & dim )
{
	return DimensionExpressionPow( exp.clone(), new DimensionExpressionRef( dim ) );
}

// pow( DimensionExpression, DimensionExpression )
inline
DimensionExpressionPow
pow( DimensionExpression const & exp1, DimensionExpression const & exp2 )
{
	return DimensionExpressionPow( exp1.clone(), exp2.clone() );
}

// pow( DimensionExpression, int )
inline
DimensionExpressionPow
pow( DimensionExpression const & exp, int const value )
{
	return DimensionExpressionPow( exp.clone(), new DimensionExpressionCon( value ) );
}

// pow( int, DimensionExpression )
inline
DimensionExpressionPow
pow( int const value, DimensionExpression const & exp )
{
	return DimensionExpressionPow( new DimensionExpressionCon( value ), exp.clone() );
}

// pow( DimensionExpression, double )
inline
DimensionExpressionPow
pow( DimensionExpression const & exp, double const value )
{
	return DimensionExpressionPow( exp.clone(), new DimensionExpressionCon( value ) );
}

// pow( double, DimensionExpression )
inline
DimensionExpressionPow
pow( double const value, DimensionExpression const & exp )
{
	return DimensionExpressionPow( new DimensionExpressionCon( value ), exp.clone() );
}

// Square

// square( Dimension )
inline
DimensionExpressionSquare
square( Dimension const & dim )
{
	return DimensionExpressionSquare( new DimensionExpressionRef( dim ) );
}

// square( DimensionExpression )
inline
DimensionExpressionSquare
square( DimensionExpression const & exp )
{
	return DimensionExpressionSquare( exp.clone() );
}

// square( int )
inline
DimensionExpressionCon
square( int const value )
{
	return DimensionExpressionCon( value * value );
}

// square( double )
inline
DimensionExpressionCon
square( double const value )
{
	return DimensionExpressionCon( value * value );
}

// Cube

// cube( Dimension )
inline
DimensionExpressionCube
cube( Dimension const & dim )
{
	return DimensionExpressionCube( new DimensionExpressionRef( dim ) );
}

// cube( DimensionExpression )
inline
DimensionExpressionCube
cube( DimensionExpression const & exp )
{
	return DimensionExpressionCube( exp.clone() );
}

// cube( int )
inline
DimensionExpressionCon
cube( int const value )
{
	return DimensionExpressionCon( value * value * value );
}

// cube( double )
inline
DimensionExpressionCon
cube( double const value )
{
	return DimensionExpressionCon( value * value * value );
}

} // ObjexxFCL

#endif // ObjexxFCL_DimensionExpressions_hh_INCLUDED
