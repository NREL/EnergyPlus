//
// Created by Dragan Vidanovic on 7/2/17.
//

#include <vector>
#include <algorithm>
#include <math.h>

#include "Polynom.hpp"



namespace FenestrationCommon {

	////////////////////////////////////////////////////////////////////
	//  Polynom
	////////////////////////////////////////////////////////////////////
	Polynom::Polynom( std::vector< double > const& t_Coeffs ) : m_Coeffs( t_Coeffs ) {

	}

	double Polynom::valueAt( double const t_X ) const {
		double result = 0;
		double curX = 1;
		for ( auto val : m_Coeffs ) {
			result += val * curX;
			curX *= t_X;
		}

		return result;
	}

	////////////////////////////////////////////////////////////////////
	//  PolynomPoint
	////////////////////////////////////////////////////////////////////
	PolynomPoint::PolynomPoint( double const t_Value, Polynom const t_Poly ) :
		m_Polynom( t_Poly ), m_Value( t_Value ) {

	}

	double PolynomPoint::value() const {
		return m_Value;
	}

	double PolynomPoint::valueAt( double const t_X ) const {
		return m_Polynom.valueAt( t_X );
	}

	////////////////////////////////////////////////////////////////////
	//  PolynomialPoints360deg
	////////////////////////////////////////////////////////////////////
	PolynomialPoints360deg::PolynomialPoints360deg() : isSorted( false ) {
	}

	void PolynomialPoints360deg::storePoint( double const t_Value, Polynom const& t_Polynom ) {
		m_Polynoms.push_back( PolynomPoint( t_Value, t_Polynom ) );
		isSorted = false;
	}

	void PolynomialPoints360deg::sortPolynomials() {
		sort( begin( m_Polynoms ), end( m_Polynoms ),
		      [ ] ( PolynomPoint const& x, PolynomPoint const& y ) {
		      return x.value() < y.value();
	      } );
	}

	double PolynomialPoints360deg::valueAt( double const t_PointValue, double const t_Value ) {
		if ( !isSorted ) {
			sortPolynomials();
		}

		auto valFirst = min_element( begin( m_Polynoms ), end( m_Polynoms ),
		                             [ & ] ( PolynomPoint const& x, PolynomPoint const& y ) {
		                             return fabs( x.value() - t_PointValue ) < fabs( y.value() - t_PointValue );
	                             } );

		auto valSecond = valFirst + 1;
		if ( valSecond == end( m_Polynoms ) )
			valSecond = begin( m_Polynoms );

		// Process case when point is above highest point in range
		auto swappedHigh = false;
		if ( ( *valFirst ).value() > ( *valSecond ).value() ) {
			swappedHigh = true;
		}

		auto swappedLow = false;
		if ( ( *valFirst ).value() > t_PointValue ) {
			valSecond = m_Polynoms.end() - 1;
			swap( valFirst, valSecond );
			swappedLow = true;
		}

		auto y1 = ( *valFirst ).valueAt( t_Value );
		auto x1 = ( *valFirst ).value();
		auto y2 = ( *valSecond ).valueAt( t_Value );
		auto x2 = ( *valSecond ).value();
		if ( swappedLow ) x1 -= 360;
		if ( swappedHigh ) x2 += 360;

		auto value = y1 + ( y2 - y1 ) / ( x2 - x1 ) * ( t_PointValue - x1 );

		return value;
	}

}
