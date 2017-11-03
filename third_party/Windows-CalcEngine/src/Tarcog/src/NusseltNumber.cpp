
#include <memory>
#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <iostream>

#include "NusseltNumber.hpp"
#include "WCEGases.hpp"
#include "WCECommon.hpp"



namespace Tarcog {

	double CNusseltNumberStrategy::pos( double const t_Value ) {
		return ( t_Value + std::abs( t_Value ) ) / 2;
	}

	double CNusseltNumberStrategy::calculate( double const, double const, double const ) {
		return 0;
	}

	double CNusseltNumber0To60::calculate( double const t_Tilt, double const t_Ra, double const ) {
		try {
			if ( t_Ra > 1e5 ) {
				throw std::runtime_error( "Rayleigh number out of range in Nusselt num. calc. for gaps (angle between 0 and 60 deg)." );
			}
		}
		catch ( std::exception& e ) {
			std::cout << e.what() << std::endl;
		}

		double subNu1 = 1 - 1708 / ( t_Ra * cos( t_Tilt ) );
		subNu1 = pos( subNu1 );
		double subNu2 = 1 - ( 1708 * pow( sin( 1.8 * t_Tilt ), 1.6 ) ) / ( t_Ra * cos( t_Tilt ) );
		double subNu3 = pow( t_Ra * cos( t_Tilt ) / 5830, 1 / 3.0 ) - 1;
		subNu3 = pos( subNu3 );
		double gnu = 1 + 1.44 * subNu1 * subNu2 + subNu3; // equation 42

		return gnu;
	}

	double CNusseltNumber60::calculate( double const, double const t_Ra, double const t_Asp ) {
		double G = 0.5 / pow( 1 + pow( t_Ra / 3160, 20.6 ), 0.1 ); // equation 47
		double Nu1 = pow( 1 + pow( 0.0936 * pow( t_Ra, 0.314 ) / ( 1 + G ), 7 ), 0.1428571 ); // equation 45
		double Nu2 = ( 0.104 + 0.175 / t_Asp ) * pow( t_Ra, 0.283 ); // equation 46
		double gnu = std::max( Nu1, Nu2 ); // equation 44

		return gnu;
	}

	double CNusseltNumber60To90::calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) {
		using ConstantsData::PI;

		std::shared_ptr< CNusseltNumber60 > nusselt60 = std::make_shared< CNusseltNumber60 >();
		double Nu60 = nusselt60->calculate( t_Tilt, t_Ra, t_Asp );
		std::shared_ptr< CNusseltNumber90 > nusselt90 = std::make_shared< CNusseltNumber90 >();
		double Nu90 = nusselt90->calculate( t_Tilt, t_Ra, t_Asp );

		// linear interpolation between 60 and 90 degrees
		double gnu = ( ( Nu90 - Nu60 ) / ( 90.0 - 60.0 ) ) * ( t_Tilt * 180 / PI - 60.0 ) + Nu60;

		return gnu;
	}

	double CNusseltNumber90::calculate( double const, double const t_Ra, double const t_Asp ) {
		double Nu1 = 0;
		double Nu2 = 0.242 * pow( t_Ra / t_Asp, 0.272 ); // equation 52
		if ( t_Ra > 5e4 ) {
			Nu1 = 0.0673838 * pow( t_Ra, 1 / 3.0 ); // equation 49
		}
		else if ( ( t_Ra > 1e4 ) && ( t_Ra < 5e4 ) ) {
			Nu1 = 0.028154 * pow( t_Ra, 0.4134 ); // equation 50
		}
		else if ( t_Ra < 1e4 ) {
			Nu1 = 1 + 1.7596678e-10 * pow( t_Ra, 2.2984755 ); // equation 51
		}
		double gnu = std::max( Nu1, Nu2 ); // equation 48

		return gnu;
	}

	double CNusseltNumber90to180::calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) {
		std::shared_ptr< CNusseltNumber90 > nusselt90 = std::make_shared< CNusseltNumber90 >();
		double Nu90 = nusselt90->calculate( t_Tilt, t_Ra, t_Asp );
		double gnu = 1 + ( Nu90 - 1 ) * sin( t_Tilt ); // equation 53

		return gnu;
	}

	double CNusseltNumber::calculate( double const t_Tilt, double const t_Ra, double const t_Asp ) {
		using ConstantsData::PI;

		double tiltRadians = t_Tilt * PI / 180;
		std::shared_ptr< CNusseltNumberStrategy > nusseltNumber;

		if ( t_Tilt >= 0 && t_Tilt < 60 ) {
			nusseltNumber = std::make_shared< CNusseltNumber0To60 >();
		}
		else if ( t_Tilt == 60 ) {
			nusseltNumber = std::make_shared< CNusseltNumber60 >();
		}
		else if ( t_Tilt > 60 && t_Tilt < 90 ) {
			nusseltNumber = std::make_shared< CNusseltNumber60To90 >();
		}
		else if ( t_Tilt == 90 ) {
			nusseltNumber = std::make_shared< CNusseltNumber90 >();
		}
		else if ( t_Tilt > 90 && t_Tilt <= 180 ) {
			nusseltNumber = std::make_shared< CNusseltNumber90to180 >();
		}
		else {
			std::runtime_error( "Window tilt angle is out of range." );
		}

		return nusseltNumber->calculate( tiltRadians, t_Ra, t_Asp );
	}

}
