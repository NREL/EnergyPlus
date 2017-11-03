#ifndef MATHFUNCTIONS_H
#define MATHFUNCTIONS_H

#include <cmath>

namespace FenestrationCommon {

	static const double PI = 4.0 * std::atan(1.0);

	inline double radians( const double d ) {
		return d * PI / 180;
	}

	inline double degrees( const double r ) {
		return r * 180 / PI;
	}

}

#endif
