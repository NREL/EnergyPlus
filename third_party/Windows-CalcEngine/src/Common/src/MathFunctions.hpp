#ifndef MATHFUNCTIONS_H
#define MATHFUNCTIONS_H

#define _USE_MATH_DEFINES
#include <math.h>

namespace FenestrationCommon {

	inline double radians( const double d ) {
		return d * M_PI / 180;
	}

	inline double degrees( const double r ) {
		return r * 180 / M_PI;
	}

}

#endif
