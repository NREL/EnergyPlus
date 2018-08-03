#ifndef POLARPOINT2D_H
#define POLARPOINT2D_H

#include "Point2D.hpp"

namespace Viewer {

	// Polar point is used to convert polar to cartesian cooridnate system and vice versa. Theta angle is measured
	// counter clockwise from x-axis
	class CPolarPoint2D : public CPoint2D {
	public:
		CPolarPoint2D( double const t_Theta, double const t_Radius );

		double theta() const;
		double radius() const;

		// Stores cartesian coordinates that are immediately converted into polar
		void setCartesian( double const x, double const y );

	protected:
		double m_Theta;
		double m_Radius;

	private:

		// Calculate polar coordinates from current cartesian coordinates stored in the object
		void calculatePolarCoordinates();

	};

}

#endif
