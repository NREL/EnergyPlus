#ifndef VIEWERCONSTANTS_H
#define VIEWERCONSTANTS_H

namespace Viewer {

	namespace ViewerConstants {
		// Minimum distance for which viewr will consider the point to be the different. If two points are created
		// within smaller distance, viwer will consider this to be the same point
		const double DISTANCE_TOLERANCE = 1e-6;

		// Number of subdivision segments used to calculate view factor when there is a blocking surface
		const int NUM_OF_SEGMENTS = 10;

		// Minimum value for view factor coefficient
		const double MIN_VIEW_COEFF = 1e-8;
	}

}

#endif
