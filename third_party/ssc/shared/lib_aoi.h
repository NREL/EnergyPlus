#ifndef __LIB_AOI_H__
#define __LIB_AOI_H__

#include "lib_pv_io_manager.h"

class AngleOfIncidenceModifier
{
public:

	/// Constructor for AngleOfIncidenceModifier model
	AngleOfIncidenceModifier(PVIOManager * IOManager);

	/// Calculate the irradiance through the cover
	calculateIrradianceThroughFrontCover(double angleOfIncidenceDegrees, double zenithAngleDegrees, double tiltDegrees, double poaBeam, double poaDiffuse, double poaGround);
};

#endif
