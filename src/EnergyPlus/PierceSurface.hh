#ifndef PierceSurface_hh_INCLUDED
#define PierceSurface_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Vector3.fwd.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

void
PierceSurface(
	int const ISurf, // Surface index
	Vector3< Real64 > const & R1, // Point from which ray originates
	Vector3< Real64 > const & RN, // Unit vector along in direction of ray whose
	int & IPIERC, // =1 if line through point R1 in direction of unit vector
	Vector3< Real64 > & CPhit // Point that ray along RN intersects plane of surface
);

} // EnergyPlus

#endif
