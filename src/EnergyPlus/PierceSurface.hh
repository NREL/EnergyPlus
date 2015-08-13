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

//     NOTICE

//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
//     and The Regents of the University of California through Ernest Orlando Lawrence
//     Berkeley National Laboratory.  All rights reserved.

//     Portions of the EnergyPlus software package have been developed and copyrighted
//     by other individuals, companies and institutions.  These portions have been
//     incorporated into the EnergyPlus software package under license.   For a complete
//     list of contributors, see "Notice" located in main.cc.

//     NOTICE: The U.S. Government is granted for itself and others acting on its
//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
//     reproduce, prepare derivative works, and perform publicly and display publicly.
//     Beginning five (5) years after permission to assert copyright is granted,
//     subject to two possible five year renewals, the U.S. Government is granted for
//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
//     worldwide license in this data to reproduce, prepare derivative works,
//     distribute copies to the public, perform publicly and display publicly, and to
//     permit others to do so.

//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // EnergyPlus

#endif
