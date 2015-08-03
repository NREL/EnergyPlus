#ifndef DElightManagerF_hh_INCLUDED
#define DElightManagerF_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DElightManagerF {

	void
	DElightInputGenerator();

	void
	GenerateDElightDaylightCoefficients(
		Real64 & dLatitude,
		int & iErrorFlag
	);

	void
	CheckForGeometricTransform(
		bool & doTransform,
		Real64 & OldAspectRatio,
		Real64 & NewAspectRatio
	);

	void
	SetupDElightOutput4EPlus();

	std::string
	ReplaceBlanksWithUnderscores(
		std::string const & InputString
	);

	void
	DElightElecLtgCtrl(
		int iNameLength,
		std::string cZoneName,
		Real64 dBldgLat,
		Real64 dHISKF,
		Real64 dHISUNF,
		Real64 dCloudFraction,
		Real64 dSOLCOSX,
		Real64 dSOLCOSY,
		Real64 dSOLCOSZ,
		Real64 & pdPowerReducFac,
		int piErrorFlag
	);

	std::vector< char >
	getCharArrayFromString( std::string const & originalString );

	std::string
	getStringFromCharArray( std::vector< char > const & originalCharArray );

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

} // DELIGHTMANAGERF

} // EnergyPlus

#endif
