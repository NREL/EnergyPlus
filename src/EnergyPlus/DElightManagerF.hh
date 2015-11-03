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

} // DELIGHTMANAGERF

} // EnergyPlus

#endif
