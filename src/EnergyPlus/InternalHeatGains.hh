#ifndef InternalHeatGains_hh_INCLUDED
#define InternalHeatGains_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace InternalHeatGains {

	// Data
	// MODULE PARAMETER DEFINITIONS:

	extern bool GetInternalHeatGainsInputFlag; // Controls the GET routine calling (limited to first time)

	// SUBROUTINE SPECIFICATIONS FOR MODULE InternalHeatGains
	//PUBLIC  SumInternalConvectionGainsByIndices
	//PUBLIC SumReturnAirConvectionGainsByIndices
	//PUBLIC  SumInternalRadiationGainsByIndices
	//PUBLIC  SumInternalLatentGainsByIndices
	//PUBLIC
	//PUBLIC  SumInternalCO2GainsByIndices
	//PUBLIC  GetInternalGainDeviceIndex

	// Functions

	void
	ManageInternalHeatGains( Optional_bool_const InitOnly = _ ); // when true, just calls the get input, if appropriate and returns.

	void
	GetInternalHeatGainsInput();

	void
	InitInternalHeatGains();

	void
	CalcZoneITEq();

	void
	ReportInternalHeatGains();

	Real64
	GetDesignLightingLevelForZone( int const WhichZone ); // name of zone

	void
	CheckLightsReplaceableMinMaxForZone( int const WhichZone ); // Zone Number

	void
	UpdateInternalGainValues(
		Optional_bool_const SuppressRadiationUpdate = _,
		Optional_bool_const SumLatentGains = _
	);

	void
	SumAllInternalConvectionGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumConvGainRate
	);

	void
	SumInternalConvectionGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumConvGainRate
	);

	void
	SumAllReturnAirConvectionGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumReturnAirGainRate
	);

	void
	SumReturnAirConvectionGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumReturnAirGainRate
	);

	void
	SumAllInternalRadiationGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumRadGainRate
	);

	void
	SumInternalRadiationGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumRadiationGainRate
	);

	void
	SumAllInternalLatentGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumLatentGainRate
	);

	void
	SumInternalLatentGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumLatentGainRate
	);

	void
	SumAllReturnAirLatentGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumRetAirLatentGainRate
	);

	void
	SumAllInternalCO2Gains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumCO2GainRate
	);

	void
	SumInternalCO2GainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumCO2GainRate
	);

	void
	SumAllInternalGenericContamGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumGCGainRate
	);

	void
	GatherComponentLoadsIntGain();

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

} // InternalHeatGains

} // EnergyPlus

#endif
