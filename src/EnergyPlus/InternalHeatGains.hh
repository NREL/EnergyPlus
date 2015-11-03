#ifndef InternalHeatGains_hh_INCLUDED
#define InternalHeatGains_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array1D.hh>
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
	clear_state();

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
	GetInternalGainDeviceIndex(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		int const IntGainTypeOfNum, // zone internal gain type number
		std::string const & IntGainName, // Internal gain name
		int & DeviceIndex, // Device index
		bool & ErrorFound
	);

	void
	SumInternalConvectionGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumConvGainRate
	);

	void
	SumInternalLatentGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumLatentGainRate
	);

	void
	SumReturnAirConvectionGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumReturnAirGainRate
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

} // InternalHeatGains

} // EnergyPlus

#endif
