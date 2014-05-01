#ifndef HeatBalanceInternalHeatGains_hh_INCLUDED
#define HeatBalanceInternalHeatGains_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

void
SetupZoneInternalGain(
	int const ZoneNum,
	std::string const & cComponentObject, // object class name for device contributing internal gain
	std::string const & cComponentName, // user unique name for device
	int const IntGainComp_TypeOfNum,
	Optional< Real64 > ConvectionGainRate = _, // pointer target for remote convection gain value to be accessed
	Optional< Real64 > ReturnAirConvectionGainRate = _,
	Optional< Real64 > ThermalRadiationGainRate = _, // pointer target for remote IR radiation gain value to be accessed
	Optional< Real64 > LatentGainRate = _,
	Optional< Real64 > ReturnAirLatentGainRate = _,
	Optional< Real64 > CarbonDioxideGainRate = _,
	Optional< Real64 > GenericContamGainRate = _
);

} // EnergyPlus

#endif
