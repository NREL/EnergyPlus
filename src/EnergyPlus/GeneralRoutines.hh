#ifndef GeneralRoutines_hh_INCLUDED
#define GeneralRoutines_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

void
ControlCompOutput(
	std::string const & CompName, // the component Name
	std::string const & CompType, // Type of component
	int & CompNum, // Index of component in component array
	bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
	Real64 const QZnReq, // zone load to be met
	int const ActuatedNode, // node that controls unit output
	Real64 const MaxFlow, // maximum water flow
	Real64 const MinFlow, // minimum water flow
	Real64 const ControlOffset, // really the tolerance
	int & ControlCompTypeNum, // Internal type num for CompType
	int & CompErrIndex, // for Recurring error call
	Optional_int_const TempInNode = _, // inlet node for output calculation
	Optional_int_const TempOutNode = _, // outlet node for output calculation
	Optional< Real64 const > AirMassFlow = _, // air mass flow rate
	Optional_int_const Action = _, // 1=reverse; 2=normal
	Optional_int_const EquipIndex = _, // Identifier for equipment of Outdoor Air Unit "ONLY"
	Optional_int_const LoopNum = _, // for plant components, plant loop index
	Optional_int_const LoopSide = _, // for plant components, plant loop side index
	Optional_int_const BranchIndex = _, // for plant components, plant branch index
	Optional_int_const ControlledZoneIndex = _ // controlled zone index for the zone containing the component
);

void
CheckSysSizing(
	std::string const & CompType, // Component Type (e.g. Chiller:Electric)
	std::string const & CompName // Component Name (e.g. Big Chiller)
);

void
CheckThisAirSystemForSizing(
	int const AirLoopNum,
	bool & AirLoopWasSized
);

void
CheckZoneSizing(
	std::string const & CompType, // Component Type (e.g. Chiller:Electric)
	std::string const & CompName // Component Name (e.g. Big Chiller)
);

void
CheckThisZoneForSizing(
	int const ZoneNum, // zone index to be checked
	bool & ZoneWasSized
);

void
ValidateComponent(
	std::string const & CompType, // Component Type (e.g. Chiller:Electric)
	std::string const & CompName, // Component Name (e.g. Big Chiller)
	bool & IsNotOK, // .TRUE. if this component pair is invalid
	std::string const & CallString // Context of this pair -- for error message
);

void
CalcPassiveExteriorBaffleGap(
	Array1S_int const SurfPtrARR, // Array of indexes pointing to Surface structure in DataSurfaces
	Real64 const VentArea, // Area available for venting the gap [m2]
	Real64 const Cv, // Oriface coefficient for volume-based discharge, wind-driven [--]
	Real64 const Cd, // oriface coefficient for discharge,  bouyancy-driven [--]
	Real64 const HdeltaNPL, // Height difference from neutral pressure level [m]
	Real64 const SolAbs, // solar absorptivity of baffle [--]
	Real64 const AbsExt, // thermal absorptance/emittance of baffle material [--]
	Real64 const Tilt, // Tilt of gap [Degrees]
	Real64 const AspRat, // aspect ratio of gap  Height/gap [--]
	Real64 const GapThick, // Thickness of air space between baffle and underlying heat transfer surface
	int const Roughness, // Roughness index (1-6), see DataHeatBalance parameters
	Real64 const QdotSource, // Source/sink term, e.g. electricity exported from solar cell [W]
	Real64 & TsBaffle, // Temperature of baffle (both sides) use lagged value on input [C]
	Real64 & TaGap, // Temperature of air gap (assumed mixed) use lagged value on input [C]
	Optional< Real64 > HcGapRpt = _,
	Optional< Real64 > HrGapRpt = _,
	Optional< Real64 > IscRpt = _,
	Optional< Real64 > MdotVentRpt = _,
	Optional< Real64 > VdotWindRpt = _,
	Optional< Real64 > VdotBouyRpt = _
);

//****************************************************************************

void
PassiveGapNusseltNumber(
	Real64 const AspRat, // Aspect Ratio of Gap height to gap width
	Real64 const Tilt, // Tilt of gap, degrees
	Real64 const Tso, // Temperature of gap surface closest to outside (K)
	Real64 const Tsi, // Temperature of gap surface closest to zone (K)
	Real64 const Gr, // Gap gas Grashof number
	Real64 & gNu // Gap gas Nusselt number
);

void
CalcBasinHeaterPower(
	Real64 const Capacity, // Basin heater capacity per degree C below setpoint (W/C)
	int const SchedulePtr, // Pointer to basin heater schedule
	Real64 const SetPointTemp, // setpoint temperature for basin heater operation (C)
	Real64 & Power // Basin heater power (W)
);

void
TestAirPathIntegrity( bool & ErrFound );

void
TestSupplyAirPathIntegrity( bool & ErrFound );

void
TestReturnAirPathIntegrity(
	bool & ErrFound,
	Array2S_int ValRetAPaths
);

//     NOTICE
//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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
