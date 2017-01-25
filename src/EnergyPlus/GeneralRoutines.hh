// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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


} // EnergyPlus

#endif
