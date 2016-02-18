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

#ifndef HVACSingleDuctInduc_hh_INCLUDED
#define HVACSingleDuctInduc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACSingleDuctInduc {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const SingleDuct_CV_FourPipeInduc;
	extern int const SingleDuct_CV_2PipeInduc;
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumIndUnits;
	extern int NumFourPipes;
	extern Array1D_bool CheckEquipName;
	extern bool GetIUInputFlag; // First time, input is "gotten"

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACSingleDuctInduc:

	// PRIVATE UpdateIndUnit
	// PRIVATE ReportIndUnit

	// Types

	struct IndUnitData
	{
		// Members
		// input data
		std::string Name; // name of unit
		std::string UnitType; // type of unit
		int UnitType_Num; // index to type of unit
		std::string Sched; // availability schedule
		int SchedPtr; // index to schedule
		Real64 MaxTotAirVolFlow; // m3/s (autosizable)
		Real64 MaxTotAirMassFlow; // kg/s
		Real64 InducRatio; // ratio of induced air flow to primary air flow
		int PriAirInNode; // unit primary air inlet node number
		int SecAirInNode; // unit induced air inlet node number
		int OutAirNode; // unit air outlet node number
		int HWControlNode; // hot water control node
		int CWControlNode; // cold water control node
		std::string HCoilType; // type of heating coil component
		std::string HCoil; // name of heating coil component
		int HCoil_Num; // index to this coil
		int HCoil_PlantTypeNum;
		Real64 MaxVolHotWaterFlow; // m3/s (autosizable)
		Real64 MaxHotWaterFlow; // kg/s
		Real64 MinVolHotWaterFlow; // m3/s
		Real64 MinHotWaterFlow; // kg/s
		Real64 HotControlOffset; // control tolerance
		int HWLoopNum; // index for plant loop with hot water coil
		int HWLoopSide; // index for plant loop side for hot water coil
		int HWBranchNum; // index for plant branch for hot water coil
		int HWCompNum; // index for plant component for hot water coil
		int HWCoilFailNum1; // index for errors
		int HWCoilFailNum2; // index for errors
		std::string CCoilType; // type of cooling coil component
		std::string CCoil; // name of cooling coil component
		int CCoil_Num; // index to this coil
		int CCoil_PlantTypeNum;
		Real64 MaxVolColdWaterFlow; // m3/s (autosizable)
		Real64 MaxColdWaterFlow; // kg/s
		Real64 MinVolColdWaterFlow; // m3/s
		Real64 MinColdWaterFlow; // kg/s
		Real64 ColdControlOffset; // control tolerance
		int CWLoopNum; // index for plant loop with chilled water coil
		int CWLoopSide; // index for plant loop side for chilled water coil
		int CWBranchNum; // index for plant branch for chilled water coil
		int CWCompNum; // index for plant component for chilled water coil
		int CWCoilFailNum1; // index for errors
		int CWCoilFailNum2; // index for errors
		std::string MixerName; // name of air mixer component
		int Mixer_Num; // index to this mixer
		Real64 MaxPriAirMassFlow; // kg/s
		Real64 MaxSecAirMassFlow; // kg/s
		int ADUNum; // index of corresponding air distribution unit
		Real64 DesCoolingLoad; // used for reporting during coil sizing
		Real64 DesHeatingLoad; // used for reporting during coil sizing

		// Default Constructor
		IndUnitData() :
			UnitType_Num( 0 ),
			SchedPtr( 0 ),
			MaxTotAirVolFlow( 0.0 ),
			MaxTotAirMassFlow( 0.0 ),
			InducRatio( 2.5 ),
			PriAirInNode( 0 ),
			SecAirInNode( 0 ),
			OutAirNode( 0 ),
			HWControlNode( 0 ),
			CWControlNode( 0 ),
			HCoil_Num( 0 ),
			HCoil_PlantTypeNum( 0 ),
			MaxVolHotWaterFlow( 0.0 ),
			MaxHotWaterFlow( 0.0 ),
			MinVolHotWaterFlow( 0.0 ),
			MinHotWaterFlow( 0.0 ),
			HotControlOffset( 0.0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			HWCoilFailNum1( 0 ),
			HWCoilFailNum2( 0 ),
			CCoil_Num( 0 ),
			CCoil_PlantTypeNum( 0 ),
			MaxVolColdWaterFlow( 0.0 ),
			MaxColdWaterFlow( 0.0 ),
			MinVolColdWaterFlow( 0.0 ),
			MinColdWaterFlow( 0.0 ),
			ColdControlOffset( 0.0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CWCoilFailNum1( 0 ),
			CWCoilFailNum2( 0 ),
			Mixer_Num( 0 ),
			MaxPriAirMassFlow( 0.0 ),
			MaxSecAirMassFlow( 0.0 ),
			ADUNum( 0 ),
			DesCoolingLoad( 0.0 ),
			DesHeatingLoad( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< IndUnitData > IndUnit;

	// Functions

	void
	SimIndUnit(
		std::string const & CompName, // name of the terminal unit
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the terminal unit
		int const ZoneNodeNum, // zone node number of zone served by the terminal unit
		int & CompIndex // which terminal unit in data structure
	);

	void
	GetIndUnits();

	void
	InitIndUnit(
		int const IUNum, // number of the current induction unit being simulated
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	);

	void
	SizeIndUnit( int const IUNum );

	void
	SimFourPipeIndUnit(
		int const IUNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	CalcFourPipeIndUnit(
		int const IUNum, // Unit index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const ZoneNode, // zone node number
		Real64 const HWFlow, // hot water flow (kg/s)
		Real64 const CWFlow, // cold water flow (kg/s)
		Real64 & LoadMet // load met by unit (watts)
	);

	Real64
	FourPipeIUHeatingResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested zone load
	);

	Real64
	FourPipeIUCoolingResidual(
		Real64 const CWFlow, // cold water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested zone load
	);

	// ========================= Utilities =======================

	bool
	FourPipeInductionUnitHasMixer( std::string const & CompName ); // component (mixer) name

} // HVACSingleDuctInduc

} // EnergyPlus

#endif
