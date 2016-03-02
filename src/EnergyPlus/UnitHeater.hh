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

#ifndef UnitHeater_hh_INCLUDED
#define UnitHeater_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace UnitHeater {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern std::string const cMO_UnitHeater;

	// Character parameters for outside air control types:
	extern std::string const ElectricCoil;
	extern std::string const GasCoil;
	extern std::string const WaterCoil;
	extern std::string const SteamCoil;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool HCoilOn; // TRUE if the heating coil (gas or electric especially) should be running
	extern int NumOfUnitHeats; // Number of unit heaters in the input file
	extern Real64 QZnReq; // heating or cooling needed by zone [watts]
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE UnitHeater

	// Types

	struct UnitHeaterData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		int AirInNode; // inlet air node number
		int AirOutNode; // outlet air node number
		int FanType_Num; // Fan type number (see DataHVACGlobals)
		std::string FanType; // type of fan
		std::string FanName; // name of fan
		int Fan_Index;
		int FanSchedPtr; // index to fan operating mode schedule
		int FanAvailSchedPtr; // index to fan availability schedule
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 MaxAirVolFlow; // m3/s
		Real64 MaxAirMassFlow; // kg/s
		std::string FanOperatesDuringNoHeating; // Indicates whether fan operates or not during no heating
		int FanOutletNode; // outlet node number for fan exit
		// (assumes fan is upstream of heating coil)
		int OpMode; // mode of operation; 1=cycling fan, cycling coil, 2=continuous fan, cycling coil
		std::string HCoilType; // type of heating coil (water, gas, electric, etc.)
		std::string HCoilTypeCh; // actual object name
		std::string HCoilName; // name of heating coil
		int HCoil_Index;
		int HCoil_PlantTypeNum;
		int HCoil_FluidIndex;
		Real64 MaxVolHotWaterFlow; // m3/s
		Real64 MaxVolHotSteamFlow; // m3/s
		Real64 MaxHotWaterFlow; // kg/s
		Real64 MaxHotSteamFlow; // m3/s
		Real64 MinVolHotWaterFlow; // m3/s
		Real64 MinVolHotSteamFlow; // m3/s
		Real64 MinHotWaterFlow; // kg/s
		Real64 MinHotSteamFlow; // kg/s
		int HotControlNode; // hot water control node, inlet of coil
		Real64 HotControlOffset; // control tolerance
		int HotCoilOutNodeNum; // outlet of coil
		int HWLoopNum; // index for plant loop with hot plant coil
		int HWLoopSide; // index for plant loop side for hot plant coil
		int HWBranchNum; // index for plant branch for hot plant coil
		int HWCompNum; // index for plant component for hot plant coil
		Real64 PartLoadFrac; // part load fraction for the unit
		// Report data
		Real64 HeatPower; // unit heating output in watts
		Real64 HeatEnergy; // unit heating output in J
		Real64 ElecPower;
		Real64 ElecEnergy;
		std::string AvailManagerListName; // Name of an availability manager list object
		int AvailStatus;
		bool FanOffNoHeating; // True when fan is on during no heating load
		Real64 FanPartLoadRatio; // fan part-load ratio for time step
		int ZonePtr; // pointer to a zone served by a unit heater
		int HVACSizingIndex; // index of a HVACSizing object for a unit heater

		// Default Constructor
		UnitHeaterData() :
			SchedPtr( 0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			FanType_Num( 0 ),
			Fan_Index( 0 ),
			FanSchedPtr( 0 ),
			FanAvailSchedPtr( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			FanOutletNode( 0 ),
			OpMode( 0 ),
			HCoil_Index( 0 ),
			HCoil_PlantTypeNum( 0 ),
			HCoil_FluidIndex( 0 ),
			MaxVolHotWaterFlow( 0.0 ),
			MaxVolHotSteamFlow( 0.0 ),
			MaxHotWaterFlow( 0.0 ),
			MaxHotSteamFlow( 0.0 ),
			MinVolHotWaterFlow( 0.0 ),
			MinVolHotSteamFlow( 0.0 ),
			MinHotWaterFlow( 0.0 ),
			MinHotSteamFlow( 0.0 ),
			HotControlNode( 0 ),
			HotControlOffset( 0.0 ),
			HotCoilOutNodeNum( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			PartLoadFrac( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			AvailStatus( 0 ),
			FanOffNoHeating( false ),
			FanPartLoadRatio( 0.0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

	};

	struct UnitHeatNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		UnitHeatNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< UnitHeaterData > UnitHeat;
	extern Array1D< UnitHeatNumericFieldData > UnitHeatNumericFields;

	// Functions

	void
	clear_state();

	void
	SimUnitHeater(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	);

	void
	GetUnitHeaterInput();

	void
	InitUnitHeater(
		int const UnitHeatNum, // index for the current unit heater
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	SizeUnitHeater( int const UnitHeatNum );

	void
	CalcUnitHeater(
		int & UnitHeatNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	CalcUnitHeaterComponents(
		int const UnitHeatNum, // Unit index in unit heater array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet, // load met by unit (watts)
		Optional_int_const OpMode = _, // fan operating mode
		Optional< Real64 const > PartLoadRatio = _ // part-load ratio
	);

	//SUBROUTINE UpdateUnitHeater

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateUnitHeater

	void
	ReportUnitHeater( int const UnitHeatNum ); // Unit index in unit heater array

	Real64
	CalcUnitHeaterResidual(
		Real64 const PartLoadRatio, // heating coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	);

} // UnitHeater

} // EnergyPlus

#endif
