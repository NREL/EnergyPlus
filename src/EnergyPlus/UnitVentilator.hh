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

#ifndef UnitVentilator_hh_INCLUDED
#define UnitVentilator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace UnitVentilator {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// Currrent Module Unit type
	extern std::string const cMO_UnitVentilator;

	// Parameters for outside air control types:
	extern int const Heating_ElectricCoilType;
	extern int const Heating_GasCoilType;
	extern int const Heating_WaterCoilType;
	extern int const Heating_SteamCoilType;
	extern int const Cooling_CoilWaterCooling;
	extern int const Cooling_CoilDetailedCooling;
	extern int const Cooling_CoilHXAssisted;
	// OA operation modes
	extern int const VariablePercent;
	extern int const FixedTemperature;
	extern int const FixedOAControl;
	// coil operation
	extern int const On; // normal coil operation
	extern int const Off; // signal coil shouldn't run
	extern int const NoneOption;
	extern int const BothOption;
	extern int const HeatingOption;
	extern int const CoolingOption;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool HCoilOn; // TRUE if the heating coil (gas or electric especially) should be running
	extern int NumOfUnitVents; // Number of unit ventilators in the input file
	extern Real64 OAMassFlowRate; // Outside air mass flow rate for the unit ventilator
	extern Real64 QZnReq; // heating or cooling needed by zone [watts]
	extern Array1D_bool MySizeFlag;
	extern bool GetUnitVentilatorInputFlag; // First time, input is "gotten"
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE UnitVentilator
	//PRIVATE UpdateUnitVentilator

	// Types

	struct UnitVentilatorData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		int AirInNode; // inlet air node number
		int AirOutNode; // outlet air node number
		int FanOutletNode; // outlet node number for fan exit
		// (assumes fan is upstream of heating coil)
		int FanType_Num; // Fan type number (see DataHVACGlobals)
		std::string FanType; // type of fan
		std::string FanName; // name of fan
		int Fan_Index;
		int FanSchedPtr; // index to fan operating mode schedule
		int FanAvailSchedPtr; // index to fan availability schedule
		int OpMode; // mode of operation; 1=cycling fan, cycling coil, 2=continuous fan, cycling coil
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 MaxAirVolFlow; // m3/s
		Real64 MaxAirMassFlow; // kg/s
		int OAControlType; // type of control; options are VARIABLE PERCENT and FIXED TEMPERATURE
		std::string MinOASchedName; // schedule of fraction for minimum outside air (all controls)
		int MinOASchedPtr; // index to schedule
		std::string MaxOASchedName; // schedule of percentages for maximum outside air fraction (variable %)
		int MaxOASchedPtr; // index to schedule
		std::string TempSchedName; // schedule of temperatures for desired "mixed air"
		// temperature (fixed temp.)
		int TempSchedPtr; // index to schedule
		int OutsideAirNode; // outside air node number
		int AirReliefNode; // relief air node number
		int OAMixerOutNode; // outlet node after the outside air mixer (inlet to coils if present)
		Real64 OutAirVolFlow; // m3/s
		Real64 OutAirMassFlow; // kg/s
		Real64 MinOutAirVolFlow; // m3/s
		Real64 MinOutAirMassFlow; // kg/s
		int CoilOption; // type of coil option; options are BOTH, HEATING, COOLING, AND NONE
		bool HCoilPresent; // .TRUE. if unit ventilator has a heating coil
		int HCoilType; // type of heating coil (water, gas, electric, etc.)
		std::string HCoilName; // name of heating coil
		std::string HCoilTypeCh; // type of heating coil character string (same as type on idf file).
		int HCoil_Index;
		int HCoil_PlantTypeNum;
		int HCoil_FluidIndex;
		std::string HCoilSchedName; // availability schedule for the heating coil
		int HCoilSchedPtr; // index to schedule
		Real64 HCoilSchedValue;
		Real64 MaxVolHotWaterFlow; // m3/s
		Real64 MaxVolHotSteamFlow; // m3/s
		Real64 MaxHotWaterFlow; // kg/s
		Real64 MaxHotSteamFlow;
		Real64 MinHotSteamFlow;
		Real64 MinVolHotWaterFlow; // m3/s
		Real64 MinVolHotSteamFlow; // m3/s
		Real64 MinHotWaterFlow; // kg/s
		int HotControlNode; // hot water control node
		int HotCoilOutNodeNum; // outlet of coil
		Real64 HotControlOffset; // control tolerance
		int HWLoopNum; // index for plant loop with hot water coil
		int HWLoopSide; // index for plant loop side for hot water coil
		int HWBranchNum; // index for plant branch for hot water coil
		int HWCompNum; // index for plant component for hot water coil
		bool CCoilPresent; // .TRUE. if unit ventilator has a cooling coil
		std::string CCoilName; // name of cooling coil
		std::string CCoilTypeCh; // type of cooling coil as character string (same as on idf file)
		int CCoil_Index;
		std::string CCoilPlantName; // name of cooling coil for plant
		std::string CCoilPlantType; // type of cooling coil for plant
		int CCoil_PlantTypeNum;
		int CCoilType; // type of cooling coil:
		// 'Coil:Cooling:Water:DetailedGeometry' or
		// 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
		std::string CCoilSchedName; // availability schedule for the cooling coil
		int CCoilSchedPtr; // index to schedule
		Real64 CCoilSchedValue;
		Real64 MaxVolColdWaterFlow; // m3/s
		Real64 MaxColdWaterFlow; // kg/s
		Real64 MinVolColdWaterFlow; // m3/s
		Real64 MinColdWaterFlow; // kg/s
		int ColdControlNode; // chilled water control node
		int ColdCoilOutNodeNum; // chilled water coil out node
		Real64 ColdControlOffset; // control tolerance
		int CWLoopNum; // index for plant loop with chilled water coil
		int CWLoopSide; // index for plant loop side for chilled water coil
		int CWBranchNum; // index for plant branch for chilled water coil
		int CWCompNum; // index for plant component for chilled water coil
		// Report data
		Real64 HeatPower; // unit heating output in watts
		Real64 HeatEnergy; // unit heating output in J
		Real64 TotCoolPower;
		Real64 TotCoolEnergy;
		Real64 SensCoolPower;
		Real64 SensCoolEnergy;
		Real64 ElecPower;
		Real64 ElecEnergy;
		std::string AvailManagerListName; // Name of an availability manager list object
		int AvailStatus;
		Real64 FanPartLoadRatio; // fan part-load ratio for time step
		Real64 PartLoadFrac; // unit ventilator part-load ratio for time step
		int ZonePtr; // pointer to a zone served by a unit ventilator
		int HVACSizingIndex; // index of a HVACSizing object for a unit ventilator
		// for unit ventilator object

		// Default Constructor
		UnitVentilatorData() :
			SchedPtr( 0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			FanOutletNode( 0 ),
			FanType_Num( 0 ),
			Fan_Index( 0 ),
			FanSchedPtr( 0 ),
			FanAvailSchedPtr( 0 ),
			OpMode( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			OAControlType( 0 ),
			MinOASchedPtr( 0 ),
			MaxOASchedPtr( 0 ),
			TempSchedPtr( 0 ),
			OutsideAirNode( 0 ),
			AirReliefNode( 0 ),
			OAMixerOutNode( 0 ),
			OutAirVolFlow( 0.0 ),
			OutAirMassFlow( 0.0 ),
			MinOutAirVolFlow( 0.0 ),
			MinOutAirMassFlow( 0.0 ),
			CoilOption( 0 ),
			HCoilPresent( false ),
			HCoilType( 0 ),
			HCoil_Index( 0 ),
			HCoil_PlantTypeNum( 0 ),
			HCoil_FluidIndex( 0 ),
			HCoilSchedPtr( 0 ),
			HCoilSchedValue( 0.0 ),
			MaxVolHotWaterFlow( 0.0 ),
			MaxVolHotSteamFlow( 0.0 ),
			MaxHotWaterFlow( 0.0 ),
			MaxHotSteamFlow( 0.0 ),
			MinHotSteamFlow( 0.0 ),
			MinVolHotWaterFlow( 0.0 ),
			MinVolHotSteamFlow( 0.0 ),
			MinHotWaterFlow( 0.0 ),
			HotControlNode( 0 ),
			HotCoilOutNodeNum( 0 ),
			HotControlOffset( 0.0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			CCoilPresent( false ),
			CCoil_Index( 0 ),
			CCoil_PlantTypeNum( 0 ),
			CCoilType( 0 ),
			CCoilSchedPtr( 0 ),
			CCoilSchedValue( 0.0 ),
			MaxVolColdWaterFlow( 0.0 ),
			MaxColdWaterFlow( 0.0 ),
			MinVolColdWaterFlow( 0.0 ),
			MinColdWaterFlow( 0.0 ),
			ColdControlNode( 0 ),
			ColdCoilOutNodeNum( 0 ),
			ColdControlOffset( 0.0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			TotCoolPower( 0.0 ),
			TotCoolEnergy( 0.0 ),
			SensCoolPower( 0.0 ),
			SensCoolEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			AvailStatus( 0 ),
			FanPartLoadRatio( 0.0 ),
			PartLoadFrac( 0.0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

	};

	struct UnitVentNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		UnitVentNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< UnitVentilatorData > UnitVent;
	extern Array1D< UnitVentNumericFieldData > UnitVentNumericFields;

	// Functions

	void
	clear_state();

	void
	SimUnitVentilator(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	);

	void
	GetUnitVentilatorInput();

	void
	InitUnitVentilator(
		int const UnitVentNum, // index for the current unit ventilator
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const ZoneNum // number of zone being served
	);

	void
	SizeUnitVentilator( int const UnitVentNum );

	void
	CalcUnitVentilator(
		int & UnitVentNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	CalcUnitVentilatorComponents(
		int const UnitVentNum, // Unit index in unit ventilator array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet, // load met by unit (watts)
		Optional_int_const OpMode = _, // Fan Type
		Optional< Real64 const > PartLoadFrac = _ // Part Load Ratio of coil and fan
	);

	void
	SimUnitVentOAMixer(
		int const UnitVentNum, // Unit index in unit ventilator array
		int const FanOpMode // unit ventilator fan operating mode
	);

	//SUBROUTINE UpdateUnitVentilator

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateUnitVentilator

	void
	ReportUnitVentilator( int const UnitVentNum ); // Unit index in unit ventilator array

	int
	GetUnitVentilatorOutAirNode( int const UnitVentNum );

	int
	GetUnitVentilatorZoneInletAirNode( int const UnitVentNum );

	int
	GetUnitVentilatorMixedAirNode( int const UnitVentNum );

	int
	GetUnitVentilatorReturnAirNode( int const UnitVentNum );

	Real64
	CalcUnitVentilatorResidual(
		Real64 const PartLoadRatio, // Coil Part Load Ratio
		Array1< Real64 > const & Par // Function parameters
	);

} // UnitVentilator

} // EnergyPlus

#endif
