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

#ifndef SingleDuct_hh_INCLUDED
#define SingleDuct_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SingleDuct {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const Normal;
	extern int const ReverseAction;
	// SysTypes represented here
	extern int const SingleDuctVAVReheat;
	extern int const SingleDuctConstVolReheat;
	extern int const SingleDuctVAVNoReheat;
	extern int const SingleDuctVAVReheatVSFan;
	extern int const SingleDuctCBVAVReheat;
	extern int const SingleDuctCBVAVNoReheat;
	// Reheat Coil Types used here
	extern int const HCoilType_None;
	extern int const HCoilType_Gas;
	extern int const HCoilType_Electric;
	extern int const HCoilType_SimpleHeating;
	extern int const HCoilType_SteamAirHeating;
	// Fan types used here
	extern int const FanType_None;
	extern int const FanType_VS;
	// Minimum Flow Fraction Input Method
	extern int const ConstantMinFrac;
	extern int const ScheduledMinFrac;
	extern int const FixedMin;
	extern int NumATMixers;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern Array1D< Real64 > MassFlow1; // previous value of the terminal unit mass flow rate
	extern Array1D< Real64 > MassFlow2; // previous value of the previous value of the mass flow rate
	extern Array1D< Real64 > MassFlow3;
	extern Array1D< Real64 > MassFlowDiff;
	extern bool GetInputFlag; // Flag set to make sure you get input once
	extern bool GetATMixerFlag; // Flag set to make sure you get input once
	extern int NumConstVolSys;
	extern Array1D_bool CheckEquipName;

	// INTERFACE BLOCK SPECIFICATIONS

	extern int NumSys; // The Number of Systems found in the Input

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Types

	struct SysDesignParams
	{
		// Members
		std::string SysName; // Name of the Sys
		std::string SysType; // Type of Sys ie. VAV, Mixing, Inducing, etc.
		int SysType_Num; // Numeric Equivalent for System type
		std::string Schedule; // Sys Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		std::string ReheatComp; // Type of the Reheat Coil Object
		int ReheatComp_Num; // Numeric Equivalent in this module for Coil type
		int ReheatComp_Index; // Returned Index number from other routines
		std::string ReheatName; // name of reheat coil
		int ReheatComp_PlantType; // typeOf_ number for plant type of heating coil
		std::string FanType; // Type of the Fan Object
		int Fan_Num; // Numeric Equivalent in this module for fan type
		int Fan_Index; // Returned Index number from other routines
		int ControlCompTypeNum;
		int CompErrIndex;
		std::string FanName; // name of fan
		Real64 MaxAirVolFlowRate; // Max Specified Volume Flow Rate of Sys (cooling max) [m3/sec]
		Real64 AirMassFlowRateMax; // Max Specified Mass Flow Rate of Sys (cooling max) [kg/sec]
		Real64 MaxHeatAirVolFlowRate; // Max specified volume flow rate of unit at max heating [m3/s]
		Real64 HeatAirMassFlowRateMax; // Max Specified Mass Flow Rate of unit at max heating [kg/sec]
		int ZoneMinAirFracMethod; // parameter for what method is used for min flow fraction
		Real64 ZoneMinAirFrac; // Fraction of supply air used as minimum flow
		Real64 ZoneMinAirFracReport; // Fraction of supply air used as minimum flow for reporting (zero if terminal unit flow is zero)
		Real64 ZoneFixedMinAir; // Absolute minimum supply air flow
		int ZoneMinAirFracSchPtr; // pointer to the schedule for min flow fraction
		bool ConstantMinAirFracSetByUser; // record if user left field blank for constant min fraction.
		bool FixedMinAirSetByUser; // record if user left field blank for constant min fraction.
		Real64 DesignMinAirFrac; // store user entered constant min flow fract for design
		Real64 DesignFixedMinAir; // store user entered constant min flow for design
		int InletNodeNum; // terminal unit inlet node number; damper inlet node number
		int OutletNodeNum; // damper outlet node number for VAV; unused by CV; coil air inlet node for VAV
		// fan outlet node, coil inlet node for VAV VS Fan
		int ReheatControlNode; // hot water inlet node for heating coil
		int ReheatCoilOutletNode; // outlet node for heating coil
		Real64 ReheatCoilMaxCapacity; // heating coil capacity, W
		int ReheatAirOutletNode; // terminal unit outlet node; heating coil air outlet node
		Real64 MaxReheatWaterVolFlow; // m3/s
		Real64 MaxReheatSteamVolFlow; // m3/s
		Real64 MaxReheatWaterFlow; // kg/s
		Real64 MaxReheatSteamFlow; // kg/s
		Real64 MinReheatWaterVolFlow; // m3/s
		Real64 MinReheatSteamVolFlow; // m3/s
		Real64 MinReheatWaterFlow; // kg/s
		Real64 MinReheatSteamFlow; // kg/s
		Real64 ControllerOffset;
		Real64 MaxReheatTemp; // C
		bool MaxReheatTempSetByUser;
		int DamperHeatingAction; // ! 1=NORMAL;  2=REVERSE ACTION
		Real64 DamperPosition;
		int ADUNum; // index of corresponding air distribution unit
		int FluidIndex; // Refrigerant index
		int ErrCount1; // iteration limit exceeded in Hot Water Flow Calc
		int ErrCount1c; // iteration limit exceeded in Hot Water Flow Calc - continue
		int ErrCount2; // bad iterations limits in hot water flow calc
		Real64 ZoneFloorArea; // Zone floor area
		int CtrlZoneNum; // Pointer to CtrlZone data structure
		int ActualZoneNum; // Pointer to Zone data Structure
		Real64 MaxAirVolFlowRateDuringReheat; // Maximum vol flow during reheat
		Real64 MaxAirVolFractionDuringReheat; // Maximum vol flow fraction during reheat
		Real64 AirMassFlowDuringReheatMax; // Maximum mass flow during reheat
		int ZoneOutdoorAirMethod; // Outdoor air method
		Real64 OutdoorAirFlowRate; // report variable for TU outdoor air flow rate
		bool NoOAFlowInputFromUser; // avoids OA calculation if no input specified by user
		int OARequirementsPtr; // - Index to DesignSpecification:OutdoorAir object
		int AirLoopNum;
		int HWLoopNum; // plant topology, loop number
		int HWLoopSide; // plant topology, loop side number
		int HWBranchIndex; // plant topology, Branch number
		int HWCompIndex; // plant topology, Component number
		std::string ZoneHVACUnitType; // type of Zone HVAC unit for air terminal mixer units
		std::string ZoneHVACUnitName; // name of Zone HVAC unit for air terminal mixer units
		int SecInNode; // zone or zone unit air node number
		// warning variables
		int IterationLimit; // Used for RegulaFalsi error -1
		int IterationFailed; // Used for RegulaFalsi error -2

		// Default Constructor
		SysDesignParams() :
			SysType_Num( 0 ),
			SchedPtr( 0 ),
			ReheatComp_Num( 0 ),
			ReheatComp_Index( 0 ),
			ReheatComp_PlantType( 0 ),
			Fan_Num( 0 ),
			Fan_Index( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			MaxAirVolFlowRate( 0.0 ),
			AirMassFlowRateMax( 0.0 ),
			MaxHeatAirVolFlowRate( 0.0 ),
			HeatAirMassFlowRateMax( 0.0 ),
			ZoneMinAirFracMethod( ConstantMinFrac ),
			ZoneMinAirFrac( 0.0 ),
			ZoneMinAirFracReport( 0.0 ),
			ZoneFixedMinAir( 0.0 ),
			ZoneMinAirFracSchPtr( 0 ),
			ConstantMinAirFracSetByUser( false ),
			FixedMinAirSetByUser( false ),
			DesignMinAirFrac( 0.0 ),
			DesignFixedMinAir( 0.0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			ReheatControlNode( 0 ),
			ReheatCoilOutletNode( 0 ),
			ReheatCoilMaxCapacity( 0.0 ),
			ReheatAirOutletNode( 0 ),
			MaxReheatWaterVolFlow( 0.0 ),
			MaxReheatSteamVolFlow( 0.0 ),
			MaxReheatWaterFlow( 0.0 ),
			MaxReheatSteamFlow( 0.0 ),
			MinReheatWaterVolFlow( 0.0 ),
			MinReheatSteamVolFlow( 0.0 ),
			MinReheatWaterFlow( 0.0 ),
			MinReheatSteamFlow( 0.0 ),
			ControllerOffset( 0.0 ),
			MaxReheatTemp( 0.0 ),
			MaxReheatTempSetByUser( false ),
			DamperHeatingAction( 0 ),
			DamperPosition( 0.0 ),
			ADUNum( 0 ),
			FluidIndex( 0 ),
			ErrCount1( 0 ),
			ErrCount1c( 0 ),
			ErrCount2( 0 ),
			ZoneFloorArea( 0.0 ),
			CtrlZoneNum( 0 ),
			ActualZoneNum( 0 ),
			MaxAirVolFlowRateDuringReheat( 0.0 ),
			MaxAirVolFractionDuringReheat( 0.0 ),
			AirMassFlowDuringReheatMax( 0.0 ),
			ZoneOutdoorAirMethod( 0 ),
			OutdoorAirFlowRate( 0.0 ),
			NoOAFlowInputFromUser( true ),
			OARequirementsPtr( 0 ),
			AirLoopNum( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchIndex( 0 ),
			HWCompIndex( 0 ),
			SecInNode( 0 ),
			IterationLimit( 0 ),
			IterationFailed( 0 )
		{}

	};

	struct AirTerminalMixerData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		int MixerType; // type of inlet mixer, 1 = inlet side, 2 = supply side
		int ZoneHVACUnitType; // type of Zone HVAC unit. ZoneHVAC:WaterToAirHeatPump =1, ZoneHVAC:FourPipeFanCoil = 2
		std::string ZoneHVACUnitName; // name of Zone HVAC unit
		int SecInNode; // secondary air inlet node number
		int PriInNode; // primary air inlet node number
		int MixedAirOutNode; // mixed air outlet node number
		Real64 ZoneAirTemp; // zone air in temp
		Real64 ZoneAirHumRat; // zone air in hum rat
		Real64 ZoneAirEnthalpy; // zone air in enthalpy
		Real64 ZoneAirPressure; // zone air in pressure
		Real64 ZoneAirMassFlowRate; // zone air in mass flow rate
		Real64 DOASTemp; // DOAS air in temp
		Real64 DOASHumRat; // DOAS air in hum rat
		Real64 DOASEnthalpy; // DOAS air in enthalpy
		Real64 DOASPressure; // DOAS air in pressure
		Real64 DOASMassFlowRate; // DOAS air in mass flow rate
		Real64 MixedAirTemp; // mixed air in temp
		Real64 MixedAirHumRat; // mixed air in hum rat
		Real64 MixedAirEnthalpy; // mixed air in enthalpy
		Real64 MixedAirPressure; // mixed air in pressure
		Real64 MixedAirMassFlowRate; // mixed air in mass flow rate
		Real64 MaxAirMassFlowRate; // maximum air mass flow rate allowed through component

		// Default Constructor
		AirTerminalMixerData() :
			MixerType( 0 ),
			ZoneHVACUnitType( 0 ),
			SecInNode( 0 ),
			PriInNode( 0 ),
			MixedAirOutNode( 0 ),
			ZoneAirTemp( 0.0 ),
			ZoneAirHumRat( 0.0 ),
			ZoneAirEnthalpy( 0.0 ),
			ZoneAirPressure( 0.0 ),
			ZoneAirMassFlowRate( 0.0 ),
			DOASTemp( 0.0 ),
			DOASHumRat( 0.0 ),
			DOASEnthalpy( 0.0 ),
			DOASPressure( 0.0 ),
			DOASMassFlowRate( 0.0 ),
			MixedAirTemp( 0.0 ),
			MixedAirHumRat( 0.0 ),
			MixedAirEnthalpy( 0.0 ),
			MixedAirPressure( 0.0 ),
			MixedAirMassFlowRate( 0.0 ),
			MaxAirMassFlowRate( 0.0 )
		{}

	};

	struct SysFlowConditions
	{
		// Members
		Real64 AirMassFlowRate; // MassFlow through the Sys being Simulated [kg/Sec]
		Real64 AirMassFlowRateMaxAvail; // MassFlow through the Sys being Simulated [kg/Sec]
		Real64 AirMassFlowRateMinAvail; // MassFlow through the Sys being Simulated [kg/Sec]
		Real64 AirTemp; // (C)
		Real64 AirHumRat; // (Kg/Kg)
		Real64 AirEnthalpy; // (J/Kg)
		Real64 AirPressure;

		// Default Constructor
		SysFlowConditions() :
			AirMassFlowRate( 0.0 ),
			AirMassFlowRateMaxAvail( 0.0 ),
			AirMassFlowRateMinAvail( 0.0 ),
			AirTemp( 0.0 ),
			AirHumRat( 0.0 ),
			AirEnthalpy( 0.0 ),
			AirPressure( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< SysDesignParams > Sys;
	extern Array1D< SysFlowConditions > SysInlet;
	extern Array1D< SysFlowConditions > SysOutlet;
	extern Array1D< AirTerminalMixerData > SysATMixer;

	// Functions
	void
	clear_state();

	void
	SimulateSingleDuct(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum,
		int & CompIndex
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetSysInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitSys(
		int const SysNum,
		bool const FirstHVACIteration
	);

	void
	SizeSys( int const SysNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimVAV(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	CalcOAMassFlow(
		int const SysNum, // index to terminal unit
		Real64 & SAMassFlow, // outside air based on optional user input
		Real64 & AirLoopOAFrac // outside air based on optional user input
	);

	void
	SimCBVAV(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	SimVAVVS(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	SimConstVol(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	CalcVAVVS(
		int const SysNum, // Unit index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const ZoneNode, // zone node number
		int const HCoilType, // type of hot water coil !unused1208
		Real64 const HWFlow, // hot water flow (kg/s)
		Real64 const HCoilReq, // gas or elec coil demand requested
		int const FanType, // type of fan
		Real64 const AirFlow, // air flow rate (kg/s)
		int const FanOn, // 1 means fan is on
		Real64 & LoadMet // load met by unit (watts)
	);

	Real64
	VAVVSCoolingResidual(
		Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	Real64
	VAVVSHWNoFanResidual(
		Real64 const HWMassFlow, // hot water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	Real64
	VAVVSHWFanOnResidual(
		Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	Real64
	VAVVSHCFanOnResidual(
		Real64 const HeatingFrac, // fraction of maximum heating output
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Sys Module
	// *****************************************************************************

	void
	UpdateSys( int const SysNum );

	//        End of Update subroutines for the Sys Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Sys Module
	// *****************************************************************************

	void
	ReportSys( int const SysNum ); // unused1208

	void
	GetHVACSingleDuctSysIndex(
		std::string const & SDSName,
		int & SDSIndex,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType = _,
		Optional_int DamperInletNode = _, // Damper inlet node number
		Optional_int DamperOutletNode = _ // Damper outlet node number
	);

	void
	SimATMixer(
		std::string const & SysName,
		bool const FirstHVACIteration,
		int & SysIndex
	);

	void
	GetATMixers();

	void
	InitATMixer(
		int const ATMixerNum,
		bool const FirstHVACIteration
	);

	void
	CalcATMixer( int const SysNum );

	void
	UpdateATMixer( int const SysNum );

	void
	GetATMixerPriNode(
		std::string const & ZoneEquipName,
		int & ATMixerPriNode
	);

	void
	GetATMixerSecNode(
		std::string const & ZoneEquipName,
		int & ATMixerSecNode
	);

	void
	GetATMixerOutNode(
		std::string const & ZoneEquipName,
		int & ATMixerOutNode
	);

	void
	GetATMixer(
		std::string const & ZoneEquipName, // zone unit name name
		std::string & ATMixerName, // air terminal mixer name
		int & ATMixerNum, // air terminal mixer index
		int & ATMixerType, // air teminal mixer type
		int & ATMixerPriNode, // air terminal mixer primary air node number
		int & ATMixerSecNode, // air terminal mixer secondary air node number
		int & ATMixerOutNode // air terminal mixer outlet air node number
	);

	void
	SetATMixerPriFlow(
		int const ATMixerNum, // Air terminal mixer index
		Optional< Real64 const > PriAirMassFlowRate = _ // Air terminal mixer primary air mass flow rate [kg/s]
	);

	//        End of Reporting subroutines for the Sys Module
	// *****************************************************************************

} // SingleDuct

} // EnergyPlus

#endif
