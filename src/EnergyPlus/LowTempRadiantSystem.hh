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

#ifndef LowTempRadiantSystem_hh_INCLUDED
#define LowTempRadiantSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace LowTempRadiantSystem {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// System types:
	extern int const HydronicSystem; // Variable flow hydronic radiant system
	extern int const ConstantFlowSystem; // Constant flow, variable (controlled) temperature radiant system
	extern int const ElectricSystem; // Electric resistance radiant heating system
	extern std::string const cHydronicSystem;
	extern std::string const cConstantFlowSystem;
	extern std::string const cElectricSystem;
	// Operating modes:
	extern int const NotOperating; // Parameter for use with OperatingMode variable, set for heating
	extern int const HeatingMode; // Parameter for use with OperatingMode variable, set for heating
	extern int const CoolingMode; // Parameter for use with OperatingMode variable, set for cooling
	// Control types:
	extern int const MATControl; // Controls system using mean air temperature
	extern int const MRTControl; // Controls system using mean radiant temperature
	extern int const OperativeControl; // Controls system using operative temperature
	extern int const ODBControl; // Controls system using outside air dry-bulb temperature
	extern int const OWBControl; // Controls system using outside air wet-bulb temperature
	// Condensation control types:
	extern int const CondCtrlNone; // Condensation control--none, so system never shuts down
	extern int const CondCtrlSimpleOff; // Condensation control--simple off, system shuts off when condensation predicted
	extern int const CondCtrlVariedOff; // Condensation control--variable off, system modulates to keep running if possible
	// Number of Circuits per Surface Calculation Method
	extern int const OneCircuit; // there is 1 circuit per surface
	extern int const CalculateFromLength; // The number of circuits is TubeLength*SurfaceFlowFrac / CircuitLength
	extern std::string const OnePerSurf;
	extern std::string const CalcFromLength;
	// Limit temperatures to indicate that a system cannot heat or cannot cool
	extern Real64 LowTempHeating; // Used to indicate that a user does not have a heating control temperature
	extern Real64 HighTempCooling; // Used to indicate that a user does not have a cooling control temperature

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	extern int NumOfHydrLowTempRadSys; // Number of hydronic low tempererature radiant systems
	extern int NumOfCFloLowTempRadSys; // Number of constant flow (hydronic) low tempererature radiant systems
	extern int NumOfElecLowTempRadSys; // Number of electric low tempererature radiant systems
	extern int CFloCondIterNum; // Number of iterations for a constant flow radiant system--controls variable cond sys ctrl
	extern int TotalNumOfRadSystems; // Total number of low temperature radiant systems
	extern int OperatingMode; // Used to keep track of whether system is in heating or cooling mode
	extern int MaxCloNumOfSurfaces; // Used to set allocate size in CalcClo routine
	extern bool VarOffCond; // Set to true when in cooling for constant flow system + variable off condensation predicted
	extern Real64 LoopReqTemp; // Temperature required at the inlet of the pump (from the loop) to meet control logic
	extern Array1D< Real64 > QRadSysSrcAvg; // Average source over the time step for a particular radiant surface
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QRadSysSrcAvg locally
	extern Array1D< Real64 > LastQRadSysSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	// Autosizing variables
	extern Array1D_bool MySizeFlagHydr;
	extern Array1D_bool MySizeFlagCFlo;
	extern Array1D_bool MySizeFlagElec;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE LowTempRadiantSystem

	// Types

	struct HydronicRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		std::string SurfListName; // Name of surface/surface list that is the radiant system
		int NumOfSurfaces; // Number of surfaces included in this radiant system (coordinated control)
		Array1D_int SurfacePtr; // Pointer to the surface(s) in the Surface derived type
		Array1D_string SurfaceName; // Name of surfaces that are the radiant system (can be one or more)
		Array1D< Real64 > SurfaceFlowFrac; // Fraction of flow/pipe length for a particular surface
		Array1D< Real64 > NumCircuits; // Number of fluid circuits in the surface
		Real64 TotalSurfaceArea; // Total surface area for all surfaces that are part of this radiant system
		Real64 TubeDiameter; // tube diameter for embedded tubing
		Real64 TubeLength; // tube length embedded in radiant surface
		int ControlType; // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
		bool HeatingSystem; // .TRUE. when the system is able to heat (parameters are valid)
		Real64 WaterVolFlowMaxHeat; // maximum water flow rate for heating, m3/s
		Real64 WaterFlowMaxHeat; // maximum water flow rate for heating, kg/s
		int HotWaterInNode; // hot water inlet node
		int HotWaterOutNode; // hot water outlet node
		Real64 HotThrottlRange; // Throttling range for heating [C]
		std::string HotSetptSched; // Schedule name for the zone setpoint temperature
		int HotSetptSchedPtr; // Schedule index for the zone setpoint temperature
		int HWLoopNum;
		int HWLoopSide;
		int HWBranchNum;
		int HWCompNum;
		Real64 WaterVolFlowMaxCool; // maximum water flow rate for cooling, m3/s
		Real64 WaterFlowMaxCool; // maximum water flow rate for cooling, kg/s
		bool CoolingSystem; // .TRUE. when the system is able to cool (parameters are valid)
		int ColdWaterInNode; // cold water inlet node
		int ColdWaterOutNode; // cold water outlet node
		Real64 ColdThrottlRange; // Throttling range for cooling [C]
		std::string ColdSetptSched; // Schedule name for the zone setpoint temperature
		int ColdSetptSchedPtr; // Schedule index for the zone setpoint temperature
		int CWLoopNum;
		int CWLoopSide;
		int CWBranchNum;
		int CWCompNum;
		int GlycolIndex; // Index to Glycol (Water) Properties
		int CondErrIndex; // Error index for recurring warning messages
		int CondCtrlType; // Condensation control type (initialize to simple off)
		Real64 CondDewPtDeltaT; // Diff between surface temperature and dew point for cond. shut-off
		Real64 CondCausedTimeOff; // Amount of time condensation did or could have turned system off
		bool CondCausedShutDown; // .TRUE. when condensation predicted at surface
		int NumCircCalcMethod; // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
		Real64 CircLength; // Circuit length {m}
		// Other parameters
		bool EMSOverrideOnWaterMdot;
		Real64 EMSWaterMdotOverrideValue;
		// Report data
		Real64 WaterInletTemp; // water inlet temperature
		Real64 WaterOutletTemp; // water outlet temperature
		Real64 WaterMassFlowRate; // water mass flow rate
		Real64 HeatPower; // heating sent to panel in Watts
		Real64 HeatEnergy; // heating sent to panel in Joules
		Real64 CoolPower; // cooling sent to panel in Watts
		Real64 CoolEnergy; // cooling sent to panel in Joules
		int OutRangeHiErrorCount; // recurring errors for crazy results too high fluid temperature
		int OutRangeLoErrorCount; // recurring errors for crazy results too low fluid temperature
		int HeatingCapMethod; // - Method for Low Temp Radiant system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // -  Low Temp Radiant system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}
		int CoolingCapMethod; // - Method for Low Temp Radiant system cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedCoolingCapacity)
		Real64 ScaledCoolingCapacity; // -  Low Temp Radiant system scaled maximum cooling capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		HydronicRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			NumOfSurfaces( 0 ),
			TotalSurfaceArea( 0.0 ),
			TubeDiameter( 0.0 ),
			TubeLength( 0.0 ),
			ControlType( 0 ),
			HeatingSystem( false ),
			WaterVolFlowMaxHeat( 0.0 ),
			WaterFlowMaxHeat( 0.0 ),
			HotWaterInNode( 0 ),
			HotWaterOutNode( 0 ),
			HotThrottlRange( 0.0 ),
			HotSetptSchedPtr( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			WaterVolFlowMaxCool( 0.0 ),
			WaterFlowMaxCool( 0.0 ),
			CoolingSystem( false ),
			ColdWaterInNode( 0 ),
			ColdWaterOutNode( 0 ),
			ColdThrottlRange( 0.0 ),
			ColdSetptSchedPtr( 0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			GlycolIndex( 0 ),
			CondErrIndex( 0 ),
			CondCtrlType( 1 ),
			CondDewPtDeltaT( 1.0 ),
			CondCausedTimeOff( 0.0 ),
			CondCausedShutDown( false ),
			NumCircCalcMethod( 0 ),
			CircLength( 0.0 ),
			EMSOverrideOnWaterMdot( false ),
			EMSWaterMdotOverrideValue( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			CoolPower( 0.0 ),
			CoolEnergy( 0.0 ),
			OutRangeHiErrorCount( 0 ),
			OutRangeLoErrorCount( 0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 ),
			CoolingCapMethod( 0 ),
			ScaledCoolingCapacity( 0.0 )
		{}

	};

	struct ConstantFlowRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		std::string SurfListName; // Name of surface/surface list that is the radiant system
		int NumOfSurfaces; // Number of surfaces included in this radiant system (coordinated control)
		Array1D_int SurfacePtr; // Pointer to the surface(s) in the Surface derived type
		Array1D_string SurfaceName; // Name of surfaces that are the radiant system (can be one or more)
		Array1D< Real64 > SurfaceFlowFrac; // Fraction of flow/pipe length for a particular surface
		Array1D< Real64 > NumCircuits; // Number of fluid circuits in the surface
		Real64 TotalSurfaceArea; // Total surface area for all surfaces that are part of this radiant system
		Real64 TubeDiameter; // tube diameter for embedded tubing
		Real64 TubeLength; // tube length embedded in radiant surface
		int ControlType; // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
		Real64 WaterVolFlowMax; // design nominal capacity of constant flow pump (volumetric flow rate)
		Real64 ColdDesignWaterMassFlowRate;
		Real64 HotDesignWaterMassFlowRate;
		Real64 WaterMassFlowRate; // current flow rate through system (calculated)
		std::string VolFlowSched; // schedule of maximum flow at the current time
		int VolFlowSchedPtr; // index to the volumetric flow schedule
		Real64 NomPumpHead; // nominal head of the constant flow pump
		Real64 NomPowerUse; // nominal power use of the constant flow pump
		Real64 MotorEffic; // efficiency of the pump motor
		Real64 PumpEffic; // overall efficiency of the pump (calculated)
		Real64 FracMotorLossToFluid; // amount of heat generated by pump motor that is added to the fluid
		bool HeatingSystem; // .TRUE. when the system is able to heat (parameters are valid)
		int HotWaterInNode; // hot water inlet node
		int HotWaterOutNode; // hot water outlet node
		std::string HotWaterHiTempSched; // Schedule name for the highest water temperature
		int HotWaterHiTempSchedPtr; // Schedule index for the highest water temperature
		std::string HotWaterLoTempSched; // Schedule name for the lowest water temperature
		int HotWaterLoTempSchedPtr; // Schedule index for the lowest water temperature
		std::string HotCtrlHiTempSched; // Schedule name for the highest control temperature
		// (where the lowest water temperature is requested)
		int HotCtrlHiTempSchedPtr; // Schedule index for the highest control temperature
		// (where the lowest water temperature is requested)
		std::string HotCtrlLoTempSched; // Schedule name for the lowest control temperature
		// (where the highest water temperature is requested)
		int HotCtrlLoTempSchedPtr; // Schedule index for the lowest control temperature
		// (where the highest water temperature is requested)
		int HWLoopNum;
		int HWLoopSide;
		int HWBranchNum;
		int HWCompNum;
		bool CoolingSystem; // .TRUE. when the system is able to cool (parameters are valid)
		int ColdWaterInNode; // cold water inlet node
		int ColdWaterOutNode; // cold water outlet node
		std::string ColdWaterHiTempSched; // Schedule name for the highest water temperature
		int ColdWaterHiTempSchedPtr; // Schedule index for the highest water temperature
		std::string ColdWaterLoTempSched; // Schedule name for the lowest water temperature
		int ColdWaterLoTempSchedPtr; // Schedule index for the lowest water temperature
		std::string ColdCtrlHiTempSched; // Schedule name for the highest control temperature
		// (where the lowest water temperature is requested)
		int ColdCtrlHiTempSchedPtr; // Schedule index for the highest control temperature
		// (where the lowest water temperature is requested)
		std::string ColdCtrlLoTempSched; // Schedule name for the lowest control temperature
		// (where the highest water temperature is requested)
		int ColdCtrlLoTempSchedPtr; // Schedule index for the lowest control temperature
		// (where the highest water temperature is requested)
		int CWLoopNum;
		int CWLoopSide;
		int CWBranchNum;
		int CWCompNum;
		int GlycolIndex; // Index to Glycol (Water) Properties
		int CondErrIndex; // Error index for warning messages
		int CondCtrlType; // Condensation control type (initialize to simple off)
		Real64 CondDewPtDeltaT; // Diff between surface temperature and dew point for cond. shut-off
		Real64 CondCausedTimeOff; // Amount of time condensation did or could have turned system off
		bool CondCausedShutDown; // .TRUE. when condensation predicted at surface
		int NumCircCalcMethod; // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
		Real64 CircLength; // Circuit length {m}
		// Other parameters
		bool EMSOverrideOnWaterMdot;
		Real64 EMSWaterMdotOverrideValue;
		// Report data
		Real64 WaterInletTemp; // water inlet temperature
		Real64 WaterOutletTemp; // water outlet temperature
		Real64 WaterInjectionRate; // water injection mass flow rate from main loop
		Real64 WaterRecircRate; // water recirculation rate (outlet from radiant system recirculated)
		Real64 HeatPower; // heating sent to panel in Watts
		Real64 HeatEnergy; // heating sent to panel in Joules
		Real64 CoolPower; // cooling sent to panel in Watts
		Real64 CoolEnergy; // cooling sent to panel in Joules
		Real64 PumpPower; // pump power in Watts
		Real64 PumpEnergy; // pump energy consumption in Joules
		Real64 PumpMassFlowRate; // mass flow rate through the radiant system in kg/sec
		Real64 PumpHeattoFluid; // heat transfer rate from pump motor to fluid in Watts
		Real64 PumpHeattoFluidEnergy; // Pump Energy dissipated into fluid stream in Joules
		Real64 PumpInletTemp; // inlet temperature of pump (inlet temperature from loop)
		int OutRangeHiErrorCount; // recurring errors for crazy results too high fluid temperature
		int OutRangeLoErrorCount; // recurring errors for crazy results too low fluid temperature

		// Default Constructor
		ConstantFlowRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			NumOfSurfaces( 0 ),
			TotalSurfaceArea( 0.0 ),
			TubeDiameter( 0.0 ),
			TubeLength( 0.0 ),
			ControlType( 0 ),
			WaterVolFlowMax( 0.0 ),
			ColdDesignWaterMassFlowRate( 0.0 ),
			HotDesignWaterMassFlowRate( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			VolFlowSchedPtr( 0 ),
			NomPumpHead( 0.0 ),
			NomPowerUse( 0.0 ),
			MotorEffic( 0.0 ),
			PumpEffic( 0.0 ),
			FracMotorLossToFluid( 0.0 ),
			HeatingSystem( false ),
			HotWaterInNode( 0 ),
			HotWaterOutNode( 0 ),
			HotWaterHiTempSchedPtr( 0 ),
			HotWaterLoTempSchedPtr( 0 ),
			HotCtrlHiTempSchedPtr( 0 ),
			HotCtrlLoTempSchedPtr( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			CoolingSystem( false ),
			ColdWaterInNode( 0 ),
			ColdWaterOutNode( 0 ),
			ColdWaterHiTempSchedPtr( 0 ),
			ColdWaterLoTempSchedPtr( 0 ),
			ColdCtrlHiTempSchedPtr( 0 ),
			ColdCtrlLoTempSchedPtr( 0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			GlycolIndex( 0 ),
			CondErrIndex( 0 ),
			CondCtrlType( 1 ),
			CondDewPtDeltaT( 1.0 ),
			CondCausedTimeOff( 0.0 ),
			CondCausedShutDown( false ),
			NumCircCalcMethod( 0 ),
			CircLength( 0.0 ),
			EMSOverrideOnWaterMdot( false ),
			EMSWaterMdotOverrideValue( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterInjectionRate( 0.0 ),
			WaterRecircRate( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			CoolPower( 0.0 ),
			CoolEnergy( 0.0 ),
			PumpPower( 0.0 ),
			PumpEnergy( 0.0 ),
			PumpMassFlowRate( 0.0 ),
			PumpHeattoFluid( 0.0 ),
			PumpHeattoFluidEnergy( 0.0 ),
			PumpInletTemp( 0.0 ),
			OutRangeHiErrorCount( 0 ),
			OutRangeLoErrorCount( 0 )
		{}

	};

	struct ElectricRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		std::string SurfListName; // Name of surface/surface list that is the radiant system
		int NumOfSurfaces; // Number of surfaces included in this radiant system (coordinated control)
		Array1D_int SurfacePtr; // Pointer to the surface(s) in the Surface derived type
		Array1D_string SurfaceName; // Name of surfaces that are the radiant system (can be one or more)
		Array1D< Real64 > SurfacePowerFrac; // Fraction of total power input to surface
		Real64 TotalSurfaceArea; // Total surface area for all surfaces that are part of this radiant system
		Real64 MaxElecPower; // Maximum electric power that can be supplied to surface, Watts
		int ControlType; // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
		Real64 ThrottlRange; // Throttling range for heating [C]
		std::string SetptSched; // Schedule name for the zone setpoint temperature
		int SetptSchedPtr; // Schedule index for the zone setpoint temperature
		// Other parameters
		// Report data
		Real64 ElecPower; // heating sent to panel in Watts
		Real64 ElecEnergy; // heating sent to panel in Joules
		Real64 HeatPower; // heating sent to panel in Watts (same as ElecPower)
		Real64 HeatEnergy; // heating sent to panel in Joules (same as ElecEnergy)
		int HeatingCapMethod;    // - Method for Low Temp Radiant system heating capacity scaledsizing calculation
		//- (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity;   // -  Low Temp Radiant system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		ElectricRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			NumOfSurfaces( 0 ),
			TotalSurfaceArea( 0.0 ),
			MaxElecPower( 0.0 ),
			ControlType( 0 ),
			ThrottlRange( 0.0 ),
			SetptSchedPtr( 0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

	};

	struct RadSysTypeData
	{
		// Members
		// This type used to track different components/types for efficiency
		std::string Name; // name of radiant system
		int SystemType; // Type of System (see System Types in Parameters)
		int CompIndex; // Index in specific system types

		// Default Constructor
		RadSysTypeData() :
			SystemType( 0 ),
			CompIndex( 0 )
		{}

	};

	struct ElecRadSysNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		ElecRadSysNumericFieldData()
		{}

	};

	struct HydronicRadiantSysNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HydronicRadiantSysNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< HydronicRadiantSystemData > HydrRadSys;
	extern Array1D< ConstantFlowRadiantSystemData > CFloRadSys;
	extern Array1D< ElectricRadiantSystemData > ElecRadSys;
	extern Array1D< RadSysTypeData > RadSysTypes;
	extern Array1D< ElecRadSysNumericFieldData > ElecRadSysNumericFields;
	extern Array1D< HydronicRadiantSysNumericFieldData > HydronicRadiantSysNumericFields;

	// Functions

	void
	clear_state();

	void
	SimLowTempRadiantSystem(
		std::string const & CompName, // name of the low temperature radiant system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & LoadMet, // load met by the radiant system, in Watts
		int & CompIndex
	);

	void
	GetLowTempRadiantSystem();

	void
	InitLowTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

	void
	SizeLowTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

	void
	CalcLowTempHydrRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	CalcLowTempHydrRadSysComps(
		int const RadSysNum, // Index for the low temperature radiant system under consideration
		Real64 & LoadMet // Load met by the low temperature radiant system, in Watts
	);

	void
	CalcLowTempCFloRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	CalcLowTempCFloRadSysComps(
		int const RadSysNum, // Index for the low temperature radiant system under consideration
		int const MainLoopNodeIn, // Node number on main loop of the inlet node to the radiant system
		bool const Iteration, // FALSE for the regular solution, TRUE when we had to loop back
		Real64 & LoadMet // Load met by the low temperature radiant system, in Watts
	);

	void
	CalcLowTempElecRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	UpdateLowTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

	void
	CheckForOutOfRangeTempResult(
		int const SystemType,
		int const RadSysNum,
		Real64 const outletTemp,
		Real64 const inletTemp,
		Real64 const mdot
	);

	Real64
	CalcRadSysHXEffectTerm(
		int const RadSysNum, // Index number of radiant system under consideration !unused1208
		int const SystemType, // Type of radiant system: hydronic, constant flow, or electric
		Real64 const Temperature, // Temperature of water entering the radiant system, in C
		Real64 const WaterMassFlow, // Mass flow rate of water in the radiant system, in kg/s
		Real64 const FlowFraction, // Mass flow rate fraction for this surface in the radiant system
		Real64 const NumCircs, // Number of fluid circuits in this surface
		Real64 const TubeLength, // Length of tubing in the radiant system, in m
		Real64 const TubeDiameter, // Inside diameter of the tubing in the radiant system, in m
		int & GlycolIndex // Index for the fluid used in this radiant system
	);

	void
	UpdateRadSysSourceValAvg( bool & LowTempRadSysOn ); // .TRUE. if the radiant system has run this zone time step

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	ReportLowTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

} // LowTempRadiantSystem

} // EnergyPlus

#endif
