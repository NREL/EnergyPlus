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

#ifndef DataHVACGlobals_hh_INCLUDED
#define DataHVACGlobals_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataHVACGlobals {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	extern Real64 const SmallTempDiff;
	extern Real64 const SmallMassFlow;
	extern Real64 const VerySmallMassFlow;
	extern Real64 const SmallLoad;
	extern Real64 const TempControlTol; // temperature control tolerance for packaged equip. [deg C]
	extern Real64 const SmallAirVolFlow;
	extern Real64 const SmallWaterVolFlow;
	extern Real64 const BlankNumeric; // indicates numeric input field was blank
	extern Real64 const RetTempMax; // maximum return air temperature [deg C]
	extern Real64 const RetTempMin; // minimum return air temperature [deg C]

	extern int const NumOfSizingTypes; // request sizing for cooling air flow rate

	extern int const CoolingAirflowSizing; // request sizing for cooling air flow rate
	extern int const CoolingWaterflowSizing; // request sizing for cooling coil water flow rate
	extern int const HeatingWaterflowSizing; // request sizing for heating coil water flow rate
	extern int const CoolingWaterDesAirInletTempSizing; // request sizing for cooling water coil inlet air temp
	extern int const CoolingWaterDesAirInletHumRatSizing; // request sizing for cooling water coil inlet air humidity ratio
	extern int const CoolingWaterDesWaterInletTempSizing; // request sizing for cooling water coil inlet water temp
	extern int const CoolingWaterDesAirOutletTempSizing; // request sizing for cooling water coil outlet air temp
	extern int const CoolingWaterDesAirOutletHumRatSizing; // request sizing for cooling water coil outlet air humidity ratio
	extern int const CoolingWaterNumofTubesPerRowSizing; // request sizing for cooling water coil number of tubes per row
	extern int const HeatingWaterDesAirInletTempSizing; // request sizing for heating water coil inlet air temp
	extern int const HeatingWaterDesAirInletHumRatSizing; // request sizing for heating water coil inlet air humidity ratio
	extern int const HeatingWaterDesCoilLoadUsedForUASizing; // request sizing for heating water coil capacity used for UA sizing
	extern int const HeatingWaterDesCoilWaterVolFlowUsedForUASizing; // request sizing for heating water coil volume flow rate used for UA sizing
	extern int const HeatingAirflowSizing; // request sizing for heating air flow rate
	extern int const HeatingAirflowUASizing; // request sizing for heating air flow rate
	extern int const SystemAirflowSizing; // request sizing for system air flow rate
	extern int const CoolingCapacitySizing; // request sizing for cooling capacity
	extern int const HeatingCapacitySizing; // request sizing for heating capacity
	extern int const WaterHeatingCapacitySizing; // request sizing for heating capacity
	extern int const WaterHeatingCoilUASizing; // request sizing for heating coil UA
	extern int const SystemCapacitySizing; // request sizing for system capacity
	extern int const CoolingSHRSizing; // request sizing for cooling SHR
	extern int const HeatingDefrostSizing; // request sizing for heating defrost capacity
	extern int const MaxHeaterOutletTempSizing; // request sizing for heating coil maximum outlet temperature
	extern int const AutoCalculateSizing; // identifies an autocalulate input
	extern int const ZoneCoolingLoadSizing; // zone cooling sensible load (zsz file)
	extern int const ZoneHeatingLoadSizing; // zome heating sensible load (zsz file)
	extern int const MinSATempCoolingSizing; // minimum SA temperature in cooling model when using ASHRAE 90.1 SZVAV method
	extern int const MaxSATempHeatingSizing; // maximum SA temperature in heating model when using ASHRAE 90.1 SZVAV method
	extern int const HeatingCoilDesAirInletTempSizing; // design inlet air temperature for heating coil
	extern int const HeatingCoilDesAirOutletTempSizing; // design outlet air temperature for heating coil
	extern int const HeatingCoilDesAirInletHumRatSizing; // design inlet air humidity ratio for heating coil


	// Condenser Type (using same numbering scheme as for chillers)
	extern int const AirCooled; // Air-cooled condenser
	extern int const WaterCooled; // Water-cooled condenser
	extern int const EvapCooled; // Evaporatively-cooled condenser
	extern int const WaterHeater; // Condenser heats water (e.g., in water heater tank)

	// The following parameters are used for system availability status
	extern int const NoAction;
	extern int const ForceOff;
	extern int const CycleOn;
	extern int const CycleOnZoneFansOnly;
	// The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
	extern int const SingleHeatingSetPoint;
	extern int const SingleCoolingSetPoint;
	extern int const SingleHeatCoolSetPoint;
	extern int const DualSetPointWithDeadBand;
	// parameters describing air duct type
	extern int const Main;
	extern int const Cooling;
	extern int const Heating;
	extern int const Other;
	extern int const RAB;
	// parameters describing fan types
	extern int const NumAllFanTypes; // cpw22Aug2010 (was 4)

	// fan types
	extern int const FanType_SimpleConstVolume;
	extern int const FanType_SimpleVAV;
	extern int const FanType_SimpleOnOff;
	extern int const FanType_ZoneExhaust;
	extern int const FanType_ComponentModel; // cpw22Aug2010 (new)
	// Fan Minimum Flow Fraction Input Method
	extern int const MinFrac;
	extern int const FixedMin;
	// Fan mode
	extern int const CycFanCycCoil; // Cycling fan, cycling coil = 1
	extern int const ContFanCycCoil; // Continuous fan, cycling coil = 2
	// Fan placement
	extern int const BlowThru; // fan before coil
	extern int const DrawThru; // fan after coil
	// OA Controller Heat Recovery Bypass Control Types
	extern int const BypassWhenWithinEconomizerLimits; // heat recovery controlled by economizer limits
	extern int const BypassWhenOAFlowGreaterThanMinimum; // heat recovery ON at minimum OA in economizer mode

	extern Array1D_string const cFanTypes; // cpw22Aug2010 | cpw22Aug2010 (new)

	// parameters describing unitary systems
	extern int const NumUnitarySystemTypes;
	// Furnace/Unitary System Types
	extern int const Furnace_HeatOnly;
	extern int const Furnace_HeatCool;
	extern int const UnitarySys_HeatOnly;
	extern int const UnitarySys_HeatCool;
	extern int const UnitarySys_HeatPump_AirToAir;
	extern int const UnitarySys_HeatPump_WaterToAir;
	extern int const UnitarySystem_AnyCoilType;
	extern Array1D_string const cFurnaceTypes;

	// parameters describing coil types
	extern int const NumAllCoilTypes;

	extern int const CoilDX_CoolingSingleSpeed;
	extern int const CoilDX_HeatingEmpirical;
	extern int const CoilDX_CoolingTwoSpeed;
	extern int const CoilDX_CoolingHXAssisted;
	extern int const CoilDX_CoolingTwoStageWHumControl;
	extern int const CoilDX_HeatPumpWaterHeaterPumped;
	extern int const CoilDX_HeatPumpWaterHeaterWrapped;
	extern int const CoilDX_MultiSpeedCooling;
	extern int const CoilDX_MultiSpeedHeating;

	extern int const Coil_HeatingGas;
	extern int const Coil_HeatingGas_MultiStage;
	extern int const Coil_HeatingElectric;
	extern int const Coil_HeatingElectric_MultiStage;
	extern int const Coil_HeatingDesuperheater;

	extern int const Coil_CoolingWater;
	extern int const Coil_CoolingWaterDetailed;
	extern int const Coil_HeatingWater;
	extern int const Coil_HeatingSteam;
	extern int const CoilWater_CoolingHXAssisted;

	extern int const Coil_CoolingWaterToAirHP;
	extern int const Coil_HeatingWaterToAirHP;
	extern int const Coil_CoolingWaterToAirHPSimple;
	extern int const Coil_HeatingWaterToAirHPSimple;
	extern int const CoilVRF_Cooling;
	extern int const CoilVRF_Heating;

	extern int const Coil_UserDefined;
	extern int const CoilDX_PackagedThermalStorageCooling;

	extern int const Coil_CoolingWaterToAirHPVSEquationFit;
	extern int const Coil_HeatingWaterToAirHPVSEquationFit;
	extern int const Coil_CoolingAirToAirVariableSpeed;
	extern int const Coil_HeatingAirToAirVariableSpeed;
	extern int const CoilDX_HeatPumpWaterHeaterVariableSpeed;
	
	extern int const CoilVRF_FluidTCtrl_Cooling;
	extern int const CoilVRF_FluidTCtrl_Heating;

	extern Array1D_string const cAllCoilTypes;

	// Water to air HP coil types
	extern int const WatertoAir_Simple;
	extern int const WatertoAir_ParEst;
	extern int const WatertoAir_VarSpeedEquationFit;
	extern int const WatertoAir_VarSpeedLooUpTable;

	// Water to Air HP Water Flow Mode
	extern int const WaterCycling; // water flow cycles with compressor
	extern int const WaterConstant; // water flow is constant
	extern int const WaterConstantOnDemand; // water flow is constant whenever the coil is operational - this is the only method used in EP V7.2 and earlier

	// parameters describing coil performance types
	extern int const CoilPerfDX_CoolBypassEmpirical;

	// Airflow per total capacity range (Regular DX coils)
	extern Real64 const MaxRatedVolFlowPerRatedTotCap1; // m3/s per watt = 450 cfm/ton
	extern Real64 const MinRatedVolFlowPerRatedTotCap1; // m3/s per watt = 300 cfm/ton
	extern Real64 const MaxHeatVolFlowPerRatedTotCap1; // m3/s per watt = 600 cfm/ton
	extern Real64 const MaxCoolVolFlowPerRatedTotCap1; // m3/s per watt = 500 cfm/ton
	extern Real64 const MinOperVolFlowPerRatedTotCap1; // m3/s per watt = 200 cfm/ton

	// 100% DOAS DX coils Airflow per total capacity ratio
	extern Real64 const MaxRatedVolFlowPerRatedTotCap2; // m3/s per watt = 250 cfm/ton
	extern Real64 const MinRatedVolFlowPerRatedTotCap2; // m3/s per watt = 125 cfm/ton
	extern Real64 const MaxHeatVolFlowPerRatedTotCap2; // m3/s per watt = 300 cfm/ton
	extern Real64 const MaxCoolVolFlowPerRatedTotCap2; // m3/s per watt = 300 cfm/ton
	extern Real64 const MinOperVolFlowPerRatedTotCap2; // m3/s per watt = 100 cfm/ton

	extern Array1D< Real64 > MaxRatedVolFlowPerRatedTotCap;
	extern Array1D< Real64 > MinRatedVolFlowPerRatedTotCap;
	extern Array1D< Real64 > MaxHeatVolFlowPerRatedTotCap;
	extern Array1D< Real64 > MaxCoolVolFlowPerRatedTotCap;
	extern Array1D< Real64 > MinOperVolFlowPerRatedTotCap;

	// dx coil type (DXCT)
	extern int const RegularDXCoil; // Regular DX coils or mixed air dx coils
	extern int const DOASDXCoil; // 100% DOAS DX coils
	extern int DXCT; // dx coil type: regular DX coil ==1, 100% DOAS DX coil = 2

	// Parameters describing Heat Exchanger types
	extern int const NumHXTypes;

	extern int const HX_AIRTOAIR_FLATPLATE;
	extern int const HX_AIRTOAIR_GENERIC;
	extern int const HX_DESICCANT_BALANCED;

	extern Array1D_string const cHXTypes;

	// Parameters describing air terminal mixers
	extern int const NumATMixerTypes;

	extern int const No_ATMixer;
	extern int const ATMixer_InletSide;
	extern int const ATMixer_SupplySide;

	extern Array1D_string const cATMixerTypes;
	extern bool const ATMixerExists;

	// Parameters describing variable refrigerant flow terminal unit types
	extern int const NumVRFTUTypes;

	extern int const VRFTUType_ConstVolume;

	extern Array1D_string const cVRFTUTypes;

	// VRF Heating Performance Curve Temperature Type
	extern int const NumVRFHeatingPerformanceOATTypes;
	extern int const WetBulbIndicator;
	extern int const DryBulbIndicator;

	extern Array1D_string const cVRFHeatingPerformanceOATTypes;

	// parameter concerning the amount of change in zone temperature is needed
	// for oscillation of zone temperature to be detected.
	extern Real64 const OscillateMagnitude;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS

	// MODULE VARIABLE DECLARATIONS:

	extern bool FirstTimeStepSysFlag; // Set to true at the start of each sub-time step

	extern Real64 TimeStepSys; // System Time Increment - the adaptive time step used by the HVAC simulation (hours)
	extern Real64 SysTimeElapsed; // elapsed system time in zone timestep (hours)
	extern Real64 FracTimeStepZone; // System time step divided by the zone time step
	extern bool ShortenTimeStepSys; // Logical flag that triggers shortening of system time step
	extern int NumOfSysTimeSteps; // for current zone time step, number of system timesteps inside  it
	extern int NumOfSysTimeStepsLastZoneTimeStep; // previous zone time step, num of system timesteps inside
	extern int LimitNumSysSteps;

	extern bool UseZoneTimeStepHistory; // triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
	extern int NumPlantLoops; // Number of plant loops specified in simulation
	extern int NumCondLoops; // Number of condenser plant loops specified in simulation
	extern int NumElecCircuits; // Number of electric circuits specified in simulation
	extern int NumGasMeters; // Number of gas meters specified in simulation
	extern int NumPrimaryAirSys; // Number of primary HVAC air systems
	extern Real64 FanElecPower; // fan power from last fan simulation
	extern Real64 OnOffFanPartLoadFraction; // fan part-load fraction (Fan:OnOff)
	extern Real64 DXCoilTotalCapacity; // DX coil total cooling capacity (eio report var for HPWHs)
	extern Real64 DXElecCoolingPower; // Electric power consumed by DX cooling coil last DX simulation
	extern Real64 DXElecHeatingPower; // Electric power consumed by DX heating coil last DX simulation
	extern Real64 ElecHeatingCoilPower; // Electric power consumed by electric heating coil
	extern Real64 AirToAirHXElecPower; // Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
	// from last simulation in HeatRecovery.cc
	extern Real64 UnbalExhMassFlow; // unbalanced zone exhaust from a zone equip component [kg/s]
	extern Real64 BalancedExhMassFlow; // balanced zone exhaust (declared as so by user)  [kg/s]
	extern Real64 PlenumInducedMassFlow; // secondary air mass flow rate induced from a return plenum [kg/s]
	extern bool TurnFansOn; // If true overrides fan schedule and cycles fans on
	extern bool TurnFansOff; // If True overides fan schedule and TurnFansOn and forces fans off
	extern bool ZoneCompTurnFansOn; // If true overrides fan schedule and cycles fans on
	extern bool ZoneCompTurnFansOff; // If True overides fan schedule and TurnFansOn and forces fans off
	extern bool SetPointErrorFlag; // True if any needed setpoints not set; if true, program terminates
	extern bool DoSetPointTest; // True one time only for sensed node setpoint test
	extern bool NightVentOn; // set TRUE in SimAirServingZone if night ventilation is happening

	extern int NumTempContComps;
	extern Real64 HPWHInletDBTemp; // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
	extern Real64 HPWHInletWBTemp; // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
	extern Real64 HPWHCrankcaseDBTemp; // Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
	extern bool AirLoopInit; // flag for whether InitAirLoops has been called
	extern bool AirLoopsSimOnce; // True means that the air loops have been simulated once in this environment

	// Hybrid ventilation control part
	extern int NumHybridVentSysAvailMgrs; // Number of hybrid ventilation control
	extern Array1D_int HybridVentSysAvailAirLoopNum; // Airloop number in hybrid vent availability manager
	extern Array1D_int HybridVentSysAvailVentCtrl; // Ventilation control action in hybrid vent availability manager
	extern Array1D_int HybridVentSysAvailActualZoneNum; // Actual zone num in hybrid vent availability manager
	extern Array1D_int HybridVentSysAvailANCtrlStatus; // AN control status in hybrid vent availability manager
	extern Array1D_int HybridVentSysAvailMaster; // Master object name: Ventilation for simple; Zone name for AN
	extern Array1D< Real64 > HybridVentSysAvailWindModifier; // Wind modifier for AirflowNetwork
	// For multispeed heat pump only
	extern Real64 MSHPMassFlowRateLow; // Mass flow rate at low speed
	extern Real64 MSHPMassFlowRateHigh; // Mass flow rate at high speed
	extern Real64 MSHPWasteHeat; // Waste heat
	extern Real64 PreviousTimeStep; // The time step length at the previous time step
	extern bool ShortenTimeStepSysRoomAir; // Logical flag that triggers shortening of system time step

	extern Real64 deviationFromSetPtThresholdHtg; // heating threshold for reporting setpoint deviation
	extern Real64 deviationFromSetPtThresholdClg; // cooling threshold for reporting setpoint deviation

	extern bool SimAirLoopsFlag; // True when the air loops need to be (re)simulated
	extern bool SimElecCircuitsFlag; // True when electic circuits need to be (re)simulated
	extern bool SimPlantLoopsFlag; // True when the main plant loops need to be (re)simulated
	extern bool SimZoneEquipmentFlag; // True when zone equipment components need to be (re)simulated
	extern bool SimNonZoneEquipmentFlag; // True when non-zone equipment components need to be (re)simulated
	extern bool ZoneMassBalanceHVACReSim; // True when zone air mass flow balance and air loop needs (re)simulated

	extern int const NumZoneHVACTerminalTypes;
	extern Array1D_string const ccZoneHVACTerminalTypes;
	extern Array1D_string const ZoneHVACTerminalTypes;
	extern int const ZoneEquipTypeOf_VariableRefrigerantFlow;
	extern int const ZoneEquipTypeOf_EnergyRecoveryVentilator;
	extern int const ZoneEquipTypeOf_FourPipeFanCoil;
	extern int const ZoneEquipTypeOf_OutdoorAirUnit;
	extern int const ZoneEquipTypeOf_PackagedTerminalAirConditioner;
	extern int const ZoneEquipTypeOf_PackagedTerminalHeatPump;
	extern int const ZoneEquipTypeOf_UnitHeater;
	extern int const ZoneEquipTypeOf_UnitVentilator;
	extern int const ZoneEquipTypeOf_VentilatedSlab;
	extern int const ZoneEquipTypeOf_WaterToAirHeatPump;
	extern int const ZoneEquipTypeOf_WindowAirConditioner;
	extern int const ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric;
	extern int const ZoneEquipTypeOf_BaseboardRadiantConvectiveWater;
	extern int const ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam;
	extern int const ZoneEquipTypeOf_BaseboardConvectiveElectric;
	extern int const ZoneEquipTypeOf_BaseboardConvectiveWater;
	extern int const ZoneEquipTypeOf_HighTemperatureRadiant;
	extern int const ZoneEquipTypeOf_DehumidifierDX;
	extern int const ZoneEquipTypeOf_IdealLoadsAirSystem;
	extern int const ZoneEquipTypeOf_RefrigerationChillerSet;
	extern int const ZoneEquipTypeOf_FanZoneExhaust;
	extern int const ZoneEquipTypeOf_WaterHeaterHeatPump;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctUncontrolled;
	extern int const ZoneEquipTypeOf_AirTerminalDualDuctConstantVolume;
	extern int const ZoneEquipTypeOf_AirTerminalDualDuctVAV;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeReheat;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctVAVReheat;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctVAVNoReheat;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctSeriesPIUReheat;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctParallelPIUReheat;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctCAVFourPipeInduction;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctVAVReheatVariableSpeedFan;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctVAVHeatAndCoolReheat;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctVAVHeatAndCoolNoReheat;
	extern int const ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeCooledBeam;
	extern int const ZoneEquipTypeOf_AirTerminalDualDuctVAVOutdoorAir;
	extern int const ZoneEquipTypeOf_AirLoopHVACReturnAir;

	// Types

	struct ComponentSetPtData
	{
		// Members
		// CHARACTER(len=MaxNameLength) :: EquipOperListName
		std::string EquipmentType;
		std::string EquipmentName;
		int NodeNumIn;
		int NodeNumOut;
		Real64 EquipDemand;
		Real64 DesignFlowRate;
		std::string HeatOrCool;
		int OpType;

		// Default Constructor
		ComponentSetPtData() :
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			EquipDemand( 0.0 ),
			DesignFlowRate( 0.0 ),
			OpType( 0 )
		{}

	};

	struct DefineZoneCompAvailMgrs
	{
		// Members
		int NumAvailManagers; // number of availability managers for this system
		int AvailStatus; // system availability status
		int StartTime; // cycle on time (in SimTimeSteps)
		int StopTime; // cycle off time (in SimTimeSteps)
		std::string AvailManagerListName; // name of each availability manager
		Array1D_string AvailManagerName; // name of each availability manager
		Array1D_int AvailManagerType; // type of availability manager
		Array1D_int AvailManagerNum; // index for availability manager
		int ZoneNum; // cycle off time (in SimTimeSteps)
		bool Input; // starts off as true to initialize zone equipment availability manager data
		int Count; // initialize twice to ensure zone equipment availability manager list name has been read in

		// Default Constructor
		DefineZoneCompAvailMgrs() :
			NumAvailManagers( 0 ),
			AvailStatus( 0 ),
			StartTime( 0 ),
			StopTime( 0 ),
			ZoneNum( 0 ),
			Input( true ),
			Count ( 0 )
		{}

	};

	struct ZoneCompTypeData
	{
		// Members
		Array1D< DefineZoneCompAvailMgrs > ZoneCompAvailMgrs;
		int TotalNumComp; // total number of components of a zone equip type

		// Default Constructor
		ZoneCompTypeData() :
			TotalNumComp( 0 )
		{}

	};

	struct OptStartDataType
	{
		// Members
		Array1D_int ActualZoneNum;
		Array1D< Real64 > OccStartTime;
		Array1D_bool OptStartFlag;

		// Default Constructor
		OptStartDataType()
		{}

	};

	// Object Data
	extern Array1D< ZoneCompTypeData > ZoneComp;
	extern OptStartDataType OptStartData; // For optimum start
	extern Array1D< ComponentSetPtData > CompSetPtEquip;

	// Clears the global data in DataHVACGlobals.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataHVACGlobals

} // EnergyPlus

#endif
