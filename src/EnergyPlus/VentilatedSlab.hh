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

#ifndef VentilatedSlab_hh_INCLUDED
#define VentilatedSlab_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace VentilatedSlab {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// Module Object
	extern std::string const cMO_VentilatedSlab;

	// Parameters for outside air control types:
	extern int const Heating_ElectricCoilType;
	extern int const Heating_GasCoilType;
	extern int const Heating_WaterCoilType;
	extern int const Heating_SteamCoilType;
	extern int const Cooling_CoilWaterCooling;
	extern int const Cooling_CoilDetailedCooling;
	extern int const Cooling_CoilHXAssisted;
	extern int const VariablePercent;
	extern int const FixedTemperature;
	extern int const FixedOAControl;
	extern int const NotOperating; // Parameter for use with OperatingMode variable, set for no heating/cooling
	extern int const HeatingMode; // Parameter for use with OperatingMode variable, set for heating
	extern int const CoolingMode; // Parameter for use with OperatingMode variable, set for cooling
	//Ventilated Slab Configurations
	extern int const SlabOnly; // Air circulate through cores of slab only
	extern int const SlabAndZone; // Circulated Air is introduced to zone
	extern int const SeriesSlabs;
	//  Control Types
	extern int const MATControl; // Controls system using mean air temperature
	extern int const MRTControl; // Controls system using mean radiant temperature
	extern int const OPTControl; // Controls system using operative temperature
	extern int const ODBControl; // Controls system using outside air dry-bulb temperature
	extern int const OWBControl; // Controls system using outside air wet-bulb temperature
	extern int const SURControl; // Controls system using surface temperature !Phase2-A
	extern int const DPTZControl; // Controls system using dew-point temperature of zone!Phase2-A

	// coil operation
	extern int const On; // normal coil operation
	extern int const Off; // signal coil shouldn't run
	extern int const NoneOption;
	extern int const BothOption;
	extern int const HeatingOption;
	extern int const CoolingOption;
	extern int OperatingMode; // Used to keep track of whether system is in heating or cooling mode

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool HCoilOn; // TRUE if the heating coil (gas or electric especially) should be running
	extern int NumOfVentSlabs; // Number of ventilated slab in the input file
	extern Real64 OAMassFlowRate; // Outside air mass flow rate for the ventilated slab
	extern Array1D_double QRadSysSrcAvg; // Average source over the time step for a particular radiant surfaceD
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
	extern int MaxCloNumOfSurfaces; // Used to set allocate size in CalcClo routine
	extern Real64 QZnReq; // heating or cooling needed by system [watts]

	// Record keeping variables used to calculate QRadSysSrcAvg locally

	extern Array1D_double LastQRadSysSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool CheckEquipName;

	// Autosizing variables
	extern Array1D_bool MySizeFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE VentilatedSlab
	// PRIVATE UpdateVentilatedSlabValAvg

	// Types

	struct VentilatedSlabData
	{
		// Members
		// Input data
		std::string Name; // name of system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		// Variables for Delivery Config.
		Array1D_string ZName; // Name of zone the system is serving
		Array1D_int ZPtr; // Point to this zone in the Zone derived type
		std::string SurfListName; // Name of surface/surface list that is the radiant system
		int NumOfSurfaces; // Number of surfaces included in this system (coordinated control)
		Array1D_int SurfacePtr; // Pointer to the slabs in the Surface derived type
		Array1D_string SurfaceName; // Name of surfaces that are the radiant system (can be one or more)
		Array1D< Real64 > SurfaceFlowFrac; // Fraction of flow/pipe length for a particular surface
		Array1D< Real64 > CDiameter; // Number of core diameter
		Array1D< Real64 > CLength; // Number of core length
		Array1D< Real64 > CNumbers; // Number of core numbers
		Array1D_string SlabIn; // Name of node that is slab inlet node
		Array1D_string SlabOut; // Name of node that is slab outlet node
		Real64 TotalSurfaceArea; // Total surface area for all surfaces that are part of this system
		Real64 CoreDiameter; // tube diameter for embedded tubing
		Real64 CoreLength; // tube length embedded in radiant surface
		Real64 CoreNumbers; // tube length embedded in radiant surface
		int ControlType; // Control type for the system
		// (MAT, MRT, Op temp, ODB, OWB, DPTZ, Surf Temp.)
		int ReturnAirNode; // inlet air node number
		int RadInNode; // outlet air node number
		int ZoneAirInNode; // outlet air node number
		int FanOutletNode; // outlet node number for fan exit
		// (assumes fan is upstream of heating coil)
		int MSlabInNode;
		int MSlabOutNode;
		std::string FanName; // name of fan
		int Fan_Index;
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
		int SysConfg; // type of coil option; options are BOTH, HEATING, COOLING, AND NONE
		int CoilOption; // type of coil option; options are BOTH, HEATING, COOLING, AND NONE
		bool HCoilPresent; // .TRUE. if ventilated slab has a heating coil
		int HCoilType; // type of heating coil (water, gas, electric, etc.)
		std::string HCoilName; // name of heating coil
		std::string HCoilTypeCh; // type of heating coil (character string)
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
		std::string HotAirHiTempSched; // Schedule name for the highest Air temperature
		int HotAirHiTempSchedPtr; // Schedule index for the highest Air temperature
		std::string HotAirLoTempSched; // Schedule name for the lowest Air temperature
		int HotAirLoTempSchedPtr; // Schedule index for the lowest Air temperature
		std::string HotCtrlHiTempSched; // Schedule name for the highest control temperature
		// (where the lowest Air temperature is requested)
		int HotCtrlHiTempSchedPtr; // Schedule index for the highest control temperature
		// (where the lowest Air temperature is requested)
		std::string HotCtrlLoTempSched; // Schedule name for the lowest control temperature
		// (where the highest Air temperature is requested)
		int HotCtrlLoTempSchedPtr; // Schedule index for the lowest control temperature
		// (where the highest Air temperature is requested)
		bool CCoilPresent; // .TRUE. if ventilated slab has a cooling coil
		std::string CCoilName; // name of cooling coil
		std::string CCoilTypeCh; // type of cooling coil (character string)
		int CCoil_Index;
		std::string CCoilPlantName; // name of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
		std::string CCoilPlantType; // type of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
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
		int ColdCoilOutNodeNum; // chilled water coil out nod
		Real64 ColdControlOffset; // control tolerance
		int CWLoopNum; // index for plant loop with chilled water coil
		int CWLoopSide; // index for plant loop side for chilled water coil
		int CWBranchNum; // index for plant branch for chilled water coil
		int CWCompNum; // index for plant component for chilled water coil
		std::string ColdAirHiTempSched; // Schedule name for the highest air temperature
		int ColdAirHiTempSchedPtr; // Schedule index for the highest Air temperature
		std::string ColdAirLoTempSched; // Schedule name for the lowest Air temperature
		int ColdAirLoTempSchedPtr; // Schedule index for the lowest Air temperature
		std::string ColdCtrlHiTempSched; // Schedule name for the highest control temperature
		// (where the lowest Air temperature is requested)
		int ColdCtrlHiTempSchedPtr; // Schedule index for the highest control temperature
		// (where the lowest Air temperature is requested)
		std::string ColdCtrlLoTempSched; // Schedule name for the lowest control temperature
		// (where the highest Air temperature is requested)
		int ColdCtrlLoTempSchedPtr; // Schedule index for the lowest control temperature
		// (where the highest Air temperature is requested)
		int CondErrIndex; // Error index for recurring warning messages
		int EnrgyImbalErrIndex; // Error index for recurring warning messages
		int RadSurfNum; // Radiant Surface Number
		int MSlabIn; // Internal Slab Inlet Node Number
		int MSlabOut; // INternal Slab Outlet Node Number
		std::string DSSlabInNodeName;
		std::string DSSlabOutNodeName;
		// Report data
		Real64 DirectHeatLossPower; // system direct heat loss in W
		Real64 DirectHeatLossEnergy; // system direct heat loss in J
		Real64 DirectHeatGainPower; // system direct heat gain in W
		Real64 DirectHeatGainEnergy; // system direct heat gain in J
		Real64 TotalVentSlabRadPower;
		Real64 RadHeatingPower; // radiant heating output in watts
		Real64 RadHeatingEnergy; // radiant heating output in J
		Real64 RadCoolingPower; // radiant cooling output in watts
		Real64 RadCoolingEnergy; // radiant cooling output in J
		Real64 HeatCoilPower;
		Real64 HeatCoilEnergy;
		Real64 TotCoolCoilPower;
		Real64 TotCoolCoilEnergy;
		Real64 SensCoolCoilPower;
		Real64 SensCoolCoilEnergy;
		Real64 LateCoolCoilPower;
		Real64 LateCoolCoilEnergy;
		Real64 ElecFanPower;
		Real64 ElecFanEnergy;
		Real64 AirMassFlowRate; // Circulated air mass flow rate in kg/s
		Real64 AirVolFlow; // Circulated air volumetric flow rate in m3/s
		Real64 SlabInTemp; // Slab inlet temp in degree C
		Real64 SlabOutTemp; // Slab outlet temp in degree C
		Real64 ReturnAirTemp;
		Real64 FanOutletTemp; // FanOutlet temp in degree C
		Real64 ZoneInletTemp; // supply air temp
		std::string AvailManagerListName; // Name of an availability manager list object
		int AvailStatus;
		int HVACSizingIndex; // index of a HVACSizing object for a ventilator slab
		// Default Constructor
		VentilatedSlabData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			NumOfSurfaces( 0 ),
			TotalSurfaceArea( 0.0 ),
			CoreDiameter( 0.0 ),
			CoreLength( 0.0 ),
			CoreNumbers( 0.0 ),
			ControlType( 0 ),
			ReturnAirNode( 0 ),
			RadInNode( 0 ),
			ZoneAirInNode( 0 ),
			FanOutletNode( 0 ),
			MSlabInNode( 0 ),
			MSlabOutNode( 0 ),
			Fan_Index( 0 ),
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
			SysConfg( 0 ),
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
			HotAirHiTempSchedPtr( 0 ),
			HotAirLoTempSchedPtr( 0 ),
			HotCtrlHiTempSchedPtr( 0 ),
			HotCtrlLoTempSchedPtr( 0 ),
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
			ColdAirHiTempSchedPtr( 0 ),
			ColdAirLoTempSchedPtr( 0 ),
			ColdCtrlHiTempSchedPtr( 0 ),
			ColdCtrlLoTempSchedPtr( 0 ),
			CondErrIndex( 0 ),
			EnrgyImbalErrIndex( 0 ),
			RadSurfNum( 0 ),
			MSlabIn( 0 ),
			MSlabOut( 0 ),
			DirectHeatLossPower( 0.0 ),
			DirectHeatLossEnergy( 0.0 ),
			DirectHeatGainPower( 0.0 ),
			DirectHeatGainEnergy( 0.0 ),
			TotalVentSlabRadPower( 0.0 ),
			RadHeatingPower( 0.0 ),
			RadHeatingEnergy( 0.0 ),
			RadCoolingPower( 0.0 ),
			RadCoolingEnergy( 0.0 ),
			HeatCoilPower( 0.0 ),
			HeatCoilEnergy( 0.0 ),
			TotCoolCoilPower( 0.0 ),
			TotCoolCoilEnergy( 0.0 ),
			SensCoolCoilPower( 0.0 ),
			SensCoolCoilEnergy( 0.0 ),
			LateCoolCoilPower( 0.0 ),
			LateCoolCoilEnergy( 0.0 ),
			ElecFanPower( 0.0 ),
			ElecFanEnergy( 0.0 ),
			AirMassFlowRate( 0.0 ),
			AirVolFlow( 0.0 ),
			SlabInTemp( 0.0 ),
			SlabOutTemp( 0.0 ),
			ReturnAirTemp( 0.0 ),
			FanOutletTemp( 0.0 ),
			ZoneInletTemp( 0.0 ),
			AvailStatus( 0 ),
			HVACSizingIndex( 0 )
		{}

	};

	struct VentSlabNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		VentSlabNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< VentilatedSlabData > VentSlab;
	extern Array1D< VentSlabNumericFieldData > VentSlabNumericFields;

	// Functions

	void
	clear_state();

	void
	SimVentilatedSlab(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	);

	void
	GetVentilatedSlabInput();

	void
	InitVentilatedSlab(
		int const Item, // index for the current ventilated slab
		int const VentSlabZoneNum, // number of zone being served
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	SizeVentilatedSlab( int const Item );

	void
	CalcVentilatedSlab(
		int & Item, // number of the current ventilated slab being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // power supplied (W)
		Real64 & LatOutputProvided // latent capacity supplied (kg/s)
	);

	void
	CalcVentilatedSlabComps(
		int const Item, // system index in ventilated slab array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet // load met by the system (watts)
	);

	void
	CalcVentilatedSlabCoilOutput(
		int const Item, // system index in ventilated slab array
		Real64 & PowerMet, // power supplied (W)
		Real64 & LatOutputProvided // latent capacity supplied (kg/s)
	);

	void
	CalcVentilatedSlabRadComps(
		int const Item, // System index in ventilated slab array
		bool const FirstHVACIteration // flag for 1st HVAV iteration in the time step !unused1208
	);

	void
	SimVentSlabOAMixer( int const Item ); // System index in Ventilated Slab array

	void
	UpdateVentilatedSlab(
		int const Item, // Index for the ventilated slab under consideration within the derived types
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep !unused1208
	);

	Real64
	CalcVentSlabHXEffectTerm(
		int const Item, // Index number of radiant system under consideration
		Real64 const Temperature, // Temperature of air entering the radiant system, in C
		Real64 const AirMassFlow, // Mass flow rate of water in the radiant system, in kg/s
		Real64 const FlowFraction, // Mass flow rate fraction for this surface in the radiant system
		Real64 const CoreLength, // Length of tubing in the radiant system, in m
		Real64 const CoreDiameter, // Inside diameter of the tubing in the radiant system, in m
		Real64 const CoreNumbers
	);

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	ReportVentilatedSlab( int const Item ); // Index for the ventilated slab under consideration within the derived types

	//*****************************************************************************************

} // VentilatedSlab

} // EnergyPlus

#endif
