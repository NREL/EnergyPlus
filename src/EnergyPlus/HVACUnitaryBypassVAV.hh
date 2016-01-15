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

#ifndef HVACUnitaryBypassVAV_hh_INCLUDED
#define HVACUnitaryBypassVAV_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACUnitaryBypassVAV {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Compressor operation
	extern int const On; // Normal compressor operation
	extern int const Off; // Signal DXCoil that compressor should not run

	// DX Coils supported in this module
	extern int const CoilDX_CoolingSingleSpeed; // Coil:DX:CoolingBypassFactorEmpirical
	extern int const CoilDX_CoolingHXAssisted; // Coil:DX:HeatExchangerAssisted
	extern int const CoilDX_CoolingTwoStageWHumControl; // Coil:Cooling:DX:TwoStageWithHumidityControlMode
	// formerly (v3 and beyond) Coil:DX:MultiMode:CoolingEmpirical
	extern int const CoilDX_HeatingEmpirical; // Coil:DX:HeatingEmpirical
	extern int const Coil_HeatingGas; // Coil:Gas:Heating
	extern int const Coil_HeatingElectric; // Coil:Electric:Heating

	// Dehumidification control modes (DehumidControlMode) for Multimode units only
	extern int const DehumidControl_None;
	extern int const DehumidControl_Multimode;
	extern int const DehumidControl_CoolReheat;

	// Mode of operation
	extern int const CoolingMode; // System operating mode is cooling
	extern int const HeatingMode; // System operating mode is heating

	// Priority control mode (prioritized thermostat signal)
	extern int const CoolingPriority; // Controls CBVAV system based on cooling priority
	extern int const HeatingPriority; // Controls CBVAV system based on heating priority
	extern int const ZonePriority; // Controls CBVAV system based on zone priority

	// Airflow control for contant fan mode
	extern int const UseCompressorOnFlow; // Set compressor OFF air flow rate equal to compressor ON air flow rate
	extern int const UseCompressorOffFlow; // Set compressor OFF air flow rate equal to user defined value

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumCBVAV; // Number of CBVAV systems in input file
	extern Real64 CompOnMassFlow; // System air mass flow rate w/ compressor ON
	extern Real64 OACompOnMassFlow; // OA mass flow rate w/ compressor ON
	extern Real64 CompOffMassFlow; // System air mass flow rate w/ compressor OFF
	extern Real64 OACompOffMassFlow; // OA mass flow rate w/ compressor OFF
	extern Real64 CompOnFlowRatio; // fan flow ratio when coil on
	extern Real64 CompOffFlowRatio; // fan flow ratio when coil off
	extern Real64 FanSpeedRatio; // ratio of air flow ratio passed to fan object
	extern Real64 BypassDuctFlowFraction; // Fraction of unit mass flow that returns to inlet of CBVAV unit through bypass duct
	extern Real64 PartLoadFrac; // Compressor part-load fraction
	extern Real64 SaveCompressorPLR; // Holds DX compressor PLR from active DX coil
	extern Real64 TempSteamIn; // steam coil steam inlet temperature
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct CBVAVData
	{
		// Members
		// input data
		std::string Name; // Name of unit
		std::string UnitType; // Type of unit
		std::string Sched; // Availability schedule name
		int SchedPtr; // Index number to availability schedule
		Real64 MaxCoolAirVolFlow; // System air volumetric flow rate during cooling operation [m3/s]
		Real64 MaxHeatAirVolFlow; // System air volumetric flow rate during heating operation [m3/s]
		Real64 MaxNoCoolHeatAirVolFlow; // System air volumetric flow rate when no cooling or heating [m3/s]
		Real64 MaxCoolAirMassFlow; // System air mass flow rate during cooling operation [kg/s]
		Real64 MaxHeatAirMassFlow; // System air mass flow rate during heating operation [kg/s]
		Real64 MaxNoCoolHeatAirMassFlow; // System air mass flow rate when no cooling or heating [kg/s]
		Real64 CoolOutAirVolFlow; // OA volumetric flow rate during cooling operation [m3/s]
		Real64 HeatOutAirVolFlow; // OA volumetric flow rate during heating operation [m3/s]
		Real64 NoCoolHeatOutAirVolFlow; // OA volumetric flow rate when no cooling or heating [m3/s]
		Real64 CoolOutAirMassFlow; // OA mass flow rate during cooling operation [kg/s]
		Real64 HeatOutAirMassFlow; // OA mass flow rate during heating operation [kg/s]
		Real64 NoCoolHeatOutAirMassFlow; // OA mass flow rate when no cooling or heating [kg/s]
		int OutAirSchPtr; // Index number to outside air multiplier schedule
		int AirInNode; // Inlet air node number for CBVAV unit
		int AirOutNode; // Outlet air node number for CBVAV unit
		int CondenserNodeNum; // DX Coil condenser air inlet node number
		int MixerOutsideAirNode; // Outside air node number for OA mixer
		int MixerMixedAirNode; // Mixed air node number for OA mixer
		int MixerReliefAirNode; // Relief air node number for OA mixer
		int MixerInletAirNode; // Return air node number for OA mixer
		int SplitterOutletAirNode; // Air node number for splitter (last component outlet node)
		std::string OAMixType; // type of outside air mixer
		std::string OAMixName; // Name of OA mixer
		int OAMixIndex; // Index to OA mixer
		std::string FanName; // Name of fan
		std::string FanType; // Type of fan
		int FanPlace; // Fan placement is either blowthru (1) or drawthru (2)
		int FanType_Num; // Fan type number (see DataHVACGlobals)
		int FanIndex; // Index number to fan
		int FanOpModeSchedPtr; // Fan operating mode schedule pointer
		Real64 FanVolFlow; // Volumetric flow rate of system supply air fan [m3/s]
		Real64 HeatingSpeedRatio; // Fan speed ratio in heating mode
		Real64 CoolingSpeedRatio; // Fan speed ratio in cooling mode
		Real64 NoHeatCoolSpeedRatio; // Fan speed ratio when no cooling or heating
		bool CheckFanFlow; // Check fan volumetric flow versus system flow in init routine.
		std::string DXCoolCoilName; // Name of DX cooling coil
		std::string DXCoolCoilType; // Type of DX cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
		//               CoilSystem:Cooling:DX:HeatExchangerAssisted
		int DXCoolCoilType_Num; // Numeric equivalent for DX cooling coil type
		int CoolCoilCompIndex; // cooling coil component index number
		int DXCoolCoilIndexNum; // actual DX cooling coil index number
		int DXHeatCoilIndexNum; // actual DX heating coil index number
		std::string HeatCoilName; // Name of heating coil
		std::string HeatCoilType; // Type of heating coil,Coil:DX:HeatingEmpirical
		// Coil:Heater:Gas, Coil:Heater:Electric, Coil:Heater:Water
		// Coil:Heater:Steam
		int HeatCoilType_Num; // Numeric equivalent for DX heating coil type
		int HeatCoilIndex; // DX heating coil index number
		int OpMode; // mode of operation; 1=cycling fan, cycling compressor
		//                    2=continuous fan, cycling compresor
		int CoilControlNode; // heating coil hot water or steam inlet node
		int CoilOutletNode; // outlet node for hot water and steam coil
		int LoopNum; // plant loop index for water heating coil
		int LoopSide; // plant loop side  index for water heating coil
		int BranchNum; // plant loop branch index for water heating coil
		int CompNum; // plant loop component index for water heating coil
		int HotWaterCoilMaxIterIndex; // Index to recurring warning message
		int HotWaterCoilMaxIterIndex2; // Index to recurring warning message
		Real64 MaxHeatCoilFluidFlow; // water or steam mass flow rate for heating coil [kg/s]
		Real64 DesignHeatingCapacity; // design heating capacity of the heating coil
		Real64 DesignSuppHeatingCapacity; // Operating capacity of supplemental Heating Coil [W]
		Real64 MinOATCompressor; // Minimum OAT for compressor operation [C]
		Real64 MinLATCooling; // Minimum leaving air temp for compressor cooling operation [C]
		Real64 MaxLATHeating; // Maximum leaving air temp for heating operation [C]
		// Report data
		Real64 TotHeatEnergyRate; // Total heating output [W]
		Real64 TotHeatEnergy; // Total heating output [J]
		Real64 TotCoolEnergyRate; // Total cooling output [W]
		Real64 TotCoolEnergy; // Total cooling output [J]
		Real64 SensHeatEnergyRate; // Sensible heating output [W]
		Real64 SensHeatEnergy; // Sensible heating output [J]
		Real64 SensCoolEnergyRate; // Sensible cooling output [W]
		Real64 SensCoolEnergy; // Sensible cooling output [J]
		Real64 LatHeatEnergyRate; // Latent heating output [W]
		Real64 LatHeatEnergy; // Latent heating output [J]
		Real64 LatCoolEnergyRate; // Latent cooling output [W]
		Real64 LatCoolEnergy; // Latent cooling output [J]
		Real64 ElecPower; // Electricity consumed [W]
		Real64 ElecConsumption; // Electricity consumed [J]
		Real64 FanPartLoadRatio; // Fan part-load ratio for time step
		Real64 CompPartLoadRatio; // Compressor part-load ratio for time step
		int LastMode; // Last mode of operation, coolingmode or heatingmode
		int AirFlowControl; // Fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
		Real64 CompPartLoadFrac; // Compressor part load ratio
		int AirLoopNumber; // Air loop served by the CBVAV system
		int NumControlledZones;
		Array1D_int ControlledZoneNum; // Index to controlled zones
		Array1D_int ActualZoneNum; // Actual zone number of controlled zone
		Array1D_int ActualZoneNodeNum; // Actual zone node num of controlled zone
		Array1D_int CBVAVBoxOutletNode; // Outlet node of CBVAV Box in controlled zone
		Array1D_int ZoneSequenceCoolingNum; // Index to cooling sequence/priority for this zone
		Array1D_int ZoneSequenceHeatingNum; // Index to heating sequence/priority for this zone
		int PriorityControl; // Control mode - CoolingPriority, HeatingPriority, or ZonePriority
		int NumZonesCooled; // Number of zones requesting cooling
		int NumZonesHeated; // Number of zones requesting heating
		int PLRMaxIter; // Counter for recurring warning message
		int PLRMaxIterIndex; // Index to recurring warning message
		int DXCoilInletNode; // Inlet node number of DX cooling coil
		int DXCoilOutletNode; // Outlet node number of DX cooling coil
		int HeatingCoilInletNode; // Inlet node of heating coil
		int HeatingCoilOutletNode; // Outlet node of heating coil
		int FanInletNodeNum; // fan inlet node number
		Real64 OutletTempSetPoint; // Oulet node temperature setpoint [C]
		Real64 CoilTempSetPoint; // Coil oulet node temperature setpoint (inc. fan heat) [C]
		int HeatCoolMode; // System operating mode (0 = floating, 1 = cooling, 2 = heating)
		Real64 BypassMassFlowRate; // Bypass mass flow rate report variable [m3/s]
		int DehumidificationMode; // Dehumidification mode (0=normal, 1=enhanced)
		int DehumidControlType; // Dehumidification control type (currently only for multimode coil)
		bool HumRatMaxCheck; // Used in Init for warning messages
		int DXIterationExceeded; // Counter for DX coil messages
		int DXIterationExceededIndex; // Counter for DX coil messages
		int DXIterationFailed; // Counter for DX coil messages
		int DXIterationFailedIndex; // Counter for DX coil messages
		int HXDXIterationExceeded; // Counter for HX assisted DX coil messages
		int HXDXIterationExceededIndex; // Counter for HX assisted DX coil messages
		int HXDXIterationFailed; // Counter for HX assisted DX coil messages
		int HXDXIterationFailedIndex; // Counter for HX assisted DX coil messages
		int MMDXIterationExceeded; // Counter for multimode DX coil messages
		int MMDXIterationExceededIndex; // Counter for multimode DX coil messages
		int MMDXIterationFailed; // Counter for multimode DX coil messages
		int MMDXIterationFailedIndex; // Counter for multimode DX coil messages
		int DMDXIterationExceeded; // Counter for dehumidifying multimode DX coil messages
		int DMDXIterationExceededIndex; // Counter for dehumidifying multimode DX coil messages
		int DMDXIterationFailed; // Counter for dehumidifying multimode DX coil messages
		int DMDXIterationFailedIndex; // Counter for dehumidifying multimode DX coil messages
		int CRDXIterationExceeded; // Counter for cool reheat multimode DX coil messages
		int CRDXIterationExceededIndex; // Counter for cool reheat multimode DX coil messages
		int CRDXIterationFailed; // Counter for cool reheat multimode DX coil messages
		int CRDXIterationFailedIndex; // Counter for cool reheat multimode DX coil messages

		// Default Constructor
		CBVAVData() :
			SchedPtr( 0 ),
			MaxCoolAirVolFlow( 0.0 ),
			MaxHeatAirVolFlow( 0.0 ),
			MaxNoCoolHeatAirVolFlow( 0.0 ),
			MaxCoolAirMassFlow( 0.0 ),
			MaxHeatAirMassFlow( 0.0 ),
			MaxNoCoolHeatAirMassFlow( 0.0 ),
			CoolOutAirVolFlow( 0.0 ),
			HeatOutAirVolFlow( 0.0 ),
			NoCoolHeatOutAirVolFlow( 0.0 ),
			CoolOutAirMassFlow( 0.0 ),
			HeatOutAirMassFlow( 0.0 ),
			NoCoolHeatOutAirMassFlow( 0.0 ),
			OutAirSchPtr( 0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			CondenserNodeNum( 0 ),
			MixerOutsideAirNode( 0 ),
			MixerMixedAirNode( 0 ),
			MixerReliefAirNode( 0 ),
			MixerInletAirNode( 0 ),
			SplitterOutletAirNode( 0 ),
			OAMixIndex( 0 ),
			FanPlace( 0 ),
			FanType_Num( 0 ),
			FanIndex( 0 ),
			FanOpModeSchedPtr( 0 ),
			FanVolFlow( 0.0 ),
			HeatingSpeedRatio( 1.0 ),
			CoolingSpeedRatio( 1.0 ),
			NoHeatCoolSpeedRatio( 1.0 ),
			CheckFanFlow( true ),
			DXCoolCoilType_Num( 0 ),
			CoolCoilCompIndex( 0 ),
			DXCoolCoilIndexNum( 0 ),
			DXHeatCoilIndexNum( 0 ),
			HeatCoilType_Num( 0 ),
			HeatCoilIndex( 0 ),
			OpMode( 0 ),
			CoilControlNode( 0 ),
			CoilOutletNode( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			HotWaterCoilMaxIterIndex( 0 ),
			HotWaterCoilMaxIterIndex2( 0 ),
			MaxHeatCoilFluidFlow( 0.0 ),
			DesignHeatingCapacity( 0.0 ),
			DesignSuppHeatingCapacity( 0.0 ),
			MinOATCompressor( 0.0 ),
			MinLATCooling( 0.0 ),
			MaxLATHeating( 0.0 ),
			TotHeatEnergyRate( 0.0 ),
			TotHeatEnergy( 0.0 ),
			TotCoolEnergyRate( 0.0 ),
			TotCoolEnergy( 0.0 ),
			SensHeatEnergyRate( 0.0 ),
			SensHeatEnergy( 0.0 ),
			SensCoolEnergyRate( 0.0 ),
			SensCoolEnergy( 0.0 ),
			LatHeatEnergyRate( 0.0 ),
			LatHeatEnergy( 0.0 ),
			LatCoolEnergyRate( 0.0 ),
			LatCoolEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecConsumption( 0.0 ),
			FanPartLoadRatio( 0.0 ),
			CompPartLoadRatio( 0.0 ),
			LastMode( 0 ),
			AirFlowControl( 0 ),
			CompPartLoadFrac( 0.0 ),
			AirLoopNumber( 0 ),
			NumControlledZones( 0 ),
			PriorityControl( 0 ),
			NumZonesCooled( 0 ),
			NumZonesHeated( 0 ),
			PLRMaxIter( 0 ),
			PLRMaxIterIndex( 0 ),
			DXCoilInletNode( 0 ),
			DXCoilOutletNode( 0 ),
			HeatingCoilInletNode( 0 ),
			HeatingCoilOutletNode( 0 ),
			FanInletNodeNum( 0 ),
			OutletTempSetPoint( 0.0 ),
			CoilTempSetPoint( 0.0 ),
			HeatCoolMode( 0 ),
			BypassMassFlowRate( 0.0 ),
			DehumidificationMode( 0 ),
			DehumidControlType( 0 ),
			HumRatMaxCheck( true ),
			DXIterationExceeded( 0 ),
			DXIterationExceededIndex( 0 ),
			DXIterationFailed( 0 ),
			DXIterationFailedIndex( 0 ),
			HXDXIterationExceeded( 0 ),
			HXDXIterationExceededIndex( 0 ),
			HXDXIterationFailed( 0 ),
			HXDXIterationFailedIndex( 0 ),
			MMDXIterationExceeded( 0 ),
			MMDXIterationExceededIndex( 0 ),
			MMDXIterationFailed( 0 ),
			MMDXIterationFailedIndex( 0 ),
			DMDXIterationExceeded( 0 ),
			DMDXIterationExceededIndex( 0 ),
			DMDXIterationFailed( 0 ),
			DMDXIterationFailedIndex( 0 ),
			CRDXIterationExceeded( 0 ),
			CRDXIterationExceededIndex( 0 ),
			CRDXIterationFailed( 0 ),
			CRDXIterationFailedIndex( 0 )
		{}

	};

	// Object Data
	extern Array1D< CBVAVData > CBVAV;

	// Functions

	void
	SimUnitaryBypassVAV(
		std::string const & CompName, // Name of the CBVAV system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system time step
		int const AirLoopNum, // air loop index
		int & CompIndex // Index to changeover-bypass VAV system
	);

	void
	SimCBVAV(
		int const CBVAVNum, // Index of the current CBVAV system being simulated
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QZnReq, // Zone load for all zones served by this air loop system
		Real64 & QSensUnitOut, // Sensible delivered capacity [W]
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	);

	void
	GetCBVAV();

	void
	InitCBVAV(
		int const CBVAVNum, // Index of the current CBVAV unit being simulated
		bool const FirstHVACIteration, // TRUE if first HVAC iteration
		int const AirLoopNum, // air loop index
		Real64 & QZnReq, // Heating/Cooling load for all zones
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to average airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	);

	void
	SizeCBVAV( int const CBVAVNum ); // Index to CBVAV system

	void
	ControlCBVAVOutput(
		int const CBVAVNum, // Index to CBVAV system
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		Real64 & QZnReq, // Cooling or heating output needed by zone [W]
		Real64 & PartLoadFrac, // Unit part load fraction
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	);

	void
	CalcCBVAV(
		int const CBVAVNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		Real64 & PartLoadFrac, // Compressor part load fraction
		Real64 & LoadMet, // Load met by unit (W)
		Real64 & QZnReq, // Zone load (W)
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool const HXUnitOn // flag to enable heat exchanger
	);

	void
	GetZoneLoads(
		int const CBVAVNum, // Index to CBVAV unit being simulated
		Real64 & QZoneReq // Total zone load served by this air loop
	);

	Real64
	CalcSetPointTempTarget( int const CBVAVNumber ); // Index to changeover-bypass VAV system

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	Real64
	HXAssistDXCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	);

	void
	SetAverageAirFlow(
		int const CBVAVNum, // Index to CBVAV system
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to average airflow over timestep
		bool const FirstHVACIteration // Flag denoting the first pass on the air loop simulation
	);

	void
	ReportCBVAV( int const CBVAVNum ); // Index of the current CBVAV unit being simulated

	void
	CalcNonDXHeatingCoils(
		int const CBVAVNum, // Changeover bypass VAV unit index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 & HeatCoilLoad, // heating coil load to be met (Watts)
		int const FanMode, // fan operation mode
		Real64 & HeatCoilLoadmet // coil heating load met
	);

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
	);

} // HVACUnitaryBypassVAV

} // EnergyPlus

#endif
