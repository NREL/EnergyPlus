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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <HVACUnitaryBypassVAV.hh>
#include <BranchNodeConnections.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <InputProcessor.hh>
#include <MixedAir.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace HVACUnitaryBypassVAV {

	// Module containing the routines for modeling changeover-bypass VAV systems

	// MODULE INFORMATION:
	//       AUTHOR         Richard Raustad
	//       DATE WRITTEN   July 2006
	//       MODIFIED       B. Nigusse, FSEC - January 2012 - Added steam and hot water heating coils
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms needed to simulate changeover-bypass
	// variable-air-volume (CBVAV) systems, which are considered "Air Loop Equipment" in EnergyPlus

	// METHODOLOGY EMPLOYED:
	// Units are modeled as a collection of components: outside air mixer,
	// supply air fan, DX cooing coil, DX/gas/elec heating coil, and variable volume boxes.
	// Control is accomplished by calculating the load in all zones to determine a mode of operation.
	// The system will either cool, heat, or operate based on fan mode selection.

	// The CBVAV system is initialized with no load (coils off) to determine the outlet temperature.
	// A setpoint temperature is calculated on FirstHVACIteration = TRUE to force one VAV box fully open.
	// Once the setpoint is calculated, the inlet node mass flow rate on FirstHVACIteration = FALSE is used to
	// determine the bypass fraction. The simulation converges quickly on mass flow rate. If the zone
	// temperatures float in the deadband, additional iterations are required to converge on mass flow rate.

	// REFERENCES:
	// "Temp & VVT Commercial Comfort Systems," Engineering Training Manual, Technical Development Program, Carrier Corp., 1995.
	// "VariTrac Changeover Bypass VAV (Tracker System CB)," VAV-PRC003-EN, Trane Company, June 2004.
	// "Ventilation for Changeover-Bypass VAV Systems," D. Stanke, ASHRAE Journal Vol. 46, No. 11, November 2004.
	//  Lawrence Berkeley Laboratory. Nov. 1993. DOE-2 Supplement Version 2.1E, Winklemann et.al.

	// OTHER NOTES: None

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DoingSizing;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::DXElecCoolingPower;
	using DataHVACGlobals::DXElecHeatingPower;
	using DataHVACGlobals::OnOffFanPartLoadFraction;
	using DataHVACGlobals::ElecHeatingCoilPower;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::CycFanCycCoil;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::DrawThru;
	using DataHVACGlobals::BlowThru;
	using DataHVACGlobals::Coil_HeatingWater;
	using DataHVACGlobals::Coil_HeatingSteam;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::StdRhoAir;
	using DXCoils::DXCoilPartLoadRatio;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Compressor operation
	int const On( 1 ); // Normal compressor operation
	int const Off( 0 ); // Signal DXCoil that compressor should not run

	// DX Coils supported in this module
	int const CoilDX_CoolingSingleSpeed( 1 ); // Coil:DX:CoolingBypassFactorEmpirical
	int const CoilDX_CoolingHXAssisted( 2 ); // Coil:DX:HeatExchangerAssisted
	int const CoilDX_CoolingTwoStageWHumControl( 3 ); // Coil:Cooling:DX:TwoStageWithHumidityControlMode
	// formerly (v3 and beyond) Coil:DX:MultiMode:CoolingEmpirical
	int const CoilDX_HeatingEmpirical( 4 ); // Coil:DX:HeatingEmpirical
	int const Coil_HeatingGas( 5 ); // Coil:Gas:Heating
	int const Coil_HeatingElectric( 6 ); // Coil:Electric:Heating

	// Dehumidification control modes (DehumidControlMode) for Multimode units only
	int const DehumidControl_None( 0 );
	int const DehumidControl_Multimode( 1 );
	int const DehumidControl_CoolReheat( 2 );

	// Mode of operation
	int const CoolingMode( 1 ); // System operating mode is cooling
	int const HeatingMode( 2 ); // System operating mode is heating

	// Priority control mode (prioritized thermostat signal)
	int const CoolingPriority( 1 ); // Controls CBVAV system based on cooling priority
	int const HeatingPriority( 2 ); // Controls CBVAV system based on heating priority
	int const ZonePriority( 3 ); // Controls CBVAV system based on zone priority

	// Airflow control for contant fan mode
	int const UseCompressorOnFlow( 1 ); // Set compressor OFF air flow rate equal to compressor ON air flow rate
	int const UseCompressorOffFlow( 2 ); // Set compressor OFF air flow rate equal to user defined value

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	int NumCBVAV( 0 ); // Number of CBVAV systems in input file
	Real64 CompOnMassFlow( 0.0 ); // System air mass flow rate w/ compressor ON
	Real64 OACompOnMassFlow( 0.0 ); // OA mass flow rate w/ compressor ON
	Real64 CompOffMassFlow( 0.0 ); // System air mass flow rate w/ compressor OFF
	Real64 OACompOffMassFlow( 0.0 ); // OA mass flow rate w/ compressor OFF
	Real64 CompOnFlowRatio( 0.0 ); // fan flow ratio when coil on
	Real64 CompOffFlowRatio( 0.0 ); // fan flow ratio when coil off
	Real64 FanSpeedRatio( 0.0 ); // ratio of air flow ratio passed to fan object
	Real64 BypassDuctFlowFraction( 0.0 ); // Fraction of unit mass flow that returns to inlet of CBVAV unit through bypass duct
	Real64 PartLoadFrac( 0.0 ); // Compressor part-load fraction
	Real64 SaveCompressorPLR( 0.0 ); // Holds DX compressor PLR from active DX coil
	Real64 TempSteamIn( 100.0 ); // steam coil steam inlet temperature
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< CBVAVData > CBVAV;

	// Functions

	void
	SimUnitaryBypassVAV(
		std::string const & CompName, // Name of the CBVAV system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system time step
		int const AirLoopNum, // air loop index
		int & CompIndex // Index to changeover-bypass VAV system
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a changeover-bypass VAV system. Called from SimAirServingZones.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CBVAVNum; // Index of CBVAV system being simulated
		static bool GetInputFlag( true ); // First time, input is "gotten"
		Real64 OnOffAirFlowRatio; // Ratio of compressor ON airflow to average airflow over timestep
		Real64 QUnitOut; // Sensible capacity delivered by this air loop system
		Real64 QZnLoad; // Zone load required by all zones served by this air loop system
		bool HXUnitOn; // flag to enable heat exchanger

		// FLOW

		// First time SimUnitaryBypassVAV is called, get the input for all the CBVAVs
		if ( GetInputFlag ) {
			GetCBVAV();
			GetInputFlag = false;
		}

		// Find the correct changeover-bypass VAV unit
		if ( CompIndex == 0 ) {
			CBVAVNum = FindItemInList( CompName, CBVAV );
			if ( CBVAVNum == 0 ) {
				ShowFatalError( "SimUnitaryBypassVAV: Unit not found=" + CompName );
			}
			CompIndex = CBVAVNum;
		} else {
			CBVAVNum = CompIndex;
			if ( CBVAVNum > NumCBVAV || CBVAVNum < 1 ) {
				ShowFatalError( "SimUnitaryBypassVAV:  Invalid CompIndex passed=" + TrimSigDigits( CBVAVNum ) + ", Number of Units=" + TrimSigDigits( NumCBVAV ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( CBVAVNum ) ) {
				if ( CompName != CBVAV( CBVAVNum ).Name ) {
					ShowFatalError( "SimUnitaryBypassVAV: Invalid CompIndex passed=" + TrimSigDigits( CBVAVNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + CBVAV( CBVAVNum ).Name );
				}
				CheckEquipName( CBVAVNum ) = false;
			}
		}

		OnOffAirFlowRatio = 0.0;
		HXUnitOn = true;

		// Initialize the changeover-bypass VAV system
		InitCBVAV( CBVAVNum, FirstHVACIteration, AirLoopNum, QZnLoad, OnOffAirFlowRatio, HXUnitOn );

		// Simulate the unit
		SimCBVAV( CBVAVNum, FirstHVACIteration, QZnLoad, QUnitOut, OnOffAirFlowRatio, HXUnitOn );

		// Report the result of the simulation
		ReportCBVAV( CBVAVNum );

	}

	void
	SimCBVAV(
		int const CBVAVNum, // Index of the current CBVAV system being simulated
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QZnReq, // Zone load for all zones served by this air loop system
		Real64 & QSensUnitOut, // Sensible delivered capacity [W]
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a changeover-bypass VAV system.

		// METHODOLOGY EMPLOYED:
		// Calls ControlCBVAVOutput to obtain the desired unit output

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		Real64 PartLoadFrac;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool UnitOn; // TRUE if unit is on
		int OutletNode; // CBVAV air outlet node
		int InletNode; // CBVAV air inlet node
		Real64 QTotUnitOut; // Total delivered capacity [W]
		Real64 AirMassFlow; // Air mass flow rate [kg/s]
		Real64 HeatingPower; // Power consumption of DX heating coil or electric heating coil [W]
		Real64 MinOutletHumRat; // Minimum of inlet and outlet air humidity ratio [kg/kg]

		// zero the fan and DX coils electricity consumption
		FanElecPower = 0.0;
		DXElecCoolingPower = 0.0;
		DXElecHeatingPower = 0.0;
		ElecHeatingCoilPower = 0.0;
		SaveCompressorPLR = 0.0;

		// initialize local variables
		UnitOn = true;
		QSensUnitOut = 0.0;
		OutletNode = CBVAV( CBVAVNum ).AirOutNode;
		InletNode = CBVAV( CBVAVNum ).AirInNode;
		AirMassFlow = Node( InletNode ).MassFlowRate;
		PartLoadFrac = 0.0;

		// set the on/off flags
		if ( CBVAV( CBVAVNum ).OpMode == CycFanCycCoil ) {
			// cycling unit only runs if there is a cooling or heating load.
			if ( CBVAV( CBVAVNum ).HeatCoolMode == 0 || AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
			}
		} else if ( CBVAV( CBVAVNum ).OpMode == ContFanCycCoil ) {
			// continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
			if ( AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
			}
		}

		OnOffFanPartLoadFraction = 1.0;

		if ( UnitOn ) {
			ControlCBVAVOutput( CBVAVNum, FirstHVACIteration, QZnReq, PartLoadFrac, OnOffAirFlowRatio, HXUnitOn );
		} else {
			CalcCBVAV( CBVAVNum, FirstHVACIteration, PartLoadFrac, QSensUnitOut, QZnReq, OnOffAirFlowRatio, HXUnitOn );
		}

		// calculate delivered capacity
		AirMassFlow = Node( OutletNode ).MassFlowRate;

		QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );

		MinOutletHumRat = min( Node( InletNode ).HumRat, Node( OutletNode ).HumRat );

		QSensUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinOutletHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinOutletHumRat ) );

		// report variables
		CBVAV( CBVAVNum ).CompPartLoadRatio = SaveCompressorPLR;
		if ( UnitOn ) {
			CBVAV( CBVAVNum ).FanPartLoadRatio = 1.0;
		} else {
			CBVAV( CBVAVNum ).FanPartLoadRatio = 0.0;
		}

		CBVAV( CBVAVNum ).TotCoolEnergyRate = std::abs( min( 0.0, QTotUnitOut ) );
		CBVAV( CBVAVNum ).TotHeatEnergyRate = std::abs( max( 0.0, QTotUnitOut ) );
		CBVAV( CBVAVNum ).SensCoolEnergyRate = std::abs( min( 0.0, QSensUnitOut ) );
		CBVAV( CBVAVNum ).SensHeatEnergyRate = std::abs( max( 0.0, QSensUnitOut ) );
		CBVAV( CBVAVNum ).LatCoolEnergyRate = std::abs( min( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );
		CBVAV( CBVAVNum ).LatHeatEnergyRate = std::abs( max( 0.0, ( QTotUnitOut - QSensUnitOut ) ) );

		if ( CBVAV( CBVAVNum ).HeatCoilType_Num == CoilDX_HeatingEmpirical ) {
			HeatingPower = DXElecHeatingPower;
		} else if ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingElectric ) {
			HeatingPower = ElecHeatingCoilPower;
		} else {
			HeatingPower = 0.0;
		}

		CBVAV( CBVAVNum ).ElecPower = FanElecPower + DXElecCoolingPower + HeatingPower;

	}

	void
	GetCBVAV()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       Bereket Nigusse, FSEC, April 2011: added OA Mixer object type
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for changeover-bypass VAV systems and stores it in CBVAV data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::GetFanType;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using General::TrimSigDigits;
		auto & GetMinOATDXCoilCompressor( DXCoils::GetMinOATCompressor );
		auto & GetDXCoilInletNode( DXCoils::GetCoilInletNode );
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		auto & GetDXCoilCondenserInletNode( DXCoils::GetCoilCondenserInletNode );
		using DXCoils::GetDXCoilIndex;
		using MixedAir::GetOAMixerNodeNumbers;
		using DataSizing::AutoSize;
		using DataAirLoop::AirToZoneNodeInfo;
		using HeatingCoils::GetCoilInletNode;
		using HeatingCoils::GetCoilOutletNode;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using DataAirSystems::PrimaryAirSystem;
		using ScheduleManager::CheckScheduleValueMinMax;
		using ScheduleManager::GetScheduleIndex;
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataHVACGlobals::NumPrimaryAirSys;
		using NodeInputManager::GetOnlySingleNode;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataZoneEquipment::AirDistUnit_Num;
		using DataZoneEquipment::DirectAir_Num;
		using BranchNodeConnections::SetUpCompSets;
		using BranchNodeConnections::TestCompSet;
		using DataZoneControls::TempControlledZone;
		using DataZoneControls::NumTempControlledZones;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		auto & GetHXDXCoilInletNode( HVACHXAssistedCoolingCoil::GetCoilInletNode );
		auto & GetHXDXCoilOutletNode( HVACHXAssistedCoolingCoil::GetCoilOutletNode );

		auto & GetSteamCoilAirInletNode( SteamCoils::GetCoilAirInletNode );
		using SteamCoils::GetSteamCoilIndex;
		using SteamCoils::GetCoilAirOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		using SteamCoils::GetTypeOfCoil;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilMaxWaterFlowRate;
		auto & GetWaterCoilInletNode( WaterCoils::GetCoilInletNode );
		auto & GetWaterCoilOutletNode( WaterCoils::GetCoilOutletNode );
		using FluidProperties::GetSatDensityRefrig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const getUnitaryHeatCoolVAVChangeoverBypass( "GetUnitaryHeatCool:VAVChangeoverBypass" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CBVAVIndex; // Loop index
		int CBVAVNum; // Current CBVAV number
		Array1D_string Alphas( 19 ); // Alpha items for object
		Array1D< Real64 > Numbers( 11 ); // Numeric items for object
		std::string CompSetFanInlet; // Used in SetUpCompSets call
		std::string CompSetCoolInlet; // Used in SetUpCompSets call
		std::string CompSetFanOutlet; // Used in SetUpCompSets call
		std::string CompSetCoolOutlet; // Used in SetUpCompSets call
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static bool DXErrorsFound( false ); // Set to true if errors in get coil input
		//unused0509  LOGICAL                        :: FanErrorsFound=.FALSE. ! Set to true if errors in get fan input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // Object type for getting and error messages
		static bool FanErrFlag( false ); // Error flag returned during CALL to GetFanType
		static bool errFlag( false ); // Error flag returned during CALL to mining functions
		int FanIndex; // Index to CBVAV's supply air fan
		Real64 FanVolFlow; // Maximum air volumetric flow rate of fan
		int AirLoopNum; // Index to air loop served by this system
		int AirLoopZoneNum; // Index to controlled zone
		int BranchNum; // Index to branch containing this system
		int CompNum; // Index to this system
		Array1D_int OANodeNums( 4 ); // Node numbers of OA mixer (OA, EA, RA, MA)
		std::string HXDXCoolCoilName; // Name of DX cooling coil used with Heat Exchanger Assisted Cooling Coil
		std::string MixerInletNodeName; // Name of mixer inlet node
		std::string SplitterOutletNodeName; // Name of splitter outlet node
		int ControlNodeNum; // Number of control zone when humidity control is specified
		int TstatZoneNum; // Index to thermostats
		bool FoundTstatZone; // TRUE if thermostat found in controlled zone
		bool OANodeErrFlag; // TRUE if DX Coil condenser node is not found
		bool DXCoilErrFlag; // used in warning messages
		Array1D_string cAlphaFields( 19 ); // Alpha field names
		Array1D_string cNumericFields( 11 ); // Numeric field names
		Array1D_bool lAlphaBlanks( 19 ); // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks( 11 ); // Logical array, numeric field input BLANK = .TRUE.
		static int EquipNum( 0 ); // local do loop index for equipment listed for a zone
		int HeatCoilInletNodeNum; // Heating coil air inlet node number
		int HeatCoilOutletNodeNum; // Heating coil air outlet node number
		int SteamIndex; // steam coil index
		Real64 SteamDensity; // steam coil steam density

		Alphas = "";
		Numbers = 0.0;
		cAlphaFields = "";
		cNumericFields = "";
		lAlphaBlanks = true;
		lNumericBlanks = true;

		// find the number of each type of CBVAV unit
		CurrentModuleObject = "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass";
		NumCBVAV = GetNumObjectsFound( CurrentModuleObject );

		// allocate the data structures
		CBVAV.allocate( NumCBVAV );
		CheckEquipName.dimension( NumCBVAV, true );

		// loop over CBVAV units; get and load the input data
		for ( CBVAVIndex = 1; CBVAVIndex <= NumCBVAV; ++CBVAVIndex ) {
			HeatCoilInletNodeNum = 0;
			HeatCoilOutletNodeNum = 0;
			GetObjectItem( CurrentModuleObject, CBVAVIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			CBVAVNum = CBVAVIndex;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), CBVAV, CBVAVNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			CBVAV( CBVAVNum ).Name = Alphas( 1 );
			CBVAV( CBVAVNum ).UnitType = CurrentModuleObject;
			CBVAV( CBVAVNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				CBVAV( CBVAVNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				CBVAV( CBVAVNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer (index number)
				if ( CBVAV( CBVAVNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + ' ' + cAlphaFields( 2 ) + " not found = " + Alphas( 2 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					ErrorsFound = true;
				}
			}

			CBVAV( CBVAVNum ).MaxCoolAirVolFlow = Numbers( 1 );
			if ( CBVAV( CBVAVNum ).MaxCoolAirVolFlow <= 0.0 && CBVAV( CBVAVNum ).MaxCoolAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 1 ) + " = " + TrimSigDigits( Numbers( 1 ), 7 ) );
				ShowContinueError( cNumericFields( 1 ) + " must be greater than zero." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).MaxHeatAirVolFlow = Numbers( 2 );
			if ( CBVAV( CBVAVNum ).MaxHeatAirVolFlow <= 0.0 && CBVAV( CBVAVNum ).MaxHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 2 ) + " = " + TrimSigDigits( Numbers( 2 ), 7 ) );
				ShowContinueError( cNumericFields( 2 ) + " must be greater than zero." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow = Numbers( 3 );
			if ( CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow < 0.0 && CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 3 ) + " = " + TrimSigDigits( Numbers( 3 ), 7 ) );
				ShowContinueError( cNumericFields( 3 ) + " must be greater than or equal to zero." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).CoolOutAirVolFlow = Numbers( 4 );
			if ( CBVAV( CBVAVNum ).CoolOutAirVolFlow < 0.0 && CBVAV( CBVAVNum ).CoolOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 4 ) + " = " + TrimSigDigits( Numbers( 4 ), 7 ) );
				ShowContinueError( cNumericFields( 4 ) + " must be greater than or equal to zero." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).HeatOutAirVolFlow = Numbers( 5 );
			if ( CBVAV( CBVAVNum ).HeatOutAirVolFlow < 0.0 && CBVAV( CBVAVNum ).HeatOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 5 ) + " = " + TrimSigDigits( Numbers( 5 ), 7 ) );
				ShowContinueError( cNumericFields( 5 ) + " must be greater than or equal to zero." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow = Numbers( 6 );
			if ( CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow < 0.0 && CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow != AutoSize ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cNumericFields( 6 ) + " = " + TrimSigDigits( Numbers( 6 ), 7 ) );
				ShowContinueError( cNumericFields( 6 ) + " must be greater than or equal to zero." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).OutAirSchPtr = GetScheduleIndex( Alphas( 3 ) ); // convert schedule name to pointer (index number)
			if ( CBVAV( CBVAVNum ).OutAirSchPtr != 0 ) {
				if ( ! CheckScheduleValueMinMax( CBVAV( CBVAVNum ).OutAirSchPtr, "<", 0.0, ">", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( "The schedule values in " + cAlphaFields( 3 ) + " must be 0 to 1." );
					ErrorsFound = true;
				}
			}

			CBVAV( CBVAVNum ).AirInNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			MixerInletNodeName = Alphas( 5 );
			SplitterOutletNodeName = Alphas( 6 );

			CBVAV( CBVAVNum ).AirOutNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			CBVAV( CBVAVNum ).MixerInletAirNode = GetOnlySingleNode( MixerInletNodeName, ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );

			CBVAV( CBVAVNum ).MixerInletAirNode = GetOnlySingleNode( MixerInletNodeName, ErrorsFound, CurrentModuleObject, Alphas( 1 ) + "_Mixer", NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			CBVAV( CBVAVNum ).SplitterOutletAirNode = GetOnlySingleNode( SplitterOutletNodeName, ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );

			CBVAV( CBVAVNum ).SplitterOutletAirNode = GetOnlySingleNode( SplitterOutletNodeName, ErrorsFound, CurrentModuleObject, Alphas( 1 ) + "_Splitter", NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			CBVAV( CBVAVNum ).OAMixType = Alphas( 8 );
			CBVAV( CBVAVNum ).OAMixName = Alphas( 9 );

			errFlag = false;
			ValidateComponent( CBVAV( CBVAVNum ).OAMixType, CBVAV( CBVAVNum ).OAMixName, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + CBVAV( CBVAVNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				// Get OA Mixer node numbers
				OANodeNums = GetOAMixerNodeNumbers( CBVAV( CBVAVNum ).OAMixName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "that was specified in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name." );
					ErrorsFound = true;
				} else {
					CBVAV( CBVAVNum ).MixerOutsideAirNode = OANodeNums( 1 );
					CBVAV( CBVAVNum ).MixerReliefAirNode = OANodeNums( 2 );
					// CBVAV(CBVAVNum)%MixerInletAirNode  = OANodeNums(3)
					CBVAV( CBVAVNum ).MixerMixedAirNode = OANodeNums( 4 );
				}
			}

			if ( CBVAV( CBVAVNum ).MixerInletAirNode != OANodeNums( 3 ) ) {
				ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( 5 ) + " = " + MixerInletNodeName + '.' );
				ShowContinueError( cAlphaFields( 5 ) + " must be the same as the return air stream node specified in the OutdoorAir:Mixer object." );
				ErrorsFound = true;
			}

			if ( CBVAV( CBVAVNum ).MixerInletAirNode == CBVAV( CBVAVNum ).AirInNode ) {
				ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( 5 ) + " = " + MixerInletNodeName + '.' );
				ShowContinueError( cAlphaFields( 5 ) + " must be different than the " + cAlphaFields( 4 ) + '.' );
				ErrorsFound = true;
			}

			if ( CBVAV( CBVAVNum ).SplitterOutletAirNode == CBVAV( CBVAVNum ).AirOutNode ) {
				ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( 6 ) + " = " + SplitterOutletNodeName + '.' );
				ShowContinueError( cAlphaFields( 6 ) + " must be different than the " + cAlphaFields( 7 ) + '.' );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).FanType = Alphas( 10 );
			CBVAV( CBVAVNum ).FanName = Alphas( 11 );

			if ( SameString( Alphas( 12 ), "BlowThrough" ) ) {
				CBVAV( CBVAVNum ).FanPlace = BlowThru;
			} else if ( SameString( Alphas( 12 ), "DrawThrough" ) ) {
				CBVAV( CBVAVNum ).FanPlace = DrawThru;
			} else {
				ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 12 ) + " = " + Alphas( 12 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			if ( CBVAV( CBVAVNum ).FanPlace == DrawThru ) {
				if ( CBVAV( CBVAVNum ).SplitterOutletAirNode != GetFanOutletNode( CBVAV( CBVAVNum ).FanType, CBVAV( CBVAVNum ).FanName, ErrorsFound ) ) {
					ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( 6 ) + " = " + SplitterOutletNodeName + '.' );
					ShowContinueError( cAlphaFields( 6 ) + " must be the same as the fan outlet node specified in " + cAlphaFields( 10 ) + " = " + CBVAV( CBVAVNum ).FanType + ": " + CBVAV( CBVAVNum ).FanName + " when draw through " + cAlphaFields( 11 ) + " is selected." );
					ErrorsFound = true;
				}
			}

			GetFanType( CBVAV( CBVAVNum ).FanName, CBVAV( CBVAVNum ).FanType_Num, FanErrFlag, CurrentModuleObject, CBVAV( CBVAVNum ).Name );
			if ( FanErrFlag ) {
				ShowContinueError( " illegal " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
				ErrorsFound = true;
				FanVolFlow = 9999.0;
			} else {
				CBVAV( CBVAVNum ).FanInletNodeNum = GetFanInletNode( CBVAV( CBVAVNum ).FanType, CBVAV( CBVAVNum ).FanName, FanErrFlag );
				GetFanIndex( CBVAV( CBVAVNum ).FanName, FanIndex, FanErrFlag );
				GetFanVolFlow( FanIndex, CBVAV( CBVAVNum ).FanVolFlow );
				FanVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
			}

			if ( FanVolFlow != AutoSize ) {
				if ( FanVolFlow < CBVAV( CBVAVNum ).MaxCoolAirVolFlow && CBVAV( CBVAVNum ).MaxCoolAirVolFlow != AutoSize ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( FanVolFlow, 7 ) + " in " + cAlphaFields( 11 ) + " = " + CBVAV( CBVAVNum ).FanName + " is less than the " + cNumericFields( 1 ) );
					ShowContinueError( ' ' + cNumericFields( 1 ) + " is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).MaxCoolAirVolFlow = FanVolFlow;
				}
				if ( FanVolFlow < CBVAV( CBVAVNum ).MaxHeatAirVolFlow && CBVAV( CBVAVNum ).MaxHeatAirVolFlow != AutoSize ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( FanVolFlow, 7 ) + " in " + cAlphaFields( 11 ) + " = " + CBVAV( CBVAVNum ).FanName + " is less than the " + cNumericFields( 2 ) );
					ShowContinueError( ' ' + cNumericFields( 2 ) + " is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).MaxHeatAirVolFlow = FanVolFlow;
				}
			}

			//   only check that OA flow in cooling is >= SA flow in cooling when they are not autosized
			if ( CBVAV( CBVAVNum ).CoolOutAirVolFlow > CBVAV( CBVAVNum ).MaxCoolAirVolFlow && CBVAV( CBVAVNum ).CoolOutAirVolFlow != AutoSize && CBVAV( CBVAVNum ).MaxCoolAirVolFlow != AutoSize ) {
				ShowWarningError( CurrentModuleObject + ": " + cNumericFields( 4 ) + " cannot be greater than " + cNumericFields( 1 ) );
				ShowContinueError( ' ' + cNumericFields( 4 ) + " is reset to the fan flow rate and the simulation continues." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).CoolOutAirVolFlow = FanVolFlow;
			}

			//   only check that SA flow in heating is >= OA flow in heating when they are not autosized
			if ( CBVAV( CBVAVNum ).HeatOutAirVolFlow > CBVAV( CBVAVNum ).MaxHeatAirVolFlow && CBVAV( CBVAVNum ).HeatOutAirVolFlow != AutoSize && CBVAV( CBVAVNum ).MaxHeatAirVolFlow != AutoSize ) {
				ShowWarningError( CurrentModuleObject + ": " + cNumericFields( 5 ) + " cannot be greater than " + cNumericFields( 2 ) );
				ShowContinueError( ' ' + cNumericFields( 5 ) + " is reset to the fan flow rate and the simulation continues." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).HeatOutAirVolFlow = FanVolFlow;
			}

			if ( SameString( Alphas( 14 ), "Coil:Cooling:DX:SingleSpeed" ) || SameString( Alphas( 14 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) || SameString( Alphas( 14 ), "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {

				CBVAV( CBVAVNum ).DXCoolCoilType = Alphas( 14 );
				CBVAV( CBVAVNum ).DXCoolCoilName = Alphas( 15 );

				if ( SameString( Alphas( 14 ), "Coil:Cooling:DX:SingleSpeed" ) ) {
					CBVAV( CBVAVNum ).DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed;
					CBVAV( CBVAVNum ).DXCoilInletNode = GetDXCoilInletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXErrorsFound );
					CBVAV( CBVAVNum ).DXCoilOutletNode = GetDXCoilOutletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXErrorsFound );
					if ( DXErrorsFound ) {
						ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
						ShowContinueError( "Coil:Cooling:DX:SingleSpeed \"" + CBVAV( CBVAVNum ).DXCoolCoilName + "\" not found." );
						ErrorsFound = true;
					} else {

						DXCoilErrFlag = false;
						GetDXCoilIndex( CBVAV( CBVAVNum ).DXCoolCoilName, CBVAV( CBVAVNum ).DXCoolCoilIndexNum, DXCoilErrFlag, CBVAV( CBVAVNum ).DXCoolCoilType );
						if ( DXCoilErrFlag ) ShowContinueError( "...occurs in " + CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );

						//         Mine outdoor condenser node from DX coil object
						OANodeErrFlag = false;
						CBVAV( CBVAVNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, OANodeErrFlag );
						if ( OANodeErrFlag ) ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					}
				} else if ( SameString( Alphas( 14 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
					CBVAV( CBVAVNum ).DXCoolCoilType_Num = CoilDX_CoolingHXAssisted;
					HXDXCoolCoilName = GetHXDXCoilName( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXErrorsFound );
					CBVAV( CBVAVNum ).DXCoilInletNode = GetHXDXCoilInletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXErrorsFound );
					CBVAV( CBVAVNum ).DXCoilOutletNode = GetHXDXCoilOutletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXErrorsFound );
					if ( DXErrorsFound ) {
						ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
						ShowContinueError( "CoilSystem:Cooling:DX:HeatExchangerAssisted \"" + CBVAV( CBVAVNum ).DXCoolCoilName + "\" not found." );
						ErrorsFound = true;
					} else {

						DXCoilErrFlag = false;
						GetDXCoilIndex( GetHXDXCoilName( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXCoilErrFlag ), CBVAV( CBVAVNum ).DXCoolCoilIndexNum, DXCoilErrFlag, "Coil:Cooling:DX:SingleSpeed" );
						if ( DXCoilErrFlag ) ShowContinueError( "...occurs in " + CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );

						//         Mine outdoor condenser node from DX coil through HXAssistedDXCoil object
						OANodeErrFlag = false;
						CBVAV( CBVAVNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( "Coil:Cooling:DX:SingleSpeed", HXDXCoolCoilName, OANodeErrFlag );
						if ( OANodeErrFlag ) ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					}
				} else if ( SameString( Alphas( 14 ), "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {
					CBVAV( CBVAVNum ).DXCoolCoilType_Num = CoilDX_CoolingTwoStageWHumControl;
					CBVAV( CBVAVNum ).DXCoilInletNode = GetDXCoilInletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXErrorsFound );
					CBVAV( CBVAVNum ).DXCoilOutletNode = GetDXCoilOutletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, DXErrorsFound );
					if ( DXErrorsFound ) {
						ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
						ShowContinueError( "Coil:Cooling:DX:TwoStageWithHumidityControlMode \"" + CBVAV( CBVAVNum ).DXCoolCoilName + "\" not found." );
						ErrorsFound = true;
					} else {

						DXCoilErrFlag = false;
						GetDXCoilIndex( CBVAV( CBVAVNum ).DXCoolCoilName, CBVAV( CBVAVNum ).DXCoolCoilIndexNum, DXCoilErrFlag, CBVAV( CBVAVNum ).DXCoolCoilType );
						if ( DXCoilErrFlag ) ShowContinueError( "...occurs in " + CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );

						//         Mine outdoor condenser node from multimode DX coil object
						OANodeErrFlag = false;
						CBVAV( CBVAVNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, OANodeErrFlag );
						if ( OANodeErrFlag ) ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					}
				}

			} else {
				ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( 14 ) + " = " + Alphas( 14 ) );
				ErrorsFound = true;
			}

			CBVAV( CBVAVNum ).FanOpModeSchedPtr = GetScheduleIndex( Alphas( 13 ) ); // convert schedule name to pointer (index number)
			if ( CBVAV( CBVAVNum ).FanOpModeSchedPtr != 0 ) {
				if ( ! CheckScheduleValueMinMax( CBVAV( CBVAVNum ).FanOpModeSchedPtr, "<", 0.0, ">", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( "The schedule values in " + cAlphaFields( 13 ) + " must be 0 to 1." );
					ShowContinueError( "A value of 0 represents cycling fan mode, any other value up to 1 represents constant fan mode." );
					ErrorsFound = true;
				}

				//     Check supply air fan operating mode for cycling fan, if NOT cycling fan set AirFlowControl
				if ( ! CheckScheduleValueMinMax( CBVAV( CBVAVNum ).FanOpModeSchedPtr, ">=", 0.0, "<=", 0.0 ) ) { //Autodesk:Note Range is 0 to 0?
					//       set air flow control mode,
					//       UseCompressorOnFlow  = operate at last cooling or heating air flow requested when compressor is off
					//       UseCompressorOffFlow = operate at value specified by user (no input for this object type, UseCompONFlow)
					//       AirFlowControl only valid if fan opmode = ContFanCycCoil
					if ( CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow == 0.0 ) {
						CBVAV( CBVAVNum ).AirFlowControl = UseCompressorOnFlow;
					} else {
						CBVAV( CBVAVNum ).AirFlowControl = UseCompressorOffFlow;
					}
				}

			} else {
				if ( ! lAlphaBlanks( 13 ) ) {
					ShowWarningError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( cAlphaFields( 13 ) + " = " + Alphas( 13 ) + " not found. Supply air fan operating mode set to constant operation and simulation continues." );
				}
				CBVAV( CBVAVNum ).OpMode = ContFanCycCoil;
				if ( CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow == 0.0 ) {
					CBVAV( CBVAVNum ).AirFlowControl = UseCompressorOnFlow;
				} else {
					CBVAV( CBVAVNum ).AirFlowControl = UseCompressorOffFlow;
				}
			}

			//   Check FanVolFlow, must be >= CBVAV flow
			if ( FanVolFlow != AutoSize ) {
				if ( FanVolFlow < CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow && CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow != AutoSize && CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow != 0.0 ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( FanVolFlow, 7 ) + " in " + cAlphaFields( 11 ) + " = " + CBVAV( CBVAVNum ).FanName + " is less than " + cNumericFields( 3 ) );
					ShowContinueError( ' ' + cNumericFields( 3 ) + " is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow = FanVolFlow;
				}
			}
			//   only check that OA flow when compressor is OFF is >= SA flow when compressor is OFF when both are not autosized and
			//   that MaxNoCoolHeatAirVolFlow is /= 0 (trigger to use compressor ON flow, see AirFlowControl variable initialization above)
			if ( CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow > CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow && CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow != AutoSize && CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow != AutoSize && CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow != 0.0 ) {
				ShowWarningError( CurrentModuleObject + ": " + cNumericFields( 6 ) + " cannot be greater than " + cNumericFields( 3 ) );
				ShowContinueError( ' ' + cNumericFields( 6 ) + " is reset to the fan flow rate and the simulation continues." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow = FanVolFlow;
			}

			if ( SameString( Alphas( 16 ), "Coil:Heating:DX:SingleSpeed" ) || SameString( Alphas( 16 ), "Coil:Heating:Gas" ) || SameString( Alphas( 16 ), "Coil:Heating:Electric" ) || SameString( Alphas( 16 ), "Coil:Heating:Water" ) || SameString( Alphas( 16 ), "Coil:Heating:Steam" ) ) {
				CBVAV( CBVAVNum ).HeatCoilType = Alphas( 16 );
				CBVAV( CBVAVNum ).HeatCoilName = Alphas( 17 );

				if ( SameString( Alphas( 16 ), "Coil:Heating:DX:SingleSpeed" ) ) {
					CBVAV( CBVAVNum ).HeatCoilType_Num = CoilDX_HeatingEmpirical;
					CBVAV( CBVAVNum ).MinOATCompressor = GetMinOATDXCoilCompressor( CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );
					CBVAV( CBVAVNum ).HeatingCoilInletNode = GetDXCoilInletNode( CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );
					CBVAV( CBVAVNum ).HeatingCoilOutletNode = GetDXCoilOutletNode( CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );
					GetDXCoilIndex( CBVAV( CBVAVNum ).HeatCoilName, CBVAV( CBVAVNum ).DXHeatCoilIndexNum, DXCoilErrFlag, CBVAV( CBVAVNum ).HeatCoilType );
					if ( DXCoilErrFlag ) ShowContinueError( "...occurs in " + CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );

				} else if ( SameString( Alphas( 16 ), "Coil:Heating:Gas" ) ) {
					CBVAV( CBVAVNum ).HeatCoilType_Num = Coil_HeatingGas;
					CBVAV( CBVAVNum ).MinOATCompressor = -999.9;
					CBVAV( CBVAVNum ).HeatingCoilInletNode = GetCoilInletNode( CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );
					CBVAV( CBVAVNum ).HeatingCoilOutletNode = GetCoilOutletNode( CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );
				} else if ( SameString( Alphas( 16 ), "Coil:Heating:Electric" ) ) {
					CBVAV( CBVAVNum ).HeatCoilType_Num = Coil_HeatingElectric;
					CBVAV( CBVAVNum ).MinOATCompressor = -999.9;
					CBVAV( CBVAVNum ).HeatingCoilInletNode = GetCoilInletNode( CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );
					CBVAV( CBVAVNum ).HeatingCoilOutletNode = GetCoilOutletNode( CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );
				} else if ( SameString( Alphas( 16 ), "Coil:Heating:Water" ) ) {
					CBVAV( CBVAVNum ).HeatCoilType_Num = Coil_HeatingWater;
					errFlag = false;
					CBVAV( CBVAVNum ).CoilControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", CBVAV( CBVAVNum ).HeatCoilName, errFlag );
					CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", CBVAV( CBVAVNum ).HeatCoilName, errFlag );
					HeatCoilInletNodeNum = GetWaterCoilInletNode( "Coil:Heating:Water", CBVAV( CBVAVNum ).HeatCoilName, errFlag );
					CBVAV( CBVAVNum ).HeatingCoilInletNode = HeatCoilInletNodeNum;
					HeatCoilOutletNodeNum = GetWaterCoilOutletNode( "Coil:Heating:Water", CBVAV( CBVAVNum ).HeatCoilName, errFlag );
					CBVAV( CBVAVNum ).HeatingCoilOutletNode = HeatCoilOutletNodeNum;
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );
						ErrorsFound = true;
					}
				} else if ( SameString( Alphas( 16 ), "COIL:HEATING:STEAM" ) ) {
					CBVAV( CBVAVNum ).HeatCoilType_Num = Coil_HeatingSteam;
					errFlag = false;
					CBVAV( CBVAVNum ).HeatCoilIndex = GetSteamCoilIndex( "COIL:HEATING:STEAM", CBVAV( CBVAVNum ).HeatCoilName, errFlag );

					HeatCoilInletNodeNum = GetSteamCoilAirInletNode( CBVAV( CBVAVNum ).HeatCoilIndex, CBVAV( CBVAVNum ).HeatCoilName, errFlag );
					CBVAV( CBVAVNum ).HeatingCoilInletNode = HeatCoilInletNodeNum;
					CBVAV( CBVAVNum ).CoilControlNode = GetCoilSteamInletNode( CBVAV( CBVAVNum ).HeatCoilIndex, CBVAV( CBVAVNum ).HeatCoilName, errFlag );
					CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( CBVAV( CBVAVNum ).HeatCoilIndex, errFlag );
					SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitaryHeatCoolVAVChangeoverBypass );
					if ( CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( CBVAV( CBVAVNum ).HeatCoilIndex, errFlag ) * SteamDensity;
					}
					HeatCoilOutletNodeNum = GetCoilAirOutletNode( CBVAV( CBVAVNum ).HeatCoilIndex, CBVAV( CBVAVNum ).HeatCoilName, errFlag );
					CBVAV( CBVAVNum ).HeatingCoilOutletNode = HeatCoilOutletNodeNum;
					if ( errFlag ) {
						ShowContinueError( "...occurs in " + CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );
						ErrorsFound = true;
					}
				}
			} else {
				ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 16 ) + " = " + Alphas( 16 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			if ( CBVAV( CBVAVNum ).DXCoilOutletNode != CBVAV( CBVAVNum ).HeatingCoilInletNode ) {
				ShowSevereError( CurrentModuleObject + " illegal coil placement. Cooling coil must be upstream of heating coil." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ErrorsFound = true;
			}

			if ( CBVAV( CBVAVNum ).FanPlace == BlowThru ) {
				if ( CBVAV( CBVAVNum ).SplitterOutletAirNode != CBVAV( CBVAVNum ).HeatingCoilOutletNode ) {
					ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( 6 ) + " = " + SplitterOutletNodeName + '.' );
					ShowContinueError( cAlphaFields( 6 ) + " must be the same as the outlet node specified in the heating coil object = " + CBVAV( CBVAVNum ).HeatCoilType + ": " + CBVAV( CBVAVNum ).HeatCoilName + " when blow through " + cAlphaFields( 12 ) + " is selected." );
					ErrorsFound = true;
				}
				if ( CBVAV( CBVAVNum ).MixerMixedAirNode != CBVAV( CBVAVNum ).FanInletNodeNum ) {
					ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( "Illegal " + cAlphaFields( 11 ) + ". The fan inlet node name must be the same as the mixed air node specified in the " + cAlphaFields( 9 ) + " = " + CBVAV( CBVAVNum ).OAMixName + " when blow through " + cAlphaFields( 12 ) + " is selected." );
					ErrorsFound = true;
				}
			}

			if ( CBVAV( CBVAVNum ).FanPlace == DrawThru ) {
				if ( CBVAV( CBVAVNum ).MixerMixedAirNode != CBVAV( CBVAVNum ).DXCoilInletNode ) {
					ShowSevereError( CurrentModuleObject + ": " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( "Illegal cooling coil placement. The cooling coil inlet node name must be the same as the mixed air node specified in the " + cAlphaFields( 9 ) + " = " + CBVAV( CBVAVNum ).OAMixName + " when draw through " + cAlphaFields( 12 ) + " is selected." );
					ErrorsFound = true;
				}
			}

			if ( SameString( Alphas( 18 ), "CoolingPriority" ) ) {
				CBVAV( CBVAVNum ).PriorityControl = CoolingPriority;
			} else if ( SameString( Alphas( 18 ), "HeatingPriority" ) ) {
				CBVAV( CBVAVNum ).PriorityControl = HeatingPriority;
			} else if ( SameString( Alphas( 18 ), "ZonePriority" ) ) {
				CBVAV( CBVAVNum ).PriorityControl = ZonePriority;
			} else {
				ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 18 ) + " = " + Alphas( 18 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ShowContinueError( "Valid choices are CoolingPriority, HeatingPriority, or ZonePriority." );
				ErrorsFound = true;
			}

			if ( Numbers( 7 ) > 0.0 ) {
				CBVAV( CBVAVNum ).MinLATCooling = Numbers( 7 );
			} else {
				CBVAV( CBVAVNum ).MinLATCooling = 10.0;
			}

			if ( Numbers( 8 ) > 0.0 ) {
				CBVAV( CBVAVNum ).MaxLATHeating = Numbers( 8 );
			} else {
				CBVAV( CBVAVNum ).MaxLATHeating = 50.0;
			}

			if ( CBVAV( CBVAVNum ).MinLATCooling > CBVAV( CBVAVNum ).MaxLATHeating ) {
				ShowWarningError( CurrentModuleObject + ": illegal leaving air temperature specified." );
				ShowContinueError( "Resetting " + cNumericFields( 7 ) + " equal to " + cNumericFields( 8 ) + " and the simulation continues." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).MinLATCooling = CBVAV( CBVAVNum ).MaxLATHeating;
			}

			// Dehumidification control mode
			if ( SameString( Alphas( 19 ), "None" ) ) {
				CBVAV( CBVAVNum ).DehumidControlType = DehumidControl_None;
			} else if ( SameString( Alphas( 19 ), "" ) ) {
				CBVAV( CBVAVNum ).DehumidControlType = DehumidControl_None;
			} else if ( SameString( Alphas( 19 ), "Multimode" ) ) {
				if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					CBVAV( CBVAVNum ).DehumidControlType = DehumidControl_Multimode;
				} else {
					ShowWarningError( "Invalid " + cAlphaFields( 19 ) + " = " + Alphas( 19 ) );
					ShowContinueError( "In " + CurrentModuleObject + " \"" + CBVAV( CBVAVNum ).Name + "\"." );
					ShowContinueError( "Valid only with " + cAlphaFields( 14 ) + " = Coil:Cooling:DX:TwoStageWithHumidityControlMode." );
					ShowContinueError( "Setting " + cAlphaFields( 19 ) + " to \"None\" and the simulation continues." );
					CBVAV( CBVAVNum ).DehumidControlType = DehumidControl_None;
				}
			} else if ( SameString( Alphas( 19 ), "CoolReheat" ) ) {
				if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					CBVAV( CBVAVNum ).DehumidControlType = DehumidControl_CoolReheat;
				} else {
					ShowWarningError( "Invalid " + cAlphaFields( 19 ) + " = " + Alphas( 19 ) );
					ShowContinueError( "In " + CurrentModuleObject + " \"" + CBVAV( CBVAVNum ).Name + "\"." );
					ShowContinueError( "Valid only with " + cAlphaFields( 14 ) + " = Coil:Cooling:DX:TwoStageWithHumidityControlMode." );
					ShowContinueError( "Setting " + cAlphaFields( 19 ) + " to \"None\" and the simulation continues." );
					CBVAV( CBVAVNum ).DehumidControlType = DehumidControl_None;
				}
			} else {
				ShowSevereError( "Invalid " + cAlphaFields( 19 ) + " =" + Alphas( 19 ) );
				ShowContinueError( "In " + CurrentModuleObject + " \"" + CBVAV( CBVAVNum ).Name + "\"." );
			}

			if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num > 0 ) {
				ControlNodeNum = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsParent );
			}

			//   Initialize last mode of compressor operation
			CBVAV( CBVAVNum ).LastMode = HeatingMode;

			if ( CBVAV( CBVAVNum ).FanType_Num != FanType_SimpleOnOff && CBVAV( CBVAVNum ).FanType_Num != FanType_SimpleConstVolume ) {
				ShowSevereError( CurrentModuleObject + " illegal " + cAlphaFields( 10 ) + " in fan object = " + CBVAV( CBVAVNum ).FanName );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
				ShowContinueError( " The fan object type must be Fan:OnOff or Fan:ConstantVolume." );
				ErrorsFound = true;
			} else if ( CBVAV( CBVAVNum ).FanType_Num == FanType_SimpleOnOff || CBVAV( CBVAVNum ).FanType_Num == FanType_SimpleConstVolume ) {
				if ( CBVAV( CBVAVNum ).FanType_Num == FanType_SimpleOnOff && ! SameString( CBVAV( CBVAVNum ).FanType, "Fan:OnOff" ) ) {
					ShowWarningError( CurrentModuleObject + " has " + cAlphaFields( 10 ) + " = " + CBVAV( CBVAVNum ).FanType + " which is inconsistent with the fan object." );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( " The fan object (" + CBVAV( CBVAVNum ).FanName + ") is actually a valid fan type and the simulation continues." );
					ShowContinueError( " Node connections errors may result due to the inconsistent fan type." );
				}
				if ( CBVAV( CBVAVNum ).FanType_Num == FanType_SimpleConstVolume && ! SameString( CBVAV( CBVAVNum ).FanType, "Fan:ConstantVolume" ) ) {
					ShowWarningError( CurrentModuleObject + " has " + cAlphaFields( 10 ) + " = " + CBVAV( CBVAVNum ).FanType + " which is inconsistent with fan object." );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + CBVAV( CBVAVNum ).Name );
					ShowContinueError( " The fan object (" + CBVAV( CBVAVNum ).FanName + ") is actually a valid fan type and the simulation continues." );
					ShowContinueError( " Node connections errors may result due to the inconsistent fan type." );
				}
			}

			// Add fan to component sets array
			if ( CBVAV( CBVAVNum ).FanPlace == BlowThru ) {
				CompSetFanInlet = NodeID( CBVAV( CBVAVNum ).MixerMixedAirNode );
				CompSetFanOutlet = NodeID( CBVAV( CBVAVNum ).DXCoilInletNode );
			} else {
				CompSetFanInlet = NodeID( CBVAV( CBVAVNum ).HeatingCoilOutletNode );
				CompSetFanOutlet = SplitterOutletNodeName;
			}
			CompSetCoolInlet = NodeID( CBVAV( CBVAVNum ).DXCoilInletNode );
			CompSetCoolOutlet = NodeID( CBVAV( CBVAVNum ).DXCoilOutletNode );

			// Add fan to component sets array
			SetUpCompSets( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, CBVAV( CBVAVNum ).FanType, CBVAV( CBVAVNum ).FanName, CompSetFanInlet, CompSetFanOutlet );

			// Add cooling coil to component sets array
			SetUpCompSets( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, CBVAV( CBVAVNum ).DXCoolCoilType, CBVAV( CBVAVNum ).DXCoolCoilName, CompSetCoolInlet, CompSetCoolOutlet );

			// Add heating coil to component sets array
			SetUpCompSets( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, CBVAV( CBVAVNum ).HeatCoilType, CBVAV( CBVAVNum ).HeatCoilName, NodeID( CBVAV( CBVAVNum ).HeatingCoilInletNode ), NodeID( CBVAV( CBVAVNum ).HeatingCoilOutletNode ) );

			// Set up component set for OA mixer - use OA node and Mixed air node
			SetUpCompSets( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, CBVAV( CBVAVNum ).OAMixType, CBVAV( CBVAVNum ).OAMixName, NodeID( CBVAV( CBVAVNum ).MixerOutsideAirNode ), NodeID( CBVAV( CBVAVNum ).MixerMixedAirNode ) );

			TestCompSet( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, NodeID( CBVAV( CBVAVNum ).AirInNode ), NodeID( CBVAV( CBVAVNum ).AirOutNode ), "Air Nodes" );

			//   Find air loop associated with CBVAV system
			for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
				for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
					for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
						if ( ! SameString( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name, CBVAV( CBVAVNum ).Name ) || ! SameString( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf, CBVAV( CBVAVNum ).UnitType ) ) continue;
						CBVAV( CBVAVNum ).AirLoopNumber = AirLoopNum;
						//         Should EXIT here or do other checking?
						break;
					}
				}
			}

			if ( CBVAV( CBVAVNum ).AirLoopNumber > 0 ) {
				CBVAV( CBVAVNum ).NumControlledZones = AirToZoneNodeInfo( CBVAV( CBVAVNum ).AirLoopNumber ).NumZonesCooled;
				CBVAV( CBVAVNum ).ControlledZoneNum.allocate( CBVAV( CBVAVNum ).NumControlledZones );
				CBVAV( CBVAVNum ).ActualZoneNum.allocate( CBVAV( CBVAVNum ).NumControlledZones );
				CBVAV( CBVAVNum ).ActualZoneNodeNum.allocate( CBVAV( CBVAVNum ).NumControlledZones );
				CBVAV( CBVAVNum ).CBVAVBoxOutletNode.allocate( CBVAV( CBVAVNum ).NumControlledZones );
				CBVAV( CBVAVNum ).ZoneSequenceCoolingNum.allocate( CBVAV( CBVAVNum ).NumControlledZones );
				CBVAV( CBVAVNum ).ZoneSequenceHeatingNum.allocate( CBVAV( CBVAVNum ).NumControlledZones );

				CBVAV( CBVAVNum ).ControlledZoneNum = 0;
				CBVAV( CBVAVNum ).ActualZoneNum = 0;
				for ( AirLoopZoneNum = 1; AirLoopZoneNum <= AirToZoneNodeInfo( CBVAV( CBVAVNum ).AirLoopNumber ).NumZonesCooled; ++AirLoopZoneNum ) {
					CBVAV( CBVAVNum ).ControlledZoneNum( AirLoopZoneNum ) = AirToZoneNodeInfo( CBVAV( CBVAVNum ).AirLoopNumber ).CoolCtrlZoneNums( AirLoopZoneNum );
					if ( CBVAV( CBVAVNum ).ControlledZoneNum( AirLoopZoneNum ) > 0 ) {
						CBVAV( CBVAVNum ).ActualZoneNodeNum( AirLoopZoneNum ) = ZoneEquipConfig( CBVAV( CBVAVNum ).ControlledZoneNum( AirLoopZoneNum ) ).ZoneNode;
						CBVAV( CBVAVNum ).ActualZoneNum( AirLoopZoneNum ) = ZoneEquipConfig( CBVAV( CBVAVNum ).ControlledZoneNum( AirLoopZoneNum ) ).ActualZoneNum;
						CBVAV( CBVAVNum ).CBVAVBoxOutletNode( AirLoopZoneNum ) = AirToZoneNodeInfo( CBVAV( CBVAVNum ).AirLoopNumber ).CoolZoneInletNodes( AirLoopZoneNum );
						// check for thermostat in controlled zone
						FoundTstatZone = false;
						for ( TstatZoneNum = 1; TstatZoneNum <= NumTempControlledZones; ++TstatZoneNum ) {
							if ( TempControlledZone( TstatZoneNum ).ActualZoneNum != CBVAV( CBVAVNum ).ControlledZoneNum( AirLoopZoneNum ) ) continue;
							FoundTstatZone = true;
						}
						if ( ! FoundTstatZone ) {
							ShowWarningError( CurrentModuleObject + " \"" + CBVAV( CBVAVNum ).Name + "\"" );
							ShowContinueError( "Thermostat not found in zone = " + ZoneEquipConfig( CBVAV( CBVAVNum ).ControlledZoneNum( AirLoopZoneNum ) ).ZoneName + " and the simulation continues." );
							ShowContinueError( "This zone will not be controlled to a temperature setpoint." );
						}
					} else {
						ShowSevereError( "Controlled Zone node not found." );
						ErrorsFound = true;
					}
					if ( ZoneEquipConfig( CBVAV( CBVAVNum ).ActualZoneNum( AirLoopZoneNum ) ).EquipListIndex > 0 ) {
						for ( EquipNum = 1; EquipNum <= ZoneEquipList( ZoneEquipConfig( CBVAV( CBVAVNum ).ActualZoneNum( AirLoopZoneNum ) ).EquipListIndex ).NumOfEquipTypes; ++EquipNum ) {
							if ( ( ZoneEquipList( ZoneEquipConfig( CBVAV( CBVAVNum ).ActualZoneNum( AirLoopZoneNum ) ).EquipListIndex ).EquipType_Num( EquipNum ) == AirDistUnit_Num ) || ( ZoneEquipList( ZoneEquipConfig( CBVAV( CBVAVNum ).ActualZoneNum( AirLoopZoneNum ) ).EquipListIndex ).EquipType_Num( EquipNum ) == DirectAir_Num ) ) {
								CBVAV( CBVAVNum ).ZoneSequenceCoolingNum( AirLoopZoneNum ) = ZoneEquipList( ZoneEquipConfig( CBVAV( CBVAVNum ).ActualZoneNum( AirLoopZoneNum ) ).EquipListIndex ).CoolingPriority( EquipNum );
								CBVAV( CBVAVNum ).ZoneSequenceHeatingNum( AirLoopZoneNum ) = ZoneEquipList( ZoneEquipConfig( CBVAV( CBVAVNum ).ActualZoneNum( AirLoopZoneNum ) ).EquipListIndex ).HeatingPriority( EquipNum );
							}
						}
					}

				}
			} else {
			}

		} // CBVAVIndex = 1,NumCBVAV

		if ( ErrorsFound ) {
			ShowFatalError( "GetCBVAV: Errors found in getting " + CurrentModuleObject + " input." );
			ShowContinueError( "... Preceding condition causes termination." );
		}

		for ( CBVAVNum = 1; CBVAVNum <= NumCBVAV; ++CBVAVNum ) {
			// Setup Report variables for the Fan Coils
			SetupOutputVariable( "Unitary System Total Heating Rate [W]", CBVAV( CBVAVNum ).TotHeatEnergyRate, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Total Heating Energy [J]", CBVAV( CBVAVNum ).TotHeatEnergy, "System", "Sum", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Total Cooling Rate [W]", CBVAV( CBVAVNum ).TotCoolEnergyRate, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Total Cooling Energy [J]", CBVAV( CBVAVNum ).TotCoolEnergy, "System", "Sum", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Heating Rate [W]", CBVAV( CBVAVNum ).SensHeatEnergyRate, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Heating Energy [J]", CBVAV( CBVAVNum ).SensHeatEnergy, "System", "Sum", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Cooling Rate [W]", CBVAV( CBVAVNum ).SensCoolEnergyRate, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Sensible Cooling Energy [J]", CBVAV( CBVAVNum ).SensCoolEnergy, "System", "Sum", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Latent Heating Rate [W]", CBVAV( CBVAVNum ).LatHeatEnergyRate, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Latent Heating Energy [J]", CBVAV( CBVAVNum ).LatHeatEnergy, "System", "Sum", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Latent Cooling Rate [W]", CBVAV( CBVAVNum ).LatCoolEnergyRate, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Latent Cooling Energy [J]", CBVAV( CBVAVNum ).LatCoolEnergy, "System", "Sum", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Electric Power [W]", CBVAV( CBVAVNum ).ElecPower, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Electric Energy [J]", CBVAV( CBVAVNum ).ElecConsumption, "System", "Sum", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Fan Part Load Ratio []", CBVAV( CBVAVNum ).FanPartLoadRatio, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Compressor Part Load Ratio []", CBVAV( CBVAVNum ).CompPartLoadRatio, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Bypass Air Mass Flow Rate [kg/s]", CBVAV( CBVAVNum ).BypassMassFlowRate, "System", "Average", CBVAV( CBVAVNum ).Name );
			SetupOutputVariable( "Unitary System Air Outlet Setpoint Temperature [C]", CBVAV( CBVAVNum ).OutletTempSetPoint, "System", "Average", CBVAV( CBVAVNum ).Name );
		}

	}

	void
	InitCBVAV(
		int const CBVAVNum, // Index of the current CBVAV unit being simulated
		bool const FirstHVACIteration, // TRUE if first HVAC iteration
		int const AirLoopNum, // air loop index
		Real64 & QZnReq, // Heating/Cooling load for all zones
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to average airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       B. Griffith, May 2009, EMS setpoint check
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the changeover-bypass VAV system components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations. The CBVAV system is simulated with no load (coils off) to
		// determine the outlet temperature. A setpoint temperature is calculated on FirstHVACIteration = TRUE.
		// Once the setpoint is calculated, the inlet mass flow rate on FirstHVACIteration = FALSE is used to
		// determine the bypass fraction. The simulation converges quickly on mass flow rate. If the zone
		// temperatures float in the deadband, additional iterations are required to converge on mass flow rate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using General::TrimSigDigits;
		using DataSizing::AutoSize;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataAirLoop::AirLoopControlInfo;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iHumidityRatioMaxSetPoint;
		using SteamCoils::SimulateSteamCoilComponents;
		auto & GetCoilMaxSteamFlowRate( SteamCoils::GetCoilMaxSteamFlowRate );
		using WaterCoils::GetCoilMaxWaterFlowRate;
		using WaterCoils::SimulateWaterCoilComponents;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::PlantLoop;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatDensityRefrig;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using DataGlobals::InitConvTemp;
		using DataGlobals::AnyPlantInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitCBVAV" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // Inlet node number in CBVAV loop
		int OutNode; // Outlet node number in CBVAV loop
		int MixerOutsideAirNode; // Outside air node number in CBVAV loop
		Real64 RhoAir; // Air density at InNode
		static bool MyOneTimeFlag( true ); // Initialization flag
		static Array1D_bool MyEnvrnFlag; // Used for initializations each begin environment flag
		static Array1D_bool MySizeFlag; // Used for sizing CBVAV inputs one time
		static Array1D_bool MyPlantScanFlag; // Used for initializations plant component for heating coils
		Real64 QSensUnitOut; // Output of CBVAV system with coils off
		Real64 OutsideAirMultiplier; // Outside air multiplier schedule (= 1.0 if no schedule)
		static bool FanErrFlag( false ); // Error flag returned during CALL to GetFanType
		int FanIndex; // Index to CBVAV's supply air fan
		std::string CurrentModuleObject; // Object type for error messages
		static bool EMSSetPointCheck( false ); // local temporary
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int SteamIndex; // steam coil index
		Real64 FluidDensity; // steam or water coil fluid density
		Real64 CoilMaxVolFlowRate; // water or steam max volumetric water flow rate
		Real64 QCoilActual; // actual CBVAV steam heating coil load met (W)
		bool ErrorFlag; // local error flag returned from data mining
		Real64 mdot; // heating coil fluid mass flow rate, kg/s

		InNode = CBVAV( CBVAVNum ).AirInNode;
		OutNode = CBVAV( CBVAVNum ).AirOutNode;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumCBVAV );
			MySizeFlag.allocate( NumCBVAV );
			MyPlantScanFlag.allocate( NumCBVAV );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;

			MyOneTimeFlag = false;

		}

		if ( MyPlantScanFlag( CBVAVNum ) && allocated( PlantLoop ) ) {
			if ( ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingWater ) || ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingSteam ) ) {
				if ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingWater ) {

					ErrorFlag = false;
					ScanPlantLoopsForObject( CBVAV( CBVAVNum ).HeatCoilName, TypeOf_CoilWaterSimpleHeating, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum, _, _, _, _, _, ErrorFlag );
					if ( ErrorFlag ) {
						ShowFatalError( "InitCBVAV: Program terminated for previous conditions." );
					}

					CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound );

					if ( CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						FluidDensity = GetDensityGlycol( PlantLoop( CBVAV( CBVAVNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( CBVAV( CBVAVNum ).LoopNum ).FluidIndex, RoutineName );
						CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", CBVAV( CBVAVNum ).HeatCoilName, ErrorsFound ) * FluidDensity;
					}

				} else if ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingSteam ) {

					ErrorFlag = false;
					ScanPlantLoopsForObject( CBVAV( CBVAVNum ).HeatCoilName, TypeOf_CoilSteamAirHeating, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum, _, _, _, _, _, ErrorFlag );

					if ( ErrorFlag ) {
						ShowFatalError( "InitCBVAV: Program terminated for previous conditions." );
					}

					CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( CBVAV( CBVAVNum ).HeatCoilIndex, ErrorsFound );

					if ( CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow > 0.0 ) {
						SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
						FluidDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
						CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate( CBVAV( CBVAVNum ).HeatCoilIndex, ErrorsFound ) * FluidDensity;
					}

				}

				//fill outlet node for heating coil
				CBVAV( CBVAVNum ).CoilOutletNode = PlantLoop( CBVAV( CBVAVNum ).LoopNum ).LoopSide( CBVAV( CBVAVNum ).LoopSide ).Branch( CBVAV( CBVAVNum ).BranchNum ).Comp( CBVAV( CBVAVNum ).CompNum ).NodeNumOut;
				MyPlantScanFlag( CBVAVNum ) = false;

			} else { // CBVAV is not connected to plant
				MyPlantScanFlag( CBVAVNum ) = false;
			}
		} else if ( MyPlantScanFlag( CBVAVNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( CBVAVNum ) = false;
		}

		if ( ! SysSizingCalc && MySizeFlag( CBVAVNum ) ) {
			SizeCBVAV( CBVAVNum );
			// Pass the fan cycling schedule index up to the air loop. Set the air loop unitary system flag.
			AirLoopControlInfo( AirLoopNum ).CycFanSchedPtr = CBVAV( CBVAVNum ).FanOpModeSchedPtr;
			//   Set UnitarySys flag to FALSE and let the heating coil autosize independently of the cooling coil
			AirLoopControlInfo( AirLoopNum ).UnitarySys = false;
			AirLoopControlInfo( AirLoopNum ).FanOpMode = CBVAV( CBVAVNum ).OpMode;
			MySizeFlag( CBVAVNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( CBVAVNum ) ) {
			MixerOutsideAirNode = CBVAV( CBVAVNum ).MixerOutsideAirNode;
			RhoAir = StdRhoAir;
			// set the mass flow rates from the input volume flow rates
			CBVAV( CBVAVNum ).MaxCoolAirMassFlow = RhoAir * CBVAV( CBVAVNum ).MaxCoolAirVolFlow;
			CBVAV( CBVAVNum ).CoolOutAirMassFlow = RhoAir * CBVAV( CBVAVNum ).CoolOutAirVolFlow;
			CBVAV( CBVAVNum ).MaxHeatAirMassFlow = RhoAir * CBVAV( CBVAVNum ).MaxHeatAirVolFlow;
			CBVAV( CBVAVNum ).HeatOutAirMassFlow = RhoAir * CBVAV( CBVAVNum ).HeatOutAirVolFlow;
			CBVAV( CBVAVNum ).MaxNoCoolHeatAirMassFlow = RhoAir * CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow;
			CBVAV( CBVAVNum ).NoCoolHeatOutAirMassFlow = RhoAir * CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow;
			// set the node max and min mass flow rates
			Node( MixerOutsideAirNode ).MassFlowRateMax = max( CBVAV( CBVAVNum ).CoolOutAirMassFlow, CBVAV( CBVAVNum ).HeatOutAirMassFlow );
			Node( MixerOutsideAirNode ).MassFlowRateMaxAvail = max( CBVAV( CBVAVNum ).CoolOutAirMassFlow, CBVAV( CBVAVNum ).HeatOutAirMassFlow );
			Node( MixerOutsideAirNode ).MassFlowRateMin = 0.0;
			Node( MixerOutsideAirNode ).MassFlowRateMinAvail = 0.0;
			Node( InNode ).MassFlowRateMax = max( CBVAV( CBVAVNum ).MaxCoolAirMassFlow, CBVAV( CBVAVNum ).MaxHeatAirMassFlow );
			Node( InNode ).MassFlowRateMaxAvail = max( CBVAV( CBVAVNum ).MaxCoolAirMassFlow, CBVAV( CBVAVNum ).MaxHeatAirMassFlow );
			Node( InNode ).MassFlowRateMin = 0.0;
			Node( InNode ).MassFlowRateMinAvail = 0.0;
			Node( OutNode ).Temp = Node( InNode ).Temp;
			Node( OutNode ).HumRat = Node( InNode ).HumRat;
			Node( OutNode ).Enthalpy = Node( InNode ).Enthalpy;
			Node( CBVAV( CBVAVNum ).MixerReliefAirNode ) = Node( MixerOutsideAirNode );
			MyEnvrnFlag( CBVAVNum ) = false;
			CBVAV( CBVAVNum ).LastMode = HeatingMode;
			//   set fluid-side hardware limits
			if ( CBVAV( CBVAVNum ).CoilControlNode > 0 ) {
				//    If water coil max water flow rate is autosized, simulate once in order to mine max water flow rate
				if ( CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow == AutoSize ) {
					if ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingWater ) {
						SimulateWaterCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex );
						ErrorFlag = false;
						CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate( "Coil:Heating:Water", CBVAV( CBVAVNum ).HeatCoilName, ErrorFlag );
						if ( ErrorFlag ) {
							ErrorsFound = true;
						}
						if ( CoilMaxVolFlowRate != AutoSize ) {
							FluidDensity = GetDensityGlycol( PlantLoop( CBVAV( CBVAVNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( CBVAV( CBVAVNum ).LoopNum ).FluidIndex, RoutineName );
							CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity;
						}
					}
					if ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingSteam ) {
						SimulateSteamCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, 1.0, QCoilActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
						ErrorFlag = false;
						CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate( CBVAV( CBVAVNum ).HeatCoilIndex, ErrorFlag );
						if ( ErrorFlag ) {
							ErrorsFound = true;
						}
						if ( CoilMaxVolFlowRate != AutoSize ) {
							SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
							FluidDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName );
							CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity;
						}
					}
				} // end of IF(CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow .EQ. AutoSize)THEN

				InitComponentNodes( 0.0, CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );

			} // end of IF(CBVAV(CBVAVNum)%CoilControlNode .GT. 0)THEN
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( CBVAVNum ) = true;
		}

		// IF CBVAV system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than CBVAV flow rates
		if ( ! DoingSizing && CBVAV( CBVAVNum ).CheckFanFlow ) {
			CurrentModuleObject = "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass";
			FanErrFlag = false;
			GetFanIndex( CBVAV( CBVAVNum ).FanName, FanIndex, FanErrFlag );
			if ( FanErrFlag ) {
				ShowContinueError( "...Fan occurs in " + CurrentModuleObject + " =" + CBVAV( CBVAVNum ).Name );
			} else {
				//     Only get the fan volumetric flow rate if the fan is valid, otherwise GetFanVolFlow returns a 0 and
				//     the following warnings are written to the error file.
				GetFanVolFlow( FanIndex, CBVAV( CBVAVNum ).FanVolFlow );
			}
			if ( CBVAV( CBVAVNum ).FanVolFlow != AutoSize ) {
				//     Check fan versus system supply air flow rates
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).MaxCoolAirVolFlow ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( CBVAV( CBVAVNum ).FanVolFlow, 7 ) + " in fan object " + CBVAV( CBVAVNum ).FanName + " is less than the maximum CBVAV system air flow rate when cooling is required (" + TrimSigDigits( CBVAV( CBVAVNum ).MaxCoolAirVolFlow, 7 ) + ")." );
					ShowContinueError( " The CBVAV system flow rate when cooling is required is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in Changeover-bypass VAV system = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).MaxCoolAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
				}
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).MaxHeatAirVolFlow ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( CBVAV( CBVAVNum ).FanVolFlow, 7 ) + " in fan object " + CBVAV( CBVAVNum ).FanName + " is less than the maximum CBVAV system air flow rate when heating is required (" + TrimSigDigits( CBVAV( CBVAVNum ).MaxHeatAirVolFlow, 7 ) + ")." );
					ShowContinueError( " The CBVAV system flow rate when heating is required is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in Changeover-bypass VAV system = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).MaxHeatAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
				}
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow && CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow != 0.0 ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( CBVAV( CBVAVNum ).FanVolFlow, 7 ) + " in fan object " + CBVAV( CBVAVNum ).FanName + " is less than the maximum CBVAV system air flow rate when no heating or cooling is needed (" + TrimSigDigits( CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow, 7 ) + ")." );
					ShowContinueError( " The CBVAV system flow rate when no heating or cooling is needed is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in Changeover-bypass VAV system = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
				}
				//     Check fan versus outdoor air flow rates
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).CoolOutAirVolFlow ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( CBVAV( CBVAVNum ).FanVolFlow, 7 ) + " in fan object " + CBVAV( CBVAVNum ).FanName + " is less than the maximum CBVAV outdoor air flow rate when cooling is required (" + TrimSigDigits( CBVAV( CBVAVNum ).CoolOutAirVolFlow, 7 ) + ")." );
					ShowContinueError( " The CBVAV outdoor flow rate when cooling is required is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in Changeover-bypass VAV system = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).CoolOutAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
				}
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).HeatOutAirVolFlow ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( CBVAV( CBVAVNum ).FanVolFlow, 7 ) + " in fan object " + CBVAV( CBVAVNum ).FanName + " is less than the maximum CBVAV outdoor air flow rate when heating is required (" + TrimSigDigits( CBVAV( CBVAVNum ).HeatOutAirVolFlow, 7 ) + ")." );
					ShowContinueError( " The CBVAV outdoor flow rate when heating is required is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in Changeover-bypass VAV system = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).HeatOutAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
				}
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow ) {
					ShowWarningError( CurrentModuleObject + " - air flow rate = " + TrimSigDigits( CBVAV( CBVAVNum ).FanVolFlow, 7 ) + " in fan object " + CBVAV( CBVAVNum ).FanName + " is less than the maximum CBVAV outdoor air flow rate when no heating or cooling is needed (" + TrimSigDigits( CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow, 7 ) + ")." );
					ShowContinueError( " The CBVAV outdoor flow rate when no heating or cooling is needed is reset to the fan flow rate and the simulation continues." );
					ShowContinueError( " Occurs in Changeover-bypass VAV system = " + CBVAV( CBVAVNum ).Name );
					CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
				}
				MixerOutsideAirNode = CBVAV( CBVAVNum ).MixerOutsideAirNode;
				RhoAir = StdRhoAir;
				// set the mass flow rates from the reset volume flow rates
				CBVAV( CBVAVNum ).MaxCoolAirMassFlow = RhoAir * CBVAV( CBVAVNum ).MaxCoolAirVolFlow;
				CBVAV( CBVAVNum ).CoolOutAirMassFlow = RhoAir * CBVAV( CBVAVNum ).CoolOutAirVolFlow;
				CBVAV( CBVAVNum ).MaxHeatAirMassFlow = RhoAir * CBVAV( CBVAVNum ).MaxHeatAirVolFlow;
				CBVAV( CBVAVNum ).HeatOutAirMassFlow = RhoAir * CBVAV( CBVAVNum ).HeatOutAirVolFlow;
				CBVAV( CBVAVNum ).MaxNoCoolHeatAirMassFlow = RhoAir * CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow;
				CBVAV( CBVAVNum ).NoCoolHeatOutAirMassFlow = RhoAir * CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow;
				// set the node max and min mass flow rates based on reset volume flow rates
				Node( MixerOutsideAirNode ).MassFlowRateMax = max( CBVAV( CBVAVNum ).CoolOutAirMassFlow, CBVAV( CBVAVNum ).HeatOutAirMassFlow );
				Node( MixerOutsideAirNode ).MassFlowRateMaxAvail = max( CBVAV( CBVAVNum ).CoolOutAirMassFlow, CBVAV( CBVAVNum ).HeatOutAirMassFlow );
				Node( MixerOutsideAirNode ).MassFlowRateMin = 0.0;
				Node( MixerOutsideAirNode ).MassFlowRateMinAvail = 0.0;
				Node( InNode ).MassFlowRateMax = max( CBVAV( CBVAVNum ).MaxCoolAirMassFlow, CBVAV( CBVAVNum ).MaxHeatAirMassFlow );
				Node( InNode ).MassFlowRateMaxAvail = max( CBVAV( CBVAVNum ).MaxCoolAirMassFlow, CBVAV( CBVAVNum ).MaxHeatAirMassFlow );
				Node( InNode ).MassFlowRateMin = 0.0;
				Node( InNode ).MassFlowRateMinAvail = 0.0;
				Node( OutNode ).Temp = Node( InNode ).Temp;
				Node( OutNode ).HumRat = Node( InNode ).HumRat;
				Node( OutNode ).Enthalpy = Node( InNode ).Enthalpy;
				Node( CBVAV( CBVAVNum ).MixerReliefAirNode ) = Node( MixerOutsideAirNode );
				CBVAV( CBVAVNum ).CheckFanFlow = false;
				if ( CBVAV( CBVAVNum ).FanVolFlow > 0.0 ) {
					CBVAV( CBVAVNum ).HeatingSpeedRatio = CBVAV( CBVAVNum ).MaxHeatAirVolFlow / CBVAV( CBVAVNum ).FanVolFlow;
					CBVAV( CBVAVNum ).CoolingSpeedRatio = CBVAV( CBVAVNum ).MaxCoolAirVolFlow / CBVAV( CBVAVNum ).FanVolFlow;
					CBVAV( CBVAVNum ).NoHeatCoolSpeedRatio = CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow / CBVAV( CBVAVNum ).FanVolFlow;
				}
			}
		}

		if ( CBVAV( CBVAVNum ).FanOpModeSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( CBVAV( CBVAVNum ).FanOpModeSchedPtr ) == 0.0 ) {
				CBVAV( CBVAVNum ).OpMode = CycFanCycCoil;
			} else {
				CBVAV( CBVAVNum ).OpMode = ContFanCycCoil;
			}
		}

		// Returns load only for zones requesting cooling (heating). If in deadband, Qzoneload = 0.
		GetZoneLoads( CBVAVNum, QZnReq );

		if ( CBVAV( CBVAVNum ).OutAirSchPtr > 0 ) {
			OutsideAirMultiplier = GetCurrentScheduleValue( CBVAV( CBVAVNum ).OutAirSchPtr );
		} else {
			OutsideAirMultiplier = 1.0;
		}

		// Set the inlet node mass flow rate
		if ( CBVAV( CBVAVNum ).OpMode == ContFanCycCoil ) {
			// constant fan mode
			if ( CBVAV( CBVAVNum ).HeatCoolMode == HeatingMode ) {
				CompOnMassFlow = CBVAV( CBVAVNum ).MaxHeatAirMassFlow;
				CompOnFlowRatio = CBVAV( CBVAVNum ).HeatingSpeedRatio;
				OACompOnMassFlow = CBVAV( CBVAVNum ).HeatOutAirMassFlow * OutsideAirMultiplier;
				CBVAV( CBVAVNum ).LastMode = HeatingMode;
			} else if ( CBVAV( CBVAVNum ).HeatCoolMode == CoolingMode ) {
				CompOnMassFlow = CBVAV( CBVAVNum ).MaxCoolAirMassFlow;
				CompOnFlowRatio = CBVAV( CBVAVNum ).CoolingSpeedRatio;
				OACompOnMassFlow = CBVAV( CBVAVNum ).CoolOutAirMassFlow * OutsideAirMultiplier;
				CBVAV( CBVAVNum ).LastMode = CoolingMode;
			} else {
				CompOnMassFlow = CBVAV( CBVAVNum ).MaxNoCoolHeatAirMassFlow;
				CompOnFlowRatio = CBVAV( CBVAVNum ).NoHeatCoolSpeedRatio;
				OACompOnMassFlow = CBVAV( CBVAVNum ).NoCoolHeatOutAirMassFlow * OutsideAirMultiplier;
			}

			if ( CBVAV( CBVAVNum ).AirFlowControl == UseCompressorOnFlow ) {
				if ( CBVAV( CBVAVNum ).LastMode == HeatingMode ) {
					CompOffMassFlow = CBVAV( CBVAVNum ).MaxHeatAirMassFlow;
					CompOffFlowRatio = CBVAV( CBVAVNum ).HeatingSpeedRatio;
					OACompOffMassFlow = CBVAV( CBVAVNum ).HeatOutAirMassFlow * OutsideAirMultiplier;
				} else {
					CompOffMassFlow = CBVAV( CBVAVNum ).MaxCoolAirMassFlow;
					CompOffFlowRatio = CBVAV( CBVAVNum ).CoolingSpeedRatio;
					OACompOffMassFlow = CBVAV( CBVAVNum ).CoolOutAirMassFlow * OutsideAirMultiplier;
				}
			} else {
				CompOffMassFlow = CBVAV( CBVAVNum ).MaxNoCoolHeatAirMassFlow;
				CompOffFlowRatio = CBVAV( CBVAVNum ).NoHeatCoolSpeedRatio;
				OACompOffMassFlow = CBVAV( CBVAVNum ).NoCoolHeatOutAirMassFlow * OutsideAirMultiplier;
			}
		} else {
			// cycling fan mode
			if ( CBVAV( CBVAVNum ).HeatCoolMode == HeatingMode ) {
				CompOnMassFlow = CBVAV( CBVAVNum ).MaxHeatAirMassFlow;
				CompOnFlowRatio = CBVAV( CBVAVNum ).HeatingSpeedRatio;
				OACompOnMassFlow = CBVAV( CBVAVNum ).HeatOutAirMassFlow * OutsideAirMultiplier;
			} else if ( CBVAV( CBVAVNum ).HeatCoolMode == CoolingMode ) {
				CompOnMassFlow = CBVAV( CBVAVNum ).MaxCoolAirMassFlow;
				CompOnFlowRatio = CBVAV( CBVAVNum ).CoolingSpeedRatio;
				OACompOnMassFlow = CBVAV( CBVAVNum ).CoolOutAirMassFlow * OutsideAirMultiplier;
			} else {
				CompOnMassFlow = CBVAV( CBVAVNum ).MaxCoolAirMassFlow;
				CompOnFlowRatio = CBVAV( CBVAVNum ).CoolingSpeedRatio;
				OACompOnMassFlow = CBVAV( CBVAVNum ).CoolOutAirMassFlow * OutsideAirMultiplier;
			}
			CompOffMassFlow = 0.0;
			CompOffFlowRatio = 0.0;
			OACompOffMassFlow = 0.0;
		}

		// Check for correct control node at outlet of unit
		if ( CBVAV( CBVAVNum ).HumRatMaxCheck ) {
			if ( CBVAV( CBVAVNum ).DehumidControlType > 0 ) {
				if ( Node( OutNode ).HumRatMax == SensedNodeFlagValue ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						ShowWarningError( "Unitary System:VAV:ChangeOverBypass = " + CBVAV( CBVAVNum ).Name );
						ShowContinueError( "Use SetpointManager:SingleZone:Humidity:Maximum to place a humidity setpoint at the air outlet node of the unitary system." );
						ShowContinueError( "Setting Dehumidification Control Type to None and simulation continues." );
						CBVAV( CBVAVNum ).DehumidControlType = 0;
					} else {
						// need call to EMS to check node
						EMSSetPointCheck = false;
						CheckIfNodeSetPointManagedByEMS( OutNode, iHumidityRatioMaxSetPoint, EMSSetPointCheck );
						if ( EMSSetPointCheck ) {
							ShowWarningError( "Unitary System:VAV:ChangeOverBypass = " + CBVAV( CBVAVNum ).Name );
							ShowContinueError( "Use SetpointManager:SingleZone:Humidity:Maximum to place a humidity setpoint at the air outlet node of the unitary system." );
							ShowContinueError( "Or use an EMS Actuator to place a maximum humidity setpoint at the air outlet node of the unitary system." );
							ShowContinueError( "Setting Dehumidification Control Type to None and simulation continues." );
							CBVAV( CBVAVNum ).DehumidControlType = 0;
						}
					}
				}
				CBVAV( CBVAVNum ).HumRatMaxCheck = false;
			} else {
				CBVAV( CBVAVNum ).HumRatMaxCheck = false;
			}
		}

		// Set the inlet node mass flow rate
		if ( GetCurrentScheduleValue( CBVAV( CBVAVNum ).SchedPtr ) > 0.0 && CompOnMassFlow != 0.0 ) {
			OnOffAirFlowRatio = 1.0;
			if ( FirstHVACIteration ) {
				Node( CBVAV( CBVAVNum ).AirInNode ).MassFlowRate = CompOnMassFlow;
				Node( CBVAV( CBVAVNum ).MixerInletAirNode ).MassFlowRate = CompOnMassFlow;
				Node( CBVAV( CBVAVNum ).MixerOutsideAirNode ).MassFlowRate = OACompOnMassFlow;
				Node( CBVAV( CBVAVNum ).MixerReliefAirNode ).MassFlowRate = OACompOnMassFlow;
				BypassDuctFlowFraction = 0.0;
				PartLoadFrac = 0.0;
			} else {
				if ( CBVAV( CBVAVNum ).HeatCoolMode != 0 ) {
					PartLoadFrac = 1.0;
				} else {
					PartLoadFrac = 0.0;
				}
				if ( CBVAV( CBVAVNum ).OpMode == CycFanCycCoil ) {
					BypassDuctFlowFraction = 0.0;
				} else {
					BypassDuctFlowFraction = max( 0.0, 1.0 - ( Node( CBVAV( CBVAVNum ).AirInNode ).MassFlowRate / CompOnMassFlow ) );
				}
			}
		} else {
			PartLoadFrac = 0.0;
			Node( CBVAV( CBVAVNum ).AirInNode ).MassFlowRate = 0.0;
			Node( CBVAV( CBVAVNum ).AirOutNode ).MassFlowRate = 0.0;
			Node( CBVAV( CBVAVNum ).AirOutNode ).MassFlowRateMaxAvail = 0.0;

			Node( CBVAV( CBVAVNum ).MixerInletAirNode ).MassFlowRate = 0.0;
			Node( CBVAV( CBVAVNum ).MixerOutsideAirNode ).MassFlowRate = 0.0;
			Node( CBVAV( CBVAVNum ).MixerReliefAirNode ).MassFlowRate = 0.0;

			OnOffAirFlowRatio = 1.0;
			BypassDuctFlowFraction = 0.0;

		}

		CalcCBVAV( CBVAVNum, FirstHVACIteration, PartLoadFrac, QSensUnitOut, QZnReq, OnOffAirFlowRatio, HXUnitOn );

		// If unit is scheduled OFF, setpoint is equal to inlet node temperature.
		if ( GetCurrentScheduleValue( CBVAV( CBVAVNum ).SchedPtr ) == 0.0 ) {
			CBVAV( CBVAVNum ).OutletTempSetPoint = Node( InNode ).Temp;
			return;
		}

		SetAverageAirFlow( CBVAVNum, OnOffAirFlowRatio, FirstHVACIteration );

		if ( FirstHVACIteration ) CBVAV( CBVAVNum ).OutletTempSetPoint = CalcSetPointTempTarget( CBVAVNum );

		// The setpoint is used to control the DX coils at their respective outlet nodes (not the unit outlet), correct
		// for fan heat for draw thru units only (fan heat is included at the outlet of each coil when blowthru is used)
		CBVAV( CBVAVNum ).CoilTempSetPoint = CBVAV( CBVAVNum ).OutletTempSetPoint;
		if ( CBVAV( CBVAVNum ).FanPlace == DrawThru ) {
			CBVAV( CBVAVNum ).CoilTempSetPoint -= ( Node( CBVAV( CBVAVNum ).AirOutNode ).Temp - Node( CBVAV( CBVAVNum ).FanInletNodeNum ).Temp );
		}

		if ( FirstHVACIteration ) {
			if ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingWater ) {
				SimulateWaterCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex );

				//     set air-side and steam-side mass flow rates
				Node( CBVAV( CBVAVNum ).HeatingCoilInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow;
				SetComponentFlowRate( mdot, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );

				//     simulate water coil to find operating capacity
				SimulateWaterCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, QCoilActual );
				CBVAV( CBVAVNum ).DesignSuppHeatingCapacity = QCoilActual;

			} // from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN

			if ( CBVAV( CBVAVNum ).HeatCoilType_Num == Coil_HeatingSteam ) {

				//     set air-side and steam-side mass flow rates
				Node( CBVAV( CBVAVNum ).HeatingCoilInletNode ).MassFlowRate = CompOnMassFlow;
				mdot = CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow;
				SetComponentFlowRate( mdot, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );

				//     simulate steam coil to find operating capacity
				SimulateSteamCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, 1.0, QCoilActual ); //QCoilReq, simulate any load > 0 to get max capacity of steam coil
				CBVAV( CBVAVNum ).DesignSuppHeatingCapacity = QCoilActual;

			} // from IF(CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingSteam) THEN
		} // from IF( FirstHVACIteration ) THEN

		if ( ( CBVAV( CBVAVNum ).HeatCoolMode == 0 && CBVAV( CBVAVNum ).OpMode == CycFanCycCoil ) || CompOnMassFlow == 0.0 ) {
			QZnReq = 0.0;
			PartLoadFrac = 0.0;
			Node( CBVAV( CBVAVNum ).AirInNode ).MassFlowRate = 0.0;
			Node( CBVAV( CBVAVNum ).AirOutNode ).MassFlowRateMaxAvail = 0.0;
			Node( CBVAV( CBVAVNum ).MixerInletAirNode ).MassFlowRate = 0.0;
			Node( CBVAV( CBVAVNum ).MixerOutsideAirNode ).MassFlowRate = 0.0;
			Node( CBVAV( CBVAVNum ).MixerReliefAirNode ).MassFlowRate = 0.0;
		}

	}

	void
	SizeCBVAV( int const CBVAVNum ) // Index to CBVAV system
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing changeover-bypass VAV components.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( CBVAV( CBVAVNum ).MaxCoolAirVolFlow == AutoSize ) {

			if ( CurSysNum > 0 ) {

				CheckSysSizing( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).MaxCoolAirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).MaxCoolAirVolFlow && CBVAV( CBVAVNum ).FanVolFlow != AutoSize ) {
					CBVAV( CBVAVNum ).MaxCoolAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
					ShowWarningError( CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );
					ShowContinueError( "The CBVAV system supply air fan air flow rate is less than the autosized value for the maximum air flow rate in cooling mode. Consider autosizing the fan for this simulation." );
					ShowContinueError( "The maximum air flow rate in cooling mode is reset to the supply air fan flow rate and the simulation continues." );
				}
				if ( CBVAV( CBVAVNum ).MaxCoolAirVolFlow < SmallAirVolFlow ) {
					CBVAV( CBVAVNum ).MaxCoolAirVolFlow = 0.0;
				}
				ReportSizingOutput( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, "maximum cooling air flow rate [m3/s]", CBVAV( CBVAVNum ).MaxCoolAirVolFlow );

			}

		}

		if ( CBVAV( CBVAVNum ).MaxHeatAirVolFlow == AutoSize ) {

			if ( CurSysNum > 0 ) {

				CheckSysSizing( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).MaxHeatAirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).MaxHeatAirVolFlow && CBVAV( CBVAVNum ).FanVolFlow != AutoSize ) {
					CBVAV( CBVAVNum ).MaxHeatAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
					ShowWarningError( CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );
					ShowContinueError( "The CBVAV system supply air fan air flow rate is less than the autosized value for the maximum air flow rate in heating mode. Consider autosizing the fan for this simulation." );
					ShowContinueError( "The maximum air flow rate in heating mode is reset to the supply air fan flow rate and the simulation continues." );
				}
				if ( CBVAV( CBVAVNum ).MaxHeatAirVolFlow < SmallAirVolFlow ) {
					CBVAV( CBVAVNum ).MaxHeatAirVolFlow = 0.0;
				}
				ReportSizingOutput( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, "maximum heating air flow rate [m3/s]", CBVAV( CBVAVNum ).MaxHeatAirVolFlow );

			}

		}

		if ( CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow == AutoSize ) {

			if ( CurSysNum > 0 ) {

				CheckSysSizing( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow && CBVAV( CBVAVNum ).FanVolFlow != AutoSize ) {
					CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
					ShowWarningError( CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );
					ShowContinueError( "The CBVAV system supply air fan air flow rate is less than the autosized value for the maximum air flow rate when no heating or cooling is needed. Consider autosizing the fan for this simulation." );
					ShowContinueError( "The maximum air flow rate when no heating or cooling is needed is reset to the supply air fan flow rate and the simulation continues." );
				}
				if ( CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow < SmallAirVolFlow ) {
					CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow = 0.0;
				}

				ReportSizingOutput( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, "maximum air flow rate when compressor/coil is off [m3/s]", CBVAV( CBVAVNum ).MaxNoCoolHeatAirVolFlow );

			}

		}

		if ( CBVAV( CBVAVNum ).CoolOutAirVolFlow == AutoSize ) {

			if ( CurSysNum > 0 ) {

				CheckSysSizing( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).CoolOutAirVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).CoolOutAirVolFlow && CBVAV( CBVAVNum ).FanVolFlow != AutoSize ) {
					CBVAV( CBVAVNum ).CoolOutAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
					ShowWarningError( CBVAV( CBVAVNum ).UnitType + " \"" + CBVAV( CBVAVNum ).Name + "\"" );
					ShowContinueError( "The CBVAV system supply air fan air flow rate is less than the autosized value for the outdoor air flow rate in cooling mode. Consider autosizing the fan for this simulation." );
					ShowContinueError( "The outdoor air flow rate in cooling mode is reset to the supply air fan flow rate and the simulation continues." );
				}
				if ( CBVAV( CBVAVNum ).CoolOutAirVolFlow < SmallAirVolFlow ) {
					CBVAV( CBVAVNum ).CoolOutAirVolFlow = 0.0;
				}
				ReportSizingOutput( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, "maximum outside air flow rate in cooling [m3/s]", CBVAV( CBVAVNum ).CoolOutAirVolFlow );

			}

		}

		if ( CBVAV( CBVAVNum ).HeatOutAirVolFlow == AutoSize ) {

			if ( CurSysNum > 0 ) {

				CheckSysSizing( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).HeatOutAirVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).HeatOutAirVolFlow && CBVAV( CBVAVNum ).FanVolFlow != AutoSize ) {
					CBVAV( CBVAVNum ).HeatOutAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
					ShowContinueError( "The CBVAV system supply air fan air flow rate is less than the autosized value for the outdoor air flow rate in heating mode. Consider autosizing the fan for this simulation." );
					ShowContinueError( "The outdoor air flow rate in heating mode is reset to the supply air fan flow rate and the simulation continues." );
				}
				if ( CBVAV( CBVAVNum ).HeatOutAirVolFlow < SmallAirVolFlow ) {
					CBVAV( CBVAVNum ).HeatOutAirVolFlow = 0.0;
				}
				ReportSizingOutput( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, "maximum outdoor air flow rate in heating [m3/s]", CBVAV( CBVAVNum ).CoolOutAirVolFlow );

			}

		}

		if ( CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow == AutoSize ) {

			if ( CurSysNum > 0 ) {

				CheckSysSizing( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name );
				CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
				if ( CBVAV( CBVAVNum ).FanVolFlow < CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow && CBVAV( CBVAVNum ).FanVolFlow != AutoSize ) {
					CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow = CBVAV( CBVAVNum ).FanVolFlow;
					ShowContinueError( "The CBVAV system supply air fan air flow rate is less than the autosized value for the outdoor air flow rate when no heating or cooling is needed. Consider autosizing the fan for this simulation." );
					ShowContinueError( "The outdoor air flow rate when no heating or cooling is needed is reset to the supply air fan flow rate and the simulation continues." );
				}
				if ( CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow < SmallAirVolFlow ) {
					CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow = 0.0;
				}
				ReportSizingOutput( CBVAV( CBVAVNum ).UnitType, CBVAV( CBVAVNum ).Name, "maximum outdoor air flow rate when compressor is off [m3/s]", CBVAV( CBVAVNum ).NoCoolHeatOutAirVolFlow );

			}

		}

	}

	void
	ControlCBVAVOutput(
		int const CBVAVNum, // Index to CBVAV system
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		Real64 & QZnReq, // Cooling or heating output needed by zone [W]
		Real64 & PartLoadFrac, // Unit part load fraction
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool & HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determine the part load fraction of the CBVAV system for this time step.

		// METHODOLOGY EMPLOYED:
		// Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::SmallTempDiff;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FullOutput; // Unit full output when compressor is operating [W]

		PartLoadFrac = 0.0;

		if ( GetCurrentScheduleValue( CBVAV( CBVAVNum ).SchedPtr ) == 0.0 ) return;

		// Get operating result
		PartLoadFrac = 1.0;
		CalcCBVAV( CBVAVNum, FirstHVACIteration, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, HXUnitOn );

		if ( ( Node( CBVAV( CBVAVNum ).AirOutNode ).Temp - CBVAV( CBVAVNum ).OutletTempSetPoint ) > SmallTempDiff && CBVAV( CBVAVNum ).HeatCoolMode > 0 && PartLoadFrac < 1.0 ) {
			CalcCBVAV( CBVAVNum, FirstHVACIteration, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, HXUnitOn );
		}

	}

	void
	CalcCBVAV(
		int const CBVAVNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // Flag for 1st HVAC iteration
		Real64 & PartLoadFrac, // Compressor part load fraction
		Real64 & LoadMet, // Load met by unit (W)
		Real64 & EP_UNUSED( QZnReq ), // Zone load (W)
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to AVERAGE airflow over timestep
		bool const HXUnitOn // flag to enable heat exchanger
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate the components making up the changeover-bypass VAV system.

		// METHODOLOGY EMPLOYED:
		// Simulates the unit components sequentially in the air flow direction.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using DXCoils::SimDXCoil;
		using DXCoils::SimDXCoilMultiMode;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using MixedAir::SimOAMixer;
		using DataHVACGlobals::SmallTempDiff;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdpFnWPb;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode; // CBVAV air outlet node
		int InletNode; // CBVAV air inlet node
		Real64 MinHumRat; // Minimum humidity ratio for sensible capacity calculation (kg/kg)
		Array1D< Real64 > Par( 6 ); // RegulaFalsi parameters
		int SolFla; // Flag of RegulaFalsi solver
		Real64 QHeater; // Load to be met by heater [W]
		Real64 QHeaterActual; // actual heating load met [W]
		Real64 CpAir; // Specific heat of air [J/kg-K]
		int MixerOutsideAirNode; // Outside air node number in OA mixer
		int MixerReliefAirNode; // Relief air node number in OA mixer
		int DehumidMode; // Dehumidification mode (0=normal, 1=enhanced)
		Real64 ApproachTemp;
		Real64 DesiredDewPoint;
		Real64 OutdoorDryBulbTemp; // Dry-bulb temperature at outdoor condenser
		Real64 OutdoorBaroPress; // Barometric pressure at outdoor condenser
		// FLOW

		OutletNode = CBVAV( CBVAVNum ).AirOutNode;
		InletNode = CBVAV( CBVAVNum ).AirInNode;
		MixerOutsideAirNode = CBVAV( CBVAVNum ).MixerOutsideAirNode;
		MixerReliefAirNode = CBVAV( CBVAVNum ).MixerReliefAirNode;
		if ( CBVAV( CBVAVNum ).CondenserNodeNum > 0 ) {
			OutdoorDryBulbTemp = Node( CBVAV( CBVAVNum ).CondenserNodeNum ).Temp;
			OutdoorBaroPress = Node( CBVAV( CBVAVNum ).CondenserNodeNum ).Press;
		} else {
			OutdoorDryBulbTemp = OutDryBulbTemp;
			OutdoorBaroPress = OutBaroPress;
		}

		SaveCompressorPLR = 0.0;

		// Bypass excess system air through bypass duct and calculate new mixed air conditions at OA mixer inlet node
		Node( CBVAV( CBVAVNum ).MixerInletAirNode ).Temp = ( 1.0 - BypassDuctFlowFraction ) * Node( InletNode ).Temp + BypassDuctFlowFraction * Node( OutletNode ).Temp;
		Node( CBVAV( CBVAVNum ).MixerInletAirNode ).HumRat = ( 1.0 - BypassDuctFlowFraction ) * Node( InletNode ).HumRat + BypassDuctFlowFraction * Node( OutletNode ).HumRat;
		Node( CBVAV( CBVAVNum ).MixerInletAirNode ).Enthalpy = PsyHFnTdbW( Node( CBVAV( CBVAVNum ).MixerInletAirNode ).Temp, Node( CBVAV( CBVAVNum ).MixerInletAirNode ).HumRat );
		SimOAMixer( CBVAV( CBVAVNum ).OAMixName, FirstHVACIteration, CBVAV( CBVAVNum ).OAMixIndex );

		if ( CBVAV( CBVAVNum ).FanPlace == BlowThru ) SimulateFanComponents( CBVAV( CBVAVNum ).FanName, FirstHVACIteration, CBVAV( CBVAVNum ).FanIndex, FanSpeedRatio );

		// Simulate cooling coil if zone load is negative (cooling load)
		if ( CBVAV( CBVAVNum ).HeatCoolMode == CoolingMode ) {
			if ( OutdoorDryBulbTemp >= CBVAV( CBVAVNum ).MinOATCompressor ) {

				{ auto const SELECT_CASE_var( CBVAV( CBVAVNum ).DXCoolCoilType_Num );

				if ( SELECT_CASE_var == CoilDX_CoolingHXAssisted ) {
					SimHXAssistedCoolingCoil( CBVAV( CBVAVNum ).DXCoolCoilName, FirstHVACIteration, On, PartLoadFrac, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, HXUnitOn );
					if ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp <= CBVAV( CBVAVNum ).CoilTempSetPoint ) {
						//         If coil inlet temp is already below the setpoint, simulated with coil off
						PartLoadFrac = 0.0;
						SimHXAssistedCoolingCoil( CBVAV( CBVAVNum ).DXCoolCoilName, FirstHVACIteration, Off, PartLoadFrac, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, HXUnitOn );
					} else if ( Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp < CBVAV( CBVAVNum ).CoilTempSetPoint && Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp > CBVAV( CBVAVNum ).CoilTempSetPoint ) {
						Par( 1 ) = double( CBVAV( CBVAVNum ).CoolCoilCompIndex );
						Par( 2 ) = CBVAV( CBVAVNum ).CoilTempSetPoint;
						Par( 3 ) = OnOffAirFlowRatio;
						Par( 4 ) = double( CBVAVNum );
						if ( FirstHVACIteration ) {
							Par( 5 ) = 1.0;
						} else {
							Par( 5 ) = 0.0;
						}
						if ( HXUnitOn ) {
							Par( 6 ) = 1.0;
						} else {
							Par( 6 ) = 0.0;
						}
						SolveRegulaFalsi( SmallTempDiff, MaxIte, SolFla, PartLoadFrac, HXAssistDXCoilResidual, 0.0, 1.0, Par );
						SimHXAssistedCoolingCoil( CBVAV( CBVAVNum ).DXCoolCoilName, FirstHVACIteration, On, PartLoadFrac, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, HXUnitOn );
						if ( SolFla == -1 && ! WarmupFlag ) {
							if ( CBVAV( CBVAVNum ).HXDXIterationExceeded < 1 ) {
								++CBVAV( CBVAVNum ).HXDXIterationExceeded;
								ShowWarningError( "Iteration limit exceeded calculating HX assisted DX unit part-load ratio, for unit = " + CBVAV( CBVAVNum ).DXCoolCoilName );
								ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
								ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
							} else {
								ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Iteration limit exceeded for HX assisted DX unit part-load ratio error continues.", CBVAV( CBVAVNum ).HXDXIterationExceededIndex, PartLoadFrac, PartLoadFrac );
							}
						} else if ( SolFla == -2 && ! WarmupFlag ) {
							PartLoadFrac = max( 0.0, min( 1.0, ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - CBVAV( CBVAVNum ).CoilTempSetPoint ) / ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp ) ) );
							if ( CBVAV( CBVAVNum ).HXDXIterationFailed < 1 ) {
								++CBVAV( CBVAVNum ).HXDXIterationFailed;
								ShowSevereError( "HX assisted DX unit part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + CBVAV( CBVAVNum ).DXCoolCoilName );
								ShowContinueErrorTimeStamp( "An estimated part-load ratio of " + RoundSigDigits( PartLoadFrac, 3 ) + "will be used and the simulation continues. Occurrence info:" );
							} else {
								ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Part-load ratio calculation failed for HX assisted DX unit error continues.", CBVAV( CBVAVNum ).HXDXIterationFailedIndex, PartLoadFrac, PartLoadFrac );
							}
						}
					}
				} else if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) {
					SimDXCoil( CBVAV( CBVAVNum ).DXCoolCoilName, On, FirstHVACIteration, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, PartLoadFrac, OnOffAirFlowRatio );
					if ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp < CBVAV( CBVAVNum ).CoilTempSetPoint ) {
						//         If coil inlet temp is already below the setpoint, simulated with coil off
						PartLoadFrac = 0.0;
						SimDXCoil( CBVAV( CBVAVNum ).DXCoolCoilName, On, FirstHVACIteration, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, PartLoadFrac, OnOffAirFlowRatio );
					} else if ( Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp < CBVAV( CBVAVNum ).CoilTempSetPoint && Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp > CBVAV( CBVAVNum ).CoilTempSetPoint ) {
						Par( 1 ) = double( CBVAV( CBVAVNum ).CoolCoilCompIndex );
						Par( 2 ) = CBVAV( CBVAVNum ).CoilTempSetPoint;
						Par( 3 ) = OnOffAirFlowRatio;
						SolveRegulaFalsi( SmallTempDiff, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilResidual, 0.0, 1.0, Par );
						SimDXCoil( CBVAV( CBVAVNum ).DXCoolCoilName, On, FirstHVACIteration, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, PartLoadFrac, OnOffAirFlowRatio );
						if ( SolFla == -1 && ! WarmupFlag ) {
							if ( CBVAV( CBVAVNum ).DXIterationExceeded < 1 ) {
								++CBVAV( CBVAVNum ).DXIterationExceeded;
								ShowWarningError( "Iteration limit exceeded calculating DX unit part-load ratio, for unit = " + CBVAV( CBVAVNum ).DXCoolCoilName );
								ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
								ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
							} else {
								ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Iteration limit exceeded for DX unit part-load ratio calculation error continues.", CBVAV( CBVAVNum ).DXIterationExceededIndex, PartLoadFrac, PartLoadFrac );
							}
						} else if ( SolFla == -2 && ! WarmupFlag ) {
							PartLoadFrac = max( 0.0, min( 1.0, ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - CBVAV( CBVAVNum ).CoilTempSetPoint ) / ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp ) ) );
							if ( CBVAV( CBVAVNum ).DXIterationFailed < 1 ) {
								++CBVAV( CBVAVNum ).DXIterationFailed;
								ShowSevereError( "DX unit part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + CBVAV( CBVAVNum ).DXCoolCoilName );
								ShowContinueErrorTimeStamp( "An estimated part-load ratio of " + RoundSigDigits( PartLoadFrac, 3 ) + "will be used and the simulation continues. Occurrence info:" );
							} else {
								ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Part-load ratio calculation failed for DX unit error continues.", CBVAV( CBVAVNum ).DXIterationFailedIndex, PartLoadFrac, PartLoadFrac );
							}
						}
					}

				} else if ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode
					// formerly (v3 and beyond) Coil:DX:MultiMode:CoolingEmpirical

					// If DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
					// Multimode coil will switch to enhanced dehumidification if available and needed, but it
					// still runs to meet the sensible load

					// Determine required part load for normal mode

					// Get full load result
					DehumidMode = 0;
					CBVAV( CBVAVNum ).DehumidificationMode = DehumidMode;
					SimDXCoilMultiMode( CBVAV( CBVAVNum ).DXCoolCoilName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil );
					if ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp < CBVAV( CBVAVNum ).CoilTempSetPoint ) {
						PartLoadFrac = 0.0;
						SimDXCoilMultiMode( CBVAV( CBVAVNum ).DXCoolCoilName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil );
					} else if ( Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp > CBVAV( CBVAVNum ).CoilTempSetPoint ) {
						PartLoadFrac = 1.0;
					} else {
						Par( 1 ) = double( CBVAV( CBVAVNum ).CoolCoilCompIndex );
						Par( 2 ) = CBVAV( CBVAVNum ).CoilTempSetPoint;
						// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
						Par( 3 ) = double( DehumidMode );
						SolveRegulaFalsi( SmallTempDiff, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par );
						if ( SolFla == -1 ) {
							if ( CBVAV( CBVAVNum ).MMDXIterationExceeded < 1 ) {
								++CBVAV( CBVAVNum ).MMDXIterationExceeded;
								ShowWarningError( "Iteration limit exceeded calculating DX unit part-load ratio, for unit=" + CBVAV( CBVAVNum ).Name );
								ShowContinueErrorTimeStamp( "Part-load ratio returned = " + RoundSigDigits( PartLoadFrac, 2 ) );
								ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
							} else {
								ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Iteration limit exceeded calculating DX unit part-load ratio error continues.", CBVAV( CBVAVNum ).MMDXIterationExceededIndex, PartLoadFrac, PartLoadFrac );
							}
						} else if ( SolFla == -2 ) {
							PartLoadFrac = max( 0.0, min( 1.0, ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - CBVAV( CBVAVNum ).CoilTempSetPoint ) / ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp ) ) );
							if ( CBVAV( CBVAVNum ).MMDXIterationFailed < 1 ) {
								++CBVAV( CBVAVNum ).MMDXIterationFailed;
								ShowSevereError( "DX unit part-load ratio calculation failed: part-load ratio limits exceeded, for unit=" + CBVAV( CBVAVNum ).Name );
								ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
								ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
							} else {
								ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Part-load ratio calculation failed for DX unit error continues.", CBVAV( CBVAVNum ).MMDXIterationFailedIndex, PartLoadFrac, PartLoadFrac );
							}
						}
					}

					// If humidity setpoint is not satisfied and humidity control type is Multimode,
					// then turn on enhanced dehumidification mode 1

					if ( ( Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).HumRat > Node( OutletNode ).HumRatMax ) && ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).HumRat > Node( OutletNode ).HumRatMax ) && ( CBVAV( CBVAVNum ).DehumidControlType == DehumidControl_Multimode ) && Node( OutletNode ).HumRatMax > 0.0 ) {

						// Determine required part load for enhanced dehumidification mode 1

						// Get full load result
						PartLoadFrac = 1.0;
						DehumidMode = 1;
						CBVAV( CBVAVNum ).DehumidificationMode = DehumidMode;
						SimDXCoilMultiMode( CBVAV( CBVAVNum ).DXCoolCoilName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil );
						if ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp < CBVAV( CBVAVNum ).CoilTempSetPoint ) {
							PartLoadFrac = 0.0;
						} else if ( Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp > CBVAV( CBVAVNum ).CoilTempSetPoint ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( CBVAV( CBVAVNum ).CoolCoilCompIndex );
							Par( 2 ) = CBVAV( CBVAVNum ).CoilTempSetPoint;
							// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
							Par( 3 ) = double( DehumidMode );
							SolveRegulaFalsi( SmallTempDiff, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( CBVAV( CBVAVNum ).DMDXIterationExceeded < 1 ) {
									++CBVAV( CBVAVNum ).DMDXIterationExceeded;
									ShowWarningError( "Iteration limit exceeded calculating DX unit dehumidifying part-load ratio, for unit = " + CBVAV( CBVAVNum ).Name );
									ShowContinueErrorTimeStamp( "Part-load ratio returned=" + RoundSigDigits( PartLoadFrac, 2 ) );
									ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
								} else {
									ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Iteration limit exceeded calculating DX unit dehumidifying part-load ratio error continues.", CBVAV( CBVAVNum ).DMDXIterationExceededIndex, PartLoadFrac, PartLoadFrac );
								}
							} else if ( SolFla == -2 ) {
								PartLoadFrac = max( 0.0, min( 1.0, ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - CBVAV( CBVAVNum ).CoilTempSetPoint ) / ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp ) ) );
								if ( CBVAV( CBVAVNum ).DMDXIterationFailed < 1 ) {
									++CBVAV( CBVAVNum ).DMDXIterationFailed;
									ShowSevereError( "DX unit dehumidifying part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + CBVAV( CBVAVNum ).Name );
									ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
									ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
								} else {
									ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Dehumidifying part-load ratio calculation failed for DX unit error continues.", CBVAV( CBVAVNum ).DMDXIterationFailedIndex, PartLoadFrac, PartLoadFrac );
								}
							}
						}
					} // End if humidity ratio setpoint not met - multimode humidity control

					// If humidity setpoint is not satisfied and humidity control type is CoolReheat,
					// then run to meet latent load

					if ( ( Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).HumRat > Node( OutletNode ).HumRatMax ) && ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).HumRat > Node( OutletNode ).HumRatMax ) && ( CBVAV( CBVAVNum ).DehumidControlType == DehumidControl_CoolReheat ) && Node( OutletNode ).HumRatMax > 0.0 ) {

						// Determine revised desired outlet temperature  - use approach temperature control strategy
						// based on CONTROLLER:SIMPLE TEMPANDHUMRAT control type.

						// Calculate the approach temperature (difference between SA dry-bulb temp and SA dew point temp)
						ApproachTemp = Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp - PsyTdpFnWPb( Node( OutletNode ).HumRat, OutdoorBaroPress );
						// Calculate the dew point temperature at the SA humidity ratio setpoint
						DesiredDewPoint = PsyTdpFnWPb( Node( OutletNode ).HumRatMax, OutdoorBaroPress );
						// Adjust the calculated dew point temperature by the approach temp
						CBVAV( CBVAVNum ).CoilTempSetPoint = min( CBVAV( CBVAVNum ).CoilTempSetPoint, ( DesiredDewPoint + ApproachTemp ) );

						// Determine required part load for cool reheat at adjusted DesiredOutletTemp

						// Get full load result
						PartLoadFrac = 1.0;
						DehumidMode = 0;
						CBVAV( CBVAVNum ).DehumidificationMode = DehumidMode;
						SimDXCoilMultiMode( CBVAV( CBVAVNum ).DXCoolCoilName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil );
						if ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp < CBVAV( CBVAVNum ).CoilTempSetPoint ) {
							PartLoadFrac = 0.0;
						} else if ( Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp > CBVAV( CBVAVNum ).CoilTempSetPoint ) {
							PartLoadFrac = 1.0;
						} else {
							Par( 1 ) = double( CBVAV( CBVAVNum ).CoolCoilCompIndex );
							Par( 2 ) = CBVAV( CBVAVNum ).CoilTempSetPoint;
							// Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
							Par( 3 ) = double( DehumidMode );
							SolveRegulaFalsi( SmallTempDiff, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0, 1.0, Par );
							if ( SolFla == -1 ) {
								if ( CBVAV( CBVAVNum ).CRDXIterationExceeded < 1 ) {
									++CBVAV( CBVAVNum ).CRDXIterationExceeded;
									ShowWarningError( "Iteration limit exceeded calculating DX unit cool reheat part-load ratio, for unit = " + CBVAV( CBVAVNum ).Name );
									ShowContinueErrorTimeStamp( "Part-load ratio returned = " + RoundSigDigits( PartLoadFrac, 2 ) );
									ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
								} else {
									ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Iteration limit exceeded calculating cool reheat part-load ratio DX unit error continues.", CBVAV( CBVAVNum ).CRDXIterationExceededIndex, PartLoadFrac, PartLoadFrac );
								}
							} else if ( SolFla == -2 ) {
								PartLoadFrac = max( 0.0, min( 1.0, ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - CBVAV( CBVAVNum ).CoilTempSetPoint ) / ( Node( CBVAV( CBVAVNum ).DXCoilInletNode ).Temp - Node( CBVAV( CBVAVNum ).DXCoilOutletNode ).Temp ) ) );
								if ( CBVAV( CBVAVNum ).CRDXIterationFailed < 1 ) {
									++CBVAV( CBVAVNum ).CRDXIterationFailed;
									ShowSevereError( "DX unit cool reheat part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + CBVAV( CBVAVNum ).Name );
									ShowContinueError( "Estimated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
									ShowContinueErrorTimeStamp( "The estimated part-load ratio will be used and the simulation continues. Occurrence info:" );
								} else {
									ShowRecurringWarningErrorAtEnd( CBVAV( CBVAVNum ).Name + ", Dehumidifying part-load ratio calculation failed for DX unit error continues.", CBVAV( CBVAVNum ).DMDXIterationFailedIndex, PartLoadFrac, PartLoadFrac );
								}
							}
						}
					} // End if humidity ratio setpoint not met - CoolReheat humidity control

					if ( PartLoadFrac > 1.0 ) {
						PartLoadFrac = 1.0;
					} else if ( PartLoadFrac < 0.0 ) {
						PartLoadFrac = 0.0;
					}

				} else {
					ShowFatalError( "SimCBVAV System: Invalid DX Cooling Coil=" + CBVAV( CBVAVNum ).DXCoolCoilType );

				}}
			} else { // IF(OutdoorDryBulbTemp .GE. CBVAV(CBVAVNum)%MinOATCompressor)THEN
				//     Simulate DX cooling coil with compressor off
				if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingHXAssisted ) {
					SimHXAssistedCoolingCoil( CBVAV( CBVAVNum ).DXCoolCoilName, FirstHVACIteration, Off, 0.0, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, HXUnitOn );
				} else if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingSingleSpeed ) {
					SimDXCoil( CBVAV( CBVAVNum ).DXCoolCoilName, Off, FirstHVACIteration, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, 0.0, OnOffAirFlowRatio );
				} else if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					SimDXCoilMultiMode( CBVAV( CBVAVNum ).DXCoolCoilName, Off, FirstHVACIteration, 0.0, 0, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil );
				}
			}
			SaveCompressorPLR = DXCoilPartLoadRatio( CBVAV( CBVAVNum ).DXCoolCoilIndexNum );
			// Simulate cooling coil with compressor off if zone requires heating
		} else { // HeatCoolMode == HeatingMode and no cooling is required, set PLR to 0
			if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingHXAssisted ) {
				SimHXAssistedCoolingCoil( CBVAV( CBVAVNum ).DXCoolCoilName, FirstHVACIteration, Off, 0.0, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, HXUnitOn );
			} else if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingSingleSpeed ) {
				SimDXCoil( CBVAV( CBVAVNum ).DXCoolCoilName, Off, FirstHVACIteration, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil, 0.0, OnOffAirFlowRatio );
			} else if ( CBVAV( CBVAVNum ).DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
				SimDXCoilMultiMode( CBVAV( CBVAVNum ).DXCoolCoilName, Off, FirstHVACIteration, 0.0, 0, CBVAV( CBVAVNum ).CoolCoilCompIndex, ContFanCycCoil );
			}
		}

		// Simulate the heating coil based on coil type
		{ auto const SELECT_CASE_var( CBVAV( CBVAVNum ).HeatCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) {
			//   Simulate DX heating coil if zone load is positive (heating load)
			if ( CBVAV( CBVAVNum ).HeatCoolMode == HeatingMode ) {
				if ( OutdoorDryBulbTemp > CBVAV( CBVAVNum ).MinOATCompressor ) {
					//       simulate the DX heating coil
					SimDXCoil( CBVAV( CBVAVNum ).HeatCoilName, On, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, ContFanCycCoil, PartLoadFrac, OnOffAirFlowRatio );
					if ( Node( CBVAV( CBVAVNum ).HeatingCoilOutletNode ).Temp > CBVAV( CBVAVNum ).CoilTempSetPoint && Node( CBVAV( CBVAVNum ).HeatingCoilInletNode ).Temp < CBVAV( CBVAVNum ).CoilTempSetPoint ) {
						// iterate to find PLR at CoilTempSetPoint
						Par( 1 ) = double( CBVAV( CBVAVNum ).HeatCoilIndex );
						Par( 2 ) = min( CBVAV( CBVAVNum ).CoilTempSetPoint, CBVAV( CBVAVNum ).MaxLATHeating );
						Par( 3 ) = OnOffAirFlowRatio;
						SolveRegulaFalsi( SmallTempDiff, MaxIte, SolFla, PartLoadFrac, DXHeatingCoilResidual, 0.0, 1.0, Par );
						SimDXCoil( CBVAV( CBVAVNum ).HeatCoilName, On, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, ContFanCycCoil, PartLoadFrac, OnOffAirFlowRatio );
						if ( SolFla == -1 && ! WarmupFlag ) {
							ShowWarningError( "Iteration limit exceeded calculating DX unit part-load ratio, for unit = " + CBVAV( CBVAVNum ).HeatCoilName );
							ShowContinueError( "Calculated part-load ratio = " + RoundSigDigits( PartLoadFrac, 3 ) );
							ShowContinueErrorTimeStamp( "The calculated part-load ratio will be used and the simulation continues. Occurrence info:" );
						} else if ( SolFla == -2 && ! WarmupFlag ) {
							ShowSevereError( "DX unit part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + CBVAV( CBVAVNum ).HeatCoilName );
							ShowContinueErrorTimeStamp( "A part-load ratio of " + RoundSigDigits( PartLoadFrac, 3 ) + "will be used and the simulation continues. Occurrence info:" );
							ShowContinueError( "Please send this information to the EnergyPlus support group." );
						}
					}
				} else { // OAT .LT. MinOATCompressor
					//       simulate DX heating coil with compressor off
					SimDXCoil( CBVAV( CBVAVNum ).HeatCoilName, Off, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, ContFanCycCoil, 0.0, OnOffAirFlowRatio );
				}
				SaveCompressorPLR = DXCoilPartLoadRatio( CBVAV( CBVAVNum ).DXHeatCoilIndexNum );
			} else { // HeatCoolMode = CoolingMode
				//     simulate DX heating coil with compressor off when cooling load is required
				SimDXCoil( CBVAV( CBVAVNum ).HeatCoilName, Off, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, ContFanCycCoil, 0.0, OnOffAirFlowRatio );
			}
		} else if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) || ( SELECT_CASE_var == Coil_HeatingWater ) || ( SELECT_CASE_var == Coil_HeatingSteam ) ) { // not a DX heating coil
			if ( CBVAV( CBVAVNum ).HeatCoolMode == HeatingMode ) {
				CpAir = PsyCpAirFnWTdb( Node( CBVAV( CBVAVNum ).HeatingCoilInletNode ).HumRat, Node( CBVAV( CBVAVNum ).HeatingCoilInletNode ).Temp );
				QHeater = Node( CBVAV( CBVAVNum ).HeatingCoilInletNode ).MassFlowRate * CpAir * ( CBVAV( CBVAVNum ).CoilTempSetPoint - Node( CBVAV( CBVAVNum ).HeatingCoilInletNode ).Temp );
			} else {
				QHeater = 0.0;
			}
			// Added None DX heating coils calling point
			CalcNonDXHeatingCoils( CBVAVNum, FirstHVACIteration, QHeater, CBVAV( CBVAVNum ).OpMode, QHeaterActual );
		} else {
			ShowFatalError( "SimCBVAV System: Invalid Heating Coil=" + CBVAV( CBVAVNum ).HeatCoilType );

		}}

		if ( CBVAV( CBVAVNum ).FanPlace == DrawThru ) SimulateFanComponents( CBVAV( CBVAVNum ).FanName, FirstHVACIteration, CBVAV( CBVAVNum ).FanIndex, FanSpeedRatio );

		Node( OutletNode ).MassFlowRate = ( 1.0 - BypassDuctFlowFraction ) * Node( CBVAV( CBVAVNum ).MixerInletAirNode ).MassFlowRate;
		Node( OutletNode ).Temp = Node( CBVAV( CBVAVNum ).SplitterOutletAirNode ).Temp;
		Node( OutletNode ).HumRat = Node( CBVAV( CBVAVNum ).SplitterOutletAirNode ).HumRat;
		Node( OutletNode ).Quality = Node( CBVAV( CBVAVNum ).SplitterOutletAirNode ).Quality;
		Node( OutletNode ).Press = Node( CBVAV( CBVAVNum ).SplitterOutletAirNode ).Press;
		Node( OutletNode ).Enthalpy = Node( CBVAV( CBVAVNum ).SplitterOutletAirNode ).Enthalpy;
		Node( OutletNode ).Height = Node( CBVAV( CBVAVNum ).SplitterOutletAirNode ).Height;

		CBVAV( CBVAVNum ).BypassMassFlowRate = BypassDuctFlowFraction * Node( CBVAV( CBVAVNum ).MixerInletAirNode ).MassFlowRate;

		// calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
		MinHumRat = min( Node( InletNode ).HumRat, Node( OutletNode ).HumRat );
		LoadMet = Node( OutletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRat ) );

	}

	void
	GetZoneLoads(
		int const CBVAVNum, // Index to CBVAV unit being simulated
		Real64 & QZoneReq // Total zone load served by this air loop
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is used to poll the thermostats in each zone and determine the
		// mode of operation, either cooling, heating, or none.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // Zone number of controlled zone in CBVAV loop
		Real64 QZoneReqCool; // Total cooling load in all controlled zones [W]
		Real64 QZoneReqHeat; // Total heating load in all controlled zones [W]
		Real64 ZoneLoad; // Total load in controlled zone [W]
		Real64 ZoneLoadToCoolSPSequenced;
		Real64 ZoneLoadToHeatSPSequenced;

		QZoneReqCool = 0.0;
		QZoneReqHeat = 0.0;
		CBVAV( CBVAVNum ).NumZonesCooled = 0;
		CBVAV( CBVAVNum ).NumZonesHeated = 0;
		CBVAV( CBVAVNum ).HeatCoolMode = 0;

		for ( ZoneNum = 1; ZoneNum <= CBVAV( CBVAVNum ).NumControlledZones; ++ZoneNum ) {
			if ( ( CBVAV( CBVAVNum ).ZoneSequenceCoolingNum( ZoneNum ) > 0 ) && ( CBVAV( CBVAVNum ).ZoneSequenceHeatingNum( ZoneNum ) > 0 ) ) {
				ZoneLoadToCoolSPSequenced = ZoneSysEnergyDemand( CBVAV( CBVAVNum ).ControlledZoneNum( ZoneNum ) ).SequencedOutputRequiredToCoolingSP( CBVAV( CBVAVNum ).ZoneSequenceCoolingNum( ZoneNum ) );
				ZoneLoadToHeatSPSequenced = ZoneSysEnergyDemand( CBVAV( CBVAVNum ).ControlledZoneNum( ZoneNum ) ).SequencedOutputRequiredToHeatingSP( CBVAV( CBVAVNum ).ZoneSequenceHeatingNum( ZoneNum ) );
				if ( ZoneLoadToHeatSPSequenced > 0.0 && ZoneLoadToCoolSPSequenced > 0.0 ) {
					ZoneLoad = ZoneLoadToHeatSPSequenced;
				} else if ( ZoneLoadToHeatSPSequenced < 0.0 && ZoneLoadToCoolSPSequenced < 0.0 ) {
					ZoneLoad = ZoneLoadToCoolSPSequenced;
				} else if ( ZoneLoadToHeatSPSequenced <= 0.0 && ZoneLoadToCoolSPSequenced >= 0.0 ) {
					ZoneLoad = 0.0;
				}
			} else {
				ZoneLoad = ZoneSysEnergyDemand( CBVAV( CBVAVNum ).ControlledZoneNum( ZoneNum ) ).RemainingOutputRequired;
			}

			if ( ! CurDeadBandOrSetback( ZoneNum ) ) {
				if ( ZoneLoad > 0.0 && std::abs( ZoneLoad ) > SmallLoad ) {
					QZoneReqHeat += ZoneLoad;
					++CBVAV( CBVAVNum ).NumZonesHeated;
				} else if ( ZoneLoad < 0.0 && std::abs( ZoneLoad ) > SmallLoad ) {
					QZoneReqCool += ZoneLoad;
					++CBVAV( CBVAVNum ).NumZonesCooled;
				}
			}
		}

		{ auto const SELECT_CASE_var( CBVAV( CBVAVNum ).PriorityControl );
		if ( SELECT_CASE_var == CoolingPriority ) {
			if ( QZoneReqCool < 0.0 ) {
				QZoneReq = QZoneReqCool;
				CBVAV( CBVAVNum ).HeatCoolMode = CoolingMode;
			} else if ( QZoneReqHeat > 0.0 ) {
				QZoneReq = QZoneReqHeat;
				CBVAV( CBVAVNum ).HeatCoolMode = HeatingMode;
			} else {
				QZoneReq = 0.0;
			}
		} else if ( SELECT_CASE_var == HeatingPriority ) {
			if ( QZoneReqHeat > 0.0 ) {
				QZoneReq = QZoneReqHeat;
				CBVAV( CBVAVNum ).HeatCoolMode = HeatingMode;
			} else if ( QZoneReqCool < 0.0 ) {
				QZoneReq = QZoneReqCool;
				CBVAV( CBVAVNum ).HeatCoolMode = CoolingMode;
			} else {
				QZoneReq = 0.0;
			}
		} else if ( SELECT_CASE_var == ZonePriority ) {
			if ( CBVAV( CBVAVNum ).NumZonesHeated > CBVAV( CBVAVNum ).NumZonesCooled ) {
				if ( QZoneReqHeat > 0.0 ) {
					QZoneReq = QZoneReqHeat;
					CBVAV( CBVAVNum ).HeatCoolMode = HeatingMode;
				} else if ( QZoneReqCool < 0.0 ) {
					QZoneReq = QZoneReqCool;
					CBVAV( CBVAVNum ).HeatCoolMode = CoolingMode;
				} else {
					QZoneReq = 0.0;
				}
			} else if ( CBVAV( CBVAVNum ).NumZonesCooled > CBVAV( CBVAVNum ).NumZonesHeated ) {
				if ( QZoneReqCool < 0.0 ) {
					QZoneReq = QZoneReqCool;
					CBVAV( CBVAVNum ).HeatCoolMode = CoolingMode;
				} else if ( QZoneReqHeat > 0.0 ) {
					QZoneReq = QZoneReqHeat;
					CBVAV( CBVAVNum ).HeatCoolMode = HeatingMode;
				} else {
					QZoneReq = 0.0;
				}
			} else {
				if ( std::abs( QZoneReqCool ) > std::abs( QZoneReqHeat ) && QZoneReqCool != 0.0 ) {
					QZoneReq = QZoneReqCool;
					CBVAV( CBVAVNum ).HeatCoolMode = CoolingMode;
				} else if ( std::abs( QZoneReqCool ) < std::abs( QZoneReqHeat ) && QZoneReqHeat != 0.0 ) {
					QZoneReq = QZoneReqHeat;
					CBVAV( CBVAVNum ).HeatCoolMode = HeatingMode;
				} else if ( std::abs( QZoneReqCool ) == std::abs( QZoneReqHeat ) && QZoneReqCool != 0.0 ) {
					QZoneReq = QZoneReqCool;
					CBVAV( CBVAVNum ).HeatCoolMode = CoolingMode;
				} else {
					QZoneReq = 0.0;
				}
			}
		}}

	}

	Real64
	CalcSetPointTempTarget( int const CBVAVNumber ) // Index to changeover-bypass VAV system
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculate outlet air node temperature setpoint

		// METHODOLOGY EMPLOYED:
		//  Calculate an outlet temperature to satisfy zone loads. This temperature is calculated
		//  based on 1 zone's VAV box fully opened. The other VAV boxes are partially open (modulated).

		// REFERENCES:
		//  na

		// Using/Aliasing
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;

		// Return value
		Real64 CalcSetPointTempTarget;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		int OutletNode; // Outlet node of CBVAV system
		int ZoneNum; // Index to controlled zone
		int ZoneNodeNum; // Zone node number of controlled zone
		int BoxOutletNodeNum; // CBVAV box outlet node (zone supply inlet node)
		Real64 DXCoolCoilInletTemp; // Air temperature of CBVAV DX cooling coil air inlet node [C]
		Real64 OutAirTemp; // Outlet air temperature of CBVAV system [C]
		Real64 OutAirHumRat; // Outlet air humidity ratio of CBVAV system [C]
		Real64 ZoneLoad; // Zone load sensed by thermostat [W]
		Real64 CpSupplyAir; // Specific heat of CBVAV system outlet air [J/kg-K]
		Real64 QToCoolSetPt; // Zone load to cooling setpoint [W]
		Real64 QToHeatSetPt; // Zone load to heating setpoint [W]
		Real64 SupplyAirTemp; // Supply air temperature required to meet load [C]
		Real64 TSupplyToHeatSetPtMax; // Maximum of the supply air temperatures required to reach the heating setpoint [C]
		Real64 TSupplyToCoolSetPtMin; // Minimum of the supply air temperatures required to reach the cooling setpoint [C]
		Real64 SupplyAirTempToHeatSetPt; // Supply air temperature required to reach the heating setpoint [C]
		Real64 SupplyAirTempToCoolSetPt; // Supply air temperature required to reach the cooling setpoint [C]

		DXCoolCoilInletTemp = Node( CBVAV( CBVAVNumber ).DXCoilInletNode ).Temp;
		OutAirTemp = Node( CBVAV( CBVAVNumber ).AirOutNode ).Temp;
		OutAirHumRat = Node( CBVAV( CBVAVNumber ).AirOutNode ).HumRat;

		if ( CBVAV( CBVAVNumber ).HeatCoolMode == CoolingMode ) { // Cooling required
			CalcSetPointTempTarget = 99999.0;
		} else if ( CBVAV( CBVAVNumber ).HeatCoolMode == HeatingMode ) { // Heating required
			CalcSetPointTempTarget = -99999.0;
		}
		TSupplyToHeatSetPtMax = -99999.0;
		TSupplyToCoolSetPtMin = 99999.0;

		OutletNode = CBVAV( CBVAVNumber ).AirOutNode;
		for ( ZoneNum = 1; ZoneNum <= CBVAV( CBVAVNumber ).NumControlledZones; ++ZoneNum ) {
			ZoneNodeNum = CBVAV( CBVAVNumber ).ActualZoneNodeNum( ZoneNum );
			BoxOutletNodeNum = CBVAV( CBVAVNumber ).CBVAVBoxOutletNode( ZoneNum );
			if ( ( CBVAV( CBVAVNumber ).ZoneSequenceCoolingNum( ZoneNum ) > 0 ) && ( CBVAV( CBVAVNumber ).ZoneSequenceHeatingNum( ZoneNum ) > 0 ) ) {
				QToCoolSetPt = ZoneSysEnergyDemand( CBVAV( CBVAVNumber ).ControlledZoneNum( ZoneNum ) ).SequencedOutputRequiredToCoolingSP( CBVAV( CBVAVNumber ).ZoneSequenceCoolingNum( ZoneNum ) );
				QToHeatSetPt = ZoneSysEnergyDemand( CBVAV( CBVAVNumber ).ControlledZoneNum( ZoneNum ) ).SequencedOutputRequiredToHeatingSP( CBVAV( CBVAVNumber ).ZoneSequenceHeatingNum( ZoneNum ) );
				if ( QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 ) {
					ZoneLoad = QToHeatSetPt;
				} else if ( QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 ) {
					ZoneLoad = QToCoolSetPt;
				} else if ( QToHeatSetPt <= 0.0 && QToCoolSetPt >= 0.0 ) {
					ZoneLoad = 0.0;
				}
			} else {
				ZoneLoad = ZoneSysEnergyDemand( CBVAV( CBVAVNumber ).ControlledZoneNum( ZoneNum ) ).RemainingOutputRequired;
				QToCoolSetPt = ZoneSysEnergyDemand( CBVAV( CBVAVNumber ).ControlledZoneNum( ZoneNum ) ).OutputRequiredToCoolingSP;
				QToHeatSetPt = ZoneSysEnergyDemand( CBVAV( CBVAVNumber ).ControlledZoneNum( ZoneNum ) ).OutputRequiredToHeatingSP;
			}

			CpSupplyAir = PsyCpAirFnWTdb( OutAirHumRat, OutAirTemp );

			//     Find the supply air temperature that will force the box to full flow
			if ( BoxOutletNodeNum > 0 ) {
				if ( CpSupplyAir * Node( BoxOutletNodeNum ).MassFlowRateMax == 0.0 ) {
					SupplyAirTemp = Node( ZoneNodeNum ).Temp;
				} else {
					//         The target supply air temperature is slightly
					SupplyAirTemp = Node( ZoneNodeNum ).Temp + ZoneLoad / ( CpSupplyAir * Node( BoxOutletNodeNum ).MassFlowRateMax );
				}
			} else {
				SupplyAirTemp = Node( ZoneNodeNum ).Temp;
			}

			//     Save the MIN (cooling) or MAX (heating) temperature for coil control
			//     One box will always operate at maximum damper position minimizing overall system energy use
			if ( CBVAV( CBVAVNumber ).HeatCoolMode == CoolingMode ) {
				CalcSetPointTempTarget = min( SupplyAirTemp, CalcSetPointTempTarget );
			} else if ( CBVAV( CBVAVNumber ).HeatCoolMode == HeatingMode ) {
				CalcSetPointTempTarget = max( SupplyAirTemp, CalcSetPointTempTarget );
			} else {
				//       Should use CpAirAtCoolSetPoint or CpAirAtHeatSetPoint here?
				//       If so, use ZoneThermostatSetPointLo(ZoneNum) and ZoneThermostatSetPointHi(ZoneNum)
				//       along with the zone humidity ratio
				if ( CpSupplyAir * Node( BoxOutletNodeNum ).MassFlowRateMax == 0.0 ) {
					SupplyAirTempToHeatSetPt = Node( ZoneNodeNum ).Temp;
					SupplyAirTempToCoolSetPt = Node( ZoneNodeNum ).Temp;
				} else {
					SupplyAirTempToHeatSetPt = Node( ZoneNodeNum ).Temp + QToHeatSetPt / ( CpSupplyAir * Node( BoxOutletNodeNum ).MassFlowRateMax );
					SupplyAirTempToCoolSetPt = Node( ZoneNodeNum ).Temp + QToCoolSetPt / ( CpSupplyAir * Node( BoxOutletNodeNum ).MassFlowRateMax );
				}
				TSupplyToHeatSetPtMax = max( SupplyAirTempToHeatSetPt, TSupplyToHeatSetPtMax );
				TSupplyToCoolSetPtMin = min( SupplyAirTempToCoolSetPt, TSupplyToCoolSetPtMin );
			}

		}

		//   Account for floating condition where cooling/heating is required to avoid overshooting setpoint
		if ( CBVAV( CBVAVNumber ).HeatCoolMode == 0 && CBVAV( CBVAVNumber ).OpMode == ContFanCycCoil ) {
			if ( OutAirTemp > TSupplyToCoolSetPtMin ) {
				CalcSetPointTempTarget = TSupplyToCoolSetPtMin;
				CBVAV( CBVAVNumber ).HeatCoolMode = CoolingMode;
			} else if ( OutAirTemp < TSupplyToHeatSetPtMax ) {
				CalcSetPointTempTarget = TSupplyToHeatSetPtMax;
				CBVAV( CBVAVNumber ).HeatCoolMode = HeatingMode;
			} else {
				CalcSetPointTempTarget = OutAirTemp;
			}
			//   Reset setpoint to inlet air temp if unit is OFF and in cycling fan mode
		} else if ( CBVAV( CBVAVNumber ).HeatCoolMode == 0 && CBVAV( CBVAVNumber ).OpMode == CycFanCycCoil ) {
			CalcSetPointTempTarget = Node( CBVAV( CBVAVNumber ).AirInNode ).Temp;
			//   Reset cooling/heating mode to OFF if mixed air inlet temperature is below/above setpoint temperature.
			//   HeatCoolMode = 0 for OFF, 1 for cooling, 2 for heating
		} else if ( CBVAV( CBVAVNumber ).HeatCoolMode == CoolingMode && DXCoolCoilInletTemp < CalcSetPointTempTarget ) {
			CalcSetPointTempTarget = DXCoolCoilInletTemp;
			CBVAV( CBVAVNumber ).HeatCoolMode = 0;
		} else if ( CBVAV( CBVAVNumber ).HeatCoolMode == HeatingMode && DXCoolCoilInletTemp > CalcSetPointTempTarget ) {
			CalcSetPointTempTarget = DXCoolCoilInletTemp;
			CBVAV( CBVAVNumber ).HeatCoolMode = 0;
		}

		//   Limit outlet node temperature to MAX/MIN specified in input
		if ( CalcSetPointTempTarget < CBVAV( CBVAVNumber ).MinLATCooling ) CalcSetPointTempTarget = CBVAV( CBVAVNumber ).MinLATCooling;
		if ( CalcSetPointTempTarget > CBVAV( CBVAVNumber ).MaxLATHeating ) CalcSetPointTempTarget = CBVAV( CBVAVNumber ).MaxLATHeating;

		return CalcSetPointTempTarget;

	}

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2006
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcDoe2DXCoil;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // Index of this coil
		Real64 OutletAirTemp; // Outlet air temperature [C]
		Real64 OnOffAirFlowFrac; // Ratio of compressor ON to compressor OFF air mass flow rate

		CoilIndex = int( Par( 1 ) );
		OnOffAirFlowFrac = Par( 3 );

		CalcDoe2DXCoil( CoilIndex, On, false, PartLoadFrac, ContFanCycCoil, _, OnOffAirFlowFrac );

		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	HXAssistDXCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2006
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired DX coil outlet temp - actual DX coil outlet temp)
		// HX Assisted DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // Index of this coil
		Real64 OutletAirTemp; // Outlet air temperature of DX cooling coil [C]
		Real64 OnOffAirFlowFrac; // Air flow fraction
		int CBVAVNumTemp; // Local index to changeover-bypass VAV system
		bool FirstHVACIter; // Local Flag denoting the first pass on the air loop simulation
		bool HXUnitOn; // flag to enable heat exchanger

		CoilIndex = int( Par( 1 ) );
		OnOffAirFlowFrac = Par( 3 );
		CBVAVNumTemp = int( Par( 4 ) );
		FirstHVACIter = ( Par( 5 ) == 1.0 );
		HXUnitOn = ( Par( 6 ) == 1.0 );

		SimHXAssistedCoolingCoil( CBVAV( CBVAVNumTemp ).DXCoolCoilName, FirstHVACIter, On, PartLoadFrac, CoilIndex, ContFanCycCoil, HXUnitOn );

		OutletAirTemp = Node( CBVAV( CBVAVNumTemp ).DXCoilOutletNode ).Temp;
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2006
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::CalcDXHeatingCoil;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// Par(2) = desired air outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // Index of this coil
		Real64 OutletAirTemp; // Outlet air temperature [C]
		Real64 OnOffAirFlowFrac; // Ratio of compressor ON to compressor OFF air mass flow rate

		CoilIndex = int( Par( 1 ) );
		OnOffAirFlowFrac = Par( 3 );

		CalcDXHeatingCoil( CoilIndex, PartLoadFrac, ContFanCycCoil, OnOffAirFlowFrac );

		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         M. J. Witte, GARD Analytics, Inc.
		//       DATE WRITTEN   February 2005
		//                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// DX Coil output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilOutletTemp;
		using DXCoils::SimDXCoilMultiMode;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(3) = dehumidification mode (0=normal, 1=enhanced)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CoilIndex; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		int DehumidMode; // dehumidification mode (par3)
		int FanOpMode; // allows parent object to control fan mode

		CoilIndex = int( Par( 1 ) );
		DehumidMode = int( Par( 3 ) );
		FanOpMode = 2;
		SimDXCoilMultiMode( "", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode );
		OutletAirTemp = DXCoilOutletTemp( CoilIndex );
		Residuum = Par( 2 ) - OutletAirTemp;

		return Residuum;
	}

	void
	SetAverageAirFlow(
		int const CBVAVNum, // Index to CBVAV system
		Real64 & OnOffAirFlowRatio, // Ratio of compressor ON airflow to average airflow over timestep
		bool const FirstHVACIteration // Flag denoting the first pass on the air loop simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the average air mass flow rates for this time step
		// Set OnOffAirFlowRatio to be used by DX coils

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::SimOAMixer;
		using Psychrometrics::PsyCpAirFnWTdb;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode; // Inlet node number for CBVAVNum
		int OutletNode; // Outlet node number for CBVAVNum
		int MixerMixedAirNode; // Mixed air node number in OA mixer
		int MixerOutsideAirNode; // Outside air node number in OA mixer
		int MixerReliefAirNode; // Relief air node number in OA mixer
		int MixerInletAirNode; // Mixed air node number in OA mixer
		Real64 AverageUnitMassFlow; // Average system air mass flow rate over time step [kg/s]
		Real64 AverageOAMassFlow; // Average outdoor air mass flow rate over time step [kg/s]
		Real64 CpSupplyAir; // Specific heat of outlet air [J/kg-K]
		Real64 CpZoneAir; // Specific heat of zone air [J/kg-K]
		Real64 DeltaCpTemp; // Temperature difference from supply air to zone air [C]
		Real64 ZoneMassFlow; // Zone mass flow rate required to meet zone load [kg/s]
		Real64 SystemMassFlow; // System mass flow rate required for all zones [kg/s]
		int ZoneNum; // Index to zone
		Real64 ZoneLoad; // Zone load calculated by ZoneTempPredictor [W]
		Real64 QToHeatSetPt; // Load to heating setpoint [W]
		Real64 QToCoolSetPt; // Load to cooling setpoint [W]
		int ZoneNodeNum; // Actual zone number
		int BoxOutletNodeNum; // Zone supply air inlet node number

		InletNode = CBVAV( CBVAVNum ).AirInNode;
		OutletNode = CBVAV( CBVAVNum ).AirOutNode;
		MixerMixedAirNode = CBVAV( CBVAVNum ).MixerMixedAirNode;
		MixerOutsideAirNode = CBVAV( CBVAVNum ).MixerOutsideAirNode;
		MixerReliefAirNode = CBVAV( CBVAVNum ).MixerReliefAirNode;
		MixerInletAirNode = CBVAV( CBVAVNum ).MixerInletAirNode;

		SystemMassFlow = 0.0;
		CpSupplyAir = PsyCpAirFnWTdb( Node( OutletNode ).HumRat, Node( OutletNode ).Temp );
		// Determine zone air flow
		for ( ZoneNum = 1; ZoneNum <= CBVAV( CBVAVNum ).NumControlledZones; ++ZoneNum ) {
			ZoneNodeNum = CBVAV( CBVAVNum ).ActualZoneNodeNum( ZoneNum );
			BoxOutletNodeNum = CBVAV( CBVAVNum ).CBVAVBoxOutletNode( ZoneNum );
			if ( ( CBVAV( CBVAVNum ).ZoneSequenceCoolingNum( ZoneNum ) > 0 ) && ( CBVAV( CBVAVNum ).ZoneSequenceHeatingNum( ZoneNum ) > 0 ) ) {
				QToCoolSetPt = ZoneSysEnergyDemand( CBVAV( CBVAVNum ).ControlledZoneNum( ZoneNum ) ).SequencedOutputRequiredToCoolingSP( CBVAV( CBVAVNum ).ZoneSequenceCoolingNum( ZoneNum ) );
				QToHeatSetPt = ZoneSysEnergyDemand( CBVAV( CBVAVNum ).ControlledZoneNum( ZoneNum ) ).SequencedOutputRequiredToHeatingSP( CBVAV( CBVAVNum ).ZoneSequenceHeatingNum( ZoneNum ) );
				if ( QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 ) {
					ZoneLoad = QToHeatSetPt;
				} else if ( QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 ) {
					ZoneLoad = QToCoolSetPt;
				} else if ( QToHeatSetPt <= 0.0 && QToCoolSetPt >= 0.0 ) {
					ZoneLoad = 0.0;
				}
			} else {
				ZoneLoad = ZoneSysEnergyDemand( CBVAV( CBVAVNum ).ControlledZoneNum( ZoneNum ) ).RemainingOutputRequired;
				QToHeatSetPt = ZoneSysEnergyDemand( CBVAV( CBVAVNum ).ControlledZoneNum( ZoneNum ) ).OutputRequiredToHeatingSP;
			}
			CpZoneAir = PsyCpAirFnWTdb( Node( ZoneNodeNum ).HumRat, Node( ZoneNodeNum ).Temp );
			DeltaCpTemp = CpSupplyAir * Node( OutletNode ).Temp - CpZoneAir * Node( ZoneNodeNum ).Temp;

			//Need to check DeltaCpTemp and ensure that it is not zero
			if ( DeltaCpTemp != 0.0 ) { // .AND. .NOT. CurDeadBandOrSetback(ZoneNum))THEN
				ZoneMassFlow = ZoneLoad / DeltaCpTemp;
			} else {
				//     reset to 0 so we don't add in the last zone's mass flow rate
				ZoneMassFlow = 0.0;
			}
			SystemMassFlow += max( Node( BoxOutletNodeNum ).MassFlowRateMin, min( ZoneMassFlow, Node( BoxOutletNodeNum ).MassFlowRateMax ) );
		}

		AverageUnitMassFlow = CompOnMassFlow;
		AverageOAMassFlow = OACompOnMassFlow;
		FanSpeedRatio = CompOnFlowRatio;

		Node( MixerInletAirNode ) = Node( InletNode );

		Node( MixerMixedAirNode ).MassFlowRateMin = 0.0;

		if ( GetCurrentScheduleValue( CBVAV( CBVAVNum ).SchedPtr ) == 0.0 || AverageUnitMassFlow == 0.0 ) {
			Node( InletNode ).MassFlowRate = 0.0;
			Node( MixerOutsideAirNode ).MassFlowRate = 0.0;
			Node( MixerReliefAirNode ).MassFlowRate = 0.0;
			OnOffAirFlowRatio = 0.0;
			BypassDuctFlowFraction = 0.0;
		} else {
			Node( MixerInletAirNode ).MassFlowRate = AverageUnitMassFlow;
			Node( MixerOutsideAirNode ).MassFlowRate = AverageOAMassFlow;
			Node( MixerReliefAirNode ).MassFlowRate = AverageOAMassFlow;
			if ( FirstHVACIteration ) {
				OnOffAirFlowRatio = 1.0;
				BypassDuctFlowFraction = 0.0;
			} else {
				OnOffAirFlowRatio = 1.0;
				BypassDuctFlowFraction = max( 0.0, 1.0 - ( Node( InletNode ).MassFlowRate / AverageUnitMassFlow ) );
			}
		}

	}

	void
	ReportCBVAV( int const CBVAVNum ) // Index of the current CBVAV unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fills some of the report variables for the changeover-bypass VAV system

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ReportingConstant;

		// FLOW
		ReportingConstant = TimeStepSys * SecInHour;

		CBVAV( CBVAVNum ).TotCoolEnergy = CBVAV( CBVAVNum ).TotCoolEnergyRate * ReportingConstant;
		CBVAV( CBVAVNum ).TotHeatEnergy = CBVAV( CBVAVNum ).TotHeatEnergyRate * ReportingConstant;
		CBVAV( CBVAVNum ).SensCoolEnergy = CBVAV( CBVAVNum ).SensCoolEnergyRate * ReportingConstant;
		CBVAV( CBVAVNum ).SensHeatEnergy = CBVAV( CBVAVNum ).SensHeatEnergyRate * ReportingConstant;
		CBVAV( CBVAVNum ).LatCoolEnergy = CBVAV( CBVAVNum ).LatCoolEnergyRate * ReportingConstant;
		CBVAV( CBVAVNum ).LatHeatEnergy = CBVAV( CBVAVNum ).LatHeatEnergyRate * ReportingConstant;
		CBVAV( CBVAVNum ).ElecConsumption = CBVAV( CBVAVNum ).ElecPower * ReportingConstant;

	}

	void
	CalcNonDXHeatingCoils(
		int const CBVAVNum, // Changeover bypass VAV unit index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 & HeatCoilLoad, // heating coil load to be met (Watts)
		int const FanMode, // fan operation mode
		Real64 & HeatCoilLoadmet // coil heating load met
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

		// METHODOLOGY EMPLOYED:
		// Simply calls the different heating coil component.  The hot water flow rate matching the coil load
		// is calculated iteratively.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using PlantUtilities::SetComponentFlowRate;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ErrTolerance( 0.001 ); // convergence limit for hotwater coil
		int const SolveMaxIter( 50 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QCoilActual; // actual heating load met
		Real64 mdot; // heating coil steam or hot water mass flow rate
		Real64 MinWaterFlow; // minimum water mass flow rate
		Real64 MaxHotWaterFlow; // maximum hot water mass flow rate, kg/s
		Real64 HotWaterMdot; // actual hot water mass flow rate
		Array1D< Real64 > Par( 3 );
		int SolFlag; // error flag

		QCoilActual = 0.0;
		if ( HeatCoilLoad > SmallLoad ) {
			{ auto const SELECT_CASE_var( CBVAV( CBVAVNum ).HeatCoilType_Num );
			if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
				SimulateHeatingCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, HeatCoilLoad, CBVAV( CBVAVNum ).HeatCoilIndex, QCoilActual, true, FanMode );
			} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
				// simulate the heating coil at maximum hot water flow rate
				MaxHotWaterFlow = CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow;
				SetComponentFlowRate( MaxHotWaterFlow, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );
				SimulateWaterCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, QCoilActual, FanMode );
				if ( QCoilActual > ( HeatCoilLoad + SmallLoad ) ) {
					// control water flow to obtain output matching HeatCoilLoad
					SolFlag = 0;
					MinWaterFlow = 0.0;
					Par( 1 ) = double( CBVAVNum );
					if ( FirstHVACIteration ) {
						Par( 2 ) = 1.0;
					} else {
						Par( 2 ) = 0.0;
					}
					Par( 3 ) = HeatCoilLoad;
					SolveRegulaFalsi( ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par );
					if ( SolFlag == -1 ) {
						if ( CBVAV( CBVAVNum ).HotWaterCoilMaxIterIndex == 0 ) {
							ShowWarningMessage( "CalcNonDXHeatingCoils: Hot water coil control failed for " + CBVAV( CBVAVNum ).UnitType + "=\"" + CBVAV( CBVAVNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "  Iteration limit [" + RoundSigDigits( SolveMaxIter ) + "] exceeded in calculating hot water mass flow rate" );
						}
						ShowRecurringWarningErrorAtEnd( "CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit [" + RoundSigDigits( SolveMaxIter ) + "]) for " + CBVAV( CBVAVNum ).UnitType + "=\"" + CBVAV( CBVAVNum ).Name, CBVAV( CBVAVNum ).HotWaterCoilMaxIterIndex );
					} else if ( SolFlag == -2 ) {
						if ( CBVAV( CBVAVNum ).HotWaterCoilMaxIterIndex2 == 0 ) {
							ShowWarningMessage( "CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for " + CBVAV( CBVAVNum ).UnitType + "=\"" + CBVAV( CBVAVNum ).Name + "\"" );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "...Bad hot water maximum flow rate limits" );
							ShowContinueError( "...Given minimum water flow rate=" + RoundSigDigits( MinWaterFlow, 3 ) + " kg/s" );
							ShowContinueError( "...Given maximum water flow rate=" + RoundSigDigits( MaxHotWaterFlow, 3 ) + " kg/s" );
						}
						ShowRecurringWarningErrorAtEnd( "CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for " + CBVAV( CBVAVNum ).UnitType + "=\"" + CBVAV( CBVAVNum ).Name + "\"", CBVAV( CBVAVNum ).HotWaterCoilMaxIterIndex2, MaxHotWaterFlow, MinWaterFlow, _, "[kg/s]", "[kg/s]" );
					}
					// simulate the hot water heating coil
					QCoilActual = HeatCoilLoad;
					// simulate the hot water heating coil
					SimulateWaterCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, QCoilActual, FanMode );
				}
			} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
				mdot = CBVAV( CBVAVNum ).MaxHeatCoilFluidFlow;
				SetComponentFlowRate( mdot, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );

				// simulate the steam heating coil
				SimulateSteamCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, HeatCoilLoad, QCoilActual, FanMode );
			}}
		} else {
			{ auto const SELECT_CASE_var( CBVAV( CBVAVNum ).HeatCoilType_Num );
			if ( ( SELECT_CASE_var == Coil_HeatingGas ) || ( SELECT_CASE_var == Coil_HeatingElectric ) ) {
				SimulateHeatingCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, HeatCoilLoad, CBVAV( CBVAVNum ).HeatCoilIndex, QCoilActual, true, FanMode );
			} else if ( SELECT_CASE_var == Coil_HeatingWater ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );
				QCoilActual = HeatCoilLoad;
				// simulate the hot water heating coil
				SimulateWaterCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, QCoilActual, FanMode );
			} else if ( SELECT_CASE_var == Coil_HeatingSteam ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );
				// simulate the steam heating coil
				SimulateSteamCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACIteration, CBVAV( CBVAVNum ).HeatCoilIndex, HeatCoilLoad, QCoilActual, FanMode );
			}}
		}
		HeatCoilLoadmet = QCoilActual;

	}

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(1) = DX coil number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   January 2012
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Actual Coil Output - Requested Coil Load) / Requested Coil Load
		// the actual coil output depends on the hot water flow rate which is varied to minimize the residual.

		// METHODOLOGY EMPLOYED:
		// Calls HotWaterCoilResidual, and calculates the residual as defined above.

		// REFERENCES:

		// Using/Aliasing
		using WaterCoils::SimulateWaterCoilComponents;
		using PlantUtilities::SetComponentFlowRate;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CBVAVNum;
		bool FirstHVACSoln;
		Real64 QCoilActual; // delivered coild load, W
		Real64 HeatCoilLoad; // requested coild load, W
		Real64 mdot;

		CBVAVNum = int( Par( 1 ) );
		FirstHVACSoln = ( Par( 2 ) > 0.0 );
		HeatCoilLoad = Par( 3 );
		QCoilActual = HeatCoilLoad;
		mdot = HWFlow;
		SetComponentFlowRate( mdot, CBVAV( CBVAVNum ).CoilControlNode, CBVAV( CBVAVNum ).CoilOutletNode, CBVAV( CBVAVNum ).LoopNum, CBVAV( CBVAVNum ).LoopSide, CBVAV( CBVAVNum ).BranchNum, CBVAV( CBVAVNum ).CompNum );

		// simulate the hot water supplemental heating coil
		SimulateWaterCoilComponents( CBVAV( CBVAVNum ).HeatCoilName, FirstHVACSoln, CBVAV( CBVAVNum ).HeatCoilIndex, QCoilActual, CBVAV( CBVAVNum ).OpMode );
		if ( HeatCoilLoad != 0.0 ) {
			Residuum = ( QCoilActual - HeatCoilLoad ) / HeatCoilLoad;
		} else { //Autodesk:Return Condition added to assure return value is set
			Residuum = 0.0;
		}
		return Residuum;
	}

} // HVACUnitaryBypassVAV

} // EnergyPlus
