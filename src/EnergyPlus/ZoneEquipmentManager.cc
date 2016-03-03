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
#include <algorithm>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ZoneEquipmentManager.hh>
#include <BaseboardElectric.hh>
#include <BaseboardRadiator.hh>
#include <CoolTower.hh>
#include <DataAirflowNetwork.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataConvergParams.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DirectAirManager.hh>
#include <DisplayRoutines.hh>
#include <EarthTube.hh>
#include <ElectricBaseboardRadiator.hh>
#include <EMSManager.hh>
#include <EvaporativeCoolers.hh>
#include <FanCoilUnits.hh>
#include <Fans.hh>
#include <General.hh>
#include <HeatRecovery.hh>
#include <HighTempRadiantSystem.hh>
#include <HVACInterfaceManager.hh>
#include <HVACStandAloneERV.hh>
#include <HVACUnitarySystem.hh>
#include <HVACVariableRefrigerantFlow.hh>
#include <HWBaseboardRadiator.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <LowTempRadiantSystem.hh>
#include <OutdoorAirUnit.hh>
#include <PackagedTerminalHeatPump.hh>
#include <Psychrometrics.hh>
#include <PurchasedAirManager.hh>
#include <RefrigeratedCase.hh>
#include <ReturnAirPathManager.hh>
#include <ScheduleManager.hh>
#include <SplitterComponent.hh>
#include <SteamBaseboardRadiator.hh>
#include <SwimmingPool.hh>
#include <SystemAvailabilityManager.hh>
#include <ThermalChimney.hh>
#include <UnitHeater.hh>
#include <UnitVentilator.hh>
#include <UserDefinedComponents.hh>
#include <UtilityRoutines.hh>
#include <VentilatedSlab.hh>
#include <WaterThermalTanks.hh>
#include <WindowAC.hh>
#include <ZoneAirLoopEquipmentManager.hh>
#include <ZoneDehumidifier.hh>
#include <ZonePlenum.hh>
#include <ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace ZoneEquipmentManager {

	// Module containing the routines dealing with the Zone Equipment Manager.

	// MODULE INFORMATION:
	//       AUTHOR         Russ Taylor
	//       DATE WRITTEN   Unknown
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module manages the zone equipment.

	// METHODOLOGY EMPLOYED: none

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::NumOfZones;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::BeginHourFlag;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::DayOfSim;
	using DataGlobals::ZoneSizingCalc;
	using DataGlobals::OutputFileDebug;
	using namespace DataSizing;
	using DataEnvironment::TotDesDays;
	using DataEnvironment::CurEnvirNum;
	using DataEnvironment::EnvironmentName;
	using DataEnvironment::TotRunDesPersDays;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using namespace DataZoneEquipment;
	// Use statements for access to subroutines in other modules
	using Psychrometrics::PsyHFnTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyHgAirFnWTdb;
	using Psychrometrics::PsyWFnTdpPb;
	using Psychrometrics::PsyWFnTdbRhPb;

	// Data
	//MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool InitZoneEquipmentOneTimeFlag( true );
		bool InitZoneEquipmentEnvrnFlag( true );
		bool FirstPassZoneEquipFlag( true ); // indicates first pass through zone equipment, used to reset selected ZoneEqSizing variables
	}

	Array1D< Real64 > AvgData; // scratch array for storing averaged data
	Array1D_int DefaultSimOrder;
	int NumOfTimeStepInDay; // number of zone time steps in a day
	bool GetZoneEquipmentInputFlag( true );
	bool SizeZoneEquipmentOneTimeFlag( true );

	//SUBROUTINE SPECIFICATIONS FOR MODULE ZoneEquipmentManager

	// Object Data
	Array1D< SimulationOrder > PrioritySimOrder;

	// Functions
	void
	clear_state()
	{
		SizeZoneEquipmentOneTimeFlag = true;
		InitZoneEquipmentOneTimeFlag =  true;
		InitZoneEquipmentEnvrnFlag = true;
		AvgData.deallocate(); // scratch array for storing averaged data
		DefaultSimOrder.deallocate();
		NumOfTimeStepInDay = 0; // number of zone time steps in a day
		GetZoneEquipmentInputFlag = true;
		PrioritySimOrder.deallocate();
		FirstPassZoneEquipFlag = true;
	}

	void
	ManageZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimZone,
		bool & SimAir
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calls the zone thermal control simulations and the interfaces
		// (water-air, refrigerant-air, steam-air, electric-electric,
		// water-water, etc)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

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

		if ( GetZoneEquipmentInputFlag ) {
			GetZoneEquipment();
			GetZoneEquipmentInputFlag = false;
			ZoneEquipInputsFilled = true;
		}

		InitZoneEquipment( FirstHVACIteration );

		if ( ZoneSizingCalc ) {
			SizeZoneEquipment();
		} else {
			SimZoneEquipment( FirstHVACIteration, SimAir );
			ZoneEquipSimulatedOnce = true;
		}

		UpdateZoneEquipment( SimAir );

		ReportZoneEquipment();

		SimZone = false;

	}

	void
	GetZoneEquipment()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1997
		//       MODIFIED       Aug 2003, FCW: set ZoneEquipConfig number for each zone
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get all the system related equipment which may be attached to
		// a zone

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Counter;
		int MaxNumOfEquipTypes;

		if ( ! ZoneEquipInputsFilled ) {
			GetZoneEquipmentData();
		}

		NumOfTimeStepInDay = NumOfTimeStepInHour * 24;

		MaxNumOfEquipTypes = 0;
		for ( Counter = 1; Counter <= NumOfZones; ++Counter ) {
			if ( ! ZoneEquipConfig( Counter ).IsControlled ) continue;
			MaxNumOfEquipTypes = max( MaxNumOfEquipTypes, ZoneEquipList( Counter ).NumOfEquipTypes );
		}

		PrioritySimOrder.allocate( MaxNumOfEquipTypes );
		DefaultSimOrder.allocate( MaxNumOfEquipTypes );
		for ( Counter = 1; Counter <= MaxNumOfEquipTypes; ++Counter ) {
			DefaultSimOrder( Counter ) = Counter;
		}

	}

	void
	InitZoneEquipment( bool const FirstHVACIteration ) // unused 1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the zone equipment prior to simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::NoAction;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::NumOfSizingTypes;
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataLoopNode::Node;
		using DataAirLoop::AirLoopFlow;
		using DataContaminantBalance::Contaminant;
		using DataContaminantBalance::OutdoorCO2;
		using DataContaminantBalance::OutdoorGC;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNodeNum;
		int InNodeNum;
		int ExhNodeNum;
		int ZoneInNode;
		int ZoneExhNode;
		int ControlledZoneNum;
		int ZoneReturnAirNode;
		/////////// hoisted into namespace ////////////
		// static bool MyOneTimeFlag( true ); // InitZoneEquipmentOneTimeFlag
		// static bool MyEnvrnFlag( true ); // InitZoneEquipmentEnvrnFlag
		///////////////////////////
		int ZoneEquipType; // Type of zone equipment
		int TotalNumComp; // Total number of zone components of ZoneEquipType
		int ZoneCompNum; // Number/index of zone equipment component
		int ZoneEquipCount;
		// Flow

		if ( InitZoneEquipmentOneTimeFlag ) {
			InitZoneEquipmentOneTimeFlag = false;
			TermUnitSizing.allocate( NumOfZones );
			ZoneEqSizing.allocate( NumOfZones );
			// setup zone equipment sequenced demand storage
			for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
				if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
				if ( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex == 0 ) continue;
				ZoneEquipCount = ZoneEquipList( ZoneEquipConfig( ControlledZoneNum ).EquipListIndex ).NumOfEquipTypes;
				ZoneSysEnergyDemand( ControlledZoneNum ).NumZoneEquipment = ZoneEquipCount;
				ZoneSysEnergyDemand( ControlledZoneNum ).SequencedOutputRequired.allocate( ZoneEquipCount );
				ZoneSysEnergyDemand( ControlledZoneNum ).SequencedOutputRequiredToHeatingSP.allocate( ZoneEquipCount );
				ZoneSysEnergyDemand( ControlledZoneNum ).SequencedOutputRequiredToCoolingSP.allocate( ZoneEquipCount );
				ZoneSysMoistureDemand( ControlledZoneNum ).NumZoneEquipment = ZoneEquipCount;
				ZoneSysMoistureDemand( ControlledZoneNum ).SequencedOutputRequired.allocate( ZoneEquipCount );
				ZoneSysMoistureDemand( ControlledZoneNum ).SequencedOutputRequiredToHumidSP.allocate( ZoneEquipCount );
				ZoneSysMoistureDemand( ControlledZoneNum ).SequencedOutputRequiredToDehumidSP.allocate( ZoneEquipCount );
				ZoneEqSizing( ControlledZoneNum ).SizingMethod.allocate( NumOfSizingTypes );
				ZoneEqSizing( ControlledZoneNum ).SizingMethod = 0;
			}
		}

		// Do the Begin Environment initializations
		if ( InitZoneEquipmentEnvrnFlag && BeginEnvrnFlag ) {

			ZoneEquipAvail = NoAction;

			if ( allocated( ZoneComp ) ) {
				for ( ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents; ++ZoneEquipType ) {
					if ( allocated( ZoneComp( ZoneEquipType ).ZoneCompAvailMgrs ) ) {
						TotalNumComp = ZoneComp( ZoneEquipType ).TotalNumComp;
						for ( ZoneCompNum = 1; ZoneCompNum <= TotalNumComp; ++ZoneCompNum ) {
							ZoneComp( ZoneEquipType ).ZoneCompAvailMgrs( ZoneCompNum ).AvailStatus = NoAction;
							ZoneComp( ZoneEquipType ).ZoneCompAvailMgrs( ZoneCompNum ).StartTime = 0;
							ZoneComp( ZoneEquipType ).ZoneCompAvailMgrs( ZoneCompNum ).StopTime = 0;
						}
					}
				}
			}
			for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
				if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;

				ZoneNodeNum = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
				Node( ZoneNodeNum ).Temp = 20.0;
				Node( ZoneNodeNum ).MassFlowRate = 0.0;
				Node( ZoneNodeNum ).Quality = 1.0;
				Node( ZoneNodeNum ).Press = OutBaroPress;
				Node( ZoneNodeNum ).HumRat = OutHumRat;
				Node( ZoneNodeNum ).Enthalpy = PsyHFnTdbW( Node( ZoneNodeNum ).Temp, Node( ZoneNodeNum ).HumRat );
				if ( Contaminant.CO2Simulation ) {
					Node( ZoneNodeNum ).CO2 = OutdoorCO2;
				}
				if ( Contaminant.GenericContamSimulation ) {
					Node( ZoneNodeNum ).GenContam = OutdoorGC;
				}

				for ( ZoneInNode = 1; ZoneInNode <= ZoneEquipConfig( ControlledZoneNum ).NumInletNodes; ++ZoneInNode ) {

					InNodeNum = ZoneEquipConfig( ControlledZoneNum ).InletNode( ZoneInNode );
					Node( InNodeNum ).Temp = 20.0;
					Node( InNodeNum ).MassFlowRate = 0.0;
					Node( InNodeNum ).Quality = 1.0;
					Node( InNodeNum ).Press = OutBaroPress;
					Node( InNodeNum ).HumRat = OutHumRat;
					Node( InNodeNum ).Enthalpy = PsyHFnTdbW( Node( InNodeNum ).Temp, Node( InNodeNum ).HumRat );
					if ( Contaminant.CO2Simulation ) {
						Node( InNodeNum ).CO2 = OutdoorCO2;
					}
					if ( Contaminant.GenericContamSimulation ) {
						Node( InNodeNum ).GenContam = OutdoorGC;
					}

				}

				for ( ZoneExhNode = 1; ZoneExhNode <= ZoneEquipConfig( ControlledZoneNum ).NumExhaustNodes; ++ZoneExhNode ) {

					ExhNodeNum = ZoneEquipConfig( ControlledZoneNum ).ExhaustNode( ZoneExhNode );
					Node( ExhNodeNum ).Temp = 20.0;
					Node( ExhNodeNum ).MassFlowRate = 0.0;
					Node( ExhNodeNum ).Quality = 1.0;
					Node( ExhNodeNum ).Press = OutBaroPress;
					Node( ExhNodeNum ).HumRat = OutHumRat;
					Node( ExhNodeNum ).Enthalpy = PsyHFnTdbW( Node( ExhNodeNum ).Temp, Node( ExhNodeNum ).HumRat );
					if ( Contaminant.CO2Simulation ) {
						Node( ExhNodeNum ).CO2 = OutdoorCO2;
					}
					if ( Contaminant.GenericContamSimulation ) {
						Node( ExhNodeNum ).GenContam = OutdoorGC;
					}

				}

				// BG CR 7122 following resets return air node.
				ZoneReturnAirNode = ZoneEquipConfig( ControlledZoneNum ).ReturnAirNode;
				if ( ZoneReturnAirNode > 0 ) {
					Node( ZoneReturnAirNode ).Temp = 20.0;
					Node( ZoneReturnAirNode ).MassFlowRate = 0.0;
					Node( ZoneReturnAirNode ).Quality = 1.0;
					Node( ZoneReturnAirNode ).Press = OutBaroPress;
					Node( ZoneReturnAirNode ).HumRat = OutHumRat;
					Node( ZoneReturnAirNode ).Enthalpy = PsyHFnTdbW( Node( ZoneReturnAirNode ).Temp, Node( ZoneReturnAirNode ).HumRat );
					if ( Contaminant.CO2Simulation ) {
						Node( ZoneReturnAirNode ).CO2 = OutdoorCO2;
					}
					if ( Contaminant.GenericContamSimulation ) {
						Node( ZoneReturnAirNode ).GenContam = OutdoorGC;
					}
				}

			}

			InitZoneEquipmentEnvrnFlag = false;

		}

		if ( ! BeginEnvrnFlag ) {
			InitZoneEquipmentEnvrnFlag = true;
		}

		// do the  HVAC time step initializations

		for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
			if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
			ZoneNodeNum = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;

			if ( FirstHVACIteration ) {
				for ( ZoneExhNode = 1; ZoneExhNode <= ZoneEquipConfig( ControlledZoneNum ).NumExhaustNodes; ++ZoneExhNode ) {
					ExhNodeNum = ZoneEquipConfig( ControlledZoneNum ).ExhaustNode( ZoneExhNode );
					Node( ExhNodeNum ).Temp = Node( ZoneNodeNum ).Temp;
					Node( ExhNodeNum ).HumRat = Node( ZoneNodeNum ).HumRat;
					Node( ExhNodeNum ).Enthalpy = Node( ZoneNodeNum ).Enthalpy;
					Node( ExhNodeNum ).Press = Node( ZoneNodeNum ).Press;
					Node( ExhNodeNum ).Quality = Node( ZoneNodeNum ).Quality;
					Node( ExhNodeNum ).MassFlowRate = 0.0;
					Node( ExhNodeNum ).MassFlowRateMaxAvail = 0.0;
					Node( ExhNodeNum ).MassFlowRateMinAvail = 0.0;
					if ( Contaminant.CO2Simulation ) {
						Node( ExhNodeNum ).CO2 = Node( ZoneNodeNum ).CO2;
					}
					if ( Contaminant.GenericContamSimulation ) {
						Node( ExhNodeNum ).GenContam = Node( ZoneNodeNum ).GenContam;
					}
				}
			}

			if ( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum > 0 ) {
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).ZoneExhaust = 0.0;
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).ZoneExhaustBalanced = 0.0;
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).SupFlow = 0.0;
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).RetFlow = 0.0;
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).RetFlow0 = 0.0;
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).RecircFlow = 0.0;
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).ZoneMixingFlow = 0.0;
				AirLoopFlow( ZoneEquipConfig( ControlledZoneNum ).AirLoopNum ).RetFlowAdjustment = 0.0;

			}

		}

	}

	void
	SizeZoneEquipment()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Performs the zone sizing calculations and fills the zone sizing
		// data arrays with the results of the calculation.

		// METHODOLOGY EMPLOYED:
		// Using the input from Zone Sizing objects and the Zone Equipment input,
		// for each controlled zone this subroutine performs a "purchased air" calculation
		// and saves the results in the zone sizing data arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::NonAirSystemResponse;
		using DataHeatBalFanSys::SysDepZoneLoads;
		using DataHeatBalFanSys::TempZoneThermostatSetPoint;
		using DataHeatBalFanSys::ZoneThermostatSetPointLo;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::DeadBandOrSetback;
		using DataLoopNode::Node;
		using DataHVACGlobals::SmallLoad;
		using DataHVACGlobals::SmallTempDiff;
		using General::RoundSigDigits;
		using DataEnvironment::StdBaroPress;

		// Parameters
		static std::string const RoutineName( "SizeZoneEquipment" );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ControlledZoneNum; // controlled zone index
		int ActualZoneNum; // index into Zone array (all zones)
		int SupplyAirNode1; // node number of 1st zone supply air node
		int SupplyAirNode2; // node number of 2nd zone supply air node
		int SupplyAirNode; // node number of supply air node for ideal air system
		int ZoneNode; // node number of controlled zone
		int ReturnNode; // node number of controlled zone return node
		Real64 DeltaTemp; // difference between supply air temp and zone temp [C]
		Real64 CpAir; // heat capacity of air [J/kg-C]
		Real64 SysOutputProvided; // system sensible output [W]
		Real64 LatOutputProvided; // system latent output [kg/s]
		Real64 Temp; // inlet temperature [C]
		Real64 HumRat; // inlet humidity ratio [kg water/kg dry air]
		Real64 Enthalpy; // inlet specific enthalpy [J/kg]
		Real64 MassFlowRate; // inlet mass flow rate [kg/s]
		Real64 RetTemp; // zone return temperature [C]
		Real64 DOASMassFlowRate( 0.0 ); // DOAS air mass flow rate for sizing [kg/s]
		Real64 DOASSupplyTemp( 0.0 ); // DOAS supply air temperature [C]
		Real64 DOASSupplyHumRat( 0.0 ); // DOAS supply air humidity ratio [kg H2O / kg dry air]
		Real64 DOASCpAir( 0.0 ); // heat capacity of DOAS air [J/kg-C]
		Real64 DOASSysOutputProvided( 0.0 ); // heating / cooling provided by DOAS system [W]
		Real64 TotDOASSysOutputProvided( 0.0 ); // total DOAS load on the zone [W]
		Real64 HR90H; // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
		Real64 HR90L; // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]

		if ( SizeZoneEquipmentOneTimeFlag ) {
			SetUpZoneSizingArrays();
			SizeZoneEquipmentOneTimeFlag = false;
		}

		for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
			if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;

			ActualZoneNum = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).ActualZoneNum;
			NonAirSystemResponse( ActualZoneNum ) = 0.0;
			SysDepZoneLoads( ActualZoneNum ) = 0.0;
			InitSystemOutputRequired( ActualZoneNum, SysOutputProvided, LatOutputProvided );
			ZoneNode = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
			SupplyAirNode = 0;
			SupplyAirNode1 = 0;
			SupplyAirNode2 = 0;
			// calculate DOAS heating/cooling effect
			if ( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).AccountForDOAS ) {
				// check for adequate number of supply nodes
				if ( ZoneEquipConfig( ControlledZoneNum ).NumInletNodes >= 2 ) {
					SupplyAirNode1 = ZoneEquipConfig( ControlledZoneNum ).InletNode( 1 );
					SupplyAirNode2 = ZoneEquipConfig( ControlledZoneNum ).InletNode( 2 );
				}
				else if ( ZoneEquipConfig( ControlledZoneNum ).NumInletNodes >= 1 ) {
					SupplyAirNode1 = ZoneEquipConfig( ControlledZoneNum ).InletNode( 1 );
					SupplyAirNode2 = 0;
				}
				else {
					ShowSevereError( RoutineName + ": to account for the effect a Dedicated Outside Air System on zone equipment sizing" );
					ShowContinueError( "there must be at least one zone air inlet node" );
					ShowFatalError( "Previous severe error causes abort " );
				}
				// set the DOAS mass flow rate and supply temperature and humidity ratio
				HR90H = PsyWFnTdbRhPb( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASHighSetpoint, 0.9, StdBaroPress );
				HR90L = PsyWFnTdbRhPb( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASLowSetpoint, 0.9, StdBaroPress );
				DOASMassFlowRate = CalcFinalZoneSizing( ControlledZoneNum ).MinOA;
				CalcDOASSupCondsForSizing( OutDryBulbTemp, OutHumRat, CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASControlStrategy,
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASLowSetpoint, CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASHighSetpoint,
					HR90H, HR90L, DOASSupplyTemp, DOASSupplyHumRat );
				DOASCpAir = PsyCpAirFnWTdb( DOASSupplyHumRat, DOASSupplyTemp );
				DOASSysOutputProvided = DOASMassFlowRate * DOASCpAir * ( DOASSupplyTemp - Node( ZoneNode ).Temp );
				TotDOASSysOutputProvided = DOASMassFlowRate *( PsyHFnTdbW( DOASSupplyTemp, DOASSupplyHumRat ) - PsyHFnTdbW( Node( ZoneNode ).Temp, Node( ZoneNode ).HumRat ) );
				UpdateSystemOutputRequired( ActualZoneNum, DOASSysOutputProvided, LatOutputProvided );
				Node( SupplyAirNode1 ).Temp = DOASSupplyTemp;
				Node( SupplyAirNode1 ).HumRat = DOASSupplyHumRat;
				Node( SupplyAirNode1 ).MassFlowRate = DOASMassFlowRate;
				Node( SupplyAirNode1 ).Enthalpy = PsyHFnTdbW( DOASSupplyTemp, DOASSupplyHumRat );
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASHeatAdd = DOASSysOutputProvided;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASLatAdd = TotDOASSysOutputProvided - DOASSysOutputProvided;
				SupplyAirNode = SupplyAirNode2;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASSupMassFlow = DOASMassFlowRate;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASSupTemp = DOASSupplyTemp;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASSupHumRat = DOASSupplyHumRat;
				if ( DOASSysOutputProvided > 0.0 ) {
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASHeatLoad = DOASSysOutputProvided;
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASCoolLoad = 0.0;
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASTotCoolLoad = 0.0;
				} else {
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASCoolLoad = DOASSysOutputProvided;
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASTotCoolLoad = TotDOASSysOutputProvided;
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).DOASHeatLoad = 0.0;
				}

			}
			else {
				if ( ZoneEquipConfig( ControlledZoneNum ).NumInletNodes > 0 ) {
					SupplyAirNode = ZoneEquipConfig( ControlledZoneNum ).InletNode( 1 );
				} else {
					SupplyAirNode = 0;
				}
			}

			// Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
			//                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
			if ( ! DeadBandOrSetback( ActualZoneNum ) && std::abs( ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired ) > SmallLoad ) {
				// Determine design supply air temperture and design supply air temperature difference
				if ( ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired < 0.0 ) { // Cooling case
					// If the user specify the design cooling supply air temperature, then
					if ( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).ZnCoolDgnSAMethod == SupplyAirTemperature ) {
						Temp = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolDesTemp;
						HumRat = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolDesHumRat;
						DeltaTemp = Temp - Node( ZoneNode ).Temp;
						// If the user specify the design cooling supply air temperature difference, then
					} else {
						DeltaTemp = -std::abs( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolDesTempDiff );
						Temp = DeltaTemp + Node( ZoneNode ).Temp;
						HumRat = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolDesHumRat;
					}
				} else { // Heating Case
					// If the user specify the design heating supply air temperature, then
					if ( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).ZnHeatDgnSAMethod == SupplyAirTemperature ) {
						Temp = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatDesTemp;
						HumRat = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatDesHumRat;
						DeltaTemp = Temp - Node( ZoneNode ).Temp;
						// If the user specify the design heating supply air temperature difference, then
					} else {
						DeltaTemp = std::abs( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatDesTempDiff );
						Temp = DeltaTemp + Node( ZoneNode ).Temp;
						HumRat = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatDesHumRat;
					}
				}

				Enthalpy = PsyHFnTdbW( Temp, HumRat );
				SysOutputProvided = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired;
				CpAir = PsyCpAirFnWTdb( HumRat, Temp );
				if ( std::abs( DeltaTemp ) > SmallTempDiff ) {
					//!!PH/WFB/LKL (UCDV model)        MassFlowRate = SysOutputProvided / (CpAir*DeltaTemp)
					MassFlowRate = max( SysOutputProvided / ( CpAir * DeltaTemp ), 0.0 );
				} else {
					MassFlowRate = 0.0;
				}

				if ( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).SupplyAirAdjustFactor > 1.0 ) {
					MassFlowRate *= CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).SupplyAirAdjustFactor;
				}
			} else {

				Temp = Node( ZoneNode ).Temp;
				HumRat = Node( ZoneNode ).HumRat;
				Enthalpy = Node( ZoneNode ).Enthalpy;
				MassFlowRate = 0.0;

			}

			UpdateSystemOutputRequired( ActualZoneNum, SysOutputProvided, LatOutputProvided );

			if ( SysOutputProvided > 0.0 ) {
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatLoad = SysOutputProvided;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatMassFlow = MassFlowRate;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatZoneTemp = Node( ZoneNode ).Temp;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatZoneHumRat = Node( ZoneNode ).HumRat;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatOutTemp = OutDryBulbTemp;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatOutHumRat = OutHumRat;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolLoad = 0.0;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolMassFlow = 0.0;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolZoneTemp = 0.0;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolZoneHumRat = 0.0;
			} else {
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolLoad = -SysOutputProvided;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolMassFlow = MassFlowRate;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolZoneTemp = Node( ZoneNode ).Temp;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolZoneHumRat = Node( ZoneNode ).HumRat;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolOutTemp = OutDryBulbTemp;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolOutHumRat = OutHumRat;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatLoad = 0.0;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatMassFlow = 0.0;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatZoneTemp = 0.0;
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatZoneHumRat = 0.0;
			}

			if ( SupplyAirNode > 0 ) {
				Node( SupplyAirNode ).Temp = Temp;
				Node( SupplyAirNode ).HumRat = HumRat;
				Node( SupplyAirNode ).Enthalpy = Enthalpy;
				Node( SupplyAirNode ).MassFlowRate = MassFlowRate;
			} else {
				NonAirSystemResponse( ActualZoneNum ) = SysOutputProvided;
			}

		}

		CalcZoneMassBalance();

		CalcZoneLeavingConditions();

		for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
			if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
			ReturnNode = ZoneEquipConfig( ControlledZoneNum ).ReturnAirNode;
			ZoneNode = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
			ActualZoneNum = CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).ActualZoneNum;
			if ( ReturnNode > 0 ) {
				RetTemp = Node( ReturnNode ).Temp;
			} else {
				RetTemp = Node( ZoneNode ).Temp;
			}
			if ( CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatLoad > 0.0 ) {
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatZoneRetTemp = RetTemp;
				if ( TempZoneThermostatSetPoint( ActualZoneNum ) > 0.0 ) {
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatTstatTemp = TempZoneThermostatSetPoint( ActualZoneNum );
				} else {
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).HeatTstatTemp = ZoneThermostatSetPointLo( ActualZoneNum );
				}
			} else {
				CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolZoneRetTemp = RetTemp;
				if ( TempZoneThermostatSetPoint( ActualZoneNum ) > 0.0 ) {
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolTstatTemp = TempZoneThermostatSetPoint( ActualZoneNum );
				} else {
					CalcZoneSizing( CurOverallSimDay, ControlledZoneNum ).CoolTstatTemp = ZoneThermostatSetPointHi( ActualZoneNum );
				}
			}
		}

	}

	void
	CalcDOASSupCondsForSizing(
		Real64 OutDB, // outside air temperature [C]
		Real64 OutHR, // outside humidity ratio [kg Water / kg Dry Air]
		int DOASControl, // dedicated outside air control strategy
		Real64 DOASLowTemp, // DOAS low setpoint [C]
		Real64 DOASHighTemp, // DOAS high setpoint [C]
		Real64 W90H, // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
		Real64 W90L, // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
		Real64 & DOASSupTemp, // DOAS supply temperature [C]
		Real64 & DOASSupHR // DOAS Supply Humidity ratio [kg Water / kg Dry Air]
		)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2015
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates supply conditions for the direct outside air system
		// (DOAS) sizing calculations

		// METHODOLOGY EMPLOYED:
		// the supply temperature and humidity ratio are set depending on the design control method
		// and the outside air temperature

		// REFERENCES:
		// Consult the "DOAS Effect On Zone Sizing" new feature proposal and design documents

		// Using/Aliasing

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcDOASSupCondsForSizing" );

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		DOASSupTemp = 0.0;
		DOASSupHR = 0.0;
		// neutral supply air
		if ( DOASControl == 1 ) {
			if ( OutDB < DOASLowTemp ) {
				DOASSupTemp = DOASLowTemp;
				DOASSupHR = OutHR;
			} else if (OutDB > DOASHighTemp) {
				DOASSupTemp = DOASHighTemp;
				DOASSupHR = min( OutHR, W90H );
			}
			else {
				DOASSupTemp = OutDB;
				DOASSupHR = OutHR;
			}
		}

		// neutral dehumidified supply air
		else if ( DOASControl == 2 ) { //
			if ( OutDB < DOASLowTemp ) {
				DOASSupTemp = DOASHighTemp;
				DOASSupHR = OutHR;
			}
			else {
				DOASSupTemp = DOASHighTemp;
				DOASSupHR = min( OutHR, W90L );
			}
		}

		// cold supply air
		else if ( DOASControl == 3 ) {
			if ( OutDB < DOASLowTemp ) {
				DOASSupTemp = DOASHighTemp;
				DOASSupHR = OutHR;
			}
			else {
				DOASSupTemp = DOASLowTemp;
				DOASSupHR = min( OutHR, W90L );
			}
		}
		else {
			ShowFatalError( RoutineName + ":illegal DOAS design control strategy" );
		}
	}

	void
	SetUpZoneSizingArrays()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Allocate and fill the ZoneSizing data array.

		// METHODOLOGY EMPLOYED:
		// Obtains data from Zone Sizing and Zone Equipment objects already input.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::OutputFileInits;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::isPulseZoneSizing;
		using InputProcessor::FindItemInList;
		using DataHeatBalance::People;
		using DataHeatBalance::TotPeople;
		using DataHeatBalance::Zone;
		using ZoneTempPredictorCorrector::VerifyThermostatInZone;
		using EMSManager::ManageEMS;
		using ScheduleManager::GetScheduleMaxValue;
		using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

		// Locals
		int NumOfTimeStepInDay; // number of zone time steps in a day

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DesDayNum; // design day index
		//unused  INTEGER :: DesDayEnvrnNum   ! design day index
		int CtrlZoneNum; // controlled zone index
		int ZoneSizNum; // zone sizing input index
		int TimeStepIndex; // zone time step index
		Real64 TotPeopleInZone; // total (maximum) number of people in a zone
		int PeopleNum; // index of People structure
		static Real64 OAFromPeople( 0.0 ); // min OA calculated from zone occupancy [m3/s]
		static Real64 OAFromArea( 0.0 ); // min OA calculated from zone area and OA flow per area [m3/s]
		int ZoneIndex; // index of Zone Sizing zone name in zone array
		int ZoneSizIndex; // zone sizing do loop index
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static Real64 SchMax( 0.0 ); // maximum people multiplier value
		Real64 OAVolumeFlowRate; // outside air flow rate (m3/s)
		bool UseOccSchFlag; // flag to use occupancy schedule when calculating OA
		bool UseMinOASchFlag; // flag to use min OA schedule when calculating OA
		int DSOAPtr; // index to DesignSpecification:OutdoorAir object

		// Formats
		static gio::Fmt Format_890( "('! <Load Timesteps in Zone Design Calculation Averaging Window>, Value')" );
		static gio::Fmt Format_891( "(' Load Timesteps in Zone Design Calculation Averaging Window, ',I4)" );
		static gio::Fmt Format_990( "('! <Heating Sizing Factor Information>, Sizing Factor ID, Value')" );
		static gio::Fmt Format_991( "(' Heating Sizing Factor Information, Global, ',G12.5)" );
		static gio::Fmt Format_992( "(' Heating Sizing Factor Information, Zone ',A,', ',G12.5)" );
		static gio::Fmt Format_993( "('! <Cooling Sizing Factor Information>, Sizing Factor ID, Value')" );
		static gio::Fmt Format_994( "(' Cooling Sizing Factor Information, Global, ',G12.5)" );
		static gio::Fmt Format_995( "(' Cooling Sizing Factor Information, Zone ',A,', ',G12.5)" );

		for ( ZoneSizIndex = 1; ZoneSizIndex <= NumZoneSizingInput; ++ZoneSizIndex ) {
			ZoneIndex = FindItemInList( ZoneSizingInput( ZoneSizIndex ).ZoneName, Zone );
			if ( ZoneIndex == 0 ) {
				ShowSevereError( "SetUpZoneSizingArrays: Sizing:Zone=\"" + ZoneSizingInput( ZoneSizIndex ).ZoneName + "\" references unknown zone" );
				ErrorsFound = true;
			}
			if ( std::any_of( ZoneEquipConfig.begin(), ZoneEquipConfig.end(), []( EquipConfiguration const & e ){ return e.IsControlled; } ) ) {
				ZoneIndex = FindItemInList( ZoneSizingInput( ZoneSizIndex ).ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName );
				if ( ZoneIndex == 0 ) {
					if ( ! isPulseZoneSizing ) {
						ShowWarningError( "SetUpZoneSizingArrays: Requested Sizing for Zone=\"" + ZoneSizingInput( ZoneSizIndex ).ZoneName + "\", Zone is not found in the Controlled Zones List" );
					}
				} else {
					ZoneSizingInput( ZoneSizIndex ).ZoneNum = ZoneIndex;
				}
				if ( ZoneSizingInput( ZoneSizIndex ).CoolAirDesMethod == FromDDCalc || ZoneSizingInput( ZoneSizIndex ).HeatAirDesMethod == FromDDCalc ) {
					if ( ! VerifyThermostatInZone( ZoneSizingInput( ZoneSizIndex ).ZoneName ) ) {
						if ( ! isPulseZoneSizing ) {
							ShowWarningError( "SetUpZoneSizingArrays: Requested Sizing for Zone=\"" + ZoneSizingInput( ZoneSizIndex ).ZoneName + "\", Zone has no thermostat (ref: ZoneControl:Thermostat, et al)" );
						}
					}
				}
			} else {
				ShowSevereError( "SetUpZoneSizingArrays: Zone Sizing is requested but there are no ZoneHVAC:EquipmentConnections statements." );
				ErrorsFound = true;
			}
		}
		if ( ErrorsFound ) {
			ShowFatalError( "SetUpZoneSizingArrays: Errors found in Sizing:Zone input" );
		}

		ZoneSizing.allocate( TotDesDays + TotRunDesPersDays, NumOfZones );
		FinalZoneSizing.allocate( NumOfZones );
		CalcZoneSizing.allocate( TotDesDays + TotRunDesPersDays, NumOfZones );
		CalcFinalZoneSizing.allocate( NumOfZones );
		TermUnitFinalZoneSizing.allocate( NumOfZones );
		DesDayWeath.allocate( TotDesDays + TotRunDesPersDays );
		NumOfTimeStepInDay = NumOfTimeStepInHour * 24;
		AvgData.allocate( NumOfTimeStepInDay );
		CoolPeakDateHrMin.allocate( NumOfZones );
		HeatPeakDateHrMin.allocate( NumOfZones );
		ZoneSizThermSetPtHi.allocate( NumOfZones );
		ZoneSizThermSetPtLo.allocate( NumOfZones );

		CoolPeakDateHrMin = "";
		HeatPeakDateHrMin = "";

		ZoneSizThermSetPtHi = 0.0;
		ZoneSizThermSetPtLo = 1000.0;

		for ( DesDayNum = 1; DesDayNum <= TotDesDays + TotRunDesPersDays; ++DesDayNum ) {
			DesDayWeath( DesDayNum ).Temp.allocate( NumOfTimeStepInHour * 24 );
			DesDayWeath( DesDayNum ).HumRat.allocate( NumOfTimeStepInHour * 24 );
			DesDayWeath( DesDayNum ).Press.allocate( NumOfTimeStepInHour * 24 );
			DesDayWeath( DesDayNum ).Temp = 0.0;
			DesDayWeath( DesDayNum ).HumRat = 0.0;
			DesDayWeath( DesDayNum ).Press = 0.0;
		}
		// Fill zone sizing arrays from input array
		for ( DesDayNum = 1; DesDayNum <= TotDesDays + TotRunDesPersDays; ++DesDayNum ) {
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				ZoneSizing( DesDayNum, CtrlZoneNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
				ZoneSizing( DesDayNum, CtrlZoneNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
				// For each Zone Sizing object, find the corresponding controlled zone
				ZoneSizNum = FindItemInList( ZoneEquipConfig( CtrlZoneNum ).ZoneName, ZoneSizingInput, &ZoneSizingInputData::ZoneName );
				if ( ZoneSizNum > 0 ) { // move data from zone sizing input
					ZoneSizing( DesDayNum, CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnCoolDgnSAMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnHeatDgnSAMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( ZoneSizNum ).CoolDesTemp;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( ZoneSizNum ).HeatDesTemp;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( ZoneSizNum ).CoolDesTempDiff;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( ZoneSizNum ).HeatDesTempDiff;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( ZoneSizNum ).CoolDesHumRat;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( ZoneSizNum ).HeatDesHumRat;
					ZoneSizing( DesDayNum, CtrlZoneNum ).OADesMethod = ZoneSizingInput( ZoneSizNum ).OADesMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( ZoneSizNum ).DesOAFlowPPer;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesOAFlowPerArea;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlow = ZoneSizingInput( ZoneSizNum ).DesOAFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( ZoneSizNum ).CoolAirDesMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( ZoneSizNum ).HeatAirDesMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowPerArea;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowFrac;
					ZoneSizing( DesDayNum, CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowPerArea;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowFrac;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( ZoneSizNum ).HeatSizingFactor;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( ZoneSizNum ).CoolSizingFactor;
					ZoneSizing( DesDayNum, CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( ZoneSizNum ).AccountForDOAS;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( ZoneSizNum ).DOASControlStrategy;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( ZoneSizNum ).DOASLowSetpoint;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( ZoneSizNum ).DOASHighSetpoint;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnCoolDgnSAMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnHeatDgnSAMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( ZoneSizNum ).CoolDesTemp;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( ZoneSizNum ).HeatDesTemp;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( ZoneSizNum ).CoolDesTempDiff;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( ZoneSizNum ).HeatDesTempDiff;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( ZoneSizNum ).CoolDesHumRat;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( ZoneSizNum ).HeatDesHumRat;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).OADesMethod = ZoneSizingInput( ZoneSizNum ).OADesMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( ZoneSizNum ).DesOAFlowPPer;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesOAFlowPerArea;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlow = ZoneSizingInput( ZoneSizNum ).DesOAFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( ZoneSizNum ).CoolAirDesMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( ZoneSizNum ).HeatAirDesMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowPerArea;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowFrac;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowPerArea;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowFrac;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( ZoneSizNum ).HeatSizingFactor;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( ZoneSizNum ).CoolSizingFactor;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( ZoneSizNum ).AccountForDOAS;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( ZoneSizNum ).DOASControlStrategy;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( ZoneSizNum ).DOASLowSetpoint;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( ZoneSizNum ).DOASHighSetpoint;
				} else { // Every controlled zone must be simulated, so set missing inputs to the first
					//LKL I think this is sufficient for warning -- no need for array
					if ( DesDayNum == 1 ) {
						if ( ! isPulseZoneSizing ) {
							ShowWarningError( "SetUpZoneSizingArrays: Sizing for Zone=\"" + ZoneEquipConfig( CtrlZoneNum ).ZoneName + "\" will use Sizing:Zone specifications listed for Zone=\"" + ZoneSizingInput( 1 ).ZoneName + "\"." );
						}
						// Following needs to be implemented first:
						//          CALL ShowContinueError('  A better option would be to set up global ZoneList objects for Sizing:Zone objects.')
					}
					ZoneSizing( DesDayNum, CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( 1 ).ZnCoolDgnSAMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( 1 ).ZnHeatDgnSAMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( 1 ).CoolDesTemp;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( 1 ).HeatDesTemp;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( 1 ).CoolDesTempDiff;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( 1 ).HeatDesTempDiff;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( 1 ).CoolDesHumRat;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( 1 ).HeatDesHumRat;
					ZoneSizing( DesDayNum, CtrlZoneNum ).OADesMethod = ZoneSizingInput( 1 ).OADesMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( 1 ).DesOAFlowPPer;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( 1 ).DesOAFlowPerArea;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlow = ZoneSizingInput( 1 ).DesOAFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( 1 ).CoolAirDesMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( 1 ).HeatAirDesMethod;
					ZoneSizing( DesDayNum, CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( 1 ).DesCoolAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( 1 ).DesCoolMinAirFlowPerArea;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( 1 ).DesCoolMinAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( 1 ).DesCoolMinAirFlowFrac;
					ZoneSizing( DesDayNum, CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( 1 ).DesHeatAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( 1 ).DesHeatMaxAirFlowPerArea;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( 1 ).DesHeatMaxAirFlow;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( 1 ).DesHeatMaxAirFlowFrac;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( 1 ).HeatSizingFactor;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( 1 ).CoolSizingFactor;
					ZoneSizing( DesDayNum, CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( 1 ).AccountForDOAS;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( 1 ).DOASControlStrategy;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( 1 ).DOASLowSetpoint;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( 1 ).DOASHighSetpoint;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( 1 ).ZnCoolDgnSAMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( 1 ).ZnHeatDgnSAMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( 1 ).CoolDesTemp;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( 1 ).HeatDesTemp;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( 1 ).CoolDesTempDiff;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( 1 ).HeatDesTempDiff;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( 1 ).CoolDesHumRat;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( 1 ).HeatDesHumRat;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).OADesMethod = ZoneSizingInput( 1 ).OADesMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( 1 ).DesOAFlowPPer;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( 1 ).DesOAFlowPerArea;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesOAFlow = ZoneSizingInput( 1 ).DesOAFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( 1 ).CoolAirDesMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( 1 ).HeatAirDesMethod;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( 1 ).DesCoolAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( 1 ).DesCoolMinAirFlowPerArea;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( 1 ).DesCoolMinAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( 1 ).DesCoolMinAirFlowFrac;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( 1 ).DesHeatAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( 1 ).DesHeatMaxAirFlowPerArea;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( 1 ).DesHeatMaxAirFlow;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( 1 ).DesHeatMaxAirFlowFrac;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( 1 ).HeatSizingFactor;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( 1 ).CoolSizingFactor;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( 1 ).AccountForDOAS;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( 1 ).DOASControlStrategy;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( 1 ).DOASLowSetpoint;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( 1 ).DOASHighSetpoint;

				}
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatSetPtSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolSetPtSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatLoadSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASCoolLoadSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatAddSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASLatAddSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlowSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASTotCoolLoadSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatLoadSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASCoolLoadSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatAddSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASLatAddSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlowSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASTotCoolLoadSeq.allocate( NumOfTimeStepInDay );
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatSetPtSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolSetPtSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatLoadSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASCoolLoadSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatAddSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASLatAddSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlowSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( DesDayNum, CtrlZoneNum ).DOASTotCoolLoadSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatLoadSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASCoolLoadSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatAddSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASLatAddSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlowSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASTotCoolLoadSeq( TimeStepIndex ) = 0.0;
				}
			}
		}

		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			FinalZoneSizing( CtrlZoneNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
			FinalZoneSizing( CtrlZoneNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
			CalcFinalZoneSizing( CtrlZoneNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			ZoneSizNum = FindItemInList( ZoneEquipConfig( CtrlZoneNum ).ZoneName, ZoneSizingInput, &ZoneSizingInputData::ZoneName );
			if ( ZoneSizNum > 0 ) { // move data from zone sizing input
				FinalZoneSizing( CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnCoolDgnSAMethod;
				FinalZoneSizing( CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnHeatDgnSAMethod;
				FinalZoneSizing( CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( ZoneSizNum ).CoolDesTemp;
				FinalZoneSizing( CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( ZoneSizNum ).HeatDesTemp;
				FinalZoneSizing( CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( ZoneSizNum ).CoolDesTempDiff;
				FinalZoneSizing( CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( ZoneSizNum ).HeatDesTempDiff;
				FinalZoneSizing( CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( ZoneSizNum ).CoolDesHumRat;
				FinalZoneSizing( CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( ZoneSizNum ).HeatDesHumRat;
				FinalZoneSizing( CtrlZoneNum ).ZoneDesignSpecOAIndex = ZoneSizingInput( ZoneSizNum ).ZoneDesignSpecOAIndex;
				FinalZoneSizing( CtrlZoneNum ).OADesMethod = ZoneSizingInput( ZoneSizNum ).OADesMethod;
				FinalZoneSizing( CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( ZoneSizNum ).DesOAFlowPPer;
				FinalZoneSizing( CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesOAFlowPerArea;
				FinalZoneSizing( CtrlZoneNum ).DesOAFlow = ZoneSizingInput( ZoneSizNum ).DesOAFlow;
				FinalZoneSizing( CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( ZoneSizNum ).CoolAirDesMethod;
				FinalZoneSizing( CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( ZoneSizNum ).HeatAirDesMethod;
				FinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowPerArea;
				FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowFrac;
				FinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowPerArea;
				FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowFrac;
				FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( ZoneSizNum ).HeatSizingFactor;
				FinalZoneSizing( CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( ZoneSizNum ).CoolSizingFactor;
				FinalZoneSizing( CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( ZoneSizNum ).AccountForDOAS;
				FinalZoneSizing( CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( ZoneSizNum ).DOASControlStrategy;
				FinalZoneSizing( CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( ZoneSizNum ).DOASLowSetpoint;
				FinalZoneSizing( CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( ZoneSizNum ).DOASHighSetpoint;
				FinalZoneSizing( CtrlZoneNum ).ZoneADEffCooling = ZoneSizingInput( ZoneSizNum ).ZoneADEffCooling;
				FinalZoneSizing( CtrlZoneNum ).ZoneADEffHeating = ZoneSizingInput( ZoneSizNum ).ZoneADEffHeating;
				FinalZoneSizing( CtrlZoneNum ).ZoneSecondaryRecirculation = ZoneSizingInput( ZoneSizNum ).ZoneSecondaryRecirculation;
				CalcFinalZoneSizing( CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnCoolDgnSAMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnHeatDgnSAMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( ZoneSizNum ).CoolDesTemp;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( ZoneSizNum ).HeatDesTemp;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( ZoneSizNum ).CoolDesTempDiff;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( ZoneSizNum ).HeatDesTempDiff;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( ZoneSizNum ).CoolDesHumRat;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( ZoneSizNum ).HeatDesHumRat;
				CalcFinalZoneSizing( CtrlZoneNum ).ZoneDesignSpecOAIndex = ZoneSizingInput( ZoneSizNum ).ZoneDesignSpecOAIndex;
				CalcFinalZoneSizing( CtrlZoneNum ).OADesMethod = ZoneSizingInput( ZoneSizNum ).OADesMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( ZoneSizNum ).DesOAFlowPPer;
				CalcFinalZoneSizing( CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesOAFlowPerArea;
				CalcFinalZoneSizing( CtrlZoneNum ).DesOAFlow = ZoneSizingInput( ZoneSizNum ).DesOAFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( ZoneSizNum ).CoolAirDesMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( ZoneSizNum ).HeatAirDesMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowPerArea;
				CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowFrac;
				CalcFinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowPerArea;
				CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowFrac;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( ZoneSizNum ).HeatSizingFactor;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( ZoneSizNum ).CoolSizingFactor;
				CalcFinalZoneSizing( CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( ZoneSizNum ).AccountForDOAS;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( ZoneSizNum ).DOASControlStrategy;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( ZoneSizNum ).DOASLowSetpoint;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( ZoneSizNum ).DOASHighSetpoint;
				CalcFinalZoneSizing( CtrlZoneNum ).ZoneADEffCooling = ZoneSizingInput( ZoneSizNum ).ZoneADEffCooling;
				CalcFinalZoneSizing( CtrlZoneNum ).ZoneADEffHeating = ZoneSizingInput( ZoneSizNum ).ZoneADEffHeating;
			} else { // Every controlled zone must be simulated, so set missing inputs to the first
				FinalZoneSizing( CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( 1 ).ZnCoolDgnSAMethod;
				FinalZoneSizing( CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( 1 ).ZnHeatDgnSAMethod;
				FinalZoneSizing( CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( 1 ).CoolDesTemp;
				FinalZoneSizing( CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( 1 ).HeatDesTemp;
				FinalZoneSizing( CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( 1 ).CoolDesTempDiff;
				FinalZoneSizing( CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( 1 ).HeatDesTempDiff;
				FinalZoneSizing( CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( 1 ).CoolDesHumRat;
				FinalZoneSizing( CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( 1 ).HeatDesHumRat;
				FinalZoneSizing( CtrlZoneNum ).ZoneDesignSpecOAIndex = ZoneSizingInput( 1 ).ZoneDesignSpecOAIndex;
				FinalZoneSizing( CtrlZoneNum ).OADesMethod = ZoneSizingInput( 1 ).OADesMethod;
				FinalZoneSizing( CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( 1 ).DesOAFlowPPer;
				FinalZoneSizing( CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( 1 ).DesOAFlowPerArea;
				FinalZoneSizing( CtrlZoneNum ).DesOAFlow = ZoneSizingInput( 1 ).DesOAFlow;
				FinalZoneSizing( CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( 1 ).CoolAirDesMethod;
				FinalZoneSizing( CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( 1 ).HeatAirDesMethod;
				FinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( 1 ).DesCoolAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( 1 ).DesCoolMinAirFlowPerArea;
				FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( 1 ).DesCoolMinAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( 1 ).DesCoolMinAirFlowFrac;
				FinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( 1 ).DesHeatAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( 1 ).DesHeatMaxAirFlowPerArea;
				FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( 1 ).DesHeatMaxAirFlow;
				FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( 1 ).DesHeatMaxAirFlowFrac;
				FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( 1 ).HeatSizingFactor;
				FinalZoneSizing( CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( 1 ).CoolSizingFactor;
				FinalZoneSizing( CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( 1 ).AccountForDOAS;
				FinalZoneSizing( CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( 1 ).DOASControlStrategy;
				FinalZoneSizing( CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( 1 ).DOASLowSetpoint;
				FinalZoneSizing( CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( 1 ).DOASHighSetpoint;
				FinalZoneSizing( CtrlZoneNum ).ZoneADEffCooling = ZoneSizingInput( 1 ).ZoneADEffCooling;
				FinalZoneSizing( CtrlZoneNum ).ZoneADEffHeating = ZoneSizingInput( 1 ).ZoneADEffHeating;
				FinalZoneSizing( CtrlZoneNum ).ZoneSecondaryRecirculation = ZoneSizingInput( 1 ).ZoneSecondaryRecirculation;
				CalcFinalZoneSizing( CtrlZoneNum ).ZnCoolDgnSAMethod = ZoneSizingInput( 1 ).ZnCoolDgnSAMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).ZnHeatDgnSAMethod = ZoneSizingInput( 1 ).ZnHeatDgnSAMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolDesTemp = ZoneSizingInput( 1 ).CoolDesTemp;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatDesTemp = ZoneSizingInput( 1 ).HeatDesTemp;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolDesTempDiff = ZoneSizingInput( 1 ).CoolDesTempDiff;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatDesTempDiff = ZoneSizingInput( 1 ).HeatDesTempDiff;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolDesHumRat = ZoneSizingInput( 1 ).CoolDesHumRat;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatDesHumRat = ZoneSizingInput( 1 ).HeatDesHumRat;
				CalcFinalZoneSizing( CtrlZoneNum ).ZoneDesignSpecOAIndex = ZoneSizingInput( 1 ).ZoneDesignSpecOAIndex;
				CalcFinalZoneSizing( CtrlZoneNum ).OADesMethod = ZoneSizingInput( 1 ).OADesMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).DesOAFlowPPer = ZoneSizingInput( 1 ).DesOAFlowPPer;
				CalcFinalZoneSizing( CtrlZoneNum ).DesOAFlowPerArea = ZoneSizingInput( 1 ).DesOAFlowPerArea;
				CalcFinalZoneSizing( CtrlZoneNum ).DesOAFlow = ZoneSizingInput( 1 ).DesOAFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolAirDesMethod = ZoneSizingInput( 1 ).CoolAirDesMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatAirDesMethod = ZoneSizingInput( 1 ).HeatAirDesMethod;
				CalcFinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow = ZoneSizingInput( 1 ).DesCoolAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( 1 ).DesCoolMinAirFlowPerArea;
				CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow = ZoneSizingInput( 1 ).DesCoolMinAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( 1 ).DesCoolMinAirFlowFrac;
				CalcFinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow = ZoneSizingInput( 1 ).DesHeatAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( 1 ).DesHeatMaxAirFlowPerArea;
				CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow = ZoneSizingInput( 1 ).DesHeatMaxAirFlow;
				CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( 1 ).DesHeatMaxAirFlowFrac;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatSizingFactor = ZoneSizingInput( 1 ).HeatSizingFactor;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolSizingFactor = ZoneSizingInput( 1 ).CoolSizingFactor;
				CalcFinalZoneSizing( CtrlZoneNum ).AccountForDOAS = ZoneSizingInput( 1 ).AccountForDOAS;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASControlStrategy = ZoneSizingInput( 1 ).DOASControlStrategy;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASLowSetpoint = ZoneSizingInput( 1 ).DOASLowSetpoint;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASHighSetpoint = ZoneSizingInput( 1 ).DOASHighSetpoint;
				CalcFinalZoneSizing( CtrlZoneNum ).ZoneADEffCooling = ZoneSizingInput( 1 ).ZoneADEffCooling;
				CalcFinalZoneSizing( CtrlZoneNum ).ZoneADEffHeating = ZoneSizingInput( 1 ).ZoneADEffHeating;
			}
			FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).HeatLoadSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolLoadSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASHeatLoadSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASCoolLoadSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASHeatAddSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASLatAddSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASSupMassFlowSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASSupTempSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASSupHumRatSeq.allocate( NumOfTimeStepInDay );
			FinalZoneSizing( CtrlZoneNum ).DOASTotCoolLoadSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatLoadSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolLoadSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASHeatLoadSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASCoolLoadSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASHeatAddSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASLatAddSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASSupMassFlowSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASSupTempSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASSupHumRatSeq.allocate( NumOfTimeStepInDay );
			CalcFinalZoneSizing( CtrlZoneNum ).DOASTotCoolLoadSeq.allocate( NumOfTimeStepInDay );
			for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
				FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASHeatLoadSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASCoolLoadSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASHeatAddSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASLatAddSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASSupMassFlowSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASSupTempSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASSupHumRatSeq( TimeStepIndex ) = 0.0;
				FinalZoneSizing( CtrlZoneNum ).DOASTotCoolLoadSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASHeatLoadSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASCoolLoadSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASHeatAddSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASLatAddSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASSupMassFlowSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASSupTempSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASSupHumRatSeq( TimeStepIndex ) = 0.0;
				CalcFinalZoneSizing( CtrlZoneNum ).DOASTotCoolLoadSeq( TimeStepIndex ) = 0.0;
			}

			// setup CalcFinalZoneSizing structure for use with EMS, some as sensors, some as actuators
			if ( AnyEnergyManagementSystemInModel ) {

				//actuate  REAL(r64)             :: DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
				SetupEMSInternalVariable( "Final Zone Design Heating Air Mass Flow Rate", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/s]", FinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow );
				SetupEMSInternalVariable( "Intermediate Zone Design Heating Air Mass Flow Rate", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/s]", CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow );
				SetupEMSActuator( "Sizing:Zone", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "Zone Design Heating Air Mass Flow Rate", "[kg/s]", CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesHeatMassOn, CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesHeatMassFlow );

				//actuate  REAL(r64)             :: DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
				SetupEMSInternalVariable( "Final Zone Design Cooling Air Mass Flow Rate", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/s]", FinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow );
				SetupEMSInternalVariable( "Intermediate Zone Design Cooling Air Mass Flow Rate", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/s]", CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow );
				SetupEMSActuator( "Sizing:Zone", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "Zone Design Cooling Air Mass Flow Rate", "[kg/s]", CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesCoolMassOn, CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesCoolMassFlow );

				//actuate  REAL(r64)             :: DesHeatLoad              = 0.0d0   ! zone design heating load [W]
				SetupEMSInternalVariable( "Final Zone Design Heating Load", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[W]", FinalZoneSizing( CtrlZoneNum ).DesHeatLoad );
				SetupEMSInternalVariable( "Intermediate Zone Design Heating Load", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[W]", CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad );
				SetupEMSActuator( "Sizing:Zone", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "Zone Design Heating Load", "[W]", CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesHeatLoadOn, CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesHeatLoad );

				//actuate  REAL(r64)             :: DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
				SetupEMSInternalVariable( "Final Zone Design Cooling Load", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[W]", FinalZoneSizing( CtrlZoneNum ).DesCoolLoad );
				SetupEMSInternalVariable( "Intermediate Zone Design Cooling Load", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[W]", CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad );
				SetupEMSActuator( "Sizing:Zone", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "Zone Design Cooling Load", "[W]", CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesCoolLoadOn, CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesCoolLoad );

				//sensor?  REAL(r64)             :: DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
				SetupEMSInternalVariable( "Final Zone Design Heating Air Density", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/m3]", FinalZoneSizing( CtrlZoneNum ).DesHeatDens );
				SetupEMSInternalVariable( "Intermediate Zone Design Heating Air Density", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/m3]", CalcFinalZoneSizing( CtrlZoneNum ).DesHeatDens );
				//sensor?  REAL(r64)             :: DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]
				SetupEMSInternalVariable( "Final Zone Design Cooling Air Density", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/m3]", FinalZoneSizing( CtrlZoneNum ).DesCoolDens );
				SetupEMSInternalVariable( "Intermediate Zone Design Cooling Air Density", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[kg/m3]", CalcFinalZoneSizing( CtrlZoneNum ).DesCoolDens );

				//actuate  REAL(r64)             :: DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
				SetupEMSInternalVariable( "Final Zone Design Heating Volume Flow", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[m3/s]", FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow );
				SetupEMSInternalVariable( "Intermediate Zone Design Heating Volume Flow", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[m3/s]", CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow );
				SetupEMSActuator( "Sizing:Zone", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "Zone Design Heating Vol Flow", "[m3/s]", CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesHeatVolOn, CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesHeatVolFlow );

				//actuate  REAL(r64)             :: DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
				SetupEMSInternalVariable( "Final Zone Design Cooling Volume Flow", FinalZoneSizing( CtrlZoneNum ).ZoneName, "[m3/s]", FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow );
				SetupEMSInternalVariable( "Intermediate Zone Design Cooling Volume Flow", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[m3/s]", CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow );
				SetupEMSActuator( "Sizing:Zone", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "Zone Design Cooling Vol Flow", "[m3/s]", CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesCoolVolOn, CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesCoolVolFlow );

				//actuate  REAL(r64)          :: DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
				//actuate  REAL(r64)          :: DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]

				SetupEMSInternalVariable( "Zone Outdoor Air Design Volume Flow Rate", CalcFinalZoneSizing( CtrlZoneNum ).ZoneName, "[m3/s]", CalcFinalZoneSizing( CtrlZoneNum ).MinOA );

			}

		}
		// Use the max occupancy data from the PEOPLE structure to calculate design min OA for each zone
		// Calculate the zone design minimum outside air flow rate from the 3 Zone Sizing OA inputs and
		// from the specified OA method
		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			// Use the max occupancy data from the PEOPLE structure to calculate design min OA for each zone
			// from the outside air flow per person input
			TotPeopleInZone = 0.0;
			ZoneIndex = FinalZoneSizing( CtrlZoneNum ).ActualZoneNum;
			for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
				if ( People( PeopleNum ).ZonePtr == FinalZoneSizing( CtrlZoneNum ).ActualZoneNum ) {
					TotPeopleInZone += ( People( PeopleNum ).NumberOfPeople * Zone( FinalZoneSizing( CtrlZoneNum ).ActualZoneNum ).Multiplier * Zone( FinalZoneSizing( CtrlZoneNum ).ActualZoneNum ).ListMultiplier );
					SchMax = GetScheduleMaxValue( People( PeopleNum ).NumberOfPeoplePtr );
					if ( SchMax > 0 ) {
						FinalZoneSizing( CtrlZoneNum ).ZonePeakOccupancy = TotPeopleInZone * SchMax;
					} else {
						FinalZoneSizing( CtrlZoneNum ).ZonePeakOccupancy = TotPeopleInZone;
					}
				}
			}
			FinalZoneSizing( CtrlZoneNum ).TotalZoneFloorArea = ( Zone( ZoneIndex ).FloorArea * Zone( FinalZoneSizing( CtrlZoneNum ).ActualZoneNum ).Multiplier * Zone( FinalZoneSizing( CtrlZoneNum ).ActualZoneNum ).ListMultiplier );
			if ( FinalZoneSizing( CtrlZoneNum ).OADesMethod == OAFlowPPer || FinalZoneSizing( CtrlZoneNum ).OADesMethod == OAFlowSum || FinalZoneSizing( CtrlZoneNum ).OADesMethod == OAFlowMax ) {
				OAFromPeople = FinalZoneSizing( CtrlZoneNum ).DesOAFlowPPer * TotPeopleInZone;
			} else {
				OAFromPeople = 0.0;
			}
			OAFromArea = FinalZoneSizing( CtrlZoneNum ).DesOAFlowPerArea * FinalZoneSizing( CtrlZoneNum ).TotalZoneFloorArea;
			FinalZoneSizing( CtrlZoneNum ).TotPeopleInZone = TotPeopleInZone;
			FinalZoneSizing( CtrlZoneNum ).TotalOAFromPeople = OAFromPeople;
			FinalZoneSizing( CtrlZoneNum ).TotalOAFromArea = OAFromArea;
			// Calculate the design min OA flow rate for this zone
			UseOccSchFlag = false;
			UseMinOASchFlag = false;
			DSOAPtr = FinalZoneSizing( CtrlZoneNum ).ZoneDesignSpecOAIndex;
			OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( DSOAPtr, ZoneIndex, UseOccSchFlag, UseMinOASchFlag );

			// Zone(ZoneIndex)%Multiplier and Zone(ZoneIndex)%ListMultiplier applied in CalcDesignSpecificationOutdoorAir
			FinalZoneSizing( CtrlZoneNum ).MinOA = OAVolumeFlowRate;
			CalcFinalZoneSizing( CtrlZoneNum ).MinOA = OAVolumeFlowRate;
			if ( FinalZoneSizing( CtrlZoneNum ).ZoneADEffCooling > 0.0 || FinalZoneSizing( CtrlZoneNum ).ZoneADEffHeating > 0.0 ) {
				FinalZoneSizing( CtrlZoneNum ).MinOA /= min( FinalZoneSizing( CtrlZoneNum ).ZoneADEffCooling, FinalZoneSizing( CtrlZoneNum ).ZoneADEffHeating );
				CalcFinalZoneSizing( CtrlZoneNum ).MinOA = FinalZoneSizing( CtrlZoneNum ).MinOA;
			}
			// calculated zone design flow rates automatically take into account zone multipliers, since the zone
			// loads are multiplied (in ZoneTempPredictorCorrector.cc). Flow rates derived directly from
			// user inputs need to be explicitly multiplied by the zone multipliers.
			FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2 = FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowPerArea * Zone( ZoneIndex ).FloorArea * Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2 = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowPerArea * Zone( ZoneIndex ).FloorArea * Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2 = FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowPerArea * Zone( ZoneIndex ).FloorArea * Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2 = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowPerArea * Zone( ZoneIndex ).FloorArea * Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			FinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			CalcFinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			FinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;
			CalcFinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow *= Zone( ZoneIndex ).Multiplier * Zone( ZoneIndex ).ListMultiplier;

			for ( DesDayNum = 1; DesDayNum <= TotDesDays + TotRunDesPersDays; ++DesDayNum ) {
				ZoneSizing( DesDayNum, CtrlZoneNum ).MinOA = FinalZoneSizing( CtrlZoneNum ).MinOA;
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).MinOA = CalcFinalZoneSizing( CtrlZoneNum ).MinOA;
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow2 = FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2;
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow2 = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2;
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow = FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow;
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMinAirFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow;
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow2 = FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2;
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow2 = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2;
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow = FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow;
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMaxAirFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow;
			}
		}

		gio::write( OutputFileInits, Format_890 );
		gio::write( OutputFileInits, Format_891 ) << NumTimeStepsInAvg;
		gio::write( OutputFileInits, Format_990 );
		gio::write( OutputFileInits, Format_991 ) << GlobalHeatSizingFactor;
		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			if ( FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor != 1.0 ) {
				gio::write( OutputFileInits, Format_992 ) << FinalZoneSizing( CtrlZoneNum ).ZoneName << FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor;
			}
		}
		gio::write( OutputFileInits, Format_993 );
		gio::write( OutputFileInits, Format_994 ) << GlobalCoolSizingFactor;
		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			if ( FinalZoneSizing( CtrlZoneNum ).CoolSizingFactor != 1.0 ) {
				gio::write( OutputFileInits, Format_995 ) << FinalZoneSizing( CtrlZoneNum ).ZoneName << FinalZoneSizing( CtrlZoneNum ).CoolSizingFactor;
			}
		}

	}

	void
	RezeroZoneSizingArrays()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   August 2012 based on SetUpZoneSizingArrays
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Zero zone sizing arrays between the pulse and normal sizing.

		// METHODOLOGY EMPLOYED:
		// Based on SetUpZoneSizingArrays but remove allocates and other calculations.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DesDayNum; // design day index
		int CtrlZoneNum; // controlled zone index
		int TimeStepIndex; // zone time step index

		DisplayString( "Re-zeroing zone sizing arrays" );

		for ( DesDayNum = 1; DesDayNum <= TotDesDays + TotRunDesPersDays; ++DesDayNum ) {
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					if ( allocated( ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlowSeq ) ) {
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlowSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatLoadSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASCoolLoadSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatAddSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASLatAddSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupHumRatSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DOASTotCoolLoadSeq( TimeStepIndex ) = 0.0;
					}
					if ( allocated( ZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq ) ) {
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
						//not used directly in output report
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatSetPtSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
					if ( allocated( CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq ) ) {
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
					if ( allocated( ZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq ) ) {
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
						//not used directly in output report
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolSetPtSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
					if ( allocated( CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq ) ) {
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
				}
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesDay = ""; // name of a cooling design day
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesDay = ""; // name of a heating design day

				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMassFlow = 0.0; // zone design heating air mass flow rate [kg/s]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMassFlow = 0.0; // zone design cooling air mass flow rate [kg/s]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatLoad = 0.0; // zone design heating load [W]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolLoad = 0.0; // zone design cooling load [W]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatDens = 0.0; // zone design heating air density [kg/m3]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolDens = 0.0; // zone design cooling air density [kg/m3]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatVolFlow = 0.0; // zone design heating air volume flow rate [m3/s]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolVolFlow = 0.0; // zone design cooling air volume flow rate [m3/s]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatVolFlowMax = 0.0; // zone design heating maximum air volume flow rate [m3/s]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolVolFlowMin = 0.0; // zone design cooling minimum air volume flow rate [m3/s]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInTemp = 0.0; // zone heating coil design air inlet temperature [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInTemp = 0.0; // zone cooling coil design air inlet temperature [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInHumRat = 0.0; // zone heating coil design air inlet humidity ratio [kg/kg]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInHumRat = 0.0; // zone cooling coil design air inlet humidity ratio [kg/kg]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInTempTU = 0.0; // zone heating coil design air inlet temperature (supply air)([C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInTempTU = 0.0; // zone cooling coil design air inlet temperature (supply air)[C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInHumRatTU = 0.0; // zone heating coil design air inlet humidity ratio
				ZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInHumRatTU = 0.0; // zone cooling coil design air inlet humidity ratio
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatMassFlow = 0.0; // current zone heating air mass flow rate (HVAC time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolMassFlow = 0.0; // current zone cooling air mass flow rate (HVAC time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoad = 0.0; // current zone heating load (HVAC time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoad = 0.0; // current zone heating load (HVAC time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTemp = 0.0; // current zone temperature (heating, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTemp = 0.0; // current outdoor temperature (heating, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTemp = 0.0; // current zone return temperature (heating, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTemp = 0.0; // current zone thermostat temperature (heating, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTemp = 0.0; // current zone temperature (cooling, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTemp = 0.0; // current Outdoor temperature (cooling, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTemp = 0.0; // current zone return temperature (cooling, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTemp = 0.0; // current zone thermostat temperature (cooling, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRat = 0.0; // current zone humidity ratio (heating, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRat = 0.0; // current zone humidity ratio (cooling, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRat = 0.0; // current outdoor humidity ratio (heating, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRat = 0.0; // current outdoor humidity ratio (cooling, time step)
				ZoneSizing( DesDayNum, CtrlZoneNum ).ZoneTempAtHeatPeak = 0.0; // zone temp at max heating [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).ZoneRetTempAtHeatPeak = 0.0; // zone return temp at max heating [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).OutTempAtHeatPeak = 0.0; // outdoor temperature at max heating [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).ZoneTempAtCoolPeak = 0.0; // zone temp at max cooling [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).ZoneRetTempAtCoolPeak = 0.0; // zone return temp at max cooling [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).OutTempAtCoolPeak = 0.0; // outdoor temperature at max cooling [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).ZoneHumRatAtHeatPeak = 0.0; // zone humidity ratio at max heating [kg/kg]
				ZoneSizing( DesDayNum, CtrlZoneNum ).ZoneHumRatAtCoolPeak = 0.0; // zone humidity ratio at max cooling [kg/kg]
				ZoneSizing( DesDayNum, CtrlZoneNum ).OutHumRatAtHeatPeak = 0.0; // outdoor humidity at max heating [kg/kg]
				ZoneSizing( DesDayNum, CtrlZoneNum ).OutHumRatAtCoolPeak = 0.0; // outdoor humidity at max cooling [kg/kg]
				ZoneSizing( DesDayNum, CtrlZoneNum ).TimeStepNumAtHeatMax = 0; // time step number (in day) at Heating peak
				ZoneSizing( DesDayNum, CtrlZoneNum ).TimeStepNumAtCoolMax = 0; // time step number (in day) at cooling peak
				ZoneSizing( DesDayNum, CtrlZoneNum ).HeatDDNum = 0; // design day index of design day causing heating peak
				ZoneSizing( DesDayNum, CtrlZoneNum ).CoolDDNum = 0; // design day index of design day causing heating peak
				ZoneSizing( DesDayNum, CtrlZoneNum ).cHeatDDDate = ""; // date of design day causing heating peak
				ZoneSizing( DesDayNum, CtrlZoneNum ).cCoolDDDate = ""; // date of design day causing cooling peak
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatLoad = 0.0; // current heating load from DOAS supply air [W]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASCoolLoad = 0.0; // current cooling load from DOAS supply air [W]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlow = 0.0; // current mass flow rate of DOAS supply air [kg/s]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupTemp = 0.0; // current DOAS supply air temperature [C]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupHumRat = 0.0; // current DOAS supply air humidity ratio [kg H2O / kg dry air]
				ZoneSizing( DesDayNum, CtrlZoneNum ).DOASTotCoolLoad = 0.0; // current total cooling load imposed by DOAS supply air [W]

				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDesDay = ""; // name of a cooling design day
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDesDay = ""; // name of a heating design day

				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatMassFlow = 0.0; // zone design heating air mass flow rate [kg/s]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolMassFlow = 0.0; // zone design cooling air mass flow rate [kg/s]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatLoad = 0.0; // zone design heating load [W]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolLoad = 0.0; // zone design cooling load [W]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatDens = 0.0; // zone design heating air density [kg/m3]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolDens = 0.0; // zone design cooling air density [kg/m3]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatVolFlow = 0.0; // zone design heating air volume flow rate [m3/s]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolVolFlow = 0.0; // zone design cooling air volume flow rate [m3/s]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatVolFlowMax = 0.0; // zone design heating maximum air volume flow rate [m3/s]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolVolFlowMin = 0.0; // zone design cooling minimum air volume flow rate [m3/s]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInTemp = 0.0; // zone heating coil design air inlet temperature [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInTemp = 0.0; // zone cooling coil design air inlet temperature [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInHumRat = 0.0; // zone heating coil design air inlet humidity ratio [kg/kg]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInHumRat = 0.0; // zone cooling coil design air inlet humidity ratio [kg/kg]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInTempTU = 0.0; // zone heating coil design air inlet temperature (supply air)([C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInTempTU = 0.0; // zone cooling coil design air inlet temperature (supply air)[C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesHeatCoilInHumRatTU = 0.0; // zone heating coil design air inlet humidity ratio
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DesCoolCoilInHumRatTU = 0.0; // zone cooling coil design air inlet humidity ratio
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatMassFlow = 0.0; // current zone heating air mass flow rate (HVAC time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolMassFlow = 0.0; // current zone cooling air mass flow rate (HVAC time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoad = 0.0; // current zone heating load (HVAC time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoad = 0.0; // current zone heating load (HVAC time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTemp = 0.0; // current zone temperature (heating, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTemp = 0.0; // current outdoor temperature (heating, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTemp = 0.0; // current zone return temperature (heating, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatTstatTemp = 0.0; // current zone thermostat temperature (heating, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTemp = 0.0; // current zone temperature (cooling, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTemp = 0.0; // current Outdoor temperature (cooling, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTemp = 0.0; // current zone return temperature (cooling, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolTstatTemp = 0.0; // current zone Tstat temperature (cooling, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRat = 0.0; // current zone humidity ratio (heating, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRat = 0.0; // current zone humidity ratio (cooling, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRat = 0.0; // current outdoor humidity ratio (heating, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRat = 0.0; // current outdoor humidity ratio (cooling, time step)
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZoneTempAtHeatPeak = 0.0; // zone temp at max heating [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZoneRetTempAtHeatPeak = 0.0; // zone return temp at max heating [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).OutTempAtHeatPeak = 0.0; // outdoor temperature at max heating [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZoneTempAtCoolPeak = 0.0; // zone temp at max cooling [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZoneRetTempAtCoolPeak = 0.0; // zone return temp at max cooling [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).OutTempAtCoolPeak = 0.0; // outdoor temperature at max cooling [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZoneHumRatAtHeatPeak = 0.0; // zone humidity ratio at max heating [kg/kg]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).ZoneHumRatAtCoolPeak = 0.0; // zone humidity ratio at max cooling [kg/kg]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).OutHumRatAtHeatPeak = 0.0; // outdoor humidity at max heating [kg/kg]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).OutHumRatAtCoolPeak = 0.0; // outdoor humidity at max cooling [kg/kg]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).TimeStepNumAtHeatMax = 0; // time step number (in day) at Heating peak
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).TimeStepNumAtCoolMax = 0; // time step number (in day) at cooling peak
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatDDNum = 0; // design day index of design day causing heating peak
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolDDNum = 0; // design day index of design day causing heating peak
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).cHeatDDDate = ""; // date of design day causing heating peak
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).cCoolDDDate = ""; // date of design day causing cooling peak
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASHeatLoad = 0.0; // current heating load from DOAS supply air [W]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASCoolLoad = 0.0; // current cooling load from DOAS supply air [W]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupMassFlow = 0.0; // current mass flow rate of DOAS supply air [kg/s]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupTemp = 0.0; // current DOAS supply air temperature [C]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASSupHumRat = 0.0; // current DOAS supply air humidity ratio [kg H2O / kg dry air]
				CalcZoneSizing( DesDayNum, CtrlZoneNum ).DOASTotCoolLoad = 0.0; // current total cooling load imposed by DOAS supply air [W]
			}
		}
		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
				if ( allocated( FinalZoneSizing( CtrlZoneNum ).DOASSupMassFlowSeq ) ) {
					FinalZoneSizing( CtrlZoneNum ).DOASSupMassFlowSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).DOASHeatLoadSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).DOASCoolLoadSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).DOASHeatAddSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).DOASLatAddSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).DOASSupTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).DOASSupHumRatSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).DOASTotCoolLoadSeq( TimeStepIndex ) = 0.0;
				}
				if ( allocated( FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq ) ) {
					FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
				}
				if ( allocated( CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq ) ) {
					CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
				}
				if ( allocated( FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq ) ) {
					FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
					FinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
				}
				if ( allocated( CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq ) ) {
					CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
				}
			}
			FinalZoneSizing( CtrlZoneNum ).CoolDesDay = ""; // name of a cooling design day
			FinalZoneSizing( CtrlZoneNum ).HeatDesDay = ""; // name of a heating design day

			FinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = 0.0; // zone design heating air mass flow rate [kg/s]
			FinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = 0.0; // zone design cooling air mass flow rate [kg/s]
			FinalZoneSizing( CtrlZoneNum ).DesHeatLoad = 0.0; // zone design heating load [W]
			FinalZoneSizing( CtrlZoneNum ).DesCoolLoad = 0.0; // zone design cooling load [W]
			FinalZoneSizing( CtrlZoneNum ).DesHeatDens = 0.0; // zone design heating air density [kg/m3]
			FinalZoneSizing( CtrlZoneNum ).DesCoolDens = 0.0; // zone design cooling air density [kg/m3]
			FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = 0.0; // zone design heating air volume flow rate [m3/s]
			FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = 0.0; // zone design cooling air volume flow rate [m3/s]
			FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlowMax = 0.0; // zone design heating maximum air volume flow rate [m3/s]
			FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin = 0.0; // zone design cooling minimum air volume flow rate [m3/s]
			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTemp = 0.0; // zone heating coil design air inlet temperature [C]
			FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTemp = 0.0; // zone cooling coil design air inlet temperature [C]
			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRat = 0.0; // zone heating coil design air inlet humidity ratio [kg/kg]
			FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRat = 0.0; // zone cooling coil design air inlet humidity ratio [kg/kg]
			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTempTU = 0.0; // zone heating coil design air inlet temperature (supply air)([C]
			FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTempTU = 0.0; // zone cooling coil design air inlet temperature (supply air)[C]
			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRatTU = 0.0; // zone heating coil design air inlet humidity ratio
			FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRatTU = 0.0; // zone cooling coil design air inlet humidity ratio
			FinalZoneSizing( CtrlZoneNum ).HeatMassFlow = 0.0; // current zone heating air mass flow rate (HVAC time step)
			FinalZoneSizing( CtrlZoneNum ).CoolMassFlow = 0.0; // current zone cooling air mass flow rate (HVAC time step)
			FinalZoneSizing( CtrlZoneNum ).HeatLoad = 0.0; // current zone heating load (HVAC time step)
			FinalZoneSizing( CtrlZoneNum ).CoolLoad = 0.0; // current zone heating load (HVAC time step)
			FinalZoneSizing( CtrlZoneNum ).HeatZoneTemp = 0.0; // current zone temperature (heating, time step)
			FinalZoneSizing( CtrlZoneNum ).HeatOutTemp = 0.0; // current outdoor temperature (heating, time step)
			FinalZoneSizing( CtrlZoneNum ).HeatZoneRetTemp = 0.0; // current zone return temperature (heating, time step)
			FinalZoneSizing( CtrlZoneNum ).HeatTstatTemp = 0.0; // current zone thermostat temperature (heating, time step)
			FinalZoneSizing( CtrlZoneNum ).CoolZoneTemp = 0.0; // current zone temperature (cooling, time step)
			FinalZoneSizing( CtrlZoneNum ).CoolOutTemp = 0.0; // current Outdoor temperature (cooling, time step)
			FinalZoneSizing( CtrlZoneNum ).CoolZoneRetTemp = 0.0; // current zone return temperature (cooling, time step)
			FinalZoneSizing( CtrlZoneNum ).CoolTstatTemp = 0.0; // current zone thermostat temperature (cooling, time step)
			FinalZoneSizing( CtrlZoneNum ).HeatZoneHumRat = 0.0; // current zone humidity ratio (heating, time step)
			FinalZoneSizing( CtrlZoneNum ).CoolZoneHumRat = 0.0; // current zone humidity ratio (cooling, time step)
			FinalZoneSizing( CtrlZoneNum ).HeatOutHumRat = 0.0; // current outdoor humidity ratio (heating, time step)
			FinalZoneSizing( CtrlZoneNum ).CoolOutHumRat = 0.0; // current outdoor humidity ratio (cooling, time step)
			FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak = 0.0; // zone temp at max heating [C]
			FinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtHeatPeak = 0.0; // zone return temp at max heating [C]
			FinalZoneSizing( CtrlZoneNum ).OutTempAtHeatPeak = 0.0; // outdoor temperature at max heating [C]
			FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak = 0.0; // zone temp at max cooling [C]
			FinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtCoolPeak = 0.0; // zone return temp at max cooling [C]
			FinalZoneSizing( CtrlZoneNum ).OutTempAtCoolPeak = 0.0; // outdoor temperature at max cooling [C]
			FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = 0.0; // zone humidity ratio at max heating [kg/kg]
			FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = 0.0; // zone humidity ratio at max cooling [kg/kg]
			FinalZoneSizing( CtrlZoneNum ).OutHumRatAtHeatPeak = 0.0; // outdoor humidity at max heating [kg/kg]
			FinalZoneSizing( CtrlZoneNum ).OutHumRatAtCoolPeak = 0.0; // outdoor humidity at max cooling [kg/kg]
			FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = 0; // time step number (in day) at Heating peak
			FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = 0; // time step number (in day) at cooling peak
			FinalZoneSizing( CtrlZoneNum ).HeatDDNum = 0; // design day index of design day causing heating peak
			FinalZoneSizing( CtrlZoneNum ).CoolDDNum = 0; // design day index of design day causing heating peak
			FinalZoneSizing( CtrlZoneNum ).cHeatDDDate = ""; // date of design day causing heating peak
			FinalZoneSizing( CtrlZoneNum ).cCoolDDDate = ""; // date of design day causing cooling peak
			FinalZoneSizing( CtrlZoneNum ).DOASHeatLoad = 0.0; // current heating load from DOAS supply air [W]
			FinalZoneSizing( CtrlZoneNum ).DOASCoolLoad = 0.0; // current cooling load from DOAS supply air [W]
			FinalZoneSizing( CtrlZoneNum ).DOASSupMassFlow = 0.0; // current mass flow rate of DOAS supply air [kg/s]
			FinalZoneSizing( CtrlZoneNum ).DOASSupTemp = 0.0; // current DOAS supply air temperature [C]
			FinalZoneSizing( CtrlZoneNum ).DOASSupHumRat = 0.0; // current DOAS supply air humidity ratio [kg H2O / kg dry air]
			FinalZoneSizing( CtrlZoneNum ).DOASTotCoolLoad = 0.0; // current total cooling load imposed by DOAS supply air [W]

			CalcFinalZoneSizing( CtrlZoneNum ).CoolDesDay = ""; // name of a cooling design day
			CalcFinalZoneSizing( CtrlZoneNum ).HeatDesDay = ""; // name of a heating design day

			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = 0.0; // zone design heating air mass flow rate [kg/s]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = 0.0; // zone design cooling air mass flow rate [kg/s]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad = 0.0; // zone design heating load [W]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad = 0.0; // zone design cooling load [W]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatDens = 0.0; // zone design heating air density [kg/m3]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolDens = 0.0; // zone design cooling air density [kg/m3]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = 0.0; // zone design heating air volume flow rate [m3/s]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = 0.0; // zone design cooling air volume flow rate [m3/s]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlowMax = 0.0; // zone design heating maximum air volume flow rate [m3/s]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin = 0.0; // zone design cooling minimum air volume flow rate [m3/s]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTemp = 0.0; // zone heating coil design air inlet temperature [C]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTemp = 0.0; // zone cooling coil design air inlet temperature [C]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRat = 0.0; // zone heating coil design air inlet humidity ratio [kg/kg]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRat = 0.0; // zone cooling coil design air inlet humidity ratio [kg/kg]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTempTU = 0.0; // zone heating coil design air inlet temperature (supply air)([C]
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTempTU = 0.0; // zone cooling coil design air inlet temperature (supply air)[C]
			CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRatTU = 0.0; // zone heating coil design air inlet humidity ratio
			CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRatTU = 0.0; // zone cooling coil design air inlet humidity ratio
			CalcFinalZoneSizing( CtrlZoneNum ).HeatMassFlow = 0.0; // current zone heating air mass flow rate (HVAC time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolMassFlow = 0.0; // current zone cooling air mass flow rate (HVAC time step)
			CalcFinalZoneSizing( CtrlZoneNum ).HeatLoad = 0.0; // current zone heating load (HVAC time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolLoad = 0.0; // current zone heating load (HVAC time step)
			CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneTemp = 0.0; // current zone temperature (heating, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).HeatOutTemp = 0.0; // current outdoor temperature (heating, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneRetTemp = 0.0; // current zone return temperature (heating, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).HeatTstatTemp = 0.0; // current zone thermostat temperature (heating, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTemp = 0.0; // current zone temperature (cooling, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTemp = 0.0; // current Outdoor temperature (cooling, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTemp = 0.0; // current zone return temperature (cooling, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolTstatTemp = 0.0; // current zone thermostat temperature (cooling, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRat = 0.0; // current zone humidity ratio (heating, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneHumRat = 0.0; // current zone humidity ratio (cooling, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRat = 0.0; // current outdoor humidity ratio (heating, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).CoolOutHumRat = 0.0; // current outdoor humidity ratio (cooling, time step)
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak = 0.0; // zone temp at max heating [C]
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtHeatPeak = 0.0; // zone return temp at max heating [C]
			CalcFinalZoneSizing( CtrlZoneNum ).OutTempAtHeatPeak = 0.0; // outdoor temperature at max heating [C]
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak = 0.0; // zone temp at max cooling [C]
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtCoolPeak = 0.0; // zone return temp at max cooling [C]
			CalcFinalZoneSizing( CtrlZoneNum ).OutTempAtCoolPeak = 0.0; // outdoor temperature at max cooling [C]
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = 0.0; // zone humidity ratio at max heating [kg/kg]
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = 0.0; // zone humidity ratio at max cooling [kg/kg]
			CalcFinalZoneSizing( CtrlZoneNum ).OutHumRatAtHeatPeak = 0.0; // outdoor humidity at max heating [kg/kg]
			CalcFinalZoneSizing( CtrlZoneNum ).OutHumRatAtCoolPeak = 0.0; // outdoor humidity at max cooling [kg/kg]
			CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = 0; // time step number (in day) at Heating peak
			CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = 0; // time step number (in day) at cooling peak
			CalcFinalZoneSizing( CtrlZoneNum ).HeatDDNum = 0; // design day index of design day causing heating peak
			CalcFinalZoneSizing( CtrlZoneNum ).CoolDDNum = 0; // design day index of design day causing heating peak
			CalcFinalZoneSizing( CtrlZoneNum ).cHeatDDDate = ""; // date of design day causing heating peak
			CalcFinalZoneSizing( CtrlZoneNum ).cCoolDDDate = ""; // date of design day causing cooling peak
			CalcFinalZoneSizing( CtrlZoneNum ).DOASHeatLoad = 0.0; // current heating load from DOAS supply air [W]
			CalcFinalZoneSizing( CtrlZoneNum ).DOASCoolLoad = 0.0; // current cooling load from DOAS supply air [W]
			CalcFinalZoneSizing( CtrlZoneNum ).DOASSupMassFlow = 0.0; // current mass flow rate of DOAS supply air [kg/s]
			CalcFinalZoneSizing( CtrlZoneNum ).DOASSupTemp = 0.0; // current DOAS supply air temperature [C]
			CalcFinalZoneSizing( CtrlZoneNum ).DOASSupHumRat = 0.0; // current DOAS supply air humidity ratio [kg H2O / kg dry air]
			CalcFinalZoneSizing( CtrlZoneNum ).DOASTotCoolLoad = 0.0; // current total cooling load imposed by DOAS supply air [W]
		}
	}

	void
	UpdateZoneSizing( int const CallIndicator )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update the result variables of the zone sizing calculation

		// METHODOLOGY EMPLOYED:
		// CallIndicator = 1 (BeginDay) zero the result arrays
		// CallIndicator = 2 (DuringDay) fill arrays, averaging over 1 zone time step
		// CallIndicator = 3 (EndDay) calculate daily maxima
		// CallIndicator = 4 (EndZoneSizingCalc) write out results

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStep;
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::BeginDay;
		using DataGlobals::DuringDay;
		using DataGlobals::EndDay;
		using DataGlobals::EndZoneSizingCalc;
		using DataGlobals::MinutesPerTimeStep;
		using DataGlobals::OutputFileZoneSizing;
		using DataGlobals::emsCallFromZoneSizing;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::isPulseZoneSizing;
		using DataHVACGlobals::FracTimeStepZone;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallTempDiff;
		using DataEnvironment::StdBaroPress;
		using DataEnvironment::StdRhoAir;
		using General::MovingAvg;
		using General::RoundSigDigits;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;
		using DataHeatBalFanSys::ZoneThermostatSetPointLo;
		using DataHeatBalFanSys::TempZoneThermostatSetPoint;
		using EMSManager::ManageEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );
		static gio::Fmt ZSizeFmt10( "('Time')" );
		static gio::Fmt ZSizeFmt11( "(A1,A,':',A,A,A1,A,':',A,A,A1,A,':',A,A,A1,A,':',A,A )" );
		static gio::Fmt ZSizeFmt20( "(I2.2,':',I2.2,':00')" );
		static gio::Fmt ZSizeFmt21( "(A1,ES12.6,A1,ES12.6,A1,ES12.6,A1,ES12.6 )" );
		static gio::Fmt ZSizeFmt30( "('Peak')" );
		static gio::Fmt ZSizeFmt31( "(A1,ES12.6,A1,ES12.6,A1,ES12.6,A1,ES12.6)" );
		static gio::Fmt ZSizeFmt40( "(/'Peak Vol Flow (m3/s)')" );
		static gio::Fmt ZSizeFmt41( "(A1,A1,A1,ES12.6,A1,ES12.6)" );
		static std::string const RoutineName( "UpdateZoneSizing" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DesDayNum; // design day index
		int TimeStepIndex; // zone time step index
		int CtrlZoneNum; // controlled zone index
		int TimeStepInDay; // zone time step in day
		int I; // write statement index
		//  REAL(r64)    :: HourFrac           ! fractional hour
		int HourCounter; // Hour Counter
		int TimeStepCounter; // Time Step Counter
		int Minutes; // Current Minutes Counter
		int HourPrint; // Hour to print (timestamp)
		Real64 OAFrac; // outside air fraction
		int TimeStepAtPeak; // time step number at heat or cool peak
		int TimeStepAtPeakF; // time step number at heat or cool peak (final)
		int DDNum; // Design Day index
		int DDNumF; // Design Day index (final)
		Real64 TotCoolSizMult; // combines user cooling design flow input with zone sizing multiplier
		Real64 TotHeatSizMult; // combines user heating design flow input with zone sizing multiplier
		Real64 MinOAMass; // zone minimum outside air mass flow rate kg/s
		Real64 MaxOfMinCoolVolFlow; // max of the user specified design cooling minimum flows and min OA flow [m3/s]
		Real64 MaxOfMinCoolMassFlow; // max of the user specified design cooling minimum flows and min OA flow [kg/s]
		Real64 MaxHeatVolFlow; // max of user specified design heating max flow [m3/s]
		std::string HrMinString; // store hour/minute string before assigning to peak string array
		Real64 SupplyTemp; // supply air temperature [C]
		Real64 DeltaTemp; // supply air delta temperature [deltaC]

		{ auto const SELECT_CASE_var( CallIndicator );

		if ( SELECT_CASE_var == BeginDay ) {

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {

				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;

				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolDesDay = EnvironmentName;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatDesDay = EnvironmentName;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatDens = StdRhoAir;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolDens = StdRhoAir;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatDDNum = CurOverallSimDay;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolDDNum = CurOverallSimDay;

			}

		} else if ( SELECT_CASE_var == DuringDay ) {

			TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;

			// save the results of the ideal zone component calculation in the CalcZoneSizing sequence variables
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				if ( ZoneThermostatSetPointHi( CtrlZoneNum ) > 0.0 && ZoneThermostatSetPointHi( CtrlZoneNum ) > ZoneSizThermSetPtHi( CtrlZoneNum ) ) {
					ZoneSizThermSetPtHi( CtrlZoneNum ) = ZoneThermostatSetPointHi( CtrlZoneNum );
				}
				if ( ZoneThermostatSetPointLo( CtrlZoneNum ) > 0.0 && ZoneThermostatSetPointLo( CtrlZoneNum ) < ZoneSizThermSetPtLo( CtrlZoneNum ) ) {
					ZoneSizThermSetPtLo( CtrlZoneNum ) = ZoneThermostatSetPointLo( CtrlZoneNum );
				}
				ZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatSetPtSeq( TimeStepInDay ) = ZoneThermostatSetPointLo( CtrlZoneNum );
				ZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatTstatTempSeq( TimeStepInDay ) = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatTstatTemp;
				ZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolSetPtSeq( TimeStepInDay ) = ZoneThermostatSetPointHi( CtrlZoneNum );
				ZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolTstatTempSeq( TimeStepInDay ) = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolTstatTemp;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatFlowSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatMassFlow * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoadSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoad * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneTempSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneTemp * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutTempSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutTemp * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneRetTemp * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneHumRat * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutHumRat * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolFlowSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolMassFlow * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolLoadSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolLoad * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneTempSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneTemp * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutTempSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutTemp * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneRetTemp * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneHumRat * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutHumRat * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASHeatLoadSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASHeatLoad * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASCoolLoadSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASCoolLoad * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASHeatAddSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASHeatAdd * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASLatAddSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASLatAdd * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASSupMassFlowSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASSupMassFlow * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASSupTempSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASSupTemp * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASSupHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASSupHumRat * FracTimeStepZone;
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASTotCoolLoadSeq( TimeStepInDay ) += CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASTotCoolLoad * FracTimeStepZone;
			}

		} else if ( SELECT_CASE_var == EndDay ) {
			// average some of the zone sequences to reduce peakiness
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolFlowSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolFlowSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolLoadSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolLoadSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatFlowSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatFlowSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoadSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoadSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneRetTempSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneRetTempSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneRetTempSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneRetTempSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( !ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASHeatAddSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASHeatAddSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( !ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASLatAddSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DOASLatAddSeq = AvgData;
			}

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {

				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				// save the sequence values at the heating peak
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					if ( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) > CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatLoad ) {
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatLoad = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatMassFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneTempAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutTempAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneRetTempAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneHumRatAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutHumRatAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).TimeStepNumAtHeatMax = TimeStepIndex;
					}
				}
				if ( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatMassFlow > 0.0 ) {
					CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatVolFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatMassFlow / CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatDens;
					OAFrac = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).MinOA / max( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatVolFlow, SmallMassFlow );
					OAFrac = min( 1.0, max( 0.0, OAFrac ) );
					TimeStepAtPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).TimeStepNumAtHeatMax;
					CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatCoilInTemp = OAFrac * DesDayWeath( CurOverallSimDay ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneTempAtHeatPeak;
					CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatCoilInHumRat = OAFrac * DesDayWeath( CurOverallSimDay ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneHumRatAtHeatPeak;
				}
				// save the sequence values at the cooling peak
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					if ( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) > CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolLoad ) {
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolLoad = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolMassFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneTempAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutTempAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneRetTempAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneHumRatAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutHumRatAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).TimeStepNumAtCoolMax = TimeStepIndex;
					}
				}
				if ( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolMassFlow > 0.0 ) {
					CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolVolFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolMassFlow / CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolDens;
					OAFrac = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).MinOA / max( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolVolFlow, SmallMassFlow );
					OAFrac = min( 1.0, max( 0.0, OAFrac ) );
					TimeStepAtPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).TimeStepNumAtCoolMax;
					CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolCoilInTemp = OAFrac * DesDayWeath( CurOverallSimDay ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneTempAtCoolPeak;
					CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolCoilInHumRat = OAFrac * DesDayWeath( CurOverallSimDay ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneHumRatAtCoolPeak;
				}
				// from all the design periods, choose the one needing the most heating and save all its design variables in CalcFinalZoneSizing
				if ( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatVolFlow > CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow ) {
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatVolFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatLoad;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatMassFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatDesDay = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatDesDay;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatDens = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatDens;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatFlowSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatLoadSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoadSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneRetTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatZoneHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatOutHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneTempAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutTempAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutTempAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneRetTempAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneHumRatAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutHumRatAtHeatPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutHumRatAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatDDNum = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatDDNum;
					CalcFinalZoneSizing( CtrlZoneNum ).cHeatDDDate = DesDayWeath( CurOverallSimDay ).DateString;
					CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).TimeStepNumAtHeatMax;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTemp = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatCoilInTemp;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRat = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesHeatCoilInHumRat;
				} else {
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatDens = StdRhoAir;
				}
				// from all the design periods, choose the one needing the most Cooling and save all its design variables in CalcFinalZoneSizing
				if ( CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolVolFlow > CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow ) {
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolVolFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolLoad;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolMassFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolDesDay = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolDesDay;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolDens = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolDens;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolFlowSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolLoadSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolLoadSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneRetTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolOutHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneTempAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutTempAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutTempAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneRetTempAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZoneHumRatAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutHumRatAtCoolPeak = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).OutHumRatAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolDDNum = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolDDNum;
					CalcFinalZoneSizing( CtrlZoneNum ).cCoolDDDate = DesDayWeath( CurOverallSimDay ).DateString;
					CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).TimeStepNumAtCoolMax;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTemp = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolCoilInTemp;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRat = CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).DesCoolCoilInHumRat;
				} else {
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolDens = StdRhoAir;
				}

			}

		} else if ( SELECT_CASE_var == EndZoneSizingCalc ) {

			// candidate EMS calling point to customize CalcFinalZoneSizing
			ManageEMS( emsCallFromZoneSizing );

			// now apply EMS overrides (if any)

			if ( AnyEnergyManagementSystemInModel ) {
				for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
					if ( CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesHeatMassOn ) {
						if ( CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow > 0.0 ) CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesHeatMassFlow;
					}
					if ( CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesCoolMassOn ) {
						if ( CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow > 0.0 ) CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesCoolMassFlow;
					}
					if ( CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesHeatLoadOn ) {
						if ( CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad > 0.0 ) CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad = CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesHeatLoad;
					}
					if ( CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesCoolLoadOn ) {
						if ( CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad > 0.0 ) CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad = CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesCoolLoad;
					}
					if ( CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesHeatVolOn ) {
						if ( CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow > 0.0 ) CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesHeatVolFlow;
					}
					if ( CalcFinalZoneSizing( CtrlZoneNum ).EMSOverrideDesCoolVolOn ) {
						if ( CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow > 0.0 ) CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = CalcFinalZoneSizing( CtrlZoneNum ).EMSValueDesCoolVolFlow;
					}
				}
			}

			if ( ! isPulseZoneSizing ) {

				for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
					if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
					if ( std::abs( CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad ) <= 1.e-8 ) {
						ShowWarningError( "Calculated design cooling load for zone=" + CalcFinalZoneSizing( CtrlZoneNum ).ZoneName + " is zero." );
						ShowContinueError( "Check Sizing:Zone and ZoneControl:Thermostat inputs." );
					}
					if ( std::abs( CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad ) <= 1.e-8 ) {
						ShowWarningError( "Calculated design heating load for zone=" + CalcFinalZoneSizing( CtrlZoneNum ).ZoneName + " is zero." );
						ShowContinueError( "Check Sizing:Zone and ZoneControl:Thermostat inputs." );
					}
				}

				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt10, flags ); }
				for ( I = 1; I <= NumOfZones; ++I ) {
					if ( ! ZoneEquipConfig( I ).IsControlled ) continue;
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt11, flags ) << SizingFileColSep << CalcFinalZoneSizing( I ).ZoneName << CalcFinalZoneSizing( I ).HeatDesDay << ":Des Heat Load [W]" << SizingFileColSep << CalcFinalZoneSizing( I ).ZoneName << CalcFinalZoneSizing( I ).CoolDesDay << ":Des Sens Cool Load [W]" << SizingFileColSep << CalcFinalZoneSizing( I ).ZoneName << CalcFinalZoneSizing( I ).HeatDesDay << ":Des Heat Mass Flow [kg/s]" << SizingFileColSep << CalcFinalZoneSizing( I ).ZoneName << CalcFinalZoneSizing( I ).CoolDesDay << ":Des Cool Mass Flow [kg/s]"; }

					// Should this be done only if there is a cooling load? Or would this message help determine why there was no load?
					if ( std::abs( CalcFinalZoneSizing( I ).DesCoolLoad ) > 1.e-8 ) {
						// check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
						if ( CalcFinalZoneSizing( I ).ZnCoolDgnSAMethod == SupplyAirTemperature ) {
							SupplyTemp = CalcFinalZoneSizing( I ).CoolDesTemp;
							DeltaTemp = SupplyTemp - CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak;
						} else {
							DeltaTemp = - std::abs( CalcFinalZoneSizing( I ).CoolDesTempDiff );
							SupplyTemp = DeltaTemp + CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak;
						}

						// check for low delta T to avoid very high flow rates
						if ( std::abs( DeltaTemp ) < 5.0 && std::abs( DeltaTemp ) > SmallTempDiff ) { // Vdot exceeds 1200 cfm/ton @ DT=5
							if ( std::abs( DeltaTemp ) >= 2.0 ) { // Vdot exceeds 3000 cfm/ton @ DT=2
								ShowWarningError( "UpdateZoneSizing: Cooling supply air temperature (calculated) within 5C of zone temperature" );
							} else {
								ShowSevereError( "UpdateZoneSizing: Cooling supply air temperature (calculated) within 2C of zone temperature" );
							}
							ShowContinueError( "...check zone thermostat set point and design supply air temperatures" );
							ShowContinueError( "...zone name = " + CalcFinalZoneSizing( I ).ZoneName );
							ShowContinueError( "...design sensible cooling load = " + RoundSigDigits( CalcFinalZoneSizing( I ).DesCoolLoad, 2 ) + " W" );
							ShowContinueError( "...thermostat set point temp    = " + RoundSigDigits( CalcFinalZoneSizing( I ).CoolTstatTemp, 3 ) + " C" );
							ShowContinueError( "...zone temperature             = " + RoundSigDigits( CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak, 3 ) + " C" );
							ShowContinueError( "...supply air temperature       = " + RoundSigDigits( SupplyTemp, 3 ) + " C" );
							ShowContinueError( "...temperature difference       = " + RoundSigDigits( DeltaTemp, 5 ) + " C" );
							ShowContinueError( "...calculated volume flow rate  = " + RoundSigDigits( ( CalcFinalZoneSizing( I ).DesCoolVolFlow ), 5 ) + " m3/s" );
							ShowContinueError( "...calculated mass flow rate    = " + RoundSigDigits( ( CalcFinalZoneSizing( I ).DesCoolMassFlow ), 5 ) + " kg/s" );
							if ( SupplyTemp > CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak ) ShowContinueError( "...Note: supply air temperature should be less than zone temperature during cooling air flow calculations" );
						} else if ( std::abs( DeltaTemp ) > SmallTempDiff && SupplyTemp > CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak ) {
							ShowSevereError( "UpdateZoneSizing: Supply air temperature is greater than zone temperature during cooling air flow calculations" );
							ShowContinueError( "...zone temperature            = " + RoundSigDigits( CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak, 3 ) + " C" );
							ShowContinueError( "...supply air temperature      = " + RoundSigDigits( SupplyTemp, 3 ) + " C" );
							ShowContinueError( "...occurs in zone              = " + CalcFinalZoneSizing( I ).ZoneName );
						}
					}
					// Should this be done only if there is a heating load? Or would this message help determine why there was no load?
					if ( std::abs( CalcFinalZoneSizing( I ).DesHeatLoad ) > 1.e-8 ) { // ABS() ?
						// check for low cooling delta T from supply to zone to see if air volume flow rate might be excessively high
						if ( CalcFinalZoneSizing( I ).ZnHeatDgnSAMethod == SupplyAirTemperature ) {
							SupplyTemp = CalcFinalZoneSizing( I ).HeatDesTemp;
							DeltaTemp = SupplyTemp - CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak;
						} else {
							DeltaTemp = CalcFinalZoneSizing( I ).HeatDesTempDiff;
							SupplyTemp = DeltaTemp + CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak;
						}

						if ( std::abs( DeltaTemp ) < 5.0 && std::abs( DeltaTemp ) > SmallTempDiff ) { // Vdot exceeds 1200 cfm/ton @ DT=5
							if ( std::abs( DeltaTemp ) >= 2.0 ) { // Vdot exceeds 3000 cfm/ton @ DT=2
								ShowWarningError( "UpdateZoneSizing: Heating supply air temperature (calculated) within 5C of zone temperature" );
							} else {
								ShowSevereError( "UpdateZoneSizing: Heating supply air temperature (calculated) within 2C of zone temperature" );
							}
							ShowContinueError( "...check zone thermostat set point and design supply air temperatures" );
							ShowContinueError( "...zone name = " + CalcFinalZoneSizing( I ).ZoneName );
							ShowContinueError( "...design heating load         = " + RoundSigDigits( CalcFinalZoneSizing( I ).DesCoolLoad, 2 ) + " W" );
							ShowContinueError( "...thermostat set piont temp   = " + RoundSigDigits( CalcFinalZoneSizing( I ).HeatTstatTemp, 3 ) + " C" );
							ShowContinueError( "...zone temperature            = " + RoundSigDigits( CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak, 3 ) + " C" );
							ShowContinueError( "...supply air temperature      = " + RoundSigDigits( SupplyTemp, 3 ) + " C" );
							ShowContinueError( "...temperature difference      = " + RoundSigDigits( DeltaTemp, 5 ) + " C" );
							ShowContinueError( "...calculated volume flow rate = " + RoundSigDigits( ( CalcFinalZoneSizing( I ).DesHeatVolFlow ), 5 ) + " m3/s" );
							ShowContinueError( "...calculated mass flow rate   = " + RoundSigDigits( ( CalcFinalZoneSizing( I ).DesHeatMassFlow ), 5 ) + " kg/s" );
							if ( SupplyTemp < CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak ) ShowContinueError( "...Note: supply air temperature should be greater than zone temperature during heating air flow calculations" );
						} else if ( std::abs( DeltaTemp ) > SmallTempDiff && SupplyTemp < CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak ) {
							ShowSevereError( "UpdateZoneSizing: Supply air temperature is less than zone temperature during heating air flow calculations" );
							ShowContinueError( "...zone temperature            = " + RoundSigDigits( CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak, 3 ) + " C" );
							ShowContinueError( "...supply air temperature      = " + RoundSigDigits( SupplyTemp, 3 ) + " C" );
							ShowContinueError( "...occurs in zone              = " + CalcFinalZoneSizing( I ).ZoneName );
						}
					}

				}

				gio::write( OutputFileZoneSizing );
				//      HourFrac = 0.0
				Minutes = 0;
				TimeStepIndex = 0;
				for ( HourCounter = 1; HourCounter <= 24; ++HourCounter ) {
					for ( TimeStepCounter = 1; TimeStepCounter <= NumOfTimeStepInHour; ++TimeStepCounter ) {
						++TimeStepIndex;
						Minutes += MinutesPerTimeStep;
						if ( Minutes == 60 ) {
							Minutes = 0;
							HourPrint = HourCounter;
						} else {
							HourPrint = HourCounter - 1;
						}
						for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
							if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
							if ( TimeStepIndex == CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax ) {
								gio::write( HrMinString, PeakHrMinFmt ) << HourPrint << Minutes;
								HeatPeakDateHrMin( CtrlZoneNum ) = CalcFinalZoneSizing( CtrlZoneNum ).cHeatDDDate + ' ' + HrMinString;
							}
							if ( TimeStepIndex == CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax ) {
								gio::write( HrMinString, PeakHrMinFmt ) << HourPrint << Minutes;
								CoolPeakDateHrMin( CtrlZoneNum ) = CalcFinalZoneSizing( CtrlZoneNum ).cCoolDDDate + ' ' + HrMinString;
							}
						}
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt20, flags ) << HourPrint << Minutes; }
						for ( I = 1; I <= NumOfZones; ++I ) {
							if ( ! ZoneEquipConfig( I ).IsControlled ) continue;
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt21, flags ) << SizingFileColSep << CalcFinalZoneSizing( I ).HeatLoadSeq( TimeStepIndex ) << SizingFileColSep << CalcFinalZoneSizing( I ).CoolLoadSeq( TimeStepIndex ) << SizingFileColSep << CalcFinalZoneSizing( I ).HeatFlowSeq( TimeStepIndex ) << SizingFileColSep << CalcFinalZoneSizing( I ).CoolFlowSeq( TimeStepIndex ); }
						}
						gio::write( OutputFileZoneSizing );
					}
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt30, flags ); }
				for ( I = 1; I <= NumOfZones; ++I ) {
					if ( ! ZoneEquipConfig( I ).IsControlled ) continue;
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt31, flags ) << SizingFileColSep << CalcFinalZoneSizing( I ).DesHeatLoad << SizingFileColSep << CalcFinalZoneSizing( I ).DesCoolLoad << SizingFileColSep << CalcFinalZoneSizing( I ).DesHeatMassFlow << SizingFileColSep << CalcFinalZoneSizing( I ).DesCoolMassFlow; }
				}
				gio::write( OutputFileZoneSizing );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt40, flags ); }
				for ( I = 1; I <= NumOfZones; ++I ) {
					if ( ! ZoneEquipConfig( I ).IsControlled ) continue;
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileZoneSizing, ZSizeFmt41, flags ) << SizingFileColSep << SizingFileColSep << SizingFileColSep << CalcFinalZoneSizing( I ).DesHeatVolFlow << SizingFileColSep << CalcFinalZoneSizing( I ).DesCoolVolFlow; }
				}
				gio::write( OutputFileZoneSizing );
				gio::close( OutputFileZoneSizing );
			}

			// Move data from Calc arrays to user modified arrays

			for ( std::size_t i = 0; i < ZoneSizing.size(); ++i ) {
				auto & z( ZoneSizing[ i ] );
				auto & c( CalcZoneSizing[ i ] );
				z.CoolDesDay = c.CoolDesDay;
				z.HeatDesDay = c.HeatDesDay;
				z.DesHeatDens = c.DesHeatDens;
				z.DesCoolDens = c.DesCoolDens;
				z.HeatDDNum = c.HeatDDNum;
				z.CoolDDNum = c.CoolDDNum;

				z.DesHeatLoad = c.DesHeatLoad;
				z.DesHeatMassFlow = c.DesHeatMassFlow;
				z.ZoneTempAtHeatPeak = c.ZoneTempAtHeatPeak;
				z.OutTempAtHeatPeak = c.OutTempAtHeatPeak;
				z.ZoneRetTempAtHeatPeak = c.ZoneRetTempAtHeatPeak;
				z.ZoneHumRatAtHeatPeak = c.ZoneHumRatAtHeatPeak;
				z.OutHumRatAtHeatPeak = c.OutHumRatAtHeatPeak;
				z.TimeStepNumAtHeatMax = c.TimeStepNumAtHeatMax;
				z.DesHeatVolFlow = c.DesHeatVolFlow;
				z.DesHeatCoilInTemp = c.DesHeatCoilInTemp;
				z.DesHeatCoilInHumRat = c.DesHeatCoilInHumRat;

				z.DesCoolLoad = c.DesCoolLoad;
				z.DesCoolMassFlow = c.DesCoolMassFlow;
				z.ZoneTempAtCoolPeak = c.ZoneTempAtCoolPeak;
				z.OutTempAtCoolPeak = c.OutTempAtCoolPeak;
				z.ZoneRetTempAtCoolPeak = c.ZoneRetTempAtCoolPeak;
				z.ZoneHumRatAtCoolPeak = c.ZoneHumRatAtCoolPeak;
				z.OutHumRatAtCoolPeak = c.OutHumRatAtCoolPeak;
				z.TimeStepNumAtCoolMax = c.TimeStepNumAtCoolMax;
				z.DesCoolVolFlow = c.DesCoolVolFlow;
				z.DesCoolCoilInTemp = c.DesCoolCoilInTemp;
				z.DesCoolCoilInHumRat = c.DesCoolCoilInHumRat;
			}

			for ( std::size_t i = 0; i < FinalZoneSizing.size(); ++i ) {
				auto & z( FinalZoneSizing[ i ] );
				auto & c( CalcFinalZoneSizing[ i ] );
				z.CoolDesDay = c.CoolDesDay;
				z.HeatDesDay = c.HeatDesDay;
				z.DesHeatDens = c.DesHeatDens;
				z.DesCoolDens = c.DesCoolDens;
				z.HeatDDNum = c.HeatDDNum;
				z.CoolDDNum = c.CoolDDNum;

				z.DesHeatLoad = c.DesHeatLoad;
				z.DesHeatMassFlow = c.DesHeatMassFlow;
				z.ZoneTempAtHeatPeak = c.ZoneTempAtHeatPeak;
				z.OutTempAtHeatPeak = c.OutTempAtHeatPeak;
				z.ZoneRetTempAtHeatPeak = c.ZoneRetTempAtHeatPeak;
				z.ZoneHumRatAtHeatPeak = c.ZoneHumRatAtHeatPeak;
				z.OutHumRatAtHeatPeak = c.OutHumRatAtHeatPeak;
				z.TimeStepNumAtHeatMax = c.TimeStepNumAtHeatMax;
				z.DesHeatVolFlow = c.DesHeatVolFlow;
				z.DesHeatCoilInTemp = c.DesHeatCoilInTemp;
				z.DesHeatCoilInHumRat = c.DesHeatCoilInHumRat;

				z.DesCoolLoad = c.DesCoolLoad;
				z.DesCoolMassFlow = c.DesCoolMassFlow;
				z.ZoneTempAtCoolPeak = c.ZoneTempAtCoolPeak;
				z.OutTempAtCoolPeak = c.OutTempAtCoolPeak;
				z.ZoneRetTempAtCoolPeak = c.ZoneRetTempAtCoolPeak;
				z.ZoneHumRatAtCoolPeak = c.ZoneHumRatAtCoolPeak;
				z.OutHumRatAtCoolPeak = c.OutHumRatAtCoolPeak;
				z.TimeStepNumAtCoolMax = c.TimeStepNumAtCoolMax;
				z.DesCoolVolFlow = c.DesCoolVolFlow;
				z.DesCoolCoilInTemp = c.DesCoolCoilInTemp;
				z.DesCoolCoilInHumRat = c.DesCoolCoilInHumRat;
			}

			for ( DesDayNum = 1; DesDayNum <= TotDesDays + TotRunDesPersDays; ++DesDayNum ) {
				for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
					if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
					for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatLoadSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolLoadSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex );
						ZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = CalcZoneSizing( DesDayNum, CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex );
					}
				}
			}

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatLoadSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatLoadSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolLoadSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolLoadSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq( TimeStepIndex );
				}
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				// Now take into account the user specified sizing factor and user specified cooling design air flow
				// rate
				TotCoolSizMult = 0.0;
				// Calculate a sizing factor from the user specified cooling design air flow rate
				if ( FinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow > 0.0 && FinalZoneSizing( CtrlZoneNum ).CoolAirDesMethod == InpDesAirFlow && FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow > 0.0 ) {
					TotCoolSizMult = ( FinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow / FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow ) * FinalZoneSizing( CtrlZoneNum ).CoolSizingFactor;
					// If no user specified cooling design air flow rate input, use the user specified szing factor
				} else {
					TotCoolSizMult = FinalZoneSizing( CtrlZoneNum ).CoolSizingFactor;
				}
				// If the cooling sizing multiplier is not 1, adjust the cooling design data
				if ( std::abs( TotCoolSizMult - 1.0 ) > 0.00001 ) {
					if ( FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow > 0.0 ) {
						TimeStepAtPeak = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax;
						DDNum = FinalZoneSizing( CtrlZoneNum ).CoolDDNum;
						FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow * TotCoolSizMult;
						FinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow * TotCoolSizMult;
						FinalZoneSizing( CtrlZoneNum ).DesCoolLoad = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad * TotCoolSizMult;
						FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq = CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq * TotCoolSizMult;
						FinalZoneSizing( CtrlZoneNum ).CoolLoadSeq = CalcFinalZoneSizing( CtrlZoneNum ).CoolLoadSeq * TotCoolSizMult;
						OAFrac = FinalZoneSizing( CtrlZoneNum ).MinOA / FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow;
						OAFrac = min( 1.0, max( 0.0, OAFrac ) );
						FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTemp = OAFrac * DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak;
						FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRat = OAFrac * DesDayWeath( DDNum ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak;
					} else {
						FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = FinalZoneSizing( CtrlZoneNum ).InpDesCoolAirFlow;
						FinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow * FinalZoneSizing( CtrlZoneNum ).DesCoolDens;
					}
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						if ( ZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow > 0.0 ) {
							TimeStepAtPeak = ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtCoolMax;
							ZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow = CalcZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow * TotCoolSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).DesCoolMassFlow = CalcZoneSizing( DDNum, CtrlZoneNum ).DesCoolMassFlow * TotCoolSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).DesCoolLoad = CalcZoneSizing( DDNum, CtrlZoneNum ).DesCoolLoad * TotCoolSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).CoolFlowSeq = CalcZoneSizing( DDNum, CtrlZoneNum ).CoolFlowSeq * TotCoolSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).CoolLoadSeq = CalcZoneSizing( DDNum, CtrlZoneNum ).CoolLoadSeq * TotCoolSizMult;
							OAFrac = ZoneSizing( DDNum, CtrlZoneNum ).MinOA / ZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow;
							OAFrac = min( 1.0, max( 0.0, OAFrac ) );
							ZoneSizing( DDNum, CtrlZoneNum ).DesCoolCoilInTemp = OAFrac * DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( DDNum, CtrlZoneNum ).ZoneTempAtCoolPeak;
							ZoneSizing( DDNum, CtrlZoneNum ).DesCoolCoilInHumRat = OAFrac * DesDayWeath( DDNum ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( DDNum, CtrlZoneNum ).ZoneHumRatAtCoolPeak;
						} else {
							ZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow = ZoneSizing( DDNum, CtrlZoneNum ).InpDesCoolAirFlow;
							ZoneSizing( DDNum, CtrlZoneNum ).DesCoolMassFlow = ZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow * ZoneSizing( DDNum, CtrlZoneNum ).DesCoolDens;
						}
					}
				}
				// Now make sure that the design cooling air flow rates are greater than or equal to the specified minimums
				if ( FinalZoneSizing( CtrlZoneNum ).CoolAirDesMethod == DesAirFlowWithLim ) {
					MaxOfMinCoolVolFlow = max( FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow, FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2, FinalZoneSizing( CtrlZoneNum ).MinOA );
				} else {
					MaxOfMinCoolVolFlow = FinalZoneSizing( CtrlZoneNum ).MinOA;
				}
				MaxOfMinCoolMassFlow = MaxOfMinCoolVolFlow * FinalZoneSizing( CtrlZoneNum ).DesCoolDens;
				if ( MaxOfMinCoolVolFlow > FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow ) {
					FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = MaxOfMinCoolVolFlow;
					FinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = MaxOfMinCoolMassFlow;
				}
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					if ( MaxOfMinCoolMassFlow > FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) ) {
						FinalZoneSizing( CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = MaxOfMinCoolMassFlow;
					}
				}
				for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
					MaxOfMinCoolVolFlow = max( ZoneSizing( DDNum, CtrlZoneNum ).DesCoolMinAirFlow, ZoneSizing( DDNum, CtrlZoneNum ).DesCoolMinAirFlow, ZoneSizing( DDNum, CtrlZoneNum ).MinOA );
					MaxOfMinCoolMassFlow = MaxOfMinCoolVolFlow * ZoneSizing( DDNum, CtrlZoneNum ).DesCoolDens;
					if ( MaxOfMinCoolVolFlow > ZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow ) {
						ZoneSizing( DDNum, CtrlZoneNum ).DesCoolVolFlow = MaxOfMinCoolVolFlow;
						ZoneSizing( DDNum, CtrlZoneNum ).DesCoolMassFlow = MaxOfMinCoolMassFlow;
					}
					for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
						if ( MaxOfMinCoolMassFlow > ZoneSizing( DDNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) ) {
							ZoneSizing( DDNum, CtrlZoneNum ).CoolFlowSeq( TimeStepIndex ) = MaxOfMinCoolMassFlow;
						}
					}
				}
				// IF cooling flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
				// check for flow rate having been set (by MinOA or other min) but no timestep at max
				//        IF (FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow > 0.0d0 .AND. &
				if ( ( FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax == 0 || FinalZoneSizing( CtrlZoneNum ).CoolDDNum == 0 ) ) {
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtCoolMax = 1;
						TimeStepAtPeak = ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtCoolMax;
						for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
							if ( DesDayWeath( DDNum ).Temp( TimeStepIndex ) > DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) ) {
								TimeStepAtPeak = TimeStepIndex;
							}
						}
						ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtCoolMax = TimeStepAtPeak;
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = 1;
					FinalZoneSizing( CtrlZoneNum ).CoolDDNum = 1;
					TimeStepAtPeakF = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax;
					DDNumF = FinalZoneSizing( CtrlZoneNum ).CoolDDNum;
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						TimeStepAtPeak = ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtCoolMax;
						if ( DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) > DesDayWeath( DDNumF ).Temp( TimeStepAtPeakF ) ) {
							DDNumF = DDNum;
							TimeStepAtPeakF = TimeStepAtPeak;
						}
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = TimeStepAtPeakF;
					FinalZoneSizing( CtrlZoneNum ).CoolDDNum = DDNumF;
					FinalZoneSizing( CtrlZoneNum ).CoolDesDay = ZoneSizing( DDNumF, CtrlZoneNum ).CoolDesDay;

					// initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
					if ( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak == 0.0 ) {
						FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak = ZoneSizing( DDNumF, CtrlZoneNum ).DesCoolSetPtSeq( TimeStepAtPeakF );
						FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = ZoneSizing( DDNumF, CtrlZoneNum ).CoolZoneHumRatSeq( TimeStepAtPeakF );
						if ( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak > 0.0 ) {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = min( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak, PsyWFnTdpPb( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak, StdBaroPress, RoutineName ) );

						} else {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = ZoneSizing( DDNumF, CtrlZoneNum ).CoolDesHumRat;
						}
						FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTemp = FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak;
						FinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRat = FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak;
						FinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtCoolPeak = FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak;
					}
				}
				// Now take into account the user specified sizing factor or user specified heating design air flow
				// rate (which overrides the sizing factor)
				TotHeatSizMult = 0.0;
				// Calculate a sizing factor from the user specified heating design air flow rate
				if ( FinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow > 0.0 && FinalZoneSizing( CtrlZoneNum ).HeatAirDesMethod == InpDesAirFlow && FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow > 0.0 ) {
					TotHeatSizMult = ( FinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow / FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow ) * FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor;
					// Calculate a sizing factor from the user specified max heating design air flow rates
				} else if ( FinalZoneSizing( CtrlZoneNum ).HeatAirDesMethod == DesAirFlowWithLim && FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow > 0.0 ) {
					MaxHeatVolFlow = max( FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow, FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow * FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowFrac );
					if ( MaxHeatVolFlow < FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow ) {
						TotHeatSizMult = ( MaxHeatVolFlow / FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow ) * FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor;
					} else {
						TotHeatSizMult = FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor;
					}
					// If no user specified heating design air flow rate input, use the user specified sizing factor
				} else {
					TotHeatSizMult = FinalZoneSizing( CtrlZoneNum ).HeatSizingFactor;
				}

				if ( std::abs( TotHeatSizMult - 1.0 ) > 0.00001 ) {
					if ( FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow > 0.0 ) {
						TimeStepAtPeak = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax;
						DDNum = FinalZoneSizing( CtrlZoneNum ).HeatDDNum;
						FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow * TotHeatSizMult;
						FinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow * TotHeatSizMult;
						FinalZoneSizing( CtrlZoneNum ).DesHeatLoad = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad * TotHeatSizMult;
						FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq = CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq * TotHeatSizMult;
						FinalZoneSizing( CtrlZoneNum ).HeatLoadSeq = CalcFinalZoneSizing( CtrlZoneNum ).HeatLoadSeq * TotHeatSizMult;
						OAFrac = FinalZoneSizing( CtrlZoneNum ).MinOA / FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow;
						OAFrac = min( 1.0, max( 0.0, OAFrac ) );
						FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTemp = OAFrac * DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak;
						FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRat = OAFrac * DesDayWeath( DDNum ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak;
					} else {
						FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = FinalZoneSizing( CtrlZoneNum ).InpDesHeatAirFlow;
						FinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow * FinalZoneSizing( CtrlZoneNum ).DesHeatDens;
					}
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						if ( ZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow > 0.0 ) {
							TimeStepAtPeak = ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtHeatMax;
							ZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow = CalcZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow * TotHeatSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).DesHeatMassFlow = CalcZoneSizing( DDNum, CtrlZoneNum ).DesHeatMassFlow * TotHeatSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).DesHeatLoad = CalcZoneSizing( DDNum, CtrlZoneNum ).DesHeatLoad * TotHeatSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).HeatFlowSeq = CalcZoneSizing( DDNum, CtrlZoneNum ).HeatFlowSeq * TotHeatSizMult;
							ZoneSizing( DDNum, CtrlZoneNum ).HeatLoadSeq = CalcZoneSizing( DDNum, CtrlZoneNum ).HeatLoadSeq * TotHeatSizMult;
							OAFrac = ZoneSizing( DDNum, CtrlZoneNum ).MinOA / ZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow;
							OAFrac = min( 1.0, max( 0.0, OAFrac ) );
							ZoneSizing( DDNum, CtrlZoneNum ).DesHeatCoilInTemp = OAFrac * DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( DDNum, CtrlZoneNum ).ZoneTempAtHeatPeak;
							ZoneSizing( DDNum, CtrlZoneNum ).DesHeatCoilInHumRat = OAFrac * DesDayWeath( DDNum ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( DDNum, CtrlZoneNum ).ZoneHumRatAtHeatPeak;
						} else {
							ZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow = ZoneSizing( DDNum, CtrlZoneNum ).InpDesHeatAirFlow;
							ZoneSizing( DDNum, CtrlZoneNum ).DesHeatMassFlow = ZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow * ZoneSizing( DDNum, CtrlZoneNum ).DesHeatDens;
						}
					}
				}
				MinOAMass = FinalZoneSizing( CtrlZoneNum ).MinOA * FinalZoneSizing( CtrlZoneNum ).DesHeatDens;
				if ( FinalZoneSizing( CtrlZoneNum ).MinOA > FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow ) {
					FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = FinalZoneSizing( CtrlZoneNum ).MinOA;
					FinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = MinOAMass;
				}
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					if ( MinOAMass > FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) ) {
						FinalZoneSizing( CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = MinOAMass;
					}
				}
				for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
					MinOAMass = ZoneSizing( DDNum, CtrlZoneNum ).MinOA * ZoneSizing( DDNum, CtrlZoneNum ).DesHeatDens;
					if ( ZoneSizing( DDNum, CtrlZoneNum ).MinOA > ZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow ) {
						ZoneSizing( DDNum, CtrlZoneNum ).DesHeatVolFlow = ZoneSizing( DDNum, CtrlZoneNum ).MinOA;
						ZoneSizing( DDNum, CtrlZoneNum ).DesHeatMassFlow = MinOAMass;
					}
					for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
						if ( MinOAMass > ZoneSizing( DDNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) ) {
							ZoneSizing( DDNum, CtrlZoneNum ).HeatFlowSeq( TimeStepIndex ) = MinOAMass;
						}
					}
				}
				// IF heating flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
				// check for flow rate having been set (by MinOA or other min) but no timestep at max
				//        IF (FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow > 0.0d0 .AND. &
				if ( ( FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax == 0 || FinalZoneSizing( CtrlZoneNum ).HeatDDNum == 0 ) ) {
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtHeatMax = 1;
						TimeStepAtPeak = ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtHeatMax;
						for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
							if ( DesDayWeath( DDNum ).Temp( TimeStepIndex ) < DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) ) {
								TimeStepAtPeak = TimeStepIndex;
							}
						}
						ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtHeatMax = TimeStepAtPeak;
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = 1;
					FinalZoneSizing( CtrlZoneNum ).HeatDDNum = 1;
					TimeStepAtPeakF = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax;
					DDNumF = FinalZoneSizing( CtrlZoneNum ).HeatDDNum;
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						TimeStepAtPeak = ZoneSizing( DDNum, CtrlZoneNum ).TimeStepNumAtHeatMax;
						if ( DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) < DesDayWeath( DDNumF ).Temp( TimeStepAtPeakF ) ) {
							DDNumF = DDNum;
							TimeStepAtPeakF = TimeStepAtPeak;
						}
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = TimeStepAtPeakF;
					FinalZoneSizing( CtrlZoneNum ).HeatDDNum = DDNumF;
					FinalZoneSizing( CtrlZoneNum ).HeatDesDay = ZoneSizing( DDNumF, CtrlZoneNum ).HeatDesDay;

					// initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
					if ( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak == 0.0 ) {
						FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak = ZoneSizing( DDNumF, CtrlZoneNum ).DesHeatSetPtSeq( TimeStepAtPeakF );
						FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = ZoneSizing( DDNumF, CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepAtPeakF );
						if ( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak > 0.0 ) {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = min( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak, PsyWFnTdpPb( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak, StdBaroPress, RoutineName ) );
						} else {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = ZoneSizing( DDNumF, CtrlZoneNum ).HeatDesHumRat;
						}
						FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTemp = FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak;
						FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRat = FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak;
						FinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtHeatPeak = FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak;
					}
				}

				// set the zone minimum cooling supply air flow rate. This will be used for autosizing VAV terminal unit
				// minimum flow rates
				FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin = max( FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow, FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow * FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlowFrac );
				// set the zone maximum heating supply air flow rate. This will be used for autosizing VAV terminal unit
				// max heating flow rates
				FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlowMax = max( FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow, FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2, FinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow * FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlowFrac );
				// Determine the design cooling supply air temperature if the supply air temperature difference is specified by user.
				if ( FinalZoneSizing( CtrlZoneNum ).ZnCoolDgnSAMethod == TemperatureDifference ) {
					FinalZoneSizing( CtrlZoneNum ).CoolDesTemp = FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak - std::abs( FinalZoneSizing( CtrlZoneNum ).CoolDesTempDiff );
				}
				// Determine the design heating supply air temperature if the supply air temperature difference is specified by user.
				if ( FinalZoneSizing( CtrlZoneNum ).ZnHeatDgnSAMethod == TemperatureDifference ) {
					FinalZoneSizing( CtrlZoneNum ).HeatDesTemp = FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak + std::abs( FinalZoneSizing( CtrlZoneNum ).HeatDesTempDiff );
				}
			}

		}}

		TermUnitFinalZoneSizing = FinalZoneSizing;

	}

	void
	SimZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimAir
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       Raustad/Shirey, FSEC, June 2003
		//       MODIFIED       Gu, FSEC, Jan. 2004, Don Shirey, Aug 2009 (LatOutputProvided)
		//                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is responsible for determining
		// how much of each type of energy every zone requires.
		// In effect, this subroutine defines and simulates all
		// the system types and in the case of hybrid systems
		// which use more than one type of energy must determine
		// how to apportion the load. An example of a hybrid system
		// is a water loop heat pump with supplemental air.  In
		// this case, a zone will require water from the loop and
		// cooled or heated air from the air system. A simpler
		// example would be a VAV system with baseboard heaters

		// METHODOLOGY EMPLOYED:
		// 1.  Determine zone load - this is zone temperature dependent
		// 2.  Determine balance point - the temperature at which the
		//     zone load is balanced by the system output. The way the
		//     balance point is determined will be different depending on
		//     the type of system being simulated.
		// 3.  Calculate zone energy requirements

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataHVACGlobals;
		using DataHeatBalFanSys::NonAirSystemResponse;
		using DataHeatBalFanSys::SysDepZoneLoads;
		using ReturnAirPathManager::SimReturnAirPath;
		using ZoneAirLoopEquipmentManager::ManageZoneAirLoopEquipment;
		using PurchasedAirManager::SimPurchasedAir;
		using DirectAirManager::SimDirectAir;
		using HWBaseboardRadiator::SimHWBaseboard;
		using SteamBaseboardRadiator::SimSteamBaseboard;
		using BaseboardRadiator::SimBaseboard;
		using BaseboardElectric::SimElectricBaseboard;
		using SplitterComponent::SimAirLoopSplitter;
		using FanCoilUnits::SimFanCoilUnit;
		using Fans::SimulateFanComponents;
		using WindowAC::SimWindowAC;
		using PackagedTerminalHeatPump::SimPackagedTerminalUnit;
		using ZoneDehumidifier::SimZoneDehumidifier;
		using UnitVentilator::SimUnitVentilator;
		using UnitHeater::SimUnitHeater;
		using HeatRecovery::SimHeatRecovery;
		using OutdoorAirUnit::SimOutdoorAirUnit;
		using HVACStandAloneERV::SimStandAloneERV;
		using LowTempRadiantSystem::SimLowTempRadiantSystem;
		using HighTempRadiantSystem::SimHighTempRadiantSystem;
		using VentilatedSlab::SimVentilatedSlab;
		using ZonePlenum::SimAirZonePlenum;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkFanActivated;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;
		using WaterThermalTanks::SimHeatPumpWaterHeater;
		using DataAirSystems::PrimaryAirSystem;
		using ElectricBaseboardRadiator::SimElecBaseboard;
		using HVACVariableRefrigerantFlow::SimulateVRF;
		using RefrigeratedCase::SimAirChillerSet;
		using UserDefinedComponents::SimZoneAirUserDefined;
		using SystemAvailabilityManager::GetZoneEqAvailabilityManager;
		using DataGlobals::isPulseZoneSizing;
		using EvaporativeCoolers::SimZoneEvaporativeCoolerUnit;
		using HVACUnitarySystem::SimUnitarySystem;
		using DataHeatBalance::ZoneAirMassFlow;
		using SwimmingPool::SimSwimmingPool;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ActualZoneNum;
		int ControlledZoneNum;
		int EquipTypeNum;
		int SupplyAirPathNum;
		int CompNum;
		int EquipPtr;
		int AirLoopNum;
		int ZoneEquipTypeNum;
		int ZoneCompNum;

		static bool SupPathInletChanged( false );
		static bool FirstCall; // indicates first call to supply air path components
		static bool MyOneTimeFlag( true );
		bool ErrorFlag;
		static bool ValidSAMComp( false );

		Real64 SysOutputProvided; // sensible output delivered by zone equipment (W)
		Real64 LatOutputProvided; // latent output delivered by zone equipment (kg/s)
		Real64 AirSysOutput;
		Real64 NonAirSysOutput;
		static bool ZoneHasAirLoopHVACTerminal( false ); // true if zone has an air loop terminal
		static bool ZoneHasAirLoopHVACDirectAir( false ); // true if zone has an uncontrolled air loop terminal
		static Array1D_bool DirectAirAndAirTerminalWarningIssued; // only warn once for each zone with problems

		// Determine flow rate and temperature of supply air based on type of damper

		bool AdjustZoneMassFlowFlag( true );  // holds zone mixing and infiltration flow calc status
		FirstCall = true;
		ErrorFlag = false;

		for ( SupplyAirPathNum = 1; SupplyAirPathNum <= NumSupplyAirPaths; ++SupplyAirPathNum ) {

			for ( CompNum = 1; CompNum <= SupplyAirPath( SupplyAirPathNum ).NumOfComponents; ++CompNum ) {
				{ auto const SELECT_CASE_var( SupplyAirPath( SupplyAirPathNum ).ComponentType_Num( CompNum ) );

				if ( SELECT_CASE_var == ZoneSplitter_Type ) { // 'AirLoopHVAC:ZoneSplitter'

					if ( ! ( AirflowNetworkFanActivated && SimulateAirflowNetwork > AirflowNetworkControlMultizone ) ) {
						SimAirLoopSplitter( SupplyAirPath( SupplyAirPathNum ).ComponentName( CompNum ), FirstHVACIteration, FirstCall, SupPathInletChanged, SupplyAirPath( SupplyAirPathNum ).ComponentIndex( CompNum ) );
					}

				} else if ( SELECT_CASE_var == ZoneSupplyPlenum_Type ) { // 'AirLoopHVAC:SupplyPlenum'

					SimAirZonePlenum( SupplyAirPath( SupplyAirPathNum ).ComponentName( CompNum ), ZoneSupplyPlenum_Type, SupplyAirPath( SupplyAirPathNum ).ComponentIndex( CompNum ), FirstHVACIteration, FirstCall, SupPathInletChanged );

				} else {
					ShowSevereError( "Error found in Supply Air Path=" + SupplyAirPath( SupplyAirPathNum ).Name );
					ShowContinueError( "Invalid Supply Air Path Component=" + SupplyAirPath( SupplyAirPathNum ).ComponentType( CompNum ) );
					ShowFatalError( "Preceding condition causes termination." );

				}}
			}

		}

		if ( FirstCall && ! allocated( DirectAirAndAirTerminalWarningIssued ) ) {
			DirectAirAndAirTerminalWarningIssued.dimension( NumOfZones, false );
		}

		FirstCall = false;

		// Simulate all of the pools. These have a potential impact on surface heat balances, zone air heat balances, and moisture balances.
		// These should be simulated first so that any systems or zone equipment devices deal with the effects of the pool properly.
		SimSwimmingPool( FirstHVACIteration );

		// Loop over all the primary air loop; simulate their components (equipment)
		// and controllers

		if ( ZoneAirMassFlow.EnforceZoneMassBalance ) {
			CalcAirFlowSimple( 0, AdjustZoneMassFlowFlag );
		}

		for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {

			if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
			ActualZoneNum = ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum;

			NonAirSystemResponse( ActualZoneNum ) = 0.0;
			SysDepZoneLoads( ActualZoneNum ) = 0.0;
			ZoneEquipConfig( ControlledZoneNum ).ZoneExh = 0.0;
			ZoneEquipConfig( ControlledZoneNum ).ZoneExhBalanced = 0.0;
			ZoneEquipConfig( ControlledZoneNum ).PlenumMassFlow = 0.0;
			ZoneHasAirLoopHVACTerminal = false;
			ZoneHasAirLoopHVACDirectAir = false;
			CurZoneEqNum = ControlledZoneNum;

			InitSystemOutputRequired( ActualZoneNum, SysOutputProvided, LatOutputProvided );

			SetZoneEquipSimOrder( ControlledZoneNum, ActualZoneNum );

			// Air loop system availability manager status only applies to PIU and exhaust fans
			// Reset fan SAM operation flags for zone fans.
			TurnFansOn = false;
			TurnFansOff = false;

			for ( EquipTypeNum = 1; EquipTypeNum <= ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes; ++EquipTypeNum ) {

				UnbalExhMassFlow = 0.0;
				BalancedExhMassFlow = 0.0;
				PlenumInducedMassFlow = 0.0;
				EquipPtr = PrioritySimOrder( EquipTypeNum ).EquipPtr;
				SysOutputProvided = 0.0;
				LatOutputProvided = 0.0;
				DataCoolCoilCap = 0.0; // reset global variable used only for heat pumps (i.e., DX cooling and heating coils)

				// Reset ZoneEqSizing data (because these may change from one equipment type to the next)
				if ( FirstPassZoneEquipFlag ) {
					ZoneEqSizing( ControlledZoneNum ).AirVolFlow = 0.0;
					ZoneEqSizing( ControlledZoneNum ).MaxHWVolFlow = 0.0;
					ZoneEqSizing( ControlledZoneNum ).MaxCWVolFlow = 0.0;
					ZoneEqSizing( ControlledZoneNum ).OAVolFlow = 0.0;
					ZoneEqSizing( ControlledZoneNum ).DesCoolingLoad = 0.0;
					ZoneEqSizing( ControlledZoneNum ).DesHeatingLoad = 0.0;
					ZoneEqSizing( ControlledZoneNum ).CoolingAirVolFlow = 0.0;
					ZoneEqSizing( ControlledZoneNum ).HeatingAirVolFlow = 0.0;
					ZoneEqSizing( ControlledZoneNum ).SystemAirVolFlow = 0.0;
					ZoneEqSizing( ControlledZoneNum ).AirFlow = false;
					ZoneEqSizing( ControlledZoneNum ).CoolingAirFlow = false;
					ZoneEqSizing( ControlledZoneNum ).HeatingAirFlow = false;
					ZoneEqSizing( ControlledZoneNum ).SystemAirFlow = false;
					ZoneEqSizing( ControlledZoneNum ).Capacity = false;
					ZoneEqSizing( ControlledZoneNum ).CoolingCapacity = false;
					ZoneEqSizing( ControlledZoneNum ).HeatingCapacity = false;
					ZoneEqSizing( ControlledZoneNum ).SystemCapacity = false;
					ZoneEqSizing( ControlledZoneNum ).DesignSizeFromParent = false;
				}

				ZoneEquipTypeNum = PrioritySimOrder( EquipTypeNum ).EquipType_Num;

				ZoneCompNum = ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr );

				ValidSAMComp = false;

				if ( ZoneEquipTypeNum <= NumValidSysAvailZoneComponents ) ValidSAMComp = true;

				if ( ZoneCompNum > 0 && ValidSAMComp ) {

					GetZoneEqAvailabilityManager( ZoneEquipTypeNum, ZoneCompNum, ErrorFlag );

					if ( ZoneComp( ZoneEquipTypeNum ).ZoneCompAvailMgrs( ZoneCompNum ).AvailStatus == CycleOn ) {
						ZoneCompTurnFansOn = true;
						ZoneCompTurnFansOff = false;
					} else if ( ZoneComp( ZoneEquipTypeNum ).ZoneCompAvailMgrs( ZoneCompNum ).AvailStatus == ForceOff ) {
						ZoneCompTurnFansOn = false;
						ZoneCompTurnFansOff = true;
					} else {
						ZoneCompTurnFansOn = TurnFansOn;
						ZoneCompTurnFansOff = TurnFansOff;
					}
				} else {
					ZoneCompTurnFansOn = TurnFansOn;
					ZoneCompTurnFansOff = TurnFansOff;
				}

				{ auto const SELECT_CASE_var( ZoneEquipTypeNum );

				if ( SELECT_CASE_var == AirDistUnit_Num ) { // 'ZoneHVAC:AirDistributionUnit'

					// Air loop system availability manager status only applies to PIU and exhaust fans
					// Check to see if System Availability Managers are asking for fans to cycle on or shut off
					// and set fan on/off flags accordingly.
					if ( ZoneEquipAvail( ControlledZoneNum ) == CycleOn || ZoneEquipAvail( ControlledZoneNum ) == CycleOnZoneFansOnly ) {
						TurnFansOn = true;
					}
					if ( ZoneEquipAvail( ControlledZoneNum ) == ForceOff ) {
						TurnFansOff = true;
					}

					ManageZoneAirLoopEquipment( PrioritySimOrder( EquipTypeNum ).EquipName, FirstHVACIteration, AirSysOutput, NonAirSysOutput, LatOutputProvided, ActualZoneNum, ControlledZoneNum, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					//            reset status flags for other zone equipment
					TurnFansOn = false;
					TurnFansOff = false;

					NonAirSystemResponse( ActualZoneNum ) += NonAirSysOutput;
					SysOutputProvided = NonAirSysOutput + AirSysOutput;
					ZoneHasAirLoopHVACTerminal = true;
				} else if ( SELECT_CASE_var == DirectAir_Num ) { // 'AirTerminal:SingleDuct:Uncontrolled'
					SimDirectAir( PrioritySimOrder( EquipTypeNum ).EquipName, ControlledZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );
					ZoneHasAirLoopHVACDirectAir = true;
				} else if ( SELECT_CASE_var == VRFTerminalUnit_Num ) { // 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
					SimulateVRF( PrioritySimOrder( EquipTypeNum ).EquipName, ControlledZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == WindowAC_Num ) { // 'ZoneHVAC:WindowAirConditioner'
					SimWindowAC( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) || ( SELECT_CASE_var == PkgTermACAirToAir_Num ) || ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) ) { // 'ZoneHVAC:PackagedTerminalHeatPump'
					// 'ZoneHVAC:PackagedTerminalAirConditioner'
					// 'ZoneHVAC:WaterToAirHeatPump'
					SimPackagedTerminalUnit( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipTypeNum, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == ZoneUnitarySystem_Num ) { // 'AirloopHVAC:UnitarySystem'
					SimUnitarySystem( PrioritySimOrder( EquipTypeNum ).EquipName, FirstHVACIteration, ActualZoneNum, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ), _, _, _, _, true );

				} else if ( SELECT_CASE_var == ZoneDXDehumidifier_Num ) { // 'ZoneHVAC:Dehumidifier:DX'
					SimZoneDehumidifier( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					SysDepZoneLoads( ActualZoneNum ) += SysOutputProvided;

					SysOutputProvided = 0.0; // Reset to 0.0 since this equipment is controlled based on zone humidity level (not
					// temperature) SysOutputProvided amount was already sent above to
					// next Predict-Correct series of calcs via SysDepZoneLoads

				} else if ( SELECT_CASE_var == FanCoil4Pipe_Num ) { // 'ZoneHVAC:FourPipeFanCoil'
					SimFanCoilUnit( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == UnitVentilator_Num ) { // 'ZoneHVAC:UnitVentilator'
					SimUnitVentilator( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == UnitHeater_Num ) { // 'ZoneHVAC:UnitHeater'
					SimUnitHeater( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == PurchasedAir_Num ) { // 'ZoneHVAC:IdealLoadsAirSystem'
					SimPurchasedAir( PrioritySimOrder( EquipTypeNum ).EquipName, SysOutputProvided, LatOutputProvided, FirstHVACIteration, ControlledZoneNum, ActualZoneNum, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == BBWater_Num ) { // 'ZoneHVAC:Baseboard:RadiantConvective:Water'
					SimHWBaseboard( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration, SysOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					NonAirSystemResponse( ActualZoneNum ) += SysOutputProvided;
					LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat

				} else if ( SELECT_CASE_var == BBSteam_Num ) { // 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
					SimSteamBaseboard( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration, SysOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					NonAirSystemResponse( ActualZoneNum ) += SysOutputProvided;
					LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat

				} else if ( SELECT_CASE_var == BBWaterConvective_Num ) { // 'ZoneHVAC:Baseboard:Convective:Water'
					SimBaseboard( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration, SysOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					NonAirSystemResponse( ActualZoneNum ) += SysOutputProvided;
					LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat

				} else if ( SELECT_CASE_var == BBElectricConvective_Num ) { // 'ZoneHVAC:Baseboard:Convective:Electric'
					SimElectricBaseboard( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, ControlledZoneNum, SysOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					NonAirSystemResponse( ActualZoneNum ) += SysOutputProvided;
					LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat

				} else if ( SELECT_CASE_var == HiTempRadiant_Num ) { // 'ZoneHVAC:HighTemperatureRadiant'
					SimHighTempRadiantSystem( PrioritySimOrder( EquipTypeNum ).EquipName, FirstHVACIteration, SysOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );
					LatOutputProvided = 0.0; // This baseboard currently sends its latent heat gain directly to predictor/corrector
					// via SumLatentHTRadSys... so setting LatOutputProvided = 0.0

				} else if ( SELECT_CASE_var == LoTempRadiant_Num ) { // 'ZoneHVAC:LowTemperatureRadiant:VariableFlow', 'ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
					// 'ZoneHVAC:LowTemperatureRadiant:Electric'
					SimLowTempRadiantSystem( PrioritySimOrder( EquipTypeNum ).EquipName, FirstHVACIteration, SysOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );
					LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat

				} else if ( SELECT_CASE_var == ZoneExhaustFan_Num ) { // 'Fan:ZoneExhaust'

					// Air loop system availability manager status only applies to PIU and exhaust fans
					// Check to see if System Availability Managers are asking for fans to cycle on or shut off
					// and set fan on/off flags accordingly.
					if ( ZoneEquipAvail( ControlledZoneNum ) == CycleOn || ZoneEquipAvail( ControlledZoneNum ) == CycleOnZoneFansOnly ) {
						TurnFansOn = true;
					}
					if ( ZoneEquipAvail( ControlledZoneNum ) == ForceOff ) {
						TurnFansOff = true;
					}

					SimulateFanComponents( PrioritySimOrder( EquipTypeNum ).EquipName, FirstHVACIteration, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					//            reset status flags for other zone equipment
					TurnFansOn = false;
					TurnFansOff = false;

				} else if ( SELECT_CASE_var == HeatXchngr_Num ) { // 'HeatExchanger:AirToAir:FlatPlate'
					SimHeatRecovery( PrioritySimOrder( EquipTypeNum ).EquipName, FirstHVACIteration, ZoneEquipList( ControlledZoneNum ).EquipIndex( EquipPtr ), ContFanCycCoil );

				} else if ( SELECT_CASE_var == ERVStandAlone_Num ) { // 'ZoneHVAC:EnergyRecoveryVentilator'
					SimStandAloneERV( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( ControlledZoneNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == HPWaterHeater_Num ) { // 'WaterHeater:HeatPump:PumpedCondenser'
					SimHeatPumpWaterHeater( PrioritySimOrder( EquipTypeNum ).EquipName, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( ControlledZoneNum ).EquipIndex( EquipPtr ) );
				} else if ( SELECT_CASE_var == VentilatedSlab_Num ) { // 'ZoneHVAC:VentilatedSlab'
					SimVentilatedSlab( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );
				} else if ( SELECT_CASE_var == OutdoorAirUnit_Num ) { // 'ZoneHVAC:OutdoorAirUnit'
					SimOutdoorAirUnit( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == BBElectric_Num ) { // 'ZoneHVAC:Baseboard:RadiantConvective:Electric'
					SimElecBaseboard( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration, SysOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					NonAirSystemResponse( ActualZoneNum ) += SysOutputProvided;
					LatOutputProvided = 0.0; // This baseboard does not add/remove any latent heat

				} else if ( SELECT_CASE_var == RefrigerationAirChillerSet_Num ) { // 'ZoneHVAC:RefrigerationChillerSet'
					SimAirChillerSet( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

					NonAirSystemResponse( ActualZoneNum ) += SysOutputProvided;

				} else if ( SELECT_CASE_var == UserDefinedZoneHVACForcedAir_Num ) {
					SimZoneAirUserDefined( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else if ( SELECT_CASE_var == ZoneEvaporativeCoolerUnit_Num ) {
					SimZoneEvaporativeCoolerUnit( PrioritySimOrder( EquipTypeNum ).EquipName, ActualZoneNum, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

				} else {

				}}

				ZoneEquipConfig( ControlledZoneNum ).ZoneExh += ( UnbalExhMassFlow + BalancedExhMassFlow ); // This is the total "exhaust" flow from equipment such as a zone exhaust fan
				ZoneEquipConfig( ControlledZoneNum ).ZoneExhBalanced += BalancedExhMassFlow;
				ZoneEquipConfig( ControlledZoneNum ).PlenumMassFlow += PlenumInducedMassFlow;

				UpdateSystemOutputRequired( ActualZoneNum, SysOutputProvided, LatOutputProvided, EquipTypeNum );

				if ( ZoneHasAirLoopHVACTerminal && ZoneHasAirLoopHVACDirectAir ) {
					// zone has both AirTerminal:SingleDuct:Uncontrolled and another kind of Air terminal unit which is not supported
					if ( ! DirectAirAndAirTerminalWarningIssued( ActualZoneNum ) ) {
						ShowSevereError( "In zone \"" + ZoneEquipConfig( ControlledZoneNum ).ZoneName + "\" there are too many air terminals served by AirLoopHVAC systems." );
						ShowContinueError( "A single zone cannot have both an AirTerminal:SingleDuct:Uncontrolled and also a second AirTerminal:* object." );

						DirectAirAndAirTerminalWarningIssued( ActualZoneNum ) = true;
						ErrorFlag = true;
					}
				}
			} // zone loop

			AirLoopNum = ZoneEquipConfig( ControlledZoneNum ).AirLoopNum;
			if ( AirLoopInit ) {
				if ( AirLoopNum > 0 ) {
					if ( ! PrimaryAirSystem( AirLoopNum ).OASysExists ) {
						if ( ZoneEquipConfig( ControlledZoneNum ).ZoneExh > 0.0 && ! ZoneEquipConfig( ControlledZoneNum ).FlowError && AirLoopsSimOnce ) {
							if ( !isPulseZoneSizing && !ZoneAirMassFlow.EnforceZoneMassBalance ) {
								ShowWarningError( "In zone " + ZoneEquipConfig( ControlledZoneNum ).ZoneName + " there is unbalanced exhaust air flow." );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "  Unless there is balancing infiltration / ventilation air flow, this will result in" );
								ShowContinueError( "  load due to induced outdoor air being neglected in the simulation." );
								ZoneEquipConfig( ControlledZoneNum ).FlowError = true;
							}
						}
						// ZoneEquipConfig(ControlledZoneNum)%ZoneExh = 0.0
					}
				} else {
					if ( ZoneEquipConfig( ControlledZoneNum ).ZoneExh > 0.0 && ! ZoneEquipConfig( ControlledZoneNum ).FlowError && AirLoopsSimOnce ) {
						if ( !isPulseZoneSizing && !ZoneAirMassFlow.EnforceZoneMassBalance ) {
							ShowWarningError( "In zone " + ZoneEquipConfig( ControlledZoneNum ).ZoneName + " there is unbalanced exhaust air flow." );
							ShowContinueErrorTimeStamp( "" );
							ShowContinueError( "  Unless there is balancing infiltration / ventilation air flow, this will result in" );
							ShowContinueError( "  load due to induced outdoor air being neglected in the simulation." );
							ZoneEquipConfig( ControlledZoneNum ).FlowError = true;
						}
					}
					// ZoneEquipConfig(ControlledZoneNum)%ZoneExh = 0.0
				}
			}
		} // End of controlled zone loop
		CurZoneEqNum = 0;
		FirstPassZoneEquipFlag = false;

		//This is the call to the Supply Air Path after the components are simulated to update
		//  the path inlets

		// Process supply air path components in reverse order
		for ( SupplyAirPathNum = 1; SupplyAirPathNum <= NumSupplyAirPaths; ++SupplyAirPathNum ) {

			SupPathInletChanged = false;

			for ( CompNum = SupplyAirPath( SupplyAirPathNum ).NumOfComponents; CompNum >= 1; --CompNum ) {
				{ auto const SELECT_CASE_var( SupplyAirPath( SupplyAirPathNum ).ComponentType_Num( CompNum ) );

				if ( SELECT_CASE_var == ZoneSplitter_Type ) { // 'AirLoopHVAC:ZoneSplitter'

					if ( ! ( AirflowNetworkFanActivated && SimulateAirflowNetwork > AirflowNetworkControlMultizone ) ) {
						SimAirLoopSplitter( SupplyAirPath( SupplyAirPathNum ).ComponentName( CompNum ), FirstHVACIteration, FirstCall, SupPathInletChanged, SupplyAirPath( SupplyAirPathNum ).ComponentIndex( CompNum ) );
					}

				} else if ( SELECT_CASE_var == ZoneSupplyPlenum_Type ) { // 'AirLoopHVAC:SupplyPlenum'

					SimAirZonePlenum( SupplyAirPath( SupplyAirPathNum ).ComponentName( CompNum ), ZoneSupplyPlenum_Type, SupplyAirPath( SupplyAirPathNum ).ComponentIndex( CompNum ), FirstHVACIteration, FirstCall, SupPathInletChanged );

				} else {
					ShowSevereError( "Error found in Supply Air Path=" + SupplyAirPath( SupplyAirPathNum ).Name );
					ShowContinueError( "Invalid Supply Air Path Component=" + SupplyAirPath( SupplyAirPathNum ).ComponentType( CompNum ) );
					ShowFatalError( "Preceding condition causes termination." );

				}}
			}

			if ( SupPathInletChanged ) {
				// If the supply air path inlet conditions have been changed, the Air Loop must be resimulated
				SimAir = true;
			}

		} // end of the Supply Air Path DO Loop

		CalcZoneMassBalance();

		CalcZoneLeavingConditions();

		SimReturnAirPath();

		if ( MyOneTimeFlag ) {
			for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
				if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
				if ( ZoneEquipConfig( ControlledZoneNum ).SupLeakToRetPlen && ZoneEquipConfig( ControlledZoneNum ).ReturnZonePlenumCondNum == 0 ) {
					ShowSevereError( "No return plenum for simple duct leakage model for Zone " + ZoneEquipConfig( ControlledZoneNum ).ZoneName );
					ShowContinueError( "  The simple duct leakage model requires plenum return for all zone with leaks" );
					ErrorFlag = true;
				}
			}
			if ( ErrorFlag ) {
				ShowFatalError( "Preceding condition causes termination" );
			}
			MyOneTimeFlag = false;
		}

	}

	void
	SetZoneEquipSimOrder(
		int const ControlledZoneNum,
		int const ActualZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set simulation priorities based on user specified priorities and
		// required conditions (heating or cooling).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
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
		int CurEqHeatingPriority; // Used to make sure "optimization features" on compilers don't defeat purpose of this routine
		int CurEqCoolingPriority; // Used to make sure "optimization features" on compilers don't defeat purpose of this routine

		auto const & zeq( ZoneEquipList( ControlledZoneNum ) );
		int const NumOfEquipTypes( zeq.NumOfEquipTypes );
		for ( int EquipTypeNum = 1; EquipTypeNum <= NumOfEquipTypes; ++EquipTypeNum ) {
			auto & pso( PrioritySimOrder( EquipTypeNum ) );
			pso.EquipType = zeq.EquipType( EquipTypeNum );
			pso.EquipName = zeq.EquipName( EquipTypeNum );
			pso.EquipType_Num = zeq.EquipType_Num( EquipTypeNum );
			pso.CoolingPriority = zeq.CoolingPriority( EquipTypeNum );
			pso.HeatingPriority = zeq.HeatingPriority( EquipTypeNum );
			pso.EquipPtr = DefaultSimOrder( EquipTypeNum );
		}
		for ( int EquipTypeNum = NumOfEquipTypes + 1, EquipTypeNum_end = PrioritySimOrder.u(); EquipTypeNum <= EquipTypeNum_end; ++EquipTypeNum ) { // Reset unused upper array portion
			auto & pso( PrioritySimOrder( EquipTypeNum ) );
			pso.EquipType.clear();
			pso.EquipName.clear();
			pso.EquipType_Num = 0;
			pso.EquipPtr = 0;
		}

		for ( int EquipTypeNum = 1; EquipTypeNum <= NumOfEquipTypes; ++EquipTypeNum ) {
			auto & pso( PrioritySimOrder( EquipTypeNum ) );

			CurEqHeatingPriority = pso.HeatingPriority;
			CurEqCoolingPriority = pso.CoolingPriority;

			for ( int ComparedEquipTypeNum = EquipTypeNum; ComparedEquipTypeNum <= NumOfEquipTypes; ++ComparedEquipTypeNum ) {
				auto & psc( PrioritySimOrder( ComparedEquipTypeNum ) );

				if ( ( CurEqCoolingPriority > psc.CoolingPriority && ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired < 0.0 ) || ( CurEqHeatingPriority > psc.HeatingPriority && ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired >= 0.0 ) ) {

					//Tuned C++ string swap avoids copying
					pso.EquipType.swap( psc.EquipType );
					pso.EquipName.swap( psc.EquipName );
					std::swap( pso.EquipPtr, psc.EquipPtr );
					std::swap( pso.EquipType_Num, psc.EquipType_Num );
					std::swap( pso.CoolingPriority, psc.CoolingPriority );
					std::swap( pso.HeatingPriority, psc.HeatingPriority );

					CurEqCoolingPriority = pso.CoolingPriority;
					CurEqHeatingPriority = pso.HeatingPriority;

				}

			}

		}

	}

	void
	InitSystemOutputRequired(
		int const ZoneNum,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       Don Shirey, Aug 2009 (latent/moisture additions)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize remaining output required variables

		// METHODOLOGY EMPLOYED:
		// Initialize remaining output variables using predictor calculations

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::DeadBandOrSetback;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;

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

		ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired;
		ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToHeatingSP;
		ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToCoolingSP;
		//init each sequenced demand to the full output
		if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequired ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequired = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired; // array assignment
		if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToHeatingSP ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToHeatingSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToHeatingSP; // array assignment
		if ( allocated( ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToCoolingSP ) ) ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToCoolingSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToCoolingSP; // array assignment

		ZoneSysMoistureDemand( ZoneNum ).RemainingOutputRequired = ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired;
		ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToHumidSP = ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToHumidifyingSP;
		ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToDehumidSP = ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToDehumidifyingSP;
		//init each sequenced demand to the full output
		if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequired ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequired = ZoneSysMoistureDemand( ZoneNum ).TotalOutputRequired; // array assignment
		if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToHumidSP ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToHumidSP = ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToHumidifyingSP; // array assignment
		if ( allocated( ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToDehumidSP ) ) ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToDehumidSP = ZoneSysMoistureDemand( ZoneNum ).OutputRequiredToDehumidifyingSP; // array assignment

		SysOutputProvided = 0.0; // sensible provided by a piece of zone equipment
		LatOutputProvided = 0.0; // latent provided by a piece of zone equipment

		CurDeadBandOrSetback( ZoneNum ) = DeadBandOrSetback( ZoneNum );

	}

	void
	UpdateSystemOutputRequired(
		int const ZoneNum,
		Real64 const SysOutputProvided, // sensible output provided by zone equipment (W)
		Real64 const LatOutputProvided, // latent output provided by zone equipment (kg/s)
		Optional_int_const EquipPriorityNum // index in PrioritySimOrder for this update
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Unknown
		//       MODIFIED       B. Griffith Sept 2011, add storage of requirements by sequence
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::DeadBandOrSetback;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using DataHVACGlobals::SingleHeatingSetPoint;
		using DataHVACGlobals::SingleCoolingSetPoint;
		using DataHVACGlobals::SingleHeatCoolSetPoint;
		using DataHVACGlobals::DualSetPointWithDeadBand;
		using DataHeatBalFanSys::TempControlType;

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

		// Determine flow rate and temperature of supply air based on type of damper

		// Sensible output updates
		ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired -= SysOutputProvided;
		ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP -= SysOutputProvided;
		ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP -= SysOutputProvided;
		// Latent output updates
		ZoneSysMoistureDemand( ZoneNum ).RemainingOutputRequired -= LatOutputProvided;
		ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToHumidSP -= LatOutputProvided;
		ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToDehumidSP -= LatOutputProvided;

		// re-evaluate if loads are now such that in dead band or set back
		{ auto const SELECT_CASE_var( TempControlType( ZoneNum ) );
		if ( SELECT_CASE_var == 0 ) { // uncontrolled zone; shouldn't ever get here, but who knows
			CurDeadBandOrSetback( ZoneNum ) = false;
		} else if ( SELECT_CASE_var == SingleHeatingSetPoint ) {
			if ( ( ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired - 1.0 ) < 0.0 ) {
				CurDeadBandOrSetback( ZoneNum ) = true;
			} else {
				CurDeadBandOrSetback( ZoneNum ) = false;
			}
		} else if ( SELECT_CASE_var == SingleCoolingSetPoint ) {
			if ( ( ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired + 1.0 ) > 0.0 ) {
				CurDeadBandOrSetback( ZoneNum ) = true;
			} else {
				CurDeadBandOrSetback( ZoneNum ) = false;
			}
		} else if ( SELECT_CASE_var == SingleHeatCoolSetPoint ) {
			if ( ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP < 0.0 && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP > 0.0 ) {
				CurDeadBandOrSetback( ZoneNum ) = true;
			} else {
				CurDeadBandOrSetback( ZoneNum ) = false;
			}
		} else if ( SELECT_CASE_var == DualSetPointWithDeadBand ) {
			if ( ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP < 0.0 && ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP > 0.0 ) {
				CurDeadBandOrSetback( ZoneNum ) = true;
			} else {
				CurDeadBandOrSetback( ZoneNum ) = false;
			}
		}}

		if ( present( EquipPriorityNum ) ) {
			//now store remaining load at the by sequence level
			if ( EquipPriorityNum + 1 <= ZoneSysEnergyDemand( ZoneNum ).NumZoneEquipment ) {
				ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequired( EquipPriorityNum + 1 ) = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired;
				ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequired( EquipPriorityNum + 1 ) = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputRequired;
			}

			if ( PrioritySimOrder( EquipPriorityNum ).HeatingPriority + 1 <= ZoneSysEnergyDemand( ZoneNum ).NumZoneEquipment ) {
				ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToHeatingSP( PrioritySimOrder( EquipPriorityNum ).HeatingPriority + 1 ) = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
				ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToHumidSP( PrioritySimOrder( EquipPriorityNum ).HeatingPriority + 1 ) = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToHumidSP;
			}
			if ( PrioritySimOrder( EquipPriorityNum ).CoolingPriority + 1 <= ZoneSysEnergyDemand( ZoneNum ).NumZoneEquipment ) {
				ZoneSysEnergyDemand( ZoneNum ).SequencedOutputRequiredToCoolingSP( PrioritySimOrder( EquipPriorityNum ).CoolingPriority + 1 ) = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
				ZoneSysMoistureDemand( ZoneNum ).SequencedOutputRequiredToDehumidSP( PrioritySimOrder( EquipPriorityNum ).CoolingPriority + 1 ) = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToDehumidSP;
			}
		}

	}

	void
	CalcZoneMassBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Perform zone mass balance to get outlet air flow conditions.

		// METHODOLOGY EMPLOYED:
		// Mass continuity equation.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataAirLoop::AirLoopFlow;
		using namespace DataRoomAirModel; // UCSD
		using DataHVACGlobals::NumPrimaryAirSys;
		using DataHVACGlobals::AirLoopsSimOnce;
		using DataAirSystems::PrimaryAirSystem;
		using DataAirflowNetwork::AirflowNetworkNumOfExhFan;
		using DataGlobals::isPulseZoneSizing;
		using DataGlobals::DoingSizing;

		using DataHeatBalance::Zone;
		using DataHeatBalance::MassConservation;
		using DataHeatBalance::Infiltration;
		using DataHeatBalance::ZoneAirMassFlow;
		using DataHeatBalance::AddInfiltrationFlow;
		using DataHeatBalance::AdjustInfiltrationFlow;
		using DataHeatBalance::NoInfiltrationFlow;
		using DataHeatBalance::AllZones;
		using DataHeatBalFanSys::ZoneMassBalanceFlag;
		using DataHeatBalFanSys::ZoneInfiltrationFlag;
		using DataHeatBalFanSys::MixingMassFlowZone;
		using DataHeatBalFanSys::ZoneReOrder;
		using DataHVACGlobals::ZoneMassBalanceHVACReSim;
		using DataHVACGlobals::SmallMassFlow;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const IterMax( 25 );
		Real64 const ConvergenceTolerance( 0.000010 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		int NodeNum;
		int RetNode; // return air node number
		int ZoneNode; // zone air node number
		int AirLoopNum;
		Real64 TotInletAirMassFlowRate;
		Real64 TotInletAirMassFlowRateMax;
		Real64 TotInletAirMassFlowRateMaxAvail;
		Real64 TotInletAirMassFlowRateMin;
		Real64 TotInletAirMassFlowRateMinAvail;
		Real64 TotExhaustAirMassFlowRate;
		Real64 TotSupplyAirMassFlowRate;

		Real64 ZoneMixingAirMassFlowRate;
		Real64 ZoneMixingNetAirMassFlowRate;
		Real64 ZoneMixMassFlowRate;
		Real64 ZoneMixingAirMassFlowRatePrevious;
		Real64 ZoneReturnAirMassFlowRate;
		Real64 ZoneInfiltrationMassFlowRate;
		Real64 BuildingZoneMixingFlowOld;
		Real64 BuildingZoneMixingFlow;
		Real64 StdReturnNodeMassFlow;
		Real64 UserReturnNodeMassFlow;
		Real64 UnbalancedExhaustDelta;
		int Iteration;
		int ZoneNum1;

		ZoneMassBalanceHVACReSim = false;
		Iteration = 0;
		BuildingZoneMixingFlow = 0.0;
		BuildingZoneMixingFlowOld = 0.0;

		do
		{
			if (ZoneAirMassFlow.EnforceZoneMassBalance) {
				for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
					if (!ZoneEquipConfig(ZoneNum).IsControlled) continue;
					if (ZoneEquipConfig(ZoneNum).AirLoopNum > 0) {
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).ZoneExhaust = 0.0;
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).ZoneExhaustBalanced = 0.0;
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).SupFlow = 0.0;
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).RetFlow = 0.0;
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).RetFlow0 = 0.0;
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).RecircFlow = 0.0;
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).ZoneMixingFlow = 0.0;
						AirLoopFlow(ZoneEquipConfig(ZoneNum).AirLoopNum).RetFlowAdjustment = 0.0;
					}
					ZoneInfiltrationFlag(ZoneNum) = false;
					MassConservation(ZoneNum).IncludeInfilToZoneMassBal = 0;
				}
			}
			BuildingZoneMixingFlowOld = BuildingZoneMixingFlow;
			BuildingZoneMixingFlow = 0.0;

			for ( ZoneNum1 = 1; ZoneNum1 <= NumOfZones; ++ZoneNum1 ) {
				ZoneNum = ZoneNum1;
				if ( ZoneAirMassFlow.EnforceZoneMassBalance ) ZoneNum = ZoneReOrder( ZoneNum1 );

				if ( !ZoneEquipConfig(ZoneNum).IsControlled ) continue;

				TotInletAirMassFlowRate = 0.0;
				TotInletAirMassFlowRateMax = 0.0;
				TotInletAirMassFlowRateMaxAvail = 0.0;
				TotInletAirMassFlowRateMin = 0.0;
				TotInletAirMassFlowRateMinAvail = 0.0;
				TotExhaustAirMassFlowRate = 0.0;

				ZoneMixingAirMassFlowRate = 0.0;
				ZoneMixingNetAirMassFlowRate = 0.0;
				ZoneMixMassFlowRate = 0.0;
				ZoneReturnAirMassFlowRate = 0.0;
				ZoneInfiltrationMassFlowRate = 0.0;
				ZoneMixingAirMassFlowRatePrevious = 0.0;

				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneNum ).NumInletNodes; ++NodeNum ) {
					TotInletAirMassFlowRate += Node( ZoneEquipConfig( ZoneNum ).InletNode( NodeNum) ).MassFlowRate;
					TotInletAirMassFlowRateMax += Node( ZoneEquipConfig( ZoneNum ).InletNode( NodeNum) ).MassFlowRateMax;
					TotInletAirMassFlowRateMaxAvail += Node( ZoneEquipConfig( ZoneNum ).InletNode( NodeNum) ).MassFlowRateMaxAvail;
					TotInletAirMassFlowRateMin += Node( ZoneEquipConfig( ZoneNum ).InletNode( NodeNum)).MassFlowRateMin;
					TotInletAirMassFlowRateMinAvail += Node( ZoneEquipConfig( ZoneNum ).InletNode( NodeNum ) ).MassFlowRateMinAvail;
				}

				for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumExhaustNodes; ++NodeNum) {

					if (AirflowNetworkNumOfExhFan == 0) TotExhaustAirMassFlowRate += Node(ZoneEquipConfig(ZoneNum).ExhaustNode(NodeNum)).MassFlowRate;

				}


				//
				// Include zone mixing mass flow rate
				if ( ZoneMassBalanceFlag( ZoneNum ) ) {
					RetNode = ZoneEquipConfig( ZoneNum ).ReturnAirNode;
					if ( RetNode > 0 ) {
						ZoneReturnAirMassFlowRate = Node( RetNode ).MassFlowRate;
					}
					// Set zone mixing incoming mass flow rate
					if ( (Iteration == 0) || ! ZoneAirMassFlow.BalanceMixing ){
						ZoneMixingAirMassFlowRate = MixingMassFlowZone( ZoneNum );
					} else {
						ZoneMixingAirMassFlowRate = max( 0.0, ZoneReturnAirMassFlowRate + TotExhaustAirMassFlowRate - TotInletAirMassFlowRate + MassConservation( ZoneNum ).MixingSourceMassFlowRate );
					}
					CalcZoneMixingFlowRateOfReceivingZone(ZoneNum, ZoneMixingAirMassFlowRate);
				}

				AirLoopNum = ZoneEquipConfig( ZoneNum ).AirLoopNum;
				ZoneNode = ZoneEquipConfig( ZoneNum ).ZoneNode;
				Node( ZoneNode ).MassFlowRate = TotInletAirMassFlowRate;
				Node( ZoneNode ).MassFlowRateMax = TotInletAirMassFlowRateMax;
				Node( ZoneNode ).MassFlowRateMaxAvail = TotInletAirMassFlowRateMaxAvail;
				Node( ZoneNode ).MassFlowRateMin = TotInletAirMassFlowRateMin;
				Node( ZoneNode ).MassFlowRateMinAvail = TotInletAirMassFlowRateMinAvail;

				// Update Return Air Node Conditions; If one Exists
				RetNode = ZoneEquipConfig( ZoneNum ).ReturnAirNode;
				UserReturnNodeMassFlow = 0.0;
				StdReturnNodeMassFlow = 0.0;
				if ( RetNode > 0 ) {
					// Calculate standard return air flow rate using default method of inlets minus exhausts adjusted for "balanced" exhuast flow
					StdReturnNodeMassFlow = max( 0.0, ( Node( ZoneNode ).MassFlowRate + ZoneMixingNetAirMassFlowRate - ( TotExhaustAirMassFlowRate - ZoneEquipConfig( ZoneNum ).ZoneExhBalanced ) ) );
					if ( AirLoopNum > 0 ) {
						if ( !PrimaryAirSystem( AirLoopNum ).OASysExists ) {
							StdReturnNodeMassFlow = max( 0.0, ( Node( ZoneNode ).MassFlowRate + ZoneMixingNetAirMassFlowRate - ( TotExhaustAirMassFlowRate - ZoneEquipConfig( ZoneNum ).ZoneExh ) ) );
						}
					}

					// Make no return air flow adjustments during sizing
					if ( DoingSizing || isPulseZoneSizing ) {
						UserReturnNodeMassFlow = StdReturnNodeMassFlow;
					} else {
						if ( ZoneEquipConfig( ZoneNum ).NumReturnFlowBasisNodes > 0 ) {
							// Set base return air flow rate using basis node flow rates
							for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneNum ).NumReturnFlowBasisNodes; ++NodeNum ) {
								UserReturnNodeMassFlow += ZoneEquipConfig( ZoneNum ).ReturnFlowBasisNode( NodeNum );
							}
							UserReturnNodeMassFlow = max( 0.0, ( UserReturnNodeMassFlow * GetCurrentScheduleValue( ZoneEquipConfig( ZoneNum ).ReturnFlowSchedPtrNum ) ) );
						} else {
							UserReturnNodeMassFlow = max( 0.0, ( StdReturnNodeMassFlow * GetCurrentScheduleValue( ZoneEquipConfig( ZoneNum ).ReturnFlowSchedPtrNum ) ) );
						}
					}
					Node( RetNode ).MassFlowRate = UserReturnNodeMassFlow;
					MassConservation( ZoneNum ).RetMassFlowRate = Node( RetNode ).MassFlowRate;
					Node( RetNode ).MassFlowRateMax = Node( ZoneNode ).MassFlowRateMax;
					Node( RetNode ).MassFlowRateMin = Node( ZoneNode ).MassFlowRateMin;
					Node( RetNode ).MassFlowRateMaxAvail = Node( ZoneNode ).MassFlowRateMaxAvail;
					Node( RetNode ).MassFlowRateMinAvail = 0.0;
				}

				// Set zone infiltration flow rate
				if ( ZoneAirMassFlow.InfiltrationTreatment != NoInfiltrationFlow ) {
					if ( MassConservation( ZoneNum ).InfiltrationPtr > 0 ) {
						if ( MassConservation( ZoneNum ).IsOnlySourceZone || ( ZoneAirMassFlow.InfiltrationZoneType == AllZones ) ) {
							ZoneInfiltrationMassFlowRate = MassConservation( ZoneNum ).MixingSourceMassFlowRate + TotExhaustAirMassFlowRate + ZoneReturnAirMassFlowRate - TotInletAirMassFlowRate;
							if (ZoneAirMassFlow.InfiltrationTreatment == AdjustInfiltrationFlow) {
								if (abs(ZoneInfiltrationMassFlowRate) > ConvergenceTolerance) {
									ZoneInfiltrationFlag(ZoneNum) = true;
									MassConservation(ZoneNum).InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
									MassConservation( ZoneNum ).IncludeInfilToZoneMassBal = 1;
									Infiltration( MassConservation( ZoneNum ).InfiltrationPtr ).MassFlowRate = ZoneInfiltrationMassFlowRate;
									Infiltration( MassConservation( ZoneNum ).InfiltrationPtr ).MassFlowRate = max( 0.0, Infiltration( MassConservation( ZoneNum ).InfiltrationPtr ).MassFlowRate );
								} else {
									MassConservation(ZoneNum).InfiltrationMassFlowRate = Infiltration(MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate;
								}
							} else if (ZoneAirMassFlow.InfiltrationTreatment == AddInfiltrationFlow) {
								if (ZoneInfiltrationMassFlowRate > ConvergenceTolerance) {
									ZoneInfiltrationFlag(ZoneNum) = true;
									MassConservation(ZoneNum).InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
									MassConservation(ZoneNum).IncludeInfilToZoneMassBal = 1;
									Infiltration( MassConservation( ZoneNum ).InfiltrationPtr ).MassFlowRate += ZoneInfiltrationMassFlowRate;
								} else {
									MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
								}
							} else if ( ZoneAirMassFlow.InfiltrationTreatment == NoInfiltrationFlow ) {
								MassConservation( ZoneNum ).InfiltrationMassFlowRate = 0.0;
							}
						} else {
							if (ZoneAirMassFlow.InfiltrationTreatment == AdjustInfiltrationFlow) {
								MassConservation(ZoneNum).InfiltrationMassFlowRate = Infiltration(MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate;
							} else if (ZoneAirMassFlow.InfiltrationTreatment == AddInfiltrationFlow) {
								MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
							} else if ( ZoneAirMassFlow.InfiltrationTreatment == NoInfiltrationFlow ) {
								MassConservation( ZoneNum ).InfiltrationMassFlowRate = 0.0;
							}
						}
					} else {
							// Zone has no infiltration objects
							MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
					}

					MassConservation(ZoneNum).InMassFlowRate = TotInletAirMassFlowRate;
					MassConservation(ZoneNum).ExhMassFlowRate = TotExhaustAirMassFlowRate;
					ZoneMixingNetAirMassFlowRate = MassConservation(ZoneNum).MixingMassFlowRate - MassConservation(ZoneNum).MixingSourceMassFlowRate;
				}
				//

				TotSupplyAirMassFlowRate = TotInletAirMassFlowRate - (TotExhaustAirMassFlowRate - ZoneEquipConfig(ZoneNum).ZoneExh) - ZoneEquipConfig(ZoneNum).PlenumMassFlow;

				if ( AirLoopNum > 0 ) {
					AirLoopFlow(AirLoopNum).ZoneExhaust += ZoneEquipConfig(ZoneNum).ZoneExh;
					AirLoopFlow(AirLoopNum).ZoneExhaustBalanced += ZoneEquipConfig(ZoneNum).ZoneExhBalanced;
					AirLoopFlow(AirLoopNum).SupFlow += TotSupplyAirMassFlowRate;
					AirLoopFlow(AirLoopNum).RetFlow0 += Node(RetNode).MassFlowRate;
					AirLoopFlow(AirLoopNum).RecircFlow += ZoneEquipConfig(ZoneNum).PlenumMassFlow;
					AirLoopFlow(AirLoopNum).RetFlowAdjustment += ( UserReturnNodeMassFlow - StdReturnNodeMassFlow );
				}
				BuildingZoneMixingFlow += MassConservation(ZoneNum).MixingMassFlowRate;
			}
			// Calculate an air loop return air flow rate
			for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
				UnbalancedExhaustDelta = (AirLoopFlow(AirLoopNum).ZoneExhaust - AirLoopFlow(AirLoopNum).RetFlowAdjustment) - (AirLoopFlow(AirLoopNum).SupFlow + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced);
				UnbalancedExhaustDelta = max(UnbalancedExhaustDelta, (AirLoopFlow(AirLoopNum).ZoneExhaust - AirLoopFlow(AirLoopNum).RetFlowAdjustment) - (AirLoopFlow(AirLoopNum).MaxOutAir + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced) );
				if ( (UnbalancedExhaustDelta > SmallMassFlow) && !AirLoopFlow( AirLoopNum ).FlowError && AirLoopsSimOnce ) {
					if ( !isPulseZoneSizing && !ZoneAirMassFlow.EnforceZoneMassBalance ) {
						ShowWarningError( "In AirLoopHVAC " + PrimaryAirSystem(AirLoopNum).Name + " there is unbalanced exhaust air flow." );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "  Unless there is balancing infiltration / ventilation air flow, this will result in" );
						ShowContinueError( "  load due to induced outdoor air being neglected in the simulation." );
						AirLoopFlow(AirLoopNum).FlowError = true;
					}
				}
				AirLoopFlow(AirLoopNum).ZoneExhaust = min(AirLoopFlow(AirLoopNum).ZoneExhaust, (AirLoopFlow(AirLoopNum).SupFlow + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced + AirLoopFlow(AirLoopNum).ZoneMixingFlow));
				AirLoopFlow(AirLoopNum).ZoneExhaust = min(AirLoopFlow(AirLoopNum).ZoneExhaust, (AirLoopFlow(AirLoopNum).MaxOutAir + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced + AirLoopFlow(AirLoopNum).ZoneMixingFlow));

				if ( AirLoopFlow(AirLoopNum).ZoneMixingFlow < 0.0 ) {
					// the source zone and the recieving zone are in different air loops
					AirLoopFlow(AirLoopNum).ZoneExhaust = max(0.0, (AirLoopFlow(AirLoopNum).ZoneExhaust - AirLoopFlow(AirLoopNum).ZoneMixingFlow));
					AirLoopFlow( AirLoopNum ).RetFlow = max(0.0, AirLoopFlow( AirLoopNum ).SupFlow - ( AirLoopFlow( AirLoopNum ).ZoneExhaust - AirLoopFlow( AirLoopNum ).ZoneExhaustBalanced - AirLoopFlow( AirLoopNum ).RetFlowAdjustment) + AirLoopFlow( AirLoopNum ).RecircFlow );
				} else {
					AirLoopFlow( AirLoopNum ).RetFlow = max(0.0, AirLoopFlow( AirLoopNum ).SupFlow - ( AirLoopFlow( AirLoopNum ).ZoneExhaust - AirLoopFlow( AirLoopNum ).ZoneExhaustBalanced - AirLoopFlow( AirLoopNum ).RetFlowAdjustment) + AirLoopFlow( AirLoopNum ).RecircFlow + AirLoopFlow( AirLoopNum ).ZoneMixingFlow);
				}
			}

			// adjust the zone return air flow rates to match the air loop return air flow rate
//			if ( ! ZoneAirMassFlow.EnforceZoneMassBalance ) {
			for ( ZoneNum1 = 1; ZoneNum1 <= NumOfZones; ++ZoneNum1 ) {
				ZoneNum = ZoneNum1;
				if (!ZoneEquipConfig(ZoneNum).IsControlled) continue;
				RetNode = ZoneEquipConfig(ZoneNum).ReturnAirNode;
				AirLoopNum = ZoneEquipConfig(ZoneNum).AirLoopNum;
				if (AirLoopNum > 0 && RetNode > 0) {
					if (PrimaryAirSystem(AirLoopNum).OASysExists) {
						if (AirLoopFlow(AirLoopNum).RetFlow0 > 0.0) {
							Node(RetNode).MassFlowRate *= (AirLoopFlow(AirLoopNum).RetFlow / AirLoopFlow(AirLoopNum).RetFlow0);
						} else {
							Node(RetNode).MassFlowRate = 0.0;
						}
					}
				}
				//       IF (AirLoopNum == 0 .AND. RetNode > 0) THEN
				//         ! sometimes models for ZoneHVAC have input a return node, but no air loop HVAC.
				//         ! this block was tried but caused problems such as UA coil sizing issues and water coil controller problems
				//         !  CR 7967, no air loop HVAC, but there is a return air node that never gets used or set
				//         Node(RetNode)%MassFlowRate = 0.0d0
				//       ENDIF
//				}
			}
			// update the
			if ( Iteration > 0 ) {
				if ( abs(BuildingZoneMixingFlow - BuildingZoneMixingFlowOld) < ConvergenceTolerance ) {
					ZoneMassBalanceHVACReSim = false;
					break;
				} else {
					ZoneMassBalanceHVACReSim = true;
				}
			}
			if ( !ZoneAirMassFlow.EnforceZoneMassBalance ) break;
			Iteration += 1;

		} while ( Iteration < IterMax );

	}

	void
	CalcZoneLeavingConditions()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   January 2001
		//       MODIFIED       June 2003, FCW: add heat from airflow window to return air
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Perform zone upate of the leaving conditions.

		// METHODOLOGY EMPLOYED:
		// Energy Balance.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataHeatBalance::RefrigCaseCredit;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::SysDepZoneLoads;
		using DataHeatBalFanSys::ZoneLatentGain;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceWindow;
		using DataSurfaces::AirFlowWindow_Destination_ReturnAir;
		using DataEnvironment::OutBaroPress;
		using DataRoomAirModel::AirPatternZoneInfo;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::DeadBandOrSetback;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using DataContaminantBalance::Contaminant;
		using InternalHeatGains::SumAllReturnAirConvectionGains;
		using InternalHeatGains::SumAllReturnAirLatentGains;
		using DataHVACGlobals::RetTempMax;
		using DataHVACGlobals::RetTempMin;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 QRetAir; // Heat to return air from lights
		Real64 CpAir; // Air heat capacity [J/kg-K]
		Real64 TempRetAir; // Return air temperature [C]
		Real64 TempZoneAir; // Zone air temperature [C]
		int ZoneNum; // Controlled zone number
		int ActualZoneNum; // Zone number
		int ZoneNode; // Node number of controlled zone
		int ReturnNode; // Node number of controlled zone's return air
		int SurfNum; // Surface number
		Real64 MassFlowRA; // Return air mass flow [kg/s]
		Real64 FlowThisTS; // Window gap air mass flow [kg/s]
		Real64 WinGapFlowToRA; // Mass flow to return air from all airflow windows in zone [kg/s]
		Real64 WinGapFlowTtoRA; // Sum of mass flow times outlet temp for all airflow windows in zone [(kg/s)-C]
		Real64 WinGapTtoRA; // Temp of outlet flow mixture to return air from all airflow windows in zone [C]
		Real64 H2OHtOfVap; // Heat of vaporization of water (W/kg)
		Real64 ZoneMult; // zone multiplier
		Real64 SumRetAirLatentGainRate;

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ! ZoneEquipConfig( ZoneNum ).IsControlled ) continue;
			ActualZoneNum = ZoneEquipConfig( ZoneNum ).ActualZoneNum;
			//A return air system may not exist for certain systems; Therefore when no return node exits
			// there is no update.  OF course if there is no return air system then you cannot update
			// the energy for the return air heat gain from the lights statements.
			ReturnNode = ZoneEquipConfig( ZoneNum ).ReturnAirNode;
			ZoneNode = ZoneEquipConfig( ZoneNum ).ZoneNode;
			ZoneMult = Zone( ActualZoneNum ).Multiplier * Zone( ActualZoneNum ).ListMultiplier;
			if ( ReturnNode > 0 ) {

				//RETURN AIR HEAT GAIN from the Lights statement; this heat gain is stored in
				// Add sensible heat gain from refrigerated cases with under case returns
				SumAllReturnAirConvectionGains( ActualZoneNum, QRetAir );

				// Need to add the energy to the return air from lights and from airflow windows. Where the heat
				// is added depends on if there is system flow or not.  If there is system flow the heat is added
				// to the Zone Return Node.  If there is no system flow then the heat is added back to the zone in the
				// Correct step through the SysDepZoneLoads variable.

				MassFlowRA = Node( ReturnNode ).MassFlowRate / ZoneMult;

				// user defined room air model may feed temp that differs from zone node
				if ( allocated( AirPatternZoneInfo ) ) {
					if ( ( AirPatternZoneInfo( ActualZoneNum ).IsUsed ) && ( ! BeginEnvrnFlag ) ) {
						TempZoneAir = AirPatternZoneInfo( ActualZoneNum ).Tleaving;
						TempRetAir = TempZoneAir;
					} else {
						TempZoneAir = Node( ZoneNode ).Temp;
						TempRetAir = TempZoneAir;
					}
				} else {
					TempZoneAir = Node( ZoneNode ).Temp;
					TempRetAir = TempZoneAir;
				}

				WinGapFlowToRA = 0.0;
				WinGapTtoRA = 0.0;
				WinGapFlowTtoRA = 0.0;

				for ( SurfNum = Zone( ActualZoneNum ).SurfaceFirst; SurfNum <= Zone( ActualZoneNum ).SurfaceLast; ++SurfNum ) {
					if ( SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 && SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_ReturnAir ) {
						FlowThisTS = PsyRhoAirFnPbTdbW( OutBaroPress, SurfaceWindow( SurfNum ).TAirflowGapOutlet, Node( ZoneNode ).HumRat ) * SurfaceWindow( SurfNum ).AirflowThisTS * Surface( SurfNum ).Width;
						WinGapFlowToRA += FlowThisTS;
						WinGapFlowTtoRA += FlowThisTS * SurfaceWindow( SurfNum ).TAirflowGapOutlet;
					}
				}
				if ( WinGapFlowToRA > 0.0 ) WinGapTtoRA = WinGapFlowTtoRA / WinGapFlowToRA;
				// the flag NoHeatToReturnAir is TRUE if the system is zonal only or is central with on/off air flow. In these
				// cases the heat to return air is treated as a zone heat gain and dealt with in CalcZoneSums in
				// MODULE ZoneTempPredictorCorrector.
				if ( ! Zone( ActualZoneNum ).NoHeatToReturnAir ) {
					CpAir = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, Node( ZoneNode ).Temp );
					if ( MassFlowRA > 0.0 ) {
						if ( WinGapFlowToRA > 0.0 ) {
							// Add heat-to-return from window gap airflow
							if ( MassFlowRA >= WinGapFlowToRA ) {
								TempRetAir = ( WinGapFlowTtoRA + ( MassFlowRA - WinGapFlowToRA ) * TempZoneAir ) / MassFlowRA;
							} else {
								// All of return air comes from flow through airflow windows
								TempRetAir = WinGapTtoRA;
								// Put heat from window airflow that exceeds return air flow into zone air
								SysDepZoneLoads( ActualZoneNum ) += ( WinGapFlowToRA - MassFlowRA ) * CpAir * ( WinGapTtoRA - TempZoneAir );
							}
						}
						// Add heat-to-return from lights
						TempRetAir += QRetAir / ( MassFlowRA * CpAir );
						if ( TempRetAir > RetTempMax ) {
							Node( ReturnNode ).Temp = RetTempMax;
							if ( ! ZoneSizingCalc ) {
								SysDepZoneLoads( ActualZoneNum ) += CpAir * MassFlowRA * ( TempRetAir - RetTempMax );
							}
						} else if ( TempRetAir < RetTempMin ) {
							Node( ReturnNode ).Temp = RetTempMin;
							if ( ! ZoneSizingCalc ) {
								SysDepZoneLoads( ActualZoneNum ) += CpAir * MassFlowRA * ( TempRetAir - RetTempMin );
							}
						} else {
							Node( ReturnNode ).Temp = TempRetAir;
						}
					} else { // No return air flow
						// Assign all heat-to-return from window gap airflow to zone air
						if ( WinGapFlowToRA > 0.0 ) SysDepZoneLoads( ActualZoneNum ) += WinGapFlowToRA * CpAir * ( WinGapTtoRA - TempZoneAir );
						// Assign all heat-to-return from lights to zone air
						if ( QRetAir > 0.0 ) SysDepZoneLoads( ActualZoneNum ) += QRetAir;
						Node( ReturnNode ).Temp = Node( ZoneNode ).Temp;
					}
				} else {
					// update the return air node for zonal and central on/off systems
					Node( ReturnNode ).Temp = Node( ZoneNode ).Temp;
				}

				// Update the rest of the Return Air Node conditions, if the return air system exists!
				Node( ReturnNode ).Press = Node( ZoneNode ).Press;

				// Include impact of under case returns for refrigerated display case when updating the return air node humidity
				if ( ! Zone( ActualZoneNum ).NoHeatToReturnAir ) {
					if ( MassFlowRA > 0 ) {
						SumAllReturnAirLatentGains( ZoneNum, SumRetAirLatentGainRate );
						H2OHtOfVap = PsyHgAirFnWTdb( Node( ZoneNode ).HumRat, Node( ReturnNode ).Temp );
						Node( ReturnNode ).HumRat = Node( ZoneNode ).HumRat + ( SumRetAirLatentGainRate / ( H2OHtOfVap * MassFlowRA ) );
					} else {
						// If no mass flow rate exists, include the latent HVAC case credit with the latent Zone case credit
						Node( ReturnNode ).HumRat = Node( ZoneNode ).HumRat;
						RefrigCaseCredit( ActualZoneNum ).LatCaseCreditToZone += RefrigCaseCredit( ActualZoneNum ).LatCaseCreditToHVAC;
						// shouldn't the HVAC term be zeroed out then?
						SumAllReturnAirLatentGains( ZoneNum, SumRetAirLatentGainRate );
						ZoneLatentGain( ActualZoneNum ) += SumRetAirLatentGainRate;

					}
				} else {
					Node( ReturnNode ).HumRat = Node( ZoneNode ).HumRat;
					RefrigCaseCredit( ActualZoneNum ).LatCaseCreditToZone += RefrigCaseCredit( ActualZoneNum ).LatCaseCreditToHVAC;
					// shouldn't the HVAC term be zeroed out then?
					SumAllReturnAirLatentGains( ZoneNum, SumRetAirLatentGainRate );
					ZoneLatentGain( ActualZoneNum ) += SumRetAirLatentGainRate;
				}

				Node( ReturnNode ).Enthalpy = PsyHFnTdbW( Node( ReturnNode ).Temp, Node( ReturnNode ).HumRat );

				if ( Contaminant.CO2Simulation ) Node( ReturnNode ).CO2 = Node( ZoneNode ).CO2;
				if ( Contaminant.GenericContamSimulation ) Node( ReturnNode ).GenContam = Node( ZoneNode ).GenContam;

			} //End of check for a return air node, which implies a return air system.

			// Reset current deadband flags, remaining output required, so no impact beyond zone equipment
			ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired = ZoneSysEnergyDemand( ActualZoneNum ).TotalOutputRequired;
			ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToHeatSP = ZoneSysEnergyDemand( ActualZoneNum ).OutputRequiredToHeatingSP;
			ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToCoolSP = ZoneSysEnergyDemand( ActualZoneNum ).OutputRequiredToCoolingSP;

			ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputRequired = ZoneSysMoistureDemand( ActualZoneNum ).TotalOutputRequired;
			ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToHumidSP = ZoneSysMoistureDemand( ActualZoneNum ).OutputRequiredToHumidifyingSP;
			ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToDehumidSP = ZoneSysMoistureDemand( ActualZoneNum ).OutputRequiredToDehumidifyingSP;

			CurDeadBandOrSetback( ActualZoneNum ) = DeadBandOrSetback( ActualZoneNum );

		}

	}

	void
	UpdateZoneEquipment( bool & SimAir )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine performs the update for Zone Equipment Management.
		// Specifically, it transfers the conditions from the zone equipment return air nodes across
		// to the air loop side, allowing for multiple return air nodes

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using HVACInterfaceManager::UpdateHVACInterface;
		using DataAirLoop::AirToZoneNodeInfo;
		using DataHVACGlobals::NumPrimaryAirSys;
		using DataConvergParams::CalledFromAirSystemDemandSide;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneGroupNum;
		int RetAirPathNum;

		// Transfer the conditions from the zone equipment return air nodes across
		// to the air loop side, allowing for multiple return air nodes
		for ( ZoneGroupNum = 1; ZoneGroupNum <= NumPrimaryAirSys; ++ZoneGroupNum ) {
			for ( RetAirPathNum = 1; RetAirPathNum <= AirToZoneNodeInfo( ZoneGroupNum ).NumReturnNodes; ++RetAirPathNum ) {
				UpdateHVACInterface( ZoneGroupNum, CalledFromAirSystemDemandSide, AirToZoneNodeInfo( ZoneGroupNum ).ZoneEquipReturnNodeNum( RetAirPathNum ), AirToZoneNodeInfo( ZoneGroupNum ).AirLoopReturnNodeNum( RetAirPathNum ), SimAir );
			}
		}

	}

	void
	ReportZoneEquipment()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is left for Module format consistency -- not needed in this module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

	}

	void
	CalcAirFlowSimple(
		int const SysTimestepLoop, // System time step index
		bool const AdjustZoneMassFlowFlag // flags to adjust zone mxing and infiltration mass flow rates
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN   na
		//       MODIFIED       Shirey, Jan 2008 (MIXING objects, use avg. conditions for Cp, Air Density and Hfg)
		//       MODIFIED       L. Lawrie and L. GU, Jan. 2008 (Allow multiple infiltration and ventilation objects)
		//                      B. Griffith. Jan 2009 add infiltration, residential basic/sherman-grimsrud and enhanced/AIM2
		//                      L. Lawrie - March 2009 - move ventilation electric calculation to this routine (for
		//                        Electricity Net.
		//                      L. Gu - Dec. 2009 - Added a new ventilation object to calculate flow rate based on wind and stack
		//                        effect through an opening.
		//       MODIFIED       Stovall - Aug 2011 (add refrigerator door mixing)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the air component of the heat balance.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutEnthalpy;
		using DataEnvironment::WindSpeed;
		using DataEnvironment::WindDir;
		using namespace DataHeatBalFanSys;
		using namespace DataHeatBalance;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdbFnHW;
		using DataRoomAirModel::ZTJET;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkControlSimple;
		using DataAirflowNetwork::AirflowNetworkControlSimpleADS;
		using DataAirflowNetwork::AirflowNetworkZoneFlag;
		using EarthTube::ManageEarthTube;
		using CoolTower::ManageCoolTower;
		using ThermalChimney::ManageThermalChimney;
		using DataZoneEquipment::ZoneEquipAvail;
		using DataZoneEquipment::ZMAT;
		using DataZoneEquipment::ZHumRat;
		using DataHVACGlobals::CycleOn;
		using DataHVACGlobals::CycleOnZoneFansOnly;
		using DataContaminantBalance::Contaminant;
		using DataContaminantBalance::ZoneAirCO2;
		using DataContaminantBalance::MixingMassFlowCO2;
		using DataContaminantBalance::ZoneAirGC;
		using DataContaminantBalance::MixingMassFlowGC;

		using DataHeatBalance::Ventilation;
		using DataGlobals::SecInHour;
		using DataGlobals::KickOffSimulation;
		using DataGlobals::HourOfDay;
		using DataHVACGlobals::TimeStepSys;
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const StdGravity( 9.80665 ); // The acceleration of gravity at the sea level (m/s2)
		static std::string const RoutineNameMixing( "CalcAirFlowSimple:Mixing" );
		static std::string const RoutineNameCrossMixing( "CalcAirFlowSimple:CrossMixing" );
		static std::string const RoutineNameRefrigerationDoorMixing( "CalcAirFlowSimple:RefrigerationDoorMixing" );
		static std::string const RoutineNameInfiltration( "CalcAirFlowSimple:Infiltration" );
		static std::string const RoutineNameZoneAirBalance( "CalcAirFlowSimple:ZoneAirBalance" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MCP;
		Real64 MCPxM;
		Real64 MCPxN;
		Real64 TZM; // Temperature of From Zone
		Real64 TZN; // Temperature of this zone
		Real64 TD; // Delta Temp limit of Mixing statement
		Real64 Tavg; // Average temperature in two zones exchanging air
		Real64 Wavg; // Average humidity ratio in two zones exchanging air
		int m; // Index to From Zone
		int n; // Index of this zone
		int j; // Loop Counter
		int NZ; // A pointer
		int I; // Ventilation master object index
		int NH; // Hybrid controlled zone number
		Real64 AirDensity; // Density of air (kg/m^3)
		Real64 CpAir; // Heat capacity of air (J/kg-C)
		Real64 OutletAirEnthalpy; // Enthlapy of outlet air (VENTILATION objects)
		Real64 TempExt;
		Real64 WindExt;
		bool MixingLimitFlag;
		Real64 MixingTmin;
		Real64 MixingTmax;

		Real64 IVF; // DESIGN INFILTRATION FLOW RATE (M**3/SEC)
		Real64 VVF; // DESIGN VENTILATION FLOW RATE (M**3/SEC)
		Real64 MCpI_temp;
		Real64 VAMFL_temp;

		Real64 Cw; // Opening effectivenss
		Real64 Cd; // Discharge coefficent
		Real64 angle; // Angle between wind direction and effective angle
		Real64 Qw; // Volumetric flow driven by wind
		Real64 Qst; // Volumetric flow driven by stack effect
		Real64 MassFlowDiff;
		//following variables used for refrigeration door mixing and all defined in EngRef
		int ZoneA;
		int ZoneB;
		Real64 TZoneA;
		Real64 TZoneB;
		Real64 HumRatZoneA;
		Real64 HumRatZoneB;
		Real64 AirDensityZoneA;
		Real64 CpAirZoneA;
		Real64 AirDensityZoneB;
		Real64 CpAirZoneB;
		Real64 AirDensityAvg;
		Real64 MassFlowDryAir;
		Real64 SchedDoorOpen;
		Real64 DoorHeight;
		Real64 DoorArea;
		Real64 DoorProt;
		Real64 FDens;
		Real64 Fb;
		Real64 FFlow;
		Real64 MassFlowToA;
		Real64 MassFlowToB;
		Real64 MassFlowXCpToA;
		Real64 MassFlowXCpToB;
		Real64 MassFlowXCpXTempToA;
		Real64 MassFlowXCpXTempToB;
		Real64 MassFlowXHumRatToA;
		Real64 MassFlowXHumRatToB;
		Real64 MassFlowRate;

		// Allocate the ZMAT and ZHumRat arrays

		if ( ! allocated( ZMAT ) ) ZMAT.allocate( NumOfZones );
		if ( ! allocated( ZHumRat ) ) ZHumRat.allocate( NumOfZones );
		if ( ! allocated( VentMCP ) ) VentMCP.allocate( TotVentilation );

		// Allocate module level logical arrays for MIXING and CROSS MIXING reporting
		if ( ! allocated( CrossMixingReportFlag ) ) CrossMixingReportFlag.allocate( TotCrossMixing );
		if ( ! allocated( MixingReportFlag ) ) MixingReportFlag.allocate( TotMixing );

		if ( ! allocated( MCPTThermChim ) ) MCPTThermChim.allocate( NumOfZones );
		if ( ! allocated( MCPThermChim ) ) MCPThermChim.allocate( NumOfZones );
		if ( ! allocated( ThermChimAMFL ) ) ThermChimAMFL.allocate( NumOfZones );

		//                                      COMPUTE ZONE AIR MIXINGS
		MCPM = 0.0;
		MCPTM = 0.0;
		MixingMassFlowZone = 0.0;
		MixingMassFlowXHumRat = 0.0;
		CrossMixingFlag = false;
		CrossMixingReportFlag = false;
		MixingReportFlag = false;
		if ( Contaminant.CO2Simulation && TotMixing + TotCrossMixing + TotRefDoorMixing > 0 ) MixingMassFlowCO2 = 0.0;
		if ( Contaminant.GenericContamSimulation && TotMixing + TotCrossMixing + TotRefDoorMixing > 0 ) MixingMassFlowGC = 0.0;

		IVF = 0.0;
		MCPTI = 0.0;
		MCPI = 0.0;
		OAMFL = 0.0;
		VVF = 0.0;
		MCPTV = 0.0;
		MCPV = 0.0;
		VAMFL = 0.0;
		VentMCP = 0.0;
		MDotCPOA = 0.0;
		MDotOA = 0.0;

		MCPThermChim = 0.0;
		ThermChimAMFL = 0.0;
		MCPTThermChim = 0.0;
		MassFlowRate = 0.0;

		if ( AirFlowFlag != UseSimpleAirFlow ) return;
		// AirflowNetwork Multizone field /= SIMPLE
		if ( ! ( SimulateAirflowNetwork == AirflowNetworkControlSimple || SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ) ) return;

		ManageEarthTube();
		ManageCoolTower();
		ManageThermalChimney();

		// Assign zone air temperature
		for ( j = 1; j <= NumOfZones; ++j ) {
			ZMAT( j ) = MAT( j );
			ZHumRat( j ) = ZoneAirHumRat( j );
			// This is only temperory fix for CR8867.  (L. Gu 8/12)
			if ( SysTimestepLoop == 1 ) {
				ZMAT( j ) = XMPT( j );
				ZHumRat( j ) = WZoneTimeMinusP( j );
			}
		}

		// Process the scheduled Ventilation for air heat balance
		if ( TotVentilation > 0 ) {
			for ( auto & e : ZnAirRpt ) e.VentilFanElec = 0.0;
		}

		// Initialization of ZoneAirBalance
		if ( TotZoneAirBalance > 0 ) {
			for ( auto & e : ZoneAirBalance ) {
				e.BalMassFlowRate = 0.0;
				e.InfMassFlowRate = 0.0;
				e.NatMassFlowRate = 0.0;
				e.ExhMassFlowRate = 0.0;
				e.IntMassFlowRate = 0.0;
				e.ERVMassFlowRate = 0.0;
			}
		}

		for ( j = 1; j <= TotVentilation; ++j ) {
			NZ = Ventilation( j ).ZonePtr;
			Ventilation( j ).FanPower = 0.0;
			TempExt = Zone( NZ ).OutDryBulbTemp;
			WindExt = Zone( NZ ).WindSpeed;
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, TempExt, OutHumRat );
			CpAir = PsyCpAirFnWTdb( OutHumRat, TempExt );
			//CR7751 should maybe use code below, indoor conditions instead of outdoor conditions
			//   AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, ZMAT(NZ), ZHumRat(NZ))
			//   CpAir = PsyCpAirFnWTdb(ZHumRat(NZ),ZMAT(NZ))
			// Hybrid ventilation global control
			if ( Ventilation( j ).HybridControlType == HybridControlTypeGlobal && Ventilation( j ).HybridControlMasterNum > 0 ) {
				I = Ventilation( j ).HybridControlMasterNum;
				NH = Ventilation( I ).ZonePtr;
				if ( j == I ) Ventilation( j ).HybridControlMasterStatus = false;
			} else {
				I = j;
				NH = NZ;
			}
			// Check scheduled temperatures
			if ( Ventilation( I ).MinIndoorTempSchedPtr > 0 ) {
				Ventilation( I ).MinIndoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MinIndoorTempSchedPtr );
			}
			if ( Ventilation( I ).MaxIndoorTempSchedPtr > 0 ) {
				Ventilation( I ).MaxIndoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MaxIndoorTempSchedPtr );
			}
			// Ensure the minimum indoor temperature <= the maximum indoor temperature
			if ( Ventilation( I ).MinIndoorTempSchedPtr > 0 || Ventilation( I ).MaxIndoorTempSchedPtr > 0 ) {
				if ( Ventilation( I ).MinIndoorTemperature > Ventilation( I ).MaxIndoorTemperature ) {
					++Ventilation( I ).IndoorTempErrCount;
					if ( Ventilation( I ).IndoorTempErrCount < 2 ) {
						ShowWarningError( "Ventilation indoor temperature control: The minimum indoor temperature is above the maximum indoor temperature in " + Ventilation( I ).Name );
						ShowContinueError( "The minimum indoor temperature is set to the maximum indoor temperature. Simulation continues." );
						ShowContinueErrorTimeStamp( " Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The minimum indoor temperature is still above the maximum indoor temperature", Ventilation( I ).IndoorTempErrIndex, Ventilation( I ).MinIndoorTemperature, Ventilation( I ).MinIndoorTemperature );
					}
					Ventilation( I ).MinIndoorTemperature = Ventilation( I ).MaxIndoorTemperature;
				}
			}
			if ( Ventilation( I ).MinOutdoorTempSchedPtr > 0 ) {
				Ventilation( I ).MinOutdoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MinOutdoorTempSchedPtr );
			}
			if ( Ventilation( I ).MaxOutdoorTempSchedPtr > 0 ) {
				Ventilation( I ).MaxOutdoorTemperature = GetCurrentScheduleValue( Ventilation( I ).MaxOutdoorTempSchedPtr );
			}
			// Ensure the minimum outdoor temperature <= the maximum outdoor temperature
			if ( Ventilation( I ).MinOutdoorTempSchedPtr > 0 || Ventilation( I ).MaxOutdoorTempSchedPtr > 0 ) {
				if ( Ventilation( I ).MinOutdoorTemperature > Ventilation( I ).MaxOutdoorTemperature ) {
					++Ventilation( I ).OutdoorTempErrCount;
					if ( Ventilation( I ).OutdoorTempErrCount < 2 ) {
						ShowWarningError( "Ventilation outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor temperature in " + Ventilation( I ).Name );
						ShowContinueError( "The minimum outdoor temperature is set to the maximum outdoor temperature. Simulation continues." );
						ShowContinueErrorTimeStamp( " Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above the maximum outdoor temperature", Ventilation( I ).OutdoorTempErrIndex, Ventilation( I ).MinOutdoorTemperature, Ventilation( I ).MinOutdoorTemperature );
					}
					Ventilation( I ).MinIndoorTemperature = Ventilation( I ).MaxIndoorTemperature;
				}
			}
			if ( Ventilation( I ).DeltaTempSchedPtr > 0 ) {
				Ventilation( I ).DelTemperature = GetCurrentScheduleValue( Ventilation( I ).DeltaTempSchedPtr );
			}
			// Skip this if the zone is below the minimum indoor temperature limit
			if ( ( ZMAT( NH ) < Ventilation( I ).MinIndoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the zone is above the maximum indoor temperature limit
			if ( ( ZMAT( NH ) > Ventilation( I ).MaxIndoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip if below the temperature difference limit (3/12/03 Negative DelTemperature allowed now)
			if ( ( ( ZMAT( NH ) - TempExt ) < Ventilation( I ).DelTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the outdoor temperature is below the minimum outdoor temperature limit
			if ( ( TempExt < Ventilation( I ).MinOutdoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the outdoor temperature is above the maximum outdoor temperature limit
			if ( ( TempExt > Ventilation( I ).MaxOutdoorTemperature ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			// Skip this if the outdoor wind speed is above the maximum windspeed limit
			if ( ( WindExt > Ventilation( I ).MaxWindSpeed ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;

			// Hybrid ventilation controls
			if ( ( Ventilation( j ).HybridControlType == HybridControlTypeClose ) && ( ! Ventilation( j ).EMSSimpleVentOn ) ) continue;
			if ( Ventilation( j ).HybridControlType == HybridControlTypeGlobal && Ventilation( j ).HybridControlMasterNum > 0 ) {
				if ( j == I ) Ventilation( j ).HybridControlMasterStatus = true;
			}

			if ( Ventilation( j ).ModelType == VentilationDesignFlowRate ) {
				// CR6845 if calculated < 0, don't propagate.
				VVF = Ventilation( j ).DesignLevel * GetCurrentScheduleValue( Ventilation( j ).SchedPtr );

				if ( Ventilation( j ).EMSSimpleVentOn ) VVF = Ventilation( j ).EMSimpleVentFlowRate;

				if ( VVF < 0.0 ) VVF = 0.0;
				VentMCP( j ) = VVF * AirDensity * CpAir * ( Ventilation( j ).ConstantTermCoef + std::abs( TempExt - ZMAT( NZ ) ) * Ventilation( j ).TemperatureTermCoef + WindExt * ( Ventilation( j ).VelocityTermCoef + WindExt * Ventilation( j ).VelocitySQTermCoef ) );
				if ( VentMCP( j ) < 0.0 ) VentMCP( j ) = 0.0;
				VAMFL_temp = VentMCP( j ) / CpAir;
				if ( Ventilation( j ).QuadratureSum ) {
					{ auto const SELECT_CASE_var( Ventilation( j ).FanType ); // ventilation type based calculation
					if ( SELECT_CASE_var == ExhaustVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).ExhMassFlowRate += VentMCP( j ) / CpAir;
					} else if ( SELECT_CASE_var == IntakeVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).IntMassFlowRate += VentMCP( j ) / CpAir;
					} else if ( SELECT_CASE_var == NaturalVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).NatMassFlowRate += VentMCP( j ) / CpAir;
					} else if ( SELECT_CASE_var == BalancedVentilation ) {
						ZoneAirBalance( Ventilation( j ).OABalancePtr ).BalMassFlowRate += VentMCP( j ) / CpAir;
					}}
				} else {
					MCPV( NZ ) += VentMCP( j );
					VAMFL( NZ ) += VAMFL_temp;
				}
				if ( Ventilation( j ).FanEfficiency > 0.0 ) {
					Ventilation( j ).FanPower = VAMFL_temp * Ventilation( j ).FanPressure / ( Ventilation( j ).FanEfficiency * AirDensity );
					if ( Ventilation( j ).FanType == BalancedVentilation ) Ventilation( j ).FanPower *= 2.0;
					// calc electric
					if ( SimulateAirflowNetwork == AirflowNetworkControlSimpleADS ) {
						// CR7608 IF (.not. TurnFansOn .or. .not. AirflowNetworkZoneFlag(NZ)) &
						if ( ! KickOffSimulation ) {
							if ( ! ( ZoneEquipAvail( NZ ) == CycleOn || ZoneEquipAvail( NZ ) == CycleOnZoneFansOnly ) || ! AirflowNetworkZoneFlag( NZ ) ) ZnAirRpt( NZ ).VentilFanElec += Ventilation( j ).FanPower * TimeStepSys * SecInHour;
						} else if ( ! AirflowNetworkZoneFlag( NZ ) ) {
							ZnAirRpt( NZ ).VentilFanElec += Ventilation( j ).FanPower * TimeStepSys * SecInHour;
						}
					} else {
						ZnAirRpt( NZ ).VentilFanElec += Ventilation( j ).FanPower * TimeStepSys * SecInHour;
					}
				}
				// Intake fans will add some heat to the air, raising the temperature for an intake fan...
				if ( Ventilation( j ).FanType == IntakeVentilation || Ventilation( j ).FanType == BalancedVentilation ) {
					if ( VAMFL_temp == 0.0 ) {
						OutletAirEnthalpy = OutEnthalpy;
					} else {
						if ( Ventilation( j ).FanPower > 0.0 ) {
							if ( Ventilation( j ).FanType == BalancedVentilation ) {
								OutletAirEnthalpy = OutEnthalpy + Ventilation( j ).FanPower / VAMFL_temp / 2.0; // Half fan power to calculate inlet T
							} else {
								OutletAirEnthalpy = OutEnthalpy + Ventilation( j ).FanPower / VAMFL_temp;
							}
						} else {
							OutletAirEnthalpy = OutEnthalpy;
						}
					}
					Ventilation( j ).AirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutHumRat );
				} else {
					Ventilation( j ).AirTemp = TempExt;
				}
				if ( ! Ventilation( j ).QuadratureSum ) MCPTV( NZ ) += VentMCP( j ) * Ventilation( j ).AirTemp;
			}

			if ( Ventilation( j ).ModelType == VentilationWindAndStack ) {
				if ( Ventilation( j ).OpenEff != AutoCalculate ) {
					Cw = Ventilation( j ).OpenEff;
				} else {
					// linear interpolation between effective angle and wind direction
					angle = std::abs( WindDir - Ventilation( j ).EffAngle );
					if ( angle > 180.0 ) angle -= 180.0;
					Cw = 0.55 + angle / 180.0 * ( 0.3 - 0.55 );
				}
				if ( Ventilation( j ).DiscCoef != AutoCalculate ) {
					Cd = Ventilation( j ).DiscCoef;
				} else {
					Cd = 0.40 + 0.0045 * std::abs( TempExt - ZMAT( NZ ) );
				}
				Qw = Cw * Ventilation( j ).OpenArea * GetCurrentScheduleValue( Ventilation( j ).OpenAreaSchedPtr ) * WindExt;
				Qst = Cd * Ventilation( j ).OpenArea * GetCurrentScheduleValue( Ventilation( j ).OpenAreaSchedPtr ) * std::sqrt( 2.0 * 9.81 * Ventilation( j ).DH * std::abs( TempExt - ZMAT( NZ ) ) / ( ZMAT( NZ ) + 273.15 ) );
				VVF = std::sqrt( Qw * Qw + Qst * Qst );
				if ( Ventilation( j ).EMSSimpleVentOn ) VVF = Ventilation( j ).EMSimpleVentFlowRate;
				if ( VVF < 0.0 ) VVF = 0.0;
				VentMCP( j ) = VVF * AirDensity * CpAir;
				if ( VentMCP( j ) < 0.0 ) VentMCP( j ) = 0.0;
				if ( Ventilation( j ).QuadratureSum ) {
					ZoneAirBalance( Ventilation( j ).OABalancePtr ).NatMassFlowRate += VentMCP( j ) / CpAir;
				} else {
					MCPV( NZ ) += VentMCP( j );
					VAMFL_temp = VentMCP( j ) / CpAir;
					VAMFL( NZ ) += VAMFL_temp;
					Ventilation( j ).AirTemp = TempExt;
					MCPTV( NZ ) += VentMCP( j ) * Ventilation( j ).AirTemp;
				}
			}
		}

		// Process Mixing
		for ( j = 1; j <= TotMixing; ++j ) {
			n = Mixing( j ).ZonePtr;
			m = Mixing( j ).FromZone;
			TD = Mixing( j ).DeltaTemperature;
			// Get scheduled delta temperature
			if ( Mixing( j ).DeltaTempSchedPtr > 0 ) {
				TD = GetCurrentScheduleValue( Mixing( j ).DeltaTempSchedPtr );
			}
			TZN = ZMAT( n );
			TZM = ZMAT( m );

			// Hybrid ventilation controls
			if ( Mixing( j ).HybridControlType == HybridControlTypeClose ) continue;
			// Check temperature limit
			MixingLimitFlag = false;

			// Hybrid ventilation global control
			if ( Mixing( j ).HybridControlType == HybridControlTypeGlobal && Mixing( j ).HybridControlMasterNum > 0 ) {
				I = Mixing( j ).HybridControlMasterNum;
				if ( ! Ventilation( I ).HybridControlMasterStatus ) continue;
			} else {
				// Ensure the minimum indoor temperature <= the maximum indoor temperature
				if ( Mixing( j ).MinIndoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( Mixing( j ).MinIndoorTempSchedPtr );
				if ( Mixing( j ).MaxIndoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( Mixing( j ).MaxIndoorTempSchedPtr );
				if ( Mixing( j ).MinIndoorTempSchedPtr > 0 && Mixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++Mixing( j ).IndoorTempErrCount;
						if ( Mixing( j ).IndoorTempErrCount < 2 ) {
							ShowWarningError( "Mixing zone temperature control: The minimum zone temperature is above the maximum zone temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum zone temperature is set to the maximum zone temperature. Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum zone temperature is still above the maximum zone temperature", Mixing( j ).IndoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( Mixing( j ).MinIndoorTempSchedPtr > 0 ) {
					if ( TZN < MixingTmin ) MixingLimitFlag = true;
				}
				if ( Mixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( TZN > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum source temperature <= the maximum source temperature
				if ( Mixing( j ).MinSourceTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( Mixing( j ).MinSourceTempSchedPtr );
				if ( Mixing( j ).MaxSourceTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( Mixing( j ).MaxSourceTempSchedPtr );
				if ( Mixing( j ).MinSourceTempSchedPtr > 0 && Mixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++Mixing( j ).SourceTempErrCount;
						if ( Mixing( j ).SourceTempErrCount < 2 ) {
							ShowWarningError( "Mixing source temperature control: The minimum source temperature is above the maximum source temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum source temperature is set to the maximum source temperature. Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum source temperature is still above the maximum source temperature", Mixing( j ).SourceTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( Mixing( j ).MinSourceTempSchedPtr > 0 ) {
					if ( TZM < MixingTmin ) MixingLimitFlag = true;
				}
				if ( Mixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( TZM > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum outdoor temperature <= the maximum outdoor temperature
				TempExt = Zone( n ).OutDryBulbTemp;
				if ( Mixing( j ).MinOutdoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( Mixing( j ).MinOutdoorTempSchedPtr );
				if ( Mixing( j ).MaxOutdoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( Mixing( j ).MaxOutdoorTempSchedPtr );
				if ( Mixing( j ).MinOutdoorTempSchedPtr > 0 && Mixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++Mixing( j ).OutdoorTempErrCount;
						if ( Mixing( j ).OutdoorTempErrCount < 2 ) {
							ShowWarningError( "Mixing outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum outdoor temperature is set to the maximum source temperature. Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above the maximum outdoor temperature", Mixing( j ).OutdoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( Mixing( j ).MinOutdoorTempSchedPtr > 0 ) {
					if ( TempExt < MixingTmin ) MixingLimitFlag = true;
				}
				if ( Mixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( TempExt > MixingTmax ) MixingLimitFlag = true;
				}
			}

			if ( Mixing( j ).HybridControlType != HybridControlTypeGlobal && MixingLimitFlag ) continue;
			if ( Mixing( j ).HybridControlType == HybridControlTypeGlobal ) TD = 0.0;

			//  If TD equals zero (default) set coefficients for full mixing otherwise test
			//    for mixing conditions if user input delta temp > 0, then from zone temp (TZM)
			//    must be td degrees warmer than zone temp (TZN).  If user input delta temp < 0,
			//    then from zone temp (TZM) must be TD degrees cooler than zone temp (TZN).
			if ( TD < 0.0 ) {
				if ( TZM < TZN + TD ) {
					//            Per Jan 17, 2008 conference call, agreed to use average conditions for Rho, Cp and Hfg
					//             RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
					//             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( TZN + TZM ) / 2.0, ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0 );
					CpAir = PsyCpAirFnWTdb( ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, ( TZN + TZM ) / 2.0 ); // Use average conditions

					Mixing(j).DesiredAirFlowRate = Mixing(j).DesiredAirFlowRateSaved;
					if ( ZoneMassBalanceFlag(n) && AdjustZoneMassFlowFlag ) {
						if ( Mixing(j).MixingMassFlowRate > 0.0 ) {
							Mixing(j).DesiredAirFlowRate = Mixing(j).MixingMassFlowRate / AirDensity;
						}
					}
					Mixing(j).MixingMassFlowRate = Mixing(j).DesiredAirFlowRate * AirDensity;

					MCP = Mixing( j ).DesiredAirFlowRate * CpAir * AirDensity;
					MCPM( n ) += MCP;
					MCPTM( n ) += MCP * TZM;

					// Now to determine the moisture conditions
					MixingMassFlowZone( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity;
					MixingMassFlowXHumRat( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZHumRat( m );
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirCO2( m );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowGC( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirGC( m );
					}
					MixingReportFlag( j ) = true;
				}
			}
			if ( TD > 0.0 ) {
				if ( TZM > TZN + TD ) {
					//             RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
					//             MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( TZN + TZM ) / 2.0, ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0 ); // Use avg conditions
					CpAir = PsyCpAirFnWTdb( ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, ( TZN + TZM ) / 2.0 ); // Use average conditions

					Mixing(j).DesiredAirFlowRate = Mixing(j).DesiredAirFlowRateSaved;
					if ( ZoneMassBalanceFlag(n) && AdjustZoneMassFlowFlag ) {
						if ( Mixing(j).MixingMassFlowRate > 0.0 ) {
							Mixing(j).DesiredAirFlowRate = Mixing(j).MixingMassFlowRate / AirDensity;
						}
					}
					Mixing(j).MixingMassFlowRate = Mixing(j).DesiredAirFlowRate * AirDensity;

					MCP = Mixing( j ).DesiredAirFlowRate * CpAir * AirDensity;
					MCPM( n ) += MCP;
					MCPTM( n ) += MCP * TZM;
					// Now to determine the moisture conditions
					MixingMassFlowZone( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity;
					MixingMassFlowXHumRat( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZHumRat( m );
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirCO2( m );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowGC( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirGC( m );
					}
					MixingReportFlag( j ) = true;
				}
			}
			if ( TD == 0.0 ) {
				//          RhoAirM = PsyRhoAirFnPbTdbW(OutBaroPress,tzm,ZHumRat(m))
				//          MCP=Mixing(J)%DesiredAirFlowRate * PsyCpAirFnWTdb(ZHumRat(m),tzm) * RhoAirM
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, ( TZN + TZM ) / 2.0, ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, RoutineNameMixing ); // Use avg conditions
				CpAir = PsyCpAirFnWTdb( ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0, ( TZN + TZM ) / 2.0 ); // Use average conditions

				Mixing(j).DesiredAirFlowRate = Mixing(j).DesiredAirFlowRateSaved;
				if ( ZoneMassBalanceFlag(n) && AdjustZoneMassFlowFlag ) {
					if ( Mixing(j).MixingMassFlowRate > 0.0 ) {
						Mixing(j).DesiredAirFlowRate = Mixing(j).MixingMassFlowRate / AirDensity;
					}
				}
				Mixing(j).MixingMassFlowRate = Mixing(j).DesiredAirFlowRate * AirDensity;

				MCP = Mixing( j ).DesiredAirFlowRate * CpAir * AirDensity;
				MCPM( n ) += MCP;
				MCPTM( n ) += MCP * TZM;
				// Now to determine the moisture conditions
				MixingMassFlowZone( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity;
				MixingMassFlowXHumRat( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZHumRat( m );
				if ( Contaminant.CO2Simulation ) {
					MixingMassFlowCO2( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirCO2( m );
				}
				if ( Contaminant.GenericContamSimulation ) {
					MixingMassFlowGC( n ) += Mixing( j ).DesiredAirFlowRate * AirDensity * ZoneAirGC( m );
				}
				MixingReportFlag( j ) = true;
			}
		}

		//                              COMPUTE CROSS ZONE
		//                              AIR MIXING
		for ( j = 1; j <= TotCrossMixing; ++j ) {
			n = CrossMixing( j ).ZonePtr;
			m = CrossMixing( j ).FromZone;
			TD = MTC( j );
			// Get scheduled delta temperature
			if ( CrossMixing( j ).DeltaTempSchedPtr > 0 ) {
				TD = GetCurrentScheduleValue( CrossMixing( j ).DeltaTempSchedPtr );
			}

			if ( TD >= 0.0 ) {
				TZN = ZMAT( n );
				TZM = ZMAT( m );
				// Check temperature limit
				MixingLimitFlag = false;
				// Ensure the minimum indoor temperature <= the maximum indoor temperature
				if ( CrossMixing( j ).MinIndoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( CrossMixing( j ).MinIndoorTempSchedPtr );
				if ( CrossMixing( j ).MaxIndoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( CrossMixing( j ).MaxIndoorTempSchedPtr );
				if ( CrossMixing( j ).MinIndoorTempSchedPtr > 0 && CrossMixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++CrossMixing( j ).IndoorTempErrCount;
						if ( CrossMixing( j ).IndoorTempErrCount < 2 ) {
							ShowWarningError( "CrossMixing zone temperature control: The minimum zone temperature is above the maximum zone temperature in " + CrossMixing( j ).Name );
							ShowContinueError( "The minimum zone temperature is set to the maximum zone temperature. Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum zone temperature is still above the maximum zone temperature", CrossMixing( j ).IndoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( CrossMixing( j ).MinIndoorTempSchedPtr > 0 ) {
					if ( TZN < MixingTmin ) MixingLimitFlag = true;
				}
				if ( CrossMixing( j ).MaxIndoorTempSchedPtr > 0 ) {
					if ( TZN > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum source temperature <= the maximum source temperature
				if ( CrossMixing( j ).MinSourceTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( CrossMixing( j ).MinSourceTempSchedPtr );
				if ( CrossMixing( j ).MaxSourceTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( CrossMixing( j ).MaxSourceTempSchedPtr );
				if ( CrossMixing( j ).MinSourceTempSchedPtr > 0 && CrossMixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++CrossMixing( j ).SourceTempErrCount;
						if ( CrossMixing( j ).SourceTempErrCount < 2 ) {
							ShowWarningError( "CrossMixing source temperature control: The minimum source temperature is above the maximum source temperature in " + CrossMixing( j ).Name );
							ShowContinueError( "The minimum source temperature is set to the maximum source temperature. Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum source temperature is still above the maximum source temperature", CrossMixing( j ).SourceTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( CrossMixing( j ).MinSourceTempSchedPtr > 0 ) {
					if ( TZM < MixingTmin ) MixingLimitFlag = true;
				}
				if ( CrossMixing( j ).MaxSourceTempSchedPtr > 0 ) {
					if ( TZM > MixingTmax ) MixingLimitFlag = true;
				}
				// Ensure the minimum outdoor temperature <= the maximum outdoor temperature
				TempExt = Zone( n ).OutDryBulbTemp;
				if ( CrossMixing( j ).MinOutdoorTempSchedPtr > 0 ) MixingTmin = GetCurrentScheduleValue( CrossMixing( j ).MinOutdoorTempSchedPtr );
				if ( CrossMixing( j ).MaxOutdoorTempSchedPtr > 0 ) MixingTmax = GetCurrentScheduleValue( CrossMixing( j ).MaxOutdoorTempSchedPtr );
				if ( CrossMixing( j ).MinOutdoorTempSchedPtr > 0 && CrossMixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( MixingTmin > MixingTmax ) {
						++CrossMixing( j ).OutdoorTempErrCount;
						if ( CrossMixing( j ).OutdoorTempErrCount < 2 ) {
							ShowWarningError( "CrossMixing outdoor temperature control: The minimum outdoor temperature is above the maximum outdoor temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum outdoor temperature is set to the maximum source temperature. Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above the maximum outdoor temperature", CrossMixing( j ).OutdoorTempErrIndex, MixingTmin, MixingTmin );
						}
						MixingTmin = MixingTmax;
					}
				}
				if ( CrossMixing( j ).MinOutdoorTempSchedPtr > 0 ) {
					if ( TempExt < MixingTmin ) MixingLimitFlag = true;
				}
				if ( CrossMixing( j ).MaxOutdoorTempSchedPtr > 0 ) {
					if ( TempExt > MixingTmax ) MixingLimitFlag = true;
				}
				if ( MixingLimitFlag ) continue;

				if ( ( TD == 0.0 || ( TD > 0.0 && ( TZM - TZN ) >= TD ) ) ) {
					CrossMixingReportFlag( j ) = true; // set reporting flag
				}

				if ( ( ( TD <= 0.0 ) && ( ! CrossMixingFlag( n ) && ! CrossMixingFlag( m ) ) ) || ( ( TD > 0.0 ) && ( TZM - TZN >= TD ) ) ) {
					//                                      SET COEFFICIENTS .
					CrossMixingFlag( n ) = true;
					CrossMixingFlag( m ) = true;

					Tavg = ( TZN + TZM ) / 2.0;
					Wavg = ( ZHumRat( n ) + ZHumRat( m ) ) / 2.0;
					AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Tavg, Wavg, RoutineNameCrossMixing );
					CpAir = PsyCpAirFnWTdb( Wavg, Tavg );
					MCPxN = MVFC( j ) * CpAir * AirDensity;
					MCPM( n ) += MCPxN;

					MCPxM = MVFC( j ) * CpAir * AirDensity;
					MCPM( m ) += MCPxM;
					MCPTM( n ) += MCPxM * TZM;
					MCPTM( m ) += MCPxN * TZN;

					// Now to determine the moisture conditions
					MixingMassFlowZone( m ) += MVFC( j ) * AirDensity;
					MixingMassFlowXHumRat( m ) += MVFC( j ) * AirDensity * ZHumRat( n );

					MixingMassFlowZone( n ) += MVFC( j ) * AirDensity;
					MixingMassFlowXHumRat( n ) += MVFC( j ) * AirDensity * ZHumRat( m );
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( m ) += MVFC( j ) * AirDensity * ZoneAirCO2( n );
						MixingMassFlowCO2( n ) += MVFC( j ) * AirDensity * ZoneAirCO2( m );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowGC( m ) += MVFC( j ) * AirDensity * ZoneAirGC( n );
						MixingMassFlowGC( n ) += MVFC( j ) * AirDensity * ZoneAirGC( m );
					}
				}
			}
		}

		//                              COMPUTE REFRIGERATION DOOR
		//                              AIR MIXING
		if ( TotRefDoorMixing > 0 ) {
			//Zone loops structured in getinput so only do each pair of zones bounding door once, even if multiple doors in one zone
			for ( ZoneA = 1; ZoneA <= ( NumOfZones - 1 ); ++ZoneA ) {
				if ( ! RefDoorMixing( ZoneA ).RefDoorMixFlag ) continue;
				for ( j = 1; j <= RefDoorMixing( ZoneA ).NumRefDoorConnections; ++j ) {
					ZoneB = RefDoorMixing( ZoneA ).MateZonePtr( j );
					TZoneA = ZMAT( ZoneA );
					TZoneB = ZMAT( ZoneB );
					HumRatZoneA = ZHumRat( ZoneA );
					HumRatZoneB = ZHumRat( ZoneB );
					AirDensityZoneA = PsyRhoAirFnPbTdbW( OutBaroPress, TZoneA, HumRatZoneA, RoutineNameRefrigerationDoorMixing );
					CpAirZoneA = PsyCpAirFnWTdb( HumRatZoneA, TZoneA );
					AirDensityZoneB = PsyRhoAirFnPbTdbW( OutBaroPress, TZoneB, HumRatZoneB, RoutineNameRefrigerationDoorMixing );
					CpAirZoneB = PsyCpAirFnWTdb( HumRatZoneB, TZoneB );
					Tavg = ( TZoneA + TZoneB ) / 2.0;
					Wavg = ( HumRatZoneA + HumRatZoneB ) / 2.0;
					AirDensityAvg = PsyRhoAirFnPbTdbW( OutBaroPress, Tavg, Wavg, RoutineNameRefrigerationDoorMixing );

					if ( RefDoorMixing( ZoneA ).EMSRefDoorMixingOn( j ) ) {
						MassFlowDryAir = RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) * AirDensityAvg;
					} else {
						SchedDoorOpen = GetCurrentScheduleValue( RefDoorMixing( ZoneA ).OpenSchedPtr( j ) );
						if ( SchedDoorOpen == 0.0 ) continue;
						DoorHeight = RefDoorMixing( ZoneA ).DoorHeight( j );
						DoorArea = RefDoorMixing( ZoneA ).DoorArea( j );
						DoorProt = RefDoorMixing( ZoneA ).Protection( j );
						if ( AirDensityZoneA >= AirDensityZoneB ) {
							// Mass of dry air flow between zones is equal,
							// but have to calc directionally to avoid sqrt(neg number)
							FDens = std::pow( 2.0 / ( 1.0 + std::pow( AirDensityZoneA / AirDensityZoneB, 1.0 / 3.0 ) ), 1.5 );
							Fb = 0.221 * DoorArea * AirDensityZoneA * FDens * std::sqrt( ( 1.0 - AirDensityZoneB / AirDensityZoneA ) * StdGravity * DoorHeight );
						} else { //ZoneADens < ZoneBDens
							FDens = std::pow( 2.0 / ( 1.0 + std::pow( AirDensityZoneB / AirDensityZoneA, 1.0 / 3.0 ) ), 1.5 );
							Fb = 0.221 * DoorArea * AirDensityZoneB * FDens * std::sqrt( ( 1.0 - AirDensityZoneA / AirDensityZoneB ) * StdGravity * DoorHeight );
						} //ZoneADens .GE. ZoneBDens
						// FFlow = Doorway flow factor, is determined by temperature difference
						FFlow = 1.1;
						if ( std::abs( TZoneA - TZoneB ) > 11.0 ) FFlow = 0.8;
						MassFlowDryAir = Fb * SchedDoorOpen * FFlow * ( 1.0 - DoorProt );
						RefDoorMixing( ZoneA ).VolRefDoorFlowRate( j ) = MassFlowDryAir / AirDensityAvg;
						//Note - VolRefDoorFlowRate is used ONLY for reporting purposes, where it is
						//       used with the avg density to generate a reported mass flow
						//       Considering the small values typical for HumRat, this is not far off.
					} // EMSRefDoorMixingOn

					MassFlowToA = MassFlowDryAir * ( 1.0 + HumRatZoneB );
					MassFlowToB = MassFlowDryAir * ( 1.0 + HumRatZoneA );
					MassFlowXCpToA = MassFlowToA * CpAirZoneB;
					MassFlowXCpToB = MassFlowToB * CpAirZoneA;
					MassFlowXCpXTempToA = MassFlowXCpToA * TZoneB;
					MassFlowXCpXTempToB = MassFlowXCpToB * TZoneA;
					MassFlowXHumRatToA = MassFlowToA * HumRatZoneB;
					MassFlowXHumRatToB = MassFlowToB * HumRatZoneA;

					MCPM( ZoneA ) += MassFlowXCpToA;
					MCPM( ZoneB ) += MassFlowXCpToB;
					MCPTM( ZoneA ) += MassFlowXCpXTempToA;
					MCPTM( ZoneB ) += MassFlowXCpXTempToB;

					// Now to determine the moisture conditions
					MixingMassFlowZone( ZoneA ) += MassFlowToA;
					MixingMassFlowZone( ZoneB ) += MassFlowToB;
					MixingMassFlowXHumRat( ZoneA ) += MassFlowXHumRatToA;
					MixingMassFlowXHumRat( ZoneB ) += MassFlowXHumRatToB;

					// Now to determine the CO2 and generic contaminant conditions
					if ( Contaminant.CO2Simulation ) {
						MixingMassFlowCO2( ZoneA ) += MassFlowToA * ZoneAirCO2( ZoneB );
						MixingMassFlowCO2( ZoneB ) += MassFlowToB * ZoneAirCO2( ZoneA );
					}
					if ( Contaminant.GenericContamSimulation ) {
						MixingMassFlowCO2( ZoneA ) += MassFlowToA * ZoneAirGC( ZoneB );
						MixingMassFlowCO2( ZoneB ) += MassFlowToB * ZoneAirGC( ZoneA );
					}

				} // J=1,RefDoorMixing(ZoneA)%NumRefDoorConnections
			} //ZoneA=1,(NumOfZones - 1)
		} //(TotRefrigerationDoorMixing > 0) THEN

		// Process the scheduled Infiltration for air heat balance depending on model type
		for ( j = 1; j <= TotInfiltration; ++j ) {

			NZ = Infiltration( j ).ZonePtr;

			TempExt = Zone( NZ ).OutDryBulbTemp;
			WindExt = Zone( NZ ).WindSpeed;
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, TempExt, OutHumRat, RoutineNameInfiltration );
			CpAir = PsyCpAirFnWTdb( OutHumRat, TempExt );
			//CR7751  should maybe use code below, indoor conditions instead of outdoor conditions
			//   AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, ZMAT(NZ), ZHumRat(NZ))
			//   CpAir = PsyCpAirFnWTdb(ZHumRat(NZ),ZMAT(NZ))
			{ auto const SELECT_CASE_var( Infiltration( j ).ModelType );

			if ( SELECT_CASE_var == InfiltrationDesignFlowRate ) {

				IVF = Infiltration( j ).DesignLevel * GetCurrentScheduleValue( Infiltration( j ).SchedPtr );
				// CR6845 if calculated < 0.0, don't propagate
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir * ( Infiltration( j ).ConstantTermCoef + std::abs( TempExt - ZMAT( NZ ) ) * Infiltration( j ).TemperatureTermCoef + WindExt * ( Infiltration( j ).VelocityTermCoef + WindExt * Infiltration( j ).VelocitySQTermCoef ) );

				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
				Infiltration(j).VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
				if ( AdjustZoneMassFlowFlag && ZoneInfiltrationFlag(NZ) ) {
					if ( ZoneAirMassFlow.InfiltrationTreatment == AdjustInfiltrationFlow ) {
						//if ( Infiltration(j).MassFlowRate > 0.0 ) {
							Infiltration(j).VolumeFlowRate = Infiltration(j).MassFlowRate / AirDensity;
							MCpI_temp = Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
						//}
					}
					if ( ZoneAirMassFlow.InfiltrationTreatment == AddInfiltrationFlow ) {
						Infiltration(j).VolumeFlowRate = Infiltration(j).VolumeFlowRate + MassConservation(NZ).InfiltrationMassFlowRate / AirDensity;
						MCpI_temp = Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
					}
				}
				Infiltration(j).MassFlowRate = Infiltration(j).VolumeFlowRate * AirDensity;
			} else if ( SELECT_CASE_var == InfiltrationShermanGrimsrud ) {
				// Sherman Grimsrud model as formulated in ASHRAE HoF
				WindExt = WindSpeed; // formulated to use wind at Meterological Station rather than local
				IVF = GetCurrentScheduleValue( Infiltration( j ).SchedPtr ) * Infiltration( j ).LeakageArea / 1000.0 * std::sqrt( Infiltration( j ).BasicStackCoefficient * std::abs( TempExt - ZMAT( NZ ) ) + Infiltration( j ).BasicWindCoefficient * pow_2( WindExt ) );
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir;
				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
				Infiltration(j).VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
				if ( AdjustZoneMassFlowFlag && ZoneInfiltrationFlag(NZ) ) {
					if ( ZoneAirMassFlow.InfiltrationTreatment == AdjustInfiltrationFlow ) {
						if ( Infiltration(j).MassFlowRate > 0.0 ) {
							Infiltration(j).VolumeFlowRate = Infiltration(j).MassFlowRate / AirDensity;
							MCpI_temp = Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
						}
					}
					if ( ZoneAirMassFlow.InfiltrationTreatment == AddInfiltrationFlow ) {
						Infiltration(j).VolumeFlowRate = Infiltration(j).VolumeFlowRate + MassConservation(NZ).InfiltrationMassFlowRate / AirDensity;
						MCpI_temp = Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
					}
				}
				Infiltration(j).MassFlowRate = Infiltration(j).VolumeFlowRate * AirDensity;
			} else if ( SELECT_CASE_var == InfiltrationAIM2 ) {
				// Walker Wilson model as formulated in ASHRAE HoF
				IVF = GetCurrentScheduleValue( Infiltration( j ).SchedPtr ) * std::sqrt( pow_2( Infiltration( j ).FlowCoefficient * Infiltration( j ).AIM2StackCoefficient * std::pow( std::abs( TempExt - ZMAT( NZ ) ), Infiltration( j ).PressureExponent ) ) + pow_2( Infiltration( j ).FlowCoefficient * Infiltration( j ).AIM2WindCoefficient * std::pow( Infiltration( j ).ShelterFactor * WindExt, 2.0 * Infiltration( j ).PressureExponent ) ) );
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir;
				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
				Infiltration(j).VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
				if ( AdjustZoneMassFlowFlag && ZoneInfiltrationFlag(NZ) ) {
					if ( ZoneAirMassFlow.InfiltrationTreatment == AdjustInfiltrationFlow ) {
						if ( Infiltration(j).MassFlowRate > 0.0 ) {
							Infiltration(j).VolumeFlowRate = Infiltration(j).MassFlowRate / AirDensity;
							MCpI_temp = Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
						}
					}
					if ( ZoneAirMassFlow.InfiltrationTreatment == AddInfiltrationFlow ) {
						Infiltration(j).VolumeFlowRate = Infiltration(j).VolumeFlowRate + MassConservation(NZ).InfiltrationMassFlowRate / AirDensity;
						MCpI_temp = Infiltration(j).VolumeFlowRate * AirDensity * CpAir;
					}
				}
				Infiltration(j).MassFlowRate = Infiltration(j).VolumeFlowRate * AirDensity;
			}}

			if ( Infiltration( j ).EMSOverrideOn ) {
				IVF = Infiltration( j ).EMSAirFlowRateValue;
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir;
				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
			}

			if ( Infiltration( j ).QuadratureSum ) {
				ZoneAirBalance( Infiltration( j ).OABalancePtr ).InfMassFlowRate += MCpI_temp / CpAir;
			} else {
				MCPI( NZ ) += MCpI_temp;
				OAMFL( NZ ) += MCpI_temp / CpAir;
				MCPTI( NZ ) += MCpI_temp * TempExt;
			}

		}

		// Add infiltration rate enhanced by the existence of thermal chimney
		for ( NZ = 1; NZ <= NumOfZones; ++NZ ) {
			MCPI( NZ ) += MCPThermChim( NZ );
			OAMFL( NZ ) += ThermChimAMFL( NZ );
			MCPTI( NZ ) += MCPTThermChim( NZ );
		}

		// Calculate combined outdoor air flows
		for ( j = 1; j <= TotZoneAirBalance; ++j ) {
			if ( ZoneAirBalance( j ).BalanceMethod == AirBalanceQuadrature ) {
				if ( ! ZoneAirBalance( j ).OneTimeFlag ) GetStandAloneERVNodes( j );
				if ( ZoneAirBalance( j ).NumOfERVs > 0 ) {
					for ( I = 1; I <= ZoneAirBalance( j ).NumOfERVs; ++I ) {
						MassFlowDiff = Node( ZoneAirBalance( j ).ERVExhaustNode( I ) ).MassFlowRate - Node( ZoneAirBalance( j ).ERVInletNode( I ) ).MassFlowRate;
						if ( MassFlowDiff > 0.0 ) {
							ZoneAirBalance( j ).ERVMassFlowRate += MassFlowDiff;
						}
					}
				}
				NZ = ZoneAirBalance( j ).ZonePtr;
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, Zone( NZ ).OutDryBulbTemp, OutHumRat, RoutineNameZoneAirBalance );
				CpAir = PsyCpAirFnWTdb( OutHumRat, Zone( NZ ).OutDryBulbTemp );
				ZoneAirBalance( j ).ERVMassFlowRate *= AirDensity;
				MDotOA( NZ ) = std::sqrt( pow_2( ZoneAirBalance( j ).NatMassFlowRate ) + pow_2( ZoneAirBalance( j ).IntMassFlowRate ) + pow_2( ZoneAirBalance( j ).ExhMassFlowRate ) + pow_2( ZoneAirBalance( j ).ERVMassFlowRate ) + pow_2( ZoneAirBalance( j ).InfMassFlowRate ) + pow_2( AirDensity * ZoneAirBalance( j ).InducedAirRate * GetCurrentScheduleValue( ZoneAirBalance( j ).InducedAirSchedPtr ) ) ) + ZoneAirBalance( j ).BalMassFlowRate;
				MDotCPOA( NZ ) = MDotOA( NZ ) * CpAir;
			}
		}

	}

	void
	GetStandAloneERVNodes(int const OutdoorNum) // Zone Air Balance Outdoor index
	{

			// SUBROUTINE INFORMATION:
			//       AUTHOR         Lixing Gu
			//       DATE WRITTEN   July 2010
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// This subroutine gets node numbers of stand alone ERVs to calculate combined outdoor air flows.

			// METHODOLOGY EMPLOYED:
			// Uses program data structures ZoneEquipInfo

			// REFERENCES:
			// na

			// Using/Aliasing
			using DataZoneEquipment::ZoneEquipList;
			using DataZoneEquipment::ERVStandAlone_Num;
			using DataHeatBalance::ZoneAirBalance;
			using DataHeatBalance::AirBalanceQuadrature;
			using HVACStandAloneERV::GetStandAloneERVOutAirNode;
			using HVACStandAloneERV::GetStandAloneERVReturnAirNode;

			// Locals
			// SUBROUTINE ARGUMENTS:

			// SUBROUTINE PARAMETER DEFINITIONS:
			// na

			// INTERFACE BLOCK SPECIFICATIONS:
			// na

			// DERIVED TYPE DEFINITIONS:
			// na

			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			//  INTEGER      :: ERVNum=0                   ! the stand alone ERV index
			static int ZoneNum(0); // zone index
			int j; // index
			int I; // index

			if (allocated(ZoneEquipList)) {
				ZoneNum = ZoneAirBalance(OutdoorNum).ZonePtr;
				ZoneAirBalance(OutdoorNum).OneTimeFlag = true;
				if (ZoneEquipList(ZoneNum).NumOfEquipTypes > 0) {
					for (I = 1; I <= ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) {
						if (ZoneEquipList(ZoneNum).EquipType_Num(I) == ERVStandAlone_Num) {
							++ZoneAirBalance(OutdoorNum).NumOfERVs;
						}
					}
					if (ZoneAirBalance(OutdoorNum).NumOfERVs > 0) {
						ZoneAirBalance(OutdoorNum).ERVInletNode.allocate(ZoneAirBalance(OutdoorNum).NumOfERVs);
						ZoneAirBalance(OutdoorNum).ERVExhaustNode.allocate(ZoneAirBalance(OutdoorNum).NumOfERVs);
						j = 1;
						for (I = 1; I <= ZoneEquipList(ZoneNum).NumOfEquipTypes; ++I) {
							if (ZoneEquipList(ZoneNum).EquipType_Num(I) == ERVStandAlone_Num) {
								ZoneAirBalance(OutdoorNum).ERVInletNode(j) = GetStandAloneERVOutAirNode(ZoneEquipList(ZoneNum).EquipIndex(I));
								ZoneAirBalance(OutdoorNum).ERVExhaustNode(j) = GetStandAloneERVReturnAirNode(ZoneEquipList(ZoneNum).EquipIndex(I));
								++j;
							}
						}
					}
				}
			}

	}

	void
	CalcZoneMixingFlowRateOfReceivingZone(int const ZoneNum, Real64 & ZoneMixingMassFlowRate)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   February 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the receiving zone mixing flow rate to ensures the zone
		// air mass balance.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na
		//

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHeatBalance::MassConservation;
		using DataHeatBalance::TotMixing;
		using DataHeatBalance::Mixing;
		using DataHeatBalFanSys::MixingMassFlowZone;

		// Enforce explicit typing of all variables in this routine

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int Loop;
		int MixingNum;
		int NumOfReceivingZoneMixingObjects;
		Real64 MixingMassFlowRate;         // current zone mixing mass flow rate, [kg/s]

		MixingMassFlowRate = 0.0;
		// distribute the total zone mixing flow rate to the source zones
		NumOfReceivingZoneMixingObjects = MassConservation(ZoneNum).NumReceivingZonesMixingObject;
		if (NumOfReceivingZoneMixingObjects > 0) {
			for (Loop = 1; Loop <= NumOfReceivingZoneMixingObjects; ++Loop) {
				MixingNum = MassConservation(ZoneNum).ZoneMixingReceivingPtr(Loop);
				Mixing(MixingNum).MixingMassFlowRate = MassConservation(ZoneNum).ZoneMixingReceivingFr(Loop) * ZoneMixingMassFlowRate;
				MixingMassFlowRate += Mixing(MixingNum).MixingMassFlowRate;
				CalcZoneMixingFlowRateOfSourceZone(Mixing(MixingNum).FromZone);
			}
		}
		MassConservation(ZoneNum).MixingMassFlowRate = MixingMassFlowRate;
		ZoneMixingMassFlowRate = MixingMassFlowRate;
	}

	void
	CalcZoneMixingFlowRateOfSourceZone(int const ZoneNum)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   February 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the zone mixing flow rate such that it ensures the zone
		// air mass balance.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na
		//

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHeatBalance::MassConservation;
		using DataHeatBalance::TotMixing;
		using DataHeatBalance::Mixing;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::MixingMassFlowZone;

		// Enforce explicit typing of all variables in this routine

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int MixingNum;
		int ZoneMixingNum;
		int NumOfSourceZoneMixingObjects;
		Real64 ZoneSourceMassFlowRate;     // current zone as a source mass flow rate for zone mixing in other zones, [kg/s]

		ZoneSourceMassFlowRate = 0.0;
		NumOfSourceZoneMixingObjects = MassConservation(ZoneNum).NumSourceZonesMixingObject;
		if (NumOfSourceZoneMixingObjects > 0) {
			for (ZoneMixingNum = 1; ZoneMixingNum <= NumOfSourceZoneMixingObjects; ++ZoneMixingNum) {
				MixingNum = MassConservation(ZoneNum).ZoneMixingSourcesPtr(ZoneMixingNum);
				for (Loop = 1; Loop <= TotMixing; ++Loop) {
					if (Loop == MixingNum) {
						ZoneSourceMassFlowRate += Mixing(Loop).MixingMassFlowRate;
					}
				}
			}
		}
		MassConservation(ZoneNum).MixingSourceMassFlowRate = ZoneSourceMassFlowRate;
	}

} // ZoneEquipmentManager

} // EnergyPlus
