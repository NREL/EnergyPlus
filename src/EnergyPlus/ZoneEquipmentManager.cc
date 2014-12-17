// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
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

	// Data
	//MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	FArray1D< Real64 > AvgData; // scratch array for storing averaged data
	FArray1D_int DefaultSimOrder;
	int NumOfTimeStepInDay; // number of zone time steps in a day
	bool GetZoneEquipmentInputFlag( true );

	//SUBROUTINE SPECIFICATIONS FOR MODULE ZoneEquipmentManager

	// Object Data
	FArray1D< SimulationOrder > PrioritySimOrder;

	// Functions

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
		int NumOfSizingTypes = 24; // number of sizing types

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
		static bool MyOneTimeFlag( true );
		static bool MyEnvrnFlag( true );
		int ZoneEquipType; // Type of zone equipment
		int TotalNumComp; // Total number of zone components of ZoneEquipType
		int ZoneCompNum; // Number/index of zone equipment component
		int ZoneEquipCount;
		// Flow

		if ( MyOneTimeFlag ) {
			MyOneTimeFlag = false;
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
		if ( MyEnvrnFlag && BeginEnvrnFlag ) {

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

			MyEnvrnFlag = false;

		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
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
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::DeadBandOrSetback;
		using DataLoopNode::Node;
		using DataHVACGlobals::SmallLoad;
		using DataHVACGlobals::SmallTempDiff;
		using General::RoundSigDigits;

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
		static bool MyOneTimeFlag( true );
		int ControlledZoneNum; // controlled zone index
		int ActualZoneNum; // index into Zone array (all zones)
		int SupplyAirNode; // node number of zone supply air node
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

		if ( MyOneTimeFlag ) {
			SetUpZoneSizingArrays();
			MyOneTimeFlag = false;
		}

		for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
			if ( ! ZoneEquipConfig( ControlledZoneNum ).IsControlled ) continue;
			ActualZoneNum = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).ActualZoneNum;
			NonAirSystemResponse( ActualZoneNum ) = 0.0;
			SysDepZoneLoads( ActualZoneNum ) = 0.0;

			InitSystemOutputRequired( ActualZoneNum, SysOutputProvided, LatOutputProvided );

			SupplyAirNode = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).SupplyAirNode;
			ZoneNode = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;

			// Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
			//                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
			if ( ! DeadBandOrSetback( ActualZoneNum ) && std::abs( ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired ) > SmallLoad ) {
				// Determine design supply air temperture and design supply air temperature difference
				if ( ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputRequired < 0.0 ) { // Cooling case
					// If the user specify the design cooling supply air temperature, then
					if ( CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).ZnCoolDgnSAMethod == SupplyAirTemperature ) {
						Temp = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolDesTemp;
						HumRat = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolDesHumRat;
						DeltaTemp = Temp - Node( ZoneNode ).Temp;
						// If the user specify the design cooling supply air temperature difference, then
					} else {
						DeltaTemp = -std::abs( CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolDesTempDiff );
						Temp = DeltaTemp + Node( ZoneNode ).Temp;
						HumRat = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolDesHumRat;
					}
				} else { // Heating Case
					// If the user specify the design heating supply air temperature, then
					if ( CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).ZnHeatDgnSAMethod == SupplyAirTemperature ) {
						Temp = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatDesTemp;
						HumRat = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatDesHumRat;
						DeltaTemp = Temp - Node( ZoneNode ).Temp;
						// If the user specify the design heating supply air temperature difference, then
					} else {
						DeltaTemp = std::abs( CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatDesTempDiff );
						Temp = DeltaTemp + Node( ZoneNode ).Temp;
						HumRat = CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatDesHumRat;
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

				if ( CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).SupplyAirAdjustFactor > 1.0 ) {
					MassFlowRate *= CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).SupplyAirAdjustFactor;
				}
			} else {

				Temp = Node( ZoneNode ).Temp;
				HumRat = Node( ZoneNode ).HumRat;
				Enthalpy = Node( ZoneNode ).Enthalpy;
				MassFlowRate = 0.0;

			}

			UpdateSystemOutputRequired( ActualZoneNum, SysOutputProvided, LatOutputProvided );

			if ( SysOutputProvided > 0.0 ) {
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatLoad = SysOutputProvided;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatMassFlow = MassFlowRate;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatZoneTemp = Node( ZoneNode ).Temp;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatZoneHumRat = Node( ZoneNode ).HumRat;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatOutTemp = OutDryBulbTemp;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatOutHumRat = OutHumRat;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolLoad = 0.0;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolMassFlow = 0.0;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolZoneTemp = 0.0;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolZoneHumRat = 0.0;
			} else {
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolLoad = -SysOutputProvided;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolMassFlow = MassFlowRate;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolZoneTemp = Node( ZoneNode ).Temp;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolZoneHumRat = Node( ZoneNode ).HumRat;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolOutTemp = OutDryBulbTemp;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolOutHumRat = OutHumRat;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatLoad = 0.0;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatMassFlow = 0.0;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatZoneTemp = 0.0;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatZoneHumRat = 0.0;
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
			if ( ReturnNode > 0 ) {
				RetTemp = Node( ReturnNode ).Temp;
			} else {
				RetTemp = Node( ZoneNode ).Temp;
			}
			if ( CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatLoad > 0.0 ) {
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatZoneRetTemp = RetTemp;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).HeatTstatTemp = TempZoneThermostatSetPoint( ActualZoneNum );
			} else {
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolZoneRetTemp = RetTemp;
				CalcZoneSizing( ControlledZoneNum, CurOverallSimDay ).CoolTstatTemp = TempZoneThermostatSetPoint( ActualZoneNum );
			}
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
			ZoneIndex = FindItemInList( ZoneSizingInput( ZoneSizIndex ).ZoneName, Zone.Name(), NumOfZones );
			if ( ZoneIndex == 0 ) {
				ShowSevereError( "SetUpZoneSizingArrays: Sizing:Zone=\"" + ZoneSizingInput( ZoneSizIndex ).ZoneName + "\" references unknown zone" );
				ErrorsFound = true;
			}
			if ( any( ZoneEquipConfig.IsControlled() ) ) {
				ZoneIndex = FindItemInList( ZoneSizingInput( ZoneSizIndex ).ZoneName, ZoneEquipConfig.ZoneName(), NumOfZones );
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
				ShowSevereError( "SetUpZoneSizingArrays: Zone Sizing is requested " "but there are no ZoneHVAC:EquipmentConnections statements." );
				ErrorsFound = true;
			}
		}
		if ( ErrorsFound ) {
			ShowFatalError( "SetUpZoneSizingArrays: Errors found in Sizing:Zone input" );
		}

		ZoneSizing.allocate( NumOfZones, TotDesDays + TotRunDesPersDays );
		FinalZoneSizing.allocate( NumOfZones );
		CalcZoneSizing.allocate( NumOfZones, TotDesDays + TotRunDesPersDays );
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
				ZoneSizing( CtrlZoneNum, DesDayNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
				ZoneSizing( CtrlZoneNum, DesDayNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
				if ( ZoneEquipConfig( CtrlZoneNum ).NumInletNodes > 0 ) {
					ZoneSizing( CtrlZoneNum, DesDayNum ).SupplyAirNode = ZoneEquipConfig( CtrlZoneNum ).InletNode( 1 );
				}
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
				if ( ZoneEquipConfig( CtrlZoneNum ).NumInletNodes > 0 ) {
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).SupplyAirNode = ZoneEquipConfig( CtrlZoneNum ).InletNode( 1 );
				}
				// For each Zone Sizing object, find the corresponding controlled zone
				ZoneSizNum = FindItemInList( ZoneEquipConfig( CtrlZoneNum ).ZoneName, ZoneSizingInput.ZoneName(), NumZoneSizingInput );
				if ( ZoneSizNum > 0 ) { // move data from zone sizing input
					ZoneSizing( CtrlZoneNum, DesDayNum ).ZnCoolDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnCoolDgnSAMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).ZnHeatDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnHeatDgnSAMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTemp = ZoneSizingInput( ZoneSizNum ).CoolDesTemp;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTemp = ZoneSizingInput( ZoneSizNum ).HeatDesTemp;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTempDiff = ZoneSizingInput( ZoneSizNum ).CoolDesTempDiff;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTempDiff = ZoneSizingInput( ZoneSizNum ).HeatDesTempDiff;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesHumRat = ZoneSizingInput( ZoneSizNum ).CoolDesHumRat;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesHumRat = ZoneSizingInput( ZoneSizNum ).HeatDesHumRat;
					ZoneSizing( CtrlZoneNum, DesDayNum ).OADesMethod = ZoneSizingInput( ZoneSizNum ).OADesMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPPer = ZoneSizingInput( ZoneSizNum ).DesOAFlowPPer;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesOAFlowPerArea;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlow = ZoneSizingInput( ZoneSizNum ).DesOAFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolAirDesMethod = ZoneSizingInput( ZoneSizNum ).CoolAirDesMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatAirDesMethod = ZoneSizingInput( ZoneSizNum ).HeatAirDesMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).InpDesCoolAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowPerArea;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowFrac;
					ZoneSizing( CtrlZoneNum, DesDayNum ).InpDesHeatAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowPerArea;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowFrac;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatSizingFactor = ZoneSizingInput( ZoneSizNum ).HeatSizingFactor;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolSizingFactor = ZoneSizingInput( ZoneSizNum ).CoolSizingFactor;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZnCoolDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnCoolDgnSAMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZnHeatDgnSAMethod = ZoneSizingInput( ZoneSizNum ).ZnHeatDgnSAMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTemp = ZoneSizingInput( ZoneSizNum ).CoolDesTemp;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTemp = ZoneSizingInput( ZoneSizNum ).HeatDesTemp;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTempDiff = ZoneSizingInput( ZoneSizNum ).CoolDesTempDiff;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTempDiff = ZoneSizingInput( ZoneSizNum ).HeatDesTempDiff;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesHumRat = ZoneSizingInput( ZoneSizNum ).CoolDesHumRat;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesHumRat = ZoneSizingInput( ZoneSizNum ).HeatDesHumRat;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).OADesMethod = ZoneSizingInput( ZoneSizNum ).OADesMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPPer = ZoneSizingInput( ZoneSizNum ).DesOAFlowPPer;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesOAFlowPerArea;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlow = ZoneSizingInput( ZoneSizNum ).DesOAFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolAirDesMethod = ZoneSizingInput( ZoneSizNum ).CoolAirDesMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatAirDesMethod = ZoneSizingInput( ZoneSizNum ).HeatAirDesMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).InpDesCoolAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowPerArea;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowFrac;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).InpDesHeatAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowPerArea;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowFrac;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatSizingFactor = ZoneSizingInput( ZoneSizNum ).HeatSizingFactor;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolSizingFactor = ZoneSizingInput( ZoneSizNum ).CoolSizingFactor;
				} else { // Every controlled zone must be simulated, so set missing inputs to the first
					//LKL I think this is sufficient for warning -- no need for array
					if ( DesDayNum == 1 ) {
						if ( ! isPulseZoneSizing ) {
							ShowWarningError( "SetUpZoneSizingArrays: Sizing for Zone=\"" + ZoneEquipConfig( CtrlZoneNum ).ZoneName + "\" will use Sizing:Zone specifications listed for Zone=\"" + ZoneSizingInput( 1 ).ZoneName + "\"." );
						}
						// Following needs to be implemented first:
						//          CALL ShowContinueError('  A better option would be to set up global ZoneList objects for Sizing:Zone objects.')
					}
					ZoneSizing( CtrlZoneNum, DesDayNum ).ZnCoolDgnSAMethod = ZoneSizingInput( 1 ).ZnCoolDgnSAMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).ZnHeatDgnSAMethod = ZoneSizingInput( 1 ).ZnHeatDgnSAMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTemp = ZoneSizingInput( 1 ).CoolDesTemp;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTemp = ZoneSizingInput( 1 ).HeatDesTemp;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTempDiff = ZoneSizingInput( 1 ).CoolDesTempDiff;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTempDiff = ZoneSizingInput( 1 ).HeatDesTempDiff;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesHumRat = ZoneSizingInput( 1 ).CoolDesHumRat;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesHumRat = ZoneSizingInput( 1 ).HeatDesHumRat;
					ZoneSizing( CtrlZoneNum, DesDayNum ).OADesMethod = ZoneSizingInput( 1 ).OADesMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPPer = ZoneSizingInput( 1 ).DesOAFlowPPer;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPerArea = ZoneSizingInput( 1 ).DesOAFlowPerArea;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlow = ZoneSizingInput( 1 ).DesOAFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolAirDesMethod = ZoneSizingInput( 1 ).CoolAirDesMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatAirDesMethod = ZoneSizingInput( 1 ).HeatAirDesMethod;
					ZoneSizing( CtrlZoneNum, DesDayNum ).InpDesCoolAirFlow = ZoneSizingInput( 1 ).DesCoolAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( 1 ).DesCoolMinAirFlowPerArea;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow = ZoneSizingInput( 1 ).DesCoolMinAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( 1 ).DesCoolMinAirFlowFrac;
					ZoneSizing( CtrlZoneNum, DesDayNum ).InpDesHeatAirFlow = ZoneSizingInput( 1 ).DesHeatAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( 1 ).DesHeatMaxAirFlowPerArea;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow = ZoneSizingInput( 1 ).DesHeatMaxAirFlow;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( 1 ).DesHeatMaxAirFlowFrac;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatSizingFactor = ZoneSizingInput( 1 ).HeatSizingFactor;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolSizingFactor = ZoneSizingInput( 1 ).CoolSizingFactor;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZnCoolDgnSAMethod = ZoneSizingInput( 1 ).ZnCoolDgnSAMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZnHeatDgnSAMethod = ZoneSizingInput( 1 ).ZnHeatDgnSAMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTemp = ZoneSizingInput( 1 ).CoolDesTemp;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTemp = ZoneSizingInput( 1 ).HeatDesTemp;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesTempDiff = ZoneSizingInput( 1 ).CoolDesTempDiff;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesTempDiff = ZoneSizingInput( 1 ).HeatDesTempDiff;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesHumRat = ZoneSizingInput( 1 ).CoolDesHumRat;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesHumRat = ZoneSizingInput( 1 ).HeatDesHumRat;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).OADesMethod = ZoneSizingInput( 1 ).OADesMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPPer = ZoneSizingInput( 1 ).DesOAFlowPPer;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlowPerArea = ZoneSizingInput( 1 ).DesOAFlowPerArea;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesOAFlow = ZoneSizingInput( 1 ).DesOAFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolAirDesMethod = ZoneSizingInput( 1 ).CoolAirDesMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatAirDesMethod = ZoneSizingInput( 1 ).HeatAirDesMethod;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).InpDesCoolAirFlow = ZoneSizingInput( 1 ).DesCoolAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowPerArea = ZoneSizingInput( 1 ).DesCoolMinAirFlowPerArea;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow = ZoneSizingInput( 1 ).DesCoolMinAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlowFrac = ZoneSizingInput( 1 ).DesCoolMinAirFlowFrac;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).InpDesHeatAirFlow = ZoneSizingInput( 1 ).DesHeatAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowPerArea = ZoneSizingInput( 1 ).DesHeatMaxAirFlowPerArea;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow = ZoneSizingInput( 1 ).DesHeatMaxAirFlow;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlowFrac = ZoneSizingInput( 1 ).DesHeatMaxAirFlowFrac;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatSizingFactor = ZoneSizingInput( 1 ).HeatSizingFactor;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolSizingFactor = ZoneSizingInput( 1 ).CoolSizingFactor;
				}
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatSetPtSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolSetPtSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq.allocate( NumOfTimeStepInDay );
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq.allocate( NumOfTimeStepInDay );
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq.allocate( NumOfTimeStepInDay );
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatSetPtSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolSetPtSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
				}
			}
		}

		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			FinalZoneSizing( CtrlZoneNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
			FinalZoneSizing( CtrlZoneNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			if ( ZoneEquipConfig( CtrlZoneNum ).NumInletNodes > 0 ) {
				FinalZoneSizing( CtrlZoneNum ).SupplyAirNode = ZoneEquipConfig( CtrlZoneNum ).InletNode( 1 );
			}
			CalcFinalZoneSizing( CtrlZoneNum ).ZoneName = ZoneEquipConfig( CtrlZoneNum ).ZoneName;
			CalcFinalZoneSizing( CtrlZoneNum ).ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			if ( ZoneEquipConfig( CtrlZoneNum ).NumInletNodes > 0 ) {
				CalcFinalZoneSizing( CtrlZoneNum ).SupplyAirNode = ZoneEquipConfig( CtrlZoneNum ).InletNode( 1 );
			}
			ZoneSizNum = FindItemInList( ZoneEquipConfig( CtrlZoneNum ).ZoneName, ZoneSizingInput.ZoneName(), NumZoneSizingInput );
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
			OAFromPeople = FinalZoneSizing( CtrlZoneNum ).DesOAFlowPPer * TotPeopleInZone;
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
				ZoneSizing( CtrlZoneNum, DesDayNum ).MinOA = FinalZoneSizing( CtrlZoneNum ).MinOA;
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).MinOA = CalcFinalZoneSizing( CtrlZoneNum ).MinOA;
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow2 = FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2;
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow2 = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow2;
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow = FinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow;
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMinAirFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMinAirFlow;
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow2 = FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2;
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow2 = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow2;
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow = FinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow;
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMaxAirFlow = CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMaxAirFlow;
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
					if ( allocated( ZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq ) ) {
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
						//not used directly in output report
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatSetPtSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
					if ( allocated( CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq ) ) {
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
					if ( allocated( ZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq ) ) {
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
						//not used directly in output report
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolSetPtSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
					if ( allocated( CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq ) ) {
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq( TimeStepIndex ) = 0.0;
						CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq( TimeStepIndex ) = 0.0;
					}
				}
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesDay = ""; // name of a cooling design day
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesDay = ""; // name of a heating design day

				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMassFlow = 0.0; // zone design heating air mass flow rate [kg/s]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMassFlow = 0.0; // zone design cooling air mass flow rate [kg/s]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatLoad = 0.0; // zone design heating load [W]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolLoad = 0.0; // zone design cooling load [W]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatDens = 0.0; // zone design heating air density [kg/m3]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolDens = 0.0; // zone design cooling air density [kg/m3]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatVolFlow = 0.0; // zone design heating air volume flow rate [m3/s]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolVolFlow = 0.0; // zone design cooling air volume flow rate [m3/s]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatVolFlowMax = 0.0; // zone design heating maximum air volume flow rate [m3/s]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolVolFlowMin = 0.0; // zone design cooling minimum air volume flow rate [m3/s]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInTemp = 0.0; // zone heating coil design air inlet temperature [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInTemp = 0.0; // zone cooling coil design air inlet temperature [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInHumRat = 0.0; // zone heating coil design air inlet humidity ratio [kg/kg]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInHumRat = 0.0; // zone cooling coil design air inlet humidity ratio [kg/kg]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInTempTU = 0.0; // zone heating coil design air inlet temperature (supply air)([C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInTempTU = 0.0; // zone cooling coil design air inlet temperature (supply air)[C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInHumRatTU = 0.0; // zone heating coil design air inlet humidity ratio
				ZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInHumRatTU = 0.0; // zone cooling coil design air inlet humidity ratio
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatMassFlow = 0.0; // current zone heating air mass flow rate (HVAC time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolMassFlow = 0.0; // current zone cooling air mass flow rate (HVAC time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoad = 0.0; // current zone heating load (HVAC time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoad = 0.0; // current zone heating load (HVAC time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTemp = 0.0; // current zone temperature (heating, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTemp = 0.0; // current outdoor temperature (heating, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTemp = 0.0; // current zone return temperature (heating, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTemp = 0.0; // current zone thermostat temperature (heating, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTemp = 0.0; // current zone temperature (cooling, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTemp = 0.0; // current Outdoor temperature (cooling, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTemp = 0.0; // current zone return temperature (cooling, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTemp = 0.0; // current zone thermostat temperature (cooling, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRat = 0.0; // current zone humidity ratio (heating, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRat = 0.0; // current zone humidity ratio (cooling, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRat = 0.0; // current outdoor humidity ratio (heating, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRat = 0.0; // current outdoor humidity ratio (cooling, time step)
				ZoneSizing( CtrlZoneNum, DesDayNum ).ZoneTempAtHeatPeak = 0.0; // zone temp at max heating [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).ZoneRetTempAtHeatPeak = 0.0; // zone return temp at max heating [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).OutTempAtHeatPeak = 0.0; // outdoor temperature at max heating [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).ZoneTempAtCoolPeak = 0.0; // zone temp at max cooling [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).ZoneRetTempAtCoolPeak = 0.0; // zone return temp at max cooling [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).OutTempAtCoolPeak = 0.0; // outdoor temperature at max cooling [C]
				ZoneSizing( CtrlZoneNum, DesDayNum ).ZoneHumRatAtHeatPeak = 0.0; // zone humidity ratio at max heating [kg/kg]
				ZoneSizing( CtrlZoneNum, DesDayNum ).ZoneHumRatAtCoolPeak = 0.0; // zone humidity ratio at max cooling [kg/kg]
				ZoneSizing( CtrlZoneNum, DesDayNum ).OutHumRatAtHeatPeak = 0.0; // outdoor humidity at max heating [kg/kg]
				ZoneSizing( CtrlZoneNum, DesDayNum ).OutHumRatAtCoolPeak = 0.0; // outdoor humidity at max cooling [kg/kg]
				ZoneSizing( CtrlZoneNum, DesDayNum ).TimeStepNumAtHeatMax = 0; // time step number (in day) at Heating peak
				ZoneSizing( CtrlZoneNum, DesDayNum ).TimeStepNumAtCoolMax = 0; // time step number (in day) at cooling peak
				ZoneSizing( CtrlZoneNum, DesDayNum ).HeatDDNum = 0; // design day index of design day causing heating peak
				ZoneSizing( CtrlZoneNum, DesDayNum ).CoolDDNum = 0; // design day index of design day causing heating peak
				ZoneSizing( CtrlZoneNum, DesDayNum ).cHeatDDDate = ""; // date of design day causing heating peak
				ZoneSizing( CtrlZoneNum, DesDayNum ).cCoolDDDate = ""; // date of design day causing cooling peak

				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDesDay = ""; // name of a cooling design day
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDesDay = ""; // name of a heating design day

				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatMassFlow = 0.0; // zone design heating air mass flow rate [kg/s]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolMassFlow = 0.0; // zone design cooling air mass flow rate [kg/s]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatLoad = 0.0; // zone design heating load [W]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolLoad = 0.0; // zone design cooling load [W]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatDens = 0.0; // zone design heating air density [kg/m3]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolDens = 0.0; // zone design cooling air density [kg/m3]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatVolFlow = 0.0; // zone design heating air volume flow rate [m3/s]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolVolFlow = 0.0; // zone design cooling air volume flow rate [m3/s]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatVolFlowMax = 0.0; // zone design heating maximum air volume flow rate [m3/s]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolVolFlowMin = 0.0; // zone design cooling minimum air volume flow rate [m3/s]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInTemp = 0.0; // zone heating coil design air inlet temperature [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInTemp = 0.0; // zone cooling coil design air inlet temperature [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInHumRat = 0.0; // zone heating coil design air inlet humidity ratio [kg/kg]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInHumRat = 0.0; // zone cooling coil design air inlet humidity ratio [kg/kg]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInTempTU = 0.0; // zone heating coil design air inlet temperature (supply air)([C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInTempTU = 0.0; // zone cooling coil design air inlet temperature (supply air)[C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesHeatCoilInHumRatTU = 0.0; // zone heating coil design air inlet humidity ratio
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).DesCoolCoilInHumRatTU = 0.0; // zone cooling coil design air inlet humidity ratio
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatMassFlow = 0.0; // current zone heating air mass flow rate (HVAC time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolMassFlow = 0.0; // current zone cooling air mass flow rate (HVAC time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoad = 0.0; // current zone heating load (HVAC time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoad = 0.0; // current zone heating load (HVAC time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTemp = 0.0; // current zone temperature (heating, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTemp = 0.0; // current outdoor temperature (heating, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTemp = 0.0; // current zone return temperature (heating, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTemp = 0.0; // current zone thermostat temperature (heating, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTemp = 0.0; // current zone temperature (cooling, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTemp = 0.0; // current Outdoor temperature (cooling, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTemp = 0.0; // current zone return temperature (cooling, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTemp = 0.0; // current zone Tstat temperature (cooling, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRat = 0.0; // current zone humidity ratio (heating, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRat = 0.0; // current zone humidity ratio (cooling, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRat = 0.0; // current outdoor humidity ratio (heating, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRat = 0.0; // current outdoor humidity ratio (cooling, time step)
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZoneTempAtHeatPeak = 0.0; // zone temp at max heating [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZoneRetTempAtHeatPeak = 0.0; // zone return temp at max heating [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).OutTempAtHeatPeak = 0.0; // outdoor temperature at max heating [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZoneTempAtCoolPeak = 0.0; // zone temp at max cooling [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZoneRetTempAtCoolPeak = 0.0; // zone return temp at max cooling [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).OutTempAtCoolPeak = 0.0; // outdoor temperature at max cooling [C]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZoneHumRatAtHeatPeak = 0.0; // zone humidity ratio at max heating [kg/kg]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).ZoneHumRatAtCoolPeak = 0.0; // zone humidity ratio at max cooling [kg/kg]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).OutHumRatAtHeatPeak = 0.0; // outdoor humidity at max heating [kg/kg]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).OutHumRatAtCoolPeak = 0.0; // outdoor humidity at max cooling [kg/kg]
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).TimeStepNumAtHeatMax = 0; // time step number (in day) at Heating peak
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).TimeStepNumAtCoolMax = 0; // time step number (in day) at cooling peak
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatDDNum = 0; // design day index of design day causing heating peak
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolDDNum = 0; // design day index of design day causing heating peak
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).cHeatDDDate = ""; // date of design day causing heating peak
				CalcZoneSizing( CtrlZoneNum, DesDayNum ).cCoolDDDate = ""; // date of design day causing cooling peak
			}
		}
		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
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
		using DataGlobals::TimeStepZone;
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::BeginDay;
		using DataGlobals::DuringDay;
		using DataGlobals::EndDay;
		using DataGlobals::EndZoneSizingCalc;
		using DataGlobals::MinutesPerTimeStep;
		using DataGlobals::OutputFileZoneSizing;
		using DataGlobals::OutputFileDebug;
		using DataGlobals::emsCallFromZoneSizing;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::isPulseZoneSizing;
		using DataGlobals::OutputFileZonePulse;
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

				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolDesDay = EnvironmentName;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatDesDay = EnvironmentName;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatDens = StdRhoAir;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolDens = StdRhoAir;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatDDNum = CurOverallSimDay;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolDDNum = CurOverallSimDay;

			}

		} else if ( SELECT_CASE_var == DuringDay ) {

			TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				if ( ZoneThermostatSetPointHi( CtrlZoneNum ) > 0.0 && ZoneThermostatSetPointHi( CtrlZoneNum ) > ZoneSizThermSetPtHi( CtrlZoneNum ) ) {
					ZoneSizThermSetPtHi( CtrlZoneNum ) = ZoneThermostatSetPointHi( CtrlZoneNum );
				}
				if ( ZoneThermostatSetPointLo( CtrlZoneNum ) > 0.0 && ZoneThermostatSetPointLo( CtrlZoneNum ) < ZoneSizThermSetPtLo( CtrlZoneNum ) ) {
					ZoneSizThermSetPtLo( CtrlZoneNum ) = ZoneThermostatSetPointLo( CtrlZoneNum );
				}
				ZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatSetPtSeq( TimeStepInDay ) = ZoneThermostatSetPointLo( CtrlZoneNum );
				ZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatTstatTempSeq( TimeStepInDay ) = TempZoneThermostatSetPoint( CtrlZoneNum );
				ZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolSetPtSeq( TimeStepInDay ) = ZoneThermostatSetPointHi( CtrlZoneNum );
				ZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolTstatTempSeq( TimeStepInDay ) = TempZoneThermostatSetPoint( CtrlZoneNum );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatFlowSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatMassFlow * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatLoadSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatLoad * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneTempSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneTemp * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutTempSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutTemp * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneRetTempSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneRetTemp * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatTstatTempSeq( TimeStepInDay ) = TempZoneThermostatSetPoint( CtrlZoneNum );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneHumRat * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutHumRat * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolFlowSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolMassFlow * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolLoadSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolLoad * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneTempSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneTemp * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutTempSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutTemp * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneRetTempSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneRetTemp * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolTstatTempSeq( TimeStepInDay ) = TempZoneThermostatSetPoint( CtrlZoneNum );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneHumRat * FracTimeStepZone;
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutHumRatSeq( TimeStepInDay ) += CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutHumRat * FracTimeStepZone;
			}

		} else if ( SELECT_CASE_var == EndDay ) {

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolFlowSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolFlowSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolLoadSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolLoadSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatFlowSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatFlowSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatLoadSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatLoadSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneRetTempSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneRetTempSeq = AvgData;
			}
			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AvgData = 0.0;
				MovingAvg( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneRetTempSeq, NumOfTimeStepInDay, NumTimeStepsInAvg, AvgData );
				CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneRetTempSeq = AvgData;
			}

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {

				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;

				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					if ( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatLoadSeq( TimeStepIndex ) > CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatLoad ) {
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatLoad = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatLoadSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatMassFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatFlowSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneTempAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutTempAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneRetTempAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneRetTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatTstatTemp = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatTstatTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneHumRatAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutHumRatAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).TimeStepNumAtHeatMax = TimeStepIndex;
					}
				}
				if ( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatMassFlow > 0.0 ) {
					CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatVolFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatMassFlow / CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatDens;
					OAFrac = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).MinOA / max( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatVolFlow, SmallMassFlow );
					OAFrac = min( 1.0, max( 0.0, OAFrac ) );
					TimeStepAtPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).TimeStepNumAtHeatMax;
					CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatCoilInTemp = OAFrac * DesDayWeath( CurOverallSimDay ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneTempAtHeatPeak;
					CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatCoilInHumRat = OAFrac * DesDayWeath( CurOverallSimDay ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneHumRatAtHeatPeak;
				}
				for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
					if ( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolLoadSeq( TimeStepIndex ) > CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolLoad ) {
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolLoad = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolLoadSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolMassFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolFlowSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneTempAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutTempAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneRetTempAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneRetTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolTstatTemp = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolTstatTempSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneHumRatAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutHumRatAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutHumRatSeq( TimeStepIndex );
						CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).TimeStepNumAtCoolMax = TimeStepIndex;
					}
				}
				if ( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolMassFlow > 0.0 ) {
					CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolVolFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolMassFlow / CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolDens;
					OAFrac = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).MinOA / max( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolVolFlow, SmallMassFlow );
					OAFrac = min( 1.0, max( 0.0, OAFrac ) );
					TimeStepAtPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).TimeStepNumAtCoolMax;
					CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolCoilInTemp = OAFrac * DesDayWeath( CurOverallSimDay ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneTempAtCoolPeak;
					CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolCoilInHumRat = OAFrac * DesDayWeath( CurOverallSimDay ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneHumRatAtCoolPeak;
				}
				if ( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatVolFlow > CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow ) {
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatVolFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatVolFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatLoad = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatLoad;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatMassFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatMassFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatDesDay = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatDesDay;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatDens = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatDens;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatFlowSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatFlowSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatLoadSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatLoadSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatOutTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneRetTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneRetTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatTstatTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatTstatTemp = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatTstatTemp;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatZoneHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatOutHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneTempAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutTempAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutTempAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneRetTempAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneHumRatAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutHumRatAtHeatPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutHumRatAtHeatPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).HeatDDNum = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).HeatDDNum;
					CalcFinalZoneSizing( CtrlZoneNum ).cHeatDDDate = DesDayWeath( CurOverallSimDay ).DateString;
					CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).TimeStepNumAtHeatMax;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTemp = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatCoilInTemp;
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRat = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesHeatCoilInHumRat;
				} else {
					CalcFinalZoneSizing( CtrlZoneNum ).DesHeatDens = StdRhoAir;
				}
				if ( CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolVolFlow > CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow ) {
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolVolFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolLoad = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolLoad;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolMassFlow = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolMassFlow;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolDesDay = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolDesDay;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolDens = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolDens;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolFlowSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolFlowSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolLoadSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolLoadSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneRetTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolTstatTempSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolTstatTemp = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolTstatTemp;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneHumRatSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolZoneHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolOutHumRatSeq = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolOutHumRatSeq;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneTempAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutTempAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutTempAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneRetTempAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneRetTempAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).ZoneHumRatAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).OutHumRatAtCoolPeak = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).OutHumRatAtCoolPeak;
					CalcFinalZoneSizing( CtrlZoneNum ).CoolDDNum = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).CoolDDNum;
					CalcFinalZoneSizing( CtrlZoneNum ).cCoolDDDate = DesDayWeath( CurOverallSimDay ).DateString;
					CalcFinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).TimeStepNumAtCoolMax;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInTemp = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolCoilInTemp;
					CalcFinalZoneSizing( CtrlZoneNum ).DesCoolCoilInHumRat = CalcZoneSizing( CtrlZoneNum, CurOverallSimDay ).DesCoolCoilInHumRat;
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

					// Should this be done only if there is a cooling load? Or would this message help dermine why there was no load?
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
								ShowWarningError( "UpdateZoneSizing: Cooling supply air temperature (calculated) within 5C of" " zone temperature" );
							} else {
								ShowSevereError( "UpdateZoneSizing: Cooling supply air temperature (calculated) within 2C of" " zone temperature" );
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
							if ( SupplyTemp > CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak ) ShowContinueError( "...Note: supply air temperature should be less than zone" " temperature during cooling air flow calculations" );
						} else if ( std::abs( DeltaTemp ) > SmallTempDiff && SupplyTemp > CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak ) {
							ShowSevereError( "UpdateZoneSizing: Supply air temperature is greater than zone" " temperature during cooling air flow calculations" );
							ShowContinueError( "...zone temperature            = " + RoundSigDigits( CalcFinalZoneSizing( I ).ZoneTempAtCoolPeak, 3 ) + " C" );
							ShowContinueError( "...supply air temperature      = " + RoundSigDigits( SupplyTemp, 3 ) + " C" );
							ShowContinueError( "...occurs in zone              = " + CalcFinalZoneSizing( I ).ZoneName );
						}
					}
					// Should this be done only if there is a heating load? Or would this message help dermine why there was no load?
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
								ShowWarningError( "UpdateZoneSizing: Heating supply air temperature (calculated) within 5C of" " zone temperature" );
							} else {
								ShowSevereError( "UpdateZoneSizing: Heating supply air temperature (calculated) within 2C of" " zone temperature" );
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
							if ( SupplyTemp < CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak ) ShowContinueError( "...Note: supply air temperature should be greater than zone" " temperature during heating air flow calculations" );
						} else if ( std::abs( DeltaTemp ) > SmallTempDiff && SupplyTemp < CalcFinalZoneSizing( I ).ZoneTempAtHeatPeak ) {
							ShowSevereError( "UpdateZoneSizing: Supply air temperature is less than zone" " temperature during heating air flow calculations" );
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

			ZoneSizing.CoolDesDay() = CalcZoneSizing.CoolDesDay();
			ZoneSizing.HeatDesDay() = CalcZoneSizing.HeatDesDay();
			ZoneSizing.DesHeatDens() = CalcZoneSizing.DesHeatDens();
			ZoneSizing.DesCoolDens() = CalcZoneSizing.DesCoolDens();
			ZoneSizing.HeatDDNum() = CalcZoneSizing.HeatDDNum();
			ZoneSizing.CoolDDNum() = CalcZoneSizing.CoolDDNum();

			ZoneSizing.DesHeatLoad() = CalcZoneSizing.DesHeatLoad();
			ZoneSizing.DesHeatMassFlow() = CalcZoneSizing.DesHeatMassFlow();
			ZoneSizing.ZoneTempAtHeatPeak() = CalcZoneSizing.ZoneTempAtHeatPeak();
			ZoneSizing.OutTempAtHeatPeak() = CalcZoneSizing.OutTempAtHeatPeak();
			ZoneSizing.ZoneRetTempAtHeatPeak() = CalcZoneSizing.ZoneRetTempAtHeatPeak();
			ZoneSizing.ZoneHumRatAtHeatPeak() = CalcZoneSizing.ZoneHumRatAtHeatPeak();
			ZoneSizing.OutHumRatAtHeatPeak() = CalcZoneSizing.OutHumRatAtHeatPeak();
			ZoneSizing.TimeStepNumAtHeatMax() = CalcZoneSizing.TimeStepNumAtHeatMax();
			ZoneSizing.DesHeatVolFlow() = CalcZoneSizing.DesHeatVolFlow();
			ZoneSizing.DesHeatCoilInTemp() = CalcZoneSizing.DesHeatCoilInTemp();
			ZoneSizing.DesHeatCoilInHumRat() = CalcZoneSizing.DesHeatCoilInHumRat();

			ZoneSizing.DesCoolLoad() = CalcZoneSizing.DesCoolLoad();
			ZoneSizing.DesCoolMassFlow() = CalcZoneSizing.DesCoolMassFlow();
			ZoneSizing.ZoneTempAtCoolPeak() = CalcZoneSizing.ZoneTempAtCoolPeak();
			ZoneSizing.OutTempAtCoolPeak() = CalcZoneSizing.OutTempAtCoolPeak();
			ZoneSizing.ZoneRetTempAtCoolPeak() = CalcZoneSizing.ZoneRetTempAtCoolPeak();
			ZoneSizing.ZoneHumRatAtCoolPeak() = CalcZoneSizing.ZoneHumRatAtCoolPeak();
			ZoneSizing.OutHumRatAtCoolPeak() = CalcZoneSizing.OutHumRatAtCoolPeak();
			ZoneSizing.TimeStepNumAtCoolMax() = CalcZoneSizing.TimeStepNumAtCoolMax();
			ZoneSizing.DesCoolVolFlow() = CalcZoneSizing.DesCoolVolFlow();
			ZoneSizing.DesCoolCoilInTemp() = CalcZoneSizing.DesCoolCoilInTemp();
			ZoneSizing.DesCoolCoilInHumRat() = CalcZoneSizing.DesCoolCoilInHumRat();

			FinalZoneSizing.CoolDesDay() = CalcFinalZoneSizing.CoolDesDay();
			FinalZoneSizing.HeatDesDay() = CalcFinalZoneSizing.HeatDesDay();
			FinalZoneSizing.DesHeatDens() = CalcFinalZoneSizing.DesHeatDens();
			FinalZoneSizing.DesCoolDens() = CalcFinalZoneSizing.DesCoolDens();
			FinalZoneSizing.HeatDDNum() = CalcFinalZoneSizing.HeatDDNum();
			FinalZoneSizing.CoolDDNum() = CalcFinalZoneSizing.CoolDDNum();

			FinalZoneSizing.DesHeatLoad() = CalcFinalZoneSizing.DesHeatLoad();
			FinalZoneSizing.DesHeatMassFlow() = CalcFinalZoneSizing.DesHeatMassFlow();
			FinalZoneSizing.ZoneTempAtHeatPeak() = CalcFinalZoneSizing.ZoneTempAtHeatPeak();
			FinalZoneSizing.OutTempAtHeatPeak() = CalcFinalZoneSizing.OutTempAtHeatPeak();
			FinalZoneSizing.ZoneRetTempAtHeatPeak() = CalcFinalZoneSizing.ZoneRetTempAtHeatPeak();
			FinalZoneSizing.ZoneHumRatAtHeatPeak() = CalcFinalZoneSizing.ZoneHumRatAtHeatPeak();
			FinalZoneSizing.OutHumRatAtHeatPeak() = CalcFinalZoneSizing.OutHumRatAtHeatPeak();
			FinalZoneSizing.TimeStepNumAtHeatMax() = CalcFinalZoneSizing.TimeStepNumAtHeatMax();
			FinalZoneSizing.DesHeatVolFlow() = CalcFinalZoneSizing.DesHeatVolFlow();
			FinalZoneSizing.DesHeatCoilInTemp() = CalcFinalZoneSizing.DesHeatCoilInTemp();
			FinalZoneSizing.DesHeatCoilInHumRat() = CalcFinalZoneSizing.DesHeatCoilInHumRat();

			FinalZoneSizing.DesCoolLoad() = CalcFinalZoneSizing.DesCoolLoad();
			FinalZoneSizing.DesCoolMassFlow() = CalcFinalZoneSizing.DesCoolMassFlow();
			FinalZoneSizing.ZoneTempAtCoolPeak() = CalcFinalZoneSizing.ZoneTempAtCoolPeak();
			FinalZoneSizing.OutTempAtCoolPeak() = CalcFinalZoneSizing.OutTempAtCoolPeak();
			FinalZoneSizing.ZoneRetTempAtCoolPeak() = CalcFinalZoneSizing.ZoneRetTempAtCoolPeak();
			FinalZoneSizing.ZoneHumRatAtCoolPeak() = CalcFinalZoneSizing.ZoneHumRatAtCoolPeak();
			FinalZoneSizing.OutHumRatAtCoolPeak() = CalcFinalZoneSizing.OutHumRatAtCoolPeak();
			FinalZoneSizing.TimeStepNumAtCoolMax() = CalcFinalZoneSizing.TimeStepNumAtCoolMax();
			FinalZoneSizing.DesCoolVolFlow() = CalcFinalZoneSizing.DesCoolVolFlow();
			FinalZoneSizing.DesCoolCoilInTemp() = CalcFinalZoneSizing.DesCoolCoilInTemp();
			FinalZoneSizing.DesCoolCoilInHumRat() = CalcFinalZoneSizing.DesCoolCoilInHumRat();

			for ( DesDayNum = 1; DesDayNum <= TotDesDays + TotRunDesPersDays; ++DesDayNum ) {
				for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
					if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
					for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatFlowSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatLoadSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolFlowSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolLoadSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneRetTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatTstatTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatZoneHumRatSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).HeatOutHumRatSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneRetTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolTstatTempSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolZoneHumRatSeq( TimeStepIndex );
						ZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq( TimeStepIndex ) = CalcZoneSizing( CtrlZoneNum, DesDayNum ).CoolOutHumRatSeq( TimeStepIndex );
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
					FinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatTstatTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatZoneHumRatSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).HeatOutHumRatSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolOutTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolZoneRetTempSeq( TimeStepIndex );
					FinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex ) = CalcFinalZoneSizing( CtrlZoneNum ).CoolTstatTempSeq( TimeStepIndex );
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
						if ( ZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow > 0.0 ) {
							TimeStepAtPeak = ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtCoolMax;
							ZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow = CalcZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow * TotCoolSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).DesCoolMassFlow = CalcZoneSizing( CtrlZoneNum, DDNum ).DesCoolMassFlow * TotCoolSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).DesCoolLoad = CalcZoneSizing( CtrlZoneNum, DDNum ).DesCoolLoad * TotCoolSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).CoolFlowSeq = CalcZoneSizing( CtrlZoneNum, DDNum ).CoolFlowSeq * TotCoolSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).CoolLoadSeq = CalcZoneSizing( CtrlZoneNum, DDNum ).CoolLoadSeq * TotCoolSizMult;
							OAFrac = ZoneSizing( CtrlZoneNum, DDNum ).MinOA / ZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow;
							OAFrac = min( 1.0, max( 0.0, OAFrac ) );
							ZoneSizing( CtrlZoneNum, DDNum ).DesCoolCoilInTemp = OAFrac * DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( CtrlZoneNum, DDNum ).ZoneTempAtCoolPeak;
							ZoneSizing( CtrlZoneNum, DDNum ).DesCoolCoilInHumRat = OAFrac * DesDayWeath( DDNum ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( CtrlZoneNum, DDNum ).ZoneHumRatAtCoolPeak;
						} else {
							ZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow = ZoneSizing( CtrlZoneNum, DDNum ).InpDesCoolAirFlow;
							ZoneSizing( CtrlZoneNum, DDNum ).DesCoolMassFlow = ZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow * ZoneSizing( CtrlZoneNum, DDNum ).DesCoolDens;
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
					MaxOfMinCoolVolFlow = max( ZoneSizing( CtrlZoneNum, DDNum ).DesCoolMinAirFlow, ZoneSizing( CtrlZoneNum, DDNum ).DesCoolMinAirFlow, ZoneSizing( CtrlZoneNum, DDNum ).MinOA );
					MaxOfMinCoolMassFlow = MaxOfMinCoolVolFlow * ZoneSizing( CtrlZoneNum, DDNum ).DesCoolDens;
					if ( MaxOfMinCoolVolFlow > ZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow ) {
						ZoneSizing( CtrlZoneNum, DDNum ).DesCoolVolFlow = MaxOfMinCoolVolFlow;
						ZoneSizing( CtrlZoneNum, DDNum ).DesCoolMassFlow = MaxOfMinCoolMassFlow;
					}
					for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
						if ( MaxOfMinCoolMassFlow > ZoneSizing( CtrlZoneNum, DDNum ).CoolFlowSeq( TimeStepIndex ) ) {
							ZoneSizing( CtrlZoneNum, DDNum ).CoolFlowSeq( TimeStepIndex ) = MaxOfMinCoolMassFlow;
						}
					}
				}
				// IF cooling flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
				// check for flow rate having been set (by MinOA or other min) but no timestep at max
				//        IF (FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow > 0.0d0 .AND. &
				if ( ( FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax == 0 || FinalZoneSizing( CtrlZoneNum ).CoolDDNum == 0 ) ) {
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtCoolMax = 1;
						TimeStepAtPeak = ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtCoolMax;
						for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
							if ( DesDayWeath( DDNum ).Temp( TimeStepIndex ) > DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) ) {
								TimeStepAtPeak = TimeStepIndex;
							}
						}
						ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtCoolMax = TimeStepAtPeak;
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = 1;
					FinalZoneSizing( CtrlZoneNum ).CoolDDNum = 1;
					TimeStepAtPeakF = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax;
					DDNumF = FinalZoneSizing( CtrlZoneNum ).CoolDDNum;
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						TimeStepAtPeak = ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtCoolMax;
						if ( DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) > DesDayWeath( DDNumF ).Temp( TimeStepAtPeakF ) ) {
							DDNumF = DDNum;
							TimeStepAtPeakF = TimeStepAtPeak;
						}
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtCoolMax = TimeStepAtPeakF;
					FinalZoneSizing( CtrlZoneNum ).CoolDDNum = DDNumF;
					FinalZoneSizing( CtrlZoneNum ).CoolDesDay = ZoneSizing( CtrlZoneNum, DDNumF ).CoolDesDay;

					// initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
					if ( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak == 0.0 ) {
						FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak = ZoneSizing( CtrlZoneNum, DDNumF ).DesCoolSetPtSeq( TimeStepAtPeakF );
						FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = ZoneSizing( CtrlZoneNum, DDNumF ).CoolZoneHumRatSeq( TimeStepAtPeakF );
						if ( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak > 0.0 ) {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = min( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak, PsyWFnTdpPb( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtCoolPeak, StdBaroPress, RoutineName ) );

						} else {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtCoolPeak = ZoneSizing( CtrlZoneNum, DDNumF ).CoolDesHumRat;
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
						if ( ZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow > 0.0 ) {
							TimeStepAtPeak = ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtHeatMax;
							ZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow = CalcZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow * TotHeatSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).DesHeatMassFlow = CalcZoneSizing( CtrlZoneNum, DDNum ).DesHeatMassFlow * TotHeatSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).DesHeatLoad = CalcZoneSizing( CtrlZoneNum, DDNum ).DesHeatLoad * TotHeatSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).HeatFlowSeq = CalcZoneSizing( CtrlZoneNum, DDNum ).HeatFlowSeq * TotHeatSizMult;
							ZoneSizing( CtrlZoneNum, DDNum ).HeatLoadSeq = CalcZoneSizing( CtrlZoneNum, DDNum ).HeatLoadSeq * TotHeatSizMult;
							OAFrac = ZoneSizing( CtrlZoneNum, DDNum ).MinOA / ZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow;
							OAFrac = min( 1.0, max( 0.0, OAFrac ) );
							ZoneSizing( CtrlZoneNum, DDNum ).DesHeatCoilInTemp = OAFrac * DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( CtrlZoneNum, DDNum ).ZoneTempAtHeatPeak;
							ZoneSizing( CtrlZoneNum, DDNum ).DesHeatCoilInHumRat = OAFrac * DesDayWeath( DDNum ).HumRat( TimeStepAtPeak ) + ( 1.0 - OAFrac ) * ZoneSizing( CtrlZoneNum, DDNum ).ZoneHumRatAtHeatPeak;
						} else {
							ZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow = ZoneSizing( CtrlZoneNum, DDNum ).InpDesHeatAirFlow;
							ZoneSizing( CtrlZoneNum, DDNum ).DesHeatMassFlow = ZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow * ZoneSizing( CtrlZoneNum, DDNum ).DesHeatDens;
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
					MinOAMass = ZoneSizing( CtrlZoneNum, DDNum ).MinOA * ZoneSizing( CtrlZoneNum, DDNum ).DesHeatDens;
					if ( ZoneSizing( CtrlZoneNum, DDNum ).MinOA > ZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow ) {
						ZoneSizing( CtrlZoneNum, DDNum ).DesHeatVolFlow = ZoneSizing( CtrlZoneNum, DDNum ).MinOA;
						ZoneSizing( CtrlZoneNum, DDNum ).DesHeatMassFlow = MinOAMass;
					}
					for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
						if ( MinOAMass > ZoneSizing( CtrlZoneNum, DDNum ).HeatFlowSeq( TimeStepIndex ) ) {
							ZoneSizing( CtrlZoneNum, DDNum ).HeatFlowSeq( TimeStepIndex ) = MinOAMass;
						}
					}
				}
				// IF heating flow rate is 0, this data may be used to size a HP so initialize DDNum, TimeStepatPeak, and sizing data (end of IF)
				// check for flow rate having been set (by MinOA or other min) but no timestep at max
				//        IF (FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow > 0.0d0 .AND. &
				if ( ( FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax == 0 || FinalZoneSizing( CtrlZoneNum ).HeatDDNum == 0 ) ) {
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtHeatMax = 1;
						TimeStepAtPeak = ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtHeatMax;
						for ( TimeStepIndex = 1; TimeStepIndex <= NumOfTimeStepInDay; ++TimeStepIndex ) {
							if ( DesDayWeath( DDNum ).Temp( TimeStepIndex ) < DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) ) {
								TimeStepAtPeak = TimeStepIndex;
							}
						}
						ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtHeatMax = TimeStepAtPeak;
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = 1;
					FinalZoneSizing( CtrlZoneNum ).HeatDDNum = 1;
					TimeStepAtPeakF = FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax;
					DDNumF = FinalZoneSizing( CtrlZoneNum ).HeatDDNum;
					for ( DDNum = 1; DDNum <= TotDesDays + TotRunDesPersDays; ++DDNum ) {
						TimeStepAtPeak = ZoneSizing( CtrlZoneNum, DDNum ).TimeStepNumAtHeatMax;
						if ( DesDayWeath( DDNum ).Temp( TimeStepAtPeak ) < DesDayWeath( DDNumF ).Temp( TimeStepAtPeakF ) ) {
							DDNumF = DDNum;
							TimeStepAtPeakF = TimeStepAtPeak;
						}
					}
					FinalZoneSizing( CtrlZoneNum ).TimeStepNumAtHeatMax = TimeStepAtPeakF;
					FinalZoneSizing( CtrlZoneNum ).HeatDDNum = DDNumF;
					FinalZoneSizing( CtrlZoneNum ).HeatDesDay = ZoneSizing( CtrlZoneNum, DDNumF ).HeatDesDay;

					// initialize sizing conditions if they have not been set (i.e., no corresponding load) to zone condition
					if ( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak == 0.0 ) {
						FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak = ZoneSizing( CtrlZoneNum, DDNumF ).DesHeatSetPtSeq( TimeStepAtPeakF );
						FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = ZoneSizing( CtrlZoneNum, DDNumF ).HeatZoneHumRatSeq( TimeStepAtPeakF );
						if ( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak > 0.0 ) {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = min( FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak, PsyWFnTdpPb( FinalZoneSizing( CtrlZoneNum ).ZoneTempAtHeatPeak, StdBaroPress, RoutineName ) );
						} else {
							FinalZoneSizing( CtrlZoneNum ).ZoneHumRatAtHeatPeak = ZoneSizing( CtrlZoneNum, DDNumF ).HeatDesHumRat;
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
		using DataAirLoop::AirLoopControlInfo;
		using ElectricBaseboardRadiator::SimElecBaseboard;
		using HVACVariableRefrigerantFlow::SimulateVRF;
		using RefrigeratedCase::SimAirChillerSet;
		using UserDefinedComponents::SimZoneAirUserDefined;
		using SystemAvailabilityManager::GetZoneEqAvailabilityManager;
		using DataGlobals::isPulseZoneSizing;
		using EvaporativeCoolers::SimZoneEvaporativeCoolerUnit;
		using HVACUnitarySystem::SimUnitarySystem;
		using DataHeatBalance::Mixing;
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
		static FArray1D_bool DirectAirAndAirTerminalWarningIssued; // only warn once for each zone with problems

		// Determine flow rate and temperature of supply air based on type of damper

		bool AdjustZoneMixingFlowFlag( true );  // holds zone mixing flow calc status
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
			 CalcAirFlowSimple( 0, AdjustZoneMixingFlowFlag );
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

				} else if ( SELECT_CASE_var == HPWaterHeater_Num ) { // 'WaterHeater:HeatPump'
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

				ZoneEquipConfig( ControlledZoneNum ).ZoneExh += UnbalExhMassFlow;
				ZoneEquipConfig( ControlledZoneNum ).ZoneExhBalanced += BalancedExhMassFlow;
				ZoneEquipConfig( ControlledZoneNum ).PlenumMassFlow += PlenumInducedMassFlow;

				UpdateSystemOutputRequired( ActualZoneNum, SysOutputProvided, LatOutputProvided, EquipTypeNum );

				if ( ZoneHasAirLoopHVACTerminal && ZoneHasAirLoopHVACDirectAir ) {
					// zone has both AirTerminal:SingleDuct:Uncontrolled and another kind of Air terminal unit which is not supported
					if ( ! DirectAirAndAirTerminalWarningIssued( ActualZoneNum ) ) {
						ShowSevereError( "In zone \"" + ZoneEquipConfig( ControlledZoneNum ).ZoneName + "\" there are too many air terminals served by AirLoopHVAC systems." );
						ShowContinueError( "A single zone cannot have both an AirTerminal:SingleDuct:Uncontrolled " "and also a second AirTerminal:* object." );

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
					std::swap( pso.EquipType, psc.EquipType );
					std::swap( pso.EquipName, psc.EquipName );
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

		using DataHeatBalance::Zone;
		using DataHeatBalance::MassConservation;
		using DataHeatBalance::Mixing;
		using DataHeatBalance::Infiltration;
		using DataHeatBalance::ZoneAirMassFlow;
		using DataHeatBalance::AddInfiltrationFlow;
		using DataHeatBalance::AdjustInfiltrationFlow;
		using DataHeatBalFanSys::ZoneMassBalanceFlag;
		using DataHeatBalFanSys::ZoneInfiltrationFlag;
		using DataHeatBalFanSys::MixingMassFlowZone;
		using DataHeatBalFanSys::ZoneReOrder;
		using DataHVACGlobals::ZoneMassBalanceHVACReSim;
		using DataHVACGlobals::SmallMassFlow;

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
		Real64 ZoneInfiltrationMassFlowRate;
		Real64 BuildingZoneMixingFlowOld;
		Real64 BuildingZoneMixingFlow;

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
					if (Iteration == 0) {
						ZoneMixingAirMassFlowRate = MixingMassFlowZone( ZoneNum );
					}
					else {
						RetNode = ZoneEquipConfig( ZoneNum ).ReturnAirNode;
						if (RetNode > 0) {
							ZoneMixingAirMassFlowRate = max(0.0, Node(RetNode).MassFlowRate + TotExhaustAirMassFlowRate - TotInletAirMassFlowRate + MassConservation(ZoneNum).MixingSourceMassFlowRate);
						}
					}
					CalcZoneMixingFlowRateOfReceivingZone(ZoneNum, ZoneMixingAirMassFlowRate);
					if (MassConservation(ZoneNum).IsOnlySourceZone) {
						ZoneInfiltrationMassFlowRate = max(0.0, MassConservation(ZoneNum).MixingSourceMassFlowRate + TotExhaustAirMassFlowRate - TotInletAirMassFlowRate);

						if (MassConservation(ZoneNum).MixingSourceMassFlowRate > TotInletAirMassFlowRate) {

							if (ZoneAirMassFlow.InfiltrationTreatment == AdjustInfiltrationFlow) {
								if (ZoneInfiltrationMassFlowRate > ConvergenceTolerance) {
									ZoneInfiltrationFlag(ZoneNum) = true;
									MassConservation(ZoneNum).InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
									Infiltration(MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate = ZoneInfiltrationMassFlowRate;
									MassConservation(ZoneNum).IncludeInfilToZoneMassBal = 1;
								}
								else {
									MassConservation(ZoneNum).InfiltrationMassFlowRate = Infiltration(MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate;
								}
							}

							if (ZoneAirMassFlow.InfiltrationTreatment == AddInfiltrationFlow) {
								if (ZoneInfiltrationMassFlowRate > ConvergenceTolerance) {
									ZoneInfiltrationFlag(ZoneNum) = true;
									MassConservation(ZoneNum).InfiltrationMassFlowRate = ZoneInfiltrationMassFlowRate;
									MassConservation(ZoneNum).IncludeInfilToZoneMassBal = 1;
								}
								else {
									MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
								}
							}
						}
						else {

							if (ZoneAirMassFlow.InfiltrationTreatment == AdjustInfiltrationFlow) {
								MassConservation(ZoneNum).InfiltrationMassFlowRate = Infiltration(MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate;
							}
							if (ZoneAirMassFlow.InfiltrationTreatment == AddInfiltrationFlow) {
								MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
							}
						}
					}
					else {
						if (MassConservation(ZoneNum).InfiltrationPtr > 0) {
							MassConservation(ZoneNum).InfiltrationMassFlowRate = Infiltration(MassConservation(ZoneNum).InfiltrationPtr).MassFlowRate;
						}
						else {
							MassConservation(ZoneNum).InfiltrationMassFlowRate = 0.0;
						}
					}

					MassConservation(ZoneNum).InMassFlowRate = TotInletAirMassFlowRate;
					MassConservation(ZoneNum).ExhMassFlowRate = TotExhaustAirMassFlowRate;
					ZoneMixingNetAirMassFlowRate = MassConservation(ZoneNum).MixingMassFlowRate - MassConservation(ZoneNum).MixingSourceMassFlowRate;
				}
				//

				AirLoopNum = ZoneEquipConfig(ZoneNum).AirLoopNum;
				ZoneNode = ZoneEquipConfig(ZoneNum).ZoneNode;
				Node(ZoneNode).MassFlowRate = TotInletAirMassFlowRate;
				Node(ZoneNode).MassFlowRateMax = TotInletAirMassFlowRateMax;
				Node(ZoneNode).MassFlowRateMaxAvail = TotInletAirMassFlowRateMaxAvail;
				Node(ZoneNode).MassFlowRateMin = TotInletAirMassFlowRateMin;
				Node(ZoneNode).MassFlowRateMinAvail = TotInletAirMassFlowRateMinAvail;

				// Update Return Air Node Conditions; If one Exists
				RetNode = ZoneEquipConfig(ZoneNum).ReturnAirNode;
				if (RetNode > 0) {
					Node(RetNode).MassFlowRate = max(Node(ZoneNode).MassFlowRate + ZoneMixingNetAirMassFlowRate - (TotExhaustAirMassFlowRate - ZoneEquipConfig(ZoneNum).ZoneExhBalanced), 0.0);
					if (AirLoopNum > 0) {
						if (!PrimaryAirSystem(AirLoopNum).OASysExists) {
							Node(RetNode).MassFlowRate = max(Node(ZoneNode).MassFlowRate + ZoneMixingNetAirMassFlowRate - (TotExhaustAirMassFlowRate - ZoneEquipConfig(ZoneNum).ZoneExh), 0.0);
						}
					}
					MassConservation(ZoneNum).RetMassFlowRate = Node(RetNode).MassFlowRate;
					Node(RetNode).MassFlowRateMax = Node(ZoneNode).MassFlowRateMax;
					Node(RetNode).MassFlowRateMin = Node(ZoneNode).MassFlowRateMin;
					Node(RetNode).MassFlowRateMaxAvail = Node(ZoneNode).MassFlowRateMaxAvail;
					Node(RetNode).MassFlowRateMinAvail = 0.0;
				}

				TotSupplyAirMassFlowRate = TotInletAirMassFlowRate - (TotExhaustAirMassFlowRate - ZoneEquipConfig(ZoneNum).ZoneExh) - ZoneEquipConfig(ZoneNum).PlenumMassFlow;

				if (AirLoopNum > 0) {
					AirLoopFlow(AirLoopNum).ZoneExhaust += ZoneEquipConfig(ZoneNum).ZoneExh;
					AirLoopFlow(AirLoopNum).ZoneExhaustBalanced += ZoneEquipConfig(ZoneNum).ZoneExhBalanced;
					AirLoopFlow(AirLoopNum).SupFlow += TotSupplyAirMassFlowRate;
					AirLoopFlow(AirLoopNum).RetFlow0 += Node(RetNode).MassFlowRate;
					AirLoopFlow(AirLoopNum).RecircFlow += ZoneEquipConfig(ZoneNum).PlenumMassFlow;
				}
				BuildingZoneMixingFlow += MassConservation(ZoneNum).MixingMassFlowRate;
			}
			// Calculate an air loop return air flow rate
			for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
				if ((AirLoopFlow(AirLoopNum).ZoneExhaust > (AirLoopFlow(AirLoopNum).SupFlow + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced) || AirLoopFlow(AirLoopNum).ZoneExhaust > (AirLoopFlow(AirLoopNum).MaxOutAir + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced)) && !AirLoopFlow(AirLoopNum).FlowError && AirLoopsSimOnce) {
					if (!isPulseZoneSizing && !ZoneAirMassFlow.EnforceZoneMassBalance) {
						ShowWarningError("In AirLoopHVAC " + PrimaryAirSystem(AirLoopNum).Name + " there is unbalanced exhaust air flow.");
						ShowContinueErrorTimeStamp(" ");
						ShowContinueError("  Unless there is balancing infiltration / ventilation air flow, this will result in");
						ShowContinueError("  load due to induced outdoor air being neglected in the simulation.");
						AirLoopFlow(AirLoopNum).FlowError = true;
					}
				}
				AirLoopFlow(AirLoopNum).ZoneExhaust = min(AirLoopFlow(AirLoopNum).ZoneExhaust, (AirLoopFlow(AirLoopNum).SupFlow + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced + AirLoopFlow(AirLoopNum).ZoneMixingFlow));
				AirLoopFlow(AirLoopNum).ZoneExhaust = min(AirLoopFlow(AirLoopNum).ZoneExhaust, (AirLoopFlow(AirLoopNum).MaxOutAir + AirLoopFlow(AirLoopNum).ZoneExhaustBalanced + AirLoopFlow(AirLoopNum).ZoneMixingFlow));

				if (AirLoopFlow(AirLoopNum).ZoneMixingFlow < 0.0) {
					// the source zone and the recieving zone are in different air loops
					AirLoopFlow(AirLoopNum).ZoneExhaust = max(0.0, (AirLoopFlow(AirLoopNum).ZoneExhaust - AirLoopFlow(AirLoopNum).ZoneMixingFlow));
					AirLoopFlow(AirLoopNum).RetFlow = AirLoopFlow(AirLoopNum).SupFlow - (AirLoopFlow(AirLoopNum).ZoneExhaust - AirLoopFlow(AirLoopNum).ZoneExhaustBalanced) + AirLoopFlow(AirLoopNum).RecircFlow;
				}
				else {
					AirLoopFlow(AirLoopNum).RetFlow = AirLoopFlow(AirLoopNum).SupFlow - (AirLoopFlow(AirLoopNum).ZoneExhaust - AirLoopFlow(AirLoopNum).ZoneExhaustBalanced) + AirLoopFlow(AirLoopNum).RecircFlow + AirLoopFlow(AirLoopNum).ZoneMixingFlow;
				}
			}

			// adjust the zone return air flow rates to match the air loop return air flow rate
			for (ZoneNum1 = 1; ZoneNum1 <= NumOfZones; ++ZoneNum1) {
				ZoneNum = ZoneNum1;
				if (ZoneAirMassFlow.EnforceZoneMassBalance) ZoneNum = ZoneReOrder(ZoneNum1);
				if (!ZoneEquipConfig(ZoneNum).IsControlled) continue;
				RetNode = ZoneEquipConfig(ZoneNum).ReturnAirNode;
				AirLoopNum = ZoneEquipConfig(ZoneNum).AirLoopNum;
				if (AirLoopNum > 0 && RetNode > 0) {
					if (PrimaryAirSystem(AirLoopNum).OASysExists) {
						if (AirLoopFlow(AirLoopNum).RetFlow0 > 0.0) {
							Node(RetNode).MassFlowRate *= (AirLoopFlow(AirLoopNum).RetFlow / AirLoopFlow(AirLoopNum).RetFlow0);
						}
						else {
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
			}
			// update the
			if (Iteration > 0) {
				if (abs(BuildingZoneMixingFlow - BuildingZoneMixingFlowOld) < ConvergenceTolerance) {
					ZoneMassBalanceHVACReSim = false;
					break;
				}
				else {
					ZoneMassBalanceHVACReSim = true;
				}
			}
			if (!ZoneAirMassFlow.EnforceZoneMassBalance) break;
			Iteration += 1;

		} while (Iteration < IterMax);

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
		using DataHeatBalance::ZoneIntGain;
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
		Real64 RhoAir; // Density of air (Kg/m3)
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

				CpAir = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, Node( ZoneNode ).Temp );

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

				H2OHtOfVap = PsyHgAirFnWTdb( Node( ZoneNode ).HumRat, Node( ReturnNode ).Temp );
				RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ReturnNode ).Temp, Node( ZoneNode ).HumRat );

				// Include impact of under case returns for refrigerated display case when updating the return air node humidity
				if ( ! Zone( ActualZoneNum ).NoHeatToReturnAir ) {
					if ( MassFlowRA > 0 ) {
						SumAllReturnAirLatentGains( ZoneNum, SumRetAirLatentGainRate );
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
		bool const AdjustZoneMixingFlowFlag // flags to adjust zone mxing mass flow rate
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
		using DataRoomAirModel::AirModel;
		using DataRoomAirModel::RoomAirModel_UCSDDV;
		using DataRoomAirModel::RoomAirModel_UCSDCV;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkControlSimple;
		using DataAirflowNetwork::AirflowNetworkControlSimpleADS;
		using DataAirflowNetwork::AirflowNetworkZoneFlag;
		using EarthTube::ManageEarthTube;
		using CoolTower::ManageCoolTower;
		using ThermalChimney::ManageThermalChimney;
		using DataZoneEquipment::ZoneEquipAvail;
		using DataHVACGlobals::CycleOn;
		using DataHVACGlobals::CycleOnZoneFansOnly;
		using DataContaminantBalance::Contaminant;
		using DataContaminantBalance::ZoneAirCO2;
		using DataContaminantBalance::MixingMassFlowCO2;
		using DataContaminantBalance::ZoneAirGC;
		using DataContaminantBalance::MixingMassFlowGC;

		using DataHeatBalance::Ventilation;
		using DataGlobals::TimeStepZone;
		using DataGlobals::WarmupFlag;
		using DataGlobals::EndHourFlag;
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
		static FArray1D< Real64 > ZMAT; // Zone air temperature
		static FArray1D< Real64 > ZHumRat; // Zone air humidity ratio
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
			ZnAirRpt.VentilFanElec() = 0.0;
		}

		// Initialization of ZoneAirBalance
		if ( TotZoneAirBalance > 0 ) {
			ZoneAirBalance.BalMassFlowRate() = 0.0;
			ZoneAirBalance.InfMassFlowRate() = 0.0;
			ZoneAirBalance.NatMassFlowRate() = 0.0;
			ZoneAirBalance.ExhMassFlowRate() = 0.0;
			ZoneAirBalance.IntMassFlowRate() = 0.0;
			ZoneAirBalance.ERVMassFlowRate() = 0.0;
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
						ShowWarningError( "Ventilation indoor temperature control: The minimum indoor temperature is above " "the maximum indoor temperature in " + Ventilation( I ).Name );
						ShowContinueError( "The minimum indoor temperature is set to the maximum indoor temperature. " "Simulation continues." );
						ShowContinueErrorTimeStamp( " Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The minimum indoor temperature is still above " "the maximum indoor temperature", Ventilation( I ).IndoorTempErrIndex, Ventilation( I ).MinIndoorTemperature, Ventilation( I ).MinIndoorTemperature );
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
						ShowWarningError( "Ventilation outdoor temperature control: The minimum outdoor temperature is above " "the maximum outdoor temperature in " + Ventilation( I ).Name );
						ShowContinueError( "The minimum outdoor temperature is set to the maximum outdoor temperature. " "Simulation continues." );
						ShowContinueErrorTimeStamp( " Occurrence info:" );
					} else {
						ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above " "the maximum outdoor temperature", Ventilation( I ).OutdoorTempErrIndex, Ventilation( I ).MinOutdoorTemperature, Ventilation( I ).MinOutdoorTemperature );
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
							ShowWarningError( "Mixing zone temperature control: The minimum zone temperature is above " "the maximum zone temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum zone temperature is set to the maximum zone temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum zone temperature is still above " "the maximum zone temperature", Mixing( j ).IndoorTempErrIndex, MixingTmin, MixingTmin );
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
							ShowWarningError( "Mixing source temperature control: The minimum source temperature is above " "the maximum source temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum source temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum source temperature is still above " "the maximum source temperature", Mixing( j ).SourceTempErrIndex, MixingTmin, MixingTmin );
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
							ShowWarningError( "Mixing outdoor temperature control: The minimum outdoor temperature is above " "the maximum outdoor temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum outdoor temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above " "the maximum outdoor temperature", Mixing( j ).OutdoorTempErrIndex, MixingTmin, MixingTmin );
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
					if ( ZoneMassBalanceFlag(n) && AdjustZoneMixingFlowFlag ) {
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
					if ( ZoneMassBalanceFlag(n) && AdjustZoneMixingFlowFlag ) {
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
				if ( ZoneMassBalanceFlag(n) && AdjustZoneMixingFlowFlag ) {
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
							ShowWarningError( "CrossMixing zone temperature control: The minimum zone temperature is above " "the maximum zone temperature in " + CrossMixing( j ).Name );
							ShowContinueError( "The minimum zone temperature is set to the maximum zone temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum zone temperature is still above " "the maximum zone temperature", CrossMixing( j ).IndoorTempErrIndex, MixingTmin, MixingTmin );
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
							ShowWarningError( "CrossMixing source temperature control: The minimum source temperature is above " "the maximum source temperature in " + CrossMixing( j ).Name );
							ShowContinueError( "The minimum source temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum source temperature is still above " "the maximum source temperature", CrossMixing( j ).SourceTempErrIndex, MixingTmin, MixingTmin );
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
							ShowWarningError( "CrossMixing outdoor temperature control: The minimum outdoor temperature is above " "the maximum outdoor temperature in " + Mixing( j ).Name );
							ShowContinueError( "The minimum outdoor temperature is set to the maximum source temperature. " "Simulation continues." );
							ShowContinueErrorTimeStamp( " Occurrence info:" );
						} else {
							ShowRecurringWarningErrorAtEnd( "The minimum outdoor temperature is still above " "the maximum outdoor temperature", CrossMixing( j ).OutdoorTempErrIndex, MixingTmin, MixingTmin );
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
				if ( AdjustZoneMixingFlowFlag && ZoneInfiltrationFlag(NZ) ) {
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
			} else if ( SELECT_CASE_var == InfiltrationShermanGrimsrud ) {
				// Sherman Grimsrud model as formulated in ASHRAE HoF
				WindExt = WindSpeed; // formulated to use wind at Meterological Station rather than local
				IVF = GetCurrentScheduleValue( Infiltration( j ).SchedPtr ) * Infiltration( j ).LeakageArea / 1000.0 * std::sqrt( Infiltration( j ).BasicStackCoefficient * std::abs( TempExt - ZMAT( NZ ) ) + Infiltration( j ).BasicWindCoefficient * pow_2( WindExt ) );
				if ( IVF < 0.0 ) IVF = 0.0;
				MCpI_temp = IVF * AirDensity * CpAir;
				if ( MCpI_temp < 0.0 ) MCpI_temp = 0.0;
				Infiltration(j).VolumeFlowRate = MCpI_temp / AirDensity / CpAir;
				if ( AdjustZoneMixingFlowFlag && ZoneInfiltrationFlag(NZ) ) {
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
				if ( AdjustZoneMixingFlowFlag && ZoneInfiltrationFlag(NZ) ) {
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


	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // ZoneEquipmentManager

} // EnergyPlus
