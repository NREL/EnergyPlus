// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/floops.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <WaterThermalTanks.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <DXCoils.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SolarCollectors.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace WaterThermalTanks {

	// MODULE INFORMATION:
	//       AUTHOR         Brandon Anderson
	//       DATE WRITTEN   May 2000
	//       MODIFIED       Feb 2005, PGE; July 2005, FSEC - added HPWH's and desuperheater water heating coils
	//                      Jan 2007, PGE - added stratified water heater
	//                      Oct 2007, BTG - extended for indirect water heater
	//                      May 2008, Stovall - added desup from condenser and removed double counting
	//                           (includes "d0"s from revision 145)
	//                       Nov 2011, BAN; corrected use and source outlet temp. calculation of stratified tank
	//       RE-ENGINEERED  Feb 2004, PGE
	//                      Sep 2008, BTG - refactored, was PlantWaterHeater.cc is now PlantWaterThermalTank.cc
	//                                 reuse water heater code for chilled water storage

	// PURPOSE OF THIS MODULE:
	// This module simulates water thermal storage tanks heaters in the plant loop.  Tanks can
	// be positioned as supply side equipment or demand side equipment.  Water heater versions can be stand-alone as
	// non-zone equipment.

	// METHODOLOGY EMPLOYED:
	// Two water thermal tank models are implemented, MIXED and STRATIFIED with hot and cold versions of each:
	// WaterHeater:Mixed simulates a well-mixed, single-node tank for hot water applications.  Source (e.g. heat recovery) and
	// use plant connections are allowed.  A scheduled domestic hot water demand can also be specified
	// to directly utilize the hot water without use side connections.
	// WaterHeater:Stratified simulates a stratified, multi-node tank for hot water applicatons.
	// The model shares most of the same capabilities as WaterHeater:Mixed
	// but also has up to two heating elements which can be operated in
	// a master-slave mode or simultaneous mode.

	// ThermalStorage:ChilledWater:Mixed simulates a well-mixed, single-node tank for chilled water applications

	// ThermalStorage:ChilledWater:Stratified simulates a stratified, multi-node tank for chilled water applications.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SecInHour;
	using DataGlobals::OutputFileInits;
	using DataHeatBalance::NumRefrigeratedRacks;
	using DataHeatBalance::HeatReclaimRefrigeratedRack;
	using DataHeatBalance::HeatReclaimRefrigCondenser;
	using DataHeatBalance::HeatReclaimDXCoil;
	using DataHeatBalance::NumRefrigCondensers;
	using namespace DataPlant;
	using General::TrimSigDigits;
	using ReportSizingManager::ReportSizingOutput;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	std::string const cMixedWHModuleObj( "WaterHeater:Mixed" );
	std::string const cStratifiedWHModuleObj( "WaterHeater:Stratified" );
	std::string const cMixedCWTankModuleObj( "ThermalStorage:ChilledWater:Mixed" );
	std::string const cStratifiedCWTankModuleObj( "ThermalStorage:ChilledWater:Stratified" );
	static std::string const BlankString;

	int const HeatMode( 1 ); // heating source is on, source will not turn off until setpoint temp is reached
	int const FloatMode( 0 ); // heating source is off, source will not turn on until cutin temp is reached
	int const VentMode( -1 ); // tank temp is above maximum temperature and water is venting
	int const CoolMode( 2 ); // cooling source is on, source will not turn off until setpoint temp is reached

	int const AmbientTempSchedule( 1 ); // ambient temperature around tank (or HPWH inlet air) is scheduled
	int const AmbientTempZone( 2 ); // tank is located in a zone or HPWH inlet air is zone air only
	int const AmbientTempOutsideAir( 3 ); // tank is located outdoors or HPWH inlet air is outdoor air only
	int const AmbientTempZoneAndOA( 4 ); // applicable to HPWH only, inlet air is mixture of OA and zone air

	int const CrankcaseTempSchedule( 1 ); // temperature controlling compressor crankcase heater is scheduled
	int const CrankcaseTempZone( 2 ); // temperature controlling compressor crankcase heater is zone air
	int const CrankcaseTempExterior( 3 ); // temperature controlling compressor crankcase heater is outdoor air

	int const ControlTypeCycle( 1 ); // water heater only, cycling heating source control
	int const ControlTypeModulate( 2 ); // water heater only, modulating heating source control

	int const TankShapeVertCylinder( 1 ); // tank shape is a vertical cylinder
	int const TankShapeHorizCylinder( 2 ); // tank shape is a horizontal cylinder
	int const TankShapeOther( 3 ); // tank shape has an arbitrary perimeter shape

	int const PriorityMasterSlave( 1 ); // water heater only, master-slave priority control of heater elements
	int const PrioritySimultaneous( 2 ); // water heater only, simultaneous control of heater elements

	int const InletModeFixed( 1 ); // water heater only, inlet water always enters at the user-specified height
	int const InletModeSeeking( 2 ); // water heater only, inlet water seeks out the node with the closest temperature

	// integer parameter for water heater
	int const MixedWaterHeater( TypeOf_WtrHeaterMixed ); // WaterHeater:Mixed
	int const StratifiedWaterHeater( TypeOf_WtrHeaterStratified ); // WaterHeater:Stratified
	int const HeatPumpWaterHeater( TypeOf_HeatPumpWtrHeater ); // WaterHeater:HeatPump
	//stovall, next line never used because all desuperheater coils used in mixed water heater types
	int const CoilWaterDesuperHeater( 4 ); // Coil:WaterHeating:Desuperheater
	int const MixedChilledWaterStorage( TypeOf_ChilledWaterTankMixed ); // 'ThermalStorage:ChilledWater:Mixed'
	int const StratifiedChilledWaterStorage( TypeOf_ChilledWaterTankStratified ); // 'ThermalStorage:ChilledWater:Stratified'

	// reclaim heat object types for Coil:WaterHeating:Desuperheater object
	int const COMPRESSORRACK_REFRIGERATEDCASE( 1 ); // reclaim heating source is refrigerated case compressor rack
	int const COIL_DX_COOLING( 2 ); // reclaim heating source is DX cooling coil
	int const COIL_DX_MULTISPEED( 3 ); // reclaim heating source is DX multispeed coil
	int const COIL_DX_MULTIMODE( 4 ); // reclaim heating source is DX multimode coil
	int const CONDENSER_REFRIGERATION( 5 ); // reclaim heating source is detailed refrigeration system condenser

	int const UseSide( 101 ); // Indicates Use side of water heater
	int const SourceSide( 102 ); // Indicates Source side of water heater

	int const SizeNotSet( 200 );
	int const SizePeakDraw( 201 );
	int const SizeResidentialMin( 202 );
	int const SizePerPerson( 203 );
	int const SizePerFloorArea( 204 );
	int const SizePerUnit( 205 );
	int const SizePerSolarColArea( 206 );

	int const HPWHControlNotSet( 500 );
	int const Heater1HPWHControl( 501 );
	int const Heater2HPWHControl( 502 );
	int const SourceInletHPWHControl( 503 );
	int const SourceOutletHPWHControl( 504 );
	int const UseInletHPWHControl( 505 );
	int const UseOutletHPWHControl( 506 );

	int const SourceSideStorageTank( 600 );
	int const SourceSideIndirectHeatPrimarySetpoint( 601 );
	int const SourceSideIndirectHeatAltSetpoint( 602 );

	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:
	Array1D_bool ValidSourceType; // Used to determine if a source for a desuperheater heating coil is valid
	Array1D_bool MyHPSizeFlag; // Used to report autosize info in Init
	Array1D_bool CheckWTTEquipName;
	Array1D_bool CheckHPWHEquipName;

	// MODULE VARIABLE DECLARATIONS:
	int NumChilledWaterMixed( 0 ); // number of mixed chilled water tanks
	int NumChilledWaterStratified( 0 ); // number of stratified chilled water tanks
	int NumWaterHeaterMixed( 0 ); // number of mixed water heaters
	int NumWaterHeaterStratified( 0 ); // number of stratified water heaters
	int NumWaterThermalTank( 0 ); // total number of water thermal tanks, hot and cold (MIXED + STRATIFIED)
	int NumWaterHeaterDesuperheater( 0 ); // number of desuperheater heating coils
	int NumHeatPumpWaterHeater( 0 ); // number of heat pump water heaters
	//INTEGER :: MaxCyclesErrorCount           =0 ! error counter for water heater that cycles more than max during time step

	Real64 HPPartLoadRatio( 0.0 ); // part load ratio of HPWH
	bool GetWaterThermalTankInputFlag( true ); // Calls to Water Heater from multiple places in code
	Real64 MixerInletAirSchedule( 0.0 ); // output of inlet air mixer node schedule
	Real64 MdotAir( 0.0 ); // mass flow rate of evaporator air, kg/s
	int NumWaterHeaterSizing( 0 ); // Number of sizing/design objects for water heaters.
	Array1D_bool AlreadyRated; // control so we don't repeat again

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< WaterThermalTankData > WaterThermalTank;
	Array1D< HeatPumpWaterHeaterData > HPWaterHeater;
	Array1D< WaterHeaterDesuperheaterData > WaterHeaterDesuperheater;

	static gio::Fmt fmtLD( "*" );

	// MODULE SUBROUTINES:

	// Functions

	void
	SimWaterThermalTank(
		int const CompType,
		std::string const & CompName,
		int & CompIndex,
		bool const EP_UNUSED( RunFlag ), // unused1208
		bool const InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration, // TRUE if First iteration of simulation
		Optional_int_const LoopNum,
		Optional_int_const LoopSideNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brandon Anderson
		//       DATE WRITTEN   May 2000
		//       MODIFIED       FSEC, July 2005
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// The main subroutine for simulating a water heater, heat pump water heater, or desuperheater
		// heating coil.  This routine will:
		// 1. Gets Input if necessary
		// 2. Determines the load the water heater (or heat pump water heater) must support
		// 3. Determine the type of water heater, heat pump water heater, or desuperheater
		//    heating coil to be simulated
		// 4. Calls simulation routines

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology. Subroutine is called from PlantLoopEquipments

		// Using/Aliasing
		using DataGlobals::KickOffSimulation;
		using InputProcessor::FindItem;
		using DataSizing::DataNonZoneNonAirloopValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool OneTimeSetupFlag( true );
		static Array1D_bool MyOneTimeFlagWH; // first pass log
		static Array1D_bool MyTwoTimeFlagWH; // second pass do input check
		static Array1D_bool MyOneTimeFlagHP; // first pass log
		static Array1D_bool MyTwoTimeFlagHP; // second pass do input check
		int tmpLoopNum;
		int tmpLoopSideNum;
		int CompNum;
		int TankNum;

		// FLOW:
		if ( GetWaterThermalTankInputFlag ) {
			GetWaterThermalTankInput();
			GetWaterThermalTankInputFlag = false;
		}

		if ( OneTimeSetupFlag ) {
			MyOneTimeFlagWH.allocate( NumWaterThermalTank );
			MyTwoTimeFlagWH.allocate( NumWaterThermalTank );
			MyOneTimeFlagHP.allocate( NumHeatPumpWaterHeater );
			MyTwoTimeFlagHP.allocate( NumHeatPumpWaterHeater );
			MyOneTimeFlagWH = true;
			MyTwoTimeFlagWH = true;
			MyOneTimeFlagHP = true;
			MyTwoTimeFlagHP = true;
			OneTimeSetupFlag = false;
		}

		// Find the correct Equipment
		if ( CompType != TypeOf_HeatPumpWtrHeater ) {
			if ( CompIndex == 0 ) {
				CompNum = FindItem( CompName, WaterThermalTank.Name(), NumWaterThermalTank );
				if ( CompNum == 0 ) {
					ShowFatalError( "SimWaterThermalTank:  Unit not found=" + CompName );
				}
				CompIndex = CompNum;
			} else {
				CompNum = CompIndex;
				if ( CompNum > NumWaterThermalTank || CompNum < 1 ) {
					ShowFatalError( "SimWaterThermalTank:  Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of Units=" + TrimSigDigits( NumWaterThermalTank ) + ", Entered Unit name=" + CompName );
				}
				if ( CheckWTTEquipName( CompNum ) ) {
					if ( CompName != WaterThermalTank( CompNum ).Name ) {
						ShowFatalError( "SimWaterThermalTank: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + WaterThermalTank( CompNum ).Name );
					}
					CheckWTTEquipName( CompNum ) = false;
				}
			}
		} else {
			if ( CompIndex == 0 ) {
				CompNum = FindItem( CompName, HPWaterHeater.Name(), NumHeatPumpWaterHeater );
				if ( CompNum == 0 ) {
					ShowFatalError( "SimWaterThermalTank:  Unit not found=" + CompName );
				}
				CompIndex = CompNum;
			} else {
				CompNum = CompIndex;
				if ( CompNum > NumWaterThermalTank || CompNum < 1 ) {
					ShowFatalError( "SimWaterThermalTank:  Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of Units=" + TrimSigDigits( NumHeatPumpWaterHeater ) + ", Entered Unit name=" + CompName );
				}
				if ( CheckHPWHEquipName( CompNum ) ) {
					if ( CompName != HPWaterHeater( CompNum ).Name ) {
						ShowFatalError( "SimWaterThermalTank: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + HPWaterHeater( CompNum ).Name );
					}
					CheckHPWHEquipName( CompNum ) = false;
				}
			}
		}

		// this case statement needs integerization.
		{ auto const SELECT_CASE_var( CompType );

		// string comparisons to remove here.
		// =========================  Water Heater and Chilled Water Storage
		if ( ( SELECT_CASE_var == TypeOf_WtrHeaterMixed ) || ( SELECT_CASE_var == TypeOf_WtrHeaterStratified ) || ( SELECT_CASE_var == TypeOf_ChilledWaterTankMixed ) || ( SELECT_CASE_var == TypeOf_ChilledWaterTankStratified ) ) {

			if ( InitLoopEquip ) {
				if ( present( LoopNum ) ) {
					InitWaterThermalTank( CompNum, FirstHVACIteration, LoopNum, LoopSideNum );
				} else {
					InitWaterThermalTank( CompNum, FirstHVACIteration );
				}
				MinePlantStructForInfo( CompNum );
				if ( present( LoopNum ) ) {
					if ( ( ( WaterThermalTank( CompNum ).SourceSidePlantLoopNum == LoopNum ) && ( WaterThermalTank( CompNum ).SourceSidePlantLoopSide == LoopSideNum ) )
						|| ( ( WaterThermalTank( CompNum ).UseSidePlantLoopNum == LoopNum ) && ( WaterThermalTank( CompNum ).UseSidePlantLoopSide == LoopSideNum ) ) ) {

						SizeTankForDemandSide( CompNum );
						SizeDemandSidePlantConnections( CompNum );
						SizeSupplySidePlantConnections( CompNum, LoopNum, LoopSideNum );
						SizeTankForSupplySide( CompNum );
					} else {
						return;
					}
				} else {
					SizeTankForDemandSide( CompNum );
					SizeDemandSidePlantConnections( CompNum );
					SizeSupplySidePlantConnections( CompNum );
					SizeTankForSupplySide( CompNum );
				}

				// Calculate and report water heater standard ratings to EIO file (now that sizing is done)
				if ( PlantFirstSizesOkayToFinalize ) {
					if ( ! WaterThermalTank( CompNum ).IsChilledWaterTank ) {
						CalcStandardRatings( CompNum );
					} else {
						ReportCWTankInits( CompNum );
					}
				}
				MinCap = 0.0;
				MaxCap = WaterThermalTank( CompNum ).MaxCapacity;
				OptCap = WaterThermalTank( CompNum ).MaxCapacity;
				if ( present( LoopNum ) ) {
					InitWaterThermalTank( CompNum, FirstHVACIteration, LoopNum, LoopSideNum );
				} else {
					InitWaterThermalTank( CompNum, FirstHVACIteration );
				}
				return;
			}

			if ( MyOneTimeFlagWH( CompNum ) ) {
				MyOneTimeFlagWH( CompNum ) = false;
			} else {
				if ( MyTwoTimeFlagWH( CompNum ) ) {
					MinePlantStructForInfo( CompNum ); // call it again to get control types filled out
					MyTwoTimeFlagWH( CompNum ) = false;
				}
			}
			WaterThermalTank( CompNum ).UseSideLoadRequested = std::abs( MyLoad );
			tmpLoopNum = WaterThermalTank( CompNum ).UseSidePlantLoopNum;
			tmpLoopSideNum = WaterThermalTank( CompNum ).UseSidePlantLoopSide;
			if ( tmpLoopNum > 0 && tmpLoopSideNum > 0 && ! KickOffSimulation ) {
				WaterThermalTank( CompNum ).UseCurrentFlowLock = PlantLoop( tmpLoopNum ).LoopSide( LoopSideNum ).FlowLock;
			} else {
				WaterThermalTank( CompNum ).UseCurrentFlowLock = 1;
			}
			tmpLoopNum = WaterThermalTank( CompNum ).SourceSidePlantLoopNum;
			tmpLoopSideNum = WaterThermalTank( CompNum ).SourceSidePlantLoopSide;
			if ( tmpLoopNum > 0 && tmpLoopSideNum > 0 && ! KickOffSimulation ) {
				WaterThermalTank( CompNum ).SourceCurrentFlowLock = PlantLoop( tmpLoopNum ).LoopSide( LoopSideNum ).FlowLock;
			} else {
				WaterThermalTank( CompNum ).SourceCurrentFlowLock = 1;
			}
			InitWaterThermalTank( CompNum, FirstHVACIteration );
			//       Plant connected water heaters may have a desuperheater heating coil attached
			if ( WaterThermalTank( CompNum ).DesuperheaterNum == 0 ) {
				if ( ( WaterThermalTank( CompNum ).TypeNum == MixedWaterHeater ) || ( WaterThermalTank( CompNum ).TypeNum == MixedChilledWaterStorage ) ) {
					CalcWaterThermalTankMixed( CompNum );
				} else if ( ( WaterThermalTank( CompNum ).TypeNum == StratifiedWaterHeater ) || ( WaterThermalTank( CompNum ).TypeNum == StratifiedChilledWaterStorage ) ) {
					CalcWaterThermalTankStratified( CompNum );
				}
			} else if ( WaterThermalTank( CompNum ).DesuperheaterNum > 0 ) {
				CalcDesuperheaterWaterHeater( CompNum, FirstHVACIteration );
			}
			UpdateWaterThermalTank( CompNum );
			ReportWaterThermalTank( CompNum );

			// =========================  Heat Pump Water Heater
		} else if ( SELECT_CASE_var == TypeOf_HeatPumpWtrHeater ) {
			if ( InitLoopEquip ) {
				// CompNum is index to heatpump model, not tank so get the tank index
				TankNum = HPWaterHeater( CompNum ).WaterHeaterTankNum;
				if ( present( LoopNum ) ) {
					InitWaterThermalTank( TankNum, FirstHVACIteration, LoopNum, LoopSideNum );
				} else {
					InitWaterThermalTank( TankNum, FirstHVACIteration );
				}
				MinePlantStructForInfo( TankNum );
				if ( present( LoopNum ) ) {
					if ( ( ( WaterThermalTank( TankNum ).SourceSidePlantLoopNum == LoopNum ) && ( WaterThermalTank( TankNum ).SourceSidePlantLoopSide == LoopSideNum ) ) || ( ( WaterThermalTank( TankNum ).UseSidePlantLoopNum == LoopNum ) && ( WaterThermalTank( TankNum ).UseSidePlantLoopSide == LoopSideNum ) ) ) {
						SizeTankForDemandSide( CompNum );
						SizeDemandSidePlantConnections( CompNum );
						SizeSupplySidePlantConnections( TankNum, LoopNum, LoopSideNum );
						SizeTankForSupplySide( TankNum );
					} else {
						return;
					}
				} else {
					SizeTankForDemandSide( CompNum );
					SizeDemandSidePlantConnections( CompNum );
					SizeSupplySidePlantConnections( TankNum );
					SizeTankForSupplySide( TankNum );
				}

				if ( PlantFirstSizesOkayToFinalize ) {
					CalcStandardRatings( TankNum );
					DataNonZoneNonAirloopValue = 0.0;
				}
				MinCap = 0.0;
				MaxCap = HPWaterHeater( CompNum ).Capacity;
				OptCap = HPWaterHeater( CompNum ).Capacity;

				return;
			}

			if ( MyOneTimeFlagHP( CompNum ) ) {
				MyOneTimeFlagHP( CompNum ) = false;
			} else {
				if ( MyTwoTimeFlagHP( CompNum ) ) {
					MinePlantStructForInfo( HPWaterHeater( CompNum ).WaterHeaterTankNum ); // call it again to get control types filled out
					MyTwoTimeFlagHP( CompNum ) = false;
				}
			}
			WaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum ).UseSideLoadRequested = std::abs( MyLoad );
			tmpLoopNum = WaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum ).UseSidePlantLoopNum;
			tmpLoopSideNum = WaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum ).UseSidePlantLoopSide;
			if ( tmpLoopNum > 0 && tmpLoopSideNum > 0 && ! KickOffSimulation ) {
				WaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum ).UseCurrentFlowLock = PlantLoop( tmpLoopNum ).LoopSide( LoopSideNum ).FlowLock;
			} else {
				WaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum ).UseCurrentFlowLock = 1;
			}
			if ( present( LoopNum ) ) {
				InitWaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum, FirstHVACIteration, LoopNum, LoopSideNum );
			} else {
				InitWaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum, FirstHVACIteration );
			}
			CalcHeatPumpWaterHeater( HPWaterHeater( CompNum ).WaterHeaterTankNum, FirstHVACIteration );
			UpdateWaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum );
			ReportWaterThermalTank( HPWaterHeater( CompNum ).WaterHeaterTankNum );

		} else {
			ShowSevereError( "SimWaterThermalTank: Invalid Water Thermal Tank Equipment Type=" + TrimSigDigits( CompType ) );
			ShowContinueError( "Occurs in Water Thermal Tank Equipment named = " + CompName );
			ShowFatalError( "Preceding condition causes termination." );

		}}

	}

	void
	SimulateWaterHeaterStandAlone(
		int const WaterHeaterNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       July 2005, FSEC - added HPWHs and desuperheater water heating coils
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine acts an interface to SimWaterHeater for stand-alone water heaters with no plant connections,
		// HPWHs not defined as zone equipment with no plant connections, and stand-alone water heaters with
		// desuperheater heating coils with no plant connections.

		// METHODOLOGY EMPLOYED:
		// The necessary control flags and dummy variables are set and passed into SimWaterHeater. This subroutine is
		// called from NonZoneEquipmentManager.

		// Using/Aliasing
		using namespace DataPlant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool LocalRunFlag; // local variables of similar name as others used in Sim modules
		bool LocalInitLoopEquip;
		Real64 MyLoad;
		Real64 MinCap;
		Real64 MaxCap;
		Real64 OptCap;
		int TestNum;

		// FLOW:
		if ( GetWaterThermalTankInputFlag ) {
			GetWaterThermalTankInput();
			GetWaterThermalTankInputFlag = false;
		}

		// Only simulate stand-alone water heaters here.  Plant connected water heaters are called by the PlantLoopEquipments.
		if ( WaterThermalTank( WaterHeaterNum ).StandAlone ) {
			LocalRunFlag = true;
			LocalInitLoopEquip = false;
			TestNum = WaterHeaterNum;
			SimWaterThermalTank( WaterThermalTank( WaterHeaterNum ).TypeNum, WaterThermalTank( WaterHeaterNum ).Name, TestNum, LocalRunFlag, LocalInitLoopEquip, MyLoad, MinCap, MaxCap, OptCap, FirstHVACIteration );
			if ( TestNum != WaterHeaterNum ) {
				ShowFatalError( "SimulateWaterHeaterStandAlone: Input WaterHeater Num [" + TrimSigDigits( WaterHeaterNum ) + "] does not match returned WaterHeater Num[" + TrimSigDigits( TestNum ) + "] Name=\"" + WaterThermalTank( WaterHeaterNum ).Name + "\"." );
			}

			// HPWHs with inlet air from a zone and not connected to a plant loop are simulated through a CALL from ZoneEquipmentManager.
			// HPWHs that are plant connected are always simulated through a CALL from PlantLoopEquipments directly to SimWaterThermalTank.

			// NOTE: HPWHs with inlet air from a zone AND plant connected are not stand alone and are simulated in PlantLoopEquipments
		} else if ( WaterThermalTank( WaterHeaterNum ).HeatPumpNum > 0 ) {
			//   Only HPWHs with inlet air from outdoors or scheduled HPWHs (not connected to a plant loop) are simulated here.
			if ( HPWaterHeater( WaterThermalTank( WaterHeaterNum ).HeatPumpNum ).StandAlone && ( HPWaterHeater( WaterThermalTank( WaterHeaterNum ).HeatPumpNum ).InletAirConfiguration == AmbientTempOutsideAir || HPWaterHeater( WaterThermalTank( WaterHeaterNum ).HeatPumpNum ).InletAirConfiguration == AmbientTempSchedule ) ) {
				LocalRunFlag = true;
				LocalInitLoopEquip = false;
				SimWaterThermalTank( HPWaterHeater( WaterThermalTank( WaterHeaterNum ).HeatPumpNum ).TypeNum, HPWaterHeater( WaterThermalTank( WaterHeaterNum ).HeatPumpNum ).Name, WaterThermalTank( WaterHeaterNum ).HeatPumpNum, LocalRunFlag, LocalInitLoopEquip, MyLoad, MinCap, MaxCap, OptCap, FirstHVACIteration );
			}

			// Only simulate stand-alone water heaters with desuperheater water heating coils here.  Plant connected water heaters
			// with desuperheater water heating coils are called by PlantLoopEquipments.
		} else if ( WaterThermalTank( WaterHeaterNum ).DesuperheaterNum > 0 ) {
			if ( WaterHeaterDesuperheater( WaterThermalTank( WaterHeaterNum ).DesuperheaterNum ).StandAlone ) {
				LocalRunFlag = true;
				LocalInitLoopEquip = false;
				TestNum = WaterHeaterNum;
				SimWaterThermalTank( WaterThermalTank( WaterHeaterNum ).TypeNum, WaterThermalTank( WaterHeaterNum ).Name, TestNum, LocalRunFlag, LocalInitLoopEquip, MyLoad, MinCap, MaxCap, OptCap, FirstHVACIteration );
				if ( TestNum != WaterHeaterNum ) {
					ShowFatalError( "SimulateWaterHeaterStandAlone: Input WaterHeater Num [" + TrimSigDigits( WaterHeaterNum ) + "] does not match returned WaterHeater Num[" + TrimSigDigits( TestNum ) + "] Name=\"" + WaterThermalTank( WaterHeaterNum ).Name + "\"." );
				}
			}
		}

	}

	void
	SimHeatPumpWaterHeater(
		std::string const & CompName,
		bool const FirstHVACIteration,
		Real64 & SensLoadMet, // sensible load met by this equipment and sent to zone, W
		Real64 & LatLoadMet, // net latent load met and sent to zone (kg/s), dehumid = negative
		int & CompIndex
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   April 2005
		//       MODIFIED       Don Shirey, Aug 2009 (LatLoadMet)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine acts as an interface to SimWaterHeater.
		// HPWHs defined as zone equipment and not connected to a plant loop are called here by ZoneEquipmentManager

		// METHODOLOGY EMPLOYED:
		// The necessary control flags and dummy variables are set and passed into SimWaterHeater.

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataGlobals::DoingSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool LocalRunFlag; // local variables of similar name as others used in Sim modules
		bool LocalInitLoopEquip;
		int LocalFlowLock; // local variables of similar name as others used in sim modules
		int HeatPumpNum;
		Real64 MyLoad;
		Real64 MinCap;
		Real64 MaxCap;
		Real64 OptCap;

		// FLOW:
		if ( GetWaterThermalTankInputFlag ) {
			GetWaterThermalTankInput();
			GetWaterThermalTankInputFlag = false;
		}

		// Find the correct Heat Pump Water Heater
		if ( CompIndex == 0 ) {
			HeatPumpNum = FindItemInList( CompName, HPWaterHeater.Name(), NumHeatPumpWaterHeater );
			if ( HeatPumpNum == 0 ) {
				ShowFatalError( "SimHeatPumpWaterHeater: Unit not found=" + CompName );
			}
			CompIndex = HeatPumpNum;
		} else {
			HeatPumpNum = CompIndex;
			if ( HeatPumpNum > NumHeatPumpWaterHeater || HeatPumpNum < 1 ) {
				ShowFatalError( "SimHeatPumpWaterHeater:  Invalid CompIndex passed=" + TrimSigDigits( HeatPumpNum ) + ", Number of Units=" + TrimSigDigits( NumHeatPumpWaterHeater ) + ", Entered Unit name=" + CompName );
			}
		}

		// Only simulate HPWHs specified as zone equipment and not connected to a plant loop.
		// HPWHs not defined as zone equipment with no plant connections are simulated in NonZoneEquipmentManager.
		// Plant connected HPWHs are called by PlantLoopEquipments (but only those on supply side ).
		SensLoadMet = 0.0;
		LatLoadMet = 0.0;

		LocalRunFlag = true;
		LocalFlowLock = 1; // .TRUE.
		LocalInitLoopEquip = false;

		// HPWH will not be included in sizing calculations, fan is initialized only during BeginEnvrnFlag (FALSE during sizing)
		// (fan will be turned off during Standard Ratings procedure yielding incorrect results)
		if ( DoingSizing ) return;

		// For HPWHs, StandAlone means not connected to a plant loop (use nodes are not used, source nodes are connected to a HPWH)
		if ( HPWaterHeater( HeatPumpNum ).StandAlone ) {
			SimWaterThermalTank( HPWaterHeater( HeatPumpNum ).TypeNum, HPWaterHeater( HeatPumpNum ).Name, HeatPumpNum, LocalRunFlag, LocalInitLoopEquip, MyLoad, MinCap, MaxCap, OptCap, FirstHVACIteration );
			SensLoadMet = HPWaterHeater( HeatPumpNum ).HPWaterHeaterSensibleCapacity;
			LatLoadMet = HPWaterHeater( HeatPumpNum ).HPWaterHeaterLatentCapacity;
		} else {
			// HPWH is plant connected and will get simulated when called from plant SimWaterThermalTank, but need to update loads met here
			SensLoadMet = HPWaterHeater( HeatPumpNum ).HPWaterHeaterSensibleCapacity;
			LatLoadMet = HPWaterHeater( HeatPumpNum ).HPWaterHeaterLatentCapacity;
		}

	}

	void
	CalcWaterThermalTankZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   March 2005
		//       MODIFIED       B. Griffith November 2011, new internal gains structure
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the zone internal gains due to water heater skin losses during sizing.
		// initilizes gains to zone at begin environment.

		// METHODOLOGY EMPLOYED:
		// Sums the tank losses from all of the water heaters in the zone to add as a gain to the zone.
		// Now used to determine tank losses during sizing.  Internal gains are summed in a centralized way now

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::DoingSizing;
		using DataHeatBalFanSys::MAT;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterThermalTankNum;
		int ZoneNum;
		static bool MyEnvrnFlag( true );
		Real64 TankTemp;
		Real64 QLossToZone;
		int SchIndex;

		// FLOW:
		if ( NumWaterThermalTank == 0 ) {

			if ( ! DoingSizing ) {
				return;
			} else {
				if ( GetWaterThermalTankInputFlag ) {
					GetWaterThermalTankInput();
					GetWaterThermalTankInputFlag = false;
				}
				if ( NumWaterThermalTank == 0 ) return;
			}

		}

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			WaterThermalTank.AmbientZoneGain() = 0.0;
			WaterThermalTank.FuelEnergy() = 0.0;
			WaterThermalTank.OffCycParaFuelEnergy() = 0.0;
			WaterThermalTank.OnCycParaFuelEnergy() = 0.0;
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		for ( WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum ) {
			if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone == 0 ) continue;
			ZoneNum = WaterThermalTank( WaterThermalTankNum ).AmbientTempZone;
			if ( DoingSizing ) {
				// Initialize tank temperature to setpoint
				// (use HPWH or Desuperheater heating coil set point if applicable)
				if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {
					SchIndex = HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTempSchedule;
				} else if ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum > 0 ) {
					SchIndex = WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).SetPointTempSchedule;
				} else {
					SchIndex = WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule;
				}

				if ( SchIndex > 0 ) {
					TankTemp = GetCurrentScheduleValue( SchIndex );
				} else {
					TankTemp = 20.0;
				}
				{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).TypeNum );
				if ( SELECT_CASE_var == MixedWaterHeater ) {
					QLossToZone = max( WaterThermalTank( WaterThermalTankNum ).OnCycLossCoeff * WaterThermalTank( WaterThermalTankNum ).OnCycLossFracToZone, WaterThermalTank( WaterThermalTankNum ).OffCycLossCoeff * WaterThermalTank( WaterThermalTankNum ).OffCycLossFracToZone ) * ( TankTemp - MAT( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone ) );
				} else if ( SELECT_CASE_var == StratifiedWaterHeater ) {
					QLossToZone = max( WaterThermalTank( WaterThermalTankNum ).Node( 1 ).OnCycLossCoeff * WaterThermalTank( WaterThermalTankNum ).SkinLossFracToZone, WaterThermalTank( WaterThermalTankNum ).Node( 1 ).OffCycLossCoeff * WaterThermalTank( WaterThermalTankNum ).SkinLossFracToZone ) * ( TankTemp - MAT( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone ) );
				} else if ( SELECT_CASE_var == MixedChilledWaterStorage ) {
					QLossToZone = WaterThermalTank( WaterThermalTankNum ).OffCycLossCoeff * WaterThermalTank( WaterThermalTankNum ).OffCycLossFracToZone * ( TankTemp - MAT( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone ) );
				} else if ( SELECT_CASE_var == StratifiedChilledWaterStorage ) {
					QLossToZone = WaterThermalTank( WaterThermalTankNum ).Node( 1 ).OffCycLossCoeff * WaterThermalTank( WaterThermalTankNum ).SkinLossFracToZone * ( TankTemp - MAT( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone ) );
				}}
				WaterThermalTank( WaterThermalTankNum ).AmbientZoneGain = QLossToZone;
			}
		}

	}

	void
	GetWaterThermalTankInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher and Brandon Anderson
		//       DATE WRITTEN   May 2000
		//       MODIFIED       R. Raustad, June 2005, added HPWH and desuperheater water heating coils
		//                      B. Griffith, Oct. 2007 extensions for indirect water heaters
		//                      B. Griffith, Feb. 2008 extensions for autosizing water heaters
		//                      BG Mar 2009.  Trap for bad heater height input for stratefied water heater CR7718
		//						B. Shen 12/2014, add air-source variable-speed heat pump water heating
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the water heater, HPWH, and/or desuperheater heating coil input from the input file.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using DataGlobals::AutoCalculate;
		using DataGlobals::ScheduleAlwaysOn;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace DataIPShortCuts;
		using NodeInputManager::GetOnlySingleNode;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using BranchNodeConnections::TestCompSet;
		using BranchNodeConnections::SetUpCompSets;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using FluidProperties::GetDensityGlycol;
		using DataLoopNode::Node; // ,NodeConnectionType_Internal
		using DataLoopNode::NodeType_Air;
		using DataLoopNode::NodeType_Water;
		using DataLoopNode::NodeConnectionType_Inlet;
		using DataLoopNode::NodeConnectionType_Outlet;
		using DataLoopNode::NodeConnectionType_ReliefAir;
		using DataLoopNode::NodeConnectionType_OutsideAirReference;
		using DataLoopNode::ObjectIsParent;
		using DataLoopNode::ObjectIsNotParent;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::CurveValue;
		using DataHeatBalance::Zone;
		using DataHeatBalance::IntGainTypeOf_WaterHeaterMixed;
		using DataHeatBalance::IntGainTypeOf_WaterHeaterStratified;
		using DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterMixed;
		using DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterStratified;
		using DXCoils::DXCoil;
		using DXCoils::GetDXCoilIndex;
		using DXCoils::NumDXCoils;
		using VariableSpeedCoils::GetCoilIndexVariableSpeed;
		using VariableSpeedCoils::GetCoilCapacityVariableSpeed;
		using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
		using VariableSpeedCoils::GetVSCoilPLFFPLR;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using Fans::GetFanType;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using DataSizing::AutoSize;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::GetZoneEquipmentData;
		using DataEnvironment::OutBaroPress;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataHVACGlobals::BlowThru;
		using DataHVACGlobals::DrawThru;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using RefrigeratedCase::CheckRefrigerationInput;
		using GlobalNames::VerifyUniqueCoilName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetWaterThermalTankInput: " );
		static std::string const RoutineNameNoColon( "GetWaterThermalTankInput" );

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterThermalTankNum; // Index to WATER HEATER:*
		int WHsizingNum; // Index to Water Heater:Sizing, for the IDF objects--not data storage
		int NodeNum; // Index to a stratified thermal node
		int CheckWaterHeaterNum; // Used to search WATER HEATER:* to find association with HP Water Heater
		int DesuperheaterNum; // Index to Coil:WaterHeating:Desuperheater
		int HPWaterHeaterNum; // Index to HEAT PUMP:WATER HEATER
		int HeatingSourceNum; // Index to DX cooling coil (heat source for desuperheater)
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		//unused1208  INTEGER                     :: NumArgs                 ! Number of elements in the object (alpha + numeric)
		int RackNum; // Index to refrigrated display case rack
		int CondNum; // Index to refrigration condenser
		int DXCoilNum; // Index to DX coils
		int IOStat; // IO Status when calling get input subroutine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool IsValid; // Flag for validating PLF curve, OutsideAirNode
		static bool ErrorsFound( false ); // Flag for any error found during GetWaterThermalTankInput
		static std::string FanInletNode; // Used to set up comp set
		static std::string FanOutletNode; // Used to set up comp set
		static std::string CoilInletNode; // Used to set up comp set
		static std::string CoilOutletNode; // Used to set up comp set
		static int SupAirIn( 0 ); // Used for error checking HPWHs
		static int ExhAirOut( 0 ); // Used for error checking HPWHs
		static bool FoundInletNode( false ); // Used for error checking HPWHs
		static bool FoundOutletNode( false ); // Used for error checking HPWHs
		static int ZoneNum( 0 ); // Used for error checking HPWHs
		static bool ValidScheduleValue( false ); // Used for error checking HPWH's inlet air mixer schedule
		static int ZoneEquipConfigNum( 0 ); // Used to determine if HPWH tank is in a Zone Equipment List (ZEL)
		static int ZoneEquipListNum( 0 ); // Used to determine if HPWH tank is in a Zone Equipment List
		static int EquipmentTypeNum( 0 ); // Used to determine if HPWH tank is in a Zone Equipment List
		static bool FoundTankInList( false ); // Used to determine if HPWH tank is listed in a Zone Equipment List
		static bool TankNotLowestPriority( false ); // Used to determine if HPWH tank is prioritized correctly in ZEL
		static int TankCoolingPriority( 0 ); // Used to determine if a HPWH tank is prioritized correctly in ZEL
		static int TankHeatingPriority( 0 ); // Used to determine if a HPWH tank is prioritized correctly in ZEL
		static bool DXCoilErrFlag( false ); // Used for error checking DX coils used with HPWHs
		static Real64 FanVolFlow( 0.0 ); // Used for error checking fans used with HPWHs
		static bool errFlag( false ); // Used for error checking used with HPWHs
		static Real64 HEffFTemp( 0.0 ); // Used for error checking desuperheater heating coils
		bool Okay;
		bool bIsVScoil( false ); // indicate if the heat pump WH coil is a variable-speed coil

		// Following allow for temporary storage of character strings but not saved in main structure
		Real64 rho; // local fluid density
		static int DummyWaterIndex( 1 );

		struct WaterHeaterSaveNodes
		{
			// Members
			std::string InletNodeName1;
			std::string OutletNodeName1;
			std::string InletNodeName2;
			std::string OutletNodeName2;

			// Default Constructor
			WaterHeaterSaveNodes()
			{}

			// Member Constructor
			WaterHeaterSaveNodes(
				std::string const & InletNodeName1,
				std::string const & OutletNodeName1,
				std::string const & InletNodeName2,
				std::string const & OutletNodeName2
			) :
				InletNodeName1( InletNodeName1 ),
				OutletNodeName1( OutletNodeName1 ),
				InletNodeName2( InletNodeName2 ),
				OutletNodeName2( OutletNodeName2 )
			{}

		};

		// Object Data
		Array1D< WaterHeaterSaveNodes > HPWHSaveNodeNames; // temporary for HPWH node names used in later checks
		Array1D< WaterHeaterSaveNodes > WHSaveNodeNames; // temporary for WH node names used in later checks
		Array1D< WaterHeaterSaveNodes > CoilSaveNodeNames; // temporary for coil node names used in later checks

		// Formats
		static gio::Fmt Format_720( "('! <Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},Standard Rated Recovery Efficiency, ','Standard Rated Energy Factor')" );
		static gio::Fmt Format_721( "('! <Heat Pump Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},','Standard Rated Recovery Efficiency,Standard Rated Energy Factor,\"DX Coil Total Cooling Rate {W, HPWH Only}\"')" );
		static gio::Fmt Format_722( "('! <Water Heater Stratified Node Information>,Node Number,Height {m},Volume {m3},Maximum Capacity {W},','Off-Cycle UA {W/K},On-Cycle UA {W/K},Number Of Inlets,Number Of Outlets')" );
		static gio::Fmt Format_725( "('! <Chilled Water Tank Information>,Type,Name,Volume {m3},Use Side Design Flow Rate {m3/s}, ','Source Side Design Flow Rate {m3/s}')" );
		static gio::Fmt Format_726( "('! <Chilled Water Tank Stratified Node Information>,Node Number,Height {m},Volume {m3},','UA {W/K},Number Of Inlets,Number Of Outlets')" );
		static gio::Fmt Format_723( "('Water Heater Stratified Node Information',8(',',A))" );
		static gio::Fmt Format_724( "('Chilled Water Tank Stratified Node Information',6(',',A))" );

		// FLOW:

		// Make sure refrigeration input is gotten before this input
		CheckRefrigerationInput();

		if ( GetWaterThermalTankInputFlag ) {
			NumWaterHeaterMixed = GetNumObjectsFound( cMixedWHModuleObj );
			NumWaterHeaterStratified = GetNumObjectsFound( cStratifiedWHModuleObj );
			NumChilledWaterMixed = GetNumObjectsFound( cMixedCWTankModuleObj );
			NumChilledWaterStratified = GetNumObjectsFound( cStratifiedCWTankModuleObj );
			NumWaterThermalTank = NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + NumChilledWaterStratified;
			NumHeatPumpWaterHeater = GetNumObjectsFound( "WaterHeater:HeatPump" );
			NumWaterHeaterDesuperheater = GetNumObjectsFound( "Coil:WaterHeating:Desuperheater" );

			if ( NumWaterThermalTank > 0 ) {
				// Write water heater header for EIO
				if ( ( NumWaterHeaterMixed > 0 ) || ( NumWaterHeaterStratified > 0 ) ) gio::write( OutputFileInits, Format_720 );
				if ( NumHeatPumpWaterHeater > 0 ) gio::write( OutputFileInits, Format_721 );
				if ( NumWaterHeaterStratified > 0 ) gio::write( OutputFileInits, Format_722 );
				if ( NumChilledWaterMixed > 0 ) gio::write( OutputFileInits, Format_725 );
				if ( NumChilledWaterStratified > 0 ) gio::write( OutputFileInits, Format_726 );

			}

			if ( NumWaterThermalTank > 0 ) {
				WaterThermalTank.allocate( NumWaterThermalTank );
				WHSaveNodeNames.allocate( NumWaterThermalTank );
				CheckWTTEquipName.dimension( NumWaterThermalTank, true );
			}
			if ( NumHeatPumpWaterHeater > 0 ) {
				HPWaterHeater.allocate( NumHeatPumpWaterHeater );
				MyHPSizeFlag.dimension( NumHeatPumpWaterHeater, true );
				CheckHPWHEquipName.dimension( NumHeatPumpWaterHeater, true );
				HPWHSaveNodeNames.allocate( NumHeatPumpWaterHeater );
			}
			if ( NumWaterHeaterDesuperheater > 0 ) {
				WaterHeaterDesuperheater.allocate( NumWaterHeaterDesuperheater );
				ValidSourceType.dimension( NumWaterHeaterDesuperheater, false );
				CoilSaveNodeNames.allocate( NumWaterHeaterDesuperheater );
			}

			//!!=======   Get Coil:WaterHeating:Desuperheater ======================================================================
			if ( NumWaterHeaterDesuperheater > 0 ) {
				cCurrentModuleObject = "Coil:WaterHeating:Desuperheater";
				for ( DesuperheaterNum = 1; DesuperheaterNum <= NumWaterHeaterDesuperheater; ++DesuperheaterNum ) {

					GetObjectItem( cCurrentModuleObject, DesuperheaterNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), WaterHeaterDesuperheater.Name(), DesuperheaterNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					VerifyUniqueCoilName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
					if ( errFlag ) {
						ErrorsFound = true;
					}
					WaterHeaterDesuperheater( DesuperheaterNum ).Name = cAlphaArgs( 1 );
					WaterHeaterDesuperheater( DesuperheaterNum ).Type = cCurrentModuleObject;

					//       convert availability schedule name to pointer
					if ( ! lAlphaFieldBlanks( 2 ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).AvailSchedPtr == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
					} else {
						WaterHeaterDesuperheater( DesuperheaterNum ).AvailSchedPtr = ScheduleAlwaysOn;
					}

					//       convert schedule name to pointer
					WaterHeaterDesuperheater( DesuperheaterNum ).SetPointTempSchedule = GetScheduleIndex( cAlphaArgs( 3 ) );
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).SetPointTempSchedule == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

					WaterHeaterDesuperheater( DesuperheaterNum ).DeadBandTempDiff = rNumericArgs( 1 );
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).DeadBandTempDiff <= 0.0 || WaterHeaterDesuperheater( DesuperheaterNum ).DeadBandTempDiff > 20.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 1 ) + " must be > 0 and <= 20. " + cNumericFieldNames( 1 ) + " = " + TrimSigDigits( rNumericArgs( 1 ), 1 ) );
						ErrorsFound = true;
					}

					//WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = rNumericArgs(2)
					// Error limits on heat reclaim efficiency applied after source type identified

					WaterHeaterDesuperheater( DesuperheaterNum ).RatedInletWaterTemp = rNumericArgs( 3 );
					WaterHeaterDesuperheater( DesuperheaterNum ).RatedOutdoorAirTemp = rNumericArgs( 4 );
					WaterHeaterDesuperheater( DesuperheaterNum ).MaxInletWaterTemp = rNumericArgs( 5 );

					if ( ! lAlphaFieldBlanks( 4 ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp = GetCurveIndex( cAlphaArgs( 4 ) );
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ":  " + cAlphaFieldNames( 4 ) + " not found = " + cAlphaArgs( 4 ) );
							ErrorsFound = true;
						} else {
							// Verify Curve Object, only legal type is Quadratic
							{ auto const SELECT_CASE_var( GetCurveType( WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp ) );

							if ( SELECT_CASE_var == "BIQUADRATIC" ) {

								if ( WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp > 0 ) {
									HEffFTemp = min( 1.0, max( 0.0, CurveValue( WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp, WaterHeaterDesuperheater( DesuperheaterNum ).RatedInletWaterTemp, WaterHeaterDesuperheater( DesuperheaterNum ).RatedOutdoorAirTemp ) ) );
									if ( std::abs( HEffFTemp - 1.0 ) > 0.05 ) {
										ShowWarningError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\":" );
										ShowContinueError( "The " + cAlphaFieldNames( 4 ) + " should be normalized " );
										ShowContinueError( " to 1.0 at the rating point. Curve output at the rating point = " + TrimSigDigits( HEffFTemp, 3 ) );
										ShowContinueError( " The simulation continues using the user-specified curve." );
									}
								}

							} else {
								ShowSevereError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\" illegal " + cAlphaFieldNames( 4 ) + " type for this object = " + GetCurveType( WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp ) );
								ErrorsFound = true;
							}}
						}
					}

					WaterHeaterDesuperheater( DesuperheaterNum ).WaterInletNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsParent );

					WaterHeaterDesuperheater( DesuperheaterNum ).WaterOutletNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsParent );

					CoilSaveNodeNames( DesuperheaterNum ).InletNodeName1 = cAlphaArgs( 5 );
					CoilSaveNodeNames( DesuperheaterNum ).OutletNodeName1 = cAlphaArgs( 6 );

					WaterHeaterDesuperheater( DesuperheaterNum ).TankType = cAlphaArgs( 7 );

					if ( ! SameString( WaterHeaterDesuperheater( DesuperheaterNum ).TankType, cMixedWHModuleObj ) && ! SameString( WaterHeaterDesuperheater( DesuperheaterNum ).TankType, cStratifiedWHModuleObj ) ) {

						ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( DesuperheaterNum ).Name + ':' );
						ShowContinueError( "Desuperheater can only be used with " + cMixedWHModuleObj + " or " + cStratifiedWHModuleObj + '.' );
						ErrorsFound = true;
					}

					WaterHeaterDesuperheater( DesuperheaterNum ).TankName = cAlphaArgs( 8 );

					//       get heat reclaim object
					if ( SameString( cAlphaArgs( 9 ), "Coil:Cooling:DX:SingleSpeed" ) || SameString( cAlphaArgs( 9 ), "Coil:Cooling:DX:TwoSpeed" ) || SameString( cAlphaArgs( 9 ), "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).HeatingSourceType = cAlphaArgs( 9 );
						WaterHeaterDesuperheater( DesuperheaterNum ).HeatingSourceName = cAlphaArgs( 10 );
						//         load DX coil structure for connection to desuperheater heating coil (refrigerated rack have been loaded)
						errFlag = false;
						GetDXCoilIndex( WaterHeaterDesuperheater( DesuperheaterNum ).HeatingSourceName, HeatingSourceNum, errFlag, cCurrentModuleObject );
						if ( errFlag ) {
							ShowContinueError( "...occurs in " + cCurrentModuleObject + '=' + WaterHeaterDesuperheater( DesuperheaterNum ).Name );
							ErrorsFound = true;
						}
					} else if ( ( SameString( cAlphaArgs( 9 ), "Refrigeration:CompressorRack" ) ) || ( SameString( cAlphaArgs( 9 ), "Refrigeration:Condenser:AirCooled" ) ) || ( SameString( cAlphaArgs( 9 ), "Refrigeration:Condenser:EvaporativeCooled" ) ) || ( SameString( cAlphaArgs( 9 ), "Refrigeration:Condenser:WaterCooled" ) ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).HeatingSourceType = cAlphaArgs( 9 );
						WaterHeaterDesuperheater( DesuperheaterNum ).HeatingSourceName = cAlphaArgs( 10 );
					} else {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ':' );
						ShowContinueError( " desuperheater can only be used with Coil:Cooling:DX:SingleSpeed, " );
						ShowContinueError( " Coil:Cooling:DX:TwoSpeed, Coil:Cooling:DX:TwoStageWithHumidityControlMode, Refrigeration:CompressorRack," );
						ShowContinueError( " Refrigeration:Condenser:AirCooled ,Refrigeration:Condenser:EvaporativeCooled, " );
						ShowContinueError( " or Refrigeration:Condenser:WaterCooled." );
						ErrorsFound = true;
					}

					//       Set up comp set for water side nodes (reverse inlet/outlet for water heater)
					SetUpCompSets( WaterHeaterDesuperheater( DesuperheaterNum ).Type, WaterHeaterDesuperheater( DesuperheaterNum ).Name, WaterHeaterDesuperheater( DesuperheaterNum ).TankType, WaterHeaterDesuperheater( DesuperheaterNum ).TankName, cAlphaArgs( 6 ), cAlphaArgs( 5 ) );

					//       Find the DX equipment index associated with the desuperheater heating coil.
					if ( SameString( cAlphaArgs( 9 ), "Refrigeration:CompressorRack" ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource = COMPRESSORRACK_REFRIGERATEDCASE;
						for ( RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum ) {
							if ( ! SameString( HeatReclaimRefrigeratedRack( RackNum ).Name, cAlphaArgs( 10 ) ) ) continue;
							WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum = RackNum;
							if ( allocated( HeatReclaimRefrigeratedRack ) ) ValidSourceType( DesuperheaterNum ) = true;
							break;
						}
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\" desuperheater heat source object not found: " + cAlphaArgs( 9 ) + " \"" + cAlphaArgs( 10 ) + "\"" );
							ErrorsFound = true;
						}
					} else if ( ( SameString( cAlphaArgs( 9 ), "Refrigeration:Condenser:AirCooled" ) ) || ( SameString( cAlphaArgs( 9 ), "Refrigeration:Condenser:EvaporativeCooled" ) ) || ( SameString( cAlphaArgs( 9 ), "Refrigeration:Condenser:WaterCooled" ) ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource = CONDENSER_REFRIGERATION;
						for ( CondNum = 1; CondNum <= NumRefrigCondensers; ++CondNum ) {
							if ( ! SameString( HeatReclaimRefrigCondenser( CondNum ).Name, cAlphaArgs( 10 ) ) ) continue;
							WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum = CondNum;
							if ( allocated( HeatReclaimRefrigCondenser ) ) ValidSourceType( DesuperheaterNum ) = true;
							break;
						}
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\" desuperheater heat source object not found: " + cAlphaArgs( 9 ) + " \"" + cAlphaArgs( 10 ) + "\"" );
							ErrorsFound = true;
						}
					} else if ( SameString( cAlphaArgs( 9 ), "Coil:Cooling:DX:SingleSpeed" ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource = COIL_DX_COOLING;
						for ( DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum ) {
							if ( ! SameString( HeatReclaimDXCoil( DXCoilNum ).Name, cAlphaArgs( 10 ) ) ) continue;
							WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum = DXCoilNum;
							if ( allocated( HeatReclaimDXCoil ) ) ValidSourceType( DesuperheaterNum ) = true;
							break;
						}
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\" desuperheater heat source object not found: " + cAlphaArgs( 9 ) + " \"" + cAlphaArgs( 10 ) + "\"" );
							ErrorsFound = true;
						}
					} else if ( SameString( cAlphaArgs( 9 ), "Coil:Cooling:DX:TwoSpeed" ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource = COIL_DX_MULTISPEED;
						for ( DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum ) {
							if ( ! SameString( HeatReclaimDXCoil( DXCoilNum ).Name, cAlphaArgs( 10 ) ) ) continue;
							WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum = DXCoilNum;
							if ( allocated( HeatReclaimDXCoil ) ) ValidSourceType( DesuperheaterNum ) = true;
							break;
						}
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\" desuperheater heat source object not found: " + cAlphaArgs( 9 ) + " \"" + cAlphaArgs( 10 ) + "\"" );
							ErrorsFound = true;
						}
					} else if ( SameString( cAlphaArgs( 9 ), "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource = COIL_DX_MULTIMODE;
						for ( DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum ) {
							if ( ! SameString( HeatReclaimDXCoil( DXCoilNum ).Name, cAlphaArgs( 10 ) ) ) continue;
							WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum = DXCoilNum;
							if ( allocated( HeatReclaimDXCoil ) ) ValidSourceType( DesuperheaterNum ) = true;
							break;
						}
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\" desuperheater heat source object not found: " + cAlphaArgs( 9 ) + " \"" + cAlphaArgs( 10 ) + "\"" );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\" invalid desuperheater heat source object: " + cAlphaArgs( 9 ) + " \"" + cAlphaArgs( 10 ) + "\"" );
						ErrorsFound = true;
					}

					//Now have source type, so set limits on heat recovery efficiency
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
						if ( lNumericFieldBlanks( 2 ) ) {
							WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff = 0.8;
						} else {
							WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff = rNumericArgs( 2 );
							if ( WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff <= 0.0 || WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff > 0.9 ) {
								ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 2 ) + " must be > 0.0 and <= 0.9, Efficiency = " + TrimSigDigits( WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff, 3 ) );
								ErrorsFound = true;
							}
						} //Blank Num(2)
					} else { // max is 0.3 for all other sources
						if ( lNumericFieldBlanks( 2 ) ) {
							WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff = 0.25;
						} else {
							WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff = rNumericArgs( 2 );
							if ( WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff <= 0.0 || WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff > 0.3 ) {
								ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 2 ) + " must be > 0.0 and <= 0.3, " + cNumericFieldNames( 2 ) + " = " + TrimSigDigits( WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff, 3 ) );
								ErrorsFound = true;
							}
						} //Blank Num(2)
					} //setting limits on heat recovery efficiency

					WaterHeaterDesuperheater( DesuperheaterNum ).OperatingWaterFlowRate = rNumericArgs( 6 );
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).OperatingWaterFlowRate <= 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 6 ) + " must be greater than 0. " + cNumericFieldNames( 6 ) + " = " + TrimSigDigits( rNumericArgs( 6 ), 6 ) );
						ErrorsFound = true;
					}

					WaterHeaterDesuperheater( DesuperheaterNum ).PumpElecPower = rNumericArgs( 7 );
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).PumpElecPower < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 7 ) + " must be >= 0. " + cNumericFieldNames( 7 ) + " = " + TrimSigDigits( rNumericArgs( 7 ), 2 ) );
						ErrorsFound = true;
					}

					if ( ( WaterHeaterDesuperheater( DesuperheaterNum ).PumpElecPower / WaterHeaterDesuperheater( DesuperheaterNum ).OperatingWaterFlowRate ) > 7.9264e6 ) {
						ShowWarningError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 7 ) + " to " + cNumericFieldNames( 6 ) + " ratio > 7.9264E6. " + cNumericFieldNames( 7 ) + " to " + cNumericFieldNames( 6 ) + " = " + TrimSigDigits( ( WaterHeaterDesuperheater( DesuperheaterNum ).PumpElecPower / WaterHeaterDesuperheater( DesuperheaterNum ).OperatingWaterFlowRate ), 3 ) );
						ShowContinueError( " Suggest reducing " + cNumericFieldNames( 7 ) + " or increasing " + cNumericFieldNames( 6 ) + '.' );
						ShowContinueError( " The simulation will continue using the user defined values." );
					}

					WaterHeaterDesuperheater( DesuperheaterNum ).PumpFracToWater = rNumericArgs( 8 );
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).PumpFracToWater < 0.0 || WaterHeaterDesuperheater( DesuperheaterNum ).PumpFracToWater > 1.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 8 ) + " must be >= 0 or <= 1. " + cNumericFieldNames( 8 ) + " = " + TrimSigDigits( rNumericArgs( 8 ), 3 ) );
						ErrorsFound = true;
					}

					WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaLoad = rNumericArgs( 9 );
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaLoad < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 9 ) + " must be >= 0. " + cNumericFieldNames( 9 ) + " = " + TrimSigDigits( rNumericArgs( 9 ), 2 ) );
						ErrorsFound = true;
					}

					WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaLoad = rNumericArgs( 10 );
					if ( WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaLoad < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ": " + cNumericFieldNames( 10 ) + " must be >= 0. " + cNumericFieldNames( 10 ) + " = " + TrimSigDigits( rNumericArgs( 10 ), 2 ) );
						ErrorsFound = true;
					}

				}

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			}

			//!!=======   Get HEAT PUMP:WATER HEATER ===============================================================================

			//   get input for heat pump water heater object
			if ( NumHeatPumpWaterHeater > 0 ) {
				cCurrentModuleObject = "WaterHeater:HeatPump";
				for ( HPWaterHeaterNum = 1; HPWaterHeaterNum <= NumHeatPumpWaterHeater; ++HPWaterHeaterNum ) {

					GetObjectItem( cCurrentModuleObject, HPWaterHeaterNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), HPWaterHeater.Name(), HPWaterHeaterNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}

					HPWaterHeater( HPWaterHeaterNum ).Name = cAlphaArgs( 1 );
					HPWaterHeater( HPWaterHeaterNum ).Type = cCurrentModuleObject;
					HPWaterHeater( HPWaterHeaterNum ).TypeNum = HeatPumpWaterHeater;

					//       convert schedule name to pointer
					if ( ! lAlphaFieldBlanks( 2 ) ) {
						HPWaterHeater( HPWaterHeaterNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( HPWaterHeater( HPWaterHeaterNum ).AvailSchedPtr == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
							ShowContinueError( cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
							ErrorsFound = true;
						}
					} else {
						HPWaterHeater( HPWaterHeaterNum ).AvailSchedPtr = ScheduleAlwaysOn;
					}

					//       convert schedule name to pointer
					if ( ! lAlphaFieldBlanks( 3 ) ) {
						HPWaterHeater( HPWaterHeaterNum ).SetPointTempSchedule = GetScheduleIndex( cAlphaArgs( 3 ) );
						if ( HPWaterHeater( HPWaterHeaterNum ).SetPointTempSchedule == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
							ShowContinueError( cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
						ShowContinueError( "required " + cAlphaFieldNames( 3 ) + " is blank." );
						ErrorsFound = true;
					}

					HPWaterHeater( HPWaterHeaterNum ).DeadBandTempDiff = rNumericArgs( 1 );
					if ( HPWaterHeater( HPWaterHeaterNum ).DeadBandTempDiff <= 0.0 || HPWaterHeater( HPWaterHeaterNum ).DeadBandTempDiff > 20.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
						ShowContinueError( cNumericFieldNames( 1 ) + " difference must be > 0 and <= 20. Dead band = " + TrimSigDigits( rNumericArgs( 1 ), 1 ) );
						ErrorsFound = true;
					}

					HPWaterHeater( HPWaterHeaterNum ).CondWaterInletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsParent );
					HPWHSaveNodeNames( HPWaterHeaterNum ).InletNodeName1 = cAlphaArgs( 4 );
					HPWaterHeater( HPWaterHeaterNum ).CondWaterOutletNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsParent );
					HPWHSaveNodeNames( HPWaterHeaterNum ).OutletNodeName1 = cAlphaArgs( 5 );

					HPWaterHeater( HPWaterHeaterNum ).OperatingWaterFlowRate = rNumericArgs( 2 );
					if ( HPWaterHeater( HPWaterHeaterNum ).OperatingWaterFlowRate <= 0.0 && rNumericArgs( 2 ) != AutoCalculate ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
						ShowContinueError( cNumericFieldNames( 2 ) + " must be greater than 0. Condenser water flow rate = " + TrimSigDigits( rNumericArgs( 2 ), 6 ) );
						ErrorsFound = true;
					}

					HPWaterHeater( HPWaterHeaterNum ).OperatingAirFlowRate = rNumericArgs( 3 );
					if ( HPWaterHeater( HPWaterHeaterNum ).OperatingAirFlowRate <= 0.0 && rNumericArgs( 3 ) != AutoCalculate ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
						ShowContinueError( cNumericFieldNames( 3 ) + " must be greater than 0. Evaporator air flow rate = " + TrimSigDigits( rNumericArgs( 3 ), 6 ) );
						ErrorsFound = true;
					}

					{ auto const SELECT_CASE_var( cAlphaArgs( 6 ) );

					if ( SELECT_CASE_var == "SCHEDULE" ) {
						HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration = AmbientTempSchedule;
						if ( ! lAlphaFieldBlanks( 11 ) ) {
							HPWaterHeater( HPWaterHeaterNum ).AmbientTempSchedule = GetScheduleIndex( cAlphaArgs( 11 ) );
							if ( HPWaterHeater( HPWaterHeaterNum ).AmbientTempSchedule == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
								ShowContinueError( cAlphaFieldNames( 11 ) + "=\"" + cAlphaArgs( 11 ) + "\"." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
							ShowContinueError( "required " + cAlphaFieldNames( 11 ) + " is blank." );
							ErrorsFound = true;
						}
						if ( ! lAlphaFieldBlanks( 12 ) ) {
							HPWaterHeater( HPWaterHeaterNum ).AmbientRHSchedule = GetScheduleIndex( cAlphaArgs( 12 ) );
							if ( HPWaterHeater( HPWaterHeaterNum ).AmbientRHSchedule == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
								ShowContinueError( cAlphaFieldNames( 12 ) + "=\"" + cAlphaArgs( 12 ) + "\"." );
								ErrorsFound = true;
							} else {
								if ( ! CheckScheduleValueMinMax( HPWaterHeater( HPWaterHeaterNum ).AmbientRHSchedule, ">=", 0.0, "<=", 1.0 ) ) {
									ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", invalid values" );
									ShowContinueError( cAlphaFieldNames( 12 ) + "=\"" + cAlphaArgs( 12 ) + "\", schedule values must be (>=0., <=1.)" );
									ErrorsFound = true;
								}
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
							ShowContinueError( "required " + cAlphaFieldNames( 12 ) + " is blank." );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "ZONEAIRONLY" ) {
						HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration = AmbientTempZone;
						if ( ! lAlphaFieldBlanks( 13 ) ) {
							HPWaterHeater( HPWaterHeaterNum ).AmbientTempZone = FindItemInList( cAlphaArgs( 13 ), Zone.Name(), NumOfZones );
							if ( HPWaterHeater( HPWaterHeaterNum ).AmbientTempZone == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
								ShowContinueError( cAlphaFieldNames( 13 ) + "=\"" + cAlphaArgs( 13 ) + "\"." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
							ShowContinueError( "required " + cAlphaFieldNames( 13 ) + " is blank." );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "OUTDOORAIRONLY" ) {
						HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration = AmbientTempOutsideAir;

					} else if ( SELECT_CASE_var == "ZONEANDOUTDOORAIR" ) {
						HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration = AmbientTempZoneAndOA;
						if ( ! lAlphaFieldBlanks( 13 ) ) {
							HPWaterHeater( HPWaterHeaterNum ).AmbientTempZone = FindItemInList( cAlphaArgs( 13 ), Zone.Name(), NumOfZones );
							if ( HPWaterHeater( HPWaterHeaterNum ).AmbientTempZone == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
								ShowContinueError( cAlphaFieldNames( 13 ) + "=\"" + cAlphaArgs( 13 ) + "\"." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
							ShowContinueError( "required " + cAlphaFieldNames( 13 ) + " is blank." );
							ErrorsFound = true;
						}

					}}

					//       Read air inlet nodes after mixer/splitter nodes have been read in (cAlphaArgs 7-10),
					//       Node_ConnectionType differs for inlet node if mixer/splitter node exists

					HPWaterHeater( HPWaterHeaterNum ).TankType = cAlphaArgs( 14 );

					if ( ! SameString( HPWaterHeater( HPWaterHeaterNum ).TankType, cMixedWHModuleObj ) && ! SameString( HPWaterHeater( HPWaterHeaterNum ).TankType, cStratifiedWHModuleObj ) ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
						ShowContinueError( "Heat pump water heater can only be used with " + cMixedWHModuleObj + " or " + cStratifiedWHModuleObj + '.' );
						ErrorsFound = true;
					}

					//       Verify tank name after Water Heater:Mixed objects have been read in
					HPWaterHeater( HPWaterHeaterNum ).TankName = cAlphaArgs( 15 );

					//       Get the water heater tank use side inlet node names for HPWHs connected to a plant loop
					//       Save the name of the node for use with set up comp sets
					HPWHSaveNodeNames( HPWaterHeaterNum ).InletNodeName2 = cAlphaArgs( 16 );
					HPWHSaveNodeNames( HPWaterHeaterNum ).OutletNodeName2 = cAlphaArgs( 17 );

					if ( ! lAlphaFieldBlanks( 16 ) && ! lAlphaFieldBlanks( 17 ) ) {
						HPWaterHeater( HPWaterHeaterNum ).WHUseInletNode = GetOnlySingleNode( cAlphaArgs( 16 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsParent );
						HPWaterHeater( HPWaterHeaterNum ).WHUseOutletNode = GetOnlySingleNode( cAlphaArgs( 17 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsParent );
					}

					//       get Coil:DX:HeatPumpWaterHeater object
					HPWaterHeater( HPWaterHeaterNum ).DXCoilType = cAlphaArgs( 18 );
					HPWaterHeater( HPWaterHeaterNum ).DXCoilName = cAlphaArgs( 19 );

					//       check that the DX Coil exists
					//		 allow both single-speed and variable-speed HPWH coils
					if (!(SameString(HPWaterHeater(HPWaterHeaterNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump") ||
						SameString(HPWaterHeater(HPWaterHeaterNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed"))) {
						ShowSevereError(cCurrentModuleObject + "=\"" + HPWaterHeater(HPWaterHeaterNum).Name + "\":");
						ShowContinueError("Heat pump water heater can only be used with Coil:WaterHeating:AirToWaterHeatPump or Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed.");
						ErrorsFound = true;
					}

					DXCoilErrFlag = false;
					bIsVScoil = false;
					if (SameString(HPWaterHeater(HPWaterHeaterNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump"))
						GetDXCoilIndex(HPWaterHeater(HPWaterHeaterNum).DXCoilName, HPWaterHeater(HPWaterHeaterNum).DXCoilNum, DXCoilErrFlag, cCurrentModuleObject);

					//find variable speed HPWH
					if (SameString(HPWaterHeater(HPWaterHeaterNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed")) {
						HPWaterHeater(HPWaterHeaterNum).DXCoilNum = GetCoilIndexVariableSpeed(HPWaterHeater(HPWaterHeaterNum).DXCoilType,
							HPWaterHeater(HPWaterHeaterNum).DXCoilName, DXCoilErrFlag);
						if (!DXCoilErrFlag) bIsVScoil = true; //it is a variable-speed coil
					}

					if ( DXCoilErrFlag ) {
						ShowContinueError( "...occurs in WaterHeater:HeatPump =" + HPWaterHeater( HPWaterHeaterNum ).Name );
						ShowContinueError( "...entered DX CoilType=" + HPWaterHeater( HPWaterHeaterNum ).DXCoilType );
						ErrorsFound = true;
					}

					//       Set up comp set for condenser water side nodes (reverse inlet/outlet for water heater)
					SetUpCompSets( HPWaterHeater( HPWaterHeaterNum ).Type, HPWaterHeater( HPWaterHeaterNum ).Name, HPWaterHeater( HPWaterHeaterNum ).DXCoilType, HPWaterHeater( HPWaterHeaterNum ).DXCoilName, cAlphaArgs( 4 ), cAlphaArgs( 5 ) );

					SetUpCompSets( HPWaterHeater( HPWaterHeaterNum ).Type, HPWaterHeater( HPWaterHeaterNum ).Name, HPWaterHeater( HPWaterHeaterNum ).TankType, HPWaterHeater( HPWaterHeaterNum ).TankName, cAlphaArgs( 5 ), cAlphaArgs( 4 ) );

					HPWaterHeater( HPWaterHeaterNum ).MinAirTempForHPOperation = rNumericArgs( 4 );
					if ( HPWaterHeater( HPWaterHeaterNum ).MinAirTempForHPOperation < 5 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\": minimum inlet air temperature for heat pump compressor operation must be greater than or equal to 5 C." );
						ShowContinueError( "...Minimum inlet air temperature = " + TrimSigDigits( rNumericArgs( 4 ), 1 ) );
					}

					//       Get compressor location
					{ auto const SELECT_CASE_var( cAlphaArgs( 20 ) );
					if ( SELECT_CASE_var == "SCHEDULE" ) {
						HPWaterHeater( HPWaterHeaterNum ).CrankcaseTempIndicator = CrankcaseTempSchedule;
						if ( ! lAlphaFieldBlanks( 21 ) ) {
							HPWaterHeater( HPWaterHeaterNum ).CrankcaseTempSchedule = GetScheduleIndex( cAlphaArgs( 21 ) );
							if ( HPWaterHeater( HPWaterHeaterNum ).CrankcaseTempSchedule == 0 ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
								ShowContinueError( cAlphaFieldNames( 21 ) + "=\"" + cAlphaArgs( 21 ) + "\"." );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", " );
							ShowContinueError( "required " + cAlphaFieldNames( 21 ) + " is blank." );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "ZONE" ) {
						HPWaterHeater( HPWaterHeaterNum ).CrankcaseTempIndicator = CrankcaseTempZone;
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempOutsideAir || HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempSchedule ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  Inlet Air Configuration must be Zone Air Only or Zone And" );
							ShowContinueError( " Outdoor Air when compressor location equals ZONE." );
							ErrorsFound = true;
						}

						if ( ! lAlphaFieldBlanks( 21 ) ) {
							ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  " + cAlphaFieldNames( 21 ) + " was provided but will not be used based on compressor location input=\"" + cAlphaArgs( 20 ) + "\"." );
						}
					} else if ( SELECT_CASE_var == "OUTDOORS" ) {
						HPWaterHeater( HPWaterHeaterNum ).CrankcaseTempIndicator = CrankcaseTempExterior;
						if ( ! lAlphaFieldBlanks( 21 ) ) {
							ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  " + cAlphaFieldNames( 21 ) + " was provided but will not be used based on " + cAlphaFieldNames( 21 ) + "=\"" + cAlphaArgs( 20 ) + "\"." );
						}

					}}

					HPWaterHeater( HPWaterHeaterNum ).FanType = cAlphaArgs( 22 );
					HPWaterHeater( HPWaterHeaterNum ).FanName = cAlphaArgs( 23 );

					//       check that the fan exists
					errFlag = false;
					GetFanIndex( HPWaterHeater( HPWaterHeaterNum ).FanName, HPWaterHeater( HPWaterHeaterNum ).FanNum, errFlag, cCurrentModuleObject );
					if ( errFlag ) {
						ShowContinueError( "...occurs in unit=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\"." );
						ErrorsFound = true;
					}

					errFlag = false;
					GetFanType( HPWaterHeater( HPWaterHeaterNum ).FanName, HPWaterHeater( HPWaterHeaterNum ).FanType_Num, errFlag, cCurrentModuleObject, HPWaterHeater( HPWaterHeaterNum ).Name );

					if ( errFlag ) {
						ErrorsFound = true;
					} else {
						if ( HPWaterHeater( HPWaterHeaterNum ).FanType_Num != FanType_SimpleOnOff ) {
							ShowSevereError( cCurrentModuleObject + " illegal fan type specified." );
							ShowContinueError( "Occurs in unit=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\"." );
							ShowContinueError( " The fan object (" + HPWaterHeater( HPWaterHeaterNum ).FanName + ") type must be Fan:OnOff when used with a heat pump water heater" );
							ErrorsFound = true;
						} else {
							if ( ! SameString( HPWaterHeater( HPWaterHeaterNum ).FanType, "Fan:OnOff" ) ) {
								ShowWarningError( cCurrentModuleObject + " illegal fan type = " + HPWaterHeater( HPWaterHeaterNum ).FanType );
								ShowContinueError( "Occurs in unit = " + HPWaterHeater( HPWaterHeaterNum ).Name );
								ShowContinueError( " The fan object (" + HPWaterHeater( HPWaterHeaterNum ).FanName + ") is actually the correct fan type and the simulation continues." );
								ShowContinueError( " Node connection errors will result due to the inconsistent fan type." );
							}
						}
					}

					GetFanVolFlow( HPWaterHeater( HPWaterHeaterNum ).FanNum, FanVolFlow );

					if ( FanVolFlow != AutoSize && ! errFlag ) {
						if ( FanVolFlow < HPWaterHeater( HPWaterHeaterNum ).OperatingAirFlowRate ) {
							ShowSevereError( cCurrentModuleObject + " - air flow rate = " + TrimSigDigits( FanVolFlow, 7 ) + " in fan object " + HPWaterHeater( HPWaterHeaterNum ).FanName + " is less than the  HPWHs evaporator air flow rate." );
							ShowContinueError( " The fan flow rate must be >= to the HPWHs evaporator volumetric air flow rate." );
							ShowContinueError( " Occurs in unit = " + HPWaterHeater( HPWaterHeaterNum ).Name );
							ErrorsFound = true;
						}
					}

					if ( SameString( cAlphaArgs( 24 ), "BlowThrough" ) ) {
						HPWaterHeater( HPWaterHeaterNum ).FanPlacement = BlowThru;

					} else if ( SameString( cAlphaArgs( 24 ), "DrawThrough" ) ) {
						HPWaterHeater( HPWaterHeaterNum ).FanPlacement = DrawThru;

					} else {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", invalid " );
						ShowContinueError( cAlphaFieldNames( 24 ) + "=\"" + cAlphaArgs( 24 ) + "\"." );
						ErrorsFound = true;
					}


					if ( ( HPWaterHeater(HPWaterHeaterNum).DXCoilNum > 0 ) && ( !bIsVScoil ) ) {
						//         get HPWH capacity, air inlet node, and PLF curve info from DX coil object
						HPWaterHeater(HPWaterHeaterNum).Capacity = DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).RatedTotCap2;
						HPWaterHeater(HPWaterHeaterNum).DXCoilAirInletNode = DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).AirInNode;
						HPWaterHeater(HPWaterHeaterNum).DXCoilPLFFPLR = DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).PLFFPLR(1);
						//         check the range of condenser pump power to be <= 5 gpm/ton
						if (DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).HPWHCondPumpElecNomPower / DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).RatedTotCap2 > 0.1422) {
							ShowWarningError(DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).DXCoilType + "= " + DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).Name + ": Rated condenser pump power per watt of rated heating capacity has exceeded the recommended maximum of 0.1422 W/W (41.67 watt/MBH). Condenser pump power per watt = " + TrimSigDigits((DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).HPWHCondPumpElecNomPower / DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).RatedTotCap2), 4));
						}
					} else if ( ( HPWaterHeater(HPWaterHeaterNum).DXCoilNum > 0 ) && ( bIsVScoil ) ) {
						HPWaterHeater(HPWaterHeaterNum).Capacity = GetCoilCapacityVariableSpeed(HPWaterHeater(HPWaterHeaterNum).DXCoilType,
							HPWaterHeater(HPWaterHeaterNum).DXCoilName, DXCoilErrFlag);
						HPWaterHeater(HPWaterHeaterNum).DXCoilAirInletNode = GetCoilInletNodeVariableSpeed(HPWaterHeater(HPWaterHeaterNum).DXCoilType,
							HPWaterHeater(HPWaterHeaterNum).DXCoilName, DXCoilErrFlag);
						HPWaterHeater(HPWaterHeaterNum).DXCoilPLFFPLR = GetVSCoilPLFFPLR(HPWaterHeater(HPWaterHeaterNum).DXCoilType,
							HPWaterHeater(HPWaterHeaterNum).DXCoilName, DXCoilErrFlag);
						//         check the range of condenser pump power to be <= 5 gpm/ton, will be checked in the coil object
					}

					if ( HPWaterHeater( HPWaterHeaterNum ).OperatingWaterFlowRate == AutoCalculate ) {
						HPWaterHeater( HPWaterHeaterNum ).OperatingWaterFlowRate = 0.00000004487 * HPWaterHeater( HPWaterHeaterNum ).Capacity;
						HPWaterHeater( HPWaterHeaterNum ).WaterFlowRateAutoSized = true;
					}

					if ( HPWaterHeater( HPWaterHeaterNum ).OperatingAirFlowRate == AutoCalculate ) {
						HPWaterHeater( HPWaterHeaterNum ).OperatingAirFlowRate = 0.00005035 * HPWaterHeater( HPWaterHeaterNum ).Capacity;
						HPWaterHeater( HPWaterHeaterNum ).AirFlowRateAutoSized = true;
					}

					HPWaterHeater( HPWaterHeaterNum ).OnCycParaLoad = rNumericArgs( 5 );
					if ( HPWaterHeater( HPWaterHeaterNum ).OnCycParaLoad < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\"," );
						ShowContinueError( cNumericFieldNames( 5 ) + " must be >= 0. " + cNumericFieldNames( 5 ) + " = " + TrimSigDigits( rNumericArgs( 5 ), 2 ) );
						ErrorsFound = true;
					}

					HPWaterHeater( HPWaterHeaterNum ).OffCycParaLoad = rNumericArgs( 6 );
					if ( HPWaterHeater( HPWaterHeaterNum ).OffCycParaLoad < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\"," );
						ShowContinueError( cNumericFieldNames( 6 ) + " must be >= 0. " + cNumericFieldNames( 6 ) + " = " + TrimSigDigits( rNumericArgs( 6 ), 2 ) );
						ErrorsFound = true;
					}

					if ( SameString( cAlphaArgs( 25 ), "Zone" ) ) {
						HPWaterHeater( HPWaterHeaterNum ).ParasiticTempIndicator = AmbientTempZone;
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempOutsideAir || HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempSchedule ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\"," );
							ShowContinueError( cAlphaFieldNames( 25 ) + " must be ZoneAirOnly or ZoneAndOutdoorAir" );
							ShowContinueError( " when parasitic heat rejection location equals Zone." );
							ErrorsFound = true;
						}
					} else if ( SameString( cAlphaArgs( 25 ), "Outdoors" ) ) {
						HPWaterHeater( HPWaterHeaterNum ).ParasiticTempIndicator = AmbientTempOutsideAir;
					} else {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
						ShowContinueError( " parasitic heat rejection location must be either Zone or Outdoors." );
						ErrorsFound = true;
					}

					//       get mixer/splitter nodes only when Inlet Air Configuration is ZoneAndOutdoorAir
					if ( ! lAlphaFieldBlanks( 26 ) ) {
						//         For the inlet air mixer node, NodeConnectionType is outlet from the HPWH inlet air node
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZoneAndOA ) {
							HPWaterHeater( HPWaterHeaterNum ).InletAirMixerNode = GetOnlySingleNode( cAlphaArgs( 26 ), ErrorsFound, "WaterHeater:HeatPump inlet air mixer", cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
						} else {
							ShowWarningError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
							ShowContinueError( "Inlet air mixer node name specified but only required when Inlet Air Configuration is selected as Zone and OutdoorAir. Node name disregarded and simulation continues." );
						}
					} else if ( lAlphaFieldBlanks( 26 ) && HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZoneAndOA ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
						ShowContinueError( "Inlet air mixer node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir." );
						ErrorsFound = true;
					}

					if ( ! lAlphaFieldBlanks( 27 ) ) {
						//         For the outlet air splitter node, NodeConnectionType is inlet to the HPWH outlet air node
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZoneAndOA ) {
							HPWaterHeater( HPWaterHeaterNum ).OutletAirSplitterNode = GetOnlySingleNode( cAlphaArgs( 27 ), ErrorsFound, cCurrentModuleObject + "-OUTLET AIR SPLITTER", cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						} else {
							ShowWarningError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
							ShowContinueError( "Outlet air splitter node name specified but only required when Inlet Air Configuration is selected as ZoneAndOutdoorAir. Node name disregarded and simulation continues." );
						}
					} else if ( lAlphaFieldBlanks( 27 ) && HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZoneAndOA ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
						ShowContinueError( "Outlet air splitter node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir." );
						ErrorsFound = true;
					}

					//       get node data for HPWH
					if ( HPWaterHeater( HPWaterHeaterNum ).InletAirMixerNode != 0 ) {
						//         when mixer/splitter nodes are used the HPWH's inlet/outlet node are set up as ObjectIsNotParent

						HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirInletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject + "-INLET AIR MIXER", cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

						HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirOutletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject + "-OUTLET AIR SPLITTER", cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

						HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsParent );
						if ( cAlphaArgs( 9 ) != "" ) {
							CheckAndAddAirNodeNumber( HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode, Okay );
							if ( ! Okay ) {
								ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\": Adding outdoor air node=" + cAlphaArgs( 9 ) );
							}
						}

						HPWaterHeater( HPWaterHeaterNum ).ExhaustAirNode = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsParent );

					} else {
						//         when mixer/splitter nodes are NOT used the HPWH's inlet/outlet nodes are set up as ObjectIsParent
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempSchedule ) {
							//           for scheduled HPWH's the inlet node is not on any branch or parent object, make it an outlet node
							//           to avoid node connection errors
							HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirInletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

							HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirOutletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

						} else { // HPWH is connected to a zone with no mixer/splitter nodes
							if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZone ) {
								HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirInletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

								HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirOutletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );
							} else { // HPWH is located outdoors
								HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsParent );
								if ( ! lAlphaFieldBlanks( 9 ) ) {
									CheckAndAddAirNodeNumber( HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode, Okay );
									if ( ! Okay ) {
										ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\": Adding outdoor air node =" + cAlphaArgs( 9 ) );
									}
								}

								HPWaterHeater( HPWaterHeaterNum ).ExhaustAirNode = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsParent );
							}
						}
					}

					//       check that the HPWH inlet and outlet nodes are in the same zone (ZoneHVAC:EquipmentConnections) when
					//       Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
					if ( ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZone || HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZoneAndOA ) && HPWaterHeater( HPWaterHeaterNum ).AmbientTempZone > 0 ) {
						if ( ! ZoneEquipInputsFilled ) {
							GetZoneEquipmentData();
							ZoneEquipInputsFilled = true;
						}
						if ( allocated( ZoneEquipConfig ) ) {
							FoundInletNode = false;
							FoundOutletNode = false;
							for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
								if ( HPWaterHeater( HPWaterHeaterNum ).AmbientTempZone == ZoneEquipConfig( ZoneNum ).ActualZoneNum ) break;
							}
							if ( ZoneNum <= NumOfZones ) {
								for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( ZoneNum ).NumInletNodes; ++SupAirIn ) {
									if ( HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirOutletNode != ZoneEquipConfig( ZoneNum ).InletNode( SupAirIn ) ) continue;
									FoundOutletNode = true;
								}
								for ( ExhAirOut = 1; ExhAirOut <= ZoneEquipConfig( ZoneNum ).NumExhaustNodes; ++ExhAirOut ) {
									if ( HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirInletNode != ZoneEquipConfig( ZoneNum ).ExhaustNode( ExhAirOut ) ) continue;
									FoundInletNode = true;
								}
								if ( ! FoundInletNode ) {
									ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
									ShowContinueError( "The HPWH's air inlet node name = " + cAlphaArgs( 7 ) + " was not properly specified " );
									ShowContinueError( "as an exhaust air node for zone = " + cAlphaArgs( 13 ) + " in a ZoneHVAC:EquipmentConnections object." );
									ErrorsFound = true;
								}
								if ( ! FoundOutletNode ) {
									ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
									ShowContinueError( "The HPWH's air outlet node name = " + cAlphaArgs( 8 ) + " was not properly specified " );
									ShowContinueError( "as an inlet air node for zone = " + cAlphaArgs( 13 ) + " in a ZoneHVAC:EquipmentConnections object." );
									ErrorsFound = true;
								}
							}
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\":" );
							ShowContinueError( "Heat pump water heater air inlet node name and air outlet node name must be listed in a ZoneHVAC:EquipmentConnections object when Inlet Air Configuration is equal to ZoneAirOnly or ZoneAndOutdoorAir." );
							ErrorsFound = true;
						}
					}

					//       only get the inlet air mixer schedule if the inlet air configuration is zone and outdoor air
					if ( ! lAlphaFieldBlanks( 28 ) && HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZoneAndOA ) {
						HPWaterHeater( HPWaterHeaterNum ).InletAirMixerSchPtr = GetScheduleIndex( cAlphaArgs( 28 ) );
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirMixerSchPtr == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
							ShowContinueError( cAlphaFieldNames( 28 ) + "=\"" + cAlphaArgs( 28 ) + "\"," );
							ErrorsFound = true;
						} else {
							//           check schedule values to be between 0 and 1
							ValidScheduleValue = CheckScheduleValueMinMax( HPWaterHeater( HPWaterHeaterNum ).InletAirMixerSchPtr, ">=", 0.0, "<=", 1.0 );
							if ( ! ValidScheduleValue ) {
								ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", not found" );
								ShowContinueError( cAlphaFieldNames( 28 ) + " values out of range of 0 to 1, Schedule=\"" + cAlphaArgs( 28 ) + "\"." );
								ErrorsFound = true;
							}
							//           set outlet air splitter schedule index equal to inlet air mixer schedule index
							//           (place holder for when zone pressurization/depressurization is allowed and different schedules can be used)
							HPWaterHeater( HPWaterHeaterNum ).OutletAirSplitterSchPtr = GetScheduleIndex( cAlphaArgs( 28 ) );
						}
					}

					//       set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
					if ( HPWaterHeater( HPWaterHeaterNum ).FanPlacement == DrawThru ) {
						if ( HPWaterHeater( HPWaterHeaterNum ).OutletAirSplitterNode != 0 ) {
							HPWaterHeater( HPWaterHeaterNum ).FanOutletNode = HPWaterHeater( HPWaterHeaterNum ).OutletAirSplitterNode;
						} else {
							if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempOutsideAir ) {
								HPWaterHeater( HPWaterHeaterNum ).FanOutletNode = HPWaterHeater( HPWaterHeaterNum ).ExhaustAirNode;
							} else {
								HPWaterHeater( HPWaterHeaterNum ).FanOutletNode = HPWaterHeater( HPWaterHeaterNum ).HeatPumpAirOutletNode;
							}
						}
					} else if ( HPWaterHeater( HPWaterHeaterNum ).FanPlacement == BlowThru ) {
						//           set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
						if ( bIsVScoil ) HPWaterHeater(HPWaterHeaterNum).FanOutletNode = GetCoilInletNodeVariableSpeed(HPWaterHeater(HPWaterHeaterNum).DXCoilType,
							HPWaterHeater(HPWaterHeaterNum).DXCoilName, DXCoilErrFlag);
						else HPWaterHeater(HPWaterHeaterNum).FanOutletNode = DXCoil(HPWaterHeater(HPWaterHeaterNum).DXCoilNum).AirInNode;

					}

					//       set the max mass flow rate for outdoor fans
					Node( HPWaterHeater( HPWaterHeaterNum ).FanOutletNode ).MassFlowRateMax = HPWaterHeater( HPWaterHeaterNum ).OperatingAirFlowRate * PsyRhoAirFnPbTdbW( OutBaroPress, 20.0, 0.0 );

					if ( HPWaterHeater( HPWaterHeaterNum ).FanPlacement == BlowThru ) {
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirMixerNode > 0 ) {
							//           cAlphaArgs(26) = Inlet Air Mixer Node
							FanInletNode = cAlphaArgs( 26 );
							FanOutletNode = "UNDEFINED";
						} else {
							if ( HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode == 0 ) {
								//             cAlphaArgs(7) = Heat Pump Air Inlet Node
								FanInletNode = cAlphaArgs( 7 );
								FanOutletNode = "UNDEFINED";
							} else {
								//             cAlphaArgs(9) = Outside Air Node
								FanInletNode = cAlphaArgs( 9 );
								FanOutletNode = "UNDEFINED";
							}
						}
						if ( HPWaterHeater( HPWaterHeaterNum ).OutletAirSplitterNode > 0 ) {
							//           cAlphaArgs(27) = Outlet Air Splitter Node
							CoilInletNode = "UNDEFINED";
							CoilOutletNode = cAlphaArgs( 27 );
						} else {
							if ( HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode == 0 ) {
								//             cAlphaArgs(8) = Heat Pump Air Outlet Node
								CoilInletNode = "UNDEFINED";
								CoilOutletNode = cAlphaArgs( 8 );
							} else {
								CoilInletNode = "UNDEFINED";
								//             cAlphaArgs(10) = Exhaust Air Node
								CoilOutletNode = cAlphaArgs( 10 );
							}
						}
					} else {
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirMixerNode > 0 ) {
							CoilInletNode = cAlphaArgs( 26 );
							CoilOutletNode = "UNDEFINED";
						} else {
							if ( HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode == 0 ) {
								CoilInletNode = cAlphaArgs( 7 );
								CoilOutletNode = "UNDEFINED";
							} else {
								CoilInletNode = cAlphaArgs( 9 );
								CoilOutletNode = "UNDEFINED";
							}
						}
						if ( HPWaterHeater( HPWaterHeaterNum ).OutletAirSplitterNode > 0 ) {
							FanInletNode = "UNDEFINED";
							FanOutletNode = cAlphaArgs( 27 );
						} else {
							if ( HPWaterHeater( HPWaterHeaterNum ).OutsideAirNode == 0 ) {
								FanInletNode = "UNDEFINED";
								FanOutletNode = cAlphaArgs( 8 );
							} else {
								FanInletNode = "UNDEFINED";
								FanOutletNode = cAlphaArgs( 10 );
							}
						}
					}

					//       set up comp set for air side nodes (can be blow thru or draw thru, may or may not have damper nodes)
					SetUpCompSets( HPWaterHeater( HPWaterHeaterNum ).Type, HPWaterHeater( HPWaterHeaterNum ).Name, HPWaterHeater( HPWaterHeaterNum ).DXCoilType, HPWaterHeater( HPWaterHeaterNum ).DXCoilName, CoilInletNode, CoilOutletNode );

					SetUpCompSets( HPWaterHeater( HPWaterHeaterNum ).Type, HPWaterHeater( HPWaterHeaterNum ).Name, HPWaterHeater( HPWaterHeaterNum ).FanType, HPWaterHeater( HPWaterHeaterNum ).FanName, FanInletNode, FanOutletNode );

					if ( ! lAlphaFieldBlanks( 29 ) ) {
						{ auto const SELECT_CASE_var( cAlphaArgs( 29 ) );
						if ( SELECT_CASE_var == "HEATER1" ) {
							HPWaterHeater( HPWaterHeaterNum ).ControlSensorLocation = Heater1HPWHControl;
						} else if ( SELECT_CASE_var == "HEATER2" ) {
							HPWaterHeater( HPWaterHeaterNum ).ControlSensorLocation = Heater2HPWHControl;
						} else if ( SELECT_CASE_var == "SOURCEINLET" ) {
							HPWaterHeater( HPWaterHeaterNum ).ControlSensorLocation = SourceInletHPWHControl;
						} else if ( SELECT_CASE_var == "SOURCEOUTLET" ) {
							HPWaterHeater( HPWaterHeaterNum ).ControlSensorLocation = SourceOutletHPWHControl;
						} else if ( SELECT_CASE_var == "USEINLET" ) {
							HPWaterHeater( HPWaterHeaterNum ).ControlSensorLocation = UseInletHPWHControl;
						} else if ( SELECT_CASE_var == "USEOUTLET" ) {
							HPWaterHeater( HPWaterHeaterNum ).ControlSensorLocation = UseOutletHPWHControl;
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + HPWaterHeater( HPWaterHeaterNum ).Name + "\", invalid " );
							ShowContinueError( cAlphaFieldNames( 29 ) + "=\"" + cAlphaArgs( 29 ) + "\"." );
							ErrorsFound = true;
						}}

					}

				} // DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			} //IF (NumHeatPumpWaterHeater > 0) THEN

			//!!=======   Get WATER HEATER:MIXED ===================================================================================
			if ( NumWaterHeaterMixed > 0 ) {
				cCurrentModuleObject = cMixedWHModuleObj;
				for ( WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterHeaterMixed; ++WaterThermalTankNum ) {

					GetObjectItem( cCurrentModuleObject, WaterThermalTankNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), WaterThermalTank.Name(), WaterThermalTankNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					WaterThermalTank( WaterThermalTankNum ).Name = cAlphaArgs( 1 );
					WaterThermalTank( WaterThermalTankNum ).Type = cCurrentModuleObject;
					WaterThermalTank( WaterThermalTankNum ).TypeNum = MixedWaterHeater;

					// default to always on
					WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = ScheduleAlwaysOn;
					WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = ScheduleAlwaysOn;

					// A user field will be added in a later release
					WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName = "Water Heater";

					WaterThermalTank( WaterThermalTankNum ).Volume = rNumericArgs( 1 );
					if ( WaterThermalTank( WaterThermalTankNum ).Volume == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized = true;
					}
					if ( rNumericArgs( 1 ) == 0.0 ) {
						// Set volume to a really small number to simulate a tankless/instantaneous water heater
						WaterThermalTank( WaterThermalTankNum ).Volume = 0.000001; // = 1 cm3
					}

					WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( lAlphaFieldBlanks( 2 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", missing data." );
						ShowContinueError( "blank field, missing " + cAlphaFieldNames( 2 ) + " is required" );
						ErrorsFound = true;
					} else if ( WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  " + cAlphaFieldNames( 2 ) + " not found = " + cAlphaArgs( 2 ) );
						ErrorsFound = true;
					}

					if ( rNumericArgs( 2 ) > 0.0001 ) {
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = rNumericArgs( 2 );
					} else {
						// Default to very small number (however it can't be TINY or it will break the algorithm)
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = 0.5;
					}

					if ( rNumericArgs( 3 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = rNumericArgs( 3 );
					} else {
						// Default to very large number
						// BG comment why a large number here why not boilng point of water?
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = 100.0; //1.0E9
					}

					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = rNumericArgs( 4 );
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacity == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized = true;
					}

					if ( ( rNumericArgs( 5 ) > WaterThermalTank( WaterThermalTankNum ).MaxCapacity ) && ( ! WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Heater Minimum Capacity cannot be greater than Heater Maximum Capacity" );
						ErrorsFound = true;
					} else {
						WaterThermalTank( WaterThermalTankNum ).MinCapacity = rNumericArgs( 5 );
					}

					// Validate Heater Control Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );
					if ( SELECT_CASE_var == "CYCLE" ) {
						WaterThermalTank( WaterThermalTankNum ).ControlType = ControlTypeCycle;
						WaterThermalTank( WaterThermalTankNum ).MinCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity;

					} else if ( SELECT_CASE_var == "MODULATE" ) {
						WaterThermalTank( WaterThermalTankNum ).ControlType = ControlTypeModulate;

						//CASE ('MODULATE WITH OVERHEAT')  ! Not yet implemented

						//CASE ('MODULATE WITH UNDERHEAT')  ! Not yet implemented

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Control Type entered=" + cAlphaArgs( 3 ) );
						ErrorsFound = true;
					}}
					WaterThermalTank( WaterThermalTankNum ).VolFlowRateMin = rNumericArgs( 6 );
					WaterThermalTank( WaterThermalTankNum ).VolFlowRateMin = max( 0.0, WaterThermalTank( WaterThermalTankNum ).VolFlowRateMin );
					//        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
					//        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMin = rNumericArgs(6) * rho   ! Not yet implemented
					WaterThermalTank( WaterThermalTankNum ).IgnitionDelay = rNumericArgs( 7 ); // Not yet implemented

					// Validate Heater Fuel Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) );
					if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) || ( SELECT_CASE_var == "ELEC" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Electric";

					} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Gas";

					} else if ( SELECT_CASE_var == "DIESEL" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Diesel";

					} else if ( SELECT_CASE_var == "GASOLINE" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Gasoline";

					} else if ( SELECT_CASE_var == "COAL" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Coal";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "FuelOil#1";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "FuelOil#2";

					} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Propane";

					} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "OtherFuel1";

					} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "OtherFuel2";

					} else if ( SELECT_CASE_var == "STEAM" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Steam";

					} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "DistrictHeating";

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Heater Fuel Type entered=" + cAlphaArgs( 4 ) );
						// Set to Electric to avoid errors when setting up output variables
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Electric";
						ErrorsFound = true;
					}}

					if ( rNumericArgs( 8 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).Efficiency = rNumericArgs( 8 );
					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Heater Thermal Efficiency must be greater than zero" );
						ErrorsFound = true;
					}

					if ( ! cAlphaArgs( 5 ).empty() ) {
						WaterThermalTank( WaterThermalTankNum ).PLFCurve = GetCurveIndex( cAlphaArgs( 5 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).PLFCurve == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Part Load Factor curve not found = " + cAlphaArgs( 5 ) );
							ErrorsFound = true;
						} else {
							ValidatePLFCurve( WaterThermalTank( WaterThermalTankNum ).PLFCurve, IsValid );

							if ( ! IsValid ) {
								ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Part Load Factor curve failed to evaluate to greater than zero for all numbers in the domain of 0 to 1" );
								ErrorsFound = true;
							}
						}
					}

					WaterThermalTank( WaterThermalTankNum ).OffCycParaLoad = rNumericArgs( 9 );

					// Validate Off-Cycle Parasitic Fuel Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 6 ) );
					if ( SELECT_CASE_var == "" ) { // If blank, default to Fuel Type for heater
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = WaterThermalTank( WaterThermalTankNum ).FuelType;

					} else if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) || ( SELECT_CASE_var == "ELEC" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Electric";

					} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Gas";

					} else if ( SELECT_CASE_var == "DIESEL" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Diesel";

					} else if ( SELECT_CASE_var == "GASOLINE" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Gasoline";

					} else if ( SELECT_CASE_var == "COAL" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Coal";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "FuelOil#1";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "FuelOil#2";

					} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Propane";

					} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "OtherFuel1";

					} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "OtherFuel2";

					} else if ( SELECT_CASE_var == "STEAM" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Steam";

					} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "DistrictHeating";

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + cAlphaArgs( 6 ) );
						// Set to Electric to avoid errors when setting up output variables
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Electric";
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).OffCycParaFracToTank = rNumericArgs( 10 );

					WaterThermalTank( WaterThermalTankNum ).OnCycParaLoad = rNumericArgs( 11 );

					// Validate On-Cycle Parasitic Fuel Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 7 ) );
					if ( SELECT_CASE_var == "" ) { // If blank, default to Fuel Type for heater
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = WaterThermalTank( WaterThermalTankNum ).FuelType;

					} else if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) || ( SELECT_CASE_var == "ELEC" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Electric";

					} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Gas";

					} else if ( SELECT_CASE_var == "DIESEL" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Diesel";

					} else if ( SELECT_CASE_var == "GASOLINE" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Gasoline";

					} else if ( SELECT_CASE_var == "COAL" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Coal";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "FuelOil#1";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "FuelOil#2";

					} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Propane";

					} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "OtherFuel1";

					} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "OtherFuel2";

					} else if ( SELECT_CASE_var == "STEAM" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Steam";

					} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "DistrictHeating";

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid On-Cycle Parasitic Fuel Type entered=" + cAlphaArgs( 7 ) );
						// Set to Electric to avoid errors when setting up output variables
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Electric";
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).OnCycParaFracToTank = rNumericArgs( 12 );

					{ auto const SELECT_CASE_var( cAlphaArgs( 8 ) );
					if ( SELECT_CASE_var == "SCHEDULE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempSchedule;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule = GetScheduleIndex( cAlphaArgs( 9 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Temperature Schedule not found = " + cAlphaArgs( 9 ) );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "ZONE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempZone;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempZone = FindItemInList( cAlphaArgs( 10 ), Zone.Name(), NumOfZones );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Temperature Zone not found = " + cAlphaArgs( 10 ) );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "OUTDOORS" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempOutsideAir;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode = GetOnlySingleNode( cAlphaArgs( 11 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
						if ( cAlphaArgs( 11 ) != "" ) {
							if ( ! CheckOutAirNodeNumber( WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode ) ) {
								ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node" );
								ShowContinueError( "...Referenced Node Name=" + cAlphaArgs( 11 ) );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors." );
							ErrorsFound = true;
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs( 8 ) );
						ShowContinueError( " Valid entries are SCHEDULE, ZONE, and OUTDOORS." );
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).OffCycLossCoeff = rNumericArgs( 13 );
					WaterThermalTank( WaterThermalTankNum ).OffCycLossFracToZone = rNumericArgs( 14 );

					WaterThermalTank( WaterThermalTankNum ).OnCycLossCoeff = rNumericArgs( 15 );
					WaterThermalTank( WaterThermalTankNum ).OnCycLossFracToZone = rNumericArgs( 16 );
					rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineNameNoColon );
					WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax = rNumericArgs( 17 ) * rho;

					if ( ( cAlphaArgs( 14 ).empty() ) && ( cAlphaArgs( 15 ).empty() ) ) {
						if ( ! cAlphaArgs( 12 ).empty() ) {
							WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule = GetScheduleIndex( cAlphaArgs( 12 ) );
							if ( WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule == 0 ) {
								ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Flow Rate Schedule not found = " + cAlphaArgs( 12 ) );
								ErrorsFound = true;
							}
						}
					}

					if ( ! cAlphaArgs( 13 ).empty() ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule = GetScheduleIndex( cAlphaArgs( 13 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Cold Water Supply Temperature Schedule not found = " + cAlphaArgs( 13 ) );
							ErrorsFound = true;
						}
					}

					if ( NumNums > 17 ) {
						if ( ( rNumericArgs( 18 ) > 1 ) || ( rNumericArgs( 18 ) < 0 ) ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use Side Effectiveness is out of bounds (0 to 1)" );
							ErrorsFound = true;
						}
						WaterThermalTank( WaterThermalTankNum ).UseEffectiveness = rNumericArgs( 18 );
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseEffectiveness = 1.0; // Default for stand-alone mode
					}

					if ( ( rNumericArgs( 19 ) > 1 ) || ( rNumericArgs( 19 ) < 0 ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Source Side Effectiveness is out of bounds (0 to 1)" );
						ErrorsFound = true;
					}
					WaterThermalTank( WaterThermalTankNum ).SourceEffectiveness = rNumericArgs( 19 );

					// If no plant nodes are connected, simulate in stand-alone mode.
					if ( cAlphaArgs( 14 ).empty() && cAlphaArgs( 15 ).empty() && cAlphaArgs( 16 ).empty() && cAlphaArgs( 17 ).empty() ) {
						WaterThermalTank( WaterThermalTankNum ).StandAlone = true;
					}

					if ( ! lNumericFieldBlanks( 20 ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = rNumericArgs( 20 );
						if ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate == AutoSize ) {
							WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized = true;
						}
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = 0.0;
					}
					WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide = DemandSupply_No;

					if ( ! lNumericFieldBlanks( 21 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = rNumericArgs( 21 );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate == AutoSize ) {
							WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized = true;
						}
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = 0.0;
					}
					WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide = DemandSupply_No;

					if ( ! lNumericFieldBlanks( 22 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime = rNumericArgs( 22 );
					} else {
						WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime = 1.5;
					}

					if ( ( ! cAlphaArgs( 14 ).empty() ) || ( ! cAlphaArgs( 15 ).empty() ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletNode = GetOnlySingleNode( cAlphaArgs( 14 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName1 = cAlphaArgs( 14 );
						WaterThermalTank( WaterThermalTankNum ).UseOutletNode = GetOnlySingleNode( cAlphaArgs( 15 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName1 = cAlphaArgs( 15 );

						if ( rNumericArgs( 17 ) > 0 ) {
							ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used" );
						}

						if ( WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule > 0 ) {
							ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used" );
						}

						if ( WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule > 0 ) {
							ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used" );
						}
					}

					if ( ( ! cAlphaArgs( 16 ).empty() ) || ( ! cAlphaArgs( 17 ).empty() ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceInletNode = GetOnlySingleNode( cAlphaArgs( 16 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName2 = cAlphaArgs( 16 );
						WaterThermalTank( WaterThermalTankNum ).SourceOutletNode = GetOnlySingleNode( cAlphaArgs( 17 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName2 = cAlphaArgs( 17 );

					}

					if ( ! lAlphaFieldBlanks( 18 ) ) {
						{ auto const SELECT_CASE_var( cAlphaArgs( 18 ) );
						if ( SELECT_CASE_var == "STORAGETANK" ) {
							WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideStorageTank;
						} else if ( SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT" ) {
							WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
						} else if ( SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT" ) {
							WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideIndirectHeatAltSetpoint;
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Control Mode entered=" + cAlphaArgs( 18 ) );
							ErrorsFound = true;
						}}
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
					}

					if ( ! lAlphaFieldBlanks( 19 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceSideAltSetpointSchedNum = GetScheduleIndex( cAlphaArgs( 19 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceSideAltSetpointSchedNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  " + cAlphaFieldNames( 19 ) + " not found = " + cAlphaArgs( 19 ) );
							ErrorsFound = true;
						}
					}

				} // WaterThermalTankNum

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			}

			//!!=======   Get WATER HEATER:STRATIFIED ==============================================================================
			if ( NumWaterHeaterStratified > 0 ) {
				cCurrentModuleObject = cStratifiedWHModuleObj; //'WaterHeater:Stratified'

				for ( WaterThermalTankNum = NumWaterHeaterMixed + 1; WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified; ++WaterThermalTankNum ) {

					GetObjectItem( cCurrentModuleObject, WaterThermalTankNum - NumWaterHeaterMixed, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), WaterThermalTank.Name(), WaterThermalTankNum - NumWaterHeaterMixed - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					WaterThermalTank( WaterThermalTankNum ).Name = cAlphaArgs( 1 );
					WaterThermalTank( WaterThermalTankNum ).Type = cCurrentModuleObject;
					WaterThermalTank( WaterThermalTankNum ).TypeNum = StratifiedWaterHeater;

					// default to always on
					WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = ScheduleAlwaysOn;
					WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = ScheduleAlwaysOn;

					WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName = cAlphaArgs( 2 );

					WaterThermalTank( WaterThermalTankNum ).Volume = rNumericArgs( 1 );
					if ( WaterThermalTank( WaterThermalTankNum ).Volume == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized = true;
					}
					rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineNameNoColon );
					WaterThermalTank( WaterThermalTankNum ).Mass = WaterThermalTank( WaterThermalTankNum ).Volume * rho;
					WaterThermalTank( WaterThermalTankNum ).Height = rNumericArgs( 2 );
					if ( WaterThermalTank( WaterThermalTankNum ).Height == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized = true;
					}

					{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );
					if ( SELECT_CASE_var == "VERTICALCYLINDER" ) {
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeVertCylinder;

					} else if ( SELECT_CASE_var == "HORIZONTALCYLINDER" ) {
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeHorizCylinder;

					} else if ( SELECT_CASE_var == "OTHER" ) {
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeOther;
						if ( rNumericArgs( 3 ) > 0.0 ) {
							WaterThermalTank( WaterThermalTankNum ).Perimeter = rNumericArgs( 3 );
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER" );
							ErrorsFound = true;
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Tank Shape entered=" + cAlphaArgs( 3 ) );
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeVertCylinder;
						ErrorsFound = true;
					}}

					if ( rNumericArgs( 4 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = rNumericArgs( 4 );
					} else {
						// Default to very large number
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = 1.0e9;
					}

					// Validate Heater Priority Control
					{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) );
					if ( SELECT_CASE_var == "MASTERSLAVE" ) {
						WaterThermalTank( WaterThermalTankNum ).ControlType = PriorityMasterSlave;

					} else if ( SELECT_CASE_var == "SIMULTANEOUS" ) {
						WaterThermalTank( WaterThermalTankNum ).ControlType = PrioritySimultaneous;

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Heater Priority Control entered=" + cAlphaArgs( 4 ) );
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule = GetScheduleIndex( cAlphaArgs( 5 ) );
					if ( lAlphaFieldBlanks( 5 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", missing data." );
						ShowContinueError( "blank field, missing " + cAlphaFieldNames( 5 ) + " is required" );
						ErrorsFound = true;
					} else if ( WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": " + cAlphaFieldNames( 5 ) + " not found = " + cAlphaArgs( 5 ) );
						ErrorsFound = true;
					}

					if ( rNumericArgs( 5 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = rNumericArgs( 5 );
					} else {
						// Default to very small number (however it can't be TINY or it will break the algorithm)
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = 0.0001;
					}

					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = rNumericArgs( 6 );
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacity == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized = true;
					}

					WaterThermalTank( WaterThermalTankNum ).HeaterHeight1 = rNumericArgs( 7 );

					//Test if Heater height is within range
					if ( ( ! WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).HeaterHeight1 > WaterThermalTank( WaterThermalTankNum ).Height ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Heater 1 is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 7 ) + " = " + RoundSigDigits( rNumericArgs( 7 ), 4 ) );
						ErrorsFound = true;
					}

					WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule2 = GetScheduleIndex( cAlphaArgs( 6 ) );
					if ( lAlphaFieldBlanks( 6 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", missing data." );
						ShowContinueError( "blank field, missing " + cAlphaFieldNames( 6 ) + " is required" );
						ErrorsFound = true;
					} else if ( WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule2 == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  " + cAlphaFieldNames( 6 ) + " not found = " + cAlphaArgs( 6 ) );
						ErrorsFound = true;
					}

					if ( rNumericArgs( 5 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp2 = rNumericArgs( 8 );
					} else {
						// Default to very small number (however it can't be TINY or it will break the algorithm)
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp2 = 0.0001;
					}

					WaterThermalTank( WaterThermalTankNum ).MaxCapacity2 = rNumericArgs( 9 );
					WaterThermalTank( WaterThermalTankNum ).HeaterHeight2 = rNumericArgs( 10 );

					//Test if Heater height is within range
					if ( ( ! WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).HeaterHeight2 > WaterThermalTank( WaterThermalTankNum ).Height ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Heater 2 is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 10 ) + " = " + RoundSigDigits( rNumericArgs( 10 ), 4 ) );
						ErrorsFound = true;
					}

					// Validate Heater Fuel Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 7 ) );
					if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) || ( SELECT_CASE_var == "ELEC" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Electric";

					} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Gas";

					} else if ( SELECT_CASE_var == "DIESEL" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Diesel";

					} else if ( SELECT_CASE_var == "GASOLINE" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Gasoline";

					} else if ( SELECT_CASE_var == "COAL" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Coal";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "FuelOil#1";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "FuelOil#2";

					} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Propane";

					} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "OtherFuel1";

					} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "OtherFuel2";

					} else if ( SELECT_CASE_var == "STEAM" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Steam";

					} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
						WaterThermalTank( WaterThermalTankNum ).FuelType = "DistrictHeating";

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Heater Fuel Type entered=" + cAlphaArgs( 7 ) );
						// Set to Electric to avoid errors when setting up output variables
						WaterThermalTank( WaterThermalTankNum ).FuelType = "Electric";
						ErrorsFound = true;
					}}

					if ( rNumericArgs( 11 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).Efficiency = rNumericArgs( 11 );
					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Heater Thermal Efficiency must be greater than zero" );
						ErrorsFound = true;
					}

					WaterThermalTank( WaterThermalTankNum ).OffCycParaLoad = rNumericArgs( 12 );

					// Validate Off-Cycle Parasitic Fuel Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 8 ) );
					if ( SELECT_CASE_var == "" ) { // If blank, default to Fuel Type for heater
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = WaterThermalTank( WaterThermalTankNum ).FuelType;

					} else if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) || ( SELECT_CASE_var == "ELEC" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Electric";

					} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Gas";

					} else if ( SELECT_CASE_var == "DIESEL" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Diesel";

					} else if ( SELECT_CASE_var == "GASOLINE" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Gasoline";

					} else if ( SELECT_CASE_var == "COAL" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Coal";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "FuelOil#1";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "FuelOil#2";

					} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Propane";

					} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "OtherFuel1";

					} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "OtherFuel2";

					} else if ( SELECT_CASE_var == "STEAM" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Steam";

					} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "DistrictHeating";

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + cAlphaArgs( 8 ) );
						// Set to Electric to avoid errors when setting up output variables
						WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Electric";
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).OffCycParaFracToTank = rNumericArgs( 13 );
					WaterThermalTank( WaterThermalTankNum ).OffCycParaHeight = rNumericArgs( 14 );

					WaterThermalTank( WaterThermalTankNum ).OnCycParaLoad = rNumericArgs( 15 );

					// Validate On-Cycle Parasitic Fuel Type
					{ auto const SELECT_CASE_var( cAlphaArgs( 9 ) );
					if ( SELECT_CASE_var == "" ) { // If blank, default to Fuel Type for heater
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = WaterThermalTank( WaterThermalTankNum ).FuelType;

					} else if ( ( SELECT_CASE_var == "ELECTRICITY" ) || ( SELECT_CASE_var == "ELECTRIC" ) || ( SELECT_CASE_var == "ELEC" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Electric";

					} else if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Gas";

					} else if ( SELECT_CASE_var == "DIESEL" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Diesel";

					} else if ( SELECT_CASE_var == "GASOLINE" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Gasoline";

					} else if ( SELECT_CASE_var == "COAL" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Coal";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "FuelOil#1";

					} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "FuelOil#2";

					} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Propane";

					} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "OtherFuel1";

					} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "OtherFuel2";

					} else if ( SELECT_CASE_var == "STEAM" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Steam";

					} else if ( SELECT_CASE_var == "DISTRICTHEATING" ) {
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "DistrictHeating";

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid On-Cycle Parasitic Fuel Type entered=" + cAlphaArgs( 9 ) );
						// Set to Electric to avoid errors when setting up output variables
						WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Electric";
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).OnCycParaFracToTank = rNumericArgs( 16 );
					WaterThermalTank( WaterThermalTankNum ).OnCycParaHeight = rNumericArgs( 17 );

					{ auto const SELECT_CASE_var( cAlphaArgs( 10 ) );
					if ( SELECT_CASE_var == "SCHEDULE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempSchedule;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule = GetScheduleIndex( cAlphaArgs( 11 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Temperature Schedule not found = " + cAlphaArgs( 11 ) );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "ZONE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempZone;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempZone = FindItemInList( cAlphaArgs( 12 ), Zone.Name(), NumOfZones );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Temperature Zone not found = " + cAlphaArgs( 12 ) );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "OUTDOORS" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempOutsideAir;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode = GetOnlySingleNode( cAlphaArgs( 13 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						if ( cAlphaArgs( 13 ) != "" ) {
							if ( ! CheckOutAirNodeNumber( WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode ) ) {
								ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node" );
								ShowContinueError( "...Referenced Node Name=" + cAlphaArgs( 13 ) );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors." );
							ErrorsFound = true;
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs( 10 ) );
						ShowContinueError( " Valid entries are Schedule, Zone, and Outdoors." );
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).SkinLossCoeff = rNumericArgs( 18 );
					WaterThermalTank( WaterThermalTankNum ).SkinLossFracToZone = rNumericArgs( 19 );
					WaterThermalTank( WaterThermalTankNum ).OffCycFlueLossCoeff = rNumericArgs( 20 );
					WaterThermalTank( WaterThermalTankNum ).OffCycFlueLossFracToZone = rNumericArgs( 21 );

					//this is temporary until we know fluid type
					rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineNameNoColon );
					WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax = rNumericArgs( 22 ) * rho;

					if ( ( cAlphaArgs( 16 ).empty() ) && ( cAlphaArgs( 17 ).empty() ) ) {
						if ( ! cAlphaArgs( 14 ).empty() ) {
							WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule = GetScheduleIndex( cAlphaArgs( 14 ) );
							if ( WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule == 0 ) {
								ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Flow Rate Schedule not found = " + cAlphaArgs( 14 ) );
								ErrorsFound = true;
							}
						}
					}

					if ( ! cAlphaArgs( 15 ).empty() ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule = GetScheduleIndex( cAlphaArgs( 15 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Cold Water Supply Temperature Schedule not found = " + cAlphaArgs( 15 ) );
							ErrorsFound = true;
						}
					}

					if ( NumNums > 22 ) {
						WaterThermalTank( WaterThermalTankNum ).UseEffectiveness = rNumericArgs( 23 );
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseEffectiveness = 1.0; // Default for stand-alone mode
					}

					if ( NumNums > 23 ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletHeight = rNumericArgs( 24 );
					} else {
						// Defaults to bottom of tank
						WaterThermalTank( WaterThermalTankNum ).UseInletHeight = 0.0;
					}
					if ( ( ! WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).UseInletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Use inlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 24 ) + " = " + RoundSigDigits( rNumericArgs( 24 ), 4 ) );
						ErrorsFound = true;
					}

					if ( ( NumNums > 24 ) && ( rNumericArgs( 25 ) != AutoCalculate ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseOutletHeight = rNumericArgs( 25 );
					} else {
						// Defaults to top of tank
						WaterThermalTank( WaterThermalTankNum ).UseOutletHeight = WaterThermalTank( WaterThermalTankNum ).Height;
					}
					if ( WaterThermalTank( WaterThermalTankNum ).UseOutletHeight == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).UseOutletHeightWasAutoSized = true;
					}
					if ( ( ! WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).UseOutletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Use outlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 25 ) + " = " + RoundSigDigits( rNumericArgs( 25 ), 4 ) );
						ErrorsFound = true;
					}

					if ( NumNums > 25 ) {
						WaterThermalTank( WaterThermalTankNum ).SourceEffectiveness = rNumericArgs( 26 );
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceEffectiveness = 1.0;
					}

					if ( ( NumNums > 26 ) && ( rNumericArgs( 27 ) != AutoCalculate ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceInletHeight = rNumericArgs( 27 );
					} else {
						// Defaults to top of tank
						WaterThermalTank( WaterThermalTankNum ).SourceInletHeight = WaterThermalTank( WaterThermalTankNum ).Height;
					}
					if ( WaterThermalTank( WaterThermalTankNum ).SourceInletHeight == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).SourceInletHeightWasAutoSized = true;
					}
					if ( ( ! WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).SourceInletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Source inlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 27 ) + " = " + RoundSigDigits( rNumericArgs( 27 ), 4 ) );
						ErrorsFound = true;
					}

					if ( ( NumNums > 27 ) && ( rNumericArgs( 28 ) != AutoCalculate ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight = rNumericArgs( 28 );
					} else {
						// Defaults to bottom of tank
						WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight = 0.0;
					}
					if ( ( ! WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Source outlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 28 ) + " = " + RoundSigDigits( rNumericArgs( 28 ), 4 ) );
						ErrorsFound = true;
					}

					// If no plant nodes are connected, simulate in stand-alone mode.
					if ( cAlphaArgs( 16 ).empty() && cAlphaArgs( 17 ).empty() && cAlphaArgs( 18 ).empty() && cAlphaArgs( 19 ).empty() ) WaterThermalTank( WaterThermalTankNum ).StandAlone = true;

					if ( ! lNumericFieldBlanks( 29 ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = rNumericArgs( 29 );
						if ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate == AutoSize ) {
							WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized = true;
						}
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = 0.0;
					}

					WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide = DemandSupply_No;

					if ( ! lNumericFieldBlanks( 30 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = rNumericArgs( 30 );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate == AutoSize ) {
							WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized = true;
						}
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = 0.0;
					}

					if ( NumNums > 30 ) {
						WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime = rNumericArgs( 31 );
					} else {
						WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime = 1.5;
					}

					WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide = DemandSupply_No;

					if ( ( ! cAlphaArgs( 16 ).empty() ) || ( ! cAlphaArgs( 17 ).empty() ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletNode = GetOnlySingleNode( cAlphaArgs( 16 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName1 = cAlphaArgs( 16 );
						WaterThermalTank( WaterThermalTankNum ).UseOutletNode = GetOnlySingleNode( cAlphaArgs( 17 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName1 = cAlphaArgs( 17 );

						if ( rNumericArgs( 22 ) > 0 ) {
							ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used" );
						}

						if ( WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule > 0 ) {
							ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used" );
						}

						if ( WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule > 0 ) {
							ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used" );
						}
					}

					if ( ( ! cAlphaArgs( 18 ).empty() ) || ( ! cAlphaArgs( 19 ).empty() ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceInletNode = GetOnlySingleNode( cAlphaArgs( 18 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName2 = cAlphaArgs( 18 );
						WaterThermalTank( WaterThermalTankNum ).SourceOutletNode = GetOnlySingleNode( cAlphaArgs( 19 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName2 = cAlphaArgs( 19 );

					}

					// Validate inlet mode
					{ auto const SELECT_CASE_var( cAlphaArgs( 20 ) );
					if ( SELECT_CASE_var == "FIXED" ) {
						WaterThermalTank( WaterThermalTankNum ).InletMode = InletModeFixed;

					} else if ( SELECT_CASE_var == "SEEKING" ) {
						WaterThermalTank( WaterThermalTankNum ).InletMode = InletModeSeeking;
					}}

					WaterThermalTank( WaterThermalTankNum ).Nodes = rNumericArgs( 32 );
					WaterThermalTank( WaterThermalTankNum ).AdditionalCond = rNumericArgs( 33 );

					WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff.allocate( WaterThermalTank( WaterThermalTankNum ).Nodes );
					WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff = 0.0;
					for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
						if ( NumNums > 32 + NodeNum ) {
							WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff( NodeNum ) = rNumericArgs( 33 + NodeNum );
						} else {
							break;
						}
					}

					if ( NumNums > 33 + WaterThermalTank( WaterThermalTankNum ).Nodes ) {
						ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used" );
					}

					SetupStratifiedNodes( WaterThermalTankNum );

					if ( ! lAlphaFieldBlanks( 21 ) ) {
						{ auto const SELECT_CASE_var( cAlphaArgs( 21 ) );
						if ( SELECT_CASE_var == "STORAGETANK" ) {
							WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideStorageTank;
						} else if ( SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT" ) {
							WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
						} else if ( SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT" ) {
							WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideIndirectHeatAltSetpoint;
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Control Mode entered=" + cAlphaArgs( 21 ) );
							ErrorsFound = true;
						}}
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
					}

					if ( ! lAlphaFieldBlanks( 22 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceSideAltSetpointSchedNum = GetScheduleIndex( cAlphaArgs( 22 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceSideAltSetpointSchedNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  " + cAlphaFieldNames( 22 ) + " not found = " + cAlphaArgs( 22 ) );
							ErrorsFound = true;
						}
					}

				} // WaterThermalTankNum

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			}

			//!!=======   Get Chilled Water :MIXED ===================================================================================
			if ( NumChilledWaterMixed > 0 ) {
				cCurrentModuleObject = cMixedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Mixed'
				for ( WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + 1; WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed; ++WaterThermalTankNum ) {

					GetObjectItem( cCurrentModuleObject, WaterThermalTankNum - ( NumWaterHeaterMixed + NumWaterHeaterStratified ), cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), WaterThermalTank.Name(), WaterThermalTankNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					WaterThermalTank( WaterThermalTankNum ).Name = cAlphaArgs( 1 );
					WaterThermalTank( WaterThermalTankNum ).Type = cCurrentModuleObject;
					WaterThermalTank( WaterThermalTankNum ).TypeNum = MixedChilledWaterStorage;
					WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank = true;
					WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName = "Chilled Water Storage";

					WaterThermalTank( WaterThermalTankNum ).Volume = rNumericArgs( 1 );
					if ( WaterThermalTank( WaterThermalTankNum ).Volume == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized = true;
					}
					if ( rNumericArgs( 1 ) == 0.0 ) {
						// Set volume to a really small number to continue simulation
						WaterThermalTank( WaterThermalTankNum ).Volume = 0.000001; // = 1 cm3
					}

					WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );

						ErrorsFound = true;
					}

					if ( rNumericArgs( 2 ) > 0.0001 ) {
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = rNumericArgs( 2 );
					} else {
						// Default to very small number (however it can't be TINY or it will break the algorithm)
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = 0.5;
					}

					if ( rNumericArgs( 3 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = rNumericArgs( 3 );
					} else {
						// default to just above freezing
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = 1.0;
					}

					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = rNumericArgs( 4 );
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacity == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized = true;
					}

					WaterThermalTank( WaterThermalTankNum ).MinCapacity = 0.0;
					WaterThermalTank( WaterThermalTankNum ).ControlType = ControlTypeCycle;

					WaterThermalTank( WaterThermalTankNum ).MassFlowRateMin = 0.0;
					WaterThermalTank( WaterThermalTankNum ).IgnitionDelay = 0.0;
					WaterThermalTank( WaterThermalTankNum ).FuelType = "Electric";
					WaterThermalTank( WaterThermalTankNum ).Efficiency = 1.0;
					WaterThermalTank( WaterThermalTankNum ).PLFCurve = 0;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaLoad = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Electric";
					WaterThermalTank( WaterThermalTankNum ).OffCycParaFracToTank = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaLoad = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Electric";
					WaterThermalTank( WaterThermalTankNum ).OnCycParaFracToTank = 0.0;

					{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );
					if ( SELECT_CASE_var == "SCHEDULE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempSchedule;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule = GetScheduleIndex( cAlphaArgs( 4 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + cAlphaArgs( 4 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Schedule was not found." );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "ZONE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempZone;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempZone = FindItemInList( cAlphaArgs( 5 ), Zone.Name(), NumOfZones );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Zone was not found." );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "OUTDOORS" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempOutsideAir;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
						if ( ! lAlphaFieldBlanks( 6 ) ) {
							if ( ! CheckOutAirNodeNumber( WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode ) ) {
								ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
								ShowContinueError( "Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node" );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors." );
							ErrorsFound = true;
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs( 3 ) );
						ShowContinueError( " Valid entries are Schedule, Zone, and Outdoors." );
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).OffCycLossCoeff = rNumericArgs( 5 );
					WaterThermalTank( WaterThermalTankNum ).OffCycLossFracToZone = 1.0;

					WaterThermalTank( WaterThermalTankNum ).OnCycLossCoeff = rNumericArgs( 5 );
					WaterThermalTank( WaterThermalTankNum ).OnCycLossFracToZone = 1.0;

					WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax = 0.0;
					WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule = 0;
					WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule = 0;

					// default to always on
					WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = ScheduleAlwaysOn;
					WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = ScheduleAlwaysOn;

					if ( ( rNumericArgs( 6 ) > 1 ) || ( rNumericArgs( 6 ) < 0 ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Use Side Effectiveness is out of bounds (0 to 1)" );
						ErrorsFound = true;
					}
					WaterThermalTank( WaterThermalTankNum ).UseEffectiveness = rNumericArgs( 6 );

					if ( ( rNumericArgs( 8 ) > 1 ) || ( rNumericArgs( 8 ) < 0 ) ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Source Side Effectiveness is out of bounds (0 to 1)" );
						ErrorsFound = true;
					}
					WaterThermalTank( WaterThermalTankNum ).SourceEffectiveness = rNumericArgs( 8 );

					if ( lNumericFieldBlanks( 7 ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = 0.0;
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = rNumericArgs( 7 );
						if ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate ) {
							WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized = true;
						}
					}

					WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide = DemandSupply_No;

					if ( lAlphaFieldBlanks( 9 ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = ScheduleAlwaysOn;
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = GetScheduleIndex( cAlphaArgs( 9 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 9 ) + " = " + cAlphaArgs( 9 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Schedule was not found." );
							ErrorsFound = true;
						}
					}

					WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide = DemandSupply_No;

					if ( lNumericFieldBlanks( 9 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = 0.0;
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = rNumericArgs( 9 );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate == AutoSize ) {
							WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized = true;
						}
					}

					if ( lAlphaFieldBlanks( 12 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = ScheduleAlwaysOn;
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = GetScheduleIndex( cAlphaArgs( 12 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 12 ) + " = " + cAlphaArgs( 12 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Schedule was not found." );
							ErrorsFound = true;
						}
					}
					if ( lNumericFieldBlanks( 10 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime = 4.0;
					} else {
						WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime = rNumericArgs( 10 );
					}

					if ( ( ! lAlphaFieldBlanks( 7 ) ) || ( ! lAlphaFieldBlanks( 8 ) ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName1 = cAlphaArgs( 7 );
						WaterThermalTank( WaterThermalTankNum ).UseOutletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName1 = cAlphaArgs( 8 );

					}

					if ( ( ! lAlphaFieldBlanks( 10 ) ) || ( ! lAlphaFieldBlanks( 11 ) ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceInletNode = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName2 = cAlphaArgs( 10 );
						WaterThermalTank( WaterThermalTankNum ).SourceOutletNode = GetOnlySingleNode( cAlphaArgs( 11 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName2 = cAlphaArgs( 11 );

					}

					if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide == DemandSide && WaterThermalTank( WaterThermalTankNum ).SourceInletNode != 0 ) {
						RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
					}

				} // WaterThermalTankNum

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			}

			//! end chilled water mixed storage

			//!!=======   Get 'ThermalStorage:ChilledWater:Stratified' =======================================================
			if ( NumChilledWaterStratified > 0 ) {
				cCurrentModuleObject = cStratifiedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Stratified'

				for ( WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + 1; WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + NumChilledWaterStratified; ++WaterThermalTankNum ) {

					GetObjectItem( cCurrentModuleObject, WaterThermalTankNum - ( NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed ), cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), WaterThermalTank.Name(), WaterThermalTankNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					WaterThermalTank( WaterThermalTankNum ).Name = cAlphaArgs( 1 );
					WaterThermalTank( WaterThermalTankNum ).Type = cCurrentModuleObject;
					WaterThermalTank( WaterThermalTankNum ).TypeNum = StratifiedChilledWaterStorage;
					WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank = true;
					WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName = "Chilled Water Storage";

					WaterThermalTank( WaterThermalTankNum ).Volume = rNumericArgs( 1 );
					if ( WaterThermalTank( WaterThermalTankNum ).Volume == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized = true;
					}
					rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineNameNoColon );
					WaterThermalTank( WaterThermalTankNum ).Mass = WaterThermalTank( WaterThermalTankNum ).Volume * rho;
					WaterThermalTank( WaterThermalTankNum ).Height = rNumericArgs( 2 );
					if ( WaterThermalTank( WaterThermalTankNum ).Height == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized = true;
					}

					{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );
					if ( SELECT_CASE_var == "VERTICALCYLINDER" ) {
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeVertCylinder;

					} else if ( SELECT_CASE_var == "HORIZONTALCYLINDER" ) {
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeHorizCylinder;

					} else if ( SELECT_CASE_var == "OTHER" ) {
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeOther;
						if ( rNumericArgs( 3 ) > 0.0 ) {
							WaterThermalTank( WaterThermalTankNum ).Perimeter = rNumericArgs( 3 );
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER" );
							ErrorsFound = true;
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Tank Shape entered=" + cAlphaArgs( 2 ) );
						WaterThermalTank( WaterThermalTankNum ).Shape = TankShapeVertCylinder;
						ErrorsFound = true;
					}}

					if ( rNumericArgs( 6 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = rNumericArgs( 6 );
					} else {
						// default to just above freezing
						WaterThermalTank( WaterThermalTankNum ).TankTempLimit = 1.0;
					}

					WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule = GetScheduleIndex( cAlphaArgs( 3 ) );
					if ( WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule == 0 ) {
						ShowSevereError( "Invalid, " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Schedule was not found." );
						ErrorsFound = true;
					}

					if ( rNumericArgs( 4 ) > 0.0 ) {
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = rNumericArgs( 4 );
					} else {
						// Default to very small number (however it can't be TINY or it will break the algorithm)
						WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp = 0.0001;
					}

					WaterThermalTank( WaterThermalTankNum ).HeaterHeight1 = rNumericArgs( 5 );
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = rNumericArgs( 7 );
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacity == AutoSize ) {
						WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized = true;
					}

					WaterThermalTank( WaterThermalTankNum ).Efficiency = 1.0;
					WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule2 = 0;
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity2 = 0.0;
					WaterThermalTank( WaterThermalTankNum ).HeaterHeight2 = 0.0;
					WaterThermalTank( WaterThermalTankNum ).FuelType = "Electric";

					WaterThermalTank( WaterThermalTankNum ).OffCycParaLoad = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType = "Electric";
					WaterThermalTank( WaterThermalTankNum ).OffCycParaFracToTank = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaHeight = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaLoad = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType = "Electric";
					WaterThermalTank( WaterThermalTankNum ).OnCycParaFracToTank = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaHeight = 0.0;

					{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) );
					if ( SELECT_CASE_var == "SCHEDULE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempSchedule;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule = GetScheduleIndex( cAlphaArgs( 5 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Schedule was not found." );
							ErrorsFound = true;
						}

					} else if ( SELECT_CASE_var == "ZONE" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempZone;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempZone = FindItemInList( cAlphaArgs( 6 ), Zone.Name(), NumOfZones );
						if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Zone was not found." );
							ErrorsFound = true;
						}
						WaterThermalTank( WaterThermalTankNum ).OffCycLossFracToZone = 1.0;

					} else if ( SELECT_CASE_var == "OUTDOORS" ) {
						WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator = AmbientTempOutsideAir;
						WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						if ( ! lAlphaFieldBlanks( 7 ) ) {
							if ( ! CheckOutAirNodeNumber( WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode ) ) {
								ShowSevereError( "Invalid, " + cAlphaFieldNames( 7 ) + " = " + cAlphaArgs( 7 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
								ShowContinueError( "Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node" );
								ErrorsFound = true;
							}
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors." );
							ErrorsFound = true;
						}

					} else {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs( 4 ) );
						ShowContinueError( "  Valid entries are Schedule, Zone, and Outdoors." );
						ErrorsFound = true;
					}}

					WaterThermalTank( WaterThermalTankNum ).SkinLossCoeff = rNumericArgs( 8 );
					WaterThermalTank( WaterThermalTankNum ).SkinLossFracToZone = 1.0;
					WaterThermalTank( WaterThermalTankNum ).OffCycFlueLossCoeff = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OffCycFlueLossFracToZone = 0.0;

					WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax = 0.0;
					WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule = 0;
					WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule = 0;
					WaterThermalTank( WaterThermalTankNum ).UseEffectiveness = rNumericArgs( 9 );
					WaterThermalTank( WaterThermalTankNum ).UseInletHeight = rNumericArgs( 10 );

					// default to always on
					WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = ScheduleAlwaysOn;
					WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = ScheduleAlwaysOn;

					if ( rNumericArgs( 10 ) == AutoCalculate ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletHeight = WaterThermalTank( WaterThermalTankNum ).Height; // top of tank
					}
					if ( WaterThermalTank( WaterThermalTankNum ).UseInletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Use inlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 10 ) + " = " + RoundSigDigits( rNumericArgs( 10 ), 4 ) );
						ErrorsFound = true;
					}

					WaterThermalTank( WaterThermalTankNum ).UseOutletHeight = rNumericArgs( 11 );
					if ( WaterThermalTank( WaterThermalTankNum ).UseOutletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Use outlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 11 ) + " = " + RoundSigDigits( rNumericArgs( 11 ), 4 ) );
						ErrorsFound = true;
					}

					WaterThermalTank( WaterThermalTankNum ).SourceEffectiveness = rNumericArgs( 13 );

					WaterThermalTank( WaterThermalTankNum ).SourceInletHeight = rNumericArgs( 14 );
					if ( WaterThermalTank( WaterThermalTankNum ).SourceInletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Source inlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 14 ) + " = " + RoundSigDigits( rNumericArgs( 14 ), 4 ) );
						ErrorsFound = true;
					}

					WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight = rNumericArgs( 15 );
					if ( rNumericArgs( 15 ) == AutoCalculate ) {
						WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight = WaterThermalTank( WaterThermalTankNum ).Height; // top of tank
					}
					if ( WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight > WaterThermalTank( WaterThermalTankNum ).Height ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ": Source outlet is located higher than overall tank height." );
						ShowContinueError( cNumericFieldNames( 2 ) + " = " + RoundSigDigits( rNumericArgs( 2 ), 4 ) );
						ShowContinueError( cNumericFieldNames( 15 ) + " = " + RoundSigDigits( rNumericArgs( 15 ), 4 ) );
						ErrorsFound = true;
					}

					WaterThermalTank( WaterThermalTankNum ).StandAlone = false;

					if ( lNumericFieldBlanks( 12 ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = 0.0;
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = rNumericArgs( 12 );
						if ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate == AutoSize ) {
							WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized = true;
						}
					}

					WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide = DemandSupply_No;

					if ( lNumericFieldBlanks( 16 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = 0.0;
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = rNumericArgs( 16 );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate == AutoSize ) {
							WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized = true;
						}
					}

					WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime = rNumericArgs( 17 );

					WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide = DemandSupply_No;

					if ( ( ! lAlphaFieldBlanks( 8 ) ) || ( ! lAlphaFieldBlanks( 9 ) ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseInletNode = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName1 = cAlphaArgs( 8 );
						WaterThermalTank( WaterThermalTankNum ).UseOutletNode = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName1 = cAlphaArgs( 9 );

					}

					if ( ( ! lAlphaFieldBlanks( 11 ) ) || ( ! lAlphaFieldBlanks( 12 ) ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceInletNode = GetOnlySingleNode( cAlphaArgs( 11 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).InletNodeName2 = cAlphaArgs( 11 );
						WaterThermalTank( WaterThermalTankNum ).SourceOutletNode = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
						WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName2 = cAlphaArgs( 12 );

					}

					if ( lAlphaFieldBlanks( 10 ) ) {
						WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = ScheduleAlwaysOn;
					} else {
						WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum = GetScheduleIndex( cAlphaArgs( 10 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 10 ) + " = " + cAlphaArgs( 10 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Schedule was not found." );
							ErrorsFound = true;
						}
					}

					if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide == DemandSide && WaterThermalTank( WaterThermalTankNum ).SourceInletNode != 0 ) {
						RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
					}

					if ( lAlphaFieldBlanks( 13 ) ) {
						WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = ScheduleAlwaysOn;
					} else {
						WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum = GetScheduleIndex( cAlphaArgs( 13 ) );
						if ( WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum == 0 ) {
							ShowSevereError( "Invalid, " + cAlphaFieldNames( 13 ) + " = " + cAlphaArgs( 13 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Schedule was not found." );
							ErrorsFound = true;
						}
					}

					// Validate inlet mode
					{ auto const SELECT_CASE_var( cAlphaArgs( 14 ) );
					if ( SELECT_CASE_var == "FIXED" ) {
						WaterThermalTank( WaterThermalTankNum ).InletMode = InletModeFixed;

					} else if ( SELECT_CASE_var == "SEEKING" ) {
						WaterThermalTank( WaterThermalTankNum ).InletMode = InletModeSeeking;
					}}

					WaterThermalTank( WaterThermalTankNum ).Nodes = rNumericArgs( 18 );
					WaterThermalTank( WaterThermalTankNum ).AdditionalCond = rNumericArgs( 19 );

					WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff.allocate( WaterThermalTank( WaterThermalTankNum ).Nodes );
					WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff = 0.0;
					for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
						if ( NumNums > 19 + NodeNum ) {
							WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff( NodeNum ) = rNumericArgs( 19 + NodeNum );
						} else {
							break;
						}
					}

					if ( NumNums > 19 + WaterThermalTank( WaterThermalTankNum ).Nodes ) {
						ShowWarningError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used" );
					}

					SetupStratifiedNodes( WaterThermalTankNum );

				} // WaterThermalTankNum

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			}
			//!  end stratified chilled water storage

			//!!=======   Check Water Heaters ======================================================================================

			//   Loop through all desuperheating coils and then search all water heaters for the tank connected to the desuperheating coil
			if ( NumWaterHeaterDesuperheater > 0 ) {
				cCurrentModuleObject = "Coil:WaterHeating:Desuperheater";
				for ( DesuperheaterNum = 1; DesuperheaterNum <= NumWaterHeaterDesuperheater; ++DesuperheaterNum ) {

					for ( CheckWaterHeaterNum = 1; CheckWaterHeaterNum <= NumWaterThermalTank; ++CheckWaterHeaterNum ) {
						if ( ! SameString( WaterHeaterDesuperheater( DesuperheaterNum ).TankName, WaterThermalTank( CheckWaterHeaterNum ).Name ) || ! SameString( WaterHeaterDesuperheater( DesuperheaterNum ).TankType, WaterThermalTank( CheckWaterHeaterNum ).Type ) ) continue;
						WaterThermalTank( CheckWaterHeaterNum ).DesuperheaterNum = DesuperheaterNum;
						WaterHeaterDesuperheater( DesuperheaterNum ).WaterHeaterTankNum = CheckWaterHeaterNum;
						WaterHeaterDesuperheater( DesuperheaterNum ).TankTypeNum = WaterThermalTank( CheckWaterHeaterNum ).TypeNum;
						WaterHeaterDesuperheater( DesuperheaterNum ).BackupElementCapacity = WaterThermalTank( CheckWaterHeaterNum ).MaxCapacity;
						if ( WaterThermalTank( CheckWaterHeaterNum ).UseInletNode == 0 && WaterThermalTank( CheckWaterHeaterNum ).UseOutletNode == 0 ) WaterHeaterDesuperheater( DesuperheaterNum ).StandAlone = true;

						//         verify Desuperheater/tank source node connections
						if ( WaterHeaterDesuperheater( DesuperheaterNum ).WaterInletNode != WaterThermalTank( CheckWaterHeaterNum ).SourceOutletNode ) {
							ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ':' );
							ShowContinueError( "Desuperheater inlet node name does not match thermal tank source outlet node name." );
							ShowContinueError( "Desuperheater water inlet and outlet node names = " + CoilSaveNodeNames( DesuperheaterNum ).InletNodeName1 + " and " + CoilSaveNodeNames( DesuperheaterNum ).OutletNodeName1 );
							ShowContinueError( "Thermal tank source side inlet and outlet node names      = " + WHSaveNodeNames( CheckWaterHeaterNum ).InletNodeName2 + " and " + WHSaveNodeNames( CheckWaterHeaterNum ).OutletNodeName2 );
							ErrorsFound = true;
						}

						if ( WaterHeaterDesuperheater( DesuperheaterNum ).WaterOutletNode != WaterThermalTank( CheckWaterHeaterNum ).SourceInletNode ) {
							ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ':' );
							ShowContinueError( "Desuperheater water outlet node name does not match thermal tank source inlet node name." );
							ShowContinueError( "Desuperheater water inlet and outlet node names = " + CoilSaveNodeNames( DesuperheaterNum ).InletNodeName1 + " and " + CoilSaveNodeNames( DesuperheaterNum ).OutletNodeName1 );
							ShowContinueError( "Thermal tank source side inlet and outlet node names      = " + WHSaveNodeNames( CheckWaterHeaterNum ).InletNodeName2 + " and " + WHSaveNodeNames( CheckWaterHeaterNum ).OutletNodeName2 );
							ErrorsFound = true;
						}

					} // DO CheckWaterHeaterNum = 1, NumWaterHeater

					if ( WaterHeaterDesuperheater( DesuperheaterNum ).WaterHeaterTankNum == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ':' );
						ShowContinueError( " Water heater tank = " + WaterHeaterDesuperheater( DesuperheaterNum ).TankName + " not found." );
						ErrorsFound = true;
					}

				} // DO DesuperheaterNum = 1, NumWaterHeaterDesuperheater
			}

			//   Loop through HPWH's and then search all water heaters for the tank connected to the HPWH
			if ( NumHeatPumpWaterHeater > 0 ) {

				cCurrentModuleObject = "WaterHeater:HeatPump";

				for ( HPWaterHeaterNum = 1; HPWaterHeaterNum <= NumHeatPumpWaterHeater; ++HPWaterHeaterNum ) {

					//       find the tank associated with the heat pump water heater and change its %TYPE to HEAT PUMP:WATER HEATER
					for ( CheckWaterHeaterNum = 1; CheckWaterHeaterNum <= NumWaterThermalTank; ++CheckWaterHeaterNum ) {
						if ( ! SameString( HPWaterHeater( HPWaterHeaterNum ).TankName, WaterThermalTank( CheckWaterHeaterNum ).Name ) || ! SameString( HPWaterHeater( HPWaterHeaterNum ).TankType, WaterThermalTank( CheckWaterHeaterNum ).Type ) ) continue;

						//         save backup element and on/off-cycle parasitic properties for use during standard rating procedure
						HPWaterHeater( HPWaterHeaterNum ).BackupElementCapacity = WaterThermalTank( CheckWaterHeaterNum ).MaxCapacity;
						HPWaterHeater( HPWaterHeaterNum ).BackupElementEfficiency = WaterThermalTank( CheckWaterHeaterNum ).Efficiency;
						HPWaterHeater( HPWaterHeaterNum ).WHOnCycParaLoad = WaterThermalTank( CheckWaterHeaterNum ).OnCycParaLoad;
						HPWaterHeater( HPWaterHeaterNum ).WHOffCycParaLoad = WaterThermalTank( CheckWaterHeaterNum ).OffCycParaLoad;
						HPWaterHeater( HPWaterHeaterNum ).WHOnCycParaFracToTank = WaterThermalTank( CheckWaterHeaterNum ).OnCycParaFracToTank;
						HPWaterHeater( HPWaterHeaterNum ).WHOffCycParaFracToTank = WaterThermalTank( CheckWaterHeaterNum ).OffCycParaFracToTank;
						HPWaterHeater( HPWaterHeaterNum ).WHPLFCurve = WaterThermalTank( CheckWaterHeaterNum ).PLFCurve;

						if ( WaterThermalTank( CheckWaterHeaterNum ).Type == "WATER HEATER:SIMPLE" ) { // name change issue here.
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
							ShowContinueError( "WaterHeater:HeatPump cannot be used with WATER HEATER:SIMPLE." );
							ErrorsFound = true;
						} else if ( ( WaterThermalTank( CheckWaterHeaterNum ).Type == cMixedWHModuleObj ) || ( WaterThermalTank( CheckWaterHeaterNum ).Type == cStratifiedWHModuleObj ) ) {
							HPWaterHeater( HPWaterHeaterNum ).TankTypeNum = WaterThermalTank( CheckWaterHeaterNum ).TypeNum;
							//           use typenum parameter to simulate heatpumpwaterheater in standard ratings procedure
							//           WaterThermalTank%TypeNum = HeatPumpWaterHeater for a HPWH
							//            WaterThermalTank(CheckWaterHeaterNum)%TypeNum = HPWaterHeater(HPWaterHeaterNum)%TypeNum
						} else {
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
							ShowContinueError( "Invalid water heater tank type =" + WaterThermalTank( CheckWaterHeaterNum ).Type );
							ErrorsFound = true;
						}

						//         do not allow modulating control for HPWH's (i.e. modulating control usually used for tankless WH's)
						if ( WaterThermalTank( CheckWaterHeaterNum ).ControlType == ControlTypeModulate ) {
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
							ShowContinueError( "Heater Control Type for " + WaterThermalTank( CheckWaterHeaterNum ).Type + " = " + WaterThermalTank( CheckWaterHeaterNum ).Name + " must be CYCLE." );
							ErrorsFound = true;
						}

						WaterThermalTank( CheckWaterHeaterNum ).HeatPumpNum = HPWaterHeaterNum;
						HPWaterHeater( HPWaterHeaterNum ).WaterHeaterTankNum = CheckWaterHeaterNum;

						if ( WaterThermalTank( CheckWaterHeaterNum ).DesuperheaterNum > 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + "and Coil:WaterHeating:Desuperheater = " + WaterHeaterDesuperheater( CheckWaterHeaterNum ).Name + ":  cannot be connected to the same water heater tank = " + WaterThermalTank( CheckWaterHeaterNum ).Name );
						}

						//         check that water heater source side effectiveness is greater than 0
						if ( WaterThermalTank( CheckWaterHeaterNum ).SourceEffectiveness <= 0.0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ":  Invalid source side effectiveness for heat pump water heater = " + TrimSigDigits( WaterThermalTank( CheckWaterHeaterNum ).SourceEffectiveness, 3 ) );
							ShowContinueError( " water heater source effectiveness will default to 1.0 and simulation continues." );
							WaterThermalTank( CheckWaterHeaterNum ).SourceEffectiveness = 1.0;
						}

						//         Set HPWH structure variable StandAlone to TRUE if use nodes are not connected
						if ( WaterThermalTank( CheckWaterHeaterNum ).UseInletNode == 0 && WaterThermalTank( CheckWaterHeaterNum ).UseOutletNode == 0 ) HPWaterHeater( HPWaterHeaterNum ).StandAlone = true;

						if ( HPWaterHeater( HPWaterHeaterNum ).WHUseInletNode != WaterThermalTank( CheckWaterHeaterNum ).UseInletNode || HPWaterHeater( HPWaterHeaterNum ).WHUseOutletNode != WaterThermalTank( CheckWaterHeaterNum ).UseOutletNode ) {
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
							ShowContinueError( "Heat pump water heater tank use side inlet and outlet node names must match the use side inlet and outlet node names for water heater tank = " + HPWaterHeater( HPWaterHeaterNum ).TankType + ": " + HPWaterHeater( HPWaterHeaterNum ).TankName );
							ShowContinueError( "Heat pump water heater use side inlet and outlet node names = " + HPWHSaveNodeNames( HPWaterHeaterNum ).InletNodeName2 + " and " + HPWHSaveNodeNames( HPWaterHeaterNum ).OutletNodeName2 );
							ShowContinueError( "Water heater tank use side inlet and outlet node names      = " + WHSaveNodeNames( CheckWaterHeaterNum ).InletNodeName1 + " and " + WHSaveNodeNames( CheckWaterHeaterNum ).OutletNodeName1 );
							ErrorsFound = true;
						} else {
							if ( ! HPWaterHeater( HPWaterHeaterNum ).StandAlone ) {
								//              removed next to avoid duplicate comp set issue, (should change so that Branch has tank object)
								//              CALL SetUpCompSets(HPWaterHeater(HPWaterHeaterNum)%Type, HPWaterHeater(HPWaterHeaterNum)%Name, &
								//                     HPWaterHeater(HPWaterHeaterNum)%TankType, &
								//                     HPWaterHeater(HPWaterHeaterNum)%TankName, &
								//                     WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName1,WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName1)
								TestCompSet( HPWaterHeater( HPWaterHeaterNum ).Type, HPWaterHeater( HPWaterHeaterNum ).Name, WHSaveNodeNames( CheckWaterHeaterNum ).InletNodeName1, WHSaveNodeNames( CheckWaterHeaterNum ).OutletNodeName1, "Water Nodes" );
							}
						}

						//         verify HP/tank source node connections
						if ( HPWaterHeater( HPWaterHeaterNum ).CondWaterInletNode != WaterThermalTank( CheckWaterHeaterNum ).SourceOutletNode ) {
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
							ShowContinueError( "Heat Pump condenser water inlet node name does not match water heater tank source outlet node name." );
							ShowContinueError( "Heat pump condenser water inlet and outlet node names = " + HPWHSaveNodeNames( HPWaterHeaterNum ).InletNodeName1 + " and " + HPWHSaveNodeNames( HPWaterHeaterNum ).OutletNodeName1 );
							ShowContinueError( "Water heater tank source side inlet and outlet node names      = " + WHSaveNodeNames( CheckWaterHeaterNum ).InletNodeName2 + " and " + WHSaveNodeNames( CheckWaterHeaterNum ).OutletNodeName2 );
							ErrorsFound = true;
						}

						if ( HPWaterHeater( HPWaterHeaterNum ).CondWaterOutletNode != WaterThermalTank( CheckWaterHeaterNum ).SourceInletNode ) {
							ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
							ShowContinueError( "Heat Pump condenser water outlet node name does not match water heater tank source inlet node name." );
							ShowContinueError( "Heat pump condenser water inlet and outlet node names = " + HPWHSaveNodeNames( HPWaterHeaterNum ).InletNodeName1 + " and " + HPWHSaveNodeNames( HPWaterHeaterNum ).OutletNodeName1 );
							ShowContinueError( "Water heater tank source side inlet and outlet node names      = " + WHSaveNodeNames( CheckWaterHeaterNum ).InletNodeName2 + " and " + WHSaveNodeNames( CheckWaterHeaterNum ).OutletNodeName2 );
							ErrorsFound = true;
						}

						HPWaterHeater( HPWaterHeaterNum ).FoundTank = true;

						//         Verify tank name is in a zone equipment list if HPWH Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
						if ( HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZone || HPWaterHeater( HPWaterHeaterNum ).InletAirConfiguration == AmbientTempZoneAndOA ) {
							if ( allocated( ZoneEquipConfig ) && allocated( ZoneEquipList ) ) {
								FoundTankInList = false;
								TankNotLowestPriority = false;
								for ( ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum ) {
									if ( ZoneEquipConfig( ZoneEquipConfigNum ).ActualZoneNum != HPWaterHeater( HPWaterHeaterNum ).AmbientTempZone ) continue;
									if ( ZoneEquipConfigNum <= NumOfZones ) {
										for ( ZoneEquipListNum = 1; ZoneEquipListNum <= NumOfZones; ++ZoneEquipListNum ) {
											if ( ZoneEquipConfig( ZoneEquipConfigNum ).EquipListName != ZoneEquipList( ZoneEquipListNum ).Name ) continue;
											if ( ZoneEquipConfigNum <= NumOfZones ) {
												for ( EquipmentTypeNum = 1; EquipmentTypeNum <= ZoneEquipList( ZoneEquipListNum ).NumOfEquipTypes; ++EquipmentTypeNum ) {
													if ( ZoneEquipList( ZoneEquipListNum ).EquipName( EquipmentTypeNum ) != HPWaterHeater( HPWaterHeaterNum ).Name ) continue;
													FoundTankInList = true;
													TankCoolingPriority = ZoneEquipList( ZoneEquipListNum ).CoolingPriority( EquipmentTypeNum );
													TankHeatingPriority = ZoneEquipList( ZoneEquipListNum ).HeatingPriority( EquipmentTypeNum );
													break;
												} // EquipmentTypeNum
												if ( ! FoundTankInList ) {
													ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
													ShowContinueError( "Heat pump water heater type and name must be listed in the correct ZoneHVAC:EquipmentList object when Inlet Air Configuration is equal to ZoneAirOnly or ZoneAndOutdoorAir." );
													ErrorsFound = true;
												}
												//                     check that tank has lower priority than all other non-HPWH objects in Zone Equipment List
												for ( EquipmentTypeNum = 1; EquipmentTypeNum <= ZoneEquipList( ZoneEquipListNum ).NumOfEquipTypes; ++EquipmentTypeNum ) {
													if ( SameString( ZoneEquipList( ZoneEquipListNum ).EquipType( EquipmentTypeNum ), cCurrentModuleObject ) ) continue;
													if ( TankCoolingPriority > ZoneEquipList( ZoneEquipListNum ).CoolingPriority( EquipmentTypeNum ) || TankHeatingPriority > ZoneEquipList( ZoneEquipListNum ).HeatingPriority( EquipmentTypeNum ) ) {
														TankNotLowestPriority = true;
													}
												} // EquipmentTypeNum
												if ( TankNotLowestPriority && FoundTankInList ) {
													ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
													ShowContinueError( "Heat pump water heaters must have lower priorities than all other equipment types in a ZoneHVAC:EquipmentList." );
													ErrorsFound = true;
												}
												break;
											} // ZoneEquipConfigNum .LE. NumOfZoneEquipLists
										} // ZoneEquipListNum
										break;
									} // ZoneEquipConfigNum .LE. NumOfZones
								} // ZoneEquipConfigNum
							} else {
								ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
								ShowContinueError( "ZoneHVAC:EquipmentList and ZoneHVAC:EquipmentConnections objects are required when Inlet Air Configuration is either ZoneAirOnly or ZoneAndOutdoorAir." );
								ErrorsFound = true;
							} // ALLOCATED
						} //InletAirConfiguration

					} // DO CheckWaterHeaterNum = 1, NumWaterHeater

					if ( ! HPWaterHeater( HPWaterHeaterNum ).FoundTank ) {
						ShowSevereError( cCurrentModuleObject + " = " + HPWaterHeater( HPWaterHeaterNum ).Name + ':' );
						ShowContinueError( "Water heater tank object not found = " + HPWaterHeater( HPWaterHeaterNum ).TankType + ", " + HPWaterHeater( HPWaterHeaterNum ).TankName );
						ErrorsFound = true;
					}

				} // DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			}

			//Get water heater sizing input.
			cCurrentModuleObject = "WaterHeater:Sizing";
			NumWaterHeaterSizing = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumWaterHeaterSizing > 0 ) {

				for ( WHsizingNum = 1; WHsizingNum <= NumWaterHeaterSizing; ++WHsizingNum ) {
					GetObjectItem( cCurrentModuleObject, WHsizingNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

					// find which water heater this object is for
					WaterThermalTankNum = FindItemInList( cAlphaArgs( 1 ), WaterThermalTank.Name(), NumWaterThermalTank );
					if ( WaterThermalTankNum == 0 ) {
						// did not match name throw warning.
						ShowSevereError( cCurrentModuleObject + " object name: " + cAlphaArgs( 1 ) + " does not match any of the water heaters defined in the file" );
						ErrorsFound = true;
						continue;
					} else { // we have a match
						// store the sizing data in "sizing" nested derived type for the correct water heater

						if ( SameString( cAlphaArgs( 2 ), "PeakDraw" ) ) {
							WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode = SizePeakDraw;
						} else if ( SameString( cAlphaArgs( 2 ), "ResidentialHUD-FHAMinimum" ) ) {
							WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode = SizeResidentialMin;
						} else if ( SameString( cAlphaArgs( 2 ), "PerPerson" ) ) {
							WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode = SizePerPerson;
						} else if ( SameString( cAlphaArgs( 2 ), "PerFloorArea" ) ) {
							WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode = SizePerFloorArea;
						} else if ( SameString( cAlphaArgs( 2 ), "PerUnit" ) ) {
							WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode = SizePerUnit;
						} else if ( SameString( cAlphaArgs( 2 ), "PerSolarCollectorArea" ) ) {
							WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode = SizePerSolarColArea;
						} else {
							// wrong design mode entered, throw error
							ShowSevereError( cCurrentModuleObject + " object named: " + cAlphaArgs( 1 ) + " contains an incorrect Design Mode of: " + cAlphaArgs( 2 ) );
							ErrorsFound = true;
						}

						WaterThermalTank( WaterThermalTankNum ).Sizing.TankDrawTime = rNumericArgs( 1 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryTime = rNumericArgs( 2 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.NominalVolForSizingDemandSideFlow = rNumericArgs( 3 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms = int( rNumericArgs( 4 ) );
						WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms = int( rNumericArgs( 5 ) );
						WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerPerson = rNumericArgs( 6 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerPerson = rNumericArgs( 7 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerArea = rNumericArgs( 8 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerArea = rNumericArgs( 9 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfUnits = rNumericArgs( 10 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerUnit = rNumericArgs( 11 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerUnit = rNumericArgs( 12 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerCollectorArea = rNumericArgs( 13 );
						WaterThermalTank( WaterThermalTankNum ).Sizing.HeightAspectRatio = rNumericArgs( 14 );

						{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode );

						if ( SELECT_CASE_var == SizeNotSet ) {
							// do nothing, error thrown if design mode not found
						} else if ( SELECT_CASE_var == SizePeakDraw ) { // need to have entered a reasonable value for TankDrawTime
							if ( WaterThermalTank( WaterThermalTankNum ).Sizing.TankDrawTime <= 0.0 ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", design mode set to Peak Draw but needs a positive value for tank draw time" );
								ErrorsFound = true;
							}
							//constrain crazy sizes by limiting to 10 years or 8760*10
							if ( WaterThermalTank( WaterThermalTankNum ).Sizing.TankDrawTime > 87600.0 ) {
								ShowWarningError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ",  has input with an unreasonably large Tank Draw Time, more than 10 years" );
								ErrorsFound = true;
							}
							// if both volume and demand side flow connections are autosized, must be a good NominalVolForSizingDemandSideFlow
							if ( ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide == DemandSide )
								&& ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized ) ) {
								if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NominalVolForSizingDemandSideFlow <= 0.0 ) {
									ShowWarningError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + " needs a value for Nominal Tank Volume for Autosizing Plant Connections" );
									ErrorsFound = true;
								}
							}
							if ( ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide == DemandSide ) && ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized ) ) {
								if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NominalVolForSizingDemandSideFlow <= 0.0 ) {
									ShowWarningError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + " needs a value for Nominal Tank Volume for Autosizing Plant Connections" );
									ErrorsFound = true;
								}
							}

						} else if ( SELECT_CASE_var == SizeResidentialMin ) {
							// it would have to have at least on bedroom and any more than 10 is crazy for this mode
							if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms < 1 ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", mode needs at least one bedroom" );
								ErrorsFound = true;
							}
							if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms > 10 ) {
								ShowWarningError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", probably has too many bedrooms for the selected design mode" );
							}

						} else if ( SELECT_CASE_var == SizePerPerson ) {

							if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerPerson <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerPerson mode needs positive value input for storage capacity per person" );
								ErrorsFound = true;
							}

							if ( ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerPerson <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerPerson mode needs positive value input for recovery capacity per person" );
								ErrorsFound = true;
							}

						} else if ( SELECT_CASE_var == SizePerFloorArea ) {
							if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerArea <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerArea mode needs positive value input for storage capacity per floor area" );
								ErrorsFound = true;
							}
							if ( ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerArea <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerArea mode needs positive value input for recovery capacity per floor area" );
								ErrorsFound = true;
							}

						} else if ( SELECT_CASE_var == SizePerUnit ) {
							if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerUnit <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerUnit mode needs positive value input for storage capacity per unit" );
								ErrorsFound = true;
							}
							if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfUnits <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerUnit mode needs positive value input for number of units" );
								ErrorsFound = true;
							}
							if ( ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerUnit <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerUnit mode needs positive value input for recovery capacity per unit" );
								ErrorsFound = true;
							}
							if ( ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfUnits <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerUnit mode needs positive value input for number of units" );
								ErrorsFound = true;
							}
						} else if ( SELECT_CASE_var == SizePerSolarColArea ) {
							if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerCollectorArea <= 0.0 ) ) {
								ShowSevereError( cCurrentModuleObject + ", named " + cAlphaArgs( 1 ) + ", PerSolarCollectorArea mode needs positive value input for storage capacity per collector area" );
								ErrorsFound = true;
							}
						}}

					} //found water heater num okay
				} // loop over sizing objects

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination." );
				}

			} // any water heater sizing objects

			//now check that if water heater fields were autosized, that there was also a sizing object for that water heater
			if ( NumWaterThermalTank > 0 ) {
				for ( WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum ) {

					if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode == SizeNotSet ) ) {
						ShowWarningError( "Water heater named " + WaterThermalTank( WaterThermalTankNum ).Name + "has tank volume set to AUTOSIZE but it is missing associated WaterHeater:Sizing object" );
						ErrorsFound = true;
					}
					if ( ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode == SizeNotSet ) ) {
						ShowWarningError( "Water heater named " + WaterThermalTank( WaterThermalTankNum ).Name + "has heater capacity set to AUTOSIZE but it is missing associated WaterHeater:Sizing object" );
						ErrorsFound = true;
					}
					if ( ( WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode == SizeNotSet ) ) {
						ShowWarningError( "Water heater named " + WaterThermalTank( WaterThermalTankNum ).Name + "has tank height set to AUTOSIZE but it is missing associated WaterHeater:Sizing object" );
						ErrorsFound = true;
					}
				}

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in water heater input. Preceding condition causes termination." );
				}
			}

			//!   now do calls to TestCompSet for tanks, depending on nodes and heat pump water heater
			if ( NumWaterThermalTank > 0 ) {
				for ( WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum ) {
					if ( WaterThermalTank( WaterThermalTankNum ).UseInletNode > 0 && WaterThermalTank( WaterThermalTankNum ).UseOutletNode > 0 ) {
						if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {
							// do nothing, Use nodes are tested for HeatPump:WaterHeater not tank
						} else {
							TestCompSet( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, WHSaveNodeNames( WaterThermalTankNum ).InletNodeName1, WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName1, "Use Side Water Nodes" );
						}
					}
					if ( WaterThermalTank( WaterThermalTankNum ).SourceInletNode > 0 && WaterThermalTank( WaterThermalTankNum ).SourceOutletNode > 0 ) {

						TestCompSet( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, WHSaveNodeNames( WaterThermalTankNum ).InletNodeName2, WHSaveNodeNames( WaterThermalTankNum ).OutletNodeName2, "Source Side Water Nodes" );
					}
				}
			}

			if ( NumWaterThermalTank > 0 ) {
				for ( WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum ) {
					if ( ( WaterThermalTank( WaterThermalTankNum ).TypeNum != MixedChilledWaterStorage ) && ( WaterThermalTank( WaterThermalTankNum ).TypeNum != StratifiedChilledWaterStorage ) ) {
						// Setup report variables for WaterHeater:Mixed
						// CurrentModuleObject='WaterHeater:Mixed'
						SetupOutputVariable( "Water Heater Tank Temperature [C]", WaterThermalTank( WaterThermalTankNum ).TankTempAvg, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Final Tank Temperature [C]", WaterThermalTank( WaterThermalTankNum ).TankTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Heat Loss Rate [W]", WaterThermalTank( WaterThermalTankNum ).LossRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Heat Loss Energy [J]", WaterThermalTank( WaterThermalTankNum ).LossEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Use Side Mass Flow Rate [kg/s]", WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Use Side Inlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).UseInletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Use Side Outlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).UseOutletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Use Side Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).UseRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Use Side Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).UseEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Source Side Mass Flow Rate [kg/s]", WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Source Side Inlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).SourceInletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Source Side Outlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Source Side Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).SourceRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Source Side Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).SourceEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "DHW", WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName, "Plant" );

						SetupOutputVariable( "Water Heater Off Cycle Parasitic Tank Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).OffCycParaRateToTank, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Off Cycle Parasitic Tank Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).OffCycParaEnergyToTank, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater On Cycle Parasitic Tank Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).OnCycParaRateToTank, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater On Cycle Parasitic Tank Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).OnCycParaEnergyToTank, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Total Demand Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).TotalDemandRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Total Demand Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).TotalDemandEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Heating Rate [W]", WaterThermalTank( WaterThermalTankNum ).HeaterRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Heating Energy [J]", WaterThermalTank( WaterThermalTankNum ).HeaterEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Unmet Demand Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).UnmetRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Unmet Demand Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).UnmetEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Venting Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).VentRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Venting Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).VentEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Net Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).NetHeatTransferRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Net Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).NetHeatTransferEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Water Heater Cycle On Count []", WaterThermalTank( WaterThermalTankNum ).CycleOnCount, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Runtime Fraction []", WaterThermalTank( WaterThermalTankNum ).RuntimeFraction, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Part Load Ratio []", WaterThermalTank( WaterThermalTankNum ).PartLoadRatio, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							SetupOutputVariable( "Water Heater Electric Power [W]", WaterThermalTank( WaterThermalTankNum ).FuelRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						} else {
							SetupOutputVariable( "Water Heater " + WaterThermalTank( WaterThermalTankNum ).FuelType + " Rate [W]", WaterThermalTank( WaterThermalTankNum ).FuelRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						}
						SetupOutputVariable( "Water Heater " + WaterThermalTank( WaterThermalTankNum ).FuelType + " Energy [J]", WaterThermalTank( WaterThermalTankNum ).FuelEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name, _, WaterThermalTank( WaterThermalTankNum ).FuelType, "DHW", WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName, "Plant" );
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType, "Electric" ) ) {
							SetupOutputVariable( "Water Heater Off Cycle Parasitic Electric Power [W]", WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						} else {
							SetupOutputVariable( "Water Heater Off Cycle Parasitic " + WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType + " Rate [W]", WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						}
						SetupOutputVariable( "Water Heater Off Cycle Parasitic " + WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType + " Energy [J]", WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name, _, WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelType, "DHW", WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName, "Plant" );
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType, "Electric" ) ) {
							SetupOutputVariable( "Water Heater On Cycle Parasitic Electric Power [W]", WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						} else {
							SetupOutputVariable( "Water Heater On Cycle Parasitic " + WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType + " Rate [W]", WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						}

						SetupOutputVariable( "Water Heater On Cycle Parasitic " + WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType + " Energy [J]", WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name, _, WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelType, "DHW", WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName, "Plant" );

						SetupOutputVariable( "Water Heater Water Volume Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).VolFlowRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Water Heater Water Volume [m3]", WaterThermalTank( WaterThermalTankNum ).VolumeConsumed, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name, _, "Water", "DHW", WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName, "Plant" );
						SetupOutputVariable( "Water Heater Mains Water Volume [m3]", WaterThermalTank( WaterThermalTankNum ).VolumeConsumed, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name, _, "MainsWater", "DHW", WaterThermalTank( WaterThermalTankNum ).EndUseSubcategoryName, "Plant" );

						if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {
							//CurrentModuleObject='WaterHeater:HeatPump'
							SetupOutputVariable( "Water Heater Compressor Part Load Ratio []", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).HeatingPLR, "System", "Average", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name );
							SetupOutputVariable( "Water Heater Off Cycle Ancillary Electric Power [W]", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).OffCycParaFuelRate, "System", "Average", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name );
							SetupOutputVariable( "Water Heater Off Cycle Ancillary Electric Energy [J]", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).OffCycParaFuelEnergy, "System", "Sum", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name, _, "Electric", "DHW", "Water Heater Parasitic", "Plant" );
							SetupOutputVariable( "Water Heater On Cycle Ancillary Electric Power [W]", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).OnCycParaFuelRate, "System", "Average", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name );
							SetupOutputVariable( "Water Heater On Cycle Ancillary Electric Energy [J]", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).OnCycParaFuelEnergy, "System", "Sum", HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name, _, "Electric", "DHW", "Water Heater Parasitic", "Plant" );
						}

						if ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum > 0 ) {
							//CurrentModuleObject='Coil:WaterHeating:Desuperheater'
							SetupOutputVariable( "Water Heater Part Load Ratio []", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).DesuperheaterPLR, "System", "Average", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name );
							SetupOutputVariable( "Water Heater On Cycle Parasitic Electric Power [W]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).OnCycParaFuelRate, "System", "Average", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name );
							SetupOutputVariable( "Water Heater On Cycle Parasitic Electric Energy [J]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).OnCycParaFuelEnergy, "System", "Sum", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name, _, "Electric", "DHW", "Water Heater Parasitic", "Plant" );
							SetupOutputVariable( "Water Heater Off Cycle Parasitic Electric Power [W]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).OffCycParaFuelRate, "System", "Average", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name );
							SetupOutputVariable( "Water Heater Off Cycle Parasitic Electric Energy [J]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).OffCycParaFuelEnergy, "System", "Sum", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name, _, "Electric", "DHW", "Water Heater Parasitic", "Plant" );
							SetupOutputVariable( "Water Heater Heat Reclaim Efficiency Modifier Multiplier []", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).HEffFTempOutput, "System", "Average", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name );
							SetupOutputVariable( "Water Heater Pump Electric Power [W]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).PumpPower, "System", "Average", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name );
							SetupOutputVariable( "Water Heater Pump Electric Energy [J]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).PumpEnergy, "System", "Sum", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name, _, "Electric", "DHW", "Desuperheater Pump", "Plant" );
							SetupOutputVariable( "Water Heater Heating Rate [W]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).HeaterRate, "System", "Average", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name );
							SetupOutputVariable( "Water Heater Heating Energy [J]", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).HeaterEnergy, "System", "Sum", WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Name, _, "EnergyTransfer", "DHW", "Water Heater", "Plant" );
						}

						// Setup report variables for WaterHeater:Stratified
						// CurrentModuleObject='WaterHeater:Stratified'
						if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedWaterHeater ) {

							SetupOutputVariable( "Water Heater Heater 1 Heating Rate [W]", WaterThermalTank( WaterThermalTankNum ).HeaterRate1, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
							SetupOutputVariable( "Water Heater Heater 2 Heating Rate [W]", WaterThermalTank( WaterThermalTankNum ).HeaterRate2, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

							SetupOutputVariable( "Water Heater Heater 1 Heating Energy [J]", WaterThermalTank( WaterThermalTankNum ).HeaterEnergy1, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );
							SetupOutputVariable( "Water Heater Heater 2 Heating Energy [J]", WaterThermalTank( WaterThermalTankNum ).HeaterEnergy2, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

							SetupOutputVariable( "Water Heater Heater 1 Cycle On Count []", WaterThermalTank( WaterThermalTankNum ).CycleOnCount1, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );
							SetupOutputVariable( "Water Heater Heater 2 Cycle On Count  []", WaterThermalTank( WaterThermalTankNum ).CycleOnCount2, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

							SetupOutputVariable( "Water Heater Heater 1 Runtime Fraction []", WaterThermalTank( WaterThermalTankNum ).RuntimeFraction1, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
							SetupOutputVariable( "Water Heater Heater 2 Runtime Fraction []", WaterThermalTank( WaterThermalTankNum ).RuntimeFraction2, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

							for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
								SetupOutputVariable( "Water Heater Temperature Node " + TrimSigDigits( NodeNum ) + " [C]", WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).TempAvg, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
							}

							for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
								SetupOutputVariable( "Water Heater Final Temperature Node " + TrimSigDigits( NodeNum ) + "  [C]", WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Temp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
							}
						}

						if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedWaterHeater ) {

							for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
								gio::write( OutputFileInits, Format_723 ) << TrimSigDigits( NodeNum ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Height, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Volume, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).MaxCapacity, 3 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OffCycLossCoeff, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OnCycLossCoeff, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Inlets ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Outlets );
							}
						}

						if ( ErrorsFound ) {
							ShowFatalError( "Errors found in getting water heater input. Preceding condition causes termination." );
						}

					} else if ( ( WaterThermalTank( WaterThermalTankNum ).TypeNum == MixedChilledWaterStorage ) || ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedChilledWaterStorage ) ) {
						// CurrentModuleObject='ThermalStorage:ChilledWater:Mixed/ThermalStorage:ChilledWater:Stratified'
						SetupOutputVariable( "Chilled Water Thermal Storage Tank Temperature [C]", WaterThermalTank( WaterThermalTankNum ).TankTempAvg, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Final Tank Temperature [C]", WaterThermalTank( WaterThermalTankNum ).TankTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Tank Heat Gain Rate [W]", WaterThermalTank( WaterThermalTankNum ).LossRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Chilled Water Thermal Storage Tank Heat Gain Energy [J]", WaterThermalTank( WaterThermalTankNum ).LossEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Use Side Mass Flow Rate [kg/s]", WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Use Side Inlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).UseInletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Use Side Outlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).UseOutletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Use Side Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).UseRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Chilled Water Thermal Storage Use Side Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).UseEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Source Side Mass Flow Rate [kg/s]", WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Source Side Inlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).SourceInletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Source Side Outlet Temperature [C]", WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );

						SetupOutputVariable( "Chilled Water Thermal Storage Source Side Heat Transfer Rate [W]", WaterThermalTank( WaterThermalTankNum ).SourceRate, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
						SetupOutputVariable( "Chilled Water Thermal Storage Source Side Heat Transfer Energy [J]", WaterThermalTank( WaterThermalTankNum ).SourceEnergy, "System", "Sum", WaterThermalTank( WaterThermalTankNum ).Name );

						if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedChilledWaterStorage ) {

							for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
								SetupOutputVariable( "Chilled Water Thermal Storage Temperature Node " + TrimSigDigits( NodeNum ) + " [C]", WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).TempAvg, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
							}

							for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
								SetupOutputVariable( "Chilled Water Thermal Storage Final Temperature Node " + TrimSigDigits( NodeNum ) + " [C]", WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Temp, "System", "Average", WaterThermalTank( WaterThermalTankNum ).Name );
							}
						}

						if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedChilledWaterStorage ) {

							for ( NodeNum = 1; NodeNum <= WaterThermalTank( WaterThermalTankNum ).Nodes; ++NodeNum ) {
								gio::write( OutputFileInits, Format_724 ) << TrimSigDigits( NodeNum ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Height, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Volume, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OffCycLossCoeff, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Inlets ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Outlets );
							}
						}

						if ( ErrorsFound ) {
							ShowFatalError( "Errors found in getting chilled water tank input. Preceding condition causes termination." );
						}
					}

					// set up internal gains if tank is in a thermal zone
					if ( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone > 0 ) {
						{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).TypeNum );

						if ( SELECT_CASE_var == MixedWaterHeater ) {
							SetupZoneInternalGain( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone, "WaterHeater:Mixed", WaterThermalTank( WaterThermalTankNum ).Name, IntGainTypeOf_WaterHeaterMixed, WaterThermalTank( WaterThermalTankNum ).AmbientZoneGain );
						} else if ( SELECT_CASE_var == StratifiedWaterHeater ) {
							SetupZoneInternalGain( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone, "WaterHeater:Stratified", WaterThermalTank( WaterThermalTankNum ).Name, IntGainTypeOf_WaterHeaterStratified, WaterThermalTank( WaterThermalTankNum ).AmbientZoneGain );
						} else if ( SELECT_CASE_var == MixedChilledWaterStorage ) {
							SetupZoneInternalGain( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone, "ThermalStorage:ChilledWater:Mixed", WaterThermalTank( WaterThermalTankNum ).Name, IntGainTypeOf_ThermalStorageChilledWaterMixed, WaterThermalTank( WaterThermalTankNum ).AmbientZoneGain );
						} else if ( SELECT_CASE_var == StratifiedChilledWaterStorage ) {
							SetupZoneInternalGain( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone, "ThermalStorage:ChilledWater:Stratified", WaterThermalTank( WaterThermalTankNum ).Name, IntGainTypeOf_ThermalStorageChilledWaterStratified, WaterThermalTank( WaterThermalTankNum ).AmbientZoneGain );
						}}

					}

				} // WaterThermalTankNum
			}

		} // get input flag

		if ( allocated( HPWHSaveNodeNames ) ) HPWHSaveNodeNames.deallocate();
		if ( allocated( WHSaveNodeNames ) ) WHSaveNodeNames.deallocate();
		if ( allocated( CoilSaveNodeNames ) ) CoilSaveNodeNames.deallocate();

	}

	void
	ValidatePLFCurve(
		int const CurveIndex,
		bool & IsValid
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   February 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Validates the Part Load Factor curve by making sure it can never be less than or equal to zero
		// over the domain of Part Load Ratio inputs from 0 to 1.

		// METHODOLOGY EMPLOYED:
		// Currently can only check 0 and 1.  Need changes in CurveManager to be able to check minimums and
		// maximums.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using CurveManager::GetCurveType;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:
		IsValid = true;

		// Check 0 and 1
		if ( CurveValue( CurveIndex, 0.0 ) <= 0 ) IsValid = false;
		if ( CurveValue( CurveIndex, 1.0 ) <= 0 ) IsValid = false;

		if ( IsValid ) { // Check min/maxs

			{ auto const SELECT_CASE_var( GetCurveType( CurveIndex ) );

			if ( SELECT_CASE_var == "QUADRATIC" ) {
				// Curve coeffs are not currently exposed so there's no good way to do this yet

			} else if ( SELECT_CASE_var == "CUBIC" ) {

			}}

		}

	}

	void
	SetupStratifiedNodes( int const WaterThermalTankNum ) // Water Heater being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sets up node properties based on the tank shape, i.e., vertical cylinder, horizontal cylinder, or other.
		// Node height, skin area, vertical conduction area, and loss coefficients are calculated and assigned.
		// Heating elements, parasitics, and fluid inlet and outlet flows are assigned according to node height.

		// METHODOLOGY EMPLOYED:
		// Tank is divided into nodes of equal mass.  For horizontal cylinders, node heights are calculated using
		// the Newton-Raphson iterative method.  For vertical cylinders and other shapes, the node heights are calculated
		// using basic geometry.

		// Using/Aliasing
		using DataGlobals::Pi;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 Tolerance( 1.0e-8 ); // Tolerance for Newton-Raphson solution
		static Real64 FluidCond( 0.6 ); // Conductivity of water (W/m-K)
		static std::string const RoutineName( "GetWaterThermalTankInput" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumNodes; // Number of stratified nodes
		int NodeNum; // Node number index
		Real64 NodeMass; // Mass of one node (kg)
		Real64 EndArea; // Circular area of one end of the cylinder (m2)
		Real64 CrossArea; // Cross sectional area (for horizontal cylinders) (m2)
		Real64 NodeEndArea; // Area of the node at the end of the horizontal cylinder (m2)
		Real64 NodeHeight; // Height of one node (m)
		Real64 ApproxEndArea; // End area approximated by Newton-Raphson iteration (m2)
		Real64 CondCoeff; // Coefficient for vertical conduction between nodes (W/K)
		Real64 Radius; // Radius of the tank (m)
		Real64 Perimeter; // Perimeter of the tank (m)
		Real64 SkinArea; // Area of skin exposed to ambient environment (m2)
		Real64 ChordLength; // Chord length for horizontal tanks (m)
		Real64 TankHeight; // Dimension in the vertical direction; for horizontal tanks it is radius * 2 (m)
		Real64 TankLength; // For horizontal tanks, it is the length in the axial direction (m)
		Real64 R; // Radius (m)
		Real64 H0; // Starting height (m)
		Real64 H; // Ending height (m)
		Real64 a; // Intermediate variables
		Real64 b;
		Real64 c;
		Real64 a0; // Intermediate variables
		Real64 b0;
		Real64 c0;
		Real64 G; // Function that should converge to zero for the Newton-Raphson solution
		Real64 rho; // local fluid density (kg/m3)
		static int DummyWaterIndex( 1 );

		// FLOW:
		NumNodes = WaterThermalTank( WaterThermalTankNum ).Nodes;
		WaterThermalTank( WaterThermalTankNum ).Node.allocate( NumNodes );

		if ( ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum > 0 ) && allocated( PlantLoop ) ) {
			rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
		} else {
			rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
		}

		NodeMass = WaterThermalTank( WaterThermalTankNum ).Volume * rho / NumNodes;

		// Mixing rate set to 50% of the max value for dt = 1.0
		WaterThermalTank( WaterThermalTankNum ).InversionMixingRate = NodeMass * 0.5 * 1.0;

		if ( ( WaterThermalTank( WaterThermalTankNum ).Shape == TankShapeVertCylinder ) || ( WaterThermalTank( WaterThermalTankNum ).Shape == TankShapeOther ) ) {

			TankHeight = WaterThermalTank( WaterThermalTankNum ).Height;
			EndArea = WaterThermalTank( WaterThermalTankNum ).Volume / TankHeight;
			NodeHeight = TankHeight / NumNodes;
			CondCoeff = ( FluidCond + WaterThermalTank( WaterThermalTankNum ).AdditionalCond ) * EndArea / NodeHeight;

			if ( WaterThermalTank( WaterThermalTankNum ).Shape == TankShapeVertCylinder ) {
				Radius = std::sqrt( EndArea / Pi );
				Perimeter = 2.0 * Pi * Radius;
			} else { // TankShapeOther
				Perimeter = WaterThermalTank( WaterThermalTankNum ).Perimeter;
			}

			for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Mass = NodeMass;
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Volume = WaterThermalTank( WaterThermalTankNum ).Volume / NumNodes;
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Height = NodeHeight;
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).CondCoeffUp = CondCoeff;
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).CondCoeffDn = CondCoeff;

				if ( ( NodeNum == 1 ) || ( NodeNum == NumNodes ) ) {
					SkinArea = Perimeter * NodeHeight + EndArea;
				} else {
					SkinArea = Perimeter * NodeHeight;
				}

				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OnCycLossCoeff = WaterThermalTank( WaterThermalTankNum ).SkinLossCoeff * SkinArea + WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff( NodeNum );

				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OffCycLossCoeff = WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OnCycLossCoeff + WaterThermalTank( WaterThermalTankNum ).OffCycFlueLossCoeff;

			} // NodeNum

			WaterThermalTank( WaterThermalTankNum ).Node( 1 ).CondCoeffUp = 0.0;
			WaterThermalTank( WaterThermalTankNum ).Node( NumNodes ).CondCoeffDn = 0.0;

		} else { // WaterThermalTank(WaterThermalTankNum)%Shape == TankShapeHorizCylinder
			TankLength = WaterThermalTank( WaterThermalTankNum ).Height; // Height is the length in the axial direction
			EndArea = WaterThermalTank( WaterThermalTankNum ).Volume / TankLength;
			Radius = std::sqrt( EndArea / Pi );
			TankHeight = 2.0 * Radius; // Actual vertical height
			NodeEndArea = EndArea / NumNodes;

			R = Radius;
			H0 = 0.0;
			ChordLength = 0.0;
			for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Mass = NodeMass;
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Volume = WaterThermalTank( WaterThermalTankNum ).Volume / NumNodes;

				if ( NodeNum == NumNodes ) {
					H = TankHeight;

				} else {
					// Use the Newton-Raphson method to solve the nonlinear algebraic equation for node height
					H = H0 + TankHeight / NumNodes; // Initial guess

					while ( true ) {
						a = std::sqrt( H );
						b = std::sqrt( 2.0 * R - H );
						c = 2.0 * R * R * std::atan( a / b ) - ( 2.0 * R * R - 3.0 * H * R + H * H ) * ( a / b );

						if ( H0 > 0.0 ) {
							a0 = std::sqrt( H0 );
							b0 = std::sqrt( 2.0 * R - H0 );
							c0 = 2.0 * R * R * std::atan( a0 / b0 ) - ( 2.0 * R * R - 3.0 * H0 * R + H0 * H0 ) * ( a0 / b0 );
						} else {
							c0 = 0.0;
						}

						ApproxEndArea = c - c0; // Area approximated by iteration
						G = ApproxEndArea - NodeEndArea; // G is the function that should converge to zero

						if ( std::abs( G ) < Tolerance ) {
							break; // Converged !!!
						} else {
							H -= G / ( 2.0 * a * b ); // Calculate next guess:  H = Hprev - G/G'
						}
					} // Newton-Raphson

				}

				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Height = H - H0;

				if ( NodeNum > 1 ) {
					CrossArea = 2.0 * ChordLength * TankLength; // Use old ChordLength from previous node
					CondCoeff = ( FluidCond + WaterThermalTank( WaterThermalTankNum ).AdditionalCond ) * CrossArea / ( 0.5 * ( H - H0 ) + 0.5 * WaterThermalTank( WaterThermalTankNum ).Node( NodeNum - 1 ).Height );
					WaterThermalTank( WaterThermalTankNum ).Node( NodeNum - 1 ).CondCoeffUp = CondCoeff; // Set for previous node
					WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).CondCoeffDn = CondCoeff; // Set for this node
				}

				ChordLength = std::sqrt( 2.0 * R * H - H * H ); // Calc new ChordLength to be used with next node

				Perimeter = 2.0 * R * ( std::acos( ( R - H ) / R ) - std::acos( ( R - H0 ) / R ) ); // Segments of circular perimeter
				SkinArea = Perimeter * TankLength + 2.0 * NodeEndArea;

				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OnCycLossCoeff = WaterThermalTank( WaterThermalTankNum ).SkinLossCoeff * SkinArea + WaterThermalTank( WaterThermalTankNum ).AdditionalLossCoeff( NodeNum );

				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OffCycLossCoeff = WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OnCycLossCoeff + WaterThermalTank( WaterThermalTankNum ).OffCycFlueLossCoeff;
				// Although it doesn't make much sense to have a flue in a horizontal tank, keep it in anyway

				H0 = H;
			} // NodeNum

			WaterThermalTank( WaterThermalTankNum ).Node( 1 ).CondCoeffUp = 0.0;
			WaterThermalTank( WaterThermalTankNum ).Node( NumNodes ).CondCoeffDn = 0.0;
		}

		// Loop through nodes again (from top to bottom this time) and assign heating elements, parasitics, flow inlets/outlets
		// according to their vertical heights in the tank.
		H0 = TankHeight;
		for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
			if ( NodeNum == NumNodes ) {
				H = -1.0; // Avoids rounding errors and ensures that anything at height 0.0 goes into the bottom node
			} else {
				H = H0 - WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Height;
			}

			// Assign heater elements to the nodes at the specified heights
			if ( ( WaterThermalTank( WaterThermalTankNum ).HeaterHeight1 <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).HeaterHeight1 > H ) ) {
				//       sensor node will not get set if user enters 0 for this heater capacity
				//       (WaterThermalTank(WaterThermalTankNum)%MaxCapacity > 0.0d0)) THEN
				WaterThermalTank( WaterThermalTankNum ).HeaterNode1 = NodeNum;
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).MaxCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity;
			}

			if ( ( WaterThermalTank( WaterThermalTankNum ).HeaterHeight2 <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).HeaterHeight2 > H ) ) {
				//       sensor node will not get set if user enters 0 for this heater capacity
				//      .AND. (WaterThermalTank(WaterThermalTankNum)%MaxCapacity2 > 0.0d0)) THEN
				WaterThermalTank( WaterThermalTankNum ).HeaterNode2 = NodeNum;

				if ( ( NodeNum == WaterThermalTank( WaterThermalTankNum ).HeaterNode1 ) && ( WaterThermalTank( WaterThermalTankNum ).ControlType == PrioritySimultaneous ) ) {
					WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).MaxCapacity += WaterThermalTank( WaterThermalTankNum ).MaxCapacity2;
				} else {
					WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).MaxCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity2;
				}
			}

			// Assign parasitic heat gains to the nodes at the specified heights
			if ( ( WaterThermalTank( WaterThermalTankNum ).OffCycParaHeight <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).OffCycParaHeight > H ) ) {
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OffCycParaLoad = WaterThermalTank( WaterThermalTankNum ).OffCycParaFracToTank * WaterThermalTank( WaterThermalTankNum ).OffCycParaLoad;
			}

			if ( ( WaterThermalTank( WaterThermalTankNum ).OnCycParaHeight <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).OnCycParaHeight > H ) ) {
				WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).OnCycParaLoad = WaterThermalTank( WaterThermalTankNum ).OnCycParaFracToTank * WaterThermalTank( WaterThermalTankNum ).OnCycParaLoad;
			}

			// Assign inlets and outlets to the nodes at the specified heights
			if ( ( WaterThermalTank( WaterThermalTankNum ).UseInletHeight <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).UseInletHeight > H ) ) {
				WaterThermalTank( WaterThermalTankNum ).UseInletStratNode = NodeNum;

				if ( ( WaterThermalTank( WaterThermalTankNum ).UseInletNode > 0 ) || ( WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax > 0.0 ) ) ++WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Inlets;
			}

			if ( ( WaterThermalTank( WaterThermalTankNum ).UseOutletHeight <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).UseOutletHeight > H ) ) {
				WaterThermalTank( WaterThermalTankNum ).UseOutletStratNode = NodeNum;

				if ( ( WaterThermalTank( WaterThermalTankNum ).UseOutletNode > 0 ) || ( WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax > 0.0 ) ) ++WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Outlets;
			}

			if ( ( WaterThermalTank( WaterThermalTankNum ).SourceInletHeight <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).SourceInletHeight > H ) && ( WaterThermalTank( WaterThermalTankNum ).SourceInletNode > 0 ) ) {

				WaterThermalTank( WaterThermalTankNum ).SourceInletStratNode = NodeNum;
				++WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Inlets;
			}

			if ( ( WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight <= H0 ) && ( WaterThermalTank( WaterThermalTankNum ).SourceOutletHeight > H ) && ( WaterThermalTank( WaterThermalTankNum ).SourceOutletNode > 0 ) ) {

				WaterThermalTank( WaterThermalTankNum ).SourceOutletStratNode = NodeNum;
				++WaterThermalTank( WaterThermalTankNum ).Node( NodeNum ).Outlets;
			}

			H0 = H;
		} // NodeNum

	}

	void
	InitWaterThermalTank(
		int const WaterThermalTankNum,
		bool const FirstHVACIteration,
		Optional_int_const EP_UNUSED( LoopNum ),
		Optional_int_const EP_UNUSED( LoopSideNum )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   February 2004
		//       MODIFIED       FSEC, July 2005
		//                      Brent Griffith, October 2007 indirect fired water heater
		//						B. Shen 12/2014, add air-source variable-speed heat pump water heating
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize the water heater, heat pump water heater, or desuperheater heating coil objects during the simulation.
		// determine flow rates thru use side and source side plant connections (if any)

		// METHODOLOGY EMPLOYED:
		// Inlet and outlet nodes are initialized.  Scheduled values are retrieved for the current timestep.

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::WarmupFlag;
		using DataGlobals::AnyPlantInModel;
		using DataLoopNode::Node;
		using DataEnvironment::WaterMainsTemp;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalFanSys::MAT;
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using DataHVACGlobals::HPWHInletDBTemp;
		using DataHVACGlobals::HPWHInletWBTemp;
		using DataHVACGlobals::HPWHCrankcaseDBTemp;
		using DataSizing::AutoSize;
		using DataSizing::CurZoneEqNum;
		using DataSizing::ZoneEqSizing;
		using DataSizing::DataNonZoneNonAirloopValue;
		using InputProcessor::SameString;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using namespace DataPlant;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using FluidProperties::GetDensityGlycol;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;
		using Fans::GetFanVolFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UseInletNode; // Water heater use inlet node number
		int UseOutletNode; // Water heater use outlet node number
		int SourceInletNode; // Water heater source inlet node number
		int SourceOutletNode; // Water heater source outlet node number
		int SchIndex; // Index to schedule
		int HPNum; // Index to heat pump
		int HPAirInletNode; // HP air inlet node number
		int HPAirOutletNode; // HP air outlet node number
		int OutdoorAirNode; // Outdoor air inlet node number
		int ExhaustAirNode; // Exhaust air outlet node number
		int HPWaterInletNode; // HP condenser water inlet node number
		int HPWaterOutletNode; // HP condenser water outlet node number
		int InletAirMixerNode; // HP inlet node number after inlet mixing damper
		int OutletAirSplitterNode; // HP outlet node number before outlet mixing damper
		Real64 HPInletDryBulbTemp( 0.0 ); // HPWH's air inlet dry-bulb temperature, C
		Real64 HPInletHumRat( 0.0 ); // HPWH's air inlet humidity ratio, kg/kg
		Real64 HPInletRelHum; // HPWH's air inlet relative humidity
		Real64 DeadBandTemp; // Minimum tank temperature (SetPointTemp - DeadBandDeltaTemp) (C)
		Real64 MulSpeedFlowScale; // scaling factor for adjusting flow rates of VS HPWH coil
		Real64 rhoAir; // air density
		int Iter; // iteration number
		Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); //place holder to calling function
		Real64 FanVolFlow(0.0); // Used for error checking fans used with HPWHs
		//  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .FALSE.  ! True after the Zone Equipment List has been checked for items
		//  Integer             :: Loop
		static bool InitWaterThermalTanksOnce( true ); // flag for 1 time initialization
		static Array1D_bool MyEnvrnFlag; // flag for init once at start of environment
		static Array1D_bool MyWarmupFlag; // flag for init after warmup complete
		static Array1D_bool SetLoopIndexFlag; // get loop number flag
//		static Array1D_bool MySizingDoneFlag; // true if sizing is finished

		static std::string const RoutineName( "InitWaterThermalTank" );
		static std::string const GetWaterThermalTankInput( "GetWaterThermalTankInput" );
		static std::string const SizeTankForDemand( "SizeTankForDemandSide" );

		Real64 sensedTemp;
		int tmpNodeNum;
		Real64 mdotUse; // local temporary for use side mass flow
		Real64 mdotSource; // local temporary for source side mass flow
		bool errFlag;
		Real64 rho; // local fluid density
		static int DummyWaterIndex( 1 );
		static Real64 TankChangeRateScale( 0.0 ); // local temporary for nominal tank change rate
		static Real64 MaxSideVolFlow( 0.0 ); // local temporary for largest connection design flow

		// FLOW:

		if ( InitWaterThermalTanksOnce ) {
			MyEnvrnFlag.allocate( NumWaterThermalTank );
			MyWarmupFlag.allocate( NumWaterThermalTank );
			SetLoopIndexFlag.allocate( NumWaterThermalTank );
//			MySizingDoneFlag.allocate( NumWaterThermalTank );
			AlreadyRated.dimension( NumWaterThermalTank, false );
			MyEnvrnFlag = true;
			MyWarmupFlag = false;
			InitWaterThermalTanksOnce = false;
			SetLoopIndexFlag = true;

		}

		UseInletNode = WaterThermalTank( WaterThermalTankNum ).UseInletNode;
		UseOutletNode = WaterThermalTank( WaterThermalTankNum ).UseOutletNode;
		SourceInletNode = WaterThermalTank( WaterThermalTankNum ).SourceInletNode;
		SourceOutletNode = WaterThermalTank( WaterThermalTankNum ).SourceOutletNode;

		if ( SetLoopIndexFlag( WaterThermalTankNum ) && allocated( PlantLoop ) ) {

			if ( ( UseInletNode > 0 ) && ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum == 0 ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( WaterThermalTank( WaterThermalTankNum ).Name, WaterThermalTank( WaterThermalTankNum ).TypeNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).UseSidePlantBranchNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantCompNum, _, _, _, UseInletNode, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitWaterThermalTank: Program terminated due to previous condition(s)." );
				}
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, GetWaterThermalTankInput );
				WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * rho;
				WaterThermalTank( WaterThermalTankNum ).Mass = WaterThermalTank( WaterThermalTankNum ).Volume * rho;
				WaterThermalTank( WaterThermalTankNum ).UseSidePlantSizNum = PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).PlantSizNum;
				if ( ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantSizNum == 0 ) ) {
					ShowSevereError( "InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = " + WaterThermalTank( WaterThermalTankNum ).Name );
					ShowFatalError( "InitWaterThermalTank: Program terminated due to previous condition(s)." );
				}
			}
			if ( ( UseInletNode > 0 ) && ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) ) {
				// this is a heat pump water heater, need a separate block because TypeOf_HeatPumpWtrHeater shows up on Branch
				//  (input should probably have been the associated tank )
				errFlag = false;
				ScanPlantLoopsForObject( HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name, TypeOf_HeatPumpWtrHeater, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).UseSidePlantBranchNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantCompNum, _, _, _, UseInletNode, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitWaterThermalTank: Program terminated due to previous condition(s)." );
				}
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, GetWaterThermalTankInput );
				WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * rho;
				WaterThermalTank( WaterThermalTankNum ).Mass = WaterThermalTank( WaterThermalTankNum ).Volume * rho;
				WaterThermalTank( WaterThermalTankNum ).UseSidePlantSizNum = PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).PlantSizNum;
				if ( ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantSizNum == 0 ) ) {
					ShowSevereError( "InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = " + WaterThermalTank( WaterThermalTankNum ).Name );
					ShowFatalError( "InitWaterThermalTank: Program terminated due to previous condition(s)." );
				}
			}
			if ( ( SourceInletNode > 0 ) && ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum == 0 ) && ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum == 0 ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( WaterThermalTank( WaterThermalTankNum ).Name, WaterThermalTank( WaterThermalTankNum ).TypeNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantBranchNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantCompNum, _, _, _, SourceInletNode, _, errFlag );
				if ( UseInletNode > 0 ) {
					InterConnectTwoPlantLoopSides( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).TypeNum, true );
				}

				if ( errFlag ) {
					ShowFatalError( "InitWaterThermalTank: Program terminated due to previous condition(s)." );
				}
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, GetWaterThermalTankInput );
				WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate * rho;
				WaterThermalTank( WaterThermalTankNum ).SourceSidePlantSizNum = PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).PlantSizNum;
				if ( ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantSizNum == 0 ) ) {
					ShowSevereError( "InitWaterThermalTank: Did not find Sizing:Plant object for source side of plant thermal tank = " + WaterThermalTank( WaterThermalTankNum ).Name );
					ShowFatalError( "InitWaterThermalTank: Program terminated due to previous condition(s)." );
				}
			}
			if ( ( ( SourceInletNode > 0 ) && ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum > 0 ) ) || ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) ) {
				SetLoopIndexFlag( WaterThermalTankNum ) = false;
			}

			if ( PlantFirstSizesOkayToFinalize ) SetLoopIndexFlag( WaterThermalTankNum ) = false;
			if ( WaterThermalTank( WaterThermalTankNum ).StandAlone ) {
				SizeStandAloneWaterHeater( WaterThermalTankNum );
				SetLoopIndexFlag( WaterThermalTankNum ) = false;
			}

		} else if ( SetLoopIndexFlag( WaterThermalTankNum ) && ! AnyPlantInModel ) {
			if ( WaterThermalTank( WaterThermalTankNum ).StandAlone ) {
				SizeStandAloneWaterHeater( WaterThermalTankNum );
			}

			CalcStandardRatings( WaterThermalTankNum );
			SetLoopIndexFlag( WaterThermalTankNum ) = false;
		}

		if ( WaterThermalTank( WaterThermalTankNum ).StandAlone && ( ! AlreadyRated( WaterThermalTankNum ) ) ) {
			CalcStandardRatings( WaterThermalTankNum );
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag( WaterThermalTankNum ) && ! SetLoopIndexFlag( WaterThermalTankNum ) ) {

			if ( PlantFirstSizesOkayToFinalize ) {

				if ( WaterThermalTank( WaterThermalTankNum ).ControlType == ControlTypeCycle ) {
					WaterThermalTank( WaterThermalTankNum ).MinCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity;
				}

				// check for sizing issues that model can not suppport

				// if stratified tank model, ensure that nominal change over rate is greater than one minute, avoid numerical problems.

				if ( ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedWaterHeater ) || ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedChilledWaterStorage ) ) {
					MaxSideVolFlow = max( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate, WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );

					if ( MaxSideVolFlow > 0.0 ) { // protect div by zero
						TankChangeRateScale = WaterThermalTank( WaterThermalTankNum ).Volume / MaxSideVolFlow;
						if ( TankChangeRateScale < 60.0 ) { // nominal change over in less than one minute
							ShowSevereError( "InitWaterThermalTank: Detected problem for stratified tank model.  Model cannot be applied." );
							ShowContinueError( "Occurs for stratified tank name = " + WaterThermalTank( WaterThermalTankNum ).Name );
							ShowContinueError( "Tank volume = " + RoundSigDigits( WaterThermalTank( WaterThermalTankNum ).Volume, 4 ) + " [m3]" );
							ShowContinueError( "Tank use side volume flow rate = " + RoundSigDigits( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate, 4 ) + " [m3/s]" );
							ShowContinueError( "Tank source side volume flow rate = " + RoundSigDigits( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate, 4 ) + " [m3/s]" );
							ShowContinueError( "Nominal tank change over rate = " + RoundSigDigits( TankChangeRateScale, 2 ) + " [s]" );
							ShowContinueError( "Change over rate is too fast, increase tank volume, decrease connection flow rates or use mixed tank model" );

							ShowFatalError( "InitWaterThermalTank: Simulation halted because of sizing problem in stratified tank model." );
						}
					}
				}

			}

			// Clear node initial conditions
			if ( UseInletNode > 0 && UseOutletNode > 0 ) {
				Node( UseInletNode ).Temp = 0.0;
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, GetWaterThermalTankInput );
				WaterThermalTank( WaterThermalTankNum ).MassFlowRateMin = WaterThermalTank( WaterThermalTankNum ).VolFlowRateMin * rho;
				WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * rho;
				InitComponentNodes( WaterThermalTank( WaterThermalTankNum ).MassFlowRateMin, WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax, UseInletNode, UseOutletNode, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).UseSidePlantBranchNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantCompNum );
				WaterThermalTank( WaterThermalTankNum ).UseOutletTemp = 0.0;
				WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate = 0.0;
				WaterThermalTank( WaterThermalTankNum ).SavedUseOutletTemp = 0.0;

				WaterThermalTank( WaterThermalTankNum ).Mass = WaterThermalTank( WaterThermalTankNum ).Volume * rho;
				WaterThermalTank( WaterThermalTankNum ).UseBranchControlType = PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).LoopSide( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide ).Branch( WaterThermalTank( WaterThermalTankNum ).UseSidePlantBranchNum ).Comp( WaterThermalTank( WaterThermalTankNum ).UseSidePlantCompNum ).FlowCtrl;

			}

			if ( ( SourceInletNode > 0 ) && ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum == 0 ) && ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum == 0 ) ) {
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, GetWaterThermalTankInput );
				WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate * rho;
				InitComponentNodes( 0.0, WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax, SourceInletNode, SourceOutletNode, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantBranchNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantCompNum );

				WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp = 0.0;
				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = 0.0;
				WaterThermalTank( WaterThermalTankNum ).SavedSourceOutletTemp = 0.0;

				WaterThermalTank( WaterThermalTankNum ).SourceBranchControlType = PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).LoopSide( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide ).Branch( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantBranchNum ).Comp( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantCompNum ).FlowCtrl;
			}

			if ( ( SourceInletNode > 0 ) && ( ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum > 0 ) || ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) ) ) {
				Node( SourceInletNode ).Temp = 0.0;
				WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp = 0.0;
				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = 0.0;
				WaterThermalTank( WaterThermalTankNum ).SavedSourceOutletTemp = 0.0;
				rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, SizeTankForDemand );
				WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate * rho;
			}

			// Initialize tank temperature to setpoint of first hour of warm up period
			// (use HPWH or Desuperheater heating coil set point if applicable)
			if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {
				HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Mode = FloatMode;
				HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SaveMode = FloatMode;
				HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SaveWHMode = FloatMode;
				SchIndex = HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTempSchedule;
			} else if ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum > 0 ) {
				WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Mode = FloatMode;
				SchIndex = WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).SetPointTempSchedule;
			} else {
				SchIndex = WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule;
			}

			if ( SchIndex > 0 ) {
				WaterThermalTank( WaterThermalTankNum ).TankTemp = GetCurrentScheduleValue( SchIndex );
				WaterThermalTank( WaterThermalTankNum ).SavedTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;

				if ( WaterThermalTank( WaterThermalTankNum ).Nodes > 0 ) {
					WaterThermalTank( WaterThermalTankNum ).Node.Temp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
					WaterThermalTank( WaterThermalTankNum ).Node.SavedTemp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
				}
			} else {
				WaterThermalTank( WaterThermalTankNum ).TankTemp = 20.0;
				WaterThermalTank( WaterThermalTankNum ).SavedTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;

				if ( WaterThermalTank( WaterThermalTankNum ).Nodes > 0 ) {
					WaterThermalTank( WaterThermalTankNum ).Node.Temp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
					WaterThermalTank( WaterThermalTankNum ).Node.SavedTemp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
				}
			}
			WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).SavedSourceOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).UseOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).SavedUseOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).TankTempAvg = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;

			WaterThermalTank( WaterThermalTankNum ).SavedHeaterOn1 = false;
			WaterThermalTank( WaterThermalTankNum ).SavedHeaterOn2 = false;
			WaterThermalTank( WaterThermalTankNum ).Mode = 0;
			WaterThermalTank( WaterThermalTankNum ).SavedMode = 0;
			WaterThermalTank( WaterThermalTankNum ).FirstRecoveryDone = false;
			WaterThermalTank( WaterThermalTankNum ).FirstRecoveryFuel = 0.0;
			WaterThermalTank( WaterThermalTankNum ).UnmetEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).LossEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).FlueLossEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).UseEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).TotalDemandEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).SourceEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).HeaterEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).HeaterEnergy1 = 0.0;
			WaterThermalTank( WaterThermalTankNum ).HeaterEnergy2 = 0.0;
			WaterThermalTank( WaterThermalTankNum ).FuelEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).FuelEnergy1 = 0.0;
			WaterThermalTank( WaterThermalTankNum ).FuelEnergy2 = 0.0;
			WaterThermalTank( WaterThermalTankNum ).VentEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).OffCycParaEnergyToTank = 0.0;
			WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelEnergy = 0.0;
			WaterThermalTank( WaterThermalTankNum ).OnCycParaEnergyToTank = 0.0;
			WaterThermalTank( WaterThermalTankNum ).NetHeatTransferEnergy = 0.0;

		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( WaterThermalTankNum ) = true;

		if ( MyWarmupFlag( WaterThermalTankNum ) && ( ! WarmupFlag ) ) {
			// reInitialize tank temperature to setpoint of first hour (use HPWH or Desuperheater heating coil set point if applicable)
			// BG's interpetation here is that its better to reset initial condition to setpoint once warm up is over.
			// (otherwise with a dynamic storage model it is difficult for the user to see the initial performance if it isn't periodic.)
			if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {
				HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Mode = FloatMode;
				SchIndex = HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTempSchedule;
			} else if ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum > 0 ) {
				WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).Mode = FloatMode;
				SchIndex = WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).SetPointTempSchedule;
			} else {
				SchIndex = WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule;
			}

			if ( SchIndex > 0 ) {
				WaterThermalTank( WaterThermalTankNum ).TankTemp = GetCurrentScheduleValue( SchIndex );
				WaterThermalTank( WaterThermalTankNum ).SavedTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;

				if ( WaterThermalTank( WaterThermalTankNum ).Nodes > 0 ) {
					WaterThermalTank( WaterThermalTankNum ).Node.Temp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
					WaterThermalTank( WaterThermalTankNum ).Node.SavedTemp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
				}
			} else {
				WaterThermalTank( WaterThermalTankNum ).TankTemp = 20.0;
				WaterThermalTank( WaterThermalTankNum ).SavedTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;

				if ( WaterThermalTank( WaterThermalTankNum ).Nodes > 0 ) {
					WaterThermalTank( WaterThermalTankNum ).Node.Temp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
					WaterThermalTank( WaterThermalTankNum ).Node.SavedTemp() = WaterThermalTank( WaterThermalTankNum ).TankTemp;
				}
			}
			WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).SavedSourceOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).UseOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).SavedUseOutletTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
			WaterThermalTank( WaterThermalTankNum ).SavedHeaterOn1 = false;
			WaterThermalTank( WaterThermalTankNum ).SavedHeaterOn2 = false;
			WaterThermalTank( WaterThermalTankNum ).Mode = 0;
			WaterThermalTank( WaterThermalTankNum ).SavedMode = 0;
			MyWarmupFlag( WaterThermalTankNum ) = false;

		}
		if ( WarmupFlag ) MyWarmupFlag( WaterThermalTankNum ) = true;

		if ( FirstHVACIteration ) {
			// Get all scheduled values
			SchIndex = WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule;
			WaterThermalTank( WaterThermalTankNum ).SetPointTemp = GetCurrentScheduleValue( SchIndex );

			if ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
				if ( WaterThermalTank( WaterThermalTankNum ).SetPointTemp > WaterThermalTank( WaterThermalTankNum ).TankTempLimit ) {
					// Setpoint temperature scheduled higher than maximum tank temperature limit
					WaterThermalTank( WaterThermalTankNum ).SetPointTemp = WaterThermalTank( WaterThermalTankNum ).TankTempLimit - 1.0;

					if ( WaterThermalTank( WaterThermalTankNum ).ShowSetPointWarning ) {
						ShowSevereError( "Water heater = " + WaterThermalTank( WaterThermalTankNum ).Name + ":  Water heater tank set point temperature is greater than the maximum tank temperature limit." );
						ShowContinueErrorTimeStamp( "Water heater tank set point temperature is reset to Tank Temperature Limit minus 1 C (" + TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).SetPointTemp, 2 ) + ") and simulation continues." );
						WaterThermalTank( WaterThermalTankNum ).ShowSetPointWarning = false;
					}

				}
			} else {
				if ( WaterThermalTank( WaterThermalTankNum ).SetPointTemp < WaterThermalTank( WaterThermalTankNum ).TankTempLimit ) {
					// Setpoint temperature scheduled lower than minimum tank temperature limit
					WaterThermalTank( WaterThermalTankNum ).SetPointTemp = WaterThermalTank( WaterThermalTankNum ).TankTempLimit + 1.0;

					if ( WaterThermalTank( WaterThermalTankNum ).ShowSetPointWarning ) {
						ShowSevereError( "Chilled Water Tank = " + WaterThermalTank( WaterThermalTankNum ).Name + ":  Water heater tank set point temperature is lower than the minimum tank temperature limit." );
						ShowContinueErrorTimeStamp( "Chilled water tank set point temperature is reset to Tank Temperature Limit plus 1 C (" + TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).SetPointTemp, 2 ) + ") and simulation continues." );
						WaterThermalTank( WaterThermalTankNum ).ShowSetPointWarning = false;
					}

				}
			}

			SchIndex = WaterThermalTank( WaterThermalTankNum ).SetPointTempSchedule2;
			if ( SchIndex > 0 ) {
				WaterThermalTank( WaterThermalTankNum ).SetPointTemp2 = GetCurrentScheduleValue( SchIndex );
			}

			{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).AmbientTempIndicator );
			if ( SELECT_CASE_var == AmbientTempSchedule ) {
				SchIndex = WaterThermalTank( WaterThermalTankNum ).AmbientTempSchedule;
				WaterThermalTank( WaterThermalTankNum ).AmbientTemp = GetCurrentScheduleValue( SchIndex );

			} else if ( SELECT_CASE_var == AmbientTempZone ) {
				WaterThermalTank( WaterThermalTankNum ).AmbientTemp = MAT( WaterThermalTank( WaterThermalTankNum ).AmbientTempZone );

			} else if ( SELECT_CASE_var == AmbientTempOutsideAir ) {
				WaterThermalTank( WaterThermalTankNum ).AmbientTemp = Node( WaterThermalTank( WaterThermalTankNum ).AmbientTempOutsideAirNode ).Temp;

			}}

			if ( UseInletNode == 0 ) { // Stand-alone operation

				SchIndex = WaterThermalTank( WaterThermalTankNum ).UseInletTempSchedule;
				if ( SchIndex > 0 ) {
					WaterThermalTank( WaterThermalTankNum ).UseInletTemp = GetCurrentScheduleValue( SchIndex );
				} else {
					WaterThermalTank( WaterThermalTankNum ).UseInletTemp = WaterMainsTemp;
				}

				SchIndex = WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule;
				if ( SchIndex > 0 ) {
					WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate = GetCurrentScheduleValue( SchIndex ) * WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax;

					WaterThermalTank( WaterThermalTankNum ).VolFlowRate = WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate / RhoH2O( InitConvTemp );
				} else {
					WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate = WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax;
					WaterThermalTank( WaterThermalTankNum ).VolFlowRate = WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate / RhoH2O( InitConvTemp );
				}

			}

			if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {
				HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTemp = GetCurrentScheduleValue( HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTempSchedule );
				if ( HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTemp >= WaterThermalTank( WaterThermalTankNum ).TankTempLimit ) {
					// HP setpoint temperature scheduled equal to or higher than tank temperature limit
					HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTemp = WaterThermalTank( WaterThermalTankNum ).TankTempLimit - 1.0;

					if ( HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).ShowSetPointWarning ) {
						ShowSevereError( "Heat Pump Water Heater = " + HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name + ":  Heat Pump water heater set point temperature is equal to or greater than the maximum tank temperature limit." );
						ShowContinueErrorTimeStamp( "Heat Pump water heater tank set point temperature is reset to Tank Temperature Limit minus 1 C (" + TrimSigDigits( HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).SetPointTemp, 2 ) + ") and simulation continues." );
						HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).ShowSetPointWarning = false;
					}

				}
			}

			if ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum > 0 ) {
				WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).SetPointTemp = GetCurrentScheduleValue( WaterHeaterDesuperheater( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum ).SetPointTempSchedule );
			}

		} // first HVAC Iteration

		if ( UseInletNode > 0 && ! SetLoopIndexFlag( WaterThermalTankNum ) ) { // setup mass flows for plant connections

			if ( WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
				DeadBandTemp = WaterThermalTank( WaterThermalTankNum ).SetPointTemp + WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp;
			} else {
				DeadBandTemp = WaterThermalTank( WaterThermalTankNum ).SetPointTemp - WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp;
			}

			mdotUse = PlantMassFlowRatesFunc( WaterThermalTankNum, UseInletNode, FirstHVACIteration, UseSide, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).UseSideSeries, WaterThermalTank( WaterThermalTankNum ).UseBranchControlType, WaterThermalTank( WaterThermalTankNum ).SavedUseOutletTemp, DeadBandTemp, WaterThermalTank( WaterThermalTankNum ).SetPointTemp );
			SetComponentFlowRate( mdotUse, UseInletNode, UseOutletNode, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).UseSidePlantBranchNum, WaterThermalTank( WaterThermalTankNum ).UseSidePlantCompNum );

			WaterThermalTank( WaterThermalTankNum ).UseInletTemp = Node( UseInletNode ).Temp;
			WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate = mdotUse;

		}

		if ( SourceInletNode > 0 && ! SetLoopIndexFlag( WaterThermalTankNum ) ) { // setup mass flows for plant connections

			if ( WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
				DeadBandTemp = WaterThermalTank( WaterThermalTankNum ).SetPointTemp + WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp;
			} else {
				DeadBandTemp = WaterThermalTank( WaterThermalTankNum ).SetPointTemp - WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp;
			}

			if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedChilledWaterStorage ) {
				tmpNodeNum = WaterThermalTank( WaterThermalTankNum ).HeaterNode1;
				sensedTemp = WaterThermalTank( WaterThermalTankNum ).Node( tmpNodeNum ).SavedTemp;
			} else {
				sensedTemp = WaterThermalTank( WaterThermalTankNum ).SavedSourceOutletTemp;
			}

			mdotSource = PlantMassFlowRatesFunc( WaterThermalTankNum, SourceInletNode, FirstHVACIteration, SourceSide, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).SourceSideSeries, WaterThermalTank( WaterThermalTankNum ).SourceBranchControlType, sensedTemp, DeadBandTemp, WaterThermalTank( WaterThermalTankNum ).SetPointTemp );
			if ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum > 0 ) {
				SetComponentFlowRate( mdotSource, SourceInletNode, SourceOutletNode, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantBranchNum, WaterThermalTank( WaterThermalTankNum ).SourceSidePlantCompNum );
			} else { //not really plant connected (desuperheater or heat pump)
				Node( SourceInletNode ).MassFlowRate = mdotSource;
				Node( SourceOutletNode ).MassFlowRate = mdotSource;

			}

			WaterThermalTank( WaterThermalTankNum ).SourceInletTemp = Node( SourceInletNode ).Temp;
			WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = mdotSource;

		}

		// initialize HPWHs each iteration
		if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {

			HPNum = WaterThermalTank( WaterThermalTankNum ).HeatPumpNum;

			if ( MyHPSizeFlag( HPNum ) ) {
				//     autosize info must be calculated in GetWaterThermalTankInputFlag for use in StandardRating procedure
				//       (called at end of GetWaterThermalTankInputFlag)
				//     report autosizing information here (must be done after GetWaterThermalTankInputFlag is complete)
				if ( HPWaterHeater( HPNum ).WaterFlowRateAutoSized ) {
					ReportSizingOutput( HPWaterHeater( HPNum ).Type, HPWaterHeater( HPNum ).Name, "Condenser water flow rate [m3/s]", HPWaterHeater( HPNum ).OperatingWaterFlowRate );
				}
				if ( HPWaterHeater( HPNum ).AirFlowRateAutoSized ) {
					ReportSizingOutput( HPWaterHeater( HPNum ).Type, HPWaterHeater( HPNum ).Name, "Evaporator air flow rate [m3/s]", HPWaterHeater( HPNum ).OperatingAirFlowRate );
				}
				DataNonZoneNonAirloopValue = HPWaterHeater( HPNum ).OperatingAirFlowRate;
				if ( CurZoneEqNum > 0 ) {
					ZoneEqSizing( CurZoneEqNum ).CoolingAirFlow = true;
					ZoneEqSizing( CurZoneEqNum ).CoolingAirVolFlow = DataNonZoneNonAirloopValue;
				}
				MyHPSizeFlag( HPNum ) = false;
			}

			HPAirInletNode = HPWaterHeater( HPNum ).HeatPumpAirInletNode;
			HPAirOutletNode = HPWaterHeater( HPNum ).HeatPumpAirOutletNode;
			OutdoorAirNode = HPWaterHeater( HPNum ).OutsideAirNode;
			ExhaustAirNode = HPWaterHeater( HPNum ).ExhaustAirNode;
			HPWaterInletNode = HPWaterHeater( HPNum ).CondWaterInletNode;
			HPWaterOutletNode = HPWaterHeater( HPNum ).CondWaterOutletNode;
			InletAirMixerNode = HPWaterHeater( HPNum ).InletAirMixerNode;
			OutletAirSplitterNode = HPWaterHeater( HPNum ).OutletAirSplitterNode;

			{ auto const SELECT_CASE_var( HPWaterHeater( HPNum ).CrankcaseTempIndicator );
			if ( SELECT_CASE_var == CrankcaseTempZone ) {
				HPWHCrankcaseDBTemp = MAT( HPWaterHeater( HPNum ).AmbientTempZone );
			} else if ( SELECT_CASE_var == CrankcaseTempExterior ) {
				HPWHCrankcaseDBTemp = OutDryBulbTemp;
			} else if ( SELECT_CASE_var == CrankcaseTempSchedule ) {
				HPWHCrankcaseDBTemp = GetCurrentScheduleValue( HPWaterHeater( HPNum ).CrankcaseTempSchedule );
			}}

			//   initialize HPWH report variables to 0 and set tank inlet node equal to outlet node
			HPWaterHeater( HPNum ).HPWaterHeaterSensibleCapacity = 0.0;
			HPWaterHeater( HPNum ).HPWaterHeaterLatentCapacity = 0.0;
			WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = 0.0;
			HPWaterHeater( HPNum ).HeatingPLR = 0.0;
			WaterThermalTank( WaterThermalTankNum ).SourceInletTemp = WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp;

			//   determine HPWH inlet air conditions based on inlet air configuration (Zone, ZoneAndOA, OutdoorAir, or Schedule)
			{ auto const SELECT_CASE_var( HPWaterHeater( HPNum ).InletAirConfiguration );
			if ( SELECT_CASE_var == AmbientTempZone ) {
				MixerInletAirSchedule = 0.0;
				HPInletDryBulbTemp = Node( HPAirInletNode ).Temp;
				HPInletHumRat = Node( HPAirInletNode ).HumRat;
			} else if ( SELECT_CASE_var == AmbientTempZoneAndOA ) {
				if ( HPWaterHeater( HPNum ).InletAirMixerSchPtr > 0 ) {
					//         schedule values are checked for boundary of 0 and 1 in GetWaterThermalTankInputFlag
					MixerInletAirSchedule = GetCurrentScheduleValue( HPWaterHeater( HPNum ).InletAirMixerSchPtr );
				} else {
					MixerInletAirSchedule = 0.0;
				}
				HPInletDryBulbTemp = MixerInletAirSchedule * Node( OutdoorAirNode ).Temp + ( 1.0 - MixerInletAirSchedule ) * Node( HPAirInletNode ).Temp;
				HPInletHumRat = MixerInletAirSchedule * Node( OutdoorAirNode ).HumRat + ( 1.0 - MixerInletAirSchedule ) * Node( HPAirInletNode ).HumRat;
			} else if ( SELECT_CASE_var == AmbientTempOutsideAir ) {
				MixerInletAirSchedule = 1.0;
				HPInletDryBulbTemp = Node( OutdoorAirNode ).Temp;
				HPInletHumRat = Node( OutdoorAirNode ).HumRat;

			} else if ( SELECT_CASE_var == AmbientTempSchedule ) {
				HPInletDryBulbTemp = GetCurrentScheduleValue( HPWaterHeater( HPNum ).AmbientTempSchedule );
				HPInletRelHum = GetCurrentScheduleValue( HPWaterHeater( HPNum ).AmbientRHSchedule );
				HPInletHumRat = PsyWFnTdbRhPb( HPInletDryBulbTemp, HPInletRelHum, OutBaroPress, RoutineName );
				Node( HPAirInletNode ).Temp = HPInletDryBulbTemp;
				Node( HPAirInletNode ).HumRat = HPInletHumRat;
				Node( HPAirInletNode ).Enthalpy = PsyHFnTdbW( HPInletDryBulbTemp, HPInletHumRat );
				Node( HPAirInletNode ).Press = OutBaroPress;

			} else {
				assert( false );
			}}

			MdotAir = HPWaterHeater( HPNum ).OperatingAirFlowRate * PsyRhoAirFnPbTdbW( OutBaroPress, HPInletDryBulbTemp, HPInletHumRat );

			//   set up initial conditions on nodes
			if ( InletAirMixerNode > 0 ) {
				Node( InletAirMixerNode ).MassFlowRate = 0.0;
				Node( InletAirMixerNode ).MassFlowRateMax = MdotAir;
				Node( InletAirMixerNode ).MassFlowRateMaxAvail = MdotAir;
				Node( InletAirMixerNode ).Temp = HPInletDryBulbTemp;
				Node( InletAirMixerNode ).HumRat = HPInletHumRat;
				Node( InletAirMixerNode ).Enthalpy = PsyHFnTdbW( HPInletDryBulbTemp, HPInletHumRat );
				Node( HPAirInletNode ).MassFlowRate = 0.0;
				Node( HPAirOutletNode ).MassFlowRate = 0.0;
				Node( OutdoorAirNode ).MassFlowRate = 0.0;
				Node( ExhaustAirNode ).MassFlowRate = 0.0;
			} else {
				if ( OutdoorAirNode == 0 ) {
					Node( HPAirInletNode ).MassFlowRate = 0.0;
					Node( HPAirInletNode ).MassFlowRateMax = MdotAir;
					Node( HPAirInletNode ).MassFlowRateMaxAvail = MdotAir;
					Node( HPAirOutletNode ).MassFlowRate = 0.0;
				} else {
					Node( OutdoorAirNode ).MassFlowRate = 0.0;
					Node( OutdoorAirNode ).MassFlowRateMax = MdotAir;
					Node( OutdoorAirNode ).MassFlowRateMaxAvail = MdotAir;
					Node( ExhaustAirNode ).MassFlowRate = 0.0;
				}
			}

			if ( OutletAirSplitterNode > 0 ) Node( OutletAirSplitterNode ).MassFlowRate = 0.0;
			//these are water nodes are not managed by plant. the HP connects
			// directly to the WH without using plant. will not change this code for DSU because of this
			Node( HPWaterInletNode ).MassFlowRate = 0.0;
			Node( HPWaterOutletNode ).MassFlowRate = 0.0;

			//   set the max mass flow rate for outdoor fans
			Node( HPWaterHeater( HPNum ).FanOutletNode ).MassFlowRateMax = MdotAir;

			//   Curve objects in CalcHPWHDXCoil will use inlet conditions to HPWH not inlet air conditions to DX Coil
			//   HPWHInletDBTemp and HPWHInletWBTemp are DataHVACGlobals to pass info to HPWHDXCoil
			HPWHInletDBTemp = HPInletDryBulbTemp;
			HPWHInletWBTemp = PsyTwbFnTdbWPb( HPWHInletDBTemp, HPInletHumRat, OutBaroPress );

			// initialize flow rates at speed levels for varaible-speed HPWH
			if (SameString(HPWaterHeater(HPNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed") &&
				(0 == HPWaterHeater(HPNum).NumofSpeed))
			{
				EMP1 = 4.0;
				EMP2 = 0.0;
				EMP3 = 0.0;
				SimVariableSpeedCoils(BlankString, HPWaterHeater(HPNum).DXCoilNum,
					0, EMP1, EMP2, EMP3, 0, 0.0, 1, 0.0, 0.0, 0.0, 0.0); //conduct the sizing operation in the VS WSHP
				HPWaterHeater(HPNum).NumofSpeed = VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NumOfSpeeds;
				// below pass the flow rates from the VS coil to the water heater object

				// scale air flow rates
				MulSpeedFlowScale = VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).RatedAirVolFlowRate /
					VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).MSRatedAirVolFlowRate(VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel);
				for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
					HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) = VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale;
				}

				// check fan flow rate, should be larger than the max flow rate of the VS coil
				GetFanVolFlow(HPWaterHeater(HPNum).FanNum, FanVolFlow);
				if (FanVolFlow  < HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed)) {
					ShowWarningError("InitWaterThermalTank: -air flow rate = " + TrimSigDigits(FanVolFlow, 7) +
						" in fan object " " is less than the MSHP system air flow rate" " when waterheating is required("
						+ TrimSigDigits(HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed), 7) + ").");
					ShowContinueError(" The MSHP system flow rate when waterheating is required is reset to the" " fan flow rate and the simulation continues.");
					ShowContinueError(" Occurs in " + HPWaterHeater(HPNum).Name);
					HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed) = FanVolFlow;
					// Check flow rates in other speeds and ensure flow rates are not above the max flow rate
					for (Iter = HPWaterHeater(HPNum).NumofSpeed - 1; Iter >= 1; --Iter) {
						if (HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) > HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1)) {
							ShowContinueError(" The MSHP system flow rate when waterheating is required is reset to the" " flow rate at higher speed and the simulation continues at Speed"
								+ TrimSigDigits(Iter) + '.');
							ShowContinueError(" Occurs in " + HPWaterHeater(HPNum).Name);
							HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) = HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1);
						}
					}
				}

				for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
					HPWaterHeater(HPNum).MSAirSpeedRatio(Iter) = HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) /
						HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed);
				}

				// scale water flow rates
				MulSpeedFlowScale = VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).RatedWaterVolFlowRate /
					VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).MSRatedWaterVolFlowRate
					(VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel);
				for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
					HPWaterHeater(HPNum).HPWHWaterVolFlowRate(Iter) = VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).MSRatedWaterVolFlowRate(Iter) * MulSpeedFlowScale;
					HPWaterHeater(HPNum).HPWHWaterMassFlowRate(Iter) = VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).MSRatedWaterMassFlowRate(Iter) * MulSpeedFlowScale;
					HPWaterHeater(HPNum).MSWaterSpeedRatio(Iter) = VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).MSRatedWaterVolFlowRate(Iter) /
						VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).MSRatedWaterVolFlowRate(HPWaterHeater(HPNum).NumofSpeed);
				}
			}

			if (HPWaterHeater(HPNum).NumofSpeed > 0) {
				rhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, HPInletDryBulbTemp, HPInletHumRat);

				for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
					HPWaterHeater(HPNum).HPWHAirMassFlowRate(Iter) =
						HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) * rhoAir;
				}

				//   set the max mass flow rate for outdoor fans
				Node(HPWaterHeater(HPNum).FanOutletNode).MassFlowRateMax =
					HPWaterHeater(HPNum).HPWHAirMassFlowRate(HPWaterHeater(HPNum).NumofSpeed);
			}

		} //  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN

	}

	void
	CalcWaterThermalTankMixed( int const WaterThermalTankNum ) // Water Heater being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a well-mixed, single node water heater tank.

		// METHODOLOGY EMPLOYED:
		// This model uses analytical calculations based on the differential equation describing the tank energy
		// balance.  The model operates in three different modes:  heating, floating, and venting.  Temperatures and
		// energies change dynamically over the timestep.  The final reported tank temperature is the average over
		// the timestep.  The final reported heat rates are averages based on the total energy transfer over the
		// timestep.

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::WarmupFlag;
		using DataGlobals::HourOfDay;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcWaterThermalTankMixed" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		Real64 SetPointTemp; // Current setpoint temperature (C)
		Real64 DeadBandTemp; // Heating: Minimum tank temperature (SetPointTemp - DeadBandDeltaTemp) (C)
		// Cooling: Maximum Tank temperature (SetPointTemp + DeadBandDeltaTemp) (C)
		Real64 MaxTemp; // Maximum tank temperature before venting (C)
		Real64 AmbientTemp; // Current ambient air temperature around tank (C)
		Real64 TankMass; // Mass of water in tank (kg)
		Real64 LossCoeff; // Loss coefficient to ambient environment (W/K)
		Real64 LossFracToZone; // Fraction of losses added to the zone as a gain
		Real64 TankTemp; // Instantaneous tank temperature (C)
		Real64 NewTankTemp; // Predicted new tank temperature (C)
		Real64 TankTempAvg; // Average tank temperature over the timestep (C)
		Real64 Cp; // Specific heat of water (J/kg K)
		Real64 Quse; // Heating rate due to use side mass flow (W)
		Real64 Qsource; // Heating rate due to source side mass flow (W)
		Real64 Qloss; // Heating rate due to ambient environment (W)
		Real64 Qlosszone; // Heating rate of fraction of losses added to the zone as a gain (W)
		Real64 Qheat; // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
		Real64 Qheater; // Heating rate of the burner or electric heating element (W)
		Real64 Qheatpump; // Heating rate of the heat pump (W)
		Real64 Qmaxcap; // Maximum capacity heating rate of the burner or electric heating element (W)
		Real64 Qmincap; // Minimum capacity heating rate of the burner or electric heating element (W)
		Real64 Qoffcycfuel; // Fuel consumption rate of off-cycle parasitics (W)
		Real64 Qoffcycheat; // Heating rate of fraction of off-cycle parasitics added to the tank (W)
		Real64 Qoncycfuel; // Fuel consumption rate on-cycle parasitics added to the tank (W)
		Real64 Qoncycheat; // Heating rate of fraction of on-cycle parasitics added to the tank (W)
		Real64 Qneeded; // Heating rate needed to recover or maintain the setpoint temperature (W)
		Real64 Qunmet; // The difference between Qneeded and Qheater (W)
		Real64 Qvent; // Heating rate due to venting because tank exceeded max temperature limit (W)
		Real64 Qnet; // Net heat transfer rate including everything (W)
		Real64 Qfuel; // Heating rate for fuel consumed (W)
		Real64 UseInletTemp; // Use side inlet temperature (C)
		Real64 UseMassFlowRate; // Use side flow rate, including effectiveness factor (kg/s)
		Real64 MinMassFlowRate; // Minimum use side flow rate required before heater is enabled (kg/s)
		Real64 SourceInletTemp; // Source side inlet temperature (C)
		Real64 SourceMassFlowRate; // Source side flow rate, including effectiveness factor (kg/s)
		int Mode; // Indicator for current operating mode (HeatMode=1 | FloatMode=0 | VentMode=-1)
		Real64 SecInTimeStep; // Seconds in one timestep (s)
		Real64 TimeRemaining; // Time remaining in the current timestep (s)
		Real64 TimeNeeded; // Time needed to reach the next substep (s)
		int CycleOnCount; // Number of times heater cycles on in the current time step
		int MaxCycles; // Maximum number of cycles allowed before exiting loop
		Real64 Runtime; // Time that heater is running (s)
		Real64 RTF; // Runtime fraction, fraction of timestep that heater is running
		Real64 PLR; // Part load ratio, fraction of maximum heater capacity
		Real64 PLRsum; // Integrated part load ratio over the timestep (J)
		Real64 PLF; // Part load factor, modifies thermal efficiency to get total energy efficiency
		Real64 Tsum; // Integrated tank temp over the timestep, dividing by time gives the average (C s)
		Real64 deltaTsum; // Change in integrated tank temperature, dividing by time gives the average (C s)
		Real64 Eloss; // Energy change due to ambient losses over the timestep (J)
		Real64 Elosszone; // Energy change to the zone due to ambient losses over the timestep (J)
		Real64 Euse; // Energy change due to use side mass flow over the timestep (J)
		Real64 Esource; // Energy change due to source side mass flow over the timestep (J)
		Real64 Eheater; // Energy change due to the heater over the timestep (J)
		Real64 Eoncycfuel; // Fuel energy consumed by on-cycle parasitics over the timestep (J)
		Real64 Eoffcycfuel; // Fuel energy consumed by off-cycle parasitics over the timestep (J)
		Real64 Event; // Energy change due to venting over the timestep (J)
		Real64 Eneeded; // Energy change needed over the timestep (J)
		Real64 Eunmet; // Energy change unmet over the timestep (J)
		Real64 Efuel; // Energy change for fuel consumed over the timestep (J)
		bool SetPointRecovered; // Flag to indicate when setpoint is recovered for the first time
		Real64 rho;
		Real64 HPWHCondenserDeltaT; // Temperature difference across the condenser for a heat pump water heater
		static int DummyWaterIndex( 1 );

		// Reference to objects
		WaterThermalTankData & Tank = WaterThermalTank( WaterThermalTankNum ); // Reference to the tank object to save typing

		// FLOW:
		TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

		if ( Tank.TimeElapsed != TimeElapsed ) {
			// The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
			// timestep for use as the initial conditions of each iteration that does not advance the system timestep.
			Tank.SavedTankTemp = Tank.TankTemp;
			Tank.SavedMode = Tank.Mode;

			// Save outlet temperatures for demand-side flow control
			Tank.SavedUseOutletTemp = Tank.UseOutletTemp;
			Tank.SavedSourceOutletTemp = Tank.SourceOutletTemp;

			Tank.TimeElapsed = TimeElapsed;
		}

		TankTemp = Tank.SavedTankTemp;
		Mode = Tank.SavedMode;

		Qmaxcap = Tank.MaxCapacity;
		Qmincap = Tank.MinCapacity;
		Qoffcycfuel = Tank.OffCycParaLoad;
		Qoffcycheat = Qoffcycfuel * Tank.OffCycParaFracToTank;
		Qoncycfuel = Tank.OnCycParaLoad;
		Qoncycheat = Qoncycfuel * Tank.OnCycParaFracToTank;

		SetPointTemp = Tank.SetPointTemp;
		DeadBandTemp = Tank.getDeadBandTemp();
		MaxTemp = Tank.TankTempLimit;
		AmbientTemp = Tank.AmbientTemp;

		UseInletTemp = Tank.UseInletTemp;
		UseMassFlowRate = Tank.UseMassFlowRate * Tank.UseEffectiveness;
		MinMassFlowRate = Tank.MassFlowRateMin;
		SourceInletTemp = Tank.SourceInletTemp;
		SourceMassFlowRate = Tank.SourceMassFlowRate * Tank.SourceEffectiveness;

		if ( Tank.UseSidePlantLoopNum > 0 ) {
			rho = GetDensityGlycol( PlantLoop( Tank.UseSidePlantLoopNum ).FluidName, TankTemp, PlantLoop( Tank.UseSidePlantLoopNum ).FluidIndex, RoutineName );
		} else {
			rho = GetDensityGlycol( fluidNameWater, TankTemp, DummyWaterIndex, RoutineName );
		}

		TankMass = rho * Tank.Volume;

		if ( Tank.UseSidePlantLoopNum > 0 ) {
			Cp = GetSpecificHeatGlycol( PlantLoop( Tank.UseSidePlantLoopNum ).FluidName, TankTemp, PlantLoop( Tank.UseSidePlantLoopNum ).FluidIndex, RoutineName );
		} else {
			Cp = GetSpecificHeatGlycol( fluidNameWater, TankTemp, DummyWaterIndex, RoutineName );
		}

		SecInTimeStep = TimeStepSys * SecInHour;
		TimeRemaining = SecInTimeStep;
		TimeNeeded = 0.0;
		CycleOnCount = 0;
		MaxCycles = SecInTimeStep;
		Runtime = 0.0;
		SetPointRecovered = false;

		Tsum = 0.0;
		Eloss = 0.0;
		Elosszone = 0.0;
		Euse = 0.0;
		Esource = 0.0;
		Eheater = 0.0;
		Event = 0.0;
		Eneeded = 0.0;
		Eunmet = 0.0;
		Efuel = 0.0;
		Eoncycfuel = 0.0;
		Eoffcycfuel = 0.0;
		PLR = 0.0;
		PLRsum = 0.0;

		Qheat = 0.0;
		Qheater = 0.0;
		Qvent = 0.0;
		Qneeded = 0.0;
		Qunmet = 0.0;
		Qnet = 0.0;
		Qfuel = 0.0;

		// Calculate the heating rate from the heat pump.
		if ( Tank.HeatPumpNum > 0 ) {
			HeatPumpWaterHeaterData const & HeatPump = HPWaterHeater(Tank.HeatPumpNum);
			DataLoopNode::NodeData const & HPWHCondWaterInletNode = DataLoopNode::Node(HeatPump.CondWaterInletNode);
			DataLoopNode::NodeData const & HPWHCondWaterOutletNode = DataLoopNode::Node(HeatPump.CondWaterOutletNode);
			HPWHCondenserDeltaT = HPWHCondWaterOutletNode.Temp - HPWHCondWaterInletNode.Temp;
		} else {
			HPWHCondenserDeltaT = 0.0;
		}
		assert( HPWHCondenserDeltaT >= 0 );

		CalcMixedTankSourceSideHeatTransferRate(HPWHCondenserDeltaT, SourceInletTemp, Cp, SetPointTemp,
												SourceMassFlowRate, Qheatpump, Qsource);

		// Calculate steady-state use heat rate.
		Quse = UseMassFlowRate * Cp * ( UseInletTemp - SetPointTemp );

		while ( TimeRemaining > 0.0 ) {

			TimeNeeded = 0.0;

			NewTankTemp = TankTemp;

			{ auto const SELECT_CASE_var( Mode );

			if ( SELECT_CASE_var == HeatMode ) { // Heater is on

				// Calculate heat rate needed to maintain the setpoint at steady-state conditions
				LossCoeff = Tank.OnCycLossCoeff;
				LossFracToZone = Tank.OnCycLossFracToZone;
				Qloss = LossCoeff * ( AmbientTemp - SetPointTemp );
				Qneeded = -Quse - Qsource - Qloss - Qoncycheat;

				if ( TankTemp > SetPointTemp ) {
					// Heater is not needed after all, possibly due to step change in scheduled SetPointTemp

					Qheater = 0.0;
					Qunmet = 0.0;
					Mode = FloatMode;
					continue;

				} else if ( TankTemp < SetPointTemp ) {
					// Attempt to recover to the setpoint as quickly as possible by using maximum heater capacity

					// Qneeded is calculated above
					// Qneeded does not account for the extra energy needed to recover to the setpoint
					Qheater = Qmaxcap;
					Qunmet = max( Qneeded - Qheater, 0.0 );
					Qheat = Qoncycheat + Qheater + Qheatpump;

					// Calculate time needed to recover to the setpoint at maximum heater capacity
					TimeNeeded = CalcTimeNeeded( TankTemp, SetPointTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat );

					if ( TimeNeeded > TimeRemaining ) {
						// Heater is at maximum capacity and heats for all of the remaining time
						// Setpoint temperature WILL NOT be recovered

						TimeNeeded = TimeRemaining;

						NewTankTemp = CalcTankTemp( TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeNeeded );

					} else { // TimeNeeded <= TimeRemaining
						// Heater is at maximum capacity but will not heat for all of the remaining time (at maximum anyway)
						// Setpoint temperature WILL be recovered

						NewTankTemp = SetPointTemp;

						SetPointRecovered = true;

					} // TimeNeeded > TimeRemaining

				} else { // TankTemp == SetPointTemp
					// Attempt to maintain the setpoint by using the needed heater capacity (modulating, if allowed)

					if ( Qneeded <= 0.0 ) {
						// Heater is not needed

						Qneeded = 0.0;
						Qheater = 0.0;
						Qunmet = 0.0;
						Mode = FloatMode;
						continue;

					} else if ( Qneeded < Qmincap ) {
						// Heater is required at less than the minimum capacity
						// If cycling, Qmincap = Qmaxcap.  Once the setpoint is reached, heater will almost always be shut off here

						{ auto const SELECT_CASE_var1( Tank.ControlType );

						if ( SELECT_CASE_var1 == ControlTypeCycle ) {
							// Control will cycle on and off based on DeadBandTemp
							Qheater = 0.0;
							Qunmet = 0.0;
							Mode = FloatMode;
							continue;

						} else if ( SELECT_CASE_var1 == ControlTypeModulate ) {
							// Control will cycle on and off based on DeadBandTemp until Qneeded > Qmincap again
							Qheater = 0.0;
							Qunmet = Qneeded;
							Mode = FloatMode;
							continue;

							//CASE (ControlTypeModulateWithOverheat)  ! Not yet implemented
							// Calculate time to reach steady-state temp; check for venting at MaxTemp limit
							//Qheater = Qmincap

							//CASE (ControlTypeModulateWithUnderheat)  ! Not yet implemented
							// Heater must not come back on until Qneeded >= Qmincap
							//Mode = FloatMode

						}}

					} else if ( Qneeded <= Qmaxcap ) {
						// Heater can exactly meet the needed heat rate (usually by modulating) and heats for all of the remaining time
						// Setpoint temperature WILL be maintained

						TimeNeeded = TimeRemaining;

						Qheater = Qneeded;
						Qunmet = 0.0;

						NewTankTemp = SetPointTemp;

					} else { // Qneeded > Qmaxcap
						// Heater is at maximum capacity and heats for all of the remaining time
						// Setpoint temperature WILL NOT be maintained

						TimeNeeded = TimeRemaining;

						Qheater = Qmaxcap;
						Qunmet = Qneeded - Qheater;
						Qheat = Qoncycheat + Qheater + Qheatpump;

						NewTankTemp = CalcTankTemp( TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeNeeded );

					} // Qneeded > Qmaxcap

				} // TankTemp > SetPointTemp

				// Update summed values
				Eneeded += Qneeded * TimeNeeded;
				Eheater += Qheater * TimeNeeded;
				Eunmet += Qunmet * TimeNeeded;
				Eoncycfuel += Qoncycfuel * TimeNeeded;

				if ( Qmaxcap > 0.0 ) PLR = Qheater / Qmaxcap;
				PLF = PartLoadFactor( WaterThermalTankNum, PLR );
				Efuel += Qheater * TimeNeeded / ( PLF * Tank.Efficiency );

				Runtime += TimeNeeded;
				PLRsum += PLR * TimeNeeded;

				if ( ! Tank.FirstRecoveryDone ) {
					Tank.FirstRecoveryFuel += Efuel + Eoffcycfuel + Eoncycfuel;
					if ( SetPointRecovered ) Tank.FirstRecoveryDone = true;
				}

			} else if ( ( SELECT_CASE_var == FloatMode ) || ( SELECT_CASE_var == CoolMode ) ) { // Heater is off

				// Calculate heat rate needed to maintain the setpoint at steady-state conditions
				LossCoeff = Tank.OffCycLossCoeff;
				LossFracToZone = Tank.OffCycLossFracToZone;
				Qloss = LossCoeff * ( AmbientTemp - SetPointTemp );
				Qneeded = -Quse - Qsource - Qloss - Qoffcycheat;

				// This section really needs to work differently depending on ControlType
				// CYCLE will look at TankTemp, MODULATE will look at Qneeded

				if ( ( TankTemp < DeadBandTemp ) && ( ! Tank.IsChilledWaterTank ) ) {
					// Tank temperature is already below the minimum, possibly due to step change in scheduled SetPointTemp

					Mode = HeatMode;
					++CycleOnCount;
					continue;

				} else if ( ( TankTemp >= DeadBandTemp ) && ( ! Tank.IsChilledWaterTank ) ) {

					Qheat = Qoffcycheat + Qheatpump;

					// Calculate time needed for tank temperature to fall to minimum (setpoint - deadband)
					TimeNeeded = CalcTimeNeeded( TankTemp, DeadBandTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat );

					if ( TimeNeeded <= TimeRemaining ) {
						// Heating will be needed in this timestep

						NewTankTemp = DeadBandTemp;
						Mode = HeatMode;
						++CycleOnCount;

					} else { // TimeNeeded > TimeRemaining
						// Heating will not be needed for all of the remaining time

						NewTankTemp = CalcTankTemp( TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining );

						if ( ( NewTankTemp < MaxTemp ) || ( Tank.IsChilledWaterTank ) ) {
							// Neither heating nor venting is needed for all of the remaining time

							TimeNeeded = TimeRemaining;

						} else { // NewTankTemp >= MaxTemp
							// Venting will be needed in this timestep

							// Calculate time needed for tank temperature to rise to the maximum
							TimeNeeded = CalcTimeNeeded( TankTemp, MaxTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat );

							NewTankTemp = MaxTemp;
							Mode = VentMode;

						} // NewTankTemp >= MaxTemp

					} // TimeNeeded <= TimeRemaining

				} else if ( ( TankTemp > DeadBandTemp ) && ( Tank.IsChilledWaterTank ) ) {
					Mode = CoolMode;
					Qheat = Qheatpump;

					NewTankTemp = CalcTankTemp( TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining );
					TimeNeeded = TimeRemaining;
				} else if ( ( TankTemp <= DeadBandTemp ) && ( Tank.IsChilledWaterTank ) ) {

					if ( TankTemp < SetPointTemp ) Mode = FloatMode;

					Qheat = Qheatpump;

					NewTankTemp = CalcTankTemp( TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining );
					TimeNeeded = TimeRemaining;
				} // TankTemp vs DeadBandTemp for heaters and chilled water tanks

				// Update summed values
				Eneeded += Qneeded * TimeNeeded;
				Eunmet += Qunmet * TimeNeeded; // Qunmet may be propagated thru from the previous iteration
				Eoffcycfuel += Qoffcycfuel * TimeNeeded;

			} else if ( SELECT_CASE_var == VentMode ) { // Excess heat is vented

				LossCoeff = Tank.OffCycLossCoeff;
				LossFracToZone = Tank.OffCycLossFracToZone;
				Qheat = Qoffcycheat + Qheatpump;

				NewTankTemp = CalcTankTemp( TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining );

				if ( NewTankTemp < MaxTemp ) {
					// Venting is no longer needed because conditions have changed

					Mode = FloatMode;
					continue;

				} else { // NewTankTemp >= MaxTemp

					TimeNeeded = TimeRemaining;

					// Calculate the steady-state venting rate needed to maintain the tank at maximum temperature
					Qloss = LossCoeff * ( AmbientTemp - MaxTemp );
					Quse = UseMassFlowRate * Cp * ( UseInletTemp - MaxTemp );
					Qsource = SourceMassFlowRate * Cp * ( SourceInletTemp - MaxTemp );
					Qvent = -Quse - Qsource - Qloss - Qoffcycheat;

					NewTankTemp = MaxTemp;

				} // NewTankTemp < MaxTemp

				// Update summed values
				Event += Qvent * TimeNeeded;
				Eoffcycfuel += Qoffcycfuel * TimeNeeded;

			} else {
				// No default
				assert( false );
			}}

			deltaTsum = CalcTempIntegral( TankTemp, NewTankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeNeeded );

			// Update summed values
			Tsum += deltaTsum;
			Eloss += LossCoeff * ( AmbientTemp * TimeNeeded - deltaTsum );
			Elosszone += LossFracToZone * LossCoeff * ( AmbientTemp * TimeNeeded - deltaTsum );
			Euse += UseMassFlowRate * Cp * ( UseInletTemp * TimeNeeded - deltaTsum );
			if ( Tank.HeatPumpNum > 0 ) {
				Esource += Qheatpump * TimeNeeded;
			} else {
				Esource += SourceMassFlowRate * Cp * ( SourceInletTemp * TimeNeeded - deltaTsum );
			}

			TankTemp = NewTankTemp; // Update tank temperature

			TimeRemaining -= TimeNeeded;

			if ( CycleOnCount > MaxCycles ) {

				if ( ! WarmupFlag ) {
					if ( Tank.MaxCycleErrorIndex == 0 ) {
						ShowWarningError( "WaterHeater:Mixed = " + Tank.Name + ":  Heater is cycling on and off more than once per second." );
						ShowContinueError( "Try increasing Deadband Temperature Difference or Tank Volume" );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "WaterHeater:Mixed = " + Tank.Name + " Heater is cycling on and off more than once per second:", Tank.MaxCycleErrorIndex );
				}

				break;

			} // CycleOnCount > MaxCycles

		} // TimeRemaining > 0.0

		// Calculate average values over the timestep based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
		TankTempAvg = Tsum / SecInTimeStep;
		Qloss = Eloss / SecInTimeStep;
		Qlosszone = Elosszone / SecInTimeStep;
		Quse = Euse / SecInTimeStep;
		Qsource = Esource / SecInTimeStep;
		Qheater = Eheater / SecInTimeStep;
		Qoffcycfuel = Eoffcycfuel / SecInTimeStep;
		Qoffcycheat = Qoffcycfuel * Tank.OffCycParaFracToTank;
		Qoncycfuel = Eoncycfuel / SecInTimeStep;
		Qoncycheat = Qoncycfuel * Tank.OnCycParaFracToTank;
		Qvent = Event / SecInTimeStep;
		Qneeded = Eneeded / SecInTimeStep;
		Qunmet = Eunmet / SecInTimeStep;
		RTF = Runtime / SecInTimeStep;
		PLR = PLRsum / SecInTimeStep;

		if ( Tank.ControlType == ControlTypeCycle ) {
			// Recalculate Part Load Factor and fuel energy based on Runtime Fraction, instead of Part Load Ratio
			PLF = PartLoadFactor( WaterThermalTankNum, RTF );
			Efuel = Eheater / ( PLF * Tank.Efficiency );
		}

		Qfuel = Efuel / SecInTimeStep;

		Tank.Mode = Mode; // Operating mode for carry-over to next timestep

		Tank.TankTemp = TankTemp; // Final tank temperature for carry-over to next timestep
		Tank.TankTempAvg = TankTempAvg; // Average tank temperature over the timestep for reporting
		Tank.UseOutletTemp = TankTempAvg; // Because entire tank is at same temperature
		Tank.SourceOutletTemp = TankTempAvg; // Because entire tank is at same temperature
		if ( Tank.HeatPumpNum > 0 ) {
			Tank.SourceInletTemp = TankTempAvg + HPWHCondenserDeltaT; // Update the source inlet temperature to the average over the timestep
		}

		Tank.LossRate = Qloss;
		Tank.UseRate = Quse;
		Tank.SourceRate = Qsource;
		Tank.OffCycParaRateToTank = Qoffcycheat;
		Tank.OnCycParaRateToTank = Qoncycheat;
		Tank.TotalDemandRate = -Quse - Qsource - Qloss - Qoffcycheat - Qoncycheat;
		Tank.HeaterRate = Qheater;
		Tank.UnmetRate = Qunmet;
		Tank.VentRate = Qvent;
		Tank.NetHeatTransferRate = Quse + Qsource + Qloss + Qoffcycheat + Qoncycheat + Qheater + Qvent;

		Tank.CycleOnCount = CycleOnCount;
		Tank.RuntimeFraction = RTF;
		Tank.PartLoadRatio = PLR;

		Tank.FuelRate = Qfuel;
		Tank.OffCycParaFuelRate = Qoffcycfuel;
		Tank.OnCycParaFuelRate = Qoncycfuel;

		// Add water heater skin losses and venting losses to ambient zone, if specified
		if ( Tank.AmbientTempZone > 0 ) Tank.AmbientZoneGain = -Qlosszone - Qvent;

	}

	void
	CalcMixedTankSourceSideHeatTransferRate(
		Real64 HPWHCondenserDeltaT, // input, The temperature difference (C) across the heat pump, zero if there is no heat pump or if the heat pump is off
		Real64 SourceInletTemp, // input, Source inlet temperature (C)
		Real64 Cp, // Specific heat of fluid (J/kg deltaC)
		Real64 SetPointTemp, // input, Mixed tank set point temperature
		Real64 & SourceMassFlowRate, // source mass flow rate (kg/s)
		Real64 & Qheatpump, // heat transfer rate from heat pump
		Real64 & Qsource // steady state heat transfer rate from a constant temperature source side flow
	)
	{
		// Function Information:
		//		Author: Noel Merket
		//		Date Written: January 2015
		//		Modified: na
		//		Re-engineered: na

		// Purpose of this function:
		// Determines if the source side heat transfer is coming from a heat pump.
		// If so it treats the source side heat transfer as a constant heat source
		// If it is not coming from a heat pump it treats the source side heat transfer
		// as a constant temperature.

		// Determine if the source side heating is coming from a heat pump.
		Qheatpump = SourceMassFlowRate * Cp * HPWHCondenserDeltaT;
		if ( Qheatpump > 0.0 ) {
			SourceMassFlowRate = 0.0; // Handle this heating as a constant heat source
			Qsource = Qheatpump;
		} else {
			Qsource = SourceMassFlowRate * Cp * ( SourceInletTemp - SetPointTemp );
		}

	}

	Real64
	CalcTimeNeeded(
		Real64 const Ti, // Initial tank temperature (C)
		Real64 const Tf, // Final tank temperature (C)
		Real64 const Ta, // Ambient environment temperature (C)
		Real64 const T1, // Temperature of flow 1 (C)
		Real64 const T2, // Temperature of flow 2 (C)
		Real64 const m, // Mass of tank fluid (kg)
		Real64 const Cp, // Specific heat of fluid (J/kg deltaC)
		Real64 const m1, // Mass flow rate 1 (kg/s)
		Real64 const m2, // Mass flow rate 2 (kg/s)
		Real64 const UA, // Heat loss coefficient to ambient environment (W/deltaC)
		Real64 const Q // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   February 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the time needed for the tank temperature to change from Ti to Tf given heat loss,
		// mass flow rates and temperatures, and net heat transfer due to heater and parasitics.

		// METHODOLOGY EMPLOYED:
		// Equations are derived by solving the differential equation governing the tank energy balance.
		// Special cases which cause the natural logarithm to blow up are trapped and interpreted as
		// requiring an infinite amount of time because Tf can never be reached under the given conditions.

		// Return value
		Real64 CalcTimeNeeded;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Infinity( 99999999.9 ); // A time interval much larger than any single timestep (s)

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 a; // Intermediate variable
		Real64 b; // Intermediate variable
		Real64 Tm; // Mixed temperature after an infinite amount of time has passed (C)
		Real64 quotient; // Intermediate variable
		Real64 t; // Time elapsed from Ti to Tf (s)

		// FLOW:
		if ( Tf == Ti ) {
			// Already at Tf; no time is needed
			t = 0.0;

		} else {

			if ( UA / Cp + m1 + m2 == 0.0 ) {

				if ( Q == 0.0 ) {
					// With no mass flow and no heat flow and Tf<>Ti, then Tf can never be reached
					t = Infinity;

				} else {
					a = Q / ( m * Cp );

					t = ( Tf - Ti ) / a;

				}

			} else {
				a = ( Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2 ) / m;
				b = -( UA / Cp + m1 + m2 ) / m;

				// Calculate the mixed temperature Tm of the tank after an infinite amount of time has passed
				Tm = -a / b;

				if ( Tm == Ti ) {
					// Mixed temperature is the same as Ti; if Tf<>Ti, then Tf can never be reached
					t = Infinity;

				} else if ( Tm == Tf ) {
					// Tf only approaches Tm; it can never actually get there in finite time (also avoids divide by zero error)
					t = Infinity;

				} else {
					quotient = ( Tf - Tm ) / ( Ti - Tm );

					if ( quotient <= 0.0 ) { //Autodesk:Num Changed < to <= to elim poss floating point error in LOG call
						// Tm is in between Ti and Tf; Tf can never be reached
						t = Infinity;

					} else {
						t = std::log( quotient ) / b;

					}
				}
			}

			if ( t < 0.0 ) t = Infinity; // If negative time, Tf can never be reached in the future

		}

		CalcTimeNeeded = t;

		return CalcTimeNeeded;

	}

	Real64
	CalcTankTemp(
		Real64 const Ti, // Initial tank temperature (C)
		Real64 const Ta, // Ambient environment temperature (C)
		Real64 const T1, // Temperature of flow 1 (C)
		Real64 const T2, // Temperature of flow 2 (C)
		Real64 const m, // Mass of tank fluid (kg)
		Real64 const Cp, // Specific heat of fluid (J/kg deltaC)
		Real64 const m1, // Mass flow rate 1 (kg/s)
		Real64 const m2, // Mass flow rate 2 (kg/s)
		Real64 const UA, // Heat loss coefficient to ambient environment (W/deltaC)
		Real64 const Q, // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
		Real64 const t // Time elapsed from Ti to Tf (s)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   February 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the final tank temperature Tf after time t has elapsed given heat loss,
		// mass flow rates and temperatures, and net heat transfer due to heater and parasitics.

		// METHODOLOGY EMPLOYED:
		// Equations are derived by solving the differential equation governing the tank energy balance.

		// Return value
		Real64 CalcTankTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 a; // Intermediate variable
		Real64 b; // Intermediate variable
		Real64 Tf; // Final tank temperature (C)

		// FLOW:
		if ( UA / Cp + m1 + m2 == 0.0 ) {
			a = Q / ( m * Cp );

			Tf = a * t + Ti;

		} else {
			a = ( Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2 ) / m;
			b = -( UA / Cp + m1 + m2 ) / m;

			Tf = ( a / b + Ti ) * std::exp( b * t ) - a / b;

		}

		CalcTankTemp = Tf;

		return CalcTankTemp;

	}

	Real64
	CalcTempIntegral(
		Real64 const Ti, // Initial tank temperature (C)
		Real64 const Tf, // Final tank temperature (C)
		Real64 const Ta, // Ambient environment temperature (C)
		Real64 const T1, // Temperature of flow 1 (C)
		Real64 const T2, // Temperature of flow 2 (C)
		Real64 const m, // Mass of tank fluid (kg)
		Real64 const Cp, // Specific heat of fluid (J/kg deltaC)
		Real64 const m1, // Mass flow rate 1 (kg/s)
		Real64 const m2, // Mass flow rate 2 (kg/s)
		Real64 const UA, // Heat loss coefficient to ambient environment (W/deltaC)
		Real64 const Q, // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
		Real64 const t // Time elapsed from Ti to Tf (s)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   February 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the integral of the tank temperature from Ti to Tf.  The integral is added to a sum which is
		// later divided by the elapsed time to yield the average tank temperature over the timestep.

		// METHODOLOGY EMPLOYED:
		// Equations are the mathematical integrals of the governing differential equations.

		// Return value
		Real64 CalcTempIntegral;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 a; // Intermediate variable
		Real64 b; // Intermediate variable
		Real64 dTsum; // Integral of tank temperature (C s)

		// FLOW:
		if ( t == 0.0 ) {
			dTsum = 0.0;

		} else if ( Tf == Ti ) { // Steady-state conditions
			dTsum = Tf * t;

		} else if ( UA / Cp + m1 + m2 == 0.0 ) {
			a = Q / ( m * Cp );

			// Integral of T(t) = a * t + Ti, evaluated from 0 to t
			dTsum = 0.5 * a * t * t + Ti * t;

		} else {
			a = ( Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2 ) / m;
			b = -( UA / Cp + m1 + m2 ) / m;

			// Integral of T(t) = (a / b + Ti) * EXP(b * t) - a / b, evaluated from 0 to t
			dTsum = ( a / b + Ti ) * ( std::exp( b * t ) - 1.0 ) / b - a * t / b;
		}

		CalcTempIntegral = dTsum;

		return CalcTempIntegral;

	}

	Real64
	PartLoadFactor(
		int const WaterThermalTankNum,
		Real64 const PartLoadRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the Part Load Factor (PLF) based on a curve correlated to Part Load Ratio, if Heater Control Type
		// is MODULATE, or correlated to Runtime Fraction, if Heater Control Type is CYCLE.

		// METHODOLOGY EMPLOYED:
		// Uses CurveManager.  Part Load Factor is not allowed below 0.1.

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Return value
		Real64 PartLoadFactor;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:
		if ( WaterThermalTank( WaterThermalTankNum ).PLFCurve > 0 ) {
			PartLoadFactor = CurveValue( WaterThermalTank( WaterThermalTankNum ).PLFCurve, PartLoadRatio );

			PartLoadFactor = max( PartLoadFactor, 0.1 );
		} else {
			// No curve was defined
			PartLoadFactor = 1.0;
		}

		return PartLoadFactor;

	}

	void
	CalcWaterThermalTankStratified( int const WaterThermalTankNum ) // Water Heater being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//                      Nov 2011, BAN; modified the use and source outlet temperature calculation
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a stratified, multi-node water heater tank with up to two heating elements.

		// METHODOLOGY EMPLOYED:
		// This model uses a numerical calculation based on the forward Euler method.  A heat balance is calculated for each
		// node at a sub time step interval of one second.  Temperatures and energies change dynamically over the system
		// time step.  Final node temperatures are reported as final instantaneous values as well as averages over the
		// time step.  Heat transfer rates are averages over the time step.

		// Using/Aliasing
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::HourOfDay;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const dt( 1.0 ); // Sub time step interval (s)
		static std::string const RoutineName( "CalcWaterThermalTankStratified" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		Real64 SecInTimeStep; // Seconds in one timestep (s)
		Real64 TimeRemaining; // Time remaining in the current timestep (s)
		int NumNodes; // Number of stratified nodes
		int NodeNum; // Node number index
		Real64 NodeMass; // Mass of water in a node (kg)
		Real64 NodeTemp; // Instantaneous node temperature (C)
		Real64 TempUp; // Temperature of the upper node (C)
		Real64 TempDn; // Temperature of the lower node (C)
		Real64 InvMixUp; // Inversion mixing rate with the upper node (kg/s)
		Real64 InvMixDn; // Inversion mixing rate with the lower node (kg/s)
		Real64 Cp; // Specific heat of water (J/kg K)
		Real64 LossCoeff; // Loss coefficient to ambient environment (W/K)
		Real64 AmbientTemp; // Current ambient air temperature around tank (C)
		Real64 SetPointTemp1; // Current set point temperature for heater 1 (C)
		Real64 SetPointTemp2; // Current set point temperature for heater 2 (C)
		Real64 MinTemp1; // Minimum tank temperature (SetPointTemp1 - DeadBandDeltaTemp1) (C)
		Real64 MinTemp2; // Minimum tank temperature (SetPointTemp2 - DeadBandDeltaTemp2) (C)
		Real64 MaxTemp; // Maximum tank temperature before venting (C)
		Real64 Quse; // Heating rate due to use side mass flow (W)
		Real64 Qsource; // Heating rate due to source side mass flow (W)
		Real64 Qcond; // Heating rate due to vertical conduction between nodes
		Real64 Qflow; // Heating rate due to fluid flow between inlet and outlet nodes
		Real64 Qmix; // Heating rate due to temperature inversion mixing between nodes
		Real64 Qloss; // Heating rate due to ambient environment (W)
		Real64 Qlosszone; // Heating rate of fraction of losses added to the zone as a gain (W)
		Real64 Qheat; // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
		Real64 Qheater1; // Heating rate of burner or electric heating element 1 (W)
		Real64 Qheater2; // Heating rate of burner or electric heating element 2 (W)
		Real64 Qheater; // Combined heating rate of heater 1 and 2 (W)
		Real64 Qoffcycfuel; // Fuel consumption rate of off-cycle parasitics (W)
		Real64 Qoffcycheat; // Heating rate of fraction of off-cycle parasitics added to the tank (W)
		Real64 Qoncycfuel; // Fuel consumption rate on-cycle parasitics added to the tank (W)
		Real64 Qoncycheat; // Heating rate of fraction of on-cycle parasitics added to the tank (W)
		Real64 Qneeded; // Heating rate needed to recover or maintain the setpoint temperature (W)
		Real64 Qunmet; // The difference between Qneeded and Qheater (W)
		Real64 Qvent; // Heating rate due to venting because tank exceeded max temperature limit (W)
		Real64 Qfuel; // Heating rate for fuel consumed (W)
		Real64 UseInletTemp; // Use side inlet temperature (C)
		Real64 UseMassFlowRate; // Use side flow rate, including effectiveness factor (kg/s)
		Real64 SourceInletTemp; // Source side inlet temperature (C)
		Real64 SourceMassFlowRate; // Source side flow rate, including effectiveness factor (kg/s)
		int CycleOnCount1; // Number of times heater 1 cycles on in the current time step
		int CycleOnCount2; // Number of times heater 2 cycles on in the current time step
		Real64 Runtime1; // Time that heater 1 is running (s)
		Real64 Runtime2; // Time that heater 2 is running (s)
		Real64 Runtime; // Time that either heater is running (s)
		Real64 RTF1; // Runtime fraction, fraction of timestep that heater 1 is running
		Real64 RTF2; // Runtime fraction, fraction of timestep that heater 2 is running
		Real64 RTF; // Runtime fraction, fraction of timestep that either heater is running
		Real64 Eloss; // Energy change due to ambient losses over the timestep (J)
		Real64 Elosszone; // Energy change to the zone due to ambient losses over the timestep (J)
		Real64 Euse; // Energy change due to use side mass flow over the timestep (J)
		Real64 Esource; // Energy change due to source side mass flow over the timestep (J)
		Real64 Eheater1; // Energy change due to heater 1 over the timestep (J)
		Real64 Eheater2; // Energy change due to heater 2 over the timestep (J)
		Real64 Eoncycfuel; // Fuel energy consumed by on-cycle parasitics over the timestep (J)
		Real64 Eoffcycfuel; // Fuel energy consumed by off-cycle parasitics over the timestep (J)
		Real64 Event; // Energy change due to venting over the timestep (J)
		Real64 Eneeded; // Energy change needed over the timestep (J)
		Real64 Eunmet; // Energy change unmet over the timestep (J)
		Real64 Efuel; // Energy change for fuel consumed over the timestep (J)
		bool SetPointRecovered; // Flag to indicate when set point is recovered for the first time
		static int DummyWaterIndex( 1 );
		Real64 HPWHCondenserDeltaT; // Temperature difference across the condenser for a heat pump water heater

		// References
		WaterThermalTankData & Tank = WaterThermalTank( WaterThermalTankNum ); // Tank object

		// FLOW:
		TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

		if ( Tank.TimeElapsed != TimeElapsed ) {
			// The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
			// timestep for use as the initial conditions of each iteration that does not advance the system timestep.
			Tank.Node.SavedTemp() = Tank.Node.Temp();
			Tank.SavedHeaterOn1 = Tank.HeaterOn1;
			Tank.SavedHeaterOn2 = Tank.HeaterOn2;

			// Save outlet temperatures for demand-side flow control
			Tank.SavedUseOutletTemp = Tank.UseOutletTemp;
			Tank.SavedSourceOutletTemp = Tank.SourceOutletTemp;

			Tank.TimeElapsed = TimeElapsed;
		}

		Tank.Node.Temp() = Tank.Node.SavedTemp();
		Tank.HeaterOn1 = Tank.SavedHeaterOn1;
		Tank.HeaterOn2 = Tank.SavedHeaterOn2;

		SecInTimeStep = TimeStepSys * SecInHour;
		NumNodes = Tank.Nodes;

		AmbientTemp = Tank.AmbientTemp;
		UseInletTemp = Tank.UseInletTemp;
		SourceInletTemp = Tank.SourceInletTemp;

		// Calculate the heating rate from the heat pump.
		if ( Tank.HeatPumpNum > 0 ) {
			HeatPumpWaterHeaterData const & HeatPump = HPWaterHeater(Tank.HeatPumpNum);
			DataLoopNode::NodeData const & HPWHCondWaterInletNode = DataLoopNode::Node(HeatPump.CondWaterInletNode);
			DataLoopNode::NodeData const & HPWHCondWaterOutletNode = DataLoopNode::Node(HeatPump.CondWaterOutletNode);
			HPWHCondenserDeltaT = HPWHCondWaterOutletNode.Temp - HPWHCondWaterInletNode.Temp;
		} else {
			HPWHCondenserDeltaT = 0.0;
		}

		SetPointTemp1 = Tank.SetPointTemp;
		MinTemp1 = SetPointTemp1 - Tank.DeadBandDeltaTemp;
		SetPointTemp2 = Tank.SetPointTemp2;
		MinTemp2 = SetPointTemp2 - Tank.DeadBandDeltaTemp2;
		MaxTemp = Tank.TankTempLimit;

		if ( Tank.UseSidePlantLoopNum > 0 ) {
			Cp = GetSpecificHeatGlycol( PlantLoop( Tank.UseSidePlantLoopNum ).FluidName, Tank.TankTemp, PlantLoop( Tank.UseSidePlantLoopNum ).FluidIndex, RoutineName );
		} else {
			Cp = GetSpecificHeatGlycol( fluidNameWater, Tank.TankTemp, DummyWaterIndex, RoutineName );
		}

		TempUp = 0.0;
		TempDn = 0.0;
		Eloss = 0.0;
		Elosszone = 0.0;
		Euse = 0.0;
		Esource = 0.0;
		Eheater1 = 0.0;
		Eheater2 = 0.0;
		Event = 0.0;
		Eneeded = 0.0;
		Eunmet = 0.0;
		Efuel = 0.0;
		Eoncycfuel = 0.0;
		Eoffcycfuel = 0.0;
		CycleOnCount1 = 0;
		CycleOnCount2 = 0;
		Runtime = 0.0;
		Runtime1 = 0.0;
		Runtime2 = 0.0;
		SetPointRecovered = false;

		if ( Tank.InletMode == InletModeFixed ) CalcNodeMassFlows( WaterThermalTankNum, InletModeFixed );

		TimeRemaining = SecInTimeStep;
		while ( TimeRemaining > 0.0 ) {

			if ( Tank.InletMode == InletModeSeeking ) CalcNodeMassFlows( WaterThermalTankNum, InletModeSeeking );

			if ( ! Tank.IsChilledWaterTank ) {

				// Control the first heater element (master)
				if ( Tank.MaxCapacity > 0.0 ) {
					NodeNum = Tank.HeaterNode1;
					NodeTemp = Tank.Node( NodeNum ).Temp;

					if ( Tank.HeaterOn1 ) {
						if ( NodeTemp >= SetPointTemp1 ) {
							Tank.HeaterOn1 = false;
							SetPointRecovered = true;
						}
					} else { // Heater is off
						if ( NodeTemp < MinTemp1 ) {
							Tank.HeaterOn1 = true;
							++CycleOnCount1;
						}
					}
				}

				if ( Tank.HeaterOn1 ) {
					Qheater1 = Tank.MaxCapacity;
					Runtime1 += dt;
				} else {
					Qheater1 = 0.0;
				}

				// Control the second heater element (slave)
				if ( Tank.MaxCapacity2 > 0.0 ) {
					if ( ( Tank.ControlType == PriorityMasterSlave ) && Tank.HeaterOn1 ) {
						Tank.HeaterOn2 = false;

					} else {
						NodeNum = Tank.HeaterNode2;
						NodeTemp = Tank.Node( NodeNum ).Temp;

						if ( Tank.HeaterOn2 ) {
							if ( NodeTemp >= SetPointTemp2 ) {
								Tank.HeaterOn2 = false;
								SetPointRecovered = true;
							}
						} else { // Heater is off
							if ( NodeTemp < MinTemp2 ) {
								Tank.HeaterOn2 = true;
								++CycleOnCount2;
							}
						}
					}
				}

				if ( Tank.HeaterOn2 ) {
					Qheater2 = Tank.MaxCapacity2;
					Runtime2 += dt;
				} else {
					Qheater2 = 0.0;
				}
			} else { // chilled water thank, no heating

				Qheater1 = 0.0;
				Qheater2 = 0.0;

			}

			if ( Tank.HeaterOn1 || Tank.HeaterOn2 ) {
				Runtime += dt;

				Qfuel = ( Qheater1 + Qheater2 ) / Tank.Efficiency;
				Qoncycfuel = Tank.OnCycParaLoad;
				Qoffcycfuel = 0.0;
			} else {
				Qfuel = 0.0;
				Qoncycfuel = 0.0;
				Qoffcycfuel = Tank.OffCycParaLoad;
			}

			// Loop through all nodes and simulate heat balance
			for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
				NodeMass = Tank.Node( NodeNum ).Mass;
				NodeTemp = Tank.Node( NodeNum ).Temp;

				UseMassFlowRate = Tank.Node( NodeNum ).UseMassFlowRate * Tank.UseEffectiveness;
				SourceMassFlowRate = Tank.Node( NodeNum ).SourceMassFlowRate * Tank.SourceEffectiveness;

				// Heat transfer due to fluid flow entering an inlet node
				Quse = UseMassFlowRate * Cp * ( UseInletTemp - NodeTemp );
				Qsource = CalcStratifiedTankSourceSideHeatTransferRate(HPWHCondenserDeltaT, SourceInletTemp, Cp, SourceMassFlowRate, NodeTemp);

				InvMixUp = 0.0;
				if ( NodeNum > 1 ) {
					TempUp = Tank.Node( NodeNum - 1 ).Temp;
					if ( TempUp < NodeTemp ) InvMixUp = Tank.InversionMixingRate;
				}

				InvMixDn = 0.0;
				if ( NodeNum < NumNodes ) {
					TempDn = Tank.Node( NodeNum + 1 ).Temp;
					if ( TempDn > NodeTemp ) InvMixDn = Tank.InversionMixingRate;
				}

				// Heat transfer due to vertical conduction between nodes
				Qcond = Tank.Node( NodeNum ).CondCoeffUp * ( TempUp - NodeTemp ) + Tank.Node( NodeNum ).CondCoeffDn * ( TempDn - NodeTemp );

				// Heat transfer due to fluid flow between inlet and outlet nodes
				Qflow = Tank.Node( NodeNum ).MassFlowFromUpper * Cp * ( TempUp - NodeTemp ) + Tank.Node( NodeNum ).MassFlowFromLower * Cp * ( TempDn - NodeTemp );

				// Heat transfer due to temperature inversion mixing between nodes
				Qmix = InvMixUp * Cp * ( TempUp - NodeTemp ) + InvMixDn * Cp * ( TempDn - NodeTemp );

				if ( Tank.HeaterOn1 || Tank.HeaterOn2 ) {
					LossCoeff = Tank.Node( NodeNum ).OnCycLossCoeff;
					Qloss = LossCoeff * ( AmbientTemp - NodeTemp );
					Qlosszone = Qloss * Tank.SkinLossFracToZone;
					Qoncycheat = Tank.Node( NodeNum ).OnCycParaLoad * Tank.OnCycParaFracToTank;

					Qneeded = max( -Quse - Qsource - Qloss - Qoncycheat, 0.0 );

					Qheat = Qoncycheat;
					if ( NodeNum == Tank.HeaterNode1 ) Qheat += Qheater1;
					if ( NodeNum == Tank.HeaterNode2 ) Qheat += Qheater2;
				} else {
					LossCoeff = Tank.Node( NodeNum ).OffCycLossCoeff;
					Qloss = LossCoeff * ( AmbientTemp - NodeTemp );
					Qlosszone = Qloss * Tank.SkinLossFracToZone;
					Qoffcycheat = Tank.Node( NodeNum ).OffCycParaLoad * Tank.OffCycParaFracToTank;

					Qneeded = max( -Quse - Qsource - Qloss - Qoffcycheat, 0.0 );
					Qheat = Qoffcycheat;
				}

				Qunmet = max( Qneeded - Qheater1 - Qheater2, 0.0 );

				// Calculate node heat balance
				Tank.Node( NodeNum ).NewTemp = NodeTemp + ( Quse + Qsource + Qcond + Qflow + Qmix + Qloss + Qheat ) * dt / ( NodeMass * Cp );

				if ( ! Tank.IsChilledWaterTank ) {
					if ( ( NodeNum == 1 ) && ( Tank.Node( 1 ).NewTemp > MaxTemp ) ) {
						Event += NodeMass * Cp * ( MaxTemp - Tank.Node( 1 ).NewTemp );
						Tank.Node( 1 ).NewTemp = MaxTemp;
					}
				}

				Esource += Qsource * dt;
				Eloss += Qloss * dt;
				Elosszone += Qlosszone * dt;
				Eneeded += Qneeded * dt;
				Eunmet += Qunmet * dt;

			} // NodeNum

			Euse += UseMassFlowRate * Cp * (UseInletTemp - Tank.Node( Tank.UseOutletStratNode ).Temp) * dt;

			// Calculation for standard ratings
			if ( ! Tank.FirstRecoveryDone ) {
				Tank.FirstRecoveryFuel += ( Qfuel + Qoffcycfuel + Qoncycfuel ) * dt;
				if ( SetPointRecovered ) Tank.FirstRecoveryDone = true;
			}

			// Update node temperatures
			Tank.Node.Temp() = Tank.Node.NewTemp();
			Tank.Node.TempSum() += Tank.Node.Temp() * dt;

			TimeRemaining -= dt;

		} // TimeRemaining > 0.0

		Eheater1 = Tank.MaxCapacity * Runtime1;
		Eheater2 = Tank.MaxCapacity2 * Runtime2;
		Efuel = ( Eheater1 + Eheater2 ) / Tank.Efficiency;
		Eoffcycfuel = Tank.OffCycParaLoad * ( SecInTimeStep - Runtime );
		Eoncycfuel = Tank.OnCycParaLoad * Runtime;

		// Calculate average values over the time step based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
		Qloss = Eloss / SecInTimeStep;
		Qlosszone = Elosszone / SecInTimeStep;
		Quse = Euse / SecInTimeStep;
		Qsource = Esource / SecInTimeStep;
		Qheater1 = Eheater1 / SecInTimeStep;
		Qheater2 = Eheater2 / SecInTimeStep;
		Qheater = Qheater1 + Qheater2;
		Qoffcycfuel = Eoffcycfuel / SecInTimeStep;
		Qoffcycheat = Qoffcycfuel * Tank.OffCycParaFracToTank;
		Qoncycfuel = Eoncycfuel / SecInTimeStep;
		Qoncycheat = Qoncycfuel * Tank.OnCycParaFracToTank;
		Qvent = Event / SecInTimeStep;
		Qneeded = Eneeded / SecInTimeStep;
		Qunmet = Eunmet / SecInTimeStep;
		RTF = Runtime / SecInTimeStep;
		RTF1 = Runtime1 / SecInTimeStep;
		RTF2 = Runtime2 / SecInTimeStep;
		Qfuel = Efuel / SecInTimeStep;

		// Calculate average node temperatures over the time step
		Tank.Node.TempAvg() = Tank.Node.TempSum() / SecInTimeStep;
		Tank.Node.TempSum() = 0.0; // Reset for next time step

		// Calculate instantaneous and average tank temperature (all nodes have equal mass)
		Tank.TankTemp = sum( Tank.Node.Temp() ) / NumNodes;
		Tank.TankTempAvg = sum( Tank.Node.TempAvg() ) / NumNodes;

		NodeNum = Tank.UseOutletStratNode;
		if ( NodeNum > 0 ) Tank.UseOutletTemp = Tank.Node( NodeNum ).TempAvg;
		// Revised use outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
		if ( NodeNum > 0 ) {
			if ( Tank.UseMassFlowRate > 0.0 ) {
				Tank.UseOutletTemp = Tank.UseInletTemp * ( 1.0 - Tank.UseEffectiveness ) + Tank.UseOutletTemp * Tank.UseEffectiveness;
			}
		}
		NodeNum = Tank.SourceOutletStratNode;
		if ( NodeNum > 0 ) Tank.SourceOutletTemp = Tank.Node( NodeNum ).TempAvg;
		if ( Tank.HeatPumpNum > 0 ) Tank.SourceInletTemp = Tank.SourceOutletTemp + HPWHCondenserDeltaT;
		// Revised use outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
		if ( NodeNum > 0 ) {
			if ( Tank.SourceMassFlowRate > 0.0 ) {
				Tank.SourceOutletTemp = Tank.SourceInletTemp * ( 1.0 - Tank.SourceEffectiveness ) + Tank.SourceOutletTemp * Tank.SourceEffectiveness;
			}
		}

		Tank.LossRate = Qloss;
		Tank.UseRate = Quse;
		Tank.SourceRate = Qsource;
		Tank.OffCycParaRateToTank = Qoffcycheat;
		Tank.OnCycParaRateToTank = Qoncycheat;
		Tank.TotalDemandRate = -Quse - Qsource - Qloss - Qoffcycheat - Qoncycheat;
		Tank.HeaterRate = Qheater;
		Tank.HeaterRate1 = Qheater1;
		Tank.HeaterRate2 = Qheater2;

		Tank.UnmetRate = Qunmet;
		Tank.VentRate = Qvent;
		Tank.NetHeatTransferRate = Quse + Qsource + Qloss + Qoffcycheat + Qoncycheat + Qheater + Qvent;

		Tank.CycleOnCount = CycleOnCount1 + CycleOnCount2;
		Tank.CycleOnCount1 = CycleOnCount1;
		Tank.CycleOnCount2 = CycleOnCount2;

		Tank.RuntimeFraction = RTF;
		Tank.RuntimeFraction1 = RTF1;
		Tank.RuntimeFraction2 = RTF2;

		Tank.FuelRate = Qfuel;
		Tank.OffCycParaFuelRate = Qoffcycfuel;
		Tank.OnCycParaFuelRate = Qoncycfuel;

		// Add water heater skin losses and venting losses to ambient zone, if specified
		if ( Tank.AmbientTempZone > 0 ) WaterThermalTank( WaterThermalTankNum ).AmbientZoneGain = -Qlosszone - Qvent;

	}

	Real64
	CalcStratifiedTankSourceSideHeatTransferRate(
		Real64 HPWHCondenserDeltaT, // input, The temperature difference (C) across the heat pump, zero if there is no heat pump or if the heat pump is off
		Real64 SourceInletTemp, // input, Source inlet temperature (C)
		Real64 Cp, // Specific heat of fluid (J/kg deltaC)
		Real64 SourceMassFlowRate, // source mass flow rate (kg/s)
		Real64 NodeTemp // temperature of the source inlet node (C)
	)
	{
		// Function Information:
		//	Author: Noel Merket
		//	Date Written: January 2015

		// Purpose of this function:
		// Calculates the source side heat transfer rate of a stratified tank.

		// Methodology:
		// If the source side heat transfer is coming from a heat pump, then
		Real64 Qsource;
		if ( HPWHCondenserDeltaT > 0.0 ) {
			Qsource = SourceMassFlowRate * Cp * HPWHCondenserDeltaT;
		} else {
			Qsource = SourceMassFlowRate * Cp * ( SourceInletTemp - NodeTemp );
		}
		return Qsource;
	}

	void
	CalcNodeMassFlows(
		int const WaterThermalTankNum, // Water Heater being simulated
		int const InletMode // InletModeFixed or InletModeSeeking
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determines mass flow rates between nodes according to the locations of the use- and source-side inlet and outlet
		// nodes.

		// METHODOLOGY EMPLOYED:
		// In 'Seeking' mode, nodes are searched between the user-specified inlet and outlet nodes to find the node closest
		// in temperature to the inlet fluid temperature.  In 'Fixed' mode, the user-specified nodes are always used.
		// Upward and downward flows are added to each node between an inlet and outlet.  Flows in both directions cancel out
		// to leave only the net flow in one direction.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumNodes; // Number of stratified nodes
		int UseInletStratNode; // Use-side inlet node number
		int UseOutletStratNode; // Use-side outlet node number
		int SourceInletStratNode; // Source-side inlet node number
		int SourceOutletStratNode; // Source-side outlet node number
		int NodeNum; // Node number index
		Real64 UseMassFlowRate; // Use side flow rate, including effectiveness factor (kg/s)
		Real64 SourceMassFlowRate; // Source side flow rate, including effectiveness factor (kg/s)
		int Step; // DO loop step direction, 1 or -1
		Real64 DeltaTemp; // Temperature difference between node and inlet (delta C)
		Real64 MinDeltaTemp; // Smallest temperature difference found so far (delta C)

		// References to objects
		WaterThermalTankData & Tank = WaterThermalTank( WaterThermalTankNum );

		// FLOW:
		NumNodes = Tank.Nodes;

		UseInletStratNode = Tank.UseInletStratNode;
		UseOutletStratNode = Tank.UseOutletStratNode;
		SourceInletStratNode = Tank.SourceInletStratNode;
		SourceOutletStratNode = Tank.SourceOutletStratNode;

		UseMassFlowRate = Tank.UseMassFlowRate * Tank.UseEffectiveness;
		SourceMassFlowRate = Tank.SourceMassFlowRate * Tank.SourceEffectiveness;

		Tank.Node.UseMassFlowRate() = 0.0;
		Tank.Node.SourceMassFlowRate() = 0.0;
		Tank.Node.MassFlowFromUpper() = 0.0;
		Tank.Node.MassFlowFromLower() = 0.0;
		Tank.Node.MassFlowToUpper() = 0.0;
		Tank.Node.MassFlowToLower() = 0.0;

		if ( InletMode == InletModeSeeking ) {
			// 'Seek' the node with the temperature closest to the inlet temperature
			// Start at the user-specified inlet node and search to the user-specified outlet node

			if ( UseMassFlowRate > 0.0 ) {
				if ( UseInletStratNode > UseOutletStratNode ) {
					Step = -1;
				} else {
					Step = 1;
				}
				MinDeltaTemp = 1.0e6; // Some big number
				int const NodeNum_stop( floop_end( UseInletStratNode, UseOutletStratNode, Step ) );
				for ( NodeNum = UseInletStratNode; NodeNum != NodeNum_stop; NodeNum += Step ) {
					DeltaTemp = std::abs( Tank.Node( NodeNum ).Temp - Tank.UseInletTemp );
					if ( DeltaTemp < MinDeltaTemp ) {
						MinDeltaTemp = DeltaTemp;
						UseInletStratNode = NodeNum;
					} else if ( DeltaTemp > MinDeltaTemp ) {
						break;
					}
				}
			}

			if ( SourceMassFlowRate > 0.0 ) {
				if ( SourceInletStratNode > SourceOutletStratNode ) {
					Step = -1;
				} else {
					Step = 1;
				}
				MinDeltaTemp = 1.0e6; // Some big number
				int const NodeNum_stop( floop_end( SourceInletStratNode, SourceOutletStratNode, Step ) );
				for ( NodeNum = SourceInletStratNode; NodeNum != NodeNum_stop; NodeNum += Step ) {
					DeltaTemp = std::abs( Tank.Node( NodeNum ).Temp - Tank.SourceInletTemp );
					if ( DeltaTemp < MinDeltaTemp ) {
						MinDeltaTemp = DeltaTemp;
						SourceInletStratNode = NodeNum;
					} else if ( DeltaTemp > MinDeltaTemp ) {
						break;
					}
				}
			}

		}

		if ( UseInletStratNode > 0 ) Tank.Node( UseInletStratNode ).UseMassFlowRate = UseMassFlowRate;
		if ( SourceInletStratNode > 0 ) Tank.Node( SourceInletStratNode ).SourceMassFlowRate = SourceMassFlowRate;

		if ( UseMassFlowRate > 0.0 ) {
			if ( UseOutletStratNode > UseInletStratNode ) {
				// Use-side flow is down
				for ( NodeNum = UseInletStratNode; NodeNum <= UseOutletStratNode - 1; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowToLower += UseMassFlowRate;
				}
				for ( NodeNum = UseInletStratNode + 1; NodeNum <= UseOutletStratNode; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowFromUpper += UseMassFlowRate;
				}

			} else if ( UseOutletStratNode < UseInletStratNode ) {
				// Use-side flow is up
				for ( NodeNum = UseOutletStratNode; NodeNum <= UseInletStratNode - 1; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowFromLower += UseMassFlowRate;
				}
				for ( NodeNum = UseOutletStratNode + 1; NodeNum <= UseInletStratNode; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowToUpper += UseMassFlowRate;
				}

			} else {
				// Use-side flow is across the node; no flow to other nodes
			}
		}

		if ( SourceMassFlowRate > 0.0 ) {
			if ( SourceOutletStratNode > SourceInletStratNode ) {
				// Source-side flow is down
				for ( NodeNum = SourceInletStratNode; NodeNum <= SourceOutletStratNode - 1; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowToLower += SourceMassFlowRate;
				}
				for ( NodeNum = SourceInletStratNode + 1; NodeNum <= SourceOutletStratNode; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowFromUpper += SourceMassFlowRate;
				}

			} else if ( SourceOutletStratNode < SourceInletStratNode ) {
				// Source-side flow is up
				for ( NodeNum = SourceOutletStratNode; NodeNum <= SourceInletStratNode - 1; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowFromLower += SourceMassFlowRate;
				}
				for ( NodeNum = SourceOutletStratNode + 1; NodeNum <= SourceInletStratNode; ++NodeNum ) {
					Tank.Node( NodeNum ).MassFlowToUpper += SourceMassFlowRate;
				}

			} else {
				// Source-side flow is across the node; no flow to other nodes
			}
		}

		// Cancel out any up and down flows
		for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
			Tank.Node( NodeNum ).MassFlowFromUpper = max( ( Tank.Node( NodeNum ).MassFlowFromUpper - Tank.Node( NodeNum ).MassFlowToUpper ), 0.0 );
			Tank.Node( NodeNum ).MassFlowFromLower = max( ( Tank.Node( NodeNum ).MassFlowFromLower - Tank.Node( NodeNum ).MassFlowToLower ), 0.0 );
		}

	}

	void
	CalcDesuperheaterWaterHeater(
		int const WaterThermalTankNum, // Water Heater being simulated
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a refrigerant desuperheater to heat water

		// METHODOLOGY EMPLOYED:
		// This model uses the rated heat reclaim recovery efficiency, recovery efficiency modifier curve,
		// set point temperature, and dead band temperature difference to simulate the desuperheater coil
		// and sets up inputs to the tank model associated with the desuperheater coil

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataLoopNode::Node;
		using DXCoils::DXCoil;
		using Psychrometrics::CPHW;
		using Psychrometrics::RhoH2O;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHeatBalance::HeatReclaimDXCoil;
		using DataGlobals::SecInHour;
		using DataGlobals::WarmupFlag;
		using DataGlobals::DoingSizing;
		using DataGlobals::KickOffSimulation;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::ShortenTimeStepSys;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DataEnvironment::OutDryBulbTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for RegulaFalsi
		Real64 const Acc( 0.00001 ); // Accuracy of result from RegulaFalsi

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AvailSchedule; // desuperheater availability schedule
		Real64 SetPointTemp; // desuperheater set point temperature (cut-out temperature, C)
		Real64 DeadBandTempDiff; // desuperheater dead band temperature difference (C)
		Real64 CutInTemp; // desuperheater cut-in temperature (SetPointTemp - DeadBandTempDiff, C)
		Real64 TankTemp; // tank temperature before simulation, C
		Real64 NewTankTemp; // tank temperature after simulation, C
		Real64 MdotWater; // mass flow rate through desuperheater, kg/s
		Real64 PartLoadRatio; // desuperheater part load ratio
		Real64 QHeatRate; // desuperheater heating rate (W)
		Real64 AverageWasteHeat; // average hating rate from DX system condenser (W)
		Real64 HEffFTemp; // output of heating efficiency as a function of temperature curve
		Real64 Effic; // efficiency of desuperheater heating coil
		Real64 CpWater; // specific heat of water (J/Kg/k)
		int WaterInletNode; // desuperheater water inlet node number
		int WaterOutletNode; // desuperheater water outlet node number
		int DesuperheaterNum; // Index to desuperheater
		int SolFla; // Flag of RegulaFalsi solver
		int SourceID; // Waste Heat Source ID number
		Array1D< Real64 > Par( 5 ); // Parameters passed to RegulaFalsi
		static Real64 MinTemp( 0.0 ); // used for error messages, C
		std::string IterNum; // Max number of iterations for warning message

		// FLOW:
		DesuperheaterNum = WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum;
		AvailSchedule = GetCurrentScheduleValue( WaterHeaterDesuperheater( DesuperheaterNum ).AvailSchedPtr );
		WaterInletNode = WaterHeaterDesuperheater( DesuperheaterNum ).WaterInletNode;
		WaterOutletNode = WaterHeaterDesuperheater( DesuperheaterNum ).WaterOutletNode;

		// initialize variables before invoking any RETURN statement
		PartLoadRatio = 0.0;
		WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = 0.0;
		// reset tank inlet temp from previous time step
		WaterThermalTank( WaterThermalTankNum ).SourceInletTemp = WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp;
		WaterHeaterDesuperheater( DesuperheaterNum ).DesuperheaterPLR = 0.0;

		Node( WaterInletNode ).MassFlowRate = 0.0;
		Node( WaterOutletNode ).MassFlowRate = 0.0;
		Node( WaterOutletNode ).Temp = WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp;

		WaterHeaterDesuperheater( DesuperheaterNum ).DesuperheaterPLR = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaFuelRate = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaFuelEnergy = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelRate = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelEnergy = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTempOutput = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).HeaterRate = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).HeaterEnergy = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).PumpPower = 0.0;
		WaterHeaterDesuperheater( DesuperheaterNum ).PumpEnergy = 0.0;

		// set up initial conditions
		QHeatRate = 0.0;
		SetPointTemp = WaterHeaterDesuperheater( DesuperheaterNum ).SetPointTemp;
		DeadBandTempDiff = WaterHeaterDesuperheater( DesuperheaterNum ).DeadBandTempDiff;

		// simulate only the water heater tank if the desuperheater coil is scheduled off
		if ( AvailSchedule == 0.0 ) {
			WaterHeaterDesuperheater( DesuperheaterNum ).Mode = FloatMode;
			CalcWaterThermalTankMixed( WaterThermalTankNum );
			return;
		}

		// simulate only the water heater tank if the lowest temperature available from the desuperheater coil
		// is less than water inlet temperature if the reclaim source is a refrigeration condenser
		if ( ValidSourceType( DesuperheaterNum ) ) {
			SourceID = WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum;
			if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
				if ( HeatReclaimRefrigCondenser( SourceID ).AvailTemperature <= WaterThermalTank( WaterThermalTankNum ).SourceInletTemp ) {
					WaterHeaterDesuperheater( DesuperheaterNum ).Mode = FloatMode;
					CalcWaterThermalTankMixed( WaterThermalTankNum );
					ShowRecurringWarningErrorAtEnd( "WaterHeating:Desuperheater " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + " - Waste heat source temperature was too low to be useful.", WaterHeaterDesuperheater( DesuperheaterNum ).InsuffTemperatureWarn );
					return;
				} // Temp too low
			} // desuperheater source is condenser_refrigeration
		} // validsourcetype

		WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelRate = WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaLoad * ( 1.0 - PartLoadRatio );
		WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelEnergy = WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelRate * TimeStepSys * SecInHour;

		// check that water heater tank cut-in temp is greater than desuperheater cut-in temp
		if ( ( SetPointTemp - DeadBandTempDiff ) <= WaterThermalTank( WaterThermalTankNum ).SetPointTemp ) {
			if ( ! WarmupFlag && ! DoingSizing && ! KickOffSimulation ) {
				MinTemp = SetPointTemp - DeadBandTempDiff;
				++WaterHeaterDesuperheater( DesuperheaterNum ).SetPointError;
				if ( WaterHeaterDesuperheater( DesuperheaterNum ).SetPointError < 5 ) {
					ShowWarningError( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the desuperheater. Desuperheater will be disabled." );
					ShowContinueErrorTimeStamp( " ...Desuperheater cut-in temperature = " + RoundSigDigits( MinTemp, 2 ) );
				} else {
					ShowRecurringWarningErrorAtEnd( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the desuperheater. Desuperheater will be disabled warning continues...", WaterHeaterDesuperheater( DesuperheaterNum ).SetPointErrIndex1, MinTemp, MinTemp );
				}
			}

			//   Simulate tank if desuperheater unavailable for water heating
			CalcWaterThermalTankMixed( WaterThermalTankNum );
			return;
		}

		Effic = WaterHeaterDesuperheater( DesuperheaterNum ).HeatReclaimRecoveryEff;

		// store first iteration tank temperature and desuperheater mode of operation
		if ( FirstHVACIteration && ! ShortenTimeStepSys ) {
			// Save conditions from end of previous system timestep
			// Every iteration that does not advance time should reset to these values
			WaterThermalTank( WaterThermalTankNum ).SavedTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;
			WaterHeaterDesuperheater( DesuperheaterNum ).SaveMode = WaterHeaterDesuperheater( DesuperheaterNum ).Mode;
		}

		TankTemp = WaterThermalTank( WaterThermalTankNum ).SavedTankTemp;
		WaterHeaterDesuperheater( DesuperheaterNum ).Mode = WaterHeaterDesuperheater( DesuperheaterNum ).SaveMode;

		if ( WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp > 0 ) {
			HEffFTemp = max( 0.0, CurveValue( WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTemp, TankTemp, OutDryBulbTemp ) );
		} else {
			HEffFTemp = 1.0;
		}

		//set limits on heat recovery efficiency
		if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
			if ( ( HEffFTemp * Effic ) > 0.9 ) HEffFTemp = 0.9 / Effic;
		} else { // max is 0.3 for all other sources
			if ( ( HEffFTemp * Effic ) > 0.3 ) HEffFTemp = 0.3 / Effic;
		} //setting limits on heat recovery efficiency

		// Access the appropriate structure to find the average heating capacity of the desuperheater heating coil
		if ( ValidSourceType( DesuperheaterNum ) ) {
			SourceID = WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum;
			if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE ) {
				//Refrigeration systems are solved outside the time step iteration, so the
				//  appropriate decrement for other waste heat applications is handled differently
				AverageWasteHeat = HeatReclaimRefrigeratedRack( SourceID ).AvailCapacity - HeatReclaimRefrigeratedRack( SourceID ).UsedHVACCoil;
				WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR = 1.0;
			} else if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
				AverageWasteHeat = HeatReclaimRefrigCondenser( SourceID ).AvailCapacity - HeatReclaimRefrigCondenser( SourceID ).UsedHVACCoil;
				WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR = 1.0;
			} else if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COIL_DX_COOLING || WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COIL_DX_MULTISPEED || WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COIL_DX_MULTIMODE ) {
				AverageWasteHeat = HeatReclaimDXCoil( SourceID ).AvailCapacity;
				WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR = DXCoil( SourceID ).PartLoadRatio;
			}
		} else {
			AverageWasteHeat = 0.0;
		}

		// simulate only water heater tank if reclaim heating source is off
		if ( WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR == 0.0 ) {
			CalcWaterThermalTankMixed( WaterThermalTankNum );
			return;
		}

		// If the set point is higher than the maximum water temp, reset both the set point and the dead band temperature difference
		if ( SetPointTemp > WaterHeaterDesuperheater( DesuperheaterNum ).MaxInletWaterTemp ) {
			CutInTemp = SetPointTemp - DeadBandTempDiff;
			SetPointTemp = WaterHeaterDesuperheater( DesuperheaterNum ).MaxInletWaterTemp;
			DeadBandTempDiff = max( 0.0, ( SetPointTemp - CutInTemp ) );
		}

		// set the water-side mass flow rate
		CpWater = CPHW( Node( WaterInletNode ).Temp );
		MdotWater = WaterHeaterDesuperheater( DesuperheaterNum ).OperatingWaterFlowRate * RhoH2O( Node( WaterInletNode ).Temp );
		if ( Node( WaterInletNode ).Temp <= WaterHeaterDesuperheater( DesuperheaterNum ).MaxInletWaterTemp + Acc ) {
			QHeatRate = ( ( AverageWasteHeat * Effic * HEffFTemp ) / WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR ) + ( WaterHeaterDesuperheater( DesuperheaterNum ).PumpElecPower * WaterHeaterDesuperheater( DesuperheaterNum ).PumpFracToWater );
		}

		if ( MdotWater > 0.0 ) {
			Node( WaterOutletNode ).Temp = Node( WaterInletNode ).Temp + QHeatRate / ( MdotWater * CpWater );
		} else {
			Node( WaterOutletNode ).Temp = Node( WaterInletNode ).Temp;
		}

		// change to tanktypenum using parameters?
		{ auto const SELECT_CASE_var( WaterHeaterDesuperheater( DesuperheaterNum ).TankTypeNum );

		if ( SELECT_CASE_var == MixedWaterHeater ) {

			WaterHeaterDesuperheater( DesuperheaterNum ).SaveWHMode = WaterThermalTank( WaterThermalTankNum ).Mode;

			{ auto const SELECT_CASE_var1( WaterHeaterDesuperheater( DesuperheaterNum ).Mode );
			if ( SELECT_CASE_var1 == HeatMode ) {

				PartLoadRatio = WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR;

				//         set the full load outlet temperature on the water heater source inlet node (init has already been called)
				WaterThermalTank( WaterThermalTankNum ).SourceInletTemp = Node( WaterOutletNode ).Temp;

				//         set the source mass flow rate for the tank
				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater * PartLoadRatio;

				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = WaterHeaterDesuperheater( DesuperheaterNum ).BackupElementCapacity;
				WaterThermalTank( WaterThermalTankNum ).MinCapacity = WaterHeaterDesuperheater( DesuperheaterNum ).BackupElementCapacity;

				CalcWaterThermalTankMixed( WaterThermalTankNum );
				NewTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;

				if ( NewTankTemp > SetPointTemp ) {
					//           Only revert to floating mode if the tank temperature is higher than the cut out temperature
					if ( NewTankTemp > WaterHeaterDesuperheater( DesuperheaterNum ).SetPointTemp ) {
						WaterHeaterDesuperheater( DesuperheaterNum ).Mode = FloatMode;
					}
					Par( 1 ) = SetPointTemp;
					Par( 2 ) = WaterHeaterDesuperheater( DesuperheaterNum ).SaveWHMode;
					Par( 3 ) = WaterThermalTankNum;
					if ( FirstHVACIteration ) {
						Par( 4 ) = 1.0;
					} else {
						Par( 4 ) = 0.0;
					}
					Par( 5 ) = MdotWater;
					SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadRatio, PLRResidualMixedTank, 0.0, WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR, Par );
					if ( SolFla == -1 ) {
						gio::write( IterNum, fmtLD ) << MaxIte;
						strip( IterNum );
						if ( ! WarmupFlag ) {
							++WaterHeaterDesuperheater( DesuperheaterNum ).IterLimitExceededNum1;
							if ( WaterHeaterDesuperheater( DesuperheaterNum ).IterLimitExceededNum1 == 1 ) {
								ShowWarningError( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\"" );
								ShowContinueError( "Iteration limit exceeded calculating desuperheater unit part-load ratio, maximum iterations = " + IterNum + ". Part-load ratio returned = " + RoundSigDigits( PartLoadRatio, 3 ) );
								ShowContinueErrorTimeStamp( "This error occurred in heating mode." );
							} else {
								ShowRecurringWarningErrorAtEnd( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\":  Iteration limit exceeded in heating mode warning continues. Part-load ratio statistics follow.", WaterHeaterDesuperheater( DesuperheaterNum ).IterLimitErrIndex1, PartLoadRatio, PartLoadRatio );
							}
						}
					} else if ( SolFla == -2 ) {
						PartLoadRatio = max( 0.0, min( WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR, ( SetPointTemp - TankTemp ) / ( NewTankTemp - TankTemp ) ) );
						if ( ! WarmupFlag ) {
							++WaterHeaterDesuperheater( DesuperheaterNum ).RegulaFalsiFailedNum1;
							if ( WaterHeaterDesuperheater( DesuperheaterNum ).RegulaFalsiFailedNum1 == 1 ) {
								ShowWarningError( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\"" );
								ShowContinueError( "Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded. Part-load ratio used = " + RoundSigDigits( PartLoadRatio, 3 ) );
								ShowContinueError( "Please send this information to the EnergyPlus support group." );
								ShowContinueErrorTimeStamp( "This error occured in heating mode." );
							} else {
								ShowRecurringWarningErrorAtEnd( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\":  Part-load ratio calculation failed in heating mode warning continues. Part-load ratio statistics follow.", WaterHeaterDesuperheater( DesuperheaterNum ).RegulaFalsiFailedIndex1, PartLoadRatio, PartLoadRatio );
							}
						}
					}
					NewTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;
				} else {
					PartLoadRatio = WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR;
				}

			} else if ( SELECT_CASE_var1 == FloatMode ) {

				//         check tank temperature by setting source inlet mass flow rate to zero
				PartLoadRatio = 0.0;

				//         set the full load outlet temperature on the water heater source inlet node (init has already been called)
				WaterThermalTank( WaterThermalTankNum ).SourceInletTemp = Node( WaterOutletNode ).Temp;

				//         check tank temperature by setting source inlet mass flow rate to zero
				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = 0.0;

				//         disable the tank heater to find PLR of the HPWH
				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = 0.0;
				WaterThermalTank( WaterThermalTankNum ).MinCapacity = 0.0;

				CalcWaterThermalTankMixed( WaterThermalTankNum );
				NewTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;

				if ( NewTankTemp <= ( SetPointTemp - DeadBandTempDiff ) ) {
					WaterHeaterDesuperheater( DesuperheaterNum ).Mode = HeatMode;
					WaterThermalTank( WaterThermalTankNum ).Mode = WaterHeaterDesuperheater( DesuperheaterNum ).SaveWHMode;
					if ( ( TankTemp - NewTankTemp ) != 0.0 ) {
						PartLoadRatio = min( WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR, max( 0.0, ( ( SetPointTemp - DeadBandTempDiff ) - NewTankTemp ) / ( TankTemp - NewTankTemp ) ) );
					} else {
						PartLoadRatio = WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR;
					}

					//           set the full load outlet temperature on the water heater source inlet node
					WaterThermalTank( WaterThermalTankNum ).SourceInletTemp = Node( WaterOutletNode ).Temp;

					//           set the source mass flow rate for the tank and enable backup heating element
					WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater * PartLoadRatio;
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = WaterHeaterDesuperheater( DesuperheaterNum ).BackupElementCapacity;
					WaterThermalTank( WaterThermalTankNum ).MinCapacity = WaterHeaterDesuperheater( DesuperheaterNum ).BackupElementCapacity;

					CalcWaterThermalTankMixed( WaterThermalTankNum );
					NewTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;
					if ( NewTankTemp > SetPointTemp ) {
						Par( 1 ) = SetPointTemp;
						Par( 2 ) = WaterHeaterDesuperheater( DesuperheaterNum ).SaveWHMode;
						Par( 3 ) = WaterThermalTankNum;
						if ( FirstHVACIteration ) {
							Par( 4 ) = 1.0;
						} else {
							Par( 4 ) = 0.0;
						}
						Par( 5 ) = MdotWater;
						SolveRegulaFalsi( Acc, MaxIte, SolFla, PartLoadRatio, PLRResidualMixedTank, 0.0, WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR, Par );
						if ( SolFla == -1 ) {
							gio::write( IterNum, fmtLD ) << MaxIte;
							strip( IterNum );
							if ( ! WarmupFlag ) {
								++WaterHeaterDesuperheater( DesuperheaterNum ).IterLimitExceededNum2;
								if ( WaterHeaterDesuperheater( DesuperheaterNum ).IterLimitExceededNum2 == 1 ) {
									ShowWarningError( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\"" );
									ShowContinueError( "Iteration limit exceeded calculating desuperheater unit part-load ratio, maximum iterations = " + IterNum + ". Part-load ratio returned = " + RoundSigDigits( PartLoadRatio, 3 ) );
									ShowContinueErrorTimeStamp( "This error occurred in float mode." );
								} else {
									ShowRecurringWarningErrorAtEnd( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\":  Iteration limit exceeded in float mode warning continues. Part-load ratio statistics follow.", WaterHeaterDesuperheater( DesuperheaterNum ).IterLimitErrIndex2, PartLoadRatio, PartLoadRatio );
								}
							}
						} else if ( SolFla == -2 ) {
							PartLoadRatio = max( 0.0, min( WaterHeaterDesuperheater( DesuperheaterNum ).DXSysPLR, ( SetPointTemp - TankTemp ) / ( NewTankTemp - TankTemp ) ) );
							if ( ! WarmupFlag ) {
								++WaterHeaterDesuperheater( DesuperheaterNum ).RegulaFalsiFailedNum2;
								if ( WaterHeaterDesuperheater( DesuperheaterNum ).RegulaFalsiFailedNum2 == 1 ) {
									ShowWarningError( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\"" );
									ShowContinueError( "Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded. Part-load ratio used = " + RoundSigDigits( PartLoadRatio, 3 ) );
									ShowContinueError( "Please send this information to the EnergyPlus support group." );
									ShowContinueErrorTimeStamp( "This error occured in float mode." );
								} else {
									ShowRecurringWarningErrorAtEnd( WaterHeaterDesuperheater( DesuperheaterNum ).Type + " \"" + WaterHeaterDesuperheater( DesuperheaterNum ).Name + "\": Part-load ratio calculation failed in float mode warning continues. Part-load ratio statistics follow.", WaterHeaterDesuperheater( DesuperheaterNum ).RegulaFalsiFailedIndex2, PartLoadRatio, PartLoadRatio );
								}
							}
						}
					}
				} else {
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = WaterHeaterDesuperheater( DesuperheaterNum ).BackupElementCapacity;
					WaterThermalTank( WaterThermalTankNum ).MinCapacity = WaterHeaterDesuperheater( DesuperheaterNum ).BackupElementCapacity;
				}

			} else {
			}}

			//   should never get here, case is checked in GetWaterThermalTankInput
		} else {
			ShowFatalError( "Coil:WaterHeating:Desuperheater = " + WaterHeaterDesuperheater( DesuperheaterNum ).Name + ":  invalid water heater tank type and name entered = " + WaterHeaterDesuperheater( DesuperheaterNum ).TankType + ", " + WaterHeaterDesuperheater( DesuperheaterNum ).TankName );

		}}

		if ( QHeatRate == 0 ) PartLoadRatio = 0.0;

		Node( WaterOutletNode ).MassFlowRate = MdotWater * PartLoadRatio;
		WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTempOutput = HEffFTemp;
		WaterHeaterDesuperheater( DesuperheaterNum ).HeaterRate = QHeatRate * PartLoadRatio;
		WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater * PartLoadRatio;

		if ( PartLoadRatio == 0 ) {
			WaterThermalTank( WaterThermalTankNum ).SourceInletTemp = WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp;
			Node( WaterOutletNode ).Temp = WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp;
			WaterHeaterDesuperheater( DesuperheaterNum ).HEffFTempOutput = 0.0;
			WaterHeaterDesuperheater( DesuperheaterNum ).HeaterRate = 0.0;
		}

		WaterHeaterDesuperheater( DesuperheaterNum ).HeaterEnergy = WaterHeaterDesuperheater( DesuperheaterNum ).HeaterRate * TimeStepSys * SecInHour;
		WaterHeaterDesuperheater( DesuperheaterNum ).DesuperheaterPLR = PartLoadRatio;
		WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaFuelRate = WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaLoad * PartLoadRatio;
		WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaFuelEnergy = WaterHeaterDesuperheater( DesuperheaterNum ).OnCycParaFuelRate * TimeStepSys * SecInHour;
		WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelRate = WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaLoad * ( 1 - PartLoadRatio );
		WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelEnergy = WaterHeaterDesuperheater( DesuperheaterNum ).OffCycParaFuelRate * TimeStepSys * SecInHour;
		WaterHeaterDesuperheater( DesuperheaterNum ).PumpPower = WaterHeaterDesuperheater( DesuperheaterNum ).PumpElecPower * ( PartLoadRatio );
		WaterHeaterDesuperheater( DesuperheaterNum ).PumpEnergy = WaterHeaterDesuperheater( DesuperheaterNum ).PumpPower * TimeStepSys * SecInHour;

		// Update remaining waste heat (just in case multiple users of waste heat use same source)
		if ( ValidSourceType( DesuperheaterNum ) ) {
			SourceID = WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSourceIndexNum;
			if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE ) {
				//    Refrigeration systems are simulated at the zone time step, do not decrement available capacity
				HeatReclaimRefrigeratedRack( SourceID ).UsedWaterHeater = WaterHeaterDesuperheater( DesuperheaterNum ).HeaterRate;
			} else if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == CONDENSER_REFRIGERATION ) {
				HeatReclaimRefrigCondenser( SourceID ).UsedWaterHeater = WaterHeaterDesuperheater( DesuperheaterNum ).HeaterRate;
			} else if ( WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COIL_DX_COOLING || WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COIL_DX_MULTISPEED || WaterHeaterDesuperheater( DesuperheaterNum ).ReclaimHeatingSource == COIL_DX_MULTIMODE ) {
				HeatReclaimDXCoil( SourceID ).AvailCapacity -= WaterHeaterDesuperheater( DesuperheaterNum ).HeaterRate;
			}
		}

	}

	void
	CalcHeatPumpWaterHeater(
		int const WaterThermalTankNum, // Water Heater tank being simulated
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   March 2005
		//       MODIFIED       B. Griffith, Jan 2012 for stratified tank
		//						B. Shen 12/2014, add air-source variable-speed heat pump water heating
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates a heat pump water heater

		// METHODOLOGY EMPLOYED:
		// Simulate the water heater tank, DX coil, and fan to meet the water heating requirements.

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataHVACGlobals::ShortenTimeStepSys;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::HPWHInletDBTemp;
		using DataHVACGlobals::HPWHInletWBTemp;
		using DataHVACGlobals::HPWHCrankcaseDBTemp;
		using DataHVACGlobals::BlowThru;
		using DataHVACGlobals::CycFanCycCoil;
		using DataHVACGlobals::SmallTempDiff;
		using DataGlobals::WarmupFlag;
		using DataGlobals::DoingSizing;
		using DataGlobals::KickOffSimulation;
		using DXCoils::SimDXCoil;
		using DXCoils::CalcHPWHDXCoil;
		using Fans::SimulateFanComponents;
		using ScheduleManager::GetCurrentScheduleValue;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using Psychrometrics::CPHW; // , PsyWFnTdbTwbPb
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::RhoH2O;
		using VariableSpeedCoils::SimVariableSpeedCoils;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // maximum number of iterations
		Real64 const Acc( 0.001 ); // Accuracy of result from RegulaFalsi

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AvailSchedule; // HP compressor availability schedule
		Real64 SetPointTemp; // HP set point temperature (cut-out temperature, C)
		Real64 DeadBandTempDiff; // HP dead band temperature difference (C)
		Real64 TankTemp( 0.0 ); // tank temperature before simulation, C
		Real64 NewTankTemp( 0.0 ); // tank temperature after simulation, C
		Real64 CpAir; // specific heat of air, kJ/kg/K
		Real64 MdotWater; // mass flow rate of condenser water, kg/s
		Real64 OutletAirSplitterSch; // output of outlet air splitter schedule
		int HPAirInletNode; // HP air inlet node number
		int HPAirOutletNode; // HP air outlet node number
		int OutdoorAirNode; // Outdoor air inlet node number
		int ExhaustAirNode; // Exhaust air outlet node number
		int HPWaterInletNode; // HP condenser water inlet node number
		int HPWaterOutletNode; // HP condenser water outlet node number
		int InletAirMixerNode; // HP inlet air mixer node number
		int OutletAirSplitterNode; // HP outlet air splitter node number
		int DXCoilAirInletNode; // Inlet air node number of DX coil
		int SolFla( 0 ); // Flag of RegulaFalsi solver
		Array1D< Real64 > Par( 5 ); // Parameters passed to RegulaFalsi
		Array1D< Real64 > ParVS( 10 ); // Parameters passed to RegulaFalsi, for variable-speed HPWH
		Real64 HPMinTemp; // used for error messages, C
		std::string HPMinTempChar; // used for error messages
		std::string IterNum; // Max number of iterations for warning message
		int CompOp; // DX compressor operation; 1=on, 0=off
		Real64 CondenserDeltaT; // HPWH condenser water temperature difference
		//new varaibles for variable-speed HPWH
		int MaxSpeedNum( 0 ); // speed number of variable speed HPWH coil
		int SpeedNum( 1 ); // selected speed number
		Real64 RhoWater; //water density
		Real64 SpeedRatio( 0.0 ); //speed ratio for interpolating between two speed levels
		bool bIterSpeed( false );// interpolation between speed level or not
		Real64 zeroResidual( 1.0 ); //residual when running VSHPWH at 0.0 part-load ratio, 1.0 needed setting >0
		int i;// index for iteration
		Real64 EMP1( 0.0 ), EMP2( 0.0 ), EMP3( 0.0 ); //place holder to calling vs HPWH function
		Real64 LowSpeedTankTemp( 0.0 ); //tank temperature resulted by a lower compressor speed
		Real64 HPWHCondInletNodeLast; // Water temp sent from WH on last iteration
		//Real64 HPWaterInletNodeTempSaved; // Water temp saved from previous timestep
		int loopIter; // iteration loop counter

		// References to objects used in this function
		WaterThermalTankData & Tank = WaterThermalTank( WaterThermalTankNum );
		HeatPumpWaterHeaterData & HeatPump = HPWaterHeater( Tank.HeatPumpNum );

		// FLOW:
		// initialize local variables
		AvailSchedule = GetCurrentScheduleValue( HeatPump.AvailSchedPtr );
		HPAirInletNode = HeatPump.HeatPumpAirInletNode;
		HPAirOutletNode = HeatPump.HeatPumpAirOutletNode;
		OutdoorAirNode = HeatPump.OutsideAirNode;
		ExhaustAirNode = HeatPump.ExhaustAirNode;
		HPWaterInletNode = HeatPump.CondWaterInletNode;
		HPWaterOutletNode = HeatPump.CondWaterOutletNode;
		InletAirMixerNode = HeatPump.InletAirMixerNode;
		OutletAirSplitterNode = HeatPump.OutletAirSplitterNode;
		DXCoilAirInletNode = HeatPump.DXCoilAirInletNode;
		HPPartLoadRatio = 0.0;
		CompOp = 0;
		HeatPump.OnCycParaFuelRate = 0.0;
		HeatPump.OnCycParaFuelEnergy = 0.0;
		HeatPump.OffCycParaFuelRate = 0.0;
		HeatPump.OffCycParaFuelEnergy = 0.0;
		Node( HPWaterOutletNode ) = Node( HPWaterInletNode );
		MaxSpeedNum = HeatPump.NumofSpeed;

		// assign set point temperature (cut-out) and dead band temp diff (cut-in = cut-out minus dead band temp diff)
		SetPointTemp = HeatPump.SetPointTemp;
		DeadBandTempDiff = HeatPump.DeadBandTempDiff;
		RhoWater = RhoH2O( SetPointTemp ); // initialize

		// store first iteration tank temperature and HP mode of operation
		// this code can be called more than once with FirstHVACIteration = .TRUE., use FirstTimeThroughFlag to control save
		if ( FirstHVACIteration && ! ShortenTimeStepSys && HeatPump.FirstTimeThroughFlag ) {
			Tank.SavedTankTemp = Tank.TankTemp;
			HeatPump.SaveMode = HeatPump.Mode;
			HeatPump.SaveWHMode = Tank.Mode;
			HeatPump.FirstTimeThroughFlag = false;
		}

		if ( ! FirstHVACIteration ) HeatPump.FirstTimeThroughFlag = true;

		// check if HPWH is off for some reason and simulate HPWH air- and water-side mass flow rates of 0
		// simulate only water heater tank if HP compressor is scheduled off
		//   simulate only water heater tank if HP compressor cut-out temperature is lower than the tank's cut-in temperature
		//    simulate only water heater tank if HP inlet air temperature is below minimum temperature for HP compressor operation
		//    if the tank maximum temperature limit is less than the HPWH set point temp, disable HPWH
		if ( AvailSchedule == 0.0 || ( SetPointTemp - DeadBandTempDiff ) <= Tank.SetPointTemp || HPWHInletDBTemp < HeatPump.MinAirTempForHPOperation || SetPointTemp >= Tank.TankTempLimit ) {
			//   revert to float mode any time HPWH compressor is OFF
			HeatPump.Mode = FloatMode;
			if ( InletAirMixerNode > 0 ) {
				Node( InletAirMixerNode ) = Node( HPAirInletNode );
			}
			//   pass node info and simulate crankcase heater
			if (MaxSpeedNum > 0) {
				SpeedRatio = 1.0;
				SpeedNum = 1;
				if (HeatPump.FanPlacement == BlowThru) {
					SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
					SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
					SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
				} else {
					SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
					SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
					SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				}
			} else {
				if (HeatPump.FanPlacement == BlowThru) {
					SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
					SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
				} else {
					SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
					SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				}
			}

			if (OutletAirSplitterNode > 0) {
				Node(HPAirOutletNode) = Node(OutletAirSplitterNode);
			}

			//   Simulate tank if HP compressor unavailable for water heating
			{ auto const SELECT_CASE_var( HeatPump.TankTypeNum );

			if ( SELECT_CASE_var == MixedWaterHeater ) {
				CalcWaterThermalTankMixed( WaterThermalTankNum );
			} else if ( SELECT_CASE_var == ( StratifiedWaterHeater ) ) {
				CalcWaterThermalTankStratified( WaterThermalTankNum );
			}}

			//   If HPWH compressor is available and unit is off for another reason, off-cycle parasitics are calculated
			if ( AvailSchedule != 0 ) {
				HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * ( 1.0 - HPPartLoadRatio );
				HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * TimeStepSys * SecInHour;
			}

			//   Warn if HPWH compressor cut-in temperature is less than the water heater tank's set point temp
			if ( ! WarmupFlag && ! DoingSizing && ! KickOffSimulation ) {
				if ( ( SetPointTemp - DeadBandTempDiff ) <= Tank.SetPointTemp ) {
					HPMinTemp = SetPointTemp - DeadBandTempDiff;
					gio::write( HPMinTempChar, fmtLD ) << HPMinTemp;
					++HeatPump.HPSetPointError;
					//!add logic for warmup, kickoffsimulation and doing sizing here
					if ( HeatPump.HPSetPointError == 1 ) {
						ShowWarningError( HeatPump.Type + " \"" + HeatPump.Name + ":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the heat pump water heater. Heat Pump will be disabled and simulation continues." );
						ShowContinueErrorTimeStamp( " ...Heat Pump cut-in temperature=" + HPMinTempChar );
					} else {
						ShowRecurringWarningErrorAtEnd( HeatPump.Type + " \"" + HeatPump.Name + ":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the heat pump water heater. Heat Pump will be disabled error continues...", HeatPump.HPSetPointErrIndex1, HPMinTemp, HPMinTemp );
					}
				}
			}
			return;
		}
		{ auto const SELECT_CASE_var( HeatPump.TankTypeNum );
		if ( SELECT_CASE_var == MixedWaterHeater ) {
			TankTemp = Tank.SavedTankTemp;
		} else if ( SELECT_CASE_var == StratifiedWaterHeater ) {
			TankTemp = FindStratifiedTankSensedTemp( WaterThermalTankNum, HeatPump.ControlSensorLocation );
		} else {
			assert( false );
		}}
		HeatPump.Mode = HeatPump.SaveMode;

		RhoWater = RhoH2O(TankTemp);//udpate water density using tank temp

		// set the heat pump air- and water-side mass flow rate
		MdotWater = HeatPump.OperatingWaterFlowRate * RhoH2O( TankTemp );

		// Select mode of operation (float mode or heat mode) from last iteration.
		// Determine if heating will occur this iteration and get an estimate of the PLR
		switch (HeatPump.Mode) {
		case HeatMode:
			// HPWH was heating last iteration and will continue to heat until the set point is reached

			HPPartLoadRatio = 1.0;
			break;

		case FloatMode:
			// HPWH was floating last iteration and will continue to float until the cut-in temperature is reached

			// set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
			{ auto const SELECT_CASE_var1( HeatPump.TankTypeNum );
			if ( SELECT_CASE_var1 == MixedWaterHeater ) {
				Node( HPWaterInletNode ).Temp = TankTemp;
				Node( HPWaterOutletNode ).Temp = TankTemp;
			} else if ( SELECT_CASE_var1 == StratifiedWaterHeater ) {
				Node( HPWaterInletNode ).Temp = Tank.SourceOutletTemp;
				Node( HPWaterOutletNode ).Temp = Tank.SourceInletTemp;
			}}
			Node( HPWaterInletNode ).MassFlowRate = 0.0;
			Node( HPWaterOutletNode ).MassFlowRate = 0.0;

			// Check tank temperature by setting source inlet mass flow rate to zero.
			HPPartLoadRatio = 0.0;

			// Set the full load outlet temperature on the water heater source inlet node (init has already been called).
			Tank.SourceInletTemp = Node( HPWaterOutletNode ).Temp;

			// Check tank temperature by setting source inlet mass flow rate to zero.
			Tank.SourceMassFlowRate = 0.0;

			// Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
			Tank.MaxCapacity = 0.0;
			Tank.MinCapacity = 0.0;
			{ auto const SELECT_CASE_var1( HeatPump.TankTypeNum );
			if ( SELECT_CASE_var1 == MixedWaterHeater ) {
				CalcWaterThermalTankMixed( WaterThermalTankNum );
				NewTankTemp = Tank.TankTemp;
			} else if ( SELECT_CASE_var1 == StratifiedWaterHeater ) {
				CalcWaterThermalTankStratified( WaterThermalTankNum );
				NewTankTemp = FindStratifiedTankSensedTemp( WaterThermalTankNum, HeatPump.ControlSensorLocation );
			} else {
				assert( false );
			}}

			// Reset the tank's internal heating element capacity.
			Tank.MaxCapacity = HeatPump.BackupElementCapacity;
			Tank.MinCapacity = HeatPump.BackupElementCapacity;

			// Check to see if the tank drifts below set point if no heating happens.
			if ( NewTankTemp <= ( SetPointTemp - DeadBandTempDiff ) ) {

				// HPWH is now in heating mode
				HeatPump.Mode = HeatMode;

				// Reset the water heater's mode (call above may have changed modes)
				Tank.Mode = HeatPump.SaveWHMode;

				// Estimate portion of time step that the HP operates based on a linear interpolation of the tank temperature decay
				// assuming that all heating sources are off.
				if ( TankTemp != NewTankTemp ) {
					HPPartLoadRatio = max( 0.0, min( 1.0, ( ( SetPointTemp - DeadBandTempDiff ) - NewTankTemp ) / ( TankTemp - NewTankTemp ) ) );
				} else {
					HPPartLoadRatio = 1.0;
				}
			}
			break;

		default:
			// Never gets here, only allowed modes for HPWH are float and heat
			assert( false );
		}

		// If the HPWH was in heating mode during the last timestep or if it was determined that
		// heating would be needed during this timestep to maintain setpoint, do the heating calculation.
		if ( HeatPump.Mode == HeatMode ) {

			// set up air flow on DX coil inlet node
			Node( DXCoilAirInletNode ).MassFlowRate = MdotAir * HPPartLoadRatio;

			// set the condenser inlet node mass flow rate prior to calling the CalcHPWHDXCoil
			Node( HPWaterInletNode ).MassFlowRate = MdotWater * HPPartLoadRatio;
			Tank.SourceMassFlowRate = MdotWater * HPPartLoadRatio;

			HPWHCondInletNodeLast = Node( HPWaterInletNode ).Temp;
			//HPWaterInletNodeTempSaved = Node( HPWaterInletNode ).Temp;
			// This for loop is intended to iterate and converge on a condenser operating temperature so that the evaporator model correctly calculates performance.
			// CHECK- embedded both the nonVS and the VS sim calls inside the for-loop, previously the VS wasn't in a for loop
			for ( loopIter = 1; loopIter <= 4; ++loopIter ) {
				if (MaxSpeedNum > 0) { // lowest speed of VS HPWH coil
					SpeedRatio = 1.0;
					HPPartLoadRatio = 1.0;
					bIterSpeed = true; // prepare for iterating between speed levels
					SpeedNum = 1;
					SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio,
						RhoWater, MdotWater, FirstHVACIteration);
					SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum,
						CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
				} else {
					CalcHPWHDXCoil(HeatPump.DXCoilNum, HPPartLoadRatio);
				}
				// Currently, HPWH heating rate is only a function of inlet evap conditions and air flow rate
				// If HPWH is ever allowed to vary fan speed, this next sub should be called.
				// CALL CalcDOE2DXCoil(DXCoilNum, HPPartLoadRatio, FirstHVACIteration,PartLoadRatio, FanOpMode)
				// (possibly with an iteration loop to converge on a solution)
				CondenserDeltaT = Node( HPWaterOutletNode ).Temp - Node( HPWaterInletNode ).Temp;
				Tank.SourceInletTemp = Node(HPWaterInletNode).Temp + CondenserDeltaT;

				// this CALL does not update node temps, must use WaterThermalTank variables
				// select tank type
				{ auto const SELECT_CASE_var1( HeatPump.TankTypeNum );
				if ( SELECT_CASE_var1 == MixedWaterHeater ) {
					CalcWaterThermalTankMixed( WaterThermalTankNum );
					NewTankTemp = Tank.TankTemp;
				} else if ( SELECT_CASE_var1 == StratifiedWaterHeater ) {
					CalcWaterThermalTankStratified( WaterThermalTankNum );
					NewTankTemp = FindStratifiedTankSensedTemp( WaterThermalTankNum, HeatPump.ControlSensorLocation );
				}}

				LowSpeedTankTemp = NewTankTemp;

				Node( HPWaterInletNode ).Temp = Tank.SourceOutletTemp;
				if ( std::abs( Node( HPWaterInletNode ).Temp - HPWHCondInletNodeLast ) < SmallTempDiff ) break;
				HPWHCondInletNodeLast = Node( HPWaterInletNode ).Temp;
			}

			if ( NewTankTemp > SetPointTemp ) {
				HeatPump.Mode = FloatMode;
				Par( 1 ) = SetPointTemp;
				Par( 2 ) = HeatPump.SaveWHMode;
				Par( 3 ) = WaterThermalTankNum;
				if ( FirstHVACIteration ) {
					Par( 4 ) = 1.0;
				} else {
					Par( 4 ) = 0.0;
				}
				Par( 5 ) = MdotWater;

				if (MaxSpeedNum > 0) {
					//square the solving, and avoid warning
					//due to very small capacity at lowest speed of VSHPWH coil
					{ auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
					if (SELECT_CASE_var1 == MixedWaterHeater) {
						zeroResidual = PLRResidualMixedTank(0.0, Par);
					} else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
						zeroResidual = PLRResidualStratifiedTank(0.0, Par);
					}}
				}

				if (zeroResidual > 0.0) { // then iteration
					{ auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
					if (SELECT_CASE_var1 == MixedWaterHeater) {
						SolveRegulaFalsi(Acc, MaxIte, SolFla, HPPartLoadRatio, PLRResidualMixedTank, 0.0, 1.0, Par);
					} else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
						SolveRegulaFalsi(Acc, MaxIte, SolFla, HPPartLoadRatio, PLRResidualStratifiedTank, 0.0, 1.0, Par);
					}}
					if (SolFla == -1) {
						gio::write(IterNum, fmtLD) << MaxIte;
						strip(IterNum);
						if (!WarmupFlag) {
							++HeatPump.IterLimitExceededNum2;
							if (HeatPump.IterLimitExceededNum2 == 1) {
								ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
								ShowContinueError("Iteration limit exceeded calculating heat pump water heater compressor part-load ratio, maximum iterations = " + IterNum + ". Part-load ratio returned = " + RoundSigDigits(HPPartLoadRatio, 3));
								ShowContinueErrorTimeStamp("This error occurred in float mode.");
							} else {
								ShowRecurringWarningErrorAtEnd(HeatPump.Type + " \"" + HeatPump.Name + "\":  Iteration limit exceeded in float mode warning continues. Part-load ratio statistics follow.", HeatPump.IterLimitErrIndex2, HPPartLoadRatio, HPPartLoadRatio);
							}
						}
					} else if (SolFla == -2) {
						HPPartLoadRatio = max(0.0, min(1.0, (SetPointTemp - TankTemp) / (NewTankTemp - TankTemp)));
						if (!WarmupFlag) {
							++HeatPump.RegulaFalsiFailedNum2;
							if (HeatPump.RegulaFalsiFailedNum2 == 1) {
								ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
								ShowContinueError("Heat pump water heater compressor part-load ratio calculation failed: PLR limits of 0 to 1 exceeded. Part-load ratio used = " + RoundSigDigits(HPPartLoadRatio, 3));
								ShowContinueError("Please send this information to the EnergyPlus support group.");
								ShowContinueErrorTimeStamp("This error occured in float mode.");
							} else {
								ShowRecurringWarningErrorAtEnd(HeatPump.Type + " \"" + HeatPump.Name + "\": Part-load ratio calculation failed in float mode warning continues. Part-load ratio statistics follow.", HeatPump.RegulaFalsiFailedIndex2, HPPartLoadRatio, HPPartLoadRatio);
							}
						}
					}
				} else {
					HPPartLoadRatio = 0.0;
				};

				// Re-calculate the HPWH Coil to get the correct heat transfer rate.
				Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
				if (MaxSpeedNum > 0) {
					SpeedRatio = 1.0;
					bIterSpeed = false; //prepare for iterating between speed levels
					SpeedNum = 1;

					SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio,
						RhoWater, MdotWater, FirstHVACIteration);
					SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum,
						CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
				} else{
					CalcHPWHDXCoil(HeatPump.DXCoilNum, HPPartLoadRatio);
				}
			} else if (bIterSpeed) {
				for ( loopIter = 1; loopIter <= 4; ++loopIter ) {
					HeatPump.Mode = HeatMode;//HeatMode is important for system convergence
					HPPartLoadRatio = 1.0;
					SpeedRatio = 1.0;
					for (i = 2; i <= MaxSpeedNum; ++i) {
						SpeedNum = i;
						SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
						SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);

						CondenserDeltaT = Node(HPWaterOutletNode).Temp - Node(HPWaterInletNode).Temp;

						//           move the full load outlet temperature rate to the water heater structure variables
						//           (water heaters source inlet node temperature/mdot are set in Init, set it here after CalcHPWHDXCoil has been called)
						WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(HPWaterInletNode).Temp + CondenserDeltaT;
						//				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

						//           this CALL does not update node temps, must use WaterThermalTank variables
						// select tank type
						{ auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
						if (SELECT_CASE_var1 == MixedWaterHeater) {
							CalcWaterThermalTankMixed(WaterThermalTankNum);
							NewTankTemp = Tank.TankTemp;
						} else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
							CalcWaterThermalTankStratified(WaterThermalTankNum);
							NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, HeatPump.ControlSensorLocation);
						}}

						if (NewTankTemp > SetPointTemp) {
							SpeedNum = i;
							break;
						} else {
							LowSpeedTankTemp = NewTankTemp;
						}
					}

					if ( NewTankTemp > SetPointTemp ) {
						ParVS(1) = WaterThermalTankNum;
						ParVS(2) = Tank.HeatPumpNum;
						ParVS(3) = SpeedNum;
						ParVS(4) = HPWaterInletNode;
						ParVS(5) = HPWaterOutletNode;
						ParVS(6) = RhoWater;
						ParVS(7) = SetPointTemp;
						ParVS(8) = HeatPump.SaveWHMode;
						if (FirstHVACIteration) {
							ParVS(9) = 1.0;
						} else {
							ParVS(9) = 0.0;
						}

						SolveRegulaFalsi(Acc, MaxIte, SolFla, SpeedRatio, PLRResidualIterSpeed, 1.0e-10, 1.0, ParVS);

						if (SolFla == -1) {
							gio::write(IterNum, fmtLD) << MaxIte;
							strip(IterNum);
							if (!WarmupFlag) {
								++HeatPump.IterLimitExceededNum1;
								if (HeatPump.IterLimitExceededNum1 == 1) {
									ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
									ShowContinueError("Iteration limit exceeded calculating heat pump water heater speed" " speed ratio ratio, maximum iterations = " + IterNum + ". speed ratio returned = " + RoundSigDigits(SpeedRatio, 3));
									ShowContinueErrorTimeStamp("This error occurred in heating mode.");
								} else {
									ShowRecurringWarningErrorAtEnd(HeatPump.Type + " \"" + HeatPump.Name + "\":  Iteration limit exceeded in heating mode warning continues. speed ratio statistics follow.", HeatPump.IterLimitErrIndex1, SpeedRatio, SpeedRatio);
								}
							}
						} else if ( SolFla == -2 ) {
							SpeedRatio = max(0.0, min(1.0, (SetPointTemp - LowSpeedTankTemp) / (NewTankTemp - LowSpeedTankTemp)));
							if (!WarmupFlag) {
								++HeatPump.RegulaFalsiFailedNum1;
								if (HeatPump.RegulaFalsiFailedNum1 == 1) {
									ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
									ShowContinueError("Heat pump water heater speed ratio calculation failed: speed ratio limits " "of 0 to 1 exceeded. speed ratio used = " + RoundSigDigits(SpeedRatio, 3));
									ShowContinueError("Please send this information to the EnergyPlus support group.");
									ShowContinueErrorTimeStamp("This error occured in heating mode.");
								} else {
									ShowRecurringWarningErrorAtEnd(HeatPump.Type + " \"" + HeatPump.Name + "\":  Speed ratio calculation failed in heating mode warning continues. Speed ratio statistics follow.", HeatPump.RegulaFalsiFailedIndex1, SpeedRatio, SpeedRatio);
								}
							}
						}
					} else {
						SpeedNum = MaxSpeedNum;
						SpeedRatio = 1.0;
					}

					HPPartLoadRatio = 1.0;
					SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio,
						RhoWater, MdotWater, FirstHVACIteration);
					SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum,
						CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);


					CondenserDeltaT = Node(HPWaterOutletNode).Temp - Node(HPWaterInletNode).Temp;

					//           move the full load outlet temperature rate to the water heater structure variables
					//           (water heaters source inlet node temperature/mdot are set in Init, set it here after CalcHPWHDXCoil has been called)
					WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(HPWaterInletNode).Temp + CondenserDeltaT;
					//				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

					//           this CALL does not update node temps, must use WaterThermalTank variables
					// select tank type
					{ auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
					if (SELECT_CASE_var1 == MixedWaterHeater) {
						CalcWaterThermalTankMixed(WaterThermalTankNum);
						NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
					} else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
						CalcWaterThermalTankStratified(WaterThermalTankNum);
						NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, HeatPump.ControlSensorLocation);
					}}
					//update inlet temp
					Node(HPWaterInletNode).Temp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
					if (std::abs(Node(HPWaterInletNode).Temp - HPWHCondInletNodeLast) < SmallTempDiff) break;
					HPWHCondInletNodeLast = Node(HPWaterInletNode).Temp;
				}

			} else {
				// Set the PLR to 1 if we're not going to reach setpoint during this timestep.
				HPPartLoadRatio = 1.0;
			}
		}

		// set air-side mass flow rate for final calculation
		if ( InletAirMixerNode > 0 ) {
			Node( InletAirMixerNode ).MassFlowRate = MdotAir * HPPartLoadRatio;
			Node( HPAirInletNode ).MassFlowRate = MdotAir * HPPartLoadRatio * ( 1.0 - MixerInletAirSchedule );
			Node( OutdoorAirNode ).MassFlowRate = MdotAir * HPPartLoadRatio * MixerInletAirSchedule;
			//   IF HPWH is off, pass zone node conditions through HPWH air-side
			if ( HPPartLoadRatio == 0 ) Node( InletAirMixerNode ) = Node( HPAirInletNode );
		} else {
			if ( OutdoorAirNode == 0 ) {
				Node( HPAirInletNode ).MassFlowRate = MdotAir * HPPartLoadRatio;
			} else {
				Node( OutdoorAirNode ).MassFlowRate = MdotAir * HPPartLoadRatio;
			}
		}
		if ( HPPartLoadRatio == 0 ) Tank.SourceInletTemp = Tank.SourceOutletTemp;

		// set water-side mass flow rate for final calculation
		Node(HPWaterInletNode).MassFlowRate = MdotWater * HPPartLoadRatio;

		if (MaxSpeedNum > 0) {

			// it is important to use MdotAir to reset the notes, otherwise, could fail to converge
			if (InletAirMixerNode > 0) {
				Node(InletAirMixerNode).MassFlowRateMax = MdotAir;
				Node(InletAirMixerNode).MassFlowRateMaxAvail = MdotAir;
			} else {
				if (OutdoorAirNode == 0) {
					Node(HPAirInletNode).MassFlowRateMax = MdotAir;
					Node(HPAirInletNode).MassFlowRateMaxAvail = MdotAir;
				} else {
					Node(OutdoorAirNode).MassFlowRateMax = MdotAir;
					Node(OutdoorAirNode).MassFlowRateMaxAvail = MdotAir;
				}
			}

			//   set the max mass flow rate for outdoor fans
			Node(HeatPump.FanOutletNode).MassFlowRateMax = MdotAir;

			// pass node information using resulting PLR
			if (HeatPump.FanPlacement == BlowThru) {
				//   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum,
					CycFanCycCoil, EMP1, EMP2, EMP3, CompOp, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum,
					CycFanCycCoil, EMP1, EMP2, EMP3, CompOp, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
			} else {
				//   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
				SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum,
					CycFanCycCoil, EMP1, EMP2, EMP3, CompOp, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				SimVariableSpeedCoils(HeatPump.DXCoilName, HeatPump.DXCoilNum,
					CycFanCycCoil, EMP1, EMP2, EMP3, CompOp, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
			}

		} else { // single speed

			// pass node information using resulting PLR
			if (HeatPump.FanPlacement == BlowThru) {
				//   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
			} else {
				//   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
				SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
				SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
				SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
			}

		}
		// set HPWH outlet node equal to the outlet air splitter node conditions if outlet air splitter node exists
		if ( OutletAirSplitterNode > 0 ) {
			Node( HPAirOutletNode ) = Node( OutletAirSplitterNode );
			Node( ExhaustAirNode ) = Node( OutletAirSplitterNode );
		}

		// Check schedule to divert air-side cooling to outdoors.
		if ( HeatPump.OutletAirSplitterSchPtr > 0 ) {
			OutletAirSplitterSch = GetCurrentScheduleValue( HeatPump.OutletAirSplitterSchPtr );
			Node( HPAirOutletNode ).MassFlowRate = MdotAir * HPPartLoadRatio * ( 1.0 - OutletAirSplitterSch );
			Node( ExhaustAirNode ).MassFlowRate = MdotAir * HPPartLoadRatio * OutletAirSplitterSch;
		}

		HeatPump.HeatingPLR = HPPartLoadRatio;
		HeatPump.OnCycParaFuelRate = HeatPump.OnCycParaLoad * HPPartLoadRatio;
		HeatPump.OnCycParaFuelEnergy = HeatPump.OnCycParaFuelRate * TimeStepSys * SecInHour;
		HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * ( 1.0 - HPPartLoadRatio );
		HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * TimeStepSys * SecInHour;

		{ auto const SELECT_CASE_var( HeatPump.InletAirConfiguration );

		//   no sensible capacity to zone for outdoor and scheduled HPWH
		if ( SELECT_CASE_var == AmbientTempOutsideAir ) {
			HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
			HeatPump.HPWaterHeaterLatentCapacity = 0.0;

		} else if ( SELECT_CASE_var == AmbientTempSchedule ) {
			HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
			HeatPump.HPWaterHeaterLatentCapacity = 0.0;

			//   calculate sensible capacity to zone for inlet air configuration equals Zone Only or Zone And Outdoor Air configurations
		} else {
			CpAir = PsyCpAirFnWTdb( Node( HPAirInletNode ).HumRat, Node( HPAirInletNode ).Temp );

			//     add parasitics to zone heat balance if parasitic heat load is to zone otherwise neglect parasitics
			if ( HeatPump.ParasiticTempIndicator == AmbientTempZone ) {
				HeatPump.HPWaterHeaterSensibleCapacity = ( Node( HPAirOutletNode ).MassFlowRate * CpAir * ( Node( HPAirOutletNode ).Temp - Node( HPAirInletNode ).Temp ) ) + HeatPump.OnCycParaFuelRate + HeatPump.OffCycParaFuelRate;
			} else {
				HeatPump.HPWaterHeaterSensibleCapacity = Node( HPAirOutletNode ).MassFlowRate * CpAir * ( Node( HPAirOutletNode ).Temp - Node( HPAirInletNode ).Temp );
			}

			HeatPump.HPWaterHeaterLatentCapacity = Node( HPAirOutletNode ).MassFlowRate * ( Node( HPAirOutletNode ).HumRat - Node( HPAirInletNode ).HumRat );

		}}

	}

	void
	SetVSHPWHFlowRates(
		int const WaterThermalTankNum, // Water Heater tank being simulated
		int const HPNum,//index of heat pump coil
		int const SpeedNum,//upper speed number
		Real64 const SpeedRatio,//interpolation ration between upper and lower speed
		Real64 const WaterDens,// tank water density
		Real64 & MdotWater, // water flow rate
		bool const FirstHVACIteration
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         B.Shen, ORNL, 12/2014
		//       DATE WRITTEN   May 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  set water and air flow rates driven by the variable-speed HPWH coil
		//  calculate resultant HPWH coil output

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using DataLoopNode::Node;
		using Fans::SimulateFanComponents;

		int DXCoilAirInletNode; // Inlet air node number of DX coil
		int HPWaterInletNode; // HP condenser water inlet node number
		int SpeedLow; //lower speed level

		SpeedLow = SpeedNum - 1;
		if (SpeedLow < 1) SpeedLow = 1;

		HPWaterInletNode = HPWaterHeater(HPNum).CondWaterInletNode;
		DXCoilAirInletNode = HPWaterHeater(HPNum).DXCoilAirInletNode;

		HPWaterHeater(HPNum).OperatingWaterFlowRate =
			HPWaterHeater(HPNum).HPWHWaterVolFlowRate(SpeedNum)* SpeedRatio +
			HPWaterHeater(HPNum).HPWHWaterVolFlowRate(SpeedLow)* (1.0 - SpeedRatio);
		HPWaterHeater(HPNum).OperatingAirFlowRate =
			HPWaterHeater(HPNum).HPWHAirVolFlowRate(SpeedNum) * SpeedRatio +
			HPWaterHeater(HPNum).HPWHAirVolFlowRate(SpeedLow)* (1.0 - SpeedRatio);
		MdotAir = HPWaterHeater(HPNum).HPWHAirMassFlowRate(SpeedNum) * SpeedRatio +
			HPWaterHeater(HPNum).HPWHAirMassFlowRate(SpeedLow)* (1.0 - SpeedRatio);
		MdotWater = HPWaterHeater(HPNum).OperatingWaterFlowRate * WaterDens;

		Node(DXCoilAirInletNode).MassFlowRate = MdotAir;
		Node(HPWaterInletNode).MassFlowRate = MdotWater;
		WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater;

		if (HPWaterHeater(HPNum).InletAirMixerNode > 0) {
			Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRate = MdotAir;
			Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRateMaxAvail = MdotAir;
		} else {
			if (HPWaterHeater(HPNum).OutsideAirNode == 0) {
				Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRate = MdotAir;
				Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRateMaxAvail = MdotAir;
			} else {
				Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRate = MdotAir;
				Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRateMaxAvail = MdotAir;
			}
		}

		// put fan component first, regardless placement, to calculate fan power
		SimulateFanComponents(HPWaterHeater(HPNum).FanName,
			FirstHVACIteration, HPWaterHeater(HPNum).FanNum);
	}

	Real64
	PLRResidualIterSpeed(
		Real64 const SpeedRatio, // speed ratio between two speed levels
		Array1< Real64 > const & Par //
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         B.Shen, ORNL, 12/2014
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired tank temp - actual tank temp), when iterating speed ration between two speed levels
		//  HP water heater output depends on the speed ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls residuals to get tank temperature at the given speed ratio between a lower and an upper speed levels
		//  and calculates the residual as defined respectively for MixedWaterHeater or StratifiedWaterHeater

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using DataLoopNode::Node;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using DataHVACGlobals::CycFanCycCoil;

		int WaterThermalTankNum; // index of water heater
		Real64 NewTankTemp( 0 ); // resulting tank temperature [C]
		int SpeedNum;
		int HPNum;
		Real64 MdotWater(0);
		Real64 RhoWater;
		int HPWaterInletNode;
		int HPWaterOutletNode;
		Real64 CondenserDeltaT;
		Real64 PLRResidualIterSpeed;
		bool FirstHVACIteration; // FirstHVACIteration flag
		Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); //place holder to calling variable-speed coil function

		WaterThermalTankNum = int( Par(1) );
		HPNum = int( Par(2) );
		SpeedNum = int( Par(3) );
		HPWaterInletNode = int( Par(4) );
		HPWaterOutletNode = int( Par(5) );
		RhoWater = Par(6);
		WaterThermalTank(WaterThermalTankNum).Mode = int( Par(8) );
		// FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par(9) == 1.0 );

		HPPartLoadRatio = 1.0;
		SetVSHPWHFlowRates(WaterThermalTankNum, HPNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
		SimVariableSpeedCoils(HPWaterHeater(HPNum).DXCoilName, HPWaterHeater(HPNum).DXCoilNum,
			CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);

		CondenserDeltaT = Node(HPWaterOutletNode).Temp - Node(HPWaterInletNode).Temp;

		//           move the full load outlet temperature rate to the water heater structure variables
		//           (water heaters source inlet node temperature/mdot are set in Init, set it here after CalcHPWHDXCoil has been called)
		WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(HPWaterInletNode).Temp + CondenserDeltaT;
		//				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

		//           this CALL does not update node temps, must use WaterThermalTank variables
		// select tank type
		{ auto const SELECT_CASE_var1(HPWaterHeater(HPNum).TankTypeNum);
		if (SELECT_CASE_var1 == MixedWaterHeater) {
			CalcWaterThermalTankMixed(WaterThermalTankNum);
			NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
		} else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
			CalcWaterThermalTankStratified(WaterThermalTankNum);
			NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, HPWaterHeater(HPNum).ControlSensorLocation);
		}}

		PLRResidualIterSpeed = Par(7) - NewTankTemp;
		return PLRResidualIterSpeed;
	}

	Real64
	PLRResidualMixedTank(
		Real64 const HPPartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = HP set point temperature [C]
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   May 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired tank temp - actual tank temp)
		//  HP water heater output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcWaterThermalTankMixed to get tank temperature at the given part load ratio (source water mass flow rate)
		//  and calculates the residual as defined above

		// REFERENCES:

		// USE STATEMENTS:
		// Using/Aliasing
		
		// Return value
		Real64 PLRResidualMixedTank;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = tank mode
		// par(3) = water heater num
		// par(4) = FirstHVACIteration
		// par(5) = MdotWater

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WaterThermalTankNum; // index of water heater
		Real64 NewTankTemp; // resulting tank temperature [C]
		bool FirstHVACIteration; // FirstHVACIteration flag

		WaterThermalTankNum = int( Par( 3 ) );
		WaterThermalTank( WaterThermalTankNum ).Mode = int( Par( 2 ) );
		WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = Par( 5 ) * HPPartLoadRatio;
		// FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 4 ) == 1.0 );
		CalcWaterThermalTankMixed( WaterThermalTankNum );
		NewTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;
		PLRResidualMixedTank = Par( 1 ) - NewTankTemp;
		return PLRResidualMixedTank;

	}

	Real64
	PLRResidualStratifiedTank(
		Real64 const HPPartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = HP set point temperature [C]
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         B.Griffith,  Richard Raustad
		//       DATE WRITTEN   Jan 2012
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired tank temp - actual tank temp)
		//  HP water heater output depends on the part load ratio which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		//  Calls CalcWaterThermalTankStratified to get tank temperature at the given part load ratio (source water mass flow rate)
		//  and calculates the residual as defined above

		// REFERENCES:

		// USE STATEMENTS:
		// Using/Aliasing
		
		// Return value
		Real64 PLRResidualStratifiedTank;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = tank mode
		// par(3) = water heater num
		// par(4) = FirstHVACIteration
		// par(5) = MdotWater

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WaterThermalTankNum; // index of water heater
		Real64 NewTankTemp; // resulting tank temperature [C]
		bool FirstHVACIteration; // FirstHVACIteration flag

		WaterThermalTankNum = int( Par( 3 ) );
		WaterThermalTank( WaterThermalTankNum ).Mode = int( Par( 2 ) );
		WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = Par( 5 ) * HPPartLoadRatio;
		// FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
		FirstHVACIteration = ( Par( 4 ) == 1.0 );
		CalcWaterThermalTankStratified( WaterThermalTankNum );
		NewTankTemp = FindStratifiedTankSensedTemp( WaterThermalTankNum, HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).ControlSensorLocation );
		PLRResidualStratifiedTank = Par( 1 ) - NewTankTemp;
		return PLRResidualStratifiedTank;

	}

	Real64
	PlantMassFlowRatesFunc(
		int const WaterThermalTankNum,
		int const InNodeNum,
		bool const FirstHVACIteration,
		int const WaterThermalTankSide,
		int const PlantLoopSide,
		bool const EP_UNUSED( PlumbedInSeries ), // !unused1208
		int const BranchControlType,
		Real64 const OutletTemp,
		Real64 const DeadBandTemp,
		Real64 const SetPointTemp
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// collect routines for setting flow rates for Water heaters
		// with plant connections.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using namespace DataBranchAirLoopPlant;
		using ScheduleManager::GetCurrentScheduleValue;

		// Return value
		Real64 PlantMassFlowRatesFunc;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const PassingFlowThru( 1 );
		int const MaybeRequestingFlow( 2 );
		int const ThrottlingFlow( 3 );

		// FUNCTION BLOCK SPECIFICATIONS:
		// na

		// FUNCTION TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CurrentMode;
		Real64 MassFlowRequest( 0.0 );
		bool NeedsHeat;
		bool NeedsCool;
		Real64 FlowResult( 0.0 );
		bool ScheduledAvail;
		Real64 AltSetpointTemp;
		Real64 AltDeadBandTemp;

		NeedsHeat = false; // init
		NeedsCool = false; // init

		// determine current mode.  there are three possible
		//  1.  passing thru what was given to inlet node
		//  2.  potentially making a flow request
		//  3.  throttling flow in response to Plant's restrictions (MassFlowRateMaxAvail)
		// init default mode to passing thru
		CurrentMode = PassingFlowThru; // default

		if ( PlantLoopSide == DemandSupply_No ) {
			CurrentMode = PassingFlowThru;
		} else if ( PlantLoopSide == SupplySide ) {
			// If FlowLock is False (0), the tank sets the plant loop mdot
			// If FlowLock is True (1),  the new resolved plant loop mdot is used
			if ( WaterThermalTank( WaterThermalTankNum ).UseCurrentFlowLock == 0 ) {
				CurrentMode = PassingFlowThru;
				if ( ( WaterThermalTank( WaterThermalTankNum ).UseSideLoadRequested > 0.0 ) && ( WaterThermalTankSide == UseSide ) ) {
					CurrentMode = MaybeRequestingFlow;
				}
			} else {
				CurrentMode = PassingFlowThru;

			}
			if ( WaterThermalTankSide == SourceSide ) {
				CurrentMode = MaybeRequestingFlow;
			}
		} else if ( PlantLoopSide == DemandSide ) {
			//  1.  pass thru is default
			CurrentMode = PassingFlowThru;

			//  2.  Might be Requesting Flow.
			if ( FirstHVACIteration ) {
				if ( BranchControlType == ControlType_Bypass ) {
					CurrentMode = PassingFlowThru;
				} else {
					// IF (.not. PlumbedInSeries) THEN
					CurrentMode = MaybeRequestingFlow;
					// ELSE
					//   CurrentMode = PassingFlowThru
					// ENDIF
				}
				// ENDIF
			} else { //(.not. FirstHVACIteration)
				if ( BranchControlType == ControlType_Bypass ) {
					CurrentMode = PassingFlowThru;
				} else {
					// 3.  ThrottlingFlow
					//  IF (.not. PlumbedInSeries) THEN
					CurrentMode = ThrottlingFlow;
					//  ELSE
					//    CurrentMode = PassingFlowThru
					//  ENDIF

				}
			}
		}

		// evaluate Availability schedule,
		ScheduledAvail = true;
		if ( WaterThermalTankSide == UseSide ) {
			//    IF (WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum > 0) Then
			if ( GetCurrentScheduleValue( WaterThermalTank( WaterThermalTankNum ).UseSideAvailSchedNum ) == 0.0 ) {
				ScheduledAvail = false;
			}
			//    ENDIF
		} else if ( WaterThermalTankSide == SourceSide ) {
			//    IF (WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum > 0) Then
			if ( GetCurrentScheduleValue( WaterThermalTank( WaterThermalTankNum ).SourceSideAvailSchedNum ) == 0.0 ) {
				ScheduledAvail = false;
			}
			//    ENDIF
		}

		// now act based on current mode
		{ auto const SELECT_CASE_var( CurrentMode );

		if ( SELECT_CASE_var == PassingFlowThru ) {
			if ( ! ScheduledAvail ) {
				FlowResult = 0.0;
			} else {
				FlowResult = Node( InNodeNum ).MassFlowRate;
			}

		} else if ( SELECT_CASE_var == ThrottlingFlow ) {
			// first determine what mass flow would be if it is to requested
			if ( ! ScheduledAvail ) {
				MassFlowRequest = 0.0;
			} else {
				if ( WaterThermalTankSide == UseSide ) {
					MassFlowRequest = WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax;
				} else if ( WaterThermalTankSide == SourceSide ) {
					MassFlowRequest = WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax;
				} else {
					assert( false );
				}
			}

			// next determine if tank temperature is such that source side flow might be requested
			if ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
				if ( OutletTemp < DeadBandTemp ) {
					NeedsHeat = true;
				} else if ( ( OutletTemp >= DeadBandTemp ) && ( OutletTemp < SetPointTemp ) ) {
					// inside the deadband, use saved mode from water heater calcs
					if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == HeatMode ) {
						NeedsHeat = true;
					} else if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == FloatMode ) {
						NeedsHeat = false;
					}
				} else if ( OutletTemp >= SetPointTemp ) {
					NeedsHeat = false;
				}
			} else { // is a chilled water tank so flip logic
				if ( OutletTemp > DeadBandTemp ) {
					NeedsCool = true;
				} else if ( ( OutletTemp <= DeadBandTemp ) && ( OutletTemp > SetPointTemp ) ) {
					// inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
					if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == MixedChilledWaterStorage ) {
						if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == CoolMode ) {
							NeedsCool = true;
						} else if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == FloatMode ) {
							NeedsCool = false;
						}
					} else if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedChilledWaterStorage ) {
						NeedsCool = true;

					}

				} else if ( OutletTemp <= SetPointTemp ) {
					NeedsCool = false;
				}
			}

			if ( MassFlowRequest > 0.0 ) {
				if ( WaterThermalTankSide == UseSide ) {
					FlowResult = MassFlowRequest;
				} else if ( WaterThermalTankSide == SourceSide ) {
					if ( NeedsHeat || NeedsCool ) {
						FlowResult = MassFlowRequest;
					} else {
						FlowResult = 0.0;
					}
				} else {
					assert( false );
				}
			} else {
				FlowResult = 0.0;
			}

			// now throttle against MassFlowRateMaxAvail, MassFlowRateMinAvail, MassFlowRateMax, and MassFlowRateMin
			// see notes about reverse dd compliance (specifically 5ZoneWaterSystems file)
			FlowResult = max( Node( InNodeNum ).MassFlowRateMinAvail, FlowResult ); // okay for compliance (reverse dd)
			FlowResult = max( Node( InNodeNum ).MassFlowRateMin, FlowResult ); // okay for compliance (reverse dd)
			FlowResult = min( Node( InNodeNum ).MassFlowRateMaxAvail, FlowResult );
			//=> following might take out of reverse dd compliance
			FlowResult = min( Node( InNodeNum ).MassFlowRateMax, FlowResult );

			//DSU> use SetComponentFlowRate for above?

		} else if ( SELECT_CASE_var == MaybeRequestingFlow ) {

			// first determine what mass flow would be if it is to requested
			if ( ! ScheduledAvail ) {
				MassFlowRequest = 0.0;
			} else {
				if ( WaterThermalTankSide == UseSide ) {
					if ( ( WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) && ( WaterThermalTank( WaterThermalTankNum ).UseSideLoadRequested > 0.0 ) ) {
						MassFlowRequest = WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax;
					} else if ( ( WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) && ( WaterThermalTank( WaterThermalTankNum ).UseSideLoadRequested == 0.0 ) ) {
						MassFlowRequest = 0.0;
					} else {
						MassFlowRequest = WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax;
					}

				} else if ( WaterThermalTankSide == SourceSide ) {
					MassFlowRequest = WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax;
				}
			}

			if ( WaterThermalTankSide == SourceSide ) { // temperature dependent controls for indirect heating/cooling
				if ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
					// next determine if tank temperature is such that flow is requested depending on mode
					if ( WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode == SourceSideIndirectHeatPrimarySetpoint ) {
						if ( OutletTemp < DeadBandTemp ) {
							NeedsHeat = true;
						} else if ( ( OutletTemp >= DeadBandTemp ) && ( OutletTemp < SetPointTemp ) ) {
							// inside the deadband, use saved mode from water heater calcs
							if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == HeatMode ) {
								NeedsHeat = true;
							} else if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == FloatMode ) {
								NeedsHeat = false;
							}

						} else if ( OutletTemp >= SetPointTemp ) {
							NeedsHeat = false;
						}
					} else if ( WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode == SourceSideIndirectHeatAltSetpoint ) {
						// get alternate setpoint
						AltSetpointTemp = GetCurrentScheduleValue( WaterThermalTank( WaterThermalTankNum ).SourceSideAltSetpointSchedNum );
						AltDeadBandTemp = AltSetpointTemp - WaterThermalTank( WaterThermalTankNum ).DeadBandDeltaTemp;
						if ( OutletTemp < AltDeadBandTemp ) {
							NeedsHeat = true;
						} else if ( ( OutletTemp >= AltDeadBandTemp ) && ( OutletTemp < AltSetpointTemp ) ) {
							// inside the deadband, use saved mode from water heater calcs
							if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == HeatMode ) {
								NeedsHeat = true;
							} else if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == FloatMode ) {
								NeedsHeat = false;
							}

						} else if ( OutletTemp >= AltSetpointTemp ) {
							NeedsHeat = false;
						}
					} else if ( WaterThermalTank( WaterThermalTankNum ).SourceSideControlMode == SourceSideStorageTank ) {
						if ( OutletTemp < WaterThermalTank( WaterThermalTankNum ).TankTempLimit ) {
							NeedsHeat = true;
						} else {
							NeedsHeat = false;
						}

					}
				} else { // is a chilled water tank so flip logic
					if ( OutletTemp > DeadBandTemp ) {
						NeedsCool = true;
					} else if ( ( OutletTemp <= DeadBandTemp ) && ( OutletTemp > SetPointTemp ) ) {
						// inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
						if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == MixedChilledWaterStorage ) {
							if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == CoolMode ) {
								NeedsCool = true;
							} else if ( WaterThermalTank( WaterThermalTankNum ).SavedMode == FloatMode ) {
								NeedsCool = false;
							}
						} else if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedChilledWaterStorage ) {
							NeedsCool = true;
						}
					} else if ( OutletTemp >= SetPointTemp ) {
						NeedsCool = false;
					}

				} // chilled water

				if ( MassFlowRequest > 0.0 ) {
					if ( NeedsHeat || NeedsCool ) {
						FlowResult = MassFlowRequest;
					} else {
						FlowResult = 0.0;
					}
				} else {
					FlowResult = 0.0;
				}
			} else { // end source side, begin use side
				if ( MassFlowRequest > 0.0 ) {
					FlowResult = MassFlowRequest;
				} else {
					FlowResult = 0.0;
				}
			}
			//    IF (FirstHVACIteration) Then
			//      Node(InNodeNum)%MassFlowRateMaxAvail = FlowResult
			//      Node(InNodeNum)%MassFlowRateMinAvail = 0.0D0
			//    ENDIF

		}}

		PlantMassFlowRatesFunc = FlowResult;

		return PlantMassFlowRatesFunc;

	}

	void
	MinePlantStructForInfo( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get information from plant loop data structure
		// check what we can learn from plant structure against user inputs

		// METHODOLOGY EMPLOYED:
		// looping

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PlantLoopNum; // Used for looking up plant info
		int LoopSideNum; // Used for looking up plant info
		//unused  INTEGER             :: BranchNum               ! Used for looking up plant info
		//  INTEGER             :: CompNum                 ! Used for looking up plant info
		int SplitNum; // used for checking series parallel in plant
		int UseInletNode; // Water heater use inlet node number
		int SourceInletNode; // Water heater source inlet node number
		bool ErrorsFound;

		ErrorsFound = false;

		//IF (WaterThermalTank(WaterThermalTankNum)%PlantStructureCheck .AND. ALLOCATED(PlantLoop)) THEN
		if ( allocated( PlantLoop ) && WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum > 0 ) {

			// check plant structure for useful data.

			UseInletNode = WaterThermalTank( WaterThermalTankNum ).UseInletNode;
			PlantLoopNum = WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum;
			LoopSideNum = WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide;

			if ( ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantSizNum == 0 ) ) {
				ShowSevereError( "Water heater = " + WaterThermalTank( WaterThermalTankNum ).Name + " for autosizing Use side flow rate, did not find Sizing:Plant object " + PlantLoop( PlantLoopNum ).Name );
				ErrorsFound = true;
			}
			//Is this wh Use side plumbed in series (default) or are there other branches in parallel?
			if ( allocated( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Splitter ) ) {
				for ( SplitNum = 1; SplitNum <= PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).NumSplitters; ++SplitNum ) {
					if ( any_eq( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Splitter( SplitNum ).NodeNumOut, UseInletNode ) ) { // this wh is on the splitter
						if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Splitter( SplitNum ).TotalOutletNodes > 1 ) {
							WaterThermalTank( WaterThermalTankNum ).UseSideSeries = false;
						}
					}
				}
			}
		}

		if ( allocated( PlantLoop ) && WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum > 0 ) {
			SourceInletNode = WaterThermalTank( WaterThermalTankNum ).SourceInletNode;
			PlantLoopNum = WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum;
			LoopSideNum = WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide;
			// was user's input correct for plant loop name?
			if ( ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantSizNum == 0 ) && ( WaterThermalTank( WaterThermalTankNum ).DesuperheaterNum == 0 ) && ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum == 0 ) ) {
				ShowSevereError( "Water heater = " + WaterThermalTank( WaterThermalTankNum ).Name + "for autosizing Source side flow rate, did not find Sizing:Plant object " + PlantLoop( PlantLoopNum ).Name );
				ErrorsFound = true;
			}
			//Is this wh Source side plumbed in series (default) or are there other branches in parallel?
			if ( allocated( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Splitter ) ) {
				for ( SplitNum = 1; SplitNum <= PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).NumSplitters; ++SplitNum ) {
					if ( any_eq( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Splitter( SplitNum ).NodeNumOut, SourceInletNode ) ) { // this wh is on the splitter
						if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Splitter( SplitNum ).TotalOutletNodes > 1 ) {
							WaterThermalTank( WaterThermalTankNum ).SourceSideSeries = false;
						}
					}
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding water heater input errors cause program termination" );
		}

	}

	void
	SizeSupplySidePlantConnections(
		int const WaterThermalTankNum,
		Optional_int_const LoopNum,
		Optional_int_const LoopSideNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing water heater plant connection flow rates
		// on the supply that have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// This routine is called later in the simulation than the sizing routine for the demand side
		//  because the simulation needs to be further along before the needed data are available.
		// For water heaters sides on Supply LoopSide, obtains hot water flow rate from the plant sizing array
		//  (adapted approach from boiler sizing routines)

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataHVACGlobals::SmallWaterVolFlow;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using DataPlant::PlantFinalSizesOkayToReport;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeSupplySidePlantConnections" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		bool ErrorsFound; // If errors detected in input
		static int DummyWaterIndex( 1 );
		Real64 rho; // temporary fluid density
		int tmpLoopNum;
		int tmpLoopSideNum;
		Real64 tmpUseDesignVolFlowRate; // local use side design flow rate
		Real64 tmpSourceDesignVolFlowRate; // local source side design flow rate

		PltSizNum = 0;
		ErrorsFound = false;
		tmpUseDesignVolFlowRate = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate;
		tmpSourceDesignVolFlowRate = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate;

		if ( ! present( LoopSideNum ) ) {
			tmpLoopSideNum = SupplySide;
		} else {
			tmpLoopSideNum = LoopSideNum;
		}
		if ( ! present( LoopNum ) ) {
			tmpLoopNum = WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum;
		} else {
			tmpLoopNum = LoopNum;
		}

		if ( ( WaterThermalTank( WaterThermalTankNum ).UseInletNode > 0 ) && ( tmpLoopNum == WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ) ) {
			if ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized ) {
				PltSizNum = WaterThermalTank( WaterThermalTankNum ).UseSidePlantSizNum;
				if ( PltSizNum > 0 ) { // we have a Plant Sizing Object
					if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide == SupplySide ) {
						if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate;
							} else {
								tmpUseDesignVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate;
							}
						} else {
							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = 0.0;
							} else {
								tmpUseDesignVolFlowRate = 0.0;
							}
						}
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Use Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Initial Use Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToFinalize ) {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).UseInletNode, WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
						} else {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).UseInletNode, tmpUseDesignVolFlowRate );
						}

						rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
						if ( PlantFirstSizesOkayToFinalize ) {
							WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * rho;
						} else {
							WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
						}
					}
				} else {
					// do nothing
				} //plant sizing object
			} else {
				RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).UseInletNode, WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
				if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
				} else {
					rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
				}

				WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * rho;

			} //autosizing needed.
		} // connected to plant

		if ( ( WaterThermalTank( WaterThermalTankNum ).SourceInletNode > 0 ) && ( tmpLoopNum == WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ) ) {
			if ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized ) {
				PltSizNum = WaterThermalTank( WaterThermalTankNum ).SourceSidePlantSizNum;
				if ( PltSizNum > 0 ) {
					if ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide == SupplySide ) {
						if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate;
							} else {
								tmpSourceDesignVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate;
							}
						} else {
							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = 0.0;
							} else {
								tmpSourceDesignVolFlowRate = 0.0;
							}
						}
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Source Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Initial Source Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToFinalize ) {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
						} else {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, tmpSourceDesignVolFlowRate );
						}
						rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, RoutineName );
						if ( PlantFirstSizesOkayToFinalize ) {
							WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate * rho;
						} else {
							WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
						}
					} // plant loop allocation
				} else {
					// do nothing
				} //plant sizing object
			} else {
				if ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide == SupplySide ) {
					RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
					if ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum > 0 ) {
						rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, RoutineName );
					} else {
						rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
					}
					WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate * rho;
				}
			} //autosizing needed.
		} // connected to plant

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	SizeTankForDemandSide( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing water heater tank volume and heater
		//  as best we can at this point in simulation. (prior to demand side
		//  sizing that needs volume).

		// METHODOLOGY EMPLOYED:
		//  depending on the sizing design mode...

		// REFERENCES:
		// BA benchmark report for residential design mode

		// Using/Aliasing
		using InputProcessor::SameString;
		using DataHeatBalance::Zone;
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataGlobals::Pi;
		using DataHVACGlobals::SmallWaterVolFlow;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using namespace OutputReportPredefined;
		using DataPlant::PlantFinalSizesOkayToReport;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeTankForDemandSide" );
		Real64 const GalTocubicMeters( 0.0037854 );
		Real64 const kBtuPerHrToWatts( 293.1 );
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tstart; // initial tank temp for sizing.
		Real64 Tfinish; // final target temp for sizing
		Real64 SumPeopleAllZones;
		Real64 SumFloorAreaAllZones;
		//unused  INTEGER   :: CollectorNum  ! do loop index
//		static bool SizeVolume( false );
//		static bool SizeMaxCapacity( false );
		Real64 rho;
		Real64 Cp;
		static int DummyWaterIndex( 1 );
		Real64 tmpTankVolume; // local temporary for tank volume m3
		Real64 tmpMaxCapacity; // local temporary for heating capacity W
		static bool FuelTypeIsLikeGas( false );

		// local inits
		Tstart = 14.44;
		Tfinish = 57.22;

		tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Volume;
		tmpMaxCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity;

		{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode );

		if ( SELECT_CASE_var == SizeNotSet ) {

		} else if ( SELECT_CASE_var == SizePeakDraw ) {

		} else if ( SELECT_CASE_var == SizeResidentialMin ) {

			// assume can propagate rules for gas to other fuels.
			FuelTypeIsLikeGas = false;
			if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Gas" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Diesel" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Gasoline" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Coal" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "FuelOil#1" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "FuelOil#2" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Propane" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Steam" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "OtherFuel1" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "OtherFuel2" ) ) {
				FuelTypeIsLikeGas = true;
			} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "DistrictHeating" ) ) {
				FuelTypeIsLikeGas = true;
			}

			if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 1 ) {
				if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
					if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 20.0 * GalTocubicMeters;
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 2.5 * 1000.0; //2.5 kW
				} else if ( FuelTypeIsLikeGas ) {
					if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 20.0 * GalTocubicMeters;
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; //27kBtu/hr
				}

			} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 2 ) {
				if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms <= 1.5 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 3.5 * 1000.0; //3.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
					}
				} else if ( ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms > 1.5 ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms < 3.0 ) ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 4.5 * 1000.0; //4.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
					}
				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms >= 3.0 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
					}
				}
			} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 3 ) {
				if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms <= 1.5 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 4.5 * 1000.0; //4.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
					}
				} else if ( ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms > 1.5 ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms < 3.0 ) ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
					}
				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms >= 3.0 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; //38 kBtu/hr
					}
				}
			} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 4 ) {
				if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms <= 1.5 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
					}
				} else if ( ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms > 1.5 ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms < 3.0 ) ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; //38 kBtu/hr
					}
				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms >= 3.0 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 66.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; //38 kBtu/hr
					}
				}
			} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 5 ) {
				if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
					if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 66.0 * GalTocubicMeters;
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
				} else if ( FuelTypeIsLikeGas ) {
					if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; //47 kBtu/hr
				}
			} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms >= 6 ) {
				if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
					if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 66.0 * GalTocubicMeters;
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
				} else if ( FuelTypeIsLikeGas ) {
					if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
					if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; //50 kBtu/hr
				}
			}

			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
			}
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			}
		} else if ( SELECT_CASE_var == SizePerPerson ) {
			// how to get number of people?

			SumPeopleAllZones = sum( Zone.TotOccupants() );
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerPerson * SumPeopleAllZones;
			if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
			}

			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = SumPeopleAllZones * WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerPerson * ( Tfinish - Tstart ) * ( 1.0 / SecInHour ) * rho * Cp; //m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
			}
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			}
		} else if ( SELECT_CASE_var == SizePerFloorArea ) {

			SumFloorAreaAllZones = sum( Zone.FloorArea() );
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
			if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
			}

			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = SumFloorAreaAllZones * WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerArea * ( Tfinish - Tstart ) * ( 1.0 / SecInHour ) * rho * Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
			}
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			}
		} else if ( SELECT_CASE_var == SizePerUnit ) {

			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerUnit * WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfUnits;
			if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum > 0 ) {
				rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
			} else {
				rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
			}
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfUnits * WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerUnit * ( Tfinish - Tstart ) * ( 1.0 / SecInHour ) * rho * Cp; //m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
			}
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			}
		} else if ( SELECT_CASE_var == SizePerSolarColArea ) {

		}}

		// if stratified, might set height.
		if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedWaterHeater ) && PlantFirstSizesOkayToFinalize ) { // might set height
			if ( ( WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized ) && ( ! WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) ) {
				WaterThermalTank( WaterThermalTankNum ).Height = std::pow( ( 4.0 * WaterThermalTank( WaterThermalTankNum ).Volume * pow_2( WaterThermalTank( WaterThermalTankNum ).Sizing.HeightAspectRatio ) ) / Pi, 0.3333333333333333 );
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Height [m]", WaterThermalTank( WaterThermalTankNum ).Height );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Height [m]", WaterThermalTank( WaterThermalTankNum ).Height );
				}
				// check if autocalculate Use outlet and source inlet are still set to autosize by earlier
				if ( WaterThermalTank( WaterThermalTankNum ).UseOutletHeightWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).UseOutletHeight = WaterThermalTank( WaterThermalTankNum ).Height;
				}
				if ( WaterThermalTank( WaterThermalTankNum ).SourceInletHeightWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).SourceInletHeight = WaterThermalTank( WaterThermalTankNum ).Height;
				}
			}
		}

	}

	void
	SizeTankForSupplySide( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   February 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing water heater tank volume and heater
		//   at a later point in the simulation when more of the plant is ready.

		// METHODOLOGY EMPLOYED:
		//  depending on the sizing design mode...

		// REFERENCES:
		// BA benchmark report for residential design mode

		// Using/Aliasing
		using DataSizing::AutoSize;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using namespace OutputReportPredefined;
		using SolarCollectors::Collector;
		using SolarCollectors::NumOfCollectors;
		using DataSurfaces::Surface;
		using DataGlobals::Pi;
		using DataPlant::PlantFinalSizesOkayToReport;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeTankForSupplySide" );
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tstart; // initial tank temp for sizing.
		Real64 Tfinish; // final target temp for sizing
		int CollectorNum;
		Real64 rho;
		Real64 Cp;
		static int DummyWaterIndex( 1 );
		Real64 tmpTankVolume; // local temporary for tank volume m3
		Real64 tmpMaxCapacity; // local temporary for heating capacity W

		// local inits
		Tstart = 14.44;
		Tfinish = 57.22;

		tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Volume;
		tmpMaxCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity;

		{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode );

		if ( SELECT_CASE_var == SizePeakDraw ) {
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankDrawTime * WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * SecInHour; // hours | m3/s | (3600 s/1 hour)
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
			}
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
				if ( WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryTime > 0.0 ) {
					if ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum > 0 ) {
						rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, ( ( Tfinish + Tstart ) / 2.0 ), PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, RoutineName );
					} else {
						rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
					}
					tmpMaxCapacity = ( WaterThermalTank( WaterThermalTankNum ).Volume * rho * Cp * ( Tfinish - Tstart ) ) / ( WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryTime * SecInHour ); // m3 | kg/m3 | J/Kg/K | K | seconds
				} else {
					ShowFatalError( "SizeTankForSupplySide: Tank=\"" + WaterThermalTank( WaterThermalTankNum ).Name + "\", requested sizing for max capacity but entered Recovery Time is zero." );
				}
			}

			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			}
		} else if ( SELECT_CASE_var == SizePerSolarColArea ) {

			WaterThermalTank( WaterThermalTankNum ).Sizing.TotalSolarCollectorArea = 0.0;
			for ( CollectorNum = 1; CollectorNum <= NumOfCollectors; ++CollectorNum ) {
				WaterThermalTank( WaterThermalTankNum ).Sizing.TotalSolarCollectorArea += Surface( Collector( CollectorNum ).Surface ).Area;
			}

			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TotalSolarCollectorArea * WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerCollectorArea;
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 0.0;
			if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
			}
			if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			}
		}}

		if ( ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) && ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedWaterHeater ) && PlantFirstSizesOkayToFinalize ) { // might set height
			if ( ( WaterThermalTank( WaterThermalTankNum ).HeightWasAutoSized )
					&& ( ! WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) ) {
				WaterThermalTank( WaterThermalTankNum ).Height = std::pow( ( 4.0 * WaterThermalTank( WaterThermalTankNum ).Volume * pow_2( WaterThermalTank( WaterThermalTankNum ).Sizing.HeightAspectRatio ) ) / Pi, 0.3333333333333333 );
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Tank Height [m]", WaterThermalTank( WaterThermalTankNum ).Height );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
						"Initial Tank Height [m]", WaterThermalTank( WaterThermalTankNum ).Height );
				}
			}
		}

	}

	void
	SizeDemandSidePlantConnections( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   October 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing water heater plant connection flow rates
		// on the demand side that have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// For water heater sides on the Demand side, hot water flow rates are modeled entirely from user input data
		// because the plant loop is not yet set up nor is plant sizing info populated.
		// sizing is done by calculating an initial
		//  recovery rate that if continued would reheat tank in user specified amount of time.
		//  intial and final tank temperatures are 14.44 and reheat to 57.22 (values from CalcStandardRatings routine)

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataHVACGlobals::SmallWaterVolFlow;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using DataPlant::PlantFinalSizesOkayToReport;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeDemandSidePlantConnections" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TankVolume; // local variable for tank volume.
		Real64 Tpdesign; // plant sizing exit temperature
		Real64 Tstart; // initial tank temp for sizing.
		Real64 Tfinish; // final target temp for sizing
		Real64 tankRecoverhours; // parameter in sizing, hours to recover
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		bool ErrorsFound; // If errors detected in input

		Real64 eff; // temporary effectiveness value for heat exchanger inside tank

		static int DummyWaterIndex( 1 );
		Real64 rho;
		Real64 tmpUseDesignVolFlowRate; // local use side design flow rate
		Real64 tmpSourceDesignVolFlowRate; // local use side design flow rate

		tankRecoverhours = WaterThermalTank( WaterThermalTankNum ).SizingRecoveryTime;
		PltSizNum = 0;
		ErrorsFound = false;
		tmpUseDesignVolFlowRate = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate;
		tmpSourceDesignVolFlowRate = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate;

		if ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
			Tstart = 14.44;
			Tfinish = 57.22;
		} else {
			Tstart = 14.44;
			Tfinish = 9.0;
		}

		// determine tank volume to use for sizing.
		TankVolume = WaterThermalTank( WaterThermalTankNum ).Volume;
		if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
			TankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.NominalVolForSizingDemandSideFlow;
		}

		if ( WaterThermalTank( WaterThermalTankNum ).UseInletNode > 0 ) {
			if ( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRateWasAutoSized ) {
				PltSizNum = WaterThermalTank( WaterThermalTankNum ).UseSidePlantSizNum;
				if ( PltSizNum > 0 ) { // we have a Plant Sizing Object
					if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopSide == DemandSide ) {
						// probably shouldn't come here as Use side is unlikley to be on demand side (?)
						// but going to treat component with symetry so if connections are reversed it'll still work
						// choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
						// in user specified hours.
						//  using the plant inlet design temp for sizing.
						Tpdesign = PlantSizData( PltSizNum ).ExitTemp;
						eff = WaterThermalTank( WaterThermalTankNum ).UseEffectiveness;
						if ( ( Tpdesign >= 58.0 ) && ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) ) {
							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							} else {
								tmpUseDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							}
						} else if ( ( Tpdesign <= 8.0 ) && ( WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) ) {
							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							} else {
								tmpUseDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							}
						} else {
							if ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
								// plant sizing object design temperature is set too low throw warning.
								ShowSevereError( "Autosizing of Use side water heater design flow rate requires Sizing:Plant object to have an exit temperature >= 58C" );
								ShowContinueError( "Occurs for water heater object=" + WaterThermalTank( WaterThermalTankNum ).Name );
							} else {
								// plant sizing object design temperature is set too hi throw warning.
								ShowSevereError( "Autosizing of Use side chilled water tank design flow rate requires Sizing:Plant object to have an exit temperature <= 8C" );
								ShowContinueError( "Occurs for chilled water storage tank object=" + WaterThermalTank( WaterThermalTankNum ).Name );

							}
							ErrorsFound = true;
						}
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Use Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Initial Use Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToFinalize ) {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).UseInletNode, WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
						} else {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).UseInletNode, tmpUseDesignVolFlowRate );
						}
						rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
						if ( PlantFirstSizesOkayToFinalize ) {
							WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * rho;
						} else {
							WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
						}
					} // Demand side
				} else {
					// do nothing
				} //plant sizing object

			} else {
				// not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
				RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).UseInletNode, WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate );
				if ( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).UseSidePlantLoopNum ).FluidIndex, RoutineName );
				} else {
					rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
				}
				WaterThermalTank( WaterThermalTankNum ).PlantUseMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate * rho;
			} //autosizing needed.
		} // connected to plant

		if ( WaterThermalTank( WaterThermalTankNum ).SourceInletNode > 0 ) {
			if ( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRateWasAutoSized ) {
				PltSizNum = WaterThermalTank( WaterThermalTankNum ).SourceSidePlantSizNum;
				if ( PltSizNum > 0 ) {
					if ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopSide == DemandSide ) {
						//  choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
						// in user specified hours.
						//  using the plant inlet design temp for sizing.
						Tpdesign = PlantSizData( PltSizNum ).ExitTemp;
						eff = WaterThermalTank( WaterThermalTankNum ).SourceEffectiveness;
						if ( ( Tpdesign >= 58.0 ) && ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) ) {

							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							} else {
								tmpSourceDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							}
						} else if ( ( Tpdesign <= 8.0 ) && ( WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) ) {
							if ( PlantFirstSizesOkayToFinalize ) {
								WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							} else {
								tmpSourceDesignVolFlowRate = -1.0 * ( TankVolume / ( tankRecoverhours * SecInHour * eff ) ) * std::log( ( Tpdesign - Tfinish ) / ( Tpdesign - Tstart ) );
							}
						} else {
							if ( ! WaterThermalTank( WaterThermalTankNum ).IsChilledWaterTank ) {
								// plant sizing object design temperature is set too low throw warning.
								ShowSevereError( "Autosizing of Source side water heater design flow rate requires Sizing:Plant object to have an exit temperature >= 58C" );
								ShowContinueError( "Occurs for WaterHeater:Mixed object=" + WaterThermalTank( WaterThermalTankNum ).Name );
							} else {
								// plant sizing object design temperature is set too hi throw warning.
								ShowSevereError( "Autosizing of Source side chilled water tank design flow rate requires Sizing:Plant object to have an exit temperature <= 8C" );
								ShowContinueError( "Occurs for chilled water storage tank object=" + WaterThermalTank( WaterThermalTankNum ).Name );
							}
							ErrorsFound = true;
						}
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Source Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name,
								"Initial Source Side Design Flow Rate [m3/s]", WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
						}
						if ( PlantFirstSizesOkayToFinalize ) {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
						} else {
							RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, tmpSourceDesignVolFlowRate );
						}
						rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, RoutineName );
						if ( PlantFirstSizesOkayToFinalize ) {
							WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate * rho;
						} else {
							WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
						}
					} // demand side
				} else {
					// do nothing
				} //plant sizing object

			} else {
				// not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
				RegisterPlantCompDesignFlow( WaterThermalTank( WaterThermalTankNum ).SourceInletNode, WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate );
				if ( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum > 0 ) {
					rho = GetDensityGlycol( PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidName, InitConvTemp, PlantLoop( WaterThermalTank( WaterThermalTankNum ).SourceSidePlantLoopNum ).FluidIndex, RoutineName );
				} else {
					rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
				}
				WaterThermalTank( WaterThermalTankNum ).PlantSourceMassFlowRateMax = WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate * rho;
			} //autosizing needed.
		} // connected to plant

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	SizeStandAloneWaterHeater( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   October 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// allow autosizing of tank volume and heat capacity for stand alone tanks

		// METHODOLOGY EMPLOYED:
		// same as for plant connected water heaters, only draws are scheduled.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::AutoSize;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using ScheduleManager::GetScheduleMaxValue;
		using InputProcessor::SameString;
		using DataHeatBalance::Zone;
		using SolarCollectors::Collector;
		using SolarCollectors::NumOfCollectors;
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const GalTocubicMeters( 0.0037854 );
		Real64 const kBtuPerHrToWatts( 293.1 );
		static std::string const RoutineName( "SizeStandAloneWaterHeater" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpTankVolume; // local temporary for tank volume m3
		Real64 tmpMaxCapacity; // local temporary for heating capacity W
		Real64 Tstart; // initial tank temp for sizing.
		Real64 Tfinish; // final target temp for sizing
		static bool FuelTypeIsLikeGas( false );
		static int DummyWaterIndex( 1 );
		Real64 rho;
		Real64 Cp;
		Real64 DrawDesignVolFlowRate;
		Real64 SumPeopleAllZones;
		Real64 SumFloorAreaAllZones;
		int CollectorNum;

		// local inits
		Tstart = 14.44;
		Tfinish = 57.22;

		tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Volume;
		tmpMaxCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity;

		if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized || WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {

			{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).Sizing.DesignMode );

			if ( SELECT_CASE_var == SizePeakDraw ) {
				// get draw rate from maximum in schedule
				rho = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
				DrawDesignVolFlowRate = GetScheduleMaxValue( WaterThermalTank( WaterThermalTankNum ).FlowRateSchedule ) * WaterThermalTank( WaterThermalTankNum ).MassFlowRateMax / rho;

				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankDrawTime * DrawDesignVolFlowRate * SecInHour; // hours | m3/s | (3600 s/1 hour)
					WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					if ( WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryTime > 0.0 ) {
						rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
						Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );

						tmpMaxCapacity = ( WaterThermalTank( WaterThermalTankNum ).Volume * rho * Cp * ( Tfinish - Tstart ) ) / ( WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryTime * SecInHour ); // m3 | kg/m3 | J/Kg/K | K | seconds
					} else {
						ShowFatalError( "SizeStandAloneWaterHeater: Tank=\"" + WaterThermalTank( WaterThermalTankNum ).Name + "\", requested sizing for max capacity but entered Recovery Time is zero." );
					}
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}

			} else if ( SELECT_CASE_var == SizeResidentialMin ) {
				// assume can propagate rules for gas to other fuels.
				FuelTypeIsLikeGas = false;
				if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Gas" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Diesel" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Gasoline" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Coal" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "FuelOil#1" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "FuelOil#2" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Propane" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Steam" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "OtherFuel1" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "OtherFuel2" ) ) {
					FuelTypeIsLikeGas = true;
				} else if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "DistrictHeating" ) ) {
					FuelTypeIsLikeGas = true;
				}

				if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 1 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 20.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 2.5 * 1000.0; //2.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 20.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; //27kBtu/hr
					}

				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 2 ) {
					if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms <= 1.5 ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 3.5 * 1000.0; //3.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
						}
					} else if ( ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms > 1.5 ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms < 3.0 ) ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 4.5 * 1000.0; //4.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
						}
					} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms >= 3.0 ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
						}
					}
				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 3 ) {
					if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms <= 1.5 ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 4.5 * 1000.0; //4.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 30.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
						}
					} else if ( ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms > 1.5 ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms < 3.0 ) ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
						}
					} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms >= 3.0 ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; //38 kBtu/hr
						}
					}
				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 4 ) {
					if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms <= 1.5 ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; //36 kBtu/hr
						}
					} else if ( ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms > 1.5 ) && ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms < 3.0 ) ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 40.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; //38 kBtu/hr
						}
					} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBathrooms >= 3.0 ) {
						if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 66.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
						} else if ( FuelTypeIsLikeGas ) {
							if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
							if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; //38 kBtu/hr
						}
					}
				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms == 5 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 66.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; //47 kBtu/hr
					}
				} else if ( WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfBedrooms >= 6 ) {
					if ( SameString( WaterThermalTank( WaterThermalTankNum ).FuelType, "Electric" ) ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 66.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 5.5 * 1000.0; //5.5 kW
					} else if ( FuelTypeIsLikeGas ) {
						if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = 50.0 * GalTocubicMeters;
						if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; //50 kBtu/hr
					}
				}
				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}

			} else if ( SELECT_CASE_var == SizePerPerson ) {
				// how to get number of people?

				SumPeopleAllZones = sum( Zone.TotOccupants() );
				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerPerson * SumPeopleAllZones;
				}
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
					tmpMaxCapacity = SumPeopleAllZones * WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerPerson * ( Tfinish - Tstart ) * ( 1.0 / SecInHour ) * rho * Cp; //m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
				}

				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}

			} else if ( SELECT_CASE_var == SizePerFloorArea ) {

				SumFloorAreaAllZones = sum( Zone.FloorArea() );
				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
				}

				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
					tmpMaxCapacity = SumFloorAreaAllZones * WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerArea * ( Tfinish - Tstart ) * ( 1.0 / SecInHour ) * rho * Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
				}
				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			} else if ( SELECT_CASE_var == SizePerUnit ) {

				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerUnit * WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfUnits;

				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					rho = GetDensityGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
					Cp = GetSpecificHeatGlycol( fluidNameWater, ( ( Tfinish + Tstart ) / 2.0 ), DummyWaterIndex, RoutineName );
					tmpMaxCapacity = WaterThermalTank( WaterThermalTankNum ).Sizing.NumberOfUnits * WaterThermalTank( WaterThermalTankNum ).Sizing.RecoveryCapacityPerUnit * ( Tfinish - Tstart ) * ( 1.0 / SecInHour ) * rho * Cp; //m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
				}

				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}
			} else if ( SELECT_CASE_var == SizePerSolarColArea ) {
				WaterThermalTank( WaterThermalTankNum ).Sizing.TotalSolarCollectorArea = 0.0;
				for ( CollectorNum = 1; CollectorNum <= NumOfCollectors; ++CollectorNum ) {
					WaterThermalTank( WaterThermalTankNum ).Sizing.TotalSolarCollectorArea += Surface( Collector( CollectorNum ).Surface ).Area;
				}

				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) tmpTankVolume = WaterThermalTank( WaterThermalTankNum ).Sizing.TotalSolarCollectorArea * WaterThermalTank( WaterThermalTankNum ).Sizing.TankCapacityPerCollectorArea;
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) tmpMaxCapacity = 0.0;
				if ( WaterThermalTank( WaterThermalTankNum ).VolumeWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).Volume = tmpTankVolume;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Tank Volume [m3]", WaterThermalTank( WaterThermalTankNum ).Volume );
				}
				if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacityWasAutoSized ) {
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = tmpMaxCapacity;
					ReportSizingOutput( WaterThermalTank( WaterThermalTankNum ).Type, WaterThermalTank( WaterThermalTankNum ).Name, "Maximum Heater Capacity [W]", WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
				}

			}}

		}

	}

	void
	UpdateWaterThermalTank( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brandon Anderson
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//                      Nov 2011, BAN; removed the use and source heat rate re-calculation for stratified tank
		//                                     for energy conservation verification.
		//       RE-ENGINEERED  Feb 2004, PGE

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the node variables with local variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataLoopNode::Node;
		using Psychrometrics::CPHW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UseInletNode;
		int UseOutletNode;
		int SourceInletNode;
		int SourceOutletNode;

		// FLOW:
		UseInletNode = WaterThermalTank( WaterThermalTankNum ).UseInletNode;
		UseOutletNode = WaterThermalTank( WaterThermalTankNum ).UseOutletNode;
		SourceInletNode = WaterThermalTank( WaterThermalTankNum ).SourceInletNode;
		SourceOutletNode = WaterThermalTank( WaterThermalTankNum ).SourceOutletNode;

		if ( UseInletNode > 0 && UseOutletNode > 0 ) {
			Node( UseOutletNode ) = Node( UseInletNode ); // this could wipe out setpoints on outlet node

			Node( UseOutletNode ).Temp = WaterThermalTank( WaterThermalTankNum ).UseOutletTemp;

		}

		if ( SourceInletNode > 0 && SourceOutletNode > 0 ) {
			Node( SourceOutletNode ) = Node( SourceInletNode );

			Node( SourceOutletNode ).Temp = WaterThermalTank( WaterThermalTankNum ).SourceOutletTemp;

		}

	}

	void
	ReportWaterThermalTank( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brandon Anderson
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  Feb 2004, PGE

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SecInTimeStep;

		// FLOW:
		SecInTimeStep = TimeStepSys * SecInHour;

		WaterThermalTank( WaterThermalTankNum ).UnmetEnergy = WaterThermalTank( WaterThermalTankNum ).UnmetRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).LossEnergy = WaterThermalTank( WaterThermalTankNum ).LossRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).FlueLossEnergy = WaterThermalTank( WaterThermalTankNum ).FlueLossRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).UseEnergy = WaterThermalTank( WaterThermalTankNum ).UseRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).TotalDemandEnergy = WaterThermalTank( WaterThermalTankNum ).TotalDemandRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).SourceEnergy = WaterThermalTank( WaterThermalTankNum ).SourceRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).HeaterEnergy = WaterThermalTank( WaterThermalTankNum ).HeaterRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).HeaterEnergy1 = WaterThermalTank( WaterThermalTankNum ).HeaterRate1 * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).HeaterEnergy2 = WaterThermalTank( WaterThermalTankNum ).HeaterRate2 * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).FuelEnergy = WaterThermalTank( WaterThermalTankNum ).FuelRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).VentEnergy = WaterThermalTank( WaterThermalTankNum ).VentRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelEnergy = WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).OffCycParaEnergyToTank = WaterThermalTank( WaterThermalTankNum ).OffCycParaRateToTank * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelEnergy = WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).OnCycParaEnergyToTank = WaterThermalTank( WaterThermalTankNum ).OnCycParaRateToTank * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).NetHeatTransferEnergy = WaterThermalTank( WaterThermalTankNum ).NetHeatTransferRate * SecInTimeStep;
		WaterThermalTank( WaterThermalTankNum ).VolumeConsumed = WaterThermalTank( WaterThermalTankNum ).VolFlowRate * SecInTimeStep;

	}

	void
	CalcStandardRatings( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2005
		//       MODIFIED       R. Raustad, July 2005 - added HPWH to ratings procedure
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the water heater standard ratings, such as Energy Factor and Recovery Efficiency.  Results are written
		// to the EIO file.  Standard ratings are not calculated for storage-only tanks, i.e., MaxCapacity = 0.

		// METHODOLOGY EMPLOYED:
		// Water heater inputs are set to the specified test conditions. For HPWHs, the heating capacity and COP are assumed
		// to be the primary element in the water heater and are used during the rating procedure.  CalcWaterThermalTankMixed
		// is iteratively called in a self-contained, 24 hour simulation of the standard test procedure.

		// REFERENCES:
		// Title 10, Code of Federal Regulations, Part 430- Energy Conservation Program for Consumer Products, Appendix E to
		// Subpart B- Uniform Test Procedure for Measuring the Energy Consumption of Water Heaters, January 1, 2004.

		// Using/Aliasing
		using Psychrometrics::RhoH2O;
		using Psychrometrics::CPHW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyHFnTdbW;
		using DataGlobals::InitConvTemp;
		using CurveManager::CurveValue;
		using DXCoils::HPWHHeatingCapacity;
		using DXCoils::HPWHHeatingCOP;
		using DXCoils::SimDXCoil;
		using Fans::SimulateFanComponents;
		using DataLoopNode::Node;
		using DataEnvironment::OutBaroPress;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::HPWHInletDBTemp;
		using DataHVACGlobals::HPWHInletWBTemp;
		using DataHVACGlobals::HPWHCrankcaseDBTemp;
		using DataHVACGlobals::DXCoilTotalCapacity;
		using DataHVACGlobals::BlowThru;
		using DataHVACGlobals::CycFanCycCoil;
		using namespace OutputReportPredefined;
		using General::TrimSigDigits;
		using InputProcessor::SameString;
		using VariableSpeedCoils::SimVariableSpeedCoils;
		using VariableSpeedCoils::VarSpeedCoil;
		using VariableSpeedCoils::VSHPWHHeatingCapacity;
		using VariableSpeedCoils::VSHPWHHeatingCOP;

		// Locals
		Real64 MdotAir; // air mass flow rate through HP water heater evaporator (kg/s)

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TotalDrawMass; // Total mass of hot water drawn during the test (kg), equivalent to 64.3 gallons
		Real64 DrawMass; // Mass of a single draw of hot water (kg)
		Real64 SecInTimeStep; // Seconds per timestep, depends on user-specified system timestep (s)
		Real64 DrawMassFlowRate; // Mass flow rate of all test draw (m3/s)
		int TimeStepPerHour; // Number of timesteps per hour
		int Step; // Current timestep in the self-contained water heater simulation
		Real64 FuelEnergy; // Cumulative fuel energy used to heat the tank (J)
		Real64 MaxCapacity; // Maximum heating capacity (W)
		Real64 RecoveryEfficiency; // Standard water heater rating
		Real64 EnergyFactor; // Standard water heater rating
		int HPNum; // index to heat pump water heater
		Real64 MdotWater; // water mass flow rate through HP water heater condenser (kg/s)
		Real64 AmbientHumRat; // used during HPWH rating procedure
		Real64 RatedDXCoilTotalCapacity; // used during HPWH rating procedure
		bool FirstTimeFlag; // used during HPWH rating procedure
		std::string equipName;
		Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); //place holder to calling vs HPWH function
		bool bIsVSCoil(false); // variable-speed HPWH identifier
		Real64 RhoWater; //water density

		// Formats
		static gio::Fmt Format_720( "('Water Heater Information',6(',',A))" );
		static gio::Fmt Format_721( "('Heat Pump Water Heater Information',7(',',A))" );

		if ( AlreadyRated( WaterThermalTankNum ) ) { // bail we already did this one
			return;
		}

		// FLOW:
		if ( WaterThermalTank( WaterThermalTankNum ).MaxCapacity > 0.0 || WaterThermalTank( WaterThermalTankNum ).HeatPumpNum > 0 ) {
			// Set test conditions
			WaterThermalTank( WaterThermalTankNum ).AmbientTemp = 19.7222; // 67.5 F
			WaterThermalTank( WaterThermalTankNum ).UseInletTemp = 14.4444; // 58 F
			WaterThermalTank( WaterThermalTankNum ).SetPointTemp = 57.2222; // 135 F
			WaterThermalTank( WaterThermalTankNum ).SetPointTemp2 = 57.2222; // 135 F
			WaterThermalTank( WaterThermalTankNum ).TankTemp = 57.2222; // Initialize tank temperature
			if ( WaterThermalTank( WaterThermalTankNum ).Nodes > 0 ) WaterThermalTank( WaterThermalTankNum ).Node.Temp() = 57.2222;

			TotalDrawMass = 0.243402 * RhoH2O( InitConvTemp ); // 64.3 gal * rho
			DrawMass = TotalDrawMass / 6.0; // 6 equal draws
			SecInTimeStep = TimeStepSys * SecInHour;
			DrawMassFlowRate = DrawMass / SecInTimeStep;
			FuelEnergy = 0.0;
			FirstTimeFlag = true;

			TimeStepPerHour = int( 1.0 / TimeStepSys );

			// Simulate 24 hour test
			for ( Step = 1; Step <= TimeStepPerHour * 24; ++Step ) {

				if ( Step == 1 || Step == ( 1 + TimeStepPerHour ) || Step == ( 1 + TimeStepPerHour * 2 ) || Step == ( 1 + TimeStepPerHour * 3 ) || Step == ( 1 + TimeStepPerHour * 4 ) || Step == ( 1 + TimeStepPerHour * 5 ) ) { // Hour 1 | Hour 2 | Hour 3 | Hour 4 | Hour 5 | Hour 6

					WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate = DrawMassFlowRate;
				} else {
					WaterThermalTank( WaterThermalTankNum ).UseMassFlowRate = 0.0;
				}

				WaterThermalTank( WaterThermalTankNum ).SavedTankTemp = WaterThermalTank( WaterThermalTankNum ).TankTemp;
				WaterThermalTank( WaterThermalTankNum ).SavedMode = WaterThermalTank( WaterThermalTankNum ).Mode;
				if ( WaterThermalTank( WaterThermalTankNum ).Nodes > 0 ) {
					WaterThermalTank( WaterThermalTankNum ).Node.SavedTemp() = WaterThermalTank( WaterThermalTankNum ).Node.Temp();
					WaterThermalTank( WaterThermalTankNum ).SavedHeaterOn1 = WaterThermalTank( WaterThermalTankNum ).HeaterOn1;
					WaterThermalTank( WaterThermalTankNum ).SavedHeaterOn2 = WaterThermalTank( WaterThermalTankNum ).HeaterOn2;
				}

				if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum == 0 ) {

					{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).TypeNum );

					if ( SELECT_CASE_var == MixedWaterHeater ) {
						CalcWaterThermalTankMixed( WaterThermalTankNum );

					} else if ( SELECT_CASE_var == StratifiedWaterHeater ) {
						CalcWaterThermalTankStratified( WaterThermalTankNum );

					} else {
						//         Unhandled water heater type

					}}

				} else {

					HPNum = WaterThermalTank( WaterThermalTankNum ).HeatPumpNum;
					AmbientHumRat = 0.00717; // Humidity ratio at 67.5 F / 50% RH

					//       set the heat pump air- and water-side mass flow rate
					MdotWater = HPWaterHeater( HPNum ).OperatingWaterFlowRate * RhoH2O( WaterThermalTank( WaterThermalTankNum ).TankTemp );
					MdotAir = HPWaterHeater( HPNum ).OperatingAirFlowRate * PsyRhoAirFnPbTdbW( OutBaroPress, WaterThermalTank( WaterThermalTankNum ).AmbientTemp, AmbientHumRat );

					//       set the condenser inlet node mass flow rate and temperature
					Node( HPWaterHeater( HPNum ).CondWaterInletNode ).MassFlowRate = MdotWater;
					Node( HPWaterHeater( HPNum ).CondWaterInletNode ).Temp = WaterThermalTank( WaterThermalTankNum ).TankTemp;

					//       initialize temperatures for HPWH DX Coil heating capacity and COP curves
					HPWHInletDBTemp = WaterThermalTank( WaterThermalTankNum ).AmbientTemp;
					HPWHInletWBTemp = PsyTwbFnTdbWPb( HPWHInletDBTemp, AmbientHumRat, OutBaroPress );

					//       set up full air flow on DX coil inlet node
					if ( HPWaterHeater( HPNum ).InletAirMixerNode > 0 ) {
						Node( HPWaterHeater( HPNum ).InletAirMixerNode ).MassFlowRate = MdotAir;
						Node( HPWaterHeater( HPNum ).InletAirMixerNode ).MassFlowRateMaxAvail = MdotAir;
						Node( HPWaterHeater( HPNum ).InletAirMixerNode ).Temp = WaterThermalTank( WaterThermalTankNum ).AmbientTemp;
						Node( HPWaterHeater( HPNum ).InletAirMixerNode ).HumRat = AmbientHumRat;
						Node( HPWaterHeater( HPNum ).InletAirMixerNode ).Enthalpy = PsyHFnTdbW( WaterThermalTank( WaterThermalTankNum ).AmbientTemp, AmbientHumRat );
					} else {
						if ( HPWaterHeater( HPNum ).OutsideAirNode == 0 ) {
							Node( HPWaterHeater( HPNum ).HeatPumpAirInletNode ).MassFlowRate = MdotAir;
							Node( HPWaterHeater( HPNum ).HeatPumpAirInletNode ).MassFlowRateMaxAvail = MdotAir;
							Node( HPWaterHeater( HPNum ).HeatPumpAirInletNode ).Temp = WaterThermalTank( WaterThermalTankNum ).AmbientTemp;
							Node( HPWaterHeater( HPNum ).HeatPumpAirInletNode ).HumRat = AmbientHumRat;
							Node( HPWaterHeater( HPNum ).HeatPumpAirInletNode ).Enthalpy = PsyHFnTdbW( WaterThermalTank( WaterThermalTankNum ).AmbientTemp, AmbientHumRat );
						} else {
							Node( HPWaterHeater( HPNum ).OutsideAirNode ).MassFlowRate = MdotAir;
							Node( HPWaterHeater( HPNum ).OutsideAirNode ).MassFlowRateMaxAvail = MdotAir;
							Node( HPWaterHeater( HPNum ).OutsideAirNode ).Temp = WaterThermalTank( WaterThermalTankNum ).AmbientTemp;
							Node( HPWaterHeater( HPNum ).OutsideAirNode ).HumRat = AmbientHumRat;
							Node( HPWaterHeater( HPNum ).OutsideAirNode ).Enthalpy = PsyHFnTdbW( WaterThermalTank( WaterThermalTankNum ).AmbientTemp, AmbientHumRat );
						}
					}

					HPWHCrankcaseDBTemp = WaterThermalTank( WaterThermalTankNum ).AmbientTemp;

					if (SameString(HPWaterHeater(HPNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed")) {
						bIsVSCoil = true;
						RhoWater = RhoH2O(WaterThermalTank(WaterThermalTankNum).TankTemp);
						SetVSHPWHFlowRates(WaterThermalTankNum, HPNum,
							VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel, 1.0,
							RhoWater, MdotWater, true);
						//       simulate the HPWH coil/fan to find heating capacity
						if (HPWaterHeater(HPNum).FanPlacement == BlowThru) {
							//   simulate fan and DX coil twice
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
							SimVariableSpeedCoils(HPWaterHeater(HPNum).DXCoilName, HPWaterHeater(HPNum).DXCoilNum,
								CycFanCycCoil, EMP1, EMP2, EMP3, 1, 1.0,
								VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel, 1.0, 0.0, 0.0, 1.0);
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
							SimVariableSpeedCoils(HPWaterHeater(HPNum).DXCoilName, HPWaterHeater(HPNum).DXCoilNum,
								CycFanCycCoil, EMP1, EMP2, EMP3, 1, 1.0,
								VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel, 1.0, 0.0, 0.0, 1.0);
						} else {
							//   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
							SimVariableSpeedCoils(HPWaterHeater(HPNum).DXCoilName, HPWaterHeater(HPNum).DXCoilNum,
								CycFanCycCoil, EMP1, EMP2, EMP3, 1, 1.0,
								VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel, 1.0, 0.0, 0.0, 1.0);
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
							SimVariableSpeedCoils(HPWaterHeater(HPNum).DXCoilName, HPWaterHeater(HPNum).DXCoilNum,
								CycFanCycCoil, EMP1, EMP2, EMP3, 1, 1.0,
								VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel, 1.0, 0.0, 0.0, 1.0);
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
						}

						WaterThermalTank(WaterThermalTankNum).MaxCapacity = VSHPWHHeatingCapacity;
						WaterThermalTank(WaterThermalTankNum).MinCapacity = VSHPWHHeatingCapacity;
						WaterThermalTank(WaterThermalTankNum).Efficiency = VSHPWHHeatingCOP;
					} else {
						bIsVSCoil = false;
						//       simulate the HPWH coil/fan to find heating capacity
						if (HPWaterHeater(HPNum).FanPlacement == BlowThru) {
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
							//         CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp,  .TRUE.,PartLoadRatio, HPWaterHeater(HPNum)%DXCoilNum,FanOpMode)
							SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
							SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
						} else {
							SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
							SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
							SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
						}

						WaterThermalTank(WaterThermalTankNum).MaxCapacity = HPWHHeatingCapacity;
						WaterThermalTank(WaterThermalTankNum).MinCapacity = HPWHHeatingCapacity;
						WaterThermalTank(WaterThermalTankNum).Efficiency = HPWHHeatingCOP;
					}


					if ( FirstTimeFlag ) {
						RatedDXCoilTotalCapacity = DXCoilTotalCapacity;
						FirstTimeFlag = false;
					}

					//       Switch the HPWH info with the tank info and call CalcWaterThermalTankMixed to get Standard Rating
					//       (backup element is assumed to be disabled during the rating procedure)
					WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = 0.0;
					//WaterThermalTank( WaterThermalTankNum ).MaxCapacity = HPWHHeatingCapacity;
					//WaterThermalTank( WaterThermalTankNum ).MinCapacity = HPWHHeatingCapacity;
					//WaterThermalTank( WaterThermalTankNum ).Efficiency = HPWHHeatingCOP; //* WaterHeater(WaterHeaterNum)%Efficiency
					WaterThermalTank( WaterThermalTankNum ).OnCycParaLoad = HPWaterHeater( HPNum ).OnCycParaLoad;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaLoad = HPWaterHeater( HPNum ).OffCycParaLoad;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaFracToTank = 0.0;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaFracToTank = 0.0;
					WaterThermalTank( WaterThermalTankNum ).PLFCurve = HPWaterHeater( HPNum ).DXCoilPLFFPLR;

					{ auto const SELECT_CASE_var( WaterThermalTank( WaterThermalTankNum ).TypeNum );

					if ( SELECT_CASE_var == MixedWaterHeater ) {
						if ( WaterThermalTank( WaterThermalTankNum ).Efficiency > 0.0 ) CalcWaterThermalTankMixed( WaterThermalTankNum );

					} else if ( SELECT_CASE_var == StratifiedWaterHeater ) {
						if ( WaterThermalTank( WaterThermalTankNum ).Efficiency > 0.0 ) CalcWaterThermalTankStratified( WaterThermalTankNum );

					} else {
						//         Unhandled water heater type

					}}

					//       reset the water heater data to original values
					WaterThermalTank( WaterThermalTankNum ).MaxCapacity = HPWaterHeater( HPNum ).BackupElementCapacity;
					WaterThermalTank( WaterThermalTankNum ).MinCapacity = HPWaterHeater( HPNum ).BackupElementCapacity;
					WaterThermalTank( WaterThermalTankNum ).Efficiency = HPWaterHeater( HPNum ).BackupElementEfficiency;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaLoad = HPWaterHeater( HPNum ).WHOnCycParaLoad;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaLoad = HPWaterHeater( HPNum ).WHOffCycParaLoad;
					WaterThermalTank( WaterThermalTankNum ).OnCycParaFracToTank = HPWaterHeater( HPNum ).WHOnCycParaFracToTank;
					WaterThermalTank( WaterThermalTankNum ).OffCycParaFracToTank = HPWaterHeater( HPNum ).WHOffCycParaFracToTank;
					WaterThermalTank( WaterThermalTankNum ).PLFCurve = HPWaterHeater( HPNum ).WHPLFCurve;

				}

				FuelEnergy += ( WaterThermalTank( WaterThermalTankNum ).FuelRate + WaterThermalTank( WaterThermalTankNum ).OffCycParaFuelRate + WaterThermalTank( WaterThermalTankNum ).OnCycParaFuelRate ) * SecInTimeStep;

			} // Step

			if ( WaterThermalTank( WaterThermalTankNum ).FirstRecoveryDone && WaterThermalTank( WaterThermalTankNum ).FirstRecoveryFuel > 0.0 ) {
				// Calculate Recovery Efficiency based on energy used to recover from the first draw
				// FirstRecoveryFuel is recorded inside the CalcWaterThermalTank subroutine
				RecoveryEfficiency = DrawMass * CPHW( 57.2222 ) * ( 57.2222 - 14.4444 ) / WaterThermalTank( WaterThermalTankNum ).FirstRecoveryFuel;

				// Calculate Energy Factor based on total energy (including parasitics) used over entire test
				EnergyFactor = TotalDrawMass * CPHW( 57.2222 ) * ( 57.2222 - 14.4444 ) / FuelEnergy;

			} else {
				RecoveryEfficiency = 0.0;
				EnergyFactor = 0.0;

				ShowWarningError( "Water heater = " + WaterThermalTank( WaterThermalTankNum ).Name + ":  Recovery Efficiency and Energy Factor could not be calculated during the test for standard ratings" );
				ShowContinueError( "Setpoint was never recovered and/or heater never turned on" );
			}

		} else {

			// Storage-only tank
			RecoveryEfficiency = 0.0;
			EnergyFactor = 0.0;

		} // WaterThermalTank(WaterThermalTankNum)%MaxCapacity > 0.0

		//create predefined report
		// Store values for the input verification and summary report
		if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum == 0 ) {
			equipName = WaterThermalTank( WaterThermalTankNum ).Name;
			PreDefTableEntry( pdchSWHType, equipName, WaterThermalTank( WaterThermalTankNum ).Type );
			PreDefTableEntry( pdchSWHVol, equipName, WaterThermalTank( WaterThermalTankNum ).Volume );
			PreDefTableEntry( pdchSWHHeatIn, equipName, WaterThermalTank( WaterThermalTankNum ).MaxCapacity );
			PreDefTableEntry( pdchSWHThEff, equipName, WaterThermalTank( WaterThermalTankNum ).Efficiency );
			PreDefTableEntry( pdchSWHRecEff, equipName, RecoveryEfficiency );
			PreDefTableEntry( pdchSWHEnFac, equipName, EnergyFactor );
		} else {
			equipName = HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name;
			PreDefTableEntry( pdchSWHType, equipName, HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Type );
			PreDefTableEntry( pdchSWHVol, equipName, WaterThermalTank( WaterThermalTankNum ).Volume );
			if ( bIsVSCoil ) {
				PreDefTableEntry( pdchSWHHeatIn, equipName, VSHPWHHeatingCapacity );
			} else {
				PreDefTableEntry( pdchSWHHeatIn, equipName, HPWHHeatingCapacity );
			}
			PreDefTableEntry( pdchSWHThEff, equipName, WaterThermalTank( WaterThermalTankNum ).Efficiency );
			PreDefTableEntry( pdchSWHRecEff, equipName, RecoveryEfficiency );
			PreDefTableEntry( pdchSWHEnFac, equipName, EnergyFactor );
		}

		// Write test results
		if ( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum == 0 ) {

			if ( WaterThermalTank( WaterThermalTankNum ).TypeNum == StratifiedWaterHeater ) {
				if ( WaterThermalTank( WaterThermalTankNum ).ControlType == PriorityMasterSlave ) {
					MaxCapacity = max( WaterThermalTank( WaterThermalTankNum ).MaxCapacity, WaterThermalTank( WaterThermalTankNum ).MaxCapacity2 );
				} else { // PrioritySimultaneous
					MaxCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity + WaterThermalTank( WaterThermalTankNum ).MaxCapacity2;
				}
			} else { // WaterHeaterMixed
				MaxCapacity = WaterThermalTank( WaterThermalTankNum ).MaxCapacity;
			}

			gio::write( OutputFileInits, Format_720 ) << WaterThermalTank( WaterThermalTankNum ).Type << WaterThermalTank( WaterThermalTankNum ).Name << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Volume, 4 ) << TrimSigDigits( MaxCapacity, 1 ) << TrimSigDigits( RecoveryEfficiency, 3 ) << TrimSigDigits( EnergyFactor, 4 );
		} else {
			gio::write( OutputFileInits, Format_721 ) << HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Type << HPWaterHeater( WaterThermalTank( WaterThermalTankNum ).HeatPumpNum ).Name << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Volume, 4 ) << TrimSigDigits( HPWHHeatingCapacity, 1 ) << TrimSigDigits( RecoveryEfficiency, 3 ) << TrimSigDigits( EnergyFactor, 4 ) << TrimSigDigits( RatedDXCoilTotalCapacity, 0 );
		}

		AlreadyRated( WaterThermalTankNum ) = true;

	}

	void
	ReportCWTankInits( int const WaterThermalTankNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// send chilled water tank info to EIO

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeSetupFlag( true ); // one time setup flag
		static Array1D_bool AlreadyReported; // control so we don't repeat again

		// Formats
		static gio::Fmt Format_728( "('Chilled Water Tank Information',5(',',A))" );

		if ( MyOneTimeSetupFlag ) {
			AlreadyReported.dimension( NumWaterThermalTank, false );
			MyOneTimeSetupFlag = false;
		}

		if ( AlreadyReported( WaterThermalTankNum ) ) { // bail we already did this one
			return;
		}

		gio::write( OutputFileInits, Format_728 ) << WaterThermalTank( WaterThermalTankNum ).Type << WaterThermalTank( WaterThermalTankNum ).Name << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).Volume, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).UseDesignVolFlowRate, 4 ) << TrimSigDigits( WaterThermalTank( WaterThermalTankNum ).SourceDesignVolFlowRate, 4 );

		AlreadyReported( WaterThermalTankNum ) = true;

	}

	Real64
	FindStratifiedTankSensedTemp(
		int const WaterThermalTankNum,
		int const ControlLocationType
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// find tank temperature depending on how sensed

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SensedTemp;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static int StratNodeToUse( 0 );

		{ auto const SELECT_CASE_var( ControlLocationType );

		if ( SELECT_CASE_var == Heater1HPWHControl ) {
			StratNodeToUse = WaterThermalTank( WaterThermalTankNum ).HeaterNode1;
		} else if ( SELECT_CASE_var == Heater2HPWHControl ) {
			StratNodeToUse = WaterThermalTank( WaterThermalTankNum ).HeaterNode2;
		} else if ( SELECT_CASE_var == SourceInletHPWHControl ) {
			StratNodeToUse = WaterThermalTank( WaterThermalTankNum ).SourceInletStratNode;
		} else if ( SELECT_CASE_var == SourceOutletHPWHControl ) {
			StratNodeToUse = WaterThermalTank( WaterThermalTankNum ).SourceOutletStratNode;
		} else if ( SELECT_CASE_var == UseInletHPWHControl ) {
			StratNodeToUse = WaterThermalTank( WaterThermalTankNum ).UseInletStratNode;
		} else if ( SELECT_CASE_var == UseOutletHPWHControl ) {
			StratNodeToUse = WaterThermalTank( WaterThermalTankNum ).UseOutletStratNode;
		}}

		SensedTemp = WaterThermalTank( WaterThermalTankNum ).Node( StratNodeToUse ).Temp;

		return SensedTemp;

	}
	
	Real64
	WaterThermalTankData::getDeadBandTemp()
	{
		if ( this->IsChilledWaterTank ) {
			return (this->SetPointTemp + this->DeadBandDeltaTemp);
		} else {
			return (this->SetPointTemp - this->DeadBandDeltaTemp);
		}
	}


	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // WaterThermalTanks

} // EnergyPlus
