// EnergyPlus::HVACUnitarySystem Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <General.hh>
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

#include "Fixtures/HVACFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::FanCoilUnits;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::WaterCoils;
using General::TrimSigDigits;
using DataEnvironment::OutDryBulbTemp;
using General::TrimSigDigits;
using DataZoneEnergyDemands::ZoneSysEnergyDemand;
using MixedAir::OAMixer;
using MixedAir::GetOAMixerInputFlag;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::ScheduleManager;


namespace EnergyPlus {
	TEST_F( HVACFixture, MultiStage4PipeFanCoilHeatingTest ) {

		ShowMessage( "Begin Test: HVACFixture, MultiStage4PipeFanCoilHeatingTest" );

		int FanCoilNum( 1 );
		int ZoneNum( 1 );
		int NumOfZones( 1 );
		int NumOAMixers( 1 );
		bool FirstHVACIteration( false );
		bool ErrorsFound( false );
		Real64 PartLoadRatio( 1.0 );
		Real64 SpeedRatio( 0.0 );
		Real64 QZnReq( 0.0 );
		Real64 WaterMassFlowRate( 0.0 );
		Real64 ColdWaterMassFlowRate( 0.0 );
		Real64 QUnitOut( 0.0 );
		Real64 AirMassFlow( 0.0 );
		Real64 MaxAirMassFlow( 0.0 );

		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::StdRhoAir = 1.20;

		std::string const idf_objects = delimited_string( {
			"	Version,8.3;",
			"	Zone,",
			"	EAST ZONE, !- Name",
			"	0, !- Direction of Relative North { deg }",
			"	0, !- X Origin { m }",
			"	0, !- Y Origin { m }",
			"	0, !- Z Origin { m }",
			"	1, !- Type",
			"	1, !- Multiplier",
			"	autocalculate, !- Ceiling Height { m }",
			"	autocalculate; !- Volume { m3 }",
			"	ZoneHVAC:EquipmentConnections,",
			"	EAST ZONE, !- Zone Name",
			"	Zone1Equipment, !- Zone Conditioning Equipment List Name",
			"	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
			"	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
			"	Zone 1 Node, !- Zone Air Node Name",
			"	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
			"	ZoneHVAC:EquipmentList,",
			"	Zone1Equipment, !- Name",
			"	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
			"	Zone1FanCoil, !- Zone Equipment 1 Name",
			"	1, !- Zone Equipment 1 Cooling Sequence",
			"	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
			"   NodeList,",
			"	Zone1Inlets, !- Name",
			"	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
			"	NodeList,",
			"	Zone1Exhausts, !- Name",
			"	Zone1FanCoilAirInletNode; !- Node 1 Name",
			"	OutdoorAir:NodeList,",
			"	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
			"	OutdoorAir:Mixer,",
			"	Zone1FanCoilOAMixer, !- Name",
			"	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
			"	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
			"	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
			"	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
			"	Schedule:Compact,",
			"	FanAndCoilAvailSched, !- Name",
			"	Fraction, !- Schedule Type Limits Name",
			"	Through: 12/31, !- Field 1",
			"	For: AllDays, !- Field 2",
			"	Until: 24:00, 1.0;        !- Field 3",
			"   Fan:OnOff,",
			"	Zone1FanCoilFan, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	0.5, !- Fan Total Efficiency",
			"	75.0, !- Pressure Rise { Pa }",
			"	0.6, !- Maximum Flow Rate { m3 / s }",
			"	0.9, !- Motor Efficiency",
			"	1.0, !- Motor In Airstream Fraction",
			"	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
			"	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
			"	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
			"	Coil:Cooling:Water,",
			"	Zone1FanCoilCoolingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Namev",
			"	0.0002, !- Design Water Flow Rate { m3 / s }",
			"	0.5000, !- Design Air Flow Rate { m3 / s }",
			"	7.22,   !- Design Inlet Water Temperature { Cv }",
			"	24.340, !- Design Inlet Air Temperature { C }",
			"	14.000, !- Design Outlet Air Temperature { C }",
			"	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
			"	SimpleAnalysis, !- Type of Analysis",
			"	CrossFlow;               !- Heat Exchanger Configuration",
			"	Coil:Heating:Water,",
			"   Zone1FanCoilHeatingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	150.0,   !- U - Factor Times Area Value { W / K }",
			"	0.00014, !- Maximum Water Flow Rate { m3 / s }",
			"	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
			"	autosize, !- Rated Capacity { W }",
			"	82.2, !- Rated Inlet Water Temperature { C }",
			"	16.6, !- Rated Inlet Air Temperature { C }",
			"	71.1, !- Rated Outlet Water Temperature { C }",
			"	32.2, !- Rated Outlet Air Temperature { C }",
			"	;     !- Rated Ratio for Air and Water Convection",
			"	ZoneHVAC:FourPipeFanCoil,",
			"	Zone1FanCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	MultiStageFan, !- Capacity Control Method",
			"	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
			"	0.3, !- Low Speed Supply Air Flow Ratio",
			"	0.6, !- Medium Speed Supply Air Flow Ratio",
			"	0.5, !- Maximum Outdoor Air Flow Rate { m3 / s }",
			"	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
			"	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
			"	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
			"	Fan:OnOff, !- Supply Air Fan Object Type",
			"	Zone1FanCoilFan, !- Supply Air Fan Name",
			"	Coil:Cooling:Water, !- Cooling Coil Object Type",
			"	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
			"	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
			"	0.001, !- Cooling Convergence Tolerance",
			"	Coil:Heating:Water, !- Heating Coil Object Type",
			"	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
			"	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
			"	0.001; !- Heating Convergence Tolerance",

		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetZoneData( ErrorsFound );
		GetZoneEquipmentData1();
		GetFanInput();
		GetFanCoilUnits();

		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );

		AirMassFlow = 0.60;
		MaxAirMassFlow = 0.60;
		FanFlowRatio = 1.0;
		WaterMassFlowRate = 1.0;
		
		Node( OAMixer( 1 ).RetNode ).MassFlowRate = AirMassFlow;
		Node( OAMixer( 1 ).RetNode ).MassFlowRateMax = MaxAirMassFlow;

		Node( OAMixer( 1 ).RetNode ).Temp = 24.0;
		Node( OAMixer( 1 ).RetNode ).HumRat = 0.0050;
		Node( OAMixer( 1 ).RetNode ).Enthalpy = 36000;

		Node( OAMixer( 1 ).InletNode ).Temp = 10.0;
		Node( OAMixer( 1 ).InletNode ).HumRat = 0.0030;
		Node( OAMixer( 1 ).InletNode ).Enthalpy = 18000;

		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = AirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMin = AirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMinAvail = AirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMax = MaxAirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMaxAvail = MaxAirMassFlow;

		// heating load only
		FanCoil( 1 ).OutAirMassFlow = AirMassFlow;
		FanCoil( 1 ).MaxAirMassFlow = MaxAirMassFlow;
		Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRateMax = MaxAirMassFlow;

		Fan( 1 ).InletAirMassFlowRate = AirMassFlow;
		Fan( 1 ).MaxAirMassFlowRate = MaxAirMassFlow;

		Node( Fan( 1 ).InletNodeNum ).MassFlowRate = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).UACoilTotal = 470.0;
		WaterCoil( 2 ).UACoilExternal = 611.0;
		WaterCoil( 2 ).UACoilInternal = 2010.0;
		WaterCoil( 2 ).TotCoilOutsideSurfArea = 50.0;

		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).InletWaterMassFlowRate = ColdWaterMassFlowRate;
		WaterCoil( 2 ).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).Temp = 6.0;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		Node( WaterCoil( 1 ).WaterInletNodeNum ).Temp = 60.0;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRate = WaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRateMaxAvail = WaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRate = WaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = WaterMassFlowRate;
		WaterCoil( 1 ).InletWaterMassFlowRate = WaterMassFlowRate;
		WaterCoil( 1 ).MaxWaterMassFlowRate = WaterMassFlowRate;

		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}

		WaterCoil( 2 ).WaterLoopNum = 1;
		WaterCoil( 2 ).WaterLoopSide = 1;
		WaterCoil( 2 ).WaterLoopBranchNum = 1;
		WaterCoil( 2 ).WaterLoopCompNum = 1;

		WaterCoil( 1 ).WaterLoopNum = 2;
		WaterCoil( 1 ).WaterLoopSide = 1;
		WaterCoil( 1 ).WaterLoopBranchNum = 1;
		WaterCoil( 1 ).WaterLoopCompNum = 1;

		PlantLoop( 2 ).Name = "ChilledWaterLoop";
		PlantLoop( 2 ).FluidName = "ChilledWater";
		PlantLoop( 2 ).FluidIndex = 1;
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 2 ).Name;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_Cooling;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 2 ).WaterInletNodeNum;

		PlantLoop( 1 ).Name = "HotWaterLoop";
		PlantLoop( 1 ).FluidName = "HotWater";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_SimpleHeating;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;

		CoolingLoad = false;
		HeatingLoad = true;
		ZoneSysEnergyDemand.allocate( 1 );
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = 0;
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 4000.0;
		WaterCoil( 1 ).TotWaterHeatingCoilRate = 0.0;
		FanCoil( 1 ).SpeedFanSel = 2;
		QZnReq = 4000.0;

		MyUAAndFlowCalcFlag.allocate( 2 );
		MyUAAndFlowCalcFlag( 1 ) = true;
		MyUAAndFlowCalcFlag( 2 ) = true;
		DataGlobals::DoingSizing = true;


		LocalTurnFansOff = false;
		LocalTurnFansOn = true;
		Fan( 1 ).AvailSchedPtrNum = DataGlobals::ScheduleAlwaysOn;
		WaterCoil( 1 ).SchedPtr = DataGlobals::ScheduleAlwaysOn;
		WaterCoil( 2 ).SchedPtr = DataGlobals::ScheduleAlwaysOn;

		InitializePsychRoutines();

		CalcMultiStage4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut );

		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );

		DataGlobals::DoingSizing = false;

		FanCoil.deallocate();
		ZoneSysEnergyDemand.deallocate();
		PlantLoop.deallocate();
		WaterCoil.allocate( 1 );
		Node.deallocate();
		ZoneEquipConfig.deallocate();
		Zone.deallocate();
	}
	TEST_F( HVACFixture, MultiStage4PipeFanCoilCoolingTest ) {

		ShowMessage( "Begin Test: HVACFixture, MultiStage4PipeFanCoilCoolingTest" );

		int FanCoilNum( 1 );
		int ZoneNum( 1 );
		int NumOfZones( 1 );
		int NumOAMixers( 1 );
		bool FirstHVACIteration( false );
		bool ErrorsFound( false );
		Real64 PartLoadRatio( 1.0 );
		Real64 SpeedRatio( 0.0 );
		Real64 QZnReq( 0.0 );
		Real64 WaterMassFlowRate( 0.0 );
		Real64 ColdWaterMassFlowRate( 0.0 );
		Real64 QUnitOut( 0.0 );
		Real64 AirMassFlow( 0.0 );
		Real64 MaxAirMassFlow( 0.0 );

		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::StdRhoAir = 1.20;

		std::string const idf_objects = delimited_string( {
			"	Version,8.3;",
			"	Zone,",
			"	EAST ZONE, !- Name",
			"	0, !- Direction of Relative North { deg }",
			"	0, !- X Origin { m }",
			"	0, !- Y Origin { m }",
			"	0, !- Z Origin { m }",
			"	1, !- Type",
			"	1, !- Multiplier",
			"	autocalculate, !- Ceiling Height { m }",
			"	autocalculate; !- Volume { m3 }",
			"	ZoneHVAC:EquipmentConnections,",
			"	EAST ZONE, !- Zone Name",
			"	Zone1Equipment, !- Zone Conditioning Equipment List Name",
			"	Zone1Inlets, !- Zone Air Inlet Node or NodeList Name",
			"	Zone1Exhausts, !- Zone Air Exhaust Node or NodeList Name",
			"	Zone 1 Node, !- Zone Air Node Name",
			"	Zone 1 Outlet Node;      !- Zone Return Air Node Name",
			"	ZoneHVAC:EquipmentList,",
			"	Zone1Equipment, !- Name",
			"	ZoneHVAC:FourPipeFanCoil, !- Zone Equipment 1 Object Type",
			"	Zone1FanCoil, !- Zone Equipment 1 Name",
			"	1, !- Zone Equipment 1 Cooling Sequence",
			"	1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
			"   NodeList,",
			"	Zone1Inlets, !- Name",
			"	Zone1FanCoilAirOutletNode;  !- Node 1 Name",
			"	NodeList,",
			"	Zone1Exhausts, !- Name",
			"	Zone1FanCoilAirInletNode; !- Node 1 Name",
			"	OutdoorAir:NodeList,",
			"	Zone1FanCoilOAInNode;    !- Node or NodeList Name 1",
			"	OutdoorAir:Mixer,",
			"	Zone1FanCoilOAMixer, !- Name",
			"	Zone1FanCoilOAMixerOutletNode, !- Mixed Air Node Name",
			"	Zone1FanCoilOAInNode, !- Outdoor Air Stream Node Name",
			"	Zone1FanCoilExhNode, !- Relief Air Stream Node Name",
			"	Zone1FanCoilAirInletNode; !- Return Air Stream Node Name",
			"	Schedule:Compact,",
			"	FanAndCoilAvailSched, !- Name",
			"	Fraction, !- Schedule Type Limits Name",
			"	Through: 12/31, !- Field 1",
			"	For: AllDays, !- Field 2",
			"	Until: 24:00, 1.0;        !- Field 3",
			"   Fan:OnOff,",
			"	Zone1FanCoilFan, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	0.5, !- Fan Total Efficiency",
			"	75.0, !- Pressure Rise { Pa }",
			"	0.6, !- Maximum Flow Rate { m3 / s }",
			"	0.9, !- Motor Efficiency",
			"	1.0, !- Motor In Airstream Fraction",
			"	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
			"	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
			"	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
			"	Coil:Cooling:Water,",
			"	Zone1FanCoilCoolingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Namev",
			"	0.0002, !- Design Water Flow Rate { m3 / s }",
			"	0.5000, !- Design Air Flow Rate { m3 / s }",
			"	7.22,   !- Design Inlet Water Temperature { Cv }",
			"	24.340, !- Design Inlet Air Temperature { C }",
			"	14.000, !- Design Outlet Air Temperature { C }",
			"	0.0095, !- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	0.0090, !- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	Zone1FanCoilChWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilChWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilFanOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Outlet Node Name",
			"	SimpleAnalysis, !- Type of Analysis",
			"	CrossFlow;               !- Heat Exchanger Configuration",
			"	Coil:Heating:Water,",
			"   Zone1FanCoilHeatingCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	150.0,   !- U - Factor Times Area Value { W / K }",
			"	0.00014, !- Maximum Water Flow Rate { m3 / s }",
			"	Zone1FanCoilHWInletNode, !- Water Inlet Node Name",
			"	Zone1FanCoilHWOutletNode, !- Water Outlet Node Name",
			"	Zone1FanCoilCCOutletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	UFactorTimesAreaAndDesignWaterFlowRate, !- Performance Input Method",
			"	autosize, !- Rated Capacity { W }",
			"	82.2, !- Rated Inlet Water Temperature { C }",
			"	16.6, !- Rated Inlet Air Temperature { C }",
			"	71.1, !- Rated Outlet Water Temperature { C }",
			"	32.2, !- Rated Outlet Air Temperature { C }",
			"	;     !- Rated Ratio for Air and Water Convection",
			"	ZoneHVAC:FourPipeFanCoil,",
			"	Zone1FanCoil, !- Name",
			"	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	MultiStageFan, !- Capacity Control Method",
			"	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
			"	0.3, !- Low Speed Supply Air Flow Ratio",
			"	0.6, !- Medium Speed Supply Air Flow Ratio",
			"	0.5, !- Maximum Outdoor Air Flow Rate { m3 / s }",
			"	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
			"	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
			"	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
			"	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
			"	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
			"	Fan:OnOff, !- Supply Air Fan Object Type",
			"	Zone1FanCoilFan, !- Supply Air Fan Name",
			"	Coil:Cooling:Water, !- Cooling Coil Object Type",
			"	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
			"	0.00014, !- Maximum Cold Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
			"	0.001, !- Cooling Convergence Tolerance",
			"	Coil:Heating:Water, !- Heating Coil Object Type",
			"	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
			"	0.00014, !- Maximum Hot Water Flow Rate { m3 / s }",
			"	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
			"	0.001; !- Heating Convergence Tolerance",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetZoneData( ErrorsFound );
		GetZoneEquipmentData1();
		GetFanInput();
		GetFanCoilUnits();

		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );

		//FanFlowRatio = 1.0;
		AirMassFlow = 0.60;
		MaxAirMassFlow = 0.60;
		WaterMassFlowRate = 0.0;
		ColdWaterMassFlowRate = 1.0;

		Node( OAMixer( 1 ).RetNode ).MassFlowRate = AirMassFlow;
		Node( OAMixer( 1 ).RetNode ).MassFlowRateMax = MaxAirMassFlow;

		Node( OAMixer( 1 ).RetNode ).Temp = 24.0;
		Node( OAMixer( 1 ).RetNode ).HumRat = 0.0050;
		Node( OAMixer( 1 ).RetNode ).Enthalpy = 36000;

		Node( OAMixer( 1 ).InletNode ).Temp = 30.0;
		Node( OAMixer( 1 ).InletNode ).HumRat = 0.0085;
		Node( OAMixer( 1 ).InletNode ).Enthalpy = 53000;

		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRate = AirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMin = AirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMinAvail = AirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMax = MaxAirMassFlow;
		Node( FanCoil( FanCoilNum ).AirInNode ).MassFlowRateMaxAvail = MaxAirMassFlow;

		// heating load only
		FanCoil( 1 ).OutAirMassFlow = AirMassFlow;
		FanCoil( 1 ).MaxAirMassFlow = MaxAirMassFlow;
		Node( FanCoil( FanCoilNum ).OutsideAirNode ).MassFlowRateMax = MaxAirMassFlow;

		Fan( 1 ).InletAirMassFlowRate = AirMassFlow;
		Fan( 1 ).MaxAirMassFlowRate = MaxAirMassFlow;

		Node( Fan( 1 ).InletNodeNum ).MassFlowRate = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( Fan( 1 ).InletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).UACoilTotal = 470.0;
		WaterCoil( 2 ).UACoilExternal = 611.0;
		WaterCoil( 2 ).UACoilInternal = 2010.0;
		WaterCoil( 2 ).TotCoilOutsideSurfArea = 50.0;

		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMin = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMax = AirMassFlow;
		Node( WaterCoil( 2 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		WaterCoil( 2 ).InletWaterMassFlowRate = ColdWaterMassFlowRate;
		WaterCoil( 2 ).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterInletNodeNum ).Temp = 6.0;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRate = ColdWaterMassFlowRate;
		Node( WaterCoil( 2 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRate = AirMassFlow;
		Node( WaterCoil( 1 ).AirInletNodeNum ).MassFlowRateMaxAvail = AirMassFlow;

		Node( WaterCoil( 1 ).WaterInletNodeNum ).Temp = 60.0;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRate = WaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterInletNodeNum ).MassFlowRateMaxAvail = WaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRate = WaterMassFlowRate;
		Node( WaterCoil( 1 ).WaterOutletNodeNum ).MassFlowRateMaxAvail = WaterMassFlowRate;
		WaterCoil( 1 ).InletWaterMassFlowRate = WaterMassFlowRate;
		WaterCoil( 1 ).MaxWaterMassFlowRate = WaterMassFlowRate;

		for ( int l = 1; l <= TotNumLoops; ++l ) {
			auto & loop( PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}

		WaterCoil( 2 ).WaterLoopNum = 1;
		WaterCoil( 2 ).WaterLoopSide = 1;
		WaterCoil( 2 ).WaterLoopBranchNum = 1;
		WaterCoil( 2 ).WaterLoopCompNum = 1;

		WaterCoil( 1 ).WaterLoopNum = 2;
		WaterCoil( 1 ).WaterLoopSide = 1;
		WaterCoil( 1 ).WaterLoopBranchNum = 1;
		WaterCoil( 1 ).WaterLoopCompNum = 1;

		PlantLoop( 2 ).Name = "ChilledWaterLoop";
		PlantLoop( 2 ).FluidName = "ChilledWater";
		PlantLoop( 2 ).FluidIndex = 1;
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 2 ).Name;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_Cooling;
		PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 2 ).WaterInletNodeNum;

		PlantLoop( 1 ).Name = "HotWaterLoop";
		PlantLoop( 1 ).FluidName = "HotWater";
		PlantLoop( 1 ).FluidIndex = 1;
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = WaterCoil( 1 ).Name;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = WaterCoil_SimpleHeating;
		PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = WaterCoil( 1 ).WaterInletNodeNum;

		HeatingLoad = false;
		CoolingLoad = true;
		ZoneSysEnergyDemand.allocate( 1 );
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToCoolSP = -4000.00;
		ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 0.0;
		WaterCoil( 1 ).TotWaterCoolingCoilRate = 0.0;
		FanCoil( 1 ).SpeedFanSel = 2;
		QZnReq = -4000.0;

		MyUAAndFlowCalcFlag.allocate( 2 );
		MyUAAndFlowCalcFlag( 1 ) = true;
		MyUAAndFlowCalcFlag( 2 ) = true;
		DataGlobals::DoingSizing = true;

		LocalTurnFansOff = false;
		LocalTurnFansOn = true;
		Fan( 1 ).AvailSchedPtrNum = DataGlobals::ScheduleAlwaysOn;
		WaterCoil( 1 ).SchedPtr = DataGlobals::ScheduleAlwaysOn;
		WaterCoil( 2 ).SchedPtr = DataGlobals::ScheduleAlwaysOn;

		InitializePsychRoutines();

		CalcMultiStage4PipeFanCoil( FanCoilNum, ZoneNum, FirstHVACIteration, QZnReq, SpeedRatio, PartLoadRatio, QUnitOut );

		EXPECT_NEAR( QZnReq, QUnitOut, 5.0 );

		DataGlobals::DoingSizing = false;
		FanCoil.deallocate();
		ZoneSysEnergyDemand.deallocate();
		PlantLoop.deallocate();
		WaterCoil.allocate( 1 );
		Node.deallocate();
		ZoneEquipConfig.deallocate();
		Zone.deallocate();

	}
}