// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <SetPointManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataPlant.hh>
#include <DataLoopNode.hh>
#include <ScheduleManager.hh>
#include "Fixtures/HVACFixture.hh"
#include "Fixtures/EnergyPlusFixture.hh"
#include <HeatBalanceManager.hh>
#include <DataHeatBalance.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataAirSystems.hh>
#include <DataZoneEquipment.hh>

using namespace EnergyPlus;
 
TEST( SetPointManager, DefineReturnWaterChWSetPointManager )
{

	// Set up the required plant loop data
	DataPlant::TotNumLoops = 1;
	DataPlant::PlantLoop.allocate(1);
	DataPlant::PlantLoop(1).FluidIndex = 1;
	DataPlant::PlantLoop(1).LoopSide.allocate(2);
	DataPlant::PlantLoop(1).LoopSide(2).NodeNumIn = 1; // Supply inlet, return
	DataPlant::PlantLoop(1).LoopSide(2).NodeNumOut = 2; // Supply outlet, supply
	DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
	DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
	DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate( 1 );
	DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
	DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 0;
	DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 0;
	DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumIn = 1;
	DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumOut = 2;

	// Set up the required node data
	DataLoopNode::Node.allocate(2);
	DataLoopNode::Node(2).MassFlowRate = 1.0;

	// Set up a cooling setpoint manager
	SetPointManager::DefineReturnWaterChWSetPointManager mySPM;
	mySPM.returnNodeIndex = 1;
	mySPM.supplyNodeIndex = 2;
	mySPM.plantLoopIndex = 0;
	mySPM.minimumChilledWaterSetpoint = 7;
	mySPM.maximumChilledWaterSetpoint = 10;
	mySPM.returnTemperatureConstantTarget = 12;

	// test 1: normal, in range
	DataLoopNode::Node(1).Temp = 11;
	DataLoopNode::Node(2).Temp = 7;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// on the first pass through it should detect the plant loop it manages
	EXPECT_EQ(1, mySPM.plantLoopIndex);
	// with a delta T of 4, and a target return of 12, it should produce 8
	EXPECT_EQ(8, mySPM.currentSupplySetPt);

	// test 2: hit the maximum reset range
	DataLoopNode::Node(1).Temp = 8;
	DataLoopNode::Node(2).Temp = 7;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// with a delta T of 1, and a target return of 12, it should try to produce 11, but be capped at 10
	EXPECT_EQ(10, mySPM.currentSupplySetPt);

	// test 3: hit the minimum reset range
	DataLoopNode::Node(1).Temp = 13;
	DataLoopNode::Node(2).Temp = 7;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// with a delta T of 6, and a target return of 12, it should try to produce 6, but be capped at 7
	EXPECT_EQ(7, mySPM.currentSupplySetPt);

	// test 4: no flow
	DataLoopNode::Node(1).Temp = 11;
	DataLoopNode::Node(2).Temp = 7;
	DataLoopNode::Node(2).MassFlowRate = 0.0;
	// with Qdemand calculated as zero, it should leave early, but only after setting setpt to design
	EXPECT_EQ(7, mySPM.currentSupplySetPt);

	// test 5: backward delta T (something awry...)
	DataLoopNode::Node(1).Temp = 5;
	DataLoopNode::Node(2).Temp = 7;
	DataLoopNode::Node(2).MassFlowRate = 1.0;
	// with Qdemand calculated as negative, it should leave early, but only after setting setpt to design
	EXPECT_EQ(7, mySPM.currentSupplySetPt);

	// test 6: can't identify plant loop
	// this is actually a passing case, since the plant may not be built when this is first called
	// the PlantManager will do a validation to ensure that a setpoint is found, so we don't have to validate here
	// this code simply defaults to getting a fluid index of 1 (water) if it can't find a matching plant
	DataLoopNode::Node(1).Temp = 11;
	DataLoopNode::Node(2).Temp = 7;
	DataLoopNode::Node(2).MassFlowRate = 1.0;
	DataPlant::PlantLoop(1).LoopSide(2).NodeNumOut = 5; // Supply outlet, supply
	mySPM.plantLoopIndex = 0;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// this time it shouldn't detect which plant it was found on
	EXPECT_EQ(0, mySPM.plantLoopIndex);
	// with a delta T of 4, and a target return of 12, it should produce 8
	EXPECT_EQ(8, mySPM.currentSupplySetPt);

	// tear down
	DataLoopNode::Node.deallocate();
	DataPlant::PlantLoop(1).LoopSide.deallocate();
	DataPlant::PlantLoop.deallocate();

}

TEST( SetPointManager, DefineReturnWaterHWSetPointManager )
{

	// Set up the required plant loop data
	DataPlant::TotNumLoops = 1;
	DataPlant::PlantLoop.allocate(1);
	DataPlant::PlantLoop(1).FluidIndex = 1;
	DataPlant::PlantLoop(1).LoopSide.allocate(2);
	DataPlant::PlantLoop(1).LoopSide(2).NodeNumIn = 1; // Supply inlet, return
	DataPlant::PlantLoop(1).LoopSide(2).NodeNumOut = 2; // Supply outlet, supply
	DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
	DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
	DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate( 1 );
	DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
	DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 0;
	DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 0;
	DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumIn = 1;
	DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumOut = 2;

	// Set up the required node data
	DataLoopNode::Node.allocate(2);
	DataLoopNode::Node(2).MassFlowRate = 1.0;

	// Set up a cooling setpoint manager
	SetPointManager::DefineReturnWaterHWSetPointManager mySPM;
	mySPM.returnNodeIndex = 1;
	mySPM.supplyNodeIndex = 2;
	mySPM.plantLoopIndex = 0;
	mySPM.maximumHotWaterSetpoint = 60;
	mySPM.minimumHotWaterSetpoint = 57;
	mySPM.returnTemperatureConstantTarget = 55;
	DataLoopNode::Node(2).Temp = 60;

	// test 1: normal, in range
	DataLoopNode::Node(1).Temp = 56;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// on the first pass through it should detect the plant loop it manages
	EXPECT_EQ(1, mySPM.plantLoopIndex);
	// with a delta T of 4, and a target return of 55, it should produce 59
	EXPECT_EQ(59, mySPM.currentSupplySetPt);

	// test 2: hit the minimum reset range
	DataLoopNode::Node(1).Temp = 59;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// with a delta T of 1, and a target return of 55, it should try to produce 56, but be capped at 57
	EXPECT_EQ(57, mySPM.currentSupplySetPt);

	// test 3: hit the maximum reset range
	DataLoopNode::Node(1).Temp = 54;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// with a delta T of 6, and a target return of 55, it should try to produce 61, but be capped at 60
	EXPECT_EQ(60, mySPM.currentSupplySetPt);

	// test 4: no flow
	DataLoopNode::Node(1).Temp = 56;
	DataLoopNode::Node(2).MassFlowRate = 0.0;
	// with Qdemand calculated as zero, it should leave early, but only after setting setpt to design
	EXPECT_EQ(60, mySPM.currentSupplySetPt);

	// test 5: backward delta T (something awry...)
	DataLoopNode::Node(1).Temp = 61;
	DataLoopNode::Node(2).MassFlowRate = 1.0;
	// with Qdemand calculated as negative, it should leave early, but only after setting setpt to design
	EXPECT_EQ(60, mySPM.currentSupplySetPt);

	// test 6: can't identify plant loop
	// this is actually a passing case, since the plant may not be built when this is first called
	// the PlantManager will do a validation to ensure that a setpoint is found, so we don't have to validate here
	// this code simply defaults to getting a fluid index of 1 (water) if it can't find a matching plant
	DataLoopNode::Node(1).Temp = 56;
	DataLoopNode::Node(2).MassFlowRate = 1.0;
	DataPlant::PlantLoop(1).LoopSide(2).NodeNumOut = 5; // Supply outlet, supply
	mySPM.plantLoopIndex = 0;
	mySPM.calculate(DataLoopNode::Node(1), DataLoopNode::Node(2));
	// this time it shouldn't detect which plant it was found on
	EXPECT_EQ(0, mySPM.plantLoopIndex);
	// with a delta T of 4, and a target return of 55, it should produce 59
	EXPECT_EQ(59, mySPM.currentSupplySetPt);

	// tear down
	DataLoopNode::Node.deallocate();
	DataPlant::PlantLoop(1).LoopSide.deallocate();
	DataPlant::PlantLoop.deallocate();

}

TEST_F( EnergyPlusFixture, CalcScheduledTESSetPoint )
{
	int const CoolOpComp ( 1 ); // a component that cools only (chillers)
	int const DualOpComp ( 2 ); // a component that heats or cools (ice storage tank)

	int schManNum = 1;
	SetPointManager::SchTESSetPtMgr.allocate(schManNum);
	SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp = 5;
	SetPointManager::SchTESSetPtMgr(schManNum).ChargeCHWTemp = -5;

	int const OnSched  = 1;
	int const OffSched = 2;
	std::string const idf_contents( delimited_string( {
		"Schedule:Constant,MyScheduleOn,,1;",
		"Schedule:Constant,MyScheduleOff,,0;",
	} ) );
	ASSERT_FALSE(process_idf(idf_contents));
	DataGlobals::NumOfTimeStepInHour = 4;
	DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
	ScheduleManager::ProcessScheduleInput();
	DataGlobals::TimeStep = 1;
	DataGlobals::HourOfDay = 1;
	DataEnvironment::DayOfWeek = 1;
	DataEnvironment::DayOfYear_Schedule = 1;
	ScheduleManager::UpdateScheduleValues();

	SetPointManager::SchTESSetPtMgr(schManNum).CompOpType = CoolOpComp;

		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtr = OnSched;

			SetPointManager::CalcScheduledTESSetPoint(schManNum);
			EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtr = OffSched;
		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtrCharge = OffSched;

			SetPointManager::CalcScheduledTESSetPoint(schManNum);
			EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtr = OffSched;
		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtrCharge = OnSched;

			SetPointManager::CalcScheduledTESSetPoint(schManNum);
			EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).ChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

	SetPointManager::SchTESSetPtMgr(schManNum).CompOpType = DualOpComp;

		SetPointManager::CalcScheduledTESSetPoint(schManNum);
		EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

}

TEST_F( HVACFixture, SZRHOAFractionImpact ) {
		std::string const idf_objects = delimited_string( { 
		"Version,8.4;",
		"SetpointManager:SingleZone:Reheat,",
		"    SupAirTemp MngrKitchen,    !- Name",
		"    Temperature,              !- Control Variable",
		"    12.8,                     !- Minimum Supply Air Temperature",
		"    40.0,                     !- Maximum Supply Air Temperature",
		"    Kitchen,                  !- Control Zone Name",
		"    Kitchen Air Node,         !- Zone Node Name",
		"    Kitchen Direct Air Inlet Node Name,    !- Zone Inlet Node Name",
		"    PSZ-AC_2:2 Supply Equipment Outlet Node;    !- Setpoint Node or NodeList Name",

		} ) ;

		ASSERT_FALSE( process_idf( idf_objects ) );
		bool ErrorsFound =  false;
		DataGlobals::NumOfZones = 1;

		DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );
		DataHeatBalance::Zone( 1).Name = "KITCHEN";

		DataAirLoop::AirLoopFlow.allocate( 1 );

		DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate( 1 );
		DataZoneEnergyDemands::DeadBandOrSetback.allocate( 1 );

		DataAirSystems::PrimaryAirSystem.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).OASysOutletNodeNum = NodeInputManager::GetOnlySingleNode( "FAN INLET NODE", ErrorsFound, "FAN", "SZRHtest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node" );
		DataAirSystems::PrimaryAirSystem( 1 ).OASysInletNodeNum =  NodeInputManager::GetOnlySingleNode( "RETURN NODE", ErrorsFound, "OA MIXER", "SZRHtest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent, "AHU node" );
		DataAirSystems::PrimaryAirSystem( 1 ).OAMixOAInNodeNum = NodeInputManager::GetOnlySingleNode( "OA INLET TO MIXER", ErrorsFound, "OA MIXER", "SZRHtest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node" );
		DataAirSystems::PrimaryAirSystem( 1 ).NumBranches = 1;
		DataAirSystems::PrimaryAirSystem( 1 ).InletBranchNum.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).InletBranchNum( 1 ) = 1;

		DataAirSystems::PrimaryAirSystem( 1 ).Branch.allocate( DataAirSystems::PrimaryAirSystem( 1 ).NumBranches );

		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).NodeNumIn =  NodeInputManager::GetOnlySingleNode( "RETURN NODE", ErrorsFound, "OAsysinlet", "SZRHtest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent, "AHU node" );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).TotalComponents = 1;
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).TypeOf = "Fan:ConstantVolume";

		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = NodeInputManager::GetOnlySingleNode( "FAN INLET NODE", ErrorsFound, "FAN", "SZRHtest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node" );

		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut =NodeInputManager::GetOnlySingleNode( "FAN OUTLET NODE", ErrorsFound, "FAN", "SZRHtest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node" );

		DataZoneEquipment::ZoneEquipConfig.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).NumInletNodes = 1;
		DataZoneEquipment::ZoneEquipConfig( 1 ).IsControlled = true;
		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitCool.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirDistUnitHeat.allocate( 1 );
		DataZoneEquipment::ZoneEquipConfig( 1 ).InletNode( 1 ) = 3;
		DataZoneEquipment::ZoneEquipConfig( 1 ).ZoneNode = NodeInputManager::GetOnlySingleNode( "KITCHEN AIR NODE", ErrorsFound, "Zone", "SZRHspmTest", DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_ZoneNode, 1, DataLoopNode::ObjectIsNotParent, "Test zone node" );
		DataZoneEquipment::ZoneEquipConfig( 1 ).AirLoopNum = 1;

		SetPointManager::GetSetPointManagerInputs();
		EXPECT_EQ( SetPointManager::SingZoneRhSetPtMgr( 1 ).ControlZoneNum, 1);
		SetPointManager::SingZoneRhSetPtMgr( 1 ).AirLoopNum = 1;

		DataZoneEquipment::ZoneEquipInputsFilled = true;
		DataAirLoop::AirLoopInputsFilled = true;

		SetPointManager::InitSetPointManagers();

		DataAirLoop::AirLoopFlow( 1 ).OAFrac     = 1.0;
		DataAirLoop::AirLoopFlow( 1 ).OAMinFrac  = 0.8;

		DataLoopNode::Node( 6 ).MassFlowRate     = 1.0; // sent zone inlet mass flwo
		DataLoopNode::Node( 6 ).HumRat           = 0.0008;
		DataLoopNode::Node( 6 ).Temp             = 20.0;

		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 0.0;
		// pick these next values so ExtrRateNoHC doesn't exceed loat to sp
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).OutputRequiredToCoolingSP = 6000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( 1 ).OutputRequiredToHeatingSP = -4000.0;
		DataZoneEnergyDemands::DeadBandOrSetback( 1 ) = true;

		DataLoopNode::Node( 5 ).Temp = 22.0; // zone air node 
		DataLoopNode::Node( 5 ).HumRat = 0.0008; 

		DataLoopNode::Node( 2 ).HumRat = 0.0008; // return node
		DataLoopNode::Node( 2 ).Temp = 22.0;
		DataLoopNode::Node( 2 ).Enthalpy = Psychrometrics::PsyHFnTdbW( DataLoopNode::Node( 2 ).Temp, DataLoopNode::Node( 2 ).HumRat );

		DataLoopNode::Node( 1 ).Temp  = 16.0;
		DataLoopNode::Node( 4 ).Temp  = 17.0; // fan rise

		// slightly cool OA
		DataLoopNode::Node( 3 ).HumRat = 0.0006; // OA intake 
		DataLoopNode::Node( 3 ).Temp = 16.0;
		DataLoopNode::Node( 3 ).Enthalpy = Psychrometrics::PsyHFnTdbW( DataLoopNode::Node( 3 ).Temp , DataLoopNode::Node( 3 ).HumRat );

		SetPointManager::SimSetPointManagers();
		SetPointManager::UpdateSetPointManagers();

		// node number table
		//  1   Fan Inlet Node   OA system outlet 
		//  2   Return Node      from zone, first on branch
		//  3   OA inlet to Mixer,  Outdoor air supplying OA damper 
		//  4   Fan Outlet Node 
		//  5   Kitchen Air Node
		//  6   Kitchen Direct Air INlet Node Name
		//  7   PSZ-AC_2:2 Supply Equipment Outlet Node

		EXPECT_NEAR( DataLoopNode::Node(7).TempSetPoint , 18.0251495, 0.001);

		DataAirLoop::AirLoopFlow( 1 ).OAFrac     = 0.8;
		DataAirLoop::AirLoopFlow( 1 ).OAMinFrac  = 0.8;

		SetPointManager::SimSetPointManagers();
		SetPointManager::UpdateSetPointManagers();

		EXPECT_NEAR( DataLoopNode::Node(7).TempSetPoint , 18.20035, 0.001);

		// warmer day outside
		DataAirLoop::AirLoopFlow( 1 ).OAFrac     = 1.0;
		DataAirLoop::AirLoopFlow( 1 ).OAMinFrac  = 0.8;

		DataLoopNode::Node( 3 ).HumRat = 0.0006; // OA intake 
		DataLoopNode::Node( 3 ).Temp = 26.0;
		DataLoopNode::Node( 3 ).Enthalpy = Psychrometrics::PsyHFnTdbW( DataLoopNode::Node( 3 ).Temp, DataLoopNode::Node( 3 ).HumRat );

		DataLoopNode::Node( 1 ).Temp  = 26.0;
		DataLoopNode::Node( 4 ).Temp  = 27.0;  // fan rise

		SetPointManager::SimSetPointManagers();
		SetPointManager::UpdateSetPointManagers();

		EXPECT_NEAR( DataLoopNode::Node(7).TempSetPoint , 27.0 , 0.001);

		DataAirLoop::AirLoopFlow( 1 ).OAFrac     = 0.8;
		DataAirLoop::AirLoopFlow( 1 ).OAMinFrac  = 0.8;

		SetPointManager::SimSetPointManagers();
		SetPointManager::UpdateSetPointManagers();

		EXPECT_NEAR( DataLoopNode::Node(7).TempSetPoint , 26.19976, 0.001);


}
