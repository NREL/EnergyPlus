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

#include "Fixtures/EnergyPlusFixture.hh"

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
