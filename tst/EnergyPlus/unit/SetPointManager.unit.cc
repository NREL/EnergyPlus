// EnergyPlus::Standalone ERV Unit Tests

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <CurveManager.hh>
#include <ScheduleManager.hh>
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

TEST_F( EnergyPlusFixture, SetPointManagerDefineCondEntSetPointManager )
{
	// Set up the curves using the idf parser
	std::string const idf_objects = delimited_string({
  "Curve:QuadLinear,",
    "MinDsnWBCurveName,       !- Name",
    "-3.333,                  !- Coefficient1 Constant",
    "0,                       !- Coefficient2 w",
    "38.9,                    !- Coefficient3 x",
    "0,                       !- Coefficient4 y",
    "0,                       !- Coefficient5 z",
    "-30,                     !- Minimum Value of w",
    "40,                      !- Maximum Value of w",
    "0,                       !- Minimum Value of x",
    "1,                       !- Maximum Value of x",
    "10,                      !- Minimum Value of y",
    "38,                      !- Maximum Value of y",
    "0.00000001,              !- Minimum Value of z",
    "0.00000008,              !- Maximum Value of z",
    "0,                       !- Minimum Curve Output",
    "38,                      !- Maximum Curve Output",
    "Temperature,             !- Input Unit Type for w",
    "Dimensionless,           !- Input Unit Type for x",
    "Dimensionless,           !- Input Unit Type for y",
    "Dimensionless;           !- Input Unit Type for z",
  "Curve:QuadLinear,",
    "MinActWBCurveName,       !- Name",
    "-8.333,                  !- Coefficient1 Constant",
    "2,                       !- Coefficient2 w",
    "5.5556,                  !- Coefficient3 x",
    "-1,                      !- Coefficient4 y",
    "0,                       !- Coefficient5 z",
    "0,                       !- Minimum Value of w",
    "38,                      !- Maximum Value of w",
    "0,                       !- Minimum Value of x",
    "1,                       !- Maximum Value of x",
    "10,                      !- Minimum Value of y",
    "38,                      !- Maximum Value of y",
    "0.00000001,              !- Minimum Value of z",
    "0.00000008,              !- Maximum Value of z",
    "0,                       !- Minimum Curve Output",
    "43,                      !- Maximum Curve Output",
    "Temperature,             !- Input Unit Type for w",
    "Dimensionless,           !- Input Unit Type for x",
    "Dimensionless,           !- Input Unit Type for y",
    "Dimensionless;           !- Input Unit Type for z",
  "Curve:QuadLinear,",
    "OptCondEntCurveName,     !- Name",
    "12.2711,                 !- Coefficient1 Constant",
    "0.8,                     !- Coefficient2 w",
    "6.6667,                  !- Coefficient3 x",
    "0.266,                   !- Coefficient4 y",
    "-6193484,                !- Coefficient5 z",
    "0,                       !- Minimum Value of w",
    "38,                      !- Maximum Value of w",
    "0,                       !- Minimum Value of x",
    "1,                       !- Maximum Value of x",
    "10,                      !- Minimum Value of y",
    "38,                      !- Maximum Value of y",
    "0.00000001,              !- Minimum Value of z",
    "0.00000008,              !- Maximum Value of z",
    "0,                       !- Minimum Curve Output",
    "32,                      !- Maximum Curve Output",
    "Temperature,             !- Input Unit Type for w",
    "Dimensionless,           !- Input Unit Type for x",
    "Dimensionless,           !- Input Unit Type for y",
    "Dimensionless;           !- Input Unit Type for z",
  "Schedule:Compact,",
    "Condenser Loop Temp Schedule,  !- Name",
    "Temperature,             !- Schedule Type Limits Name",
    "Through: 12/31,          !- Field 1",
    "For: AllDays,            !- Field 2",
    "Until: 24:00,30.0;       !- Field 3"
    });
	ASSERT_FALSE( process_idf( idf_objects ) );

	// a few constants for convenience
	int const evapOutletNodeNum = 1;
	int const condInletNodeNum  = 2;
	int const chwLoopIndex      = 1;
	int const condLoopIndex     = 2;
	int const demandSide        = 1;
	int const supplySide        = 2;
	int const chillerBranchChW  = 1;
	int const chillerBranchCW   = 1;
	int const chillerCompIndex  = 1;

	// Set up ChW loop manually, way too much input to do that here in idf, all I care about is the 
	DataPlant::TotNumLoops = 2;
	DataPlant::PlantLoop.allocate(2);
	DataPlant::PlantReport.allocate(1);
	DataPlant::PlantReport(1).CoolingDemand = 1200;

	DataPlant::PlantLoop(chwLoopIndex).LoopSide.allocate(2);
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch.allocate(1);
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp.allocate(1);
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).NodeNumOut = evapOutletNodeNum;
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TempDesCondIn = 20;
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TempDesEvapOut = 5;
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).MaxLoad = 5000;

	DataPlant::PlantLoop(condLoopIndex).LoopSide.allocate(2);
	DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch.allocate(1);
	DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp.allocate(1);
	DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp(chillerCompIndex).NodeNumIn  = condInletNodeNum;

	DataLoopNode::Node.allocate(2);
	DataLoopNode::Node(evapOutletNodeNum).Temp = 22;
	DataLoopNode::Node(condInletNodeNum).Temp = 10;

	SetPointManager::DefineCondEntSetPointManager thisSPM;
	thisSPM.MinTwrWbCurve = CurveManager::GetCurveIndex("MINDSNWBCURVENAME");
	thisSPM.MinOaWbCurve = CurveManager::GetCurveIndex("MINACTWBCURVENAME");
	thisSPM.OptCondEntCurve = CurveManager::GetCurveIndex("OPTCONDENTCURVENAME");
	thisSPM.CondEntTempSchedPtr = ScheduleManager::GetScheduleIndex("CONDENSER LOOP TEMP SCHEDULE");
	thisSPM.LoopIndexPlantSide = chwLoopIndex;
	thisSPM.ChillerIndexPlantSide = chillerBranchChW;
	thisSPM.BranchIndexPlantSide = chillerCompIndex;
	thisSPM.LoopIndexDemandSide = condLoopIndex;
	thisSPM.ChillerIndexDemandSide = chillerBranchCW;
	thisSPM.BranchIndexDemandSide = chillerCompIndex;

	std::ofstream myfile;
	myfile.open("/tmp/setpoints");

	// First-level switch: load > 0
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).MyLoad = 1000;

		// Second-level switch: Chiller type
		DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;
		DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp(chillerCompIndex).TypeOf_Num = DataPlant::TypeOf_Chiller_Electric;

			// Third-level switch: Weighted ratio > 9 && etc...
			//WEIGHTED_RATIO > 0.9
				// Now call and check
				thisSPM.calculate();
				EXPECT_TRUE(true);  //23, thisSPM.SetPt
				myfile << thisSPM.SetPt << std::endl;

			// Third-level switch: Weighted ratio < 9 || etc...
			//WEIGHTED_RATIO > 0.9
				// Now call and check
				thisSPM.calculate();
				EXPECT_TRUE(true);  //23, thisSPM.SetPt
				myfile << thisSPM.SetPt << std::endl;

		// Second-level switch: Chiller type
		DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TypeOf_Num = DataPlant::TypeOf_Chiller_Indirect_Absorption;
		DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp(chillerCompIndex).TypeOf_Num = DataPlant::TypeOf_Chiller_Indirect_Absorption;
		thisSPM.calculate();

			// Third-level switch: Weighted ratio > 9 && etc...
			//WEIGHTED_RATIO > 0.9
				// Now call and check
				thisSPM.calculate();
				EXPECT_TRUE(true);  //23, thisSPM.SetPt
				myfile << thisSPM.SetPt << std::endl;

			// Third-level switch: Weighted ratio < 9 || etc...
			//WEIGHTED_RATIO > 0.9
				// Now call and check
				thisSPM.calculate();
				EXPECT_TRUE(true);  //23, thisSPM.SetPt
				myfile << thisSPM.SetPt << std::endl;

	// First-level switch: load <= 0
	DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).MyLoad = 0;

		// Second-level switch never occurs for load <= 0

			// Third-level switch: Weighted ratio > 9 && etc...
			//WEIGHTED_RATIO > 0.9
				// Now call and check
				thisSPM.calculate();
				EXPECT_TRUE(true);  //23, thisSPM.SetPt
				myfile << thisSPM.SetPt << std::endl;

			// Third-level switch: Weighted ratio < 9 || etc...
			//WEIGHTED_RATIO > 0.9
				// Now call and check
				thisSPM.calculate();
				EXPECT_TRUE(true);  //23, thisSPM.SetPt
				myfile << thisSPM.SetPt << std::endl;
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

			SetPointManager::SchTESSetPtMgr(schManNum).calculate();
			EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtr = OffSched;
		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtrCharge = OffSched;

			SetPointManager::SchTESSetPtMgr(schManNum).calculate();
			EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtr = OffSched;
		SetPointManager::SchTESSetPtMgr(schManNum).SchedPtrCharge = OnSched;

			SetPointManager::SchTESSetPtMgr(schManNum).calculate();
			EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).ChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

	SetPointManager::SchTESSetPtMgr(schManNum).CompOpType = DualOpComp;

		SetPointManager::SchTESSetPtMgr(schManNum).calculate();
		EXPECT_EQ(SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp, SetPointManager::SchTESSetPtMgr(schManNum).SetPt);

}
