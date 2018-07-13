// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// EnergyPlus::Standalone ERV Unit Tests

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DirectAirManager.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/ReturnAirPathManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>
#include <HeatBalanceManager.hh>
#include <NodeInputManager.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SimAirServingZones.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, SetPointManager_DefineReturnWaterChWSetPointManager)
{

    // Set up the required plant loop data
    DataPlant::TotNumLoops = 1;
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(2).NodeNumIn = 1;  // Supply inlet, return
    DataPlant::PlantLoop(1).LoopSide(2).NodeNumOut = 2; // Supply outlet, supply
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
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

TEST_F(EnergyPlusFixture, SetPointManager_DefineReturnWaterHWSetPointManager)
{

    // Set up the required plant loop data
    DataPlant::TotNumLoops = 1;
    DataPlant::PlantLoop.allocate(1);
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).LoopSide.allocate(2);
    DataPlant::PlantLoop(1).LoopSide(2).NodeNumIn = 1;  // Supply inlet, return
    DataPlant::PlantLoop(1).LoopSide(2).NodeNumOut = 2; // Supply outlet, supply
    DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
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

TEST_F(EnergyPlusFixture, SetPointManager_DefineCondEntSetPointManager)
{
    // Set up the curves using the idf parser
    std::string const idf_objects = delimited_string({"Curve:QuadLinear,",
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
                                                      "Until: 24:00,30.0;       !- Field 3"});
    ASSERT_TRUE(process_idf(idf_objects));
    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
    ScheduleManager::ProcessScheduleInput();
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfYear_Schedule = 1;
    ScheduleManager::UpdateScheduleValues();

    // a few constants for convenience
    int const evapOutletNodeNum = 1;
    int const condInletNodeNum = 2;
    int const chwLoopIndex = 1;
    int const condLoopIndex = 2;
    int const demandSide = 1;
    int const supplySide = 2;
    int const chillerBranchChW = 1;
    int const chillerBranchCW = 1;
    int const chillerCompIndex = 1;

    // Set up ChW loop manually, way too much input to do that here in idf, all I care about is the
    DataPlant::TotNumLoops = 2;
    DataPlant::PlantLoop.allocate(2);
    DataPlant::PlantReport.allocate(1);

    DataPlant::PlantLoop(chwLoopIndex).LoopSide.allocate(2);
    DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch.allocate(1);
    DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp.allocate(1);
    DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).NodeNumOut = evapOutletNodeNum;
    Real64 const designCondenserEnteringTemp = 20;
    DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TempDesCondIn =
        designCondenserEnteringTemp;
    DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TempDesEvapOut = 5;
    DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).MaxLoad = 5000;

    DataPlant::PlantLoop(condLoopIndex).LoopSide.allocate(2);
    DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch.allocate(1);
    DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp.allocate(1);
    DataPlant::PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp(chillerCompIndex).NodeNumIn = condInletNodeNum;
    DataPlant::PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).MyLoad = 1000;

    DataLoopNode::Node.allocate(2);
    DataLoopNode::Node(evapOutletNodeNum).Temp = 7;
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
    thisSPM.TypeNum = DataPlant::TypeOf_Chiller_Electric;

    // switch: Weighted ratio > 9 && etc...
    DataPlant::PlantReport(1).CoolingDemand = 4700;

    // Now call and check
    thisSPM.calculate();
    EXPECT_NEAR(designCondenserEnteringTemp + 1.0, thisSPM.SetPt, 0.001);

    // switch: Weighted ratio < 9 || etc...
    DataPlant::PlantReport(1).CoolingDemand = 4000;

    // switch: OAWB>MinWb && DesignWB>MinDesignWB && CurLift>MinLift
    DataEnvironment::OutWetBulbTemp = 40;
    thisSPM.TowerDsnInletAirWetBulb = 35;
    thisSPM.MinimumLiftTD = 2;

    // Now call and check
    thisSPM.calculate();
    EXPECT_NEAR(32, thisSPM.SetPt, 0.001);

    // switch: ELSE
    DataEnvironment::OutWetBulbTemp = 30;
    thisSPM.TowerDsnInletAirWetBulb = 20;
    thisSPM.MinimumLiftTD = 5;

    // Now call and check
    thisSPM.calculate();
    EXPECT_NEAR(30, thisSPM.SetPt, 0.001);
}

TEST_F(EnergyPlusFixture, SetPointManager_setupSetPointAndFlags)
{

    Real64 totEnergy = 0.0;
    Real64 totEnergyPrevious = 0.0;
    Real64 condenserWaterSetPoint = 0.0;
    Real64 condenserWaterSetPointLimit = 10.0;
    bool statusRunOptimalCondenserEnteringTemp = false;
    bool statusRunSubOptimalCondenserEnteringTemp = false;
    bool statusRunFinalOptimalCondenserEnteringTemp = false;

    SetPointManager::DefineIdealCondEntSetPointManager thisSPM;
    thisSPM.MaxCondEntTemp = 25;

    // first pass through, leave totEnergyPrevious == 0 to kick things off but initialize current energy
    totEnergy = 1000.0;
    thisSPM.setupSetPointAndFlags(totEnergy, totEnergyPrevious, condenserWaterSetPoint, condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp, statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the values should be initialized
    // the setpoint should be set to max - 1
    EXPECT_NEAR(24, condenserWaterSetPoint, 0.0001);
    // the energy should be stored in the previous energy variable
    EXPECT_EQ(totEnergy, totEnergyPrevious);
    // the run flag should be turned on to start simulating
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    // the sub run flag should be reset to false
    EXPECT_FALSE(statusRunSubOptimalCondenserEnteringTemp);

    // second pass through, continue the optimization by having it find a lower energy usage
    totEnergy = 800.0;
    thisSPM.setupSetPointAndFlags(totEnergy, totEnergyPrevious, condenserWaterSetPoint, condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp, statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should decrement the setpoint and continue searching, storing this energy for next time
    EXPECT_NEAR(23, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_EQ(totEnergy, totEnergyPrevious);

    // third pass through have it pass the optimal point by going higher energy
    totEnergy = 900;
    thisSPM.setupSetPointAndFlags(totEnergy, totEnergyPrevious, condenserWaterSetPoint, condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp, statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should realize it passed the optimal point, back track and then set the sub-optimazation flags
    EXPECT_NEAR(23.8, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_TRUE(statusRunSubOptimalCondenserEnteringTemp);

    // fourth pass through it will be doing the sub-optimization search; perform one search; energy goes down this time
    totEnergyPrevious = 900;
    totEnergy = 890;
    thisSPM.setupSetPointAndFlags(totEnergy, totEnergyPrevious, condenserWaterSetPoint, condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp, statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should realize it has yet again overshot and start trying to work downward carefully
    EXPECT_NEAR(23.6, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_TRUE(statusRunSubOptimalCondenserEnteringTemp);

    // fifth pass through it will have hit the optimal point; it will set the setpoint back and set the final run flags
    totEnergy = 895;
    thisSPM.setupSetPointAndFlags(totEnergy, totEnergyPrevious, condenserWaterSetPoint, condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp, statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should increment the energy back to where it was, and set the final flags
    EXPECT_NEAR(23.8, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_FALSE(statusRunSubOptimalCondenserEnteringTemp);
    EXPECT_TRUE(statusRunFinalOptimalCondenserEnteringTemp);

    // and finally, the sixth pass through when it is set to run final; totEnergy doesn't matter when that flag is true, and the sp shouldn't change
    thisSPM.setupSetPointAndFlags(totEnergy, totEnergyPrevious, condenserWaterSetPoint, condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp, statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    EXPECT_NEAR(23.8, condenserWaterSetPoint, 0.0001);
    EXPECT_FALSE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_FALSE(statusRunSubOptimalCondenserEnteringTemp);
    EXPECT_FALSE(statusRunFinalOptimalCondenserEnteringTemp);
}

TEST_F(EnergyPlusFixture, CalcScheduledTESSetPoint)
{
    int const CoolOpComp(1); // a component that cools only (chillers)
    int const DualOpComp(2); // a component that heats or cools (ice storage tank)

    int schManNum = 1;
    SetPointManager::SchTESSetPtMgr.allocate(schManNum);
    SetPointManager::SchTESSetPtMgr(schManNum).NonChargeCHWTemp = 5;
    SetPointManager::SchTESSetPtMgr(schManNum).ChargeCHWTemp = -5;

    // indexes in Schedule
    int const OnSched = 1;
    int const OffSched = 2;
    std::string const idf_contents(delimited_string({
        "Schedule:Constant,MyScheduleOn,,1;",
        "Schedule:Constant,MyScheduleOff,,0;",
    }));
    ASSERT_TRUE(process_idf(idf_contents));
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

TEST_F(EnergyPlusFixture, SZRHOAFractionImpact)
{
    std::string const idf_objects = delimited_string({
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

    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    DataGlobals::NumOfZones = 1;

    DataHeatBalance::Zone.allocate(DataGlobals::NumOfZones);
    DataHeatBalance::Zone(1).Name = "KITCHEN";

    DataAirLoop::AirLoopFlow.allocate(1);

    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::DeadBandOrSetback.allocate(1);

    DataAirSystems::PrimaryAirSystem.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).OASysOutletNodeNum =
        NodeInputManager::GetOnlySingleNode("FAN INLET NODE", ErrorsFound, "FAN", "SZRHtest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node");
    DataAirSystems::PrimaryAirSystem(1).OASysInletNodeNum =
        NodeInputManager::GetOnlySingleNode("RETURN NODE", ErrorsFound, "OA MIXER", "SZRHtest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent, "AHU node");
    DataAirSystems::PrimaryAirSystem(1).OAMixOAInNodeNum =
        NodeInputManager::GetOnlySingleNode("OA INLET TO MIXER", ErrorsFound, "OA MIXER", "SZRHtest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node");
    DataAirSystems::PrimaryAirSystem(1).NumBranches = 1;
    DataAirSystems::PrimaryAirSystem(1).InletBranchNum.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).InletBranchNum(1) = 1;

    DataAirSystems::PrimaryAirSystem(1).Branch.allocate(DataAirSystems::PrimaryAirSystem(1).NumBranches);

    DataAirSystems::PrimaryAirSystem(1).Branch(1).NodeNumIn =
        NodeInputManager::GetOnlySingleNode("RETURN NODE", ErrorsFound, "OAsysinlet", "SZRHtest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent, "AHU node");
    DataAirSystems::PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "Fan:ConstantVolume";

    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).NodeNumIn =
        NodeInputManager::GetOnlySingleNode("FAN INLET NODE", ErrorsFound, "FAN", "SZRHtest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node");

    DataAirSystems::PrimaryAirSystem(1).Branch(1).Comp(1).NodeNumOut =
        NodeInputManager::GetOnlySingleNode("FAN OUTLET NODE", ErrorsFound, "FAN", "SZRHtest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_Internal, 1, DataLoopNode::ObjectIsNotParent, "AHU node");

    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).IsControlled = true;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    int zoneAirNode =
        NodeInputManager::GetOnlySingleNode("KITCHEN AIR NODE", ErrorsFound, "Zone", "SZRHspmTest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_ZoneNode, 1, DataLoopNode::ObjectIsNotParent, "Test zone node");
    DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = zoneAirNode;
    int zoneInletNode =
        NodeInputManager::GetOnlySingleNode("KITCHEN DIRECT AIR INLET NODE NAME", ErrorsFound, "Zone", "SZRHspmTest", DataLoopNode::NodeType_Air,
                                            DataLoopNode::NodeConnectionType_ZoneInlet, 1, DataLoopNode::ObjectIsNotParent, "Test zone inlet node");
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = zoneInletNode;
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;

    SetPointManager::GetSetPointManagerInputs();
    EXPECT_EQ(SetPointManager::SingZoneRhSetPtMgr(1).ControlZoneNum, 1);
    SetPointManager::SingZoneRhSetPtMgr(1).AirLoopNum = 1;

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    DataAirLoop::AirLoopInputsFilled = true;

    SetPointManager::InitSetPointManagers();

    DataAirLoop::AirLoopFlow(1).OAFrac = 1.0;
    DataAirLoop::AirLoopFlow(1).OAMinFrac = 0.8;

    DataLoopNode::Node(zoneInletNode).MassFlowRate = 1.0; // set zone inlet mass flow
    DataLoopNode::Node(zoneInletNode).HumRat = 0.0008;
    DataLoopNode::Node(zoneInletNode).Temp = 20.0;

    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).TotalOutputRequired = 0.0;
    // pick these next values so ExtrRateNoHC doesn't exceed loat to sp
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = 6000.0;
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = -4000.0;
    DataZoneEnergyDemands::DeadBandOrSetback(1) = true;

    DataLoopNode::Node(zoneAirNode).Temp = 22.0; // zone air node
    DataLoopNode::Node(zoneAirNode).HumRat = 0.0008;

    DataLoopNode::Node(2).HumRat = 0.0008; // return node
    DataLoopNode::Node(2).Temp = 22.0;
    DataLoopNode::Node(2).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(2).Temp, DataLoopNode::Node(2).HumRat);

    DataLoopNode::Node(1).Temp = 16.0;
    DataLoopNode::Node(4).Temp = 17.0; // fan rise

    // slightly cool OA
    DataLoopNode::Node(3).HumRat = 0.0006; // OA intake
    DataLoopNode::Node(3).Temp = 16.0;
    DataLoopNode::Node(3).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(3).Temp, DataLoopNode::Node(3).HumRat);

    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();

    // node number table
    //  1   Fan Inlet Node   OA system outlet
    //  2   Return Node      from zone, first on branch
    //  3   OA inlet to Mixer,  Outdoor air supplying OA damper
    //  4   Fan Outlet Node
    //  5   Kitchen Air Node
    //  6   Kitchen Direct Air Inlet Node Name
    //  7   PSZ-AC_2:2 Supply Equipment Outlet Node

    EXPECT_NEAR(DataLoopNode::Node(7).TempSetPoint, 18.0251495, 0.001);

    DataAirLoop::AirLoopFlow(1).OAFrac = 0.8;
    DataAirLoop::AirLoopFlow(1).OAMinFrac = 0.8;

    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();

    EXPECT_NEAR(DataLoopNode::Node(7).TempSetPoint, 18.20035, 0.001);

    // warmer day outside
    DataAirLoop::AirLoopFlow(1).OAFrac = 1.0;
    DataAirLoop::AirLoopFlow(1).OAMinFrac = 0.8;

    DataLoopNode::Node(3).HumRat = 0.0006; // OA intake
    DataLoopNode::Node(3).Temp = 26.0;
    DataLoopNode::Node(3).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(3).Temp, DataLoopNode::Node(3).HumRat);

    DataLoopNode::Node(1).Temp = 26.0;
    DataLoopNode::Node(4).Temp = 27.0; // fan rise

    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();

    EXPECT_NEAR(DataLoopNode::Node(7).TempSetPoint, 27.0, 0.001);

    DataAirLoop::AirLoopFlow(1).OAFrac = 0.8;
    DataAirLoop::AirLoopFlow(1).OAMinFrac = 0.8;

    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();

    EXPECT_NEAR(DataLoopNode::Node(7).TempSetPoint, 26.19976, 0.001);
}

TEST_F(EnergyPlusFixture, SetPointManager_CalcSetPointTest)
{
    Real64 SetPt1, SetPt2, SetPt3, SetPt4, SetPt5, SetPt6, SetPt7, SetPt8;
    SetPointManager::DefineOutsideAirSetPointManager thisSPM;
    // CalcSetPoint(OutLowTemp, OutHighTemp, OutDryBulbTemp, SetTempAtOutLow, SetTempAtOutHigh);
    // SetTempAtOutLow > SetTempAtOutHigh
    SetPt1 = thisSPM.CalcSetPoint(10, 5, 0, 50, 60);
    SetPt2 = thisSPM.CalcSetPoint(5, 10, 0, 50, 60);
    SetPt3 = thisSPM.CalcSetPoint(5, 10, 15, 50, 60);
    SetPt4 = thisSPM.CalcSetPoint(5, 10, 8, 50, 60);
    // SetTempAtOutLow < SetTempAtOutHigh
    SetPt5 = thisSPM.CalcSetPoint(10, 5, 0, 60, 50);
    SetPt6 = thisSPM.CalcSetPoint(5, 10, 0, 60, 50);
    SetPt7 = thisSPM.CalcSetPoint(5, 10, 15, 60, 50);
    SetPt8 = thisSPM.CalcSetPoint(5, 10, 8, 60, 50);

    EXPECT_EQ(55, SetPt1);
    EXPECT_EQ(50, SetPt2);
    EXPECT_EQ(60, SetPt3);
    EXPECT_EQ(56, SetPt4);

    EXPECT_EQ(55, SetPt5);
    EXPECT_EQ(60, SetPt6);
    EXPECT_EQ(50, SetPt7);
    EXPECT_EQ(54, SetPt8);
}

TEST(SetPointManager, DefineMixedAirSetPointManager)
{

    // Set up the required node data
    DataLoopNode::Node.allocate(5);
    DataLoopNode::Node(1).MassFlowRate = 1.0;

    // Set up a cooling setpoint manager
    SetPointManager::DefineMixedAirSetPointManager mySPM;

    mySPM.FanInNode = 1;
    mySPM.FanOutNode = 2;
    mySPM.CoolCoilInNode = 0;
    mySPM.CoolCoilOutNode = 0;
    mySPM.RefNode = 5;
    mySPM.MinCoolCoilOutTemp = 7.2;

    // test 1: Original calculation
    DataLoopNode::Node(5).TempSetPoint = 13;
    DataLoopNode::Node(2).Temp = 24.2;
    DataLoopNode::Node(1).Temp = 24.0;
    mySPM.calculate();

    EXPECT_EQ(12.8, mySPM.SetPt);

    // test 2: Freezing calculation: blow through

    mySPM.CoolCoilInNode = 3;
    mySPM.CoolCoilOutNode = 4;
    DataLoopNode::Node(5).TempSetPoint = 7.0;
    DataLoopNode::Node(5).Temp = 7.0;
    DataLoopNode::Node(3).Temp = 24.2;
    DataLoopNode::Node(4).Temp = 7.0;
    mySPM.calculate();

    EXPECT_EQ(24.2, mySPM.SetPt);

    // test 3: Freezing calculation: draw through
    DataLoopNode::Node(5).TempSetPoint = 7.3;
    DataLoopNode::Node(3).Temp = 24.2;
    DataLoopNode::Node(5).Temp = 7.2;
    DataLoopNode::Node(4).Temp = 7.0;
    DataLoopNode::Node(2).Temp = 7.2;
    DataLoopNode::Node(1).Temp = 7.0;
    mySPM.calculate();

    EXPECT_EQ(24.4, mySPM.SetPt);

    // tear down
    DataLoopNode::Node.deallocate();
}

TEST_F(EnergyPlusFixture, MixedAirSetPointManager_SameRefAndSPNodeName)
{

    std::string const idf_objects = delimited_string({
        "SetpointManager:MixedAir,",
        "  Mixed Air Temp,          !- Name",
        "  Temperature,             !- Control Variable",
        "  AirLoopSetpointNode,     !- Reference Setpoint Node Name",
        "  AirLoopFanINLET,         !- Fan Inlet Node Name",
        "  CoolingCoilAirINLET,     !- Fan Outlet Node Name",
        "  AirLoopSetpointNode;     !- Setpoint Node or NodeList Name",
    });

    ASSERT_TRUE(process_idf(idf_objects)); // read idf objects

    // GetInput should fail since reference and set point node names are the same
    bool ErrorsFound = false;
    SetPointManager::GetSetPointManagerInputData(ErrorsFound);
    EXPECT_TRUE(ErrorsFound);

    std::string const error_string = delimited_string({
        "   ** Severe  ** GetSetPointManagerInputs: SetpointManager:MixedAir=\"MIXED AIR TEMP\", reference node.",
        "   **   ~~~   ** ..Reference Node is the same as the SetPoint Node",
        "   **   ~~~   ** Reference Node Name=\"AIRLOOPSETPOINTNODE\".",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, ColdestSetPointMgrInSingleDuct)
{
    std::string const idf_objects = delimited_string({

        "Version,8.6;",

        "  Zone,",
        "    SPACE1-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    239.247360229;           !- Volume {m3}",

        "  NodeList,",
        "    OutsideAirInletNodes,    !- Name",
        "    Outside Air Inlet Node 1;!- Node 1 Name",

        "  NodeList,",
        "    SPACE1-1 In Nodes,       !- Name",
        "    SPACE1-1 In Node;        !- Node 1 Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    SPACE1-1,                !- Zone Name",
        "    SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
        "    SPACE1-1 In Nodes,       !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    SPACE1-1 Node,           !- Zone Air Node Name",
        "    SPACE1-1 Out Node;       !- Zone Return Air Node Name",

        "  ZoneControl:Thermostat,",
        "    SPACE1-1 Control,        !- Name",
        "    SPACE1-1,                !- Zone or ZoneList Name",
        "    Zone Control Type Sched, !- Control Type Schedule Name",
        "    ThermostatSetpoint:SingleCooling,  !- Control 1 Object Type",
        "    CoolingSetPoint,         !- Control 1 Name",
        "    ThermostatSetpoint:SingleHeating,  !- Control 2 Object Type",
        "    HeatingSetpoint,         !- Control 2 Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 3 Object Type",
        "    DualSetPoint;            !- Control 3 Name",

        "  ThermostatSetpoint:SingleHeating,",
        "    HeatingSetpoint,         !- Name",
        "    Htg-SetP-Sch;            !- Setpoint Temperature Schedule Name",

        "  ThermostatSetpoint:SingleCooling,",
        "    CoolingSetpoint,         !- Name",
        "    Clg-SetP-Sch;            !- Setpoint Temperature Schedule Name",

        "  ThermostatSetpoint:DualSetpoint,",
        "    DualSetPoint,            !- Name",
        "    Htg-SetP-Sch,            !- Heating Setpoint Temperature Schedule Name",
        "    Clg-SetP-Sch;            !- Cooling Setpoint Temperature Schedule Name",

        "  ZoneHVAC:EquipmentList,",
        "    SPACE1-1 Eq,             !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE1-1 ATU,            !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

        "  ZoneHVAC:AirDistributionUnit,",
        "    SPACE1-1 ATU,            !- Name",
        "    SPACE1-1 In Node,        !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:Reheat,  !- Air Terminal Object Type",
        "    SPACE1-1 VAV Reheat,     !- Air Terminal Name",
        "    0.05,                    !- Nominal Upstream Leakage Fraction",
        "    0.07;                    !- Constant Downstream Leakage Fraction",

        "  AirTerminal:SingleDuct:VAV:Reheat,",
        "    SPACE1-1 VAV Reheat,     !- Name",
        "    CoilAvailSched,          !- Availability Schedule Name",
        "    SPACE1-1 Zone Coil Air In Node,  !- Damper Air Outlet Node Name",
        "    SPACE1-1 ATU In Node,    !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    ,                        !- Zone Minimum Air Flow Input Method",
        "    ,                        !- Constant Minimum Air Flow Fraction",
        "    ,                        !- Fixed Minimum Air Flow Rate {m3/s}",
        "    ,                        !- Minimum Air Flow Fraction Schedule Name",
        "    Coil:Heating:Electric,   !- Reheat Coil Object Type",
        "    SPACE1-1 Zone Coil,      !- Reheat Coil Name",
        "    autosize,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
        "    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
        "    SPACE1-1 In Node,        !- Air Outlet Node Name",
        "    0.001,                   !- Convergence Tolerance",
        "    ,                        !- Damper Heating Action",
        "    ,                        !- Maximum Flow per Zone Floor Area During Reheat {m3/s-m2}",
        "    ;                        !- Maximum Flow Fraction During Reheat",

        "  Coil:Heating:Electric,",
        "    SPACE1-1 Zone Coil,      !- Name",
        "    CoilAvailSched,          !- Availability Schedule Name",
        "    1.0,                     !- Efficiency",
        "    autosize,                !- Nominal Capacity {W}",
        "    SPACE1-1 Zone Coil Air In Node,  !- Air Inlet Node Name",
        "    SPACE1-1 In Node;        !- Air Outlet Node Name",

        "  AirLoopHVAC:ReturnPath,",
        "    ReturnAirPath1,          !- Name",
        "    PLENUM-1 Out Node,       !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Zone Return Air Mixer;   !- Component 1 Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Zone Return Air Mixer,   !- Name",
        "    PLENUM-1 Out Node,       !- Outlet Node Name",
        "    SPACE1-1 Out Node;       !- Inlet 1 Node Name",

        "  AirLoopHVAC:SupplyPath,",
        "    Zone Supply Air Path 1,  !- Name",
        "    Zone Eq In Node,         !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Zone Supply Air Splitter 1;  !- Component 1 Name",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Zone Supply Air Splitter 1,  !- Name",
        "    Zone Eq In Node,         !- Inlet Node Name",
        "    SPACE1-1 ATU In Node;    !- Outlet 1 Node Name",

        "  AirLoopHVAC,",
        "    VAV Sys 1,               !- Name",
        "    VAV Sys 1 Controllers,   !- Controller List Name",
        "    ,                        !- Availability Manager List Name",
        "    autosize,                !- Design Supply Air Flow Rate {m3/s}",
        "    VAV Sys 1 Branches,      !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    VAV Sys 1 Inlet Node,    !- Supply Side Inlet Node Name",
        "    PLENUM-1 Out Node,       !- Demand Side Outlet Node Name",
        "    Zone Eq In Node,         !- Demand Side Inlet Node Names",
        "    VAV Sys 1 Outlet Node;   !- Supply Side Outlet Node Names",

        "  AirLoopHVAC:ControllerList,",
        "    VAV Sys 1 Controllers,   !- Name",
        "    Controller:WaterCoil,    !- Controller 1 Object Type",
        "    Central Cooling Coil Contoller 1;  !- Controller 1 Name",

        "  BranchList,",
        "    VAV Sys 1 Branches,      !- Name",
        "    VAV Sys 1 Main Branch;   !- Branch 1 Name",

        "  Branch,",
        "    VAV Sys 1 Main Branch,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    OA Sys 1,                !- Component 1 Name",
        "    VAV Sys 1 Inlet Node,    !- Component 1 Inlet Node Name",
        "    Mixed Air Node 1,        !- Component 1 Outlet Node Name",
        "    Coil:Cooling:Water,      !- Component 2 Object Type",
        "    Main Cooling Coil 1,     !- Component 2 Name",
        "    Mixed Air Node 1,        !- Component 2 Inlet Node Name",
        "    CCoil Outlet Node,       !- Component 2 Outlet Node Name",
        "    Coil:Heating:Electric,   !- Component 3 Object Type",
        "    Main Heating Coil 1,     !- Component 3 Name",
        "    CCoil Outlet Node,       !- Component 3 Inlet Node Name",
        "    HCoil Outlet Node,       !- Component 3 Outlet Node Name",
        "    Fan:VariableVolume,      !- Component 4 Object Type",
        "    Supply Fan 1,            !- Component 4 Name",
        "    HCoil Outlet Node,       !- Component 4 Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Component 4 Outlet Node Name",

        "  AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1,                !- Name",
        "    OA Sys 1 Controllers,    !- Controller List Name",
        "    OA Sys 1 Equipment;      !- Outdoor Air Equipment List Name",

        "  AirLoopHVAC:ControllerList,",
        "    OA Sys 1 Controllers,    !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller 1;         !- Controller 1 Name",

        "  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment,      !- Name",
        "    OutdoorAir:Mixer,        !- Component 1 Object Type",
        "    OA Mixing Box 1;         !- Component 1 Name",

        "  OutdoorAir:Mixer,",
        "    OA Mixing Box 1,         !- Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    Outside Air Inlet Node 1,!- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node 1,!- Relief Air Stream Node Name",
        "    VAV Sys 1 Inlet Node;    !- Return Air Stream Node Name",

        "  Coil:Cooling:Water,",
        "    Main Cooling Coil 1,     !- Name",
        "    CoilAvailSched,          !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    CCoil Water Inlet Node,  !- Water Inlet Node Name",
        "    CCoil Water Outlet Node, !- Water Outlet Node Name",
        "    Mixed Air Node 1,        !- Air Inlet Node Name",
        "    CCoil Outlet Node,       !- Air Outlet Node Name",
        "    SimpleAnalysis,          !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "  Coil:Heating:Electric,",
        "    Main Heating Coil 1,     !- Name",
        "    CoilAvailSched,          !- Availability Schedule Name",
        "    1.0,                     !- Efficiency",
        "    autosize,                !- Nominal Capacity {W}",
        "    CCoil Outlet Node,       !- Air Inlet Node Name",
        "    HCoil Outlet Node;       !- Air Outlet Node Name",

        "  Fan:VariableVolume,",
        "    Supply Fan 1,            !- Name",
        "    CoilAvailSched,          !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    600.0,                   !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    Fraction,                !- Fan Power Minimum Flow Rate Input Method",
        "    0.25,                    !- Fan Power Minimum Flow Fraction",
        "    ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    0.35071223,              !- Fan Power Coefficient 1",
        "    0.30850535,              !- Fan Power Coefficient 2",
        "    -0.54137364,             !- Fan Power Coefficient 3",
        "    0.87198823,              !- Fan Power Coefficient 4",
        "    0.000,                   !- Fan Power Coefficient 5",
        "    HCoil Outlet Node,       !- Air Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Air Outlet Node Name",

        "  Controller:WaterCoil,",
        "    Central Cooling Coil Contoller 1,  !- Name",
        "    Temperature,             !- Control Variable",
        "    Reverse,                 !- Action",
        "    FLOW,                    !- Actuator Variable",
        "    CCoil Outlet Node,       !- Sensor Node Name",
        "    CCoil Water Inlet Node,  !- Actuator Node Name",
        "    0.002,                   !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0.0;                     !- Minimum Actuated Flow {m3/s}",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node 1,!- Relief Air Outlet Node Name",
        "    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    Outside Air Inlet Node 1,!- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    FixedDryBulb,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    19.,                     !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    4.,                      !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    ProportionalMinimum;     !- Minimum Limit Type",

        "  SetpointManager:Warmest,",
        "    Supply Air Temp Manager 1,!- Name",
        "    Temperature,             !- Control Variable",
        "    VAV Sys 1,               !- HVAC Air Loop Name",
        "    11.2,                    !- Minimum Setpoint Temperature {C}",
        "    16.,                     !- Maximum Setpoint Temperature {C}",
        "    MaximumTemperature,      !- Strategy",
        "    VAV Sys 1 Outlet Node;   !- Setpoint Node or NodeList Name",

        "  SetpointManager:MixedAir,",
        "    Mixed Air Temperature Manager 1,!- Name",
        "    Temperature,             !- Control Variable",
        "    VAV Sys 1 Outlet Node,   !- Reference Setpoint Node Name",
        "    HCoil Outlet Node,       !- Fan Inlet Node Name",
        "    VAV Sys 1 Outlet Node,   !- Fan Outlet Node Name",
        "    Cooling SetPoint Node List;  !- Setpoint Node or NodeList Name",

        "  NodeList,",
        "    Cooling SetPoint Node List,  !- Name",
        "    Mixed Air Node 1,        !- Node 1 Name",
        "    CCoil Outlet Node;       !- Node 2 Name",

        "  SetpointManager:Coldest,",
        "    Supply Air Temperature Manager 2,!- Name",
        "    Temperature,             !- Control Variable",
        "    VAV Sys 1,               !- HVAC Air Loop Name",
        "    20.0,                    !- Minimum Setpoint Temperature {C}",
        "    40.0,                    !- Maximum Setpoint Temperature {C}",
        "    MinimumTemperature,      !- Strategy",
        "    HCoil Outlet Node;       !- Setpoint Node or NodeList Name",

        "  Schedule:Compact,",
        "    CoilAvailSched,          !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00, 1.0;       !- Field 3",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60;
    ScheduleManager::ProcessScheduleInput();

    HeatBalanceManager::GetZoneData(ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                    // zones are specified in the idf snippet
    DataZoneEquipment::GetZoneEquipmentData();
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
    SingleDuct::GetSysInput();

    MixedAir::GetOutsideAirSysInputs();
    SplitterComponent::GetSplitterInput();
    BranchInputManager::GetMixerInput();
    BranchInputManager::ManageBranchInput();

    DataGlobals::SysSizingCalc = true;
    SimAirServingZones::GetAirPathData();
    SimAirServingZones::InitAirLoops(true);
    // check the number of zones served by single duct or dual duct system
    EXPECT_EQ(1, DataAirLoop::AirToZoneNodeInfo(1).NumZonesCooled); // cooled and heated zone (served by single-duct)
    EXPECT_EQ(0, DataAirLoop::AirToZoneNodeInfo(1).NumZonesHeated); // no heated only zone (served by dual-duct)

    SetPointManager::GetSetPointManagerInputs();
    SetPointManager::WarmestSetPtMgr(1).AirLoopNum = 1;
    SetPointManager::ColdestSetPtMgr(1).AirLoopNum = 1;

    SetPointManager::InitSetPointManagers();
    DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate(1);
    DataZoneEnergyDemands::ZoneSysEnergyDemand(1).TotalOutputRequired = 10000.0;

    EXPECT_EQ(DataLoopNode::NodeID(2), "SPACE1-1 IN NODE");
    EXPECT_EQ(DataLoopNode::NodeID(5), "SPACE1-1 NODE");

    DataLoopNode::Node(2).MassFlowRateMax = 1.0; // zone inlet node air flow rate, kg/s
    DataLoopNode::Node(2).HumRat = 0.075;        // zone inlet node air hum ratio, kg/kg
    DataLoopNode::Node(2).Temp = 40.0;           // zone inlet node air temperature, deg C
    DataLoopNode::Node(5).Temp = 21.0;           // zone air node temperature set to 21.0 deg C

    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();

    EXPECT_EQ(DataLoopNode::NodeID(13), "VAV SYS 1 OUTLET NODE");
    EXPECT_DOUBLE_EQ(16.0, SetPointManager::WarmestSetPtMgr(1).SetPt); // no cooling load, sets to maximum limit value

    Real64 CpAir(0.0);
    Real64 ZoneSetPointTemp(0.0);

    CpAir = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(2).HumRat, DataLoopNode::Node(2).Temp);
    ZoneSetPointTemp = DataLoopNode::Node(5).Temp +
                       DataZoneEnergyDemands::ZoneSysEnergyDemand(1).TotalOutputRequired / (CpAir * DataLoopNode::Node(2).MassFlowRateMax);
    // check the value of ZoneSetPointTemp matches to the value calculated by ColdestSetPtMgr
    EXPECT_EQ(DataLoopNode::NodeID(12), "HCOIL OUTLET NODE");
    EXPECT_DOUBLE_EQ(ZoneSetPointTemp, SetPointManager::ColdestSetPtMgr(1).SetPt); // 29.74 deg C
    EXPECT_DOUBLE_EQ(ZoneSetPointTemp, DataLoopNode::Node(12).TempSetPoint);       // 29.74 deg C
}

TEST_F(EnergyPlusFixture, SetPointManager_OutdoorAirResetMaxTempTest)
{
    bool ErrorsFound = false;

    std::string const idf_objects = delimited_string({
        "Version,8.9;",

        "  SetpointManager:OutdoorAirReset,",
        "    Hot Water Loop Setpoint Manager,  !- Name",
        "    MaximumTemperature,      !- Control Variable",
        "    80.0,                    !- Setpoint at Outdoor Low Temperature {C}",
        "    -17.778,                 !- Outdoor Low Temperature {C}",
        "    40.0,                    !- Setpoint at Outdoor High Temperature {C}",
        "    21.11,                   !- Outdoor High Temperature {C}",
        "    HW Supply Outlet Node;   !- Setpoint Node or NodeList Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(ErrorsFound); // zones are specified in the idf snippet

    SetPointManager::GetSetPointManagerInputs();
    // check Set Point Manager get inputs
    EXPECT_EQ(SetPointManager::OutAirSetPtMgr(1).CtrlVarType, "MAXIMUMTEMPERATURE");
    EXPECT_EQ(SetPointManager::OutAirSetPtMgr(1).CtrlTypeMode, SetPointManager::iCtrlVarType_MaxTemp);
    EXPECT_EQ(SetPointManager::AllSetPtMgr(1).SPMType, SetPointManager::iSPMType_OutsideAir);
    EXPECT_EQ(80.0, SetPointManager::OutAirSetPtMgr(1).OutLowSetPt1);
    EXPECT_EQ(-17.778, SetPointManager::OutAirSetPtMgr(1).OutLow1);
    EXPECT_EQ(40.0, SetPointManager::OutAirSetPtMgr(1).OutHighSetPt1);
    EXPECT_EQ(21.11, SetPointManager::OutAirSetPtMgr(1).OutHigh1);
    // set out door dry bukb temp
    DataEnvironment::OutDryBulbTemp = -20.0;
    // do init
    SetPointManager::InitSetPointManagers();
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(80.0, DataLoopNode::Node(1).TempSetPointHi);
    // change the low outdoor air setpoint reset value to 60.0C
    SetPointManager::OutAirSetPtMgr(1).OutLowSetPt1 = 60.0;
    // re simulate OA Reset Set Point Manager
    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();
    // check the new reset value is set
    EXPECT_EQ(60.0, DataLoopNode::Node(1).TempSetPointHi);

    // set out door dry bukb temp
    DataEnvironment::OutDryBulbTemp = 2.0;
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();
    // SetPt = SetTempAtOutLow - ((OutDryBulbTemp - OutLowTemp)/(OutHighTemp - OutLowTemp)) * (SetTempAtOutLow - SetTempAtOutHigh);
    Real64 SetPt = 60.0 - ((2.0 - -17.778) / (21.11 - -17.778)) * (60.0 - 40.0);
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(SetPt, DataLoopNode::Node(1).TempSetPointHi);
}

TEST_F(EnergyPlusFixture, SetPointManager_OutdoorAirResetMinTempTest)
{
    bool ErrorsFound = false;

    std::string const idf_objects = delimited_string({
        "Version,8.9;",

        "  SetpointManager:OutdoorAirReset,",
        "    Hot Water Loop Setpoint Manager,  !- Name",
        "    MinimumTemperature,      !- Control Variable",
        "    80.0,                    !- Setpoint at Outdoor Low Temperature {C}",
        "    -17.778,                 !- Outdoor Low Temperature {C}",
        "    40.0,                    !- Setpoint at Outdoor High Temperature {C}",
        "    21.11,                   !- Outdoor High Temperature {C}",
        "    HW Supply Outlet Node;   !- Setpoint Node or NodeList Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(ErrorsFound); // zones are specified in the idf snippet

    SetPointManager::GetSetPointManagerInputs();
    // check Set Point Manager get inputs
    EXPECT_EQ(SetPointManager::OutAirSetPtMgr(1).CtrlVarType, "MINIMUMTEMPERATURE");
    EXPECT_EQ(SetPointManager::OutAirSetPtMgr(1).CtrlTypeMode, SetPointManager::iCtrlVarType_MinTemp);
    EXPECT_EQ(SetPointManager::AllSetPtMgr(1).SPMType, SetPointManager::iSPMType_OutsideAir);
    EXPECT_EQ(80.0, SetPointManager::OutAirSetPtMgr(1).OutLowSetPt1);
    EXPECT_EQ(-17.778, SetPointManager::OutAirSetPtMgr(1).OutLow1);
    EXPECT_EQ(40.0, SetPointManager::OutAirSetPtMgr(1).OutHighSetPt1);
    EXPECT_EQ(21.11, SetPointManager::OutAirSetPtMgr(1).OutHigh1);
    // set out door dry bukb temp
    DataEnvironment::OutDryBulbTemp = 22.0;
    // do init
    SetPointManager::InitSetPointManagers();
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(40.0, DataLoopNode::Node(1).TempSetPointLo);
    // change the low outdoor air setpoint reset value to 60.0C
    SetPointManager::OutAirSetPtMgr(1).OutHighSetPt1 = 35.0;
    // re simulate OA Reset Set Point Manager
    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();
    // check the new reset value is set
    EXPECT_EQ(35.0, DataLoopNode::Node(1).TempSetPointLo);

    // set out door dry bulb temp
    DataEnvironment::OutDryBulbTemp = 2.0;
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers();
    SetPointManager::UpdateSetPointManagers();
    // SetPt = SetTempAtOutLow - ((OutDryBulbTemp - OutLowTemp)/(OutHighTemp - OutLowTemp)) * (SetTempAtOutLow - SetTempAtOutHigh);
    Real64 SetPt = 80.0 - ((2.0 - -17.778) / (21.11 - -17.778)) * (80.0 - 35.0);
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(SetPt, DataLoopNode::Node(1).TempSetPointLo);
}

TEST_F(EnergyPlusFixture, SingZoneRhSetPtMgrZoneInletNodeTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.4;",
        "SetpointManager:SingleZone:Reheat,",
        "    SupAirTemp MngrKitchen,    !- Name",
        "    Temperature,              !- Control Variable",
        "    12.8,                     !- Minimum Supply Air Temperature",
        "    40.0,                     !- Maximum Supply Air Temperature",
        "    Kitchen,                  !- Control Zone Name",
        "    Kitchen Air Node,         !- Zone Node Name",
        "    Kitchen Inlet Node Name,    !- Zone Inlet Node Name",
        "    Equipment Outlet Node;    !- Setpoint Node or NodeList Name",

        });

    ASSERT_TRUE(process_idf(idf_objects));
    DataGlobals::NumOfZones = 1;

    DataHeatBalance::Zone.allocate(DataGlobals::NumOfZones);
    DataHeatBalance::Zone(1).Name = "KITCHEN";

    DataLoopNode::Node.allocate(3);

    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 1;
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).IsControlled = true;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 4;
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;

    SetPointManager::GetSetPointManagerInputs();

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    DataAirLoop::AirLoopInputsFilled = true;

    ASSERT_THROW(SetPointManager::InitSetPointManagers(), std::runtime_error);

    std::string const error_string = delimited_string({
        "   ** Severe  ** SetpointManager:SingleZone:Reheat=\"SUPAIRTEMP MNGRKITCHEN\", The zone inlet node of KITCHEN INLET NODE NAME",
        "   **   ~~~   ** is not found in Zone = Uncontrolled Zone. Please check inputs.",
        "   ** Severe  ** SetpointManager:SingleZone:Reheat=\"SUPAIRTEMP MNGRKITCHEN\", The zone inlet node is not connected to an air loop.",
        "   **  Fatal  ** InitSetPointManagers: Errors found in getting SetPointManager input.",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=2",
        "   ..... Last severe error=SetpointManager:SingleZone:Reheat=\"SUPAIRTEMP MNGRKITCHEN\", The zone inlet node is not connected to an air loop.",
        });

    EXPECT_TRUE(compare_err_stream(error_string, true));

    DataZoneEquipment::ZoneEquipInputsFilled = false;
    DataAirLoop::AirLoopInputsFilled = false;

}
TEST_F(EnergyPlusFixture, SingZoneCoolHeatSetPtMgrZoneInletNodeTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.4;",
        "SetpointManager:SingleZone:Heating,",
        "  Heating Supply Air Temp Manager 1,  !- Name",
        "  Temperature,             !- Control Variable",
        "  -99.,                    !- Minimum Supply Air Temperature{ C }",
        "  45.,                     !- Maximum Supply Air Temperature{ C }",
        "  ZSF1,                    !- Control Zone Name",
        "  ZSF1 Node,               !- Zone Node Name",
        "  ZNF1 Inlet Node,         !- Zone Inlet Node Name",
        "  Air Loop 1 Outlet Node;  !- Setpoint Node or NodeList Name",

        "SetpointManager:SingleZone:Cooling,",
        "  Cooling Supply Air Temp Manager 1,  !- Name",
        "  Temperature,             !- Control Variable",
        "  14.,                     !- Minimum Supply Air Temperature{ C }",
        "  99.,                     !- Maximum Supply Air Temperature{ C }",
        "  ZSF1,                    !- Control Zone Name",
        "  ZSF1 Node,               !- Zone Node Name",
        "  ZNF1 Inlet Node,         !- Zone Inlet Node Name",
        "  Zone Equipment 1 Inlet Node;  !- Setpoint Node or NodeList Name",

        });

    ASSERT_TRUE(process_idf(idf_objects));
    DataGlobals::NumOfZones = 1;

    DataHeatBalance::Zone.allocate(DataGlobals::NumOfZones);
    DataHeatBalance::Zone(1).Name = "ZSF1";

    DataLoopNode::Node.allocate(3);

    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ZoneNode = 1;
    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).IsControlled = true;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 4;
    DataZoneEquipment::ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;

    SetPointManager::GetSetPointManagerInputs();

    DataZoneEquipment::ZoneEquipInputsFilled = true;
    DataAirLoop::AirLoopInputsFilled = true;

    ASSERT_THROW(SetPointManager::InitSetPointManagers(), std::runtime_error);

    std::string const error_string = delimited_string({
        "   ** Severe  ** SetpointManager:SingleZone:Heating=\"HEATING SUPPLY AIR TEMP MANAGER 1\", The zone inlet node of ZNF1 INLET NODE",
        "   **   ~~~   ** is not found in Zone = Uncontrolled Zone. Please check inputs.",
        "   ** Severe  ** SetpointManager:SingleZone:Cooling=\"COOLING SUPPLY AIR TEMP MANAGER 1\", The zone inlet node of ZNF1 INLET NODE",
        "   **   ~~~   ** is not found in Zone = Uncontrolled Zone. Please check inputs.",
        "   **  Fatal  ** InitSetPointManagers: Errors found in getting SetPointManager input.",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=2",
        "   ..... Last severe error=SetpointManager:SingleZone:Cooling=\"COOLING SUPPLY AIR TEMP MANAGER 1\", The zone inlet node of ZNF1 INLET NODE",
        });

    EXPECT_TRUE(compare_err_stream(error_string, true));

    DataZoneEquipment::ZoneEquipInputsFilled = false;
    DataAirLoop::AirLoopInputsFilled = false;

}
