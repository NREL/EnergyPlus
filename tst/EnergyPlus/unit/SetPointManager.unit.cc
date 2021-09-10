// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReturnAirPathManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, SetPointManager_DefineReturnWaterChWSetPointManager)
{

    // Set up the required plant loop data
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(2).NodeNumIn = 1;  // Supply inlet, return
    state->dataPlnt->PlantLoop(1).LoopSide(2).NodeNumOut = 2; // Supply outlet, supply
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumIn = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumOut = 2;

    // Set up the required node data
    state->dataLoopNodes->Node.allocate(2);
    state->dataLoopNodes->Node(2).MassFlowRate = 1.0;

    // Set up a cooling setpoint manager
    SetPointManager::DefineReturnWaterChWSetPointManager mySPM;
    mySPM.returnNodeIndex = 1;
    mySPM.supplyNodeIndex = 2;
    mySPM.plantLoopIndex = 0;
    mySPM.minimumChilledWaterSetpoint = 7;
    mySPM.maximumChilledWaterSetpoint = 10;
    mySPM.returnTemperatureConstantTarget = 12;

    // test 1: normal, in range
    state->dataLoopNodes->Node(1).Temp = 11;
    state->dataLoopNodes->Node(2).Temp = 7;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // on the first pass through it should detect the plant loop it manages
    EXPECT_EQ(1, mySPM.plantLoopIndex);
    // with a delta T of 4, and a target return of 12, it should produce 8
    EXPECT_EQ(8, mySPM.currentSupplySetPt);

    // test 2: hit the maximum reset range
    state->dataLoopNodes->Node(1).Temp = 8;
    state->dataLoopNodes->Node(2).Temp = 7;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // with a delta T of 1, and a target return of 12, it should try to produce 11, but be capped at 10
    EXPECT_EQ(10, mySPM.currentSupplySetPt);

    // test 3: hit the minimum reset range
    state->dataLoopNodes->Node(1).Temp = 13;
    state->dataLoopNodes->Node(2).Temp = 7;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // with a delta T of 6, and a target return of 12, it should try to produce 6, but be capped at 7
    EXPECT_EQ(7, mySPM.currentSupplySetPt);

    // test 4: no flow
    state->dataLoopNodes->Node(1).Temp = 11;
    state->dataLoopNodes->Node(2).Temp = 7;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.0;
    // with Qdemand calculated as zero, it should leave early, but only after setting setpt to design
    EXPECT_EQ(7, mySPM.currentSupplySetPt);

    // test 5: backward delta T (something awry...)
    state->dataLoopNodes->Node(1).Temp = 5;
    state->dataLoopNodes->Node(2).Temp = 7;
    state->dataLoopNodes->Node(2).MassFlowRate = 1.0;
    // with Qdemand calculated as negative, it should leave early, but only after setting setpt to design
    EXPECT_EQ(7, mySPM.currentSupplySetPt);

    // test 6: can't identify plant loop
    // this is actually a passing case, since the plant may not be built when this is first called
    // the PlantManager will do a validation to ensure that a setpoint is found, so we don't have to validate here
    // this code simply defaults to getting a fluid index of 1 (water) if it can't find a matching plant
    state->dataLoopNodes->Node(1).Temp = 11;
    state->dataLoopNodes->Node(2).Temp = 7;
    state->dataLoopNodes->Node(2).MassFlowRate = 1.0;
    state->dataPlnt->PlantLoop(1).LoopSide(2).NodeNumOut = 5; // Supply outlet, supply
    mySPM.plantLoopIndex = 0;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // this time it shouldn't detect which plant it was found on
    EXPECT_EQ(0, mySPM.plantLoopIndex);
    // with a delta T of 4, and a target return of 12, it should produce 8
    EXPECT_EQ(8, mySPM.currentSupplySetPt);

    // tear down
    state->dataLoopNodes->Node.deallocate();
    state->dataPlnt->PlantLoop(1).LoopSide.deallocate();
    state->dataPlnt->PlantLoop.deallocate();
}

TEST_F(EnergyPlusFixture, SetPointManager_DefineReturnWaterHWSetPointManager)
{

    // Set up the required plant loop data
    state->dataPlnt->TotNumLoops = 1;
    state->dataPlnt->PlantLoop.allocate(1);
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(1).LoopSide(2).NodeNumIn = 1;  // Supply inlet, return
    state->dataPlnt->PlantLoop(1).LoopSide(2).NodeNumOut = 2; // Supply outlet, supply
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = 0;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumIn = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumOut = 2;

    // Set up the required node data
    state->dataLoopNodes->Node.allocate(2);
    state->dataLoopNodes->Node(2).MassFlowRate = 1.0;

    // Set up a cooling setpoint manager
    SetPointManager::DefineReturnWaterHWSetPointManager mySPM;
    mySPM.returnNodeIndex = 1;
    mySPM.supplyNodeIndex = 2;
    mySPM.plantLoopIndex = 0;
    mySPM.maximumHotWaterSetpoint = 60;
    mySPM.minimumHotWaterSetpoint = 57;
    mySPM.returnTemperatureConstantTarget = 55;
    state->dataLoopNodes->Node(2).Temp = 60;

    // test 1: normal, in range
    state->dataLoopNodes->Node(1).Temp = 56;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // on the first pass through it should detect the plant loop it manages
    EXPECT_EQ(1, mySPM.plantLoopIndex);
    // with a delta T of 4, and a target return of 55, it should produce 59
    EXPECT_EQ(59, mySPM.currentSupplySetPt);

    // test 2: hit the minimum reset range
    state->dataLoopNodes->Node(1).Temp = 59;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // with a delta T of 1, and a target return of 55, it should try to produce 56, but be capped at 57
    EXPECT_EQ(57, mySPM.currentSupplySetPt);

    // test 3: hit the maximum reset range
    state->dataLoopNodes->Node(1).Temp = 54;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // with a delta T of 6, and a target return of 55, it should try to produce 61, but be capped at 60
    EXPECT_EQ(60, mySPM.currentSupplySetPt);

    // test 4: no flow
    state->dataLoopNodes->Node(1).Temp = 56;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.0;
    // with Qdemand calculated as zero, it should leave early, but only after setting setpt to design
    EXPECT_EQ(60, mySPM.currentSupplySetPt);

    // test 5: backward delta T (something awry...)
    state->dataLoopNodes->Node(1).Temp = 61;
    state->dataLoopNodes->Node(2).MassFlowRate = 1.0;
    // with Qdemand calculated as negative, it should leave early, but only after setting setpt to design
    EXPECT_EQ(60, mySPM.currentSupplySetPt);

    // test 6: can't identify plant loop
    // this is actually a passing case, since the plant may not be built when this is first called
    // the PlantManager will do a validation to ensure that a setpoint is found, so we don't have to validate here
    // this code simply defaults to getting a fluid index of 1 (water) if it can't find a matching plant
    state->dataLoopNodes->Node(1).Temp = 56;
    state->dataLoopNodes->Node(2).MassFlowRate = 1.0;
    state->dataPlnt->PlantLoop(1).LoopSide(2).NodeNumOut = 5; // Supply outlet, supply
    mySPM.plantLoopIndex = 0;
    mySPM.calculate(*state, state->dataLoopNodes->Node(1), state->dataLoopNodes->Node(2));
    // this time it shouldn't detect which plant it was found on
    EXPECT_EQ(0, mySPM.plantLoopIndex);
    // with a delta T of 4, and a target return of 55, it should produce 59
    EXPECT_EQ(59, mySPM.currentSupplySetPt);

    // tear down
    state->dataLoopNodes->Node.deallocate();
    state->dataPlnt->PlantLoop(1).LoopSide.deallocate();
    state->dataPlnt->PlantLoop.deallocate();
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
                                                      "13.2711,                 !- Coefficient1 Constant",
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
    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 60 / state->dataGlobal->NumOfTimeStepInHour;
    ScheduleManager::ProcessScheduleInput(*state);
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataEnvrn->DayOfWeek = 1;
    state->dataEnvrn->DayOfYear_Schedule = 1;
    ScheduleManager::UpdateScheduleValues(*state);

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
    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(2);

    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch.allocate(1);
    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp.allocate(1);
    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).NodeNumOut = evapOutletNodeNum;
    Real64 const designCondenserEnteringTemp = 20;
    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TempDesCondIn =
        designCondenserEnteringTemp;
    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).TempDesEvapOut = 5;
    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).MaxLoad = 5000;

    state->dataPlnt->PlantLoop(condLoopIndex).LoopSide.allocate(2);
    state->dataPlnt->PlantLoop(condLoopIndex).LoopSide(demandSide).Branch.allocate(1);
    state->dataPlnt->PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp.allocate(1);
    state->dataPlnt->PlantLoop(condLoopIndex).LoopSide(demandSide).Branch(chillerBranchCW).Comp(chillerCompIndex).NodeNumIn = condInletNodeNum;
    state->dataPlnt->PlantLoop(chwLoopIndex).LoopSide(supplySide).Branch(chillerBranchChW).Comp(chillerCompIndex).MyLoad = 1000;

    state->dataLoopNodes->Node.allocate(2);
    state->dataLoopNodes->Node(evapOutletNodeNum).Temp = 7;
    state->dataLoopNodes->Node(condInletNodeNum).Temp = 10;

    SetPointManager::DefineCondEntSetPointManager thisSPM;
    thisSPM.MinTwrWbCurve = CurveManager::GetCurveIndex(*state, "MINDSNWBCURVENAME");
    thisSPM.MinOaWbCurve = CurveManager::GetCurveIndex(*state, "MINACTWBCURVENAME");
    thisSPM.OptCondEntCurve = CurveManager::GetCurveIndex(*state, "OPTCONDENTCURVENAME");
    thisSPM.CondEntTempSchedPtr = ScheduleManager::GetScheduleIndex(*state, "CONDENSER LOOP TEMP SCHEDULE");
    thisSPM.LoopIndexPlantSide = chwLoopIndex;
    thisSPM.ChillerIndexPlantSide = chillerBranchChW;
    thisSPM.BranchIndexPlantSide = chillerCompIndex;
    thisSPM.LoopIndexDemandSide = condLoopIndex;
    thisSPM.ChillerIndexDemandSide = chillerBranchCW;
    thisSPM.BranchIndexDemandSide = chillerCompIndex;
    thisSPM.TypeNum = DataPlant::TypeOf_Chiller_Electric;

    // switch: Weighted ratio > 9 && etc...
    state->dataPlnt->PlantLoop(1).CoolingDemand = 4700;

    // Now call and check
    thisSPM.calculate(*state);
    EXPECT_NEAR(designCondenserEnteringTemp + 1.0, thisSPM.SetPt, 0.001);

    // switch: Weighted ratio < 9 || etc...
    state->dataPlnt->PlantLoop(1).CoolingDemand = 4000;

    // switch: OAWB>MinWb && DesignWB>MinDesignWB && CurLift>MinLift
    state->dataEnvrn->OutWetBulbTemp = 40;
    thisSPM.TowerDsnInletAirWetBulb = 35;
    thisSPM.MinimumLiftTD = 2;

    // Now call and check
    thisSPM.calculate(*state);
    EXPECT_NEAR(32, thisSPM.SetPt, 0.001);

    // switch: ELSE
    state->dataEnvrn->OutWetBulbTemp = 30;
    thisSPM.TowerDsnInletAirWetBulb = 20;
    thisSPM.MinimumLiftTD = 5;

    // Now call and check
    thisSPM.calculate(*state);
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
    thisSPM.setupSetPointAndFlags(totEnergy,
                                  totEnergyPrevious,
                                  condenserWaterSetPoint,
                                  condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp,
                                  statusRunSubOptimalCondenserEnteringTemp,
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
    thisSPM.setupSetPointAndFlags(totEnergy,
                                  totEnergyPrevious,
                                  condenserWaterSetPoint,
                                  condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp,
                                  statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should decrement the setpoint and continue searching, storing this energy for next time
    EXPECT_NEAR(23, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_EQ(totEnergy, totEnergyPrevious);

    // third pass through have it pass the optimal point by going higher energy
    totEnergy = 900;
    thisSPM.setupSetPointAndFlags(totEnergy,
                                  totEnergyPrevious,
                                  condenserWaterSetPoint,
                                  condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp,
                                  statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should realize it passed the optimal point, back track and then set the sub-optimazation flags
    EXPECT_NEAR(23.8, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_TRUE(statusRunSubOptimalCondenserEnteringTemp);

    // fourth pass through it will be doing the sub-optimization search; perform one search; energy goes down this time
    totEnergyPrevious = 900;
    totEnergy = 890;
    thisSPM.setupSetPointAndFlags(totEnergy,
                                  totEnergyPrevious,
                                  condenserWaterSetPoint,
                                  condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp,
                                  statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should realize it has yet again overshot and start trying to work downward carefully
    EXPECT_NEAR(23.6, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_TRUE(statusRunSubOptimalCondenserEnteringTemp);

    // fifth pass through it will have hit the optimal point; it will set the setpoint back and set the final run flags
    totEnergy = 895;
    thisSPM.setupSetPointAndFlags(totEnergy,
                                  totEnergyPrevious,
                                  condenserWaterSetPoint,
                                  condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp,
                                  statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    // the optimization should increment the energy back to where it was, and set the final flags
    EXPECT_NEAR(23.8, condenserWaterSetPoint, 0.0001);
    EXPECT_TRUE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_FALSE(statusRunSubOptimalCondenserEnteringTemp);
    EXPECT_TRUE(statusRunFinalOptimalCondenserEnteringTemp);

    // and finally, the sixth pass through when it is set to run final; totEnergy doesn't matter when that flag is true, and the sp shouldn't change
    thisSPM.setupSetPointAndFlags(totEnergy,
                                  totEnergyPrevious,
                                  condenserWaterSetPoint,
                                  condenserWaterSetPointLimit,
                                  statusRunOptimalCondenserEnteringTemp,
                                  statusRunSubOptimalCondenserEnteringTemp,
                                  statusRunFinalOptimalCondenserEnteringTemp);
    EXPECT_NEAR(23.8, condenserWaterSetPoint, 0.0001);
    EXPECT_FALSE(statusRunOptimalCondenserEnteringTemp);
    EXPECT_FALSE(statusRunSubOptimalCondenserEnteringTemp);
    EXPECT_FALSE(statusRunFinalOptimalCondenserEnteringTemp);
}

TEST_F(EnergyPlusFixture, CalcScheduledTESSetPoint)
{
    int schManNum = 1;
    state->dataSetPointManager->SchTESSetPtMgr.allocate(schManNum);
    state->dataSetPointManager->SchTESSetPtMgr(schManNum).NonChargeCHWTemp = 5;
    state->dataSetPointManager->SchTESSetPtMgr(schManNum).ChargeCHWTemp = -5;

    // indexes in Schedule
    int const OnSched = 1;
    int const OffSched = 2;
    std::string const idf_contents(delimited_string({
        "Schedule:Constant,MyScheduleOn,,1;",
        "Schedule:Constant,MyScheduleOff,,0;",
    }));
    ASSERT_TRUE(process_idf(idf_contents));
    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->MinutesPerTimeStep = 60 / state->dataGlobal->NumOfTimeStepInHour;
    ScheduleManager::ProcessScheduleInput(*state);
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataEnvrn->DayOfWeek = 1;
    state->dataEnvrn->DayOfYear_Schedule = 1;
    ScheduleManager::UpdateScheduleValues(*state);

    // CtrlType Bug
    //    state->dataSetPointManager->SchTESSetPtMgr(schManNum).CompOpType = DataPlant::iCtrlType::CoolingOp;
    state->dataSetPointManager->SchTESSetPtMgr(schManNum).CompOpType = DataPlant::iCtrlType::HeatingOp;

    state->dataSetPointManager->SchTESSetPtMgr(schManNum).SchedPtr = OnSched;

    state->dataSetPointManager->SchTESSetPtMgr(schManNum).calculate(*state);
    EXPECT_EQ(state->dataSetPointManager->SchTESSetPtMgr(schManNum).NonChargeCHWTemp, state->dataSetPointManager->SchTESSetPtMgr(schManNum).SetPt);

    state->dataSetPointManager->SchTESSetPtMgr(schManNum).SchedPtr = OffSched;
    state->dataSetPointManager->SchTESSetPtMgr(schManNum).SchedPtrCharge = OffSched;

    state->dataSetPointManager->SchTESSetPtMgr(schManNum).calculate(*state);
    EXPECT_EQ(state->dataSetPointManager->SchTESSetPtMgr(schManNum).NonChargeCHWTemp, state->dataSetPointManager->SchTESSetPtMgr(schManNum).SetPt);

    state->dataSetPointManager->SchTESSetPtMgr(schManNum).SchedPtr = OffSched;
    state->dataSetPointManager->SchTESSetPtMgr(schManNum).SchedPtrCharge = OnSched;

    state->dataSetPointManager->SchTESSetPtMgr(schManNum).calculate(*state);
    EXPECT_EQ(state->dataSetPointManager->SchTESSetPtMgr(schManNum).ChargeCHWTemp, state->dataSetPointManager->SchTESSetPtMgr(schManNum).SetPt);

    // CtrlType Bug
    //    state->dataSetPointManager->SchTESSetPtMgr(schManNum).CompOpType = DataPlant::iCtrlType::DualOp;
    state->dataSetPointManager->SchTESSetPtMgr(schManNum).CompOpType = DataPlant::iCtrlType::CoolingOp;

    state->dataSetPointManager->SchTESSetPtMgr(schManNum).calculate(*state);
    EXPECT_EQ(state->dataSetPointManager->SchTESSetPtMgr(schManNum).NonChargeCHWTemp, state->dataSetPointManager->SchTESSetPtMgr(schManNum).SetPt);
}

TEST_F(EnergyPlusFixture, SZRHOAFractionImpact)
{
    std::string const idf_objects = delimited_string({
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
    state->dataGlobal->NumOfZones = 1;

    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "KITCHEN";

    state->dataAirLoop->AirLoopFlow.allocate(1);

    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataZoneEnergyDemand->DeadBandOrSetback.allocate(1);

    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(1).OASysOutletNodeNum =
        NodeInputManager::GetOnlySingleNode(*state,
                                            "FAN INLET NODE",
                                            ErrorsFound,
                                            "FAN",
                                            "SZRHtest",
                                            DataLoopNode::NodeFluidType::Air,
                                            DataLoopNode::NodeConnectionType::Internal,
                                            NodeInputManager::compFluidStream::Primary,
                                            DataLoopNode::ObjectIsNotParent,
                                            "AHU node");
    state->dataAirSystemsData->PrimaryAirSystems(1).OASysInletNodeNum =
        NodeInputManager::GetOnlySingleNode(*state,
                                            "RETURN NODE",
                                            ErrorsFound,
                                            "OA MIXER",
                                            "SZRHtest",
                                            DataLoopNode::NodeFluidType::Air,
                                            DataLoopNode::NodeConnectionType::Inlet,
                                            NodeInputManager::compFluidStream::Primary,
                                            DataLoopNode::ObjectIsNotParent,
                                            "AHU node");
    state->dataAirSystemsData->PrimaryAirSystems(1).OAMixOAInNodeNum = NodeInputManager::GetOnlySingleNode(*state,
                                                                                                           "OA INLET TO MIXER",
                                                                                                           ErrorsFound,
                                                                                                           "OA MIXER",
                                                                                                           "SZRHtest",
                                                                                                           DataLoopNode::NodeFluidType::Air,
                                                                                                           DataLoopNode::NodeConnectionType::Internal,
                                                                                                           NodeInputManager::compFluidStream::Primary,
                                                                                                           DataLoopNode::ObjectIsNotParent,
                                                                                                           "AHU node");
    state->dataAirSystemsData->PrimaryAirSystems(1).NumBranches = 1;
    state->dataAirSystemsData->PrimaryAirSystems(1).InletBranchNum.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(1).InletBranchNum(1) = 1;

    state->dataAirSystemsData->PrimaryAirSystems(1).Branch.allocate(state->dataAirSystemsData->PrimaryAirSystems(1).NumBranches);

    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).NodeNumIn =
        NodeInputManager::GetOnlySingleNode(*state,
                                            "RETURN NODE",
                                            ErrorsFound,
                                            "OAsysinlet",
                                            "SZRHtest",
                                            DataLoopNode::NodeFluidType::Air,
                                            DataLoopNode::NodeConnectionType::Inlet,
                                            NodeInputManager::compFluidStream::Primary,
                                            DataLoopNode::ObjectIsNotParent,
                                            "AHU node");
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).TotalComponents = 1;
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).TypeOf = "Fan:ConstantVolume";

    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).NodeNumIn =
        NodeInputManager::GetOnlySingleNode(*state,
                                            "FAN INLET NODE",
                                            ErrorsFound,
                                            "FAN",
                                            "SZRHtest",
                                            DataLoopNode::NodeFluidType::Air,
                                            DataLoopNode::NodeConnectionType::Internal,
                                            NodeInputManager::compFluidStream::Primary,
                                            DataLoopNode::ObjectIsNotParent,
                                            "AHU node");

    state->dataAirSystemsData->PrimaryAirSystems(1).Branch(1).Comp(1).NodeNumOut =
        NodeInputManager::GetOnlySingleNode(*state,
                                            "FAN OUTLET NODE",
                                            ErrorsFound,
                                            "FAN",
                                            "SZRHtest",
                                            DataLoopNode::NodeFluidType::Air,
                                            DataLoopNode::NodeConnectionType::Internal,
                                            NodeInputManager::compFluidStream::Primary,
                                            DataLoopNode::ObjectIsNotParent,
                                            "AHU node");

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).IsControlled = true;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    int zoneAirNode = NodeInputManager::GetOnlySingleNode(*state,
                                                          "KITCHEN AIR NODE",
                                                          ErrorsFound,
                                                          "Zone",
                                                          "SZRHspmTest",
                                                          DataLoopNode::NodeFluidType::Air,
                                                          DataLoopNode::NodeConnectionType::ZoneNode,
                                                          NodeInputManager::compFluidStream::Primary,
                                                          DataLoopNode::ObjectIsNotParent,
                                                          "Test zone node");
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = zoneAirNode;
    int zoneInletNode = NodeInputManager::GetOnlySingleNode(*state,
                                                            "KITCHEN DIRECT AIR INLET NODE NAME",
                                                            ErrorsFound,
                                                            "Zone",
                                                            "SZRHspmTest",
                                                            DataLoopNode::NodeFluidType::Air,
                                                            DataLoopNode::NodeConnectionType::ZoneInlet,
                                                            NodeInputManager::compFluidStream::Primary,
                                                            DataLoopNode::ObjectIsNotParent,
                                                            "Test zone inlet node");
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = zoneInletNode;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;

    SetPointManager::GetSetPointManagerInputs(*state);
    EXPECT_EQ(state->dataSetPointManager->SingZoneRhSetPtMgr(1).ControlZoneNum, 1);
    state->dataSetPointManager->SingZoneRhSetPtMgr(1).AirLoopNum = 1;

    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataAirLoop->AirLoopInputsFilled = true;

    SetPointManager::InitSetPointManagers(*state);

    state->dataAirLoop->AirLoopFlow(1).OAFrac = 1.0;
    state->dataAirLoop->AirLoopFlow(1).OAMinFrac = 0.8;

    state->dataLoopNodes->Node(zoneInletNode).MassFlowRate = 1.0; // set zone inlet mass flow
    state->dataLoopNodes->Node(zoneInletNode).HumRat = 0.0008;
    state->dataLoopNodes->Node(zoneInletNode).Temp = 20.0;

    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).TotalOutputRequired = 0.0;
    // pick these next values so ExtrRateNoHC doesn't exceed loat to sp
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = 6000.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = -4000.0;
    state->dataZoneEnergyDemand->DeadBandOrSetback(1) = true;

    state->dataLoopNodes->Node(zoneAirNode).Temp = 22.0; // zone air node
    state->dataLoopNodes->Node(zoneAirNode).HumRat = 0.0008;

    state->dataLoopNodes->Node(2).HumRat = 0.0008; // return node
    state->dataLoopNodes->Node(2).Temp = 22.0;
    state->dataLoopNodes->Node(2).Enthalpy = Psychrometrics::PsyHFnTdbW(state->dataLoopNodes->Node(2).Temp, state->dataLoopNodes->Node(2).HumRat);

    state->dataLoopNodes->Node(1).Temp = 16.0;
    state->dataLoopNodes->Node(4).Temp = 17.0; // fan rise

    // slightly cool OA
    state->dataLoopNodes->Node(3).HumRat = 0.0006; // OA intake
    state->dataLoopNodes->Node(3).Temp = 16.0;
    state->dataLoopNodes->Node(3).Enthalpy = Psychrometrics::PsyHFnTdbW(state->dataLoopNodes->Node(3).Temp, state->dataLoopNodes->Node(3).HumRat);

    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);

    // node number table
    //  1   Fan Inlet Node   OA system outlet
    //  2   Return Node      from zone, first on branch
    //  3   OA inlet to Mixer,  Outdoor air supplying OA damper
    //  4   Fan Outlet Node
    //  5   Kitchen Air Node
    //  6   Kitchen Direct Air Inlet Node Name
    //  7   PSZ-AC_2:2 Supply Equipment Outlet Node

    EXPECT_NEAR(state->dataLoopNodes->Node(7).TempSetPoint, 18.0251495, 0.001);

    state->dataAirLoop->AirLoopFlow(1).OAFrac = 0.8;
    state->dataAirLoop->AirLoopFlow(1).OAMinFrac = 0.8;

    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);

    EXPECT_NEAR(state->dataLoopNodes->Node(7).TempSetPoint, 18.20035, 0.001);

    // warmer day outside
    state->dataAirLoop->AirLoopFlow(1).OAFrac = 1.0;
    state->dataAirLoop->AirLoopFlow(1).OAMinFrac = 0.8;

    state->dataLoopNodes->Node(3).HumRat = 0.0006; // OA intake
    state->dataLoopNodes->Node(3).Temp = 26.0;
    state->dataLoopNodes->Node(3).Enthalpy = Psychrometrics::PsyHFnTdbW(state->dataLoopNodes->Node(3).Temp, state->dataLoopNodes->Node(3).HumRat);

    state->dataLoopNodes->Node(1).Temp = 26.0;
    state->dataLoopNodes->Node(4).Temp = 27.0; // fan rise

    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);

    EXPECT_NEAR(state->dataLoopNodes->Node(7).TempSetPoint, 27.0, 0.001);

    state->dataAirLoop->AirLoopFlow(1).OAFrac = 0.8;
    state->dataAirLoop->AirLoopFlow(1).OAMinFrac = 0.8;

    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);

    EXPECT_NEAR(state->dataLoopNodes->Node(7).TempSetPoint, 26.19976, 0.001);
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

TEST_F(EnergyPlusFixture, DefineMixedAirSetPointManager)
{

    // Set up the required node data
    state->dataLoopNodes->Node.allocate(5);
    state->dataLoopNodes->Node(1).MassFlowRate = 1.0;

    // Set up a cooling setpoint manager
    SetPointManager::DefineMixedAirSetPointManager mySPM;

    mySPM.FanInNode = 1;
    mySPM.FanOutNode = 2;
    mySPM.CoolCoilInNode = 0;
    mySPM.CoolCoilOutNode = 0;
    mySPM.RefNode = 5;
    mySPM.MinCoolCoilOutTemp = 7.2;

    // test 1: Original calculation
    state->dataLoopNodes->Node(5).TempSetPoint = 13;
    state->dataLoopNodes->Node(2).Temp = 24.2;
    state->dataLoopNodes->Node(1).Temp = 24.0;
    mySPM.calculate(*state);

    EXPECT_EQ(12.8, mySPM.SetPt);

    // test 2: Freezing calculation: blow through

    mySPM.CoolCoilInNode = 3;
    mySPM.CoolCoilOutNode = 4;
    state->dataLoopNodes->Node(5).TempSetPoint = 7.0;
    state->dataLoopNodes->Node(5).Temp = 7.0;
    state->dataLoopNodes->Node(3).Temp = 24.2;
    state->dataLoopNodes->Node(4).Temp = 7.0;
    mySPM.calculate(*state);

    EXPECT_EQ(24.2, mySPM.SetPt);

    // test 3: Freezing calculation: draw through
    state->dataLoopNodes->Node(5).TempSetPoint = 7.3;
    state->dataLoopNodes->Node(3).Temp = 24.2;
    state->dataLoopNodes->Node(5).Temp = 7.2;
    state->dataLoopNodes->Node(4).Temp = 7.0;
    state->dataLoopNodes->Node(2).Temp = 7.2;
    state->dataLoopNodes->Node(1).Temp = 7.0;
    mySPM.calculate(*state);

    EXPECT_EQ(24.4, mySPM.SetPt);

    // tear down
    state->dataLoopNodes->Node.deallocate();
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
    SetPointManager::GetSetPointManagerInputData(*state, ErrorsFound);
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

    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    ScheduleManager::ProcessScheduleInput(*state);

    HeatBalanceManager::GetZoneData(*state, ErrorsFound); // read zone data
    EXPECT_FALSE(ErrorsFound);                            // zones are specified in the idf snippet
    DataZoneEquipment::GetZoneEquipmentData(*state);
    ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(*state);
    SingleDuct::GetSysInput(*state);

    MixedAir::GetOutsideAirSysInputs(*state);
    SplitterComponent::GetSplitterInput(*state);
    BranchInputManager::GetMixerInput(*state);
    BranchInputManager::ManageBranchInput(*state);

    state->dataGlobal->SysSizingCalc = true;
    SimAirServingZones::GetAirPathData(*state);
    SimAirServingZones::InitAirLoops(*state, true);
    // check the number of zones served by single duct or dual duct system
    EXPECT_EQ(1, state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesCooled); // cooled and heated zone (served by single-duct)
    EXPECT_EQ(0, state->dataAirLoop->AirToZoneNodeInfo(1).NumZonesHeated); // no heated only zone (served by dual-duct)

    SetPointManager::GetSetPointManagerInputs(*state);
    state->dataSetPointManager->WarmestSetPtMgr(1).AirLoopNum = 1;
    state->dataSetPointManager->ColdestSetPtMgr(1).AirLoopNum = 1;

    SetPointManager::InitSetPointManagers(*state);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).TotalOutputRequired = 10000.0;

    EXPECT_EQ(state->dataLoopNodes->NodeID(2), "SPACE1-1 IN NODE");
    EXPECT_EQ(state->dataLoopNodes->NodeID(5), "SPACE1-1 NODE");

    state->dataLoopNodes->Node(2).MassFlowRateMax = 1.0; // zone inlet node air flow rate, kg/s
    state->dataLoopNodes->Node(2).HumRat = 0.075;        // zone inlet node air hum ratio, kg/kg
    state->dataLoopNodes->Node(2).Temp = 40.0;           // zone inlet node air temperature, deg C
    state->dataLoopNodes->Node(5).Temp = 21.0;           // zone air node temperature set to 21.0 deg C

    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);

    EXPECT_EQ(state->dataLoopNodes->NodeID(13), "VAV SYS 1 OUTLET NODE");
    EXPECT_DOUBLE_EQ(16.0, state->dataSetPointManager->WarmestSetPtMgr(1).SetPt); // no cooling load, sets to maximum limit value

    Real64 CpAir(0.0);
    Real64 ZoneSetPointTemp(0.0);

    CpAir = Psychrometrics::PsyCpAirFnW(state->dataLoopNodes->Node(2).HumRat);
    ZoneSetPointTemp = state->dataLoopNodes->Node(5).Temp + state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).TotalOutputRequired /
                                                                (CpAir * state->dataLoopNodes->Node(2).MassFlowRateMax);
    // check the value of ZoneSetPointTemp matches to the value calculated by ColdestSetPtMgr
    EXPECT_EQ(state->dataLoopNodes->NodeID(12), "HCOIL OUTLET NODE");
    EXPECT_DOUBLE_EQ(ZoneSetPointTemp, state->dataSetPointManager->ColdestSetPtMgr(1).SetPt); // 29.74 deg C
    EXPECT_DOUBLE_EQ(ZoneSetPointTemp, state->dataLoopNodes->Node(12).TempSetPoint);          // 29.74 deg C
}

TEST_F(EnergyPlusFixture, SetPointManager_OutdoorAirResetMaxTempTest)
{
    bool ErrorsFound = false;

    std::string const idf_objects = delimited_string({
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

    SetPointManager::GetSetPointManagerInputs(*state);
    // check Set Point Manager get inputs
    EXPECT_EQ(state->dataSetPointManager->OutAirSetPtMgr(1).CtrlVarType, "MAXIMUMTEMPERATURE");
    EXPECT_TRUE(compare_enums(state->dataSetPointManager->OutAirSetPtMgr(1).CtrlTypeMode, SetPointManager::iCtrlVarType::MaxTemp));
    EXPECT_TRUE(compare_enums(state->dataSetPointManager->AllSetPtMgr(1).SPMType, SetPointManager::SetPointManagerType::OutsideAir));
    EXPECT_EQ(80.0, state->dataSetPointManager->OutAirSetPtMgr(1).OutLowSetPt1);
    EXPECT_EQ(-17.778, state->dataSetPointManager->OutAirSetPtMgr(1).OutLow1);
    EXPECT_EQ(40.0, state->dataSetPointManager->OutAirSetPtMgr(1).OutHighSetPt1);
    EXPECT_EQ(21.11, state->dataSetPointManager->OutAirSetPtMgr(1).OutHigh1);
    // set out door dry bukb temp
    state->dataEnvrn->OutDryBulbTemp = -20.0;
    // do init
    SetPointManager::InitSetPointManagers(*state);
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(80.0, state->dataLoopNodes->Node(1).TempSetPointHi);
    // change the low outdoor air setpoint reset value to 60.0C
    state->dataSetPointManager->OutAirSetPtMgr(1).OutLowSetPt1 = 60.0;
    // re simulate OA Reset Set Point Manager
    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);
    // check the new reset value is set
    EXPECT_EQ(60.0, state->dataLoopNodes->Node(1).TempSetPointHi);

    // set out door dry bukb temp
    state->dataEnvrn->OutDryBulbTemp = 2.0;
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);
    // SetPt = SetTempAtOutLow - ((OutDryBulbTemp - OutLowTemp)/(OutHighTemp - OutLowTemp)) * (SetTempAtOutLow - SetTempAtOutHigh);
    Real64 SetPt = 60.0 - ((2.0 - -17.778) / (21.11 - -17.778)) * (60.0 - 40.0);
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(SetPt, state->dataLoopNodes->Node(1).TempSetPointHi);
}

TEST_F(EnergyPlusFixture, SetPointManager_OutdoorAirResetMinTempTest)
{
    bool ErrorsFound = false;

    std::string const idf_objects = delimited_string({
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

    SetPointManager::GetSetPointManagerInputs(*state);
    // check Set Point Manager get inputs
    EXPECT_EQ(state->dataSetPointManager->OutAirSetPtMgr(1).CtrlVarType, "MINIMUMTEMPERATURE");
    EXPECT_TRUE(compare_enums(state->dataSetPointManager->OutAirSetPtMgr(1).CtrlTypeMode, SetPointManager::iCtrlVarType::MinTemp));
    EXPECT_TRUE(compare_enums(state->dataSetPointManager->AllSetPtMgr(1).SPMType, SetPointManager::SetPointManagerType::OutsideAir));
    EXPECT_EQ(80.0, state->dataSetPointManager->OutAirSetPtMgr(1).OutLowSetPt1);
    EXPECT_EQ(-17.778, state->dataSetPointManager->OutAirSetPtMgr(1).OutLow1);
    EXPECT_EQ(40.0, state->dataSetPointManager->OutAirSetPtMgr(1).OutHighSetPt1);
    EXPECT_EQ(21.11, state->dataSetPointManager->OutAirSetPtMgr(1).OutHigh1);
    // set out door dry bukb temp
    state->dataEnvrn->OutDryBulbTemp = 22.0;
    // do init
    SetPointManager::InitSetPointManagers(*state);
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(40.0, state->dataLoopNodes->Node(1).TempSetPointLo);
    // change the low outdoor air setpoint reset value to 60.0C
    state->dataSetPointManager->OutAirSetPtMgr(1).OutHighSetPt1 = 35.0;
    // re simulate OA Reset Set Point Manager
    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);
    // check the new reset value is set
    EXPECT_EQ(35.0, state->dataLoopNodes->Node(1).TempSetPointLo);

    // set out door dry bulb temp
    state->dataEnvrn->OutDryBulbTemp = 2.0;
    // check OA Reset Set Point Manager run
    SetPointManager::SimSetPointManagers(*state);
    SetPointManager::UpdateSetPointManagers(*state);
    // SetPt = SetTempAtOutLow - ((OutDryBulbTemp - OutLowTemp)/(OutHighTemp - OutLowTemp)) * (SetTempAtOutLow - SetTempAtOutHigh);
    Real64 SetPt = 80.0 - ((2.0 - -17.778) / (21.11 - -17.778)) * (80.0 - 35.0);
    // check OA Reset Set Point Manager sim
    EXPECT_EQ(SetPt, state->dataLoopNodes->Node(1).TempSetPointLo);
}

TEST_F(EnergyPlusFixture, SingZoneRhSetPtMgrZoneInletNodeTest)
{
    std::string const idf_objects = delimited_string({
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
    state->dataGlobal->NumOfZones = 1;

    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "KITCHEN";

    state->dataLoopNodes->Node.allocate(3);

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).IsControlled = true;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;

    SetPointManager::GetSetPointManagerInputs(*state);

    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataAirLoop->AirLoopInputsFilled = true;

    ASSERT_THROW(SetPointManager::InitSetPointManagers(*state), std::runtime_error);

    std::string const error_string = delimited_string({
        "   ** Severe  ** SetpointManager:SingleZone:Reheat=\"SUPAIRTEMP MNGRKITCHEN\", The zone inlet node of KITCHEN INLET NODE NAME",
        "   **   ~~~   ** is not found in Zone = Uncontrolled Zone. Please check inputs.",
        "   ** Severe  ** SetpointManager:SingleZone:Reheat=\"SUPAIRTEMP MNGRKITCHEN\", The zone inlet node is not connected to an air loop.",
        "   **  Fatal  ** InitSetPointManagers: Errors found in getting SetPointManager input.",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=2",
        "   ..... Last severe error=SetpointManager:SingleZone:Reheat=\"SUPAIRTEMP MNGRKITCHEN\", The zone inlet node is not connected to an air "
        "loop.",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));

    state->dataZoneEquip->ZoneEquipInputsFilled = false;
    state->dataAirLoop->AirLoopInputsFilled = false;
}
TEST_F(EnergyPlusFixture, SingZoneCoolHeatSetPtMgrZoneInletNodeTest)
{
    std::string const idf_objects = delimited_string({
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
    state->dataGlobal->NumOfZones = 1;

    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "ZSF1";

    state->dataLoopNodes->Node.allocate(3);

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).IsControlled = true;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;

    SetPointManager::GetSetPointManagerInputs(*state);

    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataAirLoop->AirLoopInputsFilled = true;

    ASSERT_THROW(SetPointManager::InitSetPointManagers(*state), std::runtime_error);

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

    state->dataZoneEquip->ZoneEquipInputsFilled = false;
    state->dataAirLoop->AirLoopInputsFilled = false;
}
TEST_F(EnergyPlusFixture, SingZoneCoolHeatSetPtMgrSetPtTest)
{
    std::string const idf_objects = delimited_string({
        "SetpointManager:SingleZone:Heating,",
        "  Heating Supply Air Temp Manager 1,  !- Name",
        "  Temperature,             !- Control Variable",
        "  -99.,                    !- Minimum Supply Air Temperature{ C }",
        "  45.,                     !- Maximum Supply Air Temperature{ C }",
        "  ZSF1,                    !- Control Zone Name",
        "  ZSF1 Node,               !- Zone Node Name",
        "  ZSF1 Inlet Node,         !- Zone Inlet Node Name",
        "  Air Loop 1 Outlet Node;  !- Setpoint Node or NodeList Name",

        "SetpointManager:SingleZone:Cooling,",
        "  Cooling Supply Air Temp Manager 1,  !- Name",
        "  Temperature,             !- Control Variable",
        "  14.,                     !- Minimum Supply Air Temperature{ C }",
        "  99.,                     !- Maximum Supply Air Temperature{ C }",
        "  ZSF1,                    !- Control Zone Name",
        "  ZSF1 Node,               !- Zone Node Name",
        "  ZSF1 Inlet Node,         !- Zone Inlet Node Name",
        "  Zone Equipment 1 Inlet Node;  !- Setpoint Node or NodeList Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    state->dataGlobal->NumOfZones = 1;

    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).Name = "ZSF1";
    SetPointManager::GetSetPointManagerInputs(*state);

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).IsControlled = true;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    int zoneNodeNum = UtilityRoutines::FindItemInList("ZSF1 NODE", state->dataLoopNodes->NodeID);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = zoneNodeNum;
    int inletNodeNum = UtilityRoutines::FindItemInList("ZSF1 INLET NODE", state->dataLoopNodes->NodeID);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = inletNodeNum;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;
    int coolSPNodeNum = UtilityRoutines::FindItemInList("ZONE EQUIPMENT 1 INLET NODE", state->dataLoopNodes->NodeID);
    int heatSPNodeNum = UtilityRoutines::FindItemInList("AIR LOOP 1 OUTLET NODE", state->dataLoopNodes->NodeID);

    auto &zoneNode(state->dataLoopNodes->Node(zoneNodeNum));
    auto &inletNode(state->dataLoopNodes->Node(inletNodeNum));
    auto &coolSPNode(state->dataLoopNodes->Node(coolSPNodeNum));
    auto &heatSPNode(state->dataLoopNodes->Node(heatSPNodeNum));

    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = 0.0;
    state->dataZoneEquip->ZoneEquipInputsFilled = true;
    state->dataAirLoop->AirLoopInputsFilled = true;

    SetPointManager::InitSetPointManagers(*state);
    EXPECT_FALSE(has_err_output(true));

    // Case 1 - No load
    inletNode.MassFlowRate = 0.1;
    zoneNode.Temp = 20.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = 0.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = 0.0;
    SetPointManager::ManageSetPoints(*state);
    EXPECT_NEAR(coolSPNode.TempSetPoint, zoneNode.Temp, 0.001);
    EXPECT_NEAR(heatSPNode.TempSetPoint, zoneNode.Temp, 0.001);

    // Case 2 - Small heating load
    inletNode.MassFlowRate = 0.1;
    zoneNode.Temp = 20.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = 100.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = 50.0;
    SetPointManager::ManageSetPoints(*state);
    EXPECT_NEAR(coolSPNode.TempSetPoint, 20.994, 0.01);
    EXPECT_NEAR(heatSPNode.TempSetPoint, 20.497, 0.01);

    // Case 3 - Large heating load
    inletNode.MassFlowRate = 0.1;
    zoneNode.Temp = 20.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = 10000.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = 5000.0;
    SetPointManager::ManageSetPoints(*state);
    EXPECT_NEAR(coolSPNode.TempSetPoint, 99.0, 0.01);
    EXPECT_NEAR(heatSPNode.TempSetPoint, 45.0, 0.01);

    // Case 4 - Small cooling load
    inletNode.MassFlowRate = 0.1;
    zoneNode.Temp = 20.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = -50.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = -100.0;
    SetPointManager::ManageSetPoints(*state);
    EXPECT_NEAR(coolSPNode.TempSetPoint, 19.50, 0.01);
    EXPECT_NEAR(heatSPNode.TempSetPoint, 19.01, 0.01);

    // Case 5 - Large cooling load
    inletNode.MassFlowRate = 0.1;
    zoneNode.Temp = 20.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToCoolingSP = -5000.0;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).OutputRequiredToHeatingSP = -20000.0;
    SetPointManager::ManageSetPoints(*state);
    EXPECT_NEAR(coolSPNode.TempSetPoint, 14.0, 0.01);
    EXPECT_NEAR(heatSPNode.TempSetPoint, -99.0, 0.01);
}
