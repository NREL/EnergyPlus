// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ChillerReformulatedEIR.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Plant/Enums.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ChillerReformulatedEIR;
using namespace EnergyPlus::DataLoopNode;

TEST_F(EnergyPlusFixture, ChillerElectricReformulatedEIR_WaterCooledChillerVariableSpeedCondenser)
{

    bool RunFlag(true);
    state->dataPlnt->TotNumLoops = 2;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    state->dataGlobal->HourOfDay = 1;
    state->dataEnvrn->DayOfWeek = 1;
    state->dataEnvrn->Month = 1;
    state->dataEnvrn->DayOfMonth = 1;
    state->dataEnvrn->DayOfYear_Schedule = General::OrdinalDay(state->dataEnvrn->Month, state->dataEnvrn->DayOfMonth, 1);
    Psychrometrics::InitializePsychRoutines(*state);

    std::string const idf_objects = delimited_string({
        "Chiller:Electric:ReformulatedEIR,",
        "  WaterChiller,                       !- Name",
        "  autosize,                           !- Reference Capacity {W}",
        "  1,                                  !- Reference COP {W/W}",
        "  6.67,                               !- Reference Leaving Chilled Water Temperature {C}",
        "  29.40,                              !- Reference Entering Condenser Fluid Temperature {C}",
        "  autosize,                           !- Reference Chilled Water Flow Rate {m3/s}",
        "  0.001,                              !- Reference Condenser Fluid Flow Rate {m3/s}",
        "  DummyCapfT,                         !- Cooling Capacity Function of Temperature Curve Name",
        "  DummyEIRfT,                         !- Electric Input to Cooling Output Ratio Function of Temperature Curve Name",
        "  LeavingCondenserWaterTemperature,   !- Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Type",
        "  DummyEIRfPLR,                       !- Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
        "  0.10,                               !- Minimum Part Load Ratio",
        "  1.00,                               !- Maximum Part Load Ratio",
        "  1.00,                               !- Optimum Part Load Ratio",
        "  0.25,                               !- Minimum Unloading Ratio",
        "  CHW Inlet Node,                     !- Chilled Water Inlet Node Name",
        "  CHW Outlet Node,                    !- Chilled Water Outlet Node Name",
        "  Condenser Inlet Node,               !- Condenser Inlet Node Name",
        "  Condenser Outlet Node,              !- Condenser Outlet Node Name",
        "  1,                                  !- Fraction of Compressor Electric Consumption Rejected by Condenser",
        "  2,                                  !- Leaving Chilled Water Lower Temperature Limit {C}",
        "  ConstantFlow,                       !- Chiller Flow Mode",
        "  0.0,                                !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  ,                                   !- Heat Recovery Inlet Node Name",
        "  ,                                   !- Heat Recovery Outlet Node Name",
        "  1.00,                               !- Sizing Factor",
        "  1.00,                               !- Condenser Heat Recovery Relative Capacity Fraction",
        "  ,                                   !- Heat Recovery Inlet High Temperature Limit Schedule Name",
        "  ,                                   !- Heat Recovery Leaving Temperature Setpoint Node Name",
        "  ,                                   !- End-Use Subcategory",
        "  ModulatedLoopPLR,                   !- Condenser Flow Control",
        "  Y=F(X),                             !- Condenser Loop Flow Rate Fraction Function of Loop Part Load Ratio Curve Name",
        "  CondenserdT,                        !- Temperature Difference Across Condenser Schedule Name",
        "  0.35,                               !- Condenser Minimum Flow Fraction",
        "  ThermoCapFracCurve;                 !- Thermosiphon Capacity Fraction Curve Name",

        "Curve:Linear, ThermoCapFracCurve, 0.0, 0.06, 0.0, 1.0, 0.0, 1.0, Dimensionless, Dimensionless;",
        "Curve:Linear,Y=F(X),0,1,0,1;",
        "Schedule:Constant,CondenserdT,,10.0;"
        "Curve:Biquadratic, DummyCapfT, 1, 0, 0, 0, 0, 0, 5, 10, 24, 35, , , , , ;",
        "Curve:Biquadratic, DummyEIRfT, 1, 0,  0, 0, 0, 0,   5, 10, 24, 35, , , , , ;",
        "Curve:Biquadratic, DummyEIRfPLR, 1, 0,  0, 0, 0, 0,   5, 10, 0, 1, , , , , ;",
    });

    EXPECT_TRUE(process_idf(idf_objects, false));

    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loopside(state->dataPlnt->PlantLoop(l).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(l).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    GetElecReformEIRChillerInput(*state);
    auto &thisChiller = state->dataChillerReformulatedEIR->ElecReformEIRChiller(1);
    state->dataLoopNodes->Node.allocate(4);

    state->dataPlnt->PlantLoop(1).Name = "ChilledWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).TempSetPointNodeNum = 10;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Name = thisChiller.Name;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn = thisChiller.EvapInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumOut = thisChiller.EvapOutletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::LoopDemandCalcScheme::SingleSetPoint;
    state->dataPlnt->PlantLoop(1).LoopSide(EnergyPlus::DataPlant::LoopSideLocation::Demand).TempSetPoint = 4.4;
    state->dataLoopNodes->Node(thisChiller.EvapOutletNodeNum).TempSetPoint = 4.4;

    state->dataSize->PlantSizData.allocate(2);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.001;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;

    state->dataPlnt->PlantLoop(2).Name = "CondenserWaterLoop";
    state->dataPlnt->PlantLoop(2).FluidIndex = 1;
    state->dataPlnt->PlantLoop(2).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(2).FluidName = "WATER";
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Name = thisChiller.Name;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::Chiller_ElectricReformEIR;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn = thisChiller.CondInletNodeNum;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumOut = thisChiller.CondOutletNodeNum;

    state->dataSize->PlantSizData(2).DesVolFlowRate = 0.001;
    state->dataSize->PlantSizData(2).DeltaT = 5.0;

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;

    Real64 MyLoad(0.0);
    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.size(*state);
    MyLoad = -thisChiller.RefCap;
    state->dataSize->PlantSizData(1).DesCapacity = std::abs(MyLoad) * 2;
    ScheduleManager::UpdateScheduleValues(*state);

    // run through init again after sizing is complete to set mass flow rate
    state->dataGlobal->BeginEnvrnFlag = true;
    thisChiller.initialize(*state, RunFlag, MyLoad);

    // set node temperatures
    state->dataLoopNodes->Node(thisChiller.CondInletNodeNum).Temp = 25.0;
    state->dataLoopNodes->Node(thisChiller.EvapInletNodeNum).Temp = 15.0;
    state->dataPlnt->PlantLoop(thisChiller.CWPlantLoc.loopNum).LoopSide(thisChiller.CWPlantLoc.loopSideNum).UpdatedDemandToLoopSetPoint = MyLoad;

    thisChiller.control(*state, MyLoad, RunFlag, false);
    EXPECT_NEAR(thisChiller.CondMassFlowRate, thisChiller.CondMassFlowRateMax / 2, 0.00001);

    thisChiller.CondenserFlowControl = DataPlant::CondenserFlowControl::ModulatedChillerPLR;
    MyLoad /= 2;
    thisChiller.control(*state, MyLoad, RunFlag, false);
    EXPECT_NEAR(thisChiller.CondMassFlowRate, thisChiller.CondMassFlowRateMax / 2, 0.00001);

    thisChiller.CondenserFlowControl = DataPlant::CondenserFlowControl::ModulatedDeltaTemperature;
    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(*state,
                                                       state->dataPlnt->PlantLoop(thisChiller.CWPlantLoc.loopNum).FluidName,
                                                       thisChiller.CondInletTemp,
                                                       state->dataPlnt->PlantLoop(thisChiller.CWPlantLoc.loopNum).FluidIndex,
                                                       "ChillerElectricEIR_WaterCooledChillerVariableSpeedCondenser");
    thisChiller.control(*state, MyLoad, RunFlag, false);
    Real64 ActualCondFlow = 3.0 * std::abs(MyLoad) / (Cp * 10.0);
    EXPECT_NEAR(thisChiller.CondMassFlowRate, ActualCondFlow, 0.00001);

    thisChiller.CondenserFlowControl = DataPlant::CondenserFlowControl::ConstantFlow;
    thisChiller.control(*state, MyLoad, RunFlag, false);
    EXPECT_NEAR(thisChiller.CondMassFlowRate, thisChiller.CondMassFlowRateMax, 0.00001);

    // Test the minimum condenser flow rate
    MyLoad = -500;
    thisChiller.CondenserFlowControl = DataPlant::CondenserFlowControl::ModulatedChillerPLR;
    thisChiller.control(*state, MyLoad, RunFlag, false);
    EXPECT_NEAR(thisChiller.CondMassFlowRate, thisChiller.CondMassFlowRateMax * 0.35, 0.00001);

    // test thermosiphon model
    MyLoad = -15000.0;
    Real64 FalsiCondOutTemp = state->dataLoopNodes->Node(thisChiller.CondInletNodeNum).Temp;
    state->dataLoopNodes->Node(thisChiller.EvapInletNodeNum).Temp = 10.0;
    state->dataLoopNodes->Node(thisChiller.EvapOutletNodeNum).Temp = 6.0;
    state->dataLoopNodes->Node(thisChiller.EvapOutletNodeNum).TempSetPoint = 6.0;
    state->dataLoopNodes->Node(thisChiller.CondInletNodeNum).Temp = 12.0; // condenser inlet temp > evap outlet temp

    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.calculate(*state, MyLoad, RunFlag, FalsiCondOutTemp);
    EXPECT_GT(thisChiller.ChillerPartLoadRatio, 0.7); // load is large
    EXPECT_EQ(thisChiller.thermosiphonStatus, 0);     // thermosiphon is off
    EXPECT_GT(thisChiller.Power, 20000.0);            // power is non-zero

    state->dataLoopNodes->Node(thisChiller.CondInletNodeNum).Temp = 5.0; // condenser inlet temp < evap outlet temp

    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.calculate(*state, MyLoad, RunFlag, FalsiCondOutTemp);
    EXPECT_GT(thisChiller.ChillerPartLoadRatio, 0.7); // load is large
    EXPECT_EQ(thisChiller.thermosiphonStatus, 0);     // thermosiphon is off
    EXPECT_GT(thisChiller.Power, 20000.0);            // power is non-zero

    MyLoad /= 15.0; // reduce load such that thermosiphon can meet load
    thisChiller.initialize(*state, RunFlag, MyLoad);
    thisChiller.calculate(*state, MyLoad, RunFlag, FalsiCondOutTemp);
    Real64 dT = thisChiller.EvapOutletTemp - thisChiller.CondInletTemp;
    Real64 thermosiphonCapFrac = Curve::CurveValue(*state, thisChiller.thermosiphonTempCurveIndex, dT);
    EXPECT_LT(thisChiller.ChillerPartLoadRatio, 0.05);                // load is small
    EXPECT_GT(thermosiphonCapFrac, thisChiller.ChillerPartLoadRatio); // thermosiphon capacity can meet load
    EXPECT_EQ(thisChiller.thermosiphonStatus, 1);                     // thermosiphon is on
    EXPECT_EQ(thisChiller.Power, 0.0);                                // power is zero
}
