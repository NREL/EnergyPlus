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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ChillerElectricEIR;
using namespace EnergyPlus::DataLoopNode;

TEST_F(EnergyPlusFixture, ChillerElectricEIR_TestOutletNodeConditions)
{
    state->dataChillerElectricEIR->ElectricEIRChiller.allocate(1);
    auto &thisEIR = state->dataChillerElectricEIR->ElectricEIRChiller(1);

    thisEIR.EvapInletNodeNum = 1;
    thisEIR.EvapOutletNodeNum = 2;
    thisEIR.CondInletNodeNum = 3;
    thisEIR.CondOutletNodeNum = 4;
    thisEIR.HeatRecInletNodeNum = 5;
    thisEIR.HeatRecOutletNodeNum = 6;

    state->dataLoopNodes->Node.allocate(6);
    state->dataLoopNodes->Node(thisEIR.EvapInletNodeNum).Temp = 18.0;
    state->dataLoopNodes->Node(thisEIR.CondInletNodeNum).Temp = 35.0;

    thisEIR.update(*state, -2000, true);

    EXPECT_EQ(18, thisEIR.EvapOutletTemp);
    EXPECT_EQ(35, thisEIR.CondOutletTemp);

    state->dataLoopNodes->Node.deallocate();
}

TEST_F(EnergyPlusFixture, ElectricEIRChiller_HeatRecoveryAutosizeTest)
{
    // unit test for autosizing heat recovery in Chiller:Electric:EIR
    state->dataChillerElectricEIR->ElectricEIRChiller.allocate(1);
    auto &thisEIR = state->dataChillerElectricEIR->ElectricEIRChiller(1);

    thisEIR.SizFac = 1.0;
    thisEIR.DesignHeatRecVolFlowRateWasAutoSized = true;
    thisEIR.HeatRecCapacityFraction = 0.5;
    thisEIR.HeatRecActive = true;
    thisEIR.CondenserType = DataPlant::CondenserType::WaterCooled;
    thisEIR.CWLoopNum = 1;
    thisEIR.CDLoopNum = 2;
    thisEIR.EvapVolFlowRate = 1.0;
    thisEIR.CondVolFlowRate = 1.0;
    thisEIR.RefCap = 10000;
    thisEIR.RefCOP = 3.0;

    state->dataPlnt->PlantLoop.allocate(2);
    state->dataSize->PlantSizData.allocate(2);
    // chilled water loop
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataSize->PlantSizData(1).DesVolFlowRate = 1.0;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;
    // condenser water loop
    state->dataPlnt->PlantLoop(2).PlantSizNum = 2;
    state->dataPlnt->PlantLoop(2).FluidIndex = 1;
    state->dataPlnt->PlantLoop(2).FluidName = "WATER";
    state->dataSize->PlantSizData(2).DesVolFlowRate = 1.0;
    state->dataSize->PlantSizData(2).DeltaT = 5.0;

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;

    // now call sizing routine
    thisEIR.size(*state);
    // see if heat recovery flow rate is as expected
    EXPECT_NEAR(thisEIR.DesignHeatRecVolFlowRate, 0.5, 0.00001);

    state->dataSize->PlantSizData.deallocate();
    state->dataPlnt->PlantLoop.deallocate();
}

TEST_F(EnergyPlusFixture, ChillerElectricEIR_AirCooledChiller)
{

    bool RunFlag(true);
    Real64 MyLoad(-10000.0);

    state->dataPlnt->TotNumLoops = 2;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    Psychrometrics::InitializePsychRoutines(*state);

    std::string const idf_objects = delimited_string({
        "Chiller:Electric:EIR,",
        "  AirCooledChiller,                   !- Name",
        "  autosize,                           !- Reference Capacity {W}",
        "  5.50,                               !- Reference COP {W/W}",
        "  6.67,                               !- Reference Leaving Chilled Water Temperature {C}",
        "  29.40,                              !- Reference Entering Condenser Fluid Temperature {C}",
        "  autosize,                           !- Reference Chilled Water Flow Rate {m3/s}",
        "  autosize,                           !- Reference Condenser Fluid Flow Rate {m3/s}",
        "  Air cooled CentCapFT,               !- Cooling Capacity Function of Temperature Curve Name",
        "  Air cooled CentEIRFT,               !- Electric Input to Cooling Output Ratio Function of Temperature Curve Name",
        "  Air cooled CentEIRFPLR,             !- Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
        "  0.10,                               !- Minimum Part Load Ratio",
        "  1.00,                               !- Maximum Part Load Ratio",
        "  1.00,                               !- Optimum Part Load Ratio",
        "  0.25,                               !- Minimum Unloading Ratio",
        "  CHW Inlet Node,                     !- Chilled Water Inlet Node Name",
        "  CHW Outlet Node,                    !- Chilled Water Outlet Node Name",
        "  Outdoor Air Condenser Inlet Node,   !- Condenser Inlet Node Name",
        "  Outdoor Air Condenser Outlet Node,  !- Condenser Outlet Node Name",
        "  AirCooled,                          !- Condenser Type",
        "  0.04,                               !- Condenser Fan Power Ratio {W/W}",
        "  1.00,                               !- Fraction of Compressor Electric Consumption Rejected by Condenser",
        "  5.00,                               !- Leaving Chilled Water Lower Temperature Limit {C}",
        "  NotModulated,                       !- Chiller Flow Mode",
        "  0.0,                                !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  ,                                   !- Heat Recovery Inlet Node Name",
        "  ,                                   !- Heat Recovery Outlet Node Name",
        "  1.00,                               !- Sizing Factor",
        "  0.00,                               !- Basin Heater Capacity {W/K}",
        "  2.00,                               !- Basin Heater Setpoint Temperature {C}",
        "  ,                                   !- Basin Heater Operating Schedule Name",
        "  1.00,                               !- Condenser Heat Recovery Relative Capacity Fraction",
        "  ,                                   !- Heat Recovery Inlet High Temperature Limit Schedule Name",
        "  ;                                   !- Heat Recovery Leaving Temperature Setpoint Node Name",

        "Curve:Biquadratic, Air cooled CentCapFT, 0.257896, 0.0389016, -0.00021708, 0.0468684, -0.00094284, -0.00034344, 5, 10, 24, 35, , , , , ;",
        "Curve:Biquadratic, Air cooled CentEIRFT, 0.933884, -0.058212,  0.00450036, 0.00243,    0.000486,   -0.001215,   5, 10, 24, 35, , , , , ;",
        "Curve:Quadratic, Air cooled CentEIRFPLR, 0.222903,  0.313387,  0.46371,    0, 1, , , , ;",

    });

    EXPECT_TRUE(process_idf(idf_objects, false));

    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loop(state->dataPlnt->PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(state->dataPlnt->PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    GetElectricEIRChillerInput(*state);
    auto &thisEIR = state->dataChillerElectricEIR->ElectricEIRChiller(1);

    state->dataPlnt->PlantLoop(1).Name = "ChilledWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "ChilledWater";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = thisEIR.Name;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_ElectricEIR;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = thisEIR.EvapInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = thisEIR.EvapOutletNodeNum;

    state->dataSize->PlantSizData.allocate(1);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.001;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;

    thisEIR.initialize(*state, RunFlag, MyLoad);
    thisEIR.size(*state);

    // run through init again after sizing is complete to set mass flow rate
    state->dataGlobal->BeginEnvrnFlag = true;
    thisEIR.initialize(*state, RunFlag, MyLoad);

    // check chiller water side evap flow rate is non-zero
    EXPECT_NEAR(thisEIR.EvapMassFlowRateMax, 0.999898, 0.0000001);

    // check autocalculate for air-cooled or evap-cooled chiller condenser side fluid flow rate
    Real64 CalcCondVolFlow = thisEIR.RefCap * 0.000114;
    EXPECT_EQ(CalcCondVolFlow, thisEIR.CondVolFlowRate);
    EXPECT_NEAR(thisEIR.CondVolFlowRate, 2.3925760323498, 0.0000001);
    EXPECT_NEAR(thisEIR.CondMassFlowRateMax, 2.7918772761695, 0.0000001);
}

TEST_F(EnergyPlusFixture, ChillerElectricEIR_EvaporativelyCooled_Calculate)
{

    std::string const idf_objects = delimited_string({
        "Chiller:Electric:EIR,",
        "  EvapCooledChiller,                  !- Name",
        "  autosize,                           !- Reference Capacity {W}",
        "  5.50,                               !- Reference COP {W/W}",
        "  6.67,                               !- Reference Leaving Chilled Water Temperature {C}",
        "  29.40,                              !- Reference Entering Condenser Fluid Temperature {C}",
        "  autosize,                           !- Reference Chilled Water Flow Rate {m3/s}",
        "  autosize,                           !- Reference Condenser Fluid Flow Rate {m3/s}",
        "  Evap cooled CentCapFT,              !- Cooling Capacity Function of Temperature Curve Name",
        "  Evap cooled CentEIRFT,              !- Electric Input to Cooling Output Ratio Function of Temperature Curve Name",
        "  Evap cooled CentEIRFPLR,            !- Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
        "  0.10,                               !- Minimum Part Load Ratio",
        "  1.00,                               !- Maximum Part Load Ratio",
        "  1.00,                               !- Optimum Part Load Ratio",
        "  0.25,                               !- Minimum Unloading Ratio",
        "  CHW Inlet Node,                     !- Chilled Water Inlet Node Name",
        "  CHW Outlet Node,                    !- Chilled Water Outlet Node Name",
        "  OutdoorAir Condenser Inlet Node,    !- Condenser Inlet Node Name",
        "  OutdoorAir Condenser Outlet Node,   !- Condenser Outlet Node Name",
        "  EvaporativelyCooled,                !- Condenser Type",
        "  0.04,                               !- Condenser Fan Power Ratio {W/W}",
        "  1.00,                               !- Fraction of Compressor Electric Consumption Rejected by Condenser",
        "  5.00,                               !- Leaving Chilled Water Lower Temperature Limit {C}",
        "  NotModulated,                       !- Chiller Flow Mode",
        "  0.0,                                !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  ,                                   !- Heat Recovery Inlet Node Name",
        "  ,                                   !- Heat Recovery Outlet Node Name",
        "  1.00,                               !- Sizing Factor",
        "  0.00,                               !- Basin Heater Capacity {W/K}",
        "  2.00,                               !- Basin Heater Setpoint Temperature {C}",
        "  ,                                   !- Basin Heater Operating Schedule Name",
        "  1.00,                               !- Condenser Heat Recovery Relative Capacity Fraction",
        "  ,                                   !- Heat Recovery Inlet High Temperature Limit Schedule Name",
        "  ;                                   !- Heat Recovery Leaving Temperature Setpoint Node Name",

        "Curve:Biquadratic, Evap cooled CentCapFT, 0.257896, 0.0389016, -0.00021708, 0.0468684, -0.00094284, -0.00034344, 5, 10, 24, 35, , , , , ;",
        "Curve:Biquadratic, Evap cooled CentEIRFT, 0.933884, -0.058212,  0.00450036, 0.00243,    0.000486,   -0.001215,   5, 10, 24, 35, , , , , ;",
        "Curve:Quadratic, Evap cooled CentEIRFPLR, 0.222903,  0.313387,  0.46371,    0, 1, , , , ;",

    });

    EXPECT_TRUE(process_idf(idf_objects, false));

    state->dataPlnt->TotNumLoops = 2;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    Psychrometrics::InitializePsychRoutines(*state);

    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loop(state->dataPlnt->PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(state->dataPlnt->PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    GetElectricEIRChillerInput(*state);
    auto &thisEIRChiller = state->dataChillerElectricEIR->ElectricEIRChiller(1);

    state->dataPlnt->PlantLoop(1).Name = "ChilledWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "ChilledWater";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = thisEIRChiller.Name;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_Chiller_ElectricEIR;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = thisEIRChiller.EvapInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = thisEIRChiller.EvapOutletNodeNum;

    state->dataSize->PlantSizData.allocate(1);
    state->dataSize->PlantSizData(1).DesVolFlowRate = 0.001;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;

    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlnt->PlantFirstSizesOkayToReport = true;
    state->dataPlnt->PlantFinalSizesOkayToReport = true;

    state->dataEnvrn->OutDryBulbTemp = 29.4;
    state->dataEnvrn->OutWetBulbTemp = 23.0;
    state->dataEnvrn->OutHumRat =
        Psychrometrics::PsyWFnTdbTwbPb(*state, state->dataEnvrn->OutDryBulbTemp, state->dataEnvrn->OutWetBulbTemp, state->dataEnvrn->OutBaroPress);
    state->dataLoopNodes->Node(thisEIRChiller.CondInletNodeNum).Temp = state->dataEnvrn->OutDryBulbTemp;
    state->dataLoopNodes->Node(thisEIRChiller.CondInletNodeNum).OutAirWetBulb = state->dataEnvrn->OutWetBulbTemp;
    state->dataLoopNodes->Node(thisEIRChiller.CondInletNodeNum).HumRat = state->dataEnvrn->OutHumRat;

    // set load and run flag
    bool RunFlag(true);
    Real64 MyLoad(-18000.0);
    openOutputFiles(*state);

    state->dataPlnt->PlantLoop(1).LoopDemandCalcScheme = DataPlant::iLoopDemandCalcScheme::SingleSetPoint;
    state->dataLoopNodes->Node(thisEIRChiller.EvapOutletNodeNum).TempSetPoint = 6.67;
    state->dataLoopNodes->Node(thisEIRChiller.EvapInletNodeNum).Temp = 16.0;
    // init and size
    thisEIRChiller.initialize(*state, RunFlag, MyLoad);
    thisEIRChiller.size(*state);
    // init again after sizing is complete to set mass flow rate
    state->dataGlobal->BeginEnvrnFlag = true;
    thisEIRChiller.initialize(*state, RunFlag, MyLoad);
    // check chiller water side evap flow rate is non-zero
    EXPECT_NEAR(thisEIRChiller.EvapMassFlowRateMax, 0.999898, 0.0000001);
    // check autocalculate for evaporatively-cooled chiller condenser side fluid flow rate
    Real64 resultCondVolFlowRate = thisEIRChiller.RefCap * 0.000114;
    EXPECT_EQ(resultCondVolFlowRate, thisEIRChiller.CondVolFlowRate);
    EXPECT_NEAR(thisEIRChiller.CondVolFlowRate, 2.3925760323498, 0.0000001);
    EXPECT_NEAR(thisEIRChiller.CondMassFlowRateMax, 2.7918772761695, 0.0000001);
    // run chiller
    thisEIRChiller.calculate(*state, MyLoad, RunFlag);
    // calc evap-cooler water consumption rate
    Real64 EvapCondWaterVolFlowRate = thisEIRChiller.CondMassFlowRate * (thisEIRChiller.CondOutletHumRat - state->dataEnvrn->OutHumRat) /
                                      Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
    // check evap-cooled condenser water consumption rate
    EXPECT_NEAR(2.31460814, thisEIRChiller.CondMassFlowRate, 0.0000001);
    EXPECT_NEAR(6.22019725E-06, EvapCondWaterVolFlowRate, 0.000000001);
    EXPECT_NEAR(EvapCondWaterVolFlowRate, thisEIRChiller.EvapWaterConsumpRate, 0.000000001);
}
