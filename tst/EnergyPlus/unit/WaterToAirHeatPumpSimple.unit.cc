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

// EnergyPlus::WaterToAirHeatPumpSimple Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>
#include <EnergyPlus/CurveManager.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::WaterToAirHeatPumpSimple;
using namespace EnergyPlus::CurveManager;

TEST_F(EnergyPlusFixture, WaterToAirHeatPumpSimpleTest_SizeHVACWaterToAir)
{
    // This unit test is intended to check if supply air Humidity ratio used in the cooling sizing calculation is
    // reset to the minimum of entering mixed air humidity ratio and the user specified supply air design Humidity
    // ratio such that the total cooling capacity is always greater than or equal to the sensible cooling capacity.
    // This test was added to test bug issue #4893 fix, a defect that resulted in SHR greater than 1.0.

    int HPNum(1);
    int CurveNum;

    SysSizingRunDone = true;
    ZoneSizingRunDone = true;
    CurSysNum = 0;
    CurZoneEqNum = 1;

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP.allocate(HPNum);
    FinalZoneSizing.allocate(CurZoneEqNum);
    ZoneEqSizing.allocate(CurZoneEqNum);
    DesDayWeath.allocate(1);
    DesDayWeath(1).Temp.allocate(24);

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WatertoAirHPType = "COOLING";
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedAirVolFlowRate = AutoSize;
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCapCoolTotal = AutoSize;
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCapCoolSens = AutoSize;
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedWaterVolFlowRate = 0.0;

    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.20;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.20;
    FinalZoneSizing(CurZoneEqNum).CoolDesTemp = 13.0;
    FinalZoneSizing(CurZoneEqNum).CoolDesHumRat = 0.0075;
    FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax = 15;
    FinalZoneSizing(CurZoneEqNum).CoolDDNum = 1;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp = 25.5;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat = 0.0045;
    FinalZoneSizing(CurZoneEqNum).ZoneRetTempAtCoolPeak = 25.5;
    FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtCoolPeak = 0.0045;
    ZoneEqSizing(CurZoneEqNum).OAVolFlow = 0.0;
    
    state->dataCurveManager->NumCurves = 2;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    CurveNum = 1;
    state->dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::QuadLinear;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:QuadLinear";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = -9.149069561;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 10.878140260;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = -1.718780157;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.746414818;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 80.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var3Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var3Max = 2.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var4Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var4Max = 2.0;

    CurveNum = 2;
    state->dataCurveManager->PerfCurve(CurveNum).CurveType = CurveTypeEnum::QuintLinear;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:QuintLinear";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpTypeEnum::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = -5.462690012;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 17.95968138;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = -11.87818402;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = -0.980163419;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.767285761;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var3Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var3Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var4Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var4Max = 1.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var5Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var5Max = 1.0;

    // performance curve coefficients
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).TotalCoolCapCurveIndex = 1;
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).SensCoolCapCurveIndex = 2;

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCOPCool = 5.12;

    DesDayWeath(1).Temp(15) = 32.0;
    state->dataEnvrn->StdBaroPress = 101325.0;
    ZoneEqDXCoil = true;

    WaterToAirHeatPumpSimple::SizeHVACWaterToAir(*state, HPNum);

    // check that the design oulet air humidity ratio did not change
    EXPECT_DOUBLE_EQ(0.0075, FinalZoneSizing(CurZoneEqNum).CoolDesHumRat);

    // check that the total cooling capacity is >= the sensible cooling capacity
    EXPECT_GE(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCapCoolTotal, state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCapCoolSens);

    if (state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCapCoolTotal != 0.0) {
        ShowMessage(*state,
                    format("SizeHVACWaterToAir: Rated Sensible Heat Ratio = {:.2R} [-]",
                           state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCapCoolSens /
                               state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).RatedCapCoolTotal));
    }
}

TEST_F(EnergyPlusFixture, WaterToAirHeatPumpSimple_TestWaterFlowControl)
{

    std::string const idf_objects = delimited_string({

        " Coil:Cooling:WaterToAirHeatPump:EquationFit,",
        "   Sys 5 Heat Pump Cooling Mode,  !- Name",
        "   Sys 5 Water to Air Heat Pump Source Side1 Inlet Node,  !- Water Inlet Node Name",
        "   Sys 5 Water to Air Heat Pump Source Side1 Outlet Node,  !- Water Outlet Node Name",
        "   Sys 5 Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "   Sys 5 Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "   1.0,                     !- Rated Air Flow Rate {m3/s}",
        "   0.0033,                  !- Rated Water Flow Rate {m3/s}",
        "   23125.59,                !- Gross Rated Total Cooling Capacity {W}",
        "   16267,                   !- Gross Rated Sensible Cooling Capacity {W}",
        "   7.007757577,             !- Gross Rated Cooling COP",
        "   TotCoolCapCurve,         !- Total Cooling Capacity Curve Name",
        "   SensCoolCapCurve,        !- Sensible Cooling Capacity Curve Name",
        "   CoolPowCurve,            !- Cooling Power Consumption Curve Name",
        "   0,                       !- Nominal Time for Condensate Removal to Begin {s}",
        "   0;                       !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",

        " Coil:Heating:WaterToAirHeatPump:EquationFit,",
        "  Sys 5 Heat Pump Heating Mode,  !- Name",
        "  Sys 5 Water to Air Heat Pump Source Side2 Inlet Node,  !- Water Inlet Node Name",
        "  Sys 5 Water to Air Heat Pump Source Side2 Outlet Node,  !- Water Outlet Node Name",
        "  Sys 5 Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
        "  Sys 5 SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "  1.0,                     !- Rated Air Flow Rate {m3/s}",
        "  0.0033,                  !- Rated Water Flow Rate {m3/s}",
        "  19156.73,                !- Gross Rated Heating Capacity {W}",
        "  3.167053691,             !- Gross Rated Heating COP",
        "  HeatCapCurve,             !- Heating Capacity Curve Name",
        "  HeatPowCurve;             !- Heating Power Curve Name",

        "Curve:QuintLinear,",
        "  SensCoolCapCurve,     ! Curve Name",
        "  2.24209455,           ! CoefficientC1",
        "  7.28913391,           ! CoefficientC2",
        "  -9.06079896,          ! CoefficientC3",
        "  -0.36729404,          ! CoefficientC4",
        "  0.218826161,          ! CoefficientC5",
        "  0.00901534,           ! CoefficientC6",
        "  0.,                   ! Minimum Value of v",
        "  100.,                 ! Maximum Value of v",
        "  0.,                   ! Minimum Value of w",
        "  100.,                 ! Maximum Value of w",
        "  0.,                   ! Minimum Value of x",
        "  100.,                 ! Maximum Value of x",
        "  0.,                   ! Minimum Value of y",
        "  100.,                 ! Maximum Value of y",
        "  0,                    ! Minimum Value of z",
        "  100,                  ! Maximum Value of z",
        "  0.,                   ! Minimum Curve Output",
        "  38.;                  ! Maximum Curve Output",
        
        "Curve:QuadLinear,",
        "  TotCoolCapCurve,      ! Curve Name",
        "  -0.68126221,          ! CoefficientC1",
        "  1.99529297,           ! CoefficientC2",
        "  -0.93611888,          ! CoefficientC3",
        "  0.02081177,           ! CoefficientC4",
        "  0.008438868,          ! CoefficientC5",
        "  0.,                   ! Minimum Value of w",
        "  100.,                 ! Maximum Value of w",
        "  0.,                   ! Minimum Value of x",
        "  100.,                 ! Maximum Value of x",
        "  0.,                   ! Minimum Value of y",
        "  100.,                 ! Maximum Value of y",
        "  0,                    ! Minimum Value of z",
        "  100,                  ! Maximum Value of z",
        "  0.,                   ! Minimum Curve Output",
        "  38.;                  ! Maximum Curve Output",
        
        "Curve:QuadLinear,",
        "  CoolPowCurve,      ! Curve Name",
        "  -3.20456384,          ! CoefficientC1",
        "  0.47656454,           ! CoefficientC2",
        "  3.16734236,           ! CoefficientC3",
        "  0.10244637,           ! CoefficientC4",
        "  -0.038132556,         ! CoefficientC5",
        "  0.,                   ! Minimum Value of w",
        "  100.,                 ! Maximum Value of w",
        "  0.,                   ! Minimum Value of x",
        "  100.,                 ! Maximum Value of x",
        "  0.,                   ! Minimum Value of y",
        "  100.,                 ! Maximum Value of y",
        "  0,                    ! Minimum Value of z",
        "  100,                  ! Maximum Value of z",
        "  0.,                   ! Minimum Curve Output",
        "  38.;                  ! Maximum Curve Output",
        
        "Curve:QuadLinear,",
        "  HeatCapCurve,         ! Curve Name",
        "  -5.50102734,          ! CoefficientC1",
        "  -0.96688754,          ! CoefficientC2",
        "  7.70755007,           ! CoefficientC3",
        "  0.031928881,          ! CoefficientC4",
        "  0.028112522,          ! CoefficientC5",
        "  0.,                   ! Minimum Value of w",
        "  100.,                 ! Maximum Value of w",
        "  0.,                   ! Minimum Value of x",
        "  100.,                 ! Maximum Value of x",
        "  0.,                   ! Minimum Value of y",
        "  100.,                 ! Maximum Value of y",
        "  0,                    ! Minimum Value of z",
        "  100,                  ! Maximum Value of z",
        "  0.,                   ! Minimum Curve Output",
        "  38.;                  ! Maximum Curve Output",

        "Curve:QuadLinear,",
        "  HeatPowCurve,         ! Curve Name",
        "  -7.47517858,          ! CoefficientC1",
        "  6.40876653,           ! CoefficientC2",
        "  1.99711665,           ! CoefficientC3",
        "  -0.050682973,         ! CoefficientC4",
        "  0.011385145,          ! CoefficientC5",
        "  0.,                   ! Minimum Value of w",
        "  100.,                 ! Maximum Value of w",
        "  0.,                   ! Minimum Value of x",
        "  100.,                 ! Maximum Value of x",
        "  0.,                   ! Minimum Value of y",
        "  100.,                 ! Maximum Value of y",
        "  0,                    ! Minimum Value of z",
        "  100,                  ! Maximum Value of z",
        "  0.,                   ! Minimum Curve Output",
        "  38.;                  ! Maximum Curve Output",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    
    GetCurveInput(*state);
    GetSimpleWatertoAirHPInput(*state);

    int HPNum(1);
    Real64 DesignAirflow(2.0);
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).Temp = 5.0;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).Enthalpy = 44650.0;

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate = 15.0;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMax = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMaxAvail = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate;

    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).MassFlowRate = DesignAirflow;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).Temp = 26.0;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).HumRat = 0.007;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).Enthalpy = 43970.75;

    state->dataPlnt->TotNumLoops = 2;
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

    state->dataPlnt->PlantLoop(1).Name = "ChilledWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "ChilledWater";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).Name;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WAHPPlantTypeOfNum;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum;

    int CompOp(1);
    int CyclingScheme(1);
    bool FirstHVACIteration(true);
    Real64 MaxONOFFCyclesperHour(4.0);
    Real64 HPTimeConstant(0.1);
    Real64 FanDelayTime(60.0);
    Real64 SensLoad(38000.0);
    Real64 LatentLoad(0.0);
    Real64 PartLoadRatio(1.0);
    Real64 RuntimeFrac(1.0);
    Real64 OnOffAirFlowRatio(1.0);
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).LoopNum = 1;

    state->dataEnvrn->OutBaroPress = 101325.0;

    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPCoolingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 15.0);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 5.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 5.19458, 0.00001);

    PartLoadRatio = 0.5;
    RuntimeFrac = 0.5;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPCoolingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 15.0);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 5.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 5.09729, 0.00001);

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterCyclingMode = WaterCycling;
    PartLoadRatio = 1.0;
    RuntimeFrac = 1.0;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPCoolingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 15.0);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 5.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 5.19458, 0.00001);

    PartLoadRatio = 0.5;
    RuntimeFrac = 0.5;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPCoolingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 7.5);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 5.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 5.19458, 0.00001);

    // test reduced flow at coil water inlet node
    PartLoadRatio = 0.25;
    RuntimeFrac = 0.25;
    Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate = 3.75;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPCoolingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, LatentLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 3.75);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 5.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 5.19458, 0.00001);
    UpdateSimpleWatertoAirHP(*state, HPNum);
    EXPECT_EQ(Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate, 3.75);
    EXPECT_EQ(Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterOutletNodeNum).MassFlowRate, 3.75);
    EXPECT_NEAR(Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterOutletNodeNum).Temp, 5.19458, 0.00001);

    HPNum = 2;
    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).LoopNum = 2;
    state->dataPlnt->PlantLoop(2).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(2).FluidName = "HotWater";
    state->dataPlnt->PlantLoop(2).FluidIndex = 1;
    state->dataPlnt->PlantLoop(2).FluidName = "WATER";
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).Name;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WAHPPlantTypeOfNum;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum;

    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).Temp = 35.0;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).Enthalpy = 43950.0;

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate = 15.0;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMax = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMaxAvail = state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate;

    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).MassFlowRate = DesignAirflow;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).Temp = 15.0;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).HumRat = 0.004;
    DataLoopNode::Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).AirInletNodeNum).Enthalpy = PsyHFnTdbW(15.0, 0.004);

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).DesignWaterMassFlowRate = 15.0;

    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);

    PartLoadRatio = 1.0;
    RuntimeFrac = 1.0;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPHeatingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 15.0);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 35.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 34.514131, 0.00001);

    PartLoadRatio = 0.5;
    RuntimeFrac = 0.5;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPHeatingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 15.0);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 35.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 34.757065, 0.00001);

    state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterCyclingMode = WaterCycling;
    PartLoadRatio = 1.0;
    RuntimeFrac = 1.0;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPHeatingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 15.0);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 35.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 34.514131, 0.00001);

    PartLoadRatio = 0.5;
    RuntimeFrac = 0.5;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPHeatingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 7.5);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 35.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 34.514131, 0.00001);

    // test reduced flow at coil water inlet node
    PartLoadRatio = 0.25;
    RuntimeFrac = 0.25;
    Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate = 3.75;
    InitSimpleWatertoAirHP(*state, HPNum, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, CyclingScheme, OnOffAirFlowRatio,
                           FirstHVACIteration);
    CalcHPHeatingSimple(*state, HPNum, CyclingScheme, RuntimeFrac, SensLoad, CompOp, PartLoadRatio, OnOffAirFlowRatio);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterMassFlowRate, 3.75);
    EXPECT_EQ(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).InletWaterTemp, 35.0);
    EXPECT_NEAR(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).OutletWaterTemp, 34.514131, 0.00001);
    UpdateSimpleWatertoAirHP(*state, HPNum);
    EXPECT_EQ(Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate, 3.75);
    EXPECT_EQ(Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterOutletNodeNum).MassFlowRate, 3.75);
    EXPECT_NEAR(Node(state->dataWaterToAirHeatPumpSimple->SimpleWatertoAirHP(HPNum).WaterOutletNodeNum).Temp, 34.514131, 0.00001);
}
