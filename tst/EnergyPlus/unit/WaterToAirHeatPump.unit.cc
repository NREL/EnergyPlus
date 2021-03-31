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
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/WaterToAirHeatPump.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::WaterToAirHeatPump;

TEST_F(EnergyPlusFixture, WaterToAirHeatPumpTest_SimWaterToAir)
{

    std::string const idf_objects =
        delimited_string({" Coil:Cooling:WaterToAirHeatPump:ParameterEstimation, ",
                          "   Sys 1 Heat Pump Cooling Mode, !- Name",
                          "   Scroll,      !- Compressor Type",
                          "   R22,         !- Refrigerant Type",
                          "   0.0015,      !- Design Source Side Flow Rate{ m3 / s }",
                          "   38000,       !- Nominal Cooling Coil Capacity{ W }",
                          "   0,           !- Nominal Time for Condensate Removal to Begin{ s }",
                          "   0,           !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
                          "   3000000,     !- High Pressure Cutoff{ Pa }",
                          "   0,           !- Low Pressure Cutoff{ Pa }",
                          "   Sys 1 Water to Air Heat Pump Source Side1 Inlet Node, !- Water Inlet Node Name",
                          "   Sys 1 Water to Air Heat Pump Source Side1 Outlet Node, !- Water Outlet Node Name",
                          "   Sys 1 Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
                          "   Sys 1 Heating Coil Air Inlet Node, !- Air Outlet Node Name",
                          "   3.78019E+03, !- Load Side Total Heat Transfer Coefficient{ W / K }",
                          "   2.80303E+03, !- Load Side Outside Surface Heat Transfer Coefficient{ W / K }",
                          "   7.93591E-01, !- Superheat Temperature at the Evaporator Outlet{ C }",
                          "   1.91029E+03, !- Compressor Power Losses{ W }",
                          "   2.66127E+00, !- Compressor Efficiency",
                          "   ,            !- Compressor Piston Displacement{ m3 / s }",
                          "   ,            !- Compressor Suction / Discharge Pressure Drop{ Pa }",
                          "   ,            !- Compressor Clearance Factor{ dimensionless }",
                          "   1.06009E-01, !- Refrigerant Volume Flow Rate{ m3 / s }",
                          "   1.65103E+00, !- Volume Ratio{ dimensionless }",
                          "   9.73887E-03, !- Leak Rate Coefficient",
                          "   1.04563E+03, !- Source Side Heat Transfer Coefficient{ W / K }",
                          "   0.8,         !- Source Side Heat Transfer Resistance1{ dimensionless }",
                          "   20.0;       !- Source Side Heat Transfer Resistance2{ W / K }",

                          " Coil:Heating:WaterToAirHeatPump:ParameterEstimation,",
                          "   Sys 1 Heat Pump HEATING Mode, !- Name",
                          "   Scroll,      !- Compressor Type",
                          "   R22,         !- Refrigerant Type",
                          "   0.0015,      !- Design Source Side Flow Rate{ m3 / s }",
                          "   38000,       !- Gross Rated Heating Capacity{ W }",
                          "   3000000,     !- High Pressure Cutoff",
                          "   0,           !- Low Pressure Cutoff{ Pa }",
                          "   Sys 1 Water to Air Heat Pump Source Side2 Inlet Node, !- Water Inlet Node Name",
                          "   Sys 1 Water to Air Heat Pump Source Side2 Outlet Node, !- Water Outlet Node Name",
                          "   Sys 1 Heating Coil Air Inlet Node, !- Air Inlet Node Name",
                          "   Sys 1 SuppHeating Coil Air Inlet Node, !- Air Outlet Node Name",
                          "   3.91379E+03, !- Load Side Total Heat Transfer Coefficient{ W / K }",
                          "   5.94753E-01, !- Superheat Temperature at the Evaporator Outlet{ C }",
                          "   2.49945E+03, !- Compressor Power Losses{ W }",
                          "   8.68734E-01, !- Compressor Efficiency",
                          "   ,            !- Compressor Piston Displacement{ m3 / s }",
                          "   ,            !- Compressor Suction / Discharge Pressure Drop{ Pa }",
                          "   ,            !- Compressor Clearance Factor{ dimensionless }",
                          "   7.23595E-02, !- Refrigerant Volume Flow Rate{ m3 / s }",
                          "   3.69126E+00, !- Volume Ratio{ dimensionless }",
                          "   1.75701E-05, !- Leak Rate Coefficient{ dimensionless }",
                          "   3.65348E+03, !- Source Side Heat Transfer Coefficient{ W / K }",
                          "   0.8,         !- Source Side Heat Transfer Resistance1{ dimensionless }",
                          "   20.0;        !- Source Side Heat Transfer Resistance2{ W / K }"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataFluidProps->RefrigData.allocate(1);
    state->dataFluidProps->RefrigData(1).Name = "R22";
    state->dataFluidProps->RefrigData(1).PsLowTempIndex = 1;
    state->dataFluidProps->RefrigData(1).PsHighTempIndex = 2;
    state->dataFluidProps->RefrigData(1).PsTemps.allocate(2);
    state->dataFluidProps->RefrigData(1).PsTemps(1) = -157.42;
    state->dataFluidProps->RefrigData(1).PsTemps(2) = 96.145;
    state->dataFluidProps->RefrigData(1).PsValues.allocate(2);
    state->dataFluidProps->RefrigData(1).PsValues(1) = 0.3795;
    state->dataFluidProps->RefrigData(1).PsValues(2) = 4990000.0;

    state->dataFluidProps->RefrigData(1).HfLowTempIndex = 1;
    state->dataFluidProps->RefrigData(1).HfHighTempIndex = 2;
    state->dataFluidProps->RefrigData(1).PsLowPresIndex = 1;
    state->dataFluidProps->RefrigData(1).PsHighPresIndex = 2;
    state->dataFluidProps->RefrigData(1).HTemps.allocate(2);
    state->dataFluidProps->RefrigData(1).HfValues.allocate(2);
    state->dataFluidProps->RefrigData(1).HfgValues.allocate(2);

    state->dataFluidProps->RefrigData(1).HTemps(1) = -157.42;
    state->dataFluidProps->RefrigData(1).HTemps(2) = 96.145;
    state->dataFluidProps->RefrigData(1).HfValues(1) = 29600.0;
    state->dataFluidProps->RefrigData(1).HfValues(2) = 366900.0;
    state->dataFluidProps->RefrigData(1).HfgValues(1) = 332700.0;
    state->dataFluidProps->RefrigData(1).HfgValues(2) = 366900.0;
    state->dataFluidProps->RefrigData(1).NumSuperTempPts = 2;
    state->dataFluidProps->RefrigData(1).NumSuperPressPts = 2;
    state->dataFluidProps->RefrigData(1).SHTemps.allocate(2);
    state->dataFluidProps->RefrigData(1).SHPress.allocate(2);
    state->dataFluidProps->RefrigData(1).SHTemps(1) = -157.15;
    state->dataFluidProps->RefrigData(1).SHTemps(2) = 152.85;
    state->dataFluidProps->RefrigData(1).SHPress(1) = 0.4043;
    state->dataFluidProps->RefrigData(1).SHPress(2) = 16500000.0;
    state->dataFluidProps->RefrigData(1).HshValues.allocate(2, 2);
    state->dataFluidProps->RefrigData(1).HshValues(1, 1) = 332800.0;
    state->dataFluidProps->RefrigData(1).HshValues(1, 2) = 537000.0;
    state->dataFluidProps->RefrigData(1).HshValues(2, 1) = 332800.0;
    state->dataFluidProps->RefrigData(1).HshValues(2, 2) = 537000.0;
    state->dataFluidProps->RefrigData(1).RhoshValues.allocate(2, 2);
    state->dataFluidProps->RefrigData(1).RhoshValues(1, 1) = 0.00003625;
    state->dataFluidProps->RefrigData(1).RhoshValues(1, 2) = 0.0;
    state->dataFluidProps->RefrigData(1).RhoshValues(2, 1) = 0.00003625;
    state->dataFluidProps->RefrigData(1).RhoshValues(2, 2) = 0.0;

    state->dataFluidProps->RefrigData(1).RhofLowTempIndex = 1;
    state->dataFluidProps->RefrigData(1).RhofHighTempIndex = 2;
    state->dataFluidProps->RefrigData(1).RhoTemps.allocate(2);
    state->dataFluidProps->RefrigData(1).RhoTemps(1) = -157.42;
    state->dataFluidProps->RefrigData(1).RhoTemps(2) = 96.145;
    state->dataFluidProps->RefrigData(1).RhofValues.allocate(2);
    state->dataFluidProps->RefrigData(1).RhofValues(1) = 1721.0;
    state->dataFluidProps->RefrigData(1).RhofValues(2) = 523.8;
    state->dataFluidProps->RefrigData(1).RhofgValues.allocate(2);
    state->dataFluidProps->RefrigData(1).RhofgValues(1) = 0.341;
    state->dataFluidProps->RefrigData(1).RhofgValues(2) = 523.8;

    GetWatertoAirHPInput(*state);

    int HPNum(1);
    Real64 DesignAirflow(2.0);
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).Temp = 5.0;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).Enthalpy = 44650.0;

    state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate = 15.0;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate =
        state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMax =
        state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMaxAvail =
        state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate;

    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).MassFlowRate = DesignAirflow;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).Temp = 26.0;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).HumRat = 0.007;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).Enthalpy = 43970.75;

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
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum;
    state->dataPlnt->PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum;

    bool InitFlag(true);
    Real64 MaxONOFFCyclesperHour(4.0);
    Real64 HPTimeConstant(0.1);
    Real64 FanDelayTime(60.0);
    Real64 SensLoad(38000.0);
    Real64 LatentLoad(0.0);
    Real64 PartLoadRatio(1.0);
    int CyclingScheme(1);
    bool FirstHVACIteration(true);
    Real64 RuntimeFrac(1.0);
    int CompOp(1);
    state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum = 1;

    InitWatertoAirHP(
        *state, HPNum, InitFlag, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio);

    CalcWatertoAirHPCooling(*state, HPNum, CyclingScheme, FirstHVACIteration, RuntimeFrac, InitFlag, SensLoad, CompOp, PartLoadRatio);

    // make sure the coil is active
    EXPECT_NE(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource, 0.0);
    EXPECT_NE(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power, 0.0);
    // check the source side energy balance
    EXPECT_NEAR(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource,
                state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate *
                    (state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy -
                     state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterEnthalpy),
                0.000000001);

    HPNum = 2;
    state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).LoopNum = 2;
    state->dataPlnt->PlantLoop(2).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(2).FluidName = "HotWater";
    state->dataPlnt->PlantLoop(2).FluidIndex = 1;
    state->dataPlnt->PlantLoop(2).FluidName = "WATER";
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).Name;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WAHPPlantTypeOfNum;
    state->dataPlnt->PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum;

    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).Temp = 35.0;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).Enthalpy = 43950.0;

    state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate = 15.0;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRate =
        state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMax =
        state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).WaterInletNodeNum).MassFlowRateMaxAvail =
        state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate;

    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).MassFlowRate = DesignAirflow;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).Temp = 15.0;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).HumRat = 0.004;
    state->dataLoopNodes->Node(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).AirInletNodeNum).Enthalpy = PsyHFnTdbW(15.0, 0.004);

    state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).DesignWaterMassFlowRate = 15.0;

    InitWatertoAirHP(
        *state, HPNum, InitFlag, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, SensLoad, LatentLoad, DesignAirflow, PartLoadRatio);

    CalcWatertoAirHPHeating(*state, HPNum, CyclingScheme, FirstHVACIteration, RuntimeFrac, InitFlag, SensLoad, CompOp, PartLoadRatio);

    // make sure the coil is active
    EXPECT_NE(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource, 0.0);
    EXPECT_NE(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).Power, 0.0);
    // check the source side energy balance
    EXPECT_NEAR(state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).QSource,
                state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterMassFlowRate *
                    (state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).InletWaterEnthalpy -
                     state->dataWaterToAirHeatPump->WatertoAirHP(HPNum).OutletWaterEnthalpy),
                0.000000001);

    // clean up
    state->dataWaterToAirHeatPump->WatertoAirHP.deallocate();
    state->dataFluidProps->RefrigData.deallocate();
}
