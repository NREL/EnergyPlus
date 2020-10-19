// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::WaterCoils Unit Tests
// FUNCTION INFORMATION:
//       AUTHOR         R Raustad
//       DATE WRITTEN   March 2015
//       MODIFIED
//       RE-ENGINEERED  na

// PURPOSE OF THIS FUNCTION:
// This function performs the sizing calculation on a chilled water coil.
// The water coil should size to use sizing data or Data* globals. After a coil is sized, the Data* variables should all be reset.
// Defect file showed a second coil in the input that autosized the same as the first (incorrect based on zone flow rates).

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace DataAirLoop;
using namespace DataAirSystems;
using namespace DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::FanCoilUnits;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::FluidProperties;
using namespace EnergyPlus::General;
using namespace EnergyPlus::GlobalNames;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::WaterCoils;
using namespace EnergyPlus::Psychrometrics;

class WaterCoilsTest : public EnergyPlusFixture
{

public:
    static void SetUpTestCase()
    {
        EnergyPlusFixture::SetUpTestCase(); // Sets up the base fixture
    }
    static void TearDownTestCase()
    {
    }

    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up individual test cases.

        CurZoneEqNum = 0;
        CurSysNum = 0;
        CurOASysNum = 0;
        state.dataWaterCoils->NumWaterCoils = 1;
        state.dataWaterCoils->WaterCoil.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->WaterCoilNumericFields.allocate(state.dataWaterCoils->NumWaterCoils);
        state.dataWaterCoils->WaterCoilNumericFields(state.dataWaterCoils->NumWaterCoils).FieldNames.allocate(17); // max N fields for water coil
        TotNumLoops = 1;
        PlantLoop.allocate(TotNumLoops);
        PlantSizData.allocate(1);
        ZoneEqSizing.allocate(1);
        UnitarySysEqSizing.allocate(1);
        OASysEqSizing.allocate(1);
        SysSizInput.allocate(1);
        ZoneSizingInput.allocate(1);
        SysSizPeakDDNum.allocate(1);
        SysSizPeakDDNum(1).TimeStepAtSensCoolPk.allocate(1);
        SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.allocate(1);
        SysSizPeakDDNum(1).TimeStepAtTotCoolPk.allocate(1);
        SysSizPeakDDNum(1).SensCoolPeakDD = 1;
        SysSizPeakDDNum(1).CoolFlowPeakDD = 1;
        SysSizPeakDDNum(1).TotCoolPeakDD = 1;
        FinalSysSizing.allocate(1);
        FinalZoneSizing.allocate(1);
        PrimaryAirSystem.allocate(1);
        state.dataAirLoop->AirLoopControlInfo.allocate(1);
        InitializePsychRoutines();
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!

        state.dataWaterCoils->NumWaterCoils = 0;
        state.dataWaterCoils->WaterCoil.clear();
        state.dataWaterCoils->WaterCoilNumericFields.clear();
        PlantLoop.clear();
        PlantSizData.clear();
        ZoneEqSizing.clear();
        UnitarySysEqSizing.clear();
        OASysEqSizing.clear();
        SysSizInput.clear();
        SysSizPeakDDNum.clear();
        FinalSysSizing.clear();
        SysSizPeakDDNum.clear();
        PrimaryAirSystem.clear();
        state.dataAirLoop->AirLoopControlInfo.clear();
        cached_Twb.clear();
        cached_Psat.clear();
    }
};

TEST_F(WaterCoilsTest, WaterCoolingCoilSizing)
{
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "WaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    FinalSysSizing(1).CoolSupTemp = 10.0;
    FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    FinalSysSizing(1).DesMainVolFlow = 0.00159;
    FinalSysSizing(1).HeatSupTemp = 25.0;
    FinalSysSizing(1).HeatOutTemp = 5.0;
    FinalSysSizing(1).HeatRetTemp = 20.0;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Cooling Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Cooling;
    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(3) = "Maximum Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;
    SysSizInput(1).CoolCapControl = VAV;
    PlantSizData(1).ExitTemp = 5.7;
    PlantSizData(1).DeltaT = 5.0;
    FinalSysSizing(1).MassFlowAtCoolPeak = FinalSysSizing(1).DesMainVolFlow * StdRhoAir;
    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    createCoilSelectionReportObj();
    SizeWaterCoil(state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00159, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    // Check that all Data* variables have been reset
    EXPECT_EQ(0, DataPltSizCoolNum);
    EXPECT_EQ(0, DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletWaterTemp);

    // set second cooling coil to size at a different air flow, adjust sizing data, and autosize input data
    FinalSysSizing(1).DesMainVolFlow = 0.00259;
    FinalSysSizing(1).MassFlowAtCoolPeak = FinalSysSizing(1).DesMainVolFlow * StdRhoAir;

    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Cooling Coil 2";
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00259, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    EXPECT_EQ(0, DataPltSizCoolNum);
    EXPECT_EQ(0, DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletWaterTemp);

    // size heating coil
    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;
    FinalSysSizing(1).DesMainVolFlow = 0.00359;
    FinalSysSizing(1).MassFlowAtCoolPeak = FinalSysSizing(1).DesMainVolFlow * StdRhoAir;
    state.dataAirLoop->AirLoopControlInfo(1).UnitarySys = true;

    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Heating Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00359, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    EXPECT_EQ(0, DataPltSizCoolNum);
    EXPECT_EQ(0, DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletWaterTemp);

    // size 2nd heating coil
    FinalSysSizing(1).DesMainVolFlow = 0.00459;
    FinalSysSizing(1).MassFlowAtCoolPeak = FinalSysSizing(1).DesMainVolFlow * StdRhoAir;

    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Heating Coil 2";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00459, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    EXPECT_EQ(0, DataPltSizCoolNum);
    EXPECT_EQ(0, DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, DataDesInletWaterTemp);

    // size zone water heating coil
    CurZoneEqNum = 1;
    CurSysNum = 0;
    PlantSizData(1).ExitTemp = 60.0;
    DataSizing::NumZoneSizingInput = 1;
    DataSizing::ZoneSizingRunDone = true;
    ZoneSizingInput(1).ZoneNum = CurZoneEqNum;
    ZoneEqSizing(1).SizingMethod.allocate(25);
    ZoneEqSizing(1).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;
    FinalZoneSizing(1).ZoneTempAtHeatPeak = 20.0;
    FinalZoneSizing(1).OutTempAtHeatPeak = -20.0;
    FinalZoneSizing(1).DesHeatCoilInTemp = -20.0; // simulates zone heating air flow rate <= zone OA flow rate
    FinalZoneSizing(1).DesHeatCoilInHumRat = 0.005;
    FinalZoneSizing(1).HeatDesTemp = 30.0;
    FinalZoneSizing(1).HeatDesHumRat = 0.005;
    ZoneEqSizing(1).OAVolFlow = 0.01;
    ZoneEqSizing(1).HeatingAirFlow = true;
    ZoneEqSizing(1).HeatingAirVolFlow = 0.1;
    FinalZoneSizing(1).DesHeatMassFlow = StdRhoAir * ZoneEqSizing(1).HeatingAirVolFlow;
    state.dataWaterCoils->MySizeFlag.allocate(1);
    state.dataWaterCoils->MySizeFlag(CoilNum) = true;
    state.dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state.dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum) = true;

    state.dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(state, CoilNum);
    EXPECT_NEAR(state.dataWaterCoils->WaterCoil(CoilNum).InletAirTemp, 16.0, 0.0001); // a mixture of zone air (20 C) and 10% OA (-20 C) = 16 C
    EXPECT_NEAR(state.dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad, 1709.8638, 0.0001);
    EXPECT_NEAR(state.dataWaterCoils->WaterCoil(CoilNum).UACoil, 51.2456, 0.0001);
    EXPECT_NEAR(state.dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp, 30.1302, 0.0001); // reasonable delta T above inlet air temp
}

TEST_F(WaterCoilsTest, TdbFnHRhPbTest)
{

    // using IP PsyCalc
    //   http://linricsoftw.web701.discountasp.net/webpsycalc.aspx

    EXPECT_NEAR(25.0, TdbFnHRhPb(45170., 0.40, 101312.), 0.05);
    EXPECT_NEAR(20.0, TdbFnHRhPb(34760., 0.40, 101312.), 0.05);
    EXPECT_NEAR(25.0, TdbFnHRhPb(50290., 0.50, 101312.), 0.05);
    EXPECT_NEAR(20.0, TdbFnHRhPb(38490., 0.50, 101312.), 0.05);
}

TEST_F(WaterCoilsTest, CoilHeatingWaterUASizing)
{
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "HotWaterLoop";
    PlantSizData(1).ExitTemp = 60.0;
    PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).DesMainVolFlow = 1.00;
    FinalSysSizing(1).HeatSupTemp = 40.0;
    FinalSysSizing(1).HeatOutTemp = 5.0;
    FinalSysSizing(1).HeatRetTemp = 20.0;
    FinalSysSizing(1).HeatOAOption = 1;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_SimpleHeating; // Coil:Heating:Water
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = DataGlobals::ScheduleAlwaysOn;

    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;
    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    state.dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state.dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state.dataWaterCoils->MySizeFlag.allocate(1);
    state.dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    createCoilSelectionReportObj();
    SizeWaterCoil(state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (10.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);

    // check coil UA-value sizing
    EXPECT_NEAR(1435.01, state.dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01);

    // test single zone VAV reheat coil sizing
    CurZoneEqNum = 1;
    CurTermUnitSizingNum = 1;
    CurSysNum = 0;
    TermUnitSizing.allocate(1);
    TermUnitFinalZoneSizing.allocate(1);
    TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate / 3.0; // DesAirVolFlowRate = 1.0
    TermUnitSizing(CurTermUnitSizingNum).MaxHWVolFlow = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate / 3.0;
    TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac = 0.5;
    DataSizing::TermUnitSingDuct = true;

    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    SysSizingRunDone = false;
    ZoneSizingRunDone = true;
    NumZoneSizingInput = 1;
    ZoneSizingInput.allocate(1);
    ZoneSizingInput(1).ZoneNum = 1;
    ZoneEqSizing.allocate(1);
    ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(20);
    ZoneEqSizing(CurZoneEqNum).SizingMethod(DataHVACGlobals::HeatingAirflowSizing) = DataHVACGlobals::HeatingAirflowSizing;
    ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow = 0.0;
    ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow = 1.0;
    FinalZoneSizing.allocate(1);
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 1.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInTempTU = 10.0;
    FinalZoneSizing(CurZoneEqNum).ZoneTempAtHeatPeak = 21.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInHumRatTU = 0.006;
    FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.008;
    TermUnitFinalZoneSizing(CurTermUnitSizingNum) = FinalZoneSizing(CurZoneEqNum);

    state.dataWaterCoils->MySizeFlag(1) = true;
    // run water coil sizing
    SizeWaterCoil(state, CoilNum);

    // check coil UA-value sizing
    EXPECT_NEAR(577.686, state.dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01); // smaller UA than result above at 1435.00
}

TEST_F(WaterCoilsTest, CoilHeatingWaterLowAirFlowUASizing)
{
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "HotWaterLoop";
    PlantSizData(1).ExitTemp = 60.0;
    PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).DesMainVolFlow = 1.00;
    FinalSysSizing(1).HeatSupTemp = 40.0;
    FinalSysSizing(1).HeatOutTemp = 5.0;
    FinalSysSizing(1).HeatRetTemp = 20.0;
    FinalSysSizing(1).HeatOAOption = 1;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_SimpleHeating; // Coil:Heating:Water
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = DataGlobals::ScheduleAlwaysOn;

    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;
    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    state.dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state.dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state.dataWaterCoils->MySizeFlag.allocate(1);
    state.dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    createCoilSelectionReportObj();
    SizeWaterCoil(state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (10.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);

    // check coil UA-value sizing
    EXPECT_NEAR(1435.01, state.dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01);

    // test single zone VAV reheat coil sizing
    CurZoneEqNum = 1;
    CurSysNum = 0;
    CurTermUnitSizingNum = 1;
    TermUnitSizing.allocate(1);
    TermUnitFinalZoneSizing.allocate(1);
    TermUnitSizing(CurTermUnitSizingNum).AirVolFlow =
        state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate / 1500.0; // DesAirVolFlowRate = 1.0 so TU air flow = 0.00067 (lower than 0.001)
    TermUnitSizing(CurTermUnitSizingNum).MaxHWVolFlow = state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate / 1500.0;
    TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac = 0.5;
    DataSizing::TermUnitSingDuct = true;

    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    SysSizingRunDone = false;
    ZoneSizingRunDone = true;
    NumZoneSizingInput = 1;
    ZoneSizingInput.allocate(1);
    ZoneSizingInput(1).ZoneNum = 1;
    ZoneEqSizing.allocate(1);
    ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(20);
    ZoneEqSizing(CurZoneEqNum).SizingMethod(DataHVACGlobals::HeatingAirflowSizing) = DataHVACGlobals::HeatingAirflowSizing;
    ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow = 0.0;
    ZoneEqSizing(CurZoneEqNum).HeatingAirVolFlow = 1.0;
    FinalZoneSizing.allocate(1);
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow = 0.00095;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInTempTU = 10.0;
    FinalZoneSizing(CurZoneEqNum).ZoneTempAtHeatPeak = 21.0;
    FinalZoneSizing(CurZoneEqNum).DesHeatCoilInHumRatTU = 0.006;
    FinalZoneSizing(CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.008;
    TermUnitFinalZoneSizing(CurTermUnitSizingNum) = FinalZoneSizing(CurZoneEqNum);

    state.dataWaterCoils->MySizeFlag(1) = true;
    // run water coil sizing
    SizeWaterCoil(state, CoilNum);

    // water flow rate should be non-zero, and air flow rate being so small will get set to 0 during sizing
    EXPECT_GT(state.dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate, 0.0);
    EXPECT_EQ(0.0, state.dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate);
    // check coil UA-value sizing
    EXPECT_NEAR(1.0, state.dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.0001); // TU air flow is too low to size, set to 0, so UA is set to 1.0
}

TEST_F(WaterCoilsTest, CoilHeatingWaterUASizingLowHwaterInletTemp)
{
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "HotWaterLoop";
    PlantSizData(1).ExitTemp = 40.0; // low heating coil inlet water temp
    PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).DesMainVolFlow = 1.00;
    FinalSysSizing(1).HeatSupTemp = 40.0;
    FinalSysSizing(1).HeatOutTemp = 5.0;
    FinalSysSizing(1).HeatRetTemp = 20.0;
    FinalSysSizing(1).HeatOAOption = 1;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_SimpleHeating; // Coil:Heating:Water
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = DataGlobals::ScheduleAlwaysOn;

    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;

    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    state.dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state.dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state.dataWaterCoils->MySizeFlag.allocate(1);
    state.dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    createCoilSelectionReportObj();
    SizeWaterCoil(state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (10.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);

    // check coil UA-value sizing for low design loop exit temp
    EXPECT_NEAR(2479.27, state.dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01);

    Real64 DesCoilInletWaterTempUsed = 0.0;
    Real64 DataFanOpMode = ContFanCycCoil;
    Real64 UAMax = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate;

    // check if coil design inlet water temperature is increased above the plant loop exit temp
    EstimateCoilInletWaterTemp(state, CoilNum, DataFanOpMode, 1.0, UAMax, DesCoilInletWaterTempUsed);
    EXPECT_GT(DesCoilInletWaterTempUsed, PlantSizData(1).ExitTemp);
    EXPECT_NEAR(48.73, DesCoilInletWaterTempUsed, 0.01);
}

TEST_F(WaterCoilsTest, CoilCoolingWaterSimpleSizing)
{
    InitializePsychRoutines();
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);
    ShowMessage("Begin Test: state.dataWaterCoils->WaterCoilsTest, CoilCoolingWaterSimpleSizing");

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "WaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    FinalSysSizing(1).CoolSupTemp = 10.0;
    FinalSysSizing(1).CoolSupHumRat = 0.0085;
    FinalSysSizing(1).DesMainVolFlow = 1.00;
    FinalSysSizing(1).MassFlowAtCoolPeak = FinalSysSizing(1).DesMainVolFlow * StdRhoAir;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Simple Water Cooling Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Cooling;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_Cooling; // Coil:Cooling:Water
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = state.dataWaterCoils->CoilModel_Cooling;

    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = 6.67;
    state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(1) = "Design Water Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;
    SysSizInput(1).CoolCapControl = VAV;
    PlantSizData(1).ExitTemp = 5.7;
    PlantSizData(1).DeltaT = 5.0;

    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    // run water coil sizing
    createCoilSelectionReportObj();
    SizeWaterCoil(state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 DesCoilCoolingLoad = 0.0;
    Real64 CoilInEnth = 0.0;
    Real64 CoilOutEnth = 0.0;

    CoilInEnth = PsyHFnTdbW(20.0, 0.01);
    CoilOutEnth = PsyHFnTdbW(10.0, 0.0085);
    DesCoilCoolingLoad = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * StdRhoAir * (CoilInEnth - CoilOutEnth);

    // check cooling coil design load
    EXPECT_DOUBLE_EQ(DesCoilCoolingLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::CWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::CWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate / (state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp * Cp * rho);

    // check cooling coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}

TEST_F(WaterCoilsTest, CoilCoolingWaterDetailedSizing)
{
    InitializePsychRoutines();
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);
    ShowMessage("Begin Test: state.dataWaterCoils->WaterCoilsTest, CoilCoolingWaterDetailedSizing");

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "WaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    FinalSysSizing(1).CoolSupTemp = 10.0;
    FinalSysSizing(1).CoolSupHumRat = 0.0085;
    FinalSysSizing(1).DesMainVolFlow = 1.00;
    FinalSysSizing(1).MassFlowAtCoolPeak = FinalSysSizing(1).DesMainVolFlow * StdRhoAir;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Detailed Water Cooling Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Cooling;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_DetFlatFinCooling; // Coil:Cooling:Water:DetailedGeometry
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = state.dataWaterCoils->CoilModel_Detailed;

    state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea = 6.23816;
    state.dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea = 6.20007018;
    state.dataWaterCoils->WaterCoil(CoilNum).FinSurfArea = 101.7158224;
    state.dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea = 0.810606367;
    state.dataWaterCoils->WaterCoil(CoilNum).CoilDepth = 0.165097968;
    state.dataWaterCoils->WaterCoil(CoilNum).FinDiam = 0.43507152;
    state.dataWaterCoils->WaterCoil(CoilNum).FinThickness = 0.001499982;
    state.dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam = 0.014449958;
    state.dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam = 0.015879775;
    state.dataWaterCoils->WaterCoil(CoilNum).TubeThermConductivity = 385.764854;
    state.dataWaterCoils->WaterCoil(CoilNum).FinThermConductivity = 203.882537;
    state.dataWaterCoils->WaterCoil(CoilNum).FinSpacing = 0.001814292;
    state.dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing = 0.02589977;
    state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows = 6;
    state.dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow = 16;
    state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = 6.67;
    state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;

    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(1) = "Design Water Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;
    SysSizInput(1).CoolCapControl = VAV;
    PlantSizData(1).ExitTemp = 5.7;
    PlantSizData(1).DeltaT = 5.0;
    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    // run water coil sizing
    createCoilSelectionReportObj();
    SizeWaterCoil(state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 DesCoilCoolingLoad = 0.0;
    Real64 CoilInEnth = 0.0;
    Real64 CoilOutEnth = 0.0;

    CoilInEnth = PsyHFnTdbW(FinalSysSizing(1).MixTempAtCoolPeak, FinalSysSizing(1).MixHumRatAtCoolPeak);
    CoilOutEnth = PsyHFnTdbW(FinalSysSizing(1).CoolSupTemp, FinalSysSizing(1).CoolSupHumRat);
    DesCoilCoolingLoad = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * StdRhoAir * (CoilInEnth - CoilOutEnth);
    // check cooling coil design load
    EXPECT_DOUBLE_EQ(DesCoilCoolingLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::CWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::CWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate / (6.67 * Cp * rho);
    // check cooling coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}
TEST_F(WaterCoilsTest, CoilHeatingWaterSimpleSizing)
{
    InitializePsychRoutines();
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);
    ShowMessage("Begin Test: state.dataWaterCoils->WaterCoilsTest, CoilHeatingWaterSimpleSizing");

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "WaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).DesMainVolFlow = 1.00;
    FinalSysSizing(1).HeatSupTemp = 40.0;
    FinalSysSizing(1).HeatOutTemp = 5.0;
    FinalSysSizing(1).HeatRetTemp = 20.0;
    FinalSysSizing(1).HeatOAOption = 1;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Test Simple Water Heating Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_SimpleHeating; // Coil:Heating:Water
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = 11.0;
    state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;

    PlantSizData(1).ExitTemp = 60.0;
    PlantSizData(1).DeltaT = 10.0;

    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    // run water coil sizing
    createCoilSelectionReportObj();
    SizeWaterCoil(state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = FinalSysSizing(1).DesMainVolFlow * StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(state, PlantLoop(1).FluidName, DataGlobalConstants::HWInitConvTemp(), PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (11.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}
TEST_F(WaterCoilsTest, HotWaterHeatingCoilAutoSizeTempTest)
{
    OutBaroPress = 101325.0;
    StdRhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    SysSizingRunDone = true;

    // set up plant sizing
    NumPltSizInput = 1;
    PlantSizData(1).PlantLoopName = "HotWaterLoop";
    PlantSizData(1).ExitTemp = 60.0; // hot water coil inlet water temp
    PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(1).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(1).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "FluidWaterLoop";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    FinalSysSizing(1).DesMainVolFlow = 1.00;
    FinalSysSizing(1).HeatSupTemp = 40.0;
    FinalSysSizing(1).HeatOutTemp = 5.0;
    FinalSysSizing(1).HeatRetTemp = 20.0;
    FinalSysSizing(1).HeatOAOption = 1;

    // set up water coil
    int CoilNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType_Num = state.dataWaterCoils->WaterCoil_SimpleHeating; // Coil:Heating:Water
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = state.dataWaterCoils->CoilType_Heating;
    state.dataWaterCoils->WaterCoil(CoilNum).SchedPtr = DataGlobals::ScheduleAlwaysOn;

    state.dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state.dataWaterCoils->UAandFlow;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state.dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state.dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    CurZoneEqNum = 0;
    CurSysNum = 1;
    CurOASysNum = 0;

    DataWaterLoopNum = 1;
    NumOfGlycols = 1;

    state.dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state.dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state.dataWaterCoils->MySizeFlag.allocate(1);
    state.dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    SizeWaterCoil(state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd(0.0);
    Real64 DesMassFlow(0.0);
    Real64 DesCoilHeatingLoad(0.0);

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state.dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * StdRhoAir;
    DesCoilHeatingLoad = DesMassFlow * CpAirStd * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state.dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp(0.0);
    Real64 rho(0.0);
    Real64 DesWaterFlowRate(0.0);

    // now size heating coil hot water flow rate at 60.0C
    Cp = GetSpecificHeatGlycol(state, PlantLoop(1).FluidName, 60.0, PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(state, PlantLoop(1).FluidName, 60.0, PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = DesCoilHeatingLoad / (PlantSizData(1).DeltaT * Cp * rho);

    // check heating coil design water flow rate calculated here and sizing results are identical
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state.dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}

TEST_F(WaterCoilsTest, FanCoilCoolingWaterFlowTest)
{
    PlantSizData.allocate(2);
    NumPltSizInput = 2;

    PlantSizData(2).PlantLoopName = "ChilledWaterLoop";
    PlantSizData(2).ExitTemp = 7.22; // chilled water coil inlet water temp
    PlantSizData(2).DeltaT = 6.67;
    PlantSizData(1).PlantLoopName = "HotWaterLoop";
    PlantSizData(1).ExitTemp = 60; // hot water coil inlet water temp
    PlantSizData(1).DeltaT = 12;
    int FanCoilNum(1);
    int ZoneNum(1);
    int ControlledZoneNum(1);
    bool FirstHVACIteration(true);
    bool ErrorsFound(false);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);
    Real64 LatOutputProvided(0.0);

    DataEnvironment::OutBaroPress = 101325.0;
    DataEnvironment::StdRhoAir = 1.20;
    state.dataWaterCoils->GetWaterCoilsInputFlag = true;
    NumCoils = 0;
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::TimeStep = 1;
    DataGlobals::MinutesPerTimeStep = 60;

    InitializePsychRoutines();

    std::string const idf_objects = delimited_string({
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
         "   SequentialLoad,          !- Load Distribution Scheme",
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
         "	ScheduleTypeLimits,",
         "	Fraction, !- Name",
         "	0.0, !- Lower Limit Value",
         "	1.0, !- Upper Limit Value",
         "	CONTINUOUS;              !- Numeric Type",
         "   Fan:OnOff,",
         "	Zone1FanCoilFan, !- Name",
         "	FanAndCoilAvailSched, !- Availability Schedule Name",
         "	0.5, !- Fan Total Efficiency",
         "	75.0, !- Pressure Rise { Pa }",
         "	Autosize, !- Maximum Flow Rate { m3 / s }",
         "	0.9, !- Motor Efficiency",
         "	1.0, !- Motor In Airstream Fraction",
         "	Zone1FanCoilOAMixerOutletNode, !- Air Inlet Node Name",
         "	Zone1FanCoilFanOutletNode, !- Air Outlet Node Name",
         "	, !- Fan Power Ratio Function of Speed Ratio Curve Name",
         "	;                        !- Fan Efficiency Ratio Function of Speed Ratio Curve Name	",
         "	Coil:Cooling:Water,",
         "	Zone1FanCoilCoolingCoil, !- Name",
         "	FanAndCoilAvailSched, !- Availability Schedule Namev",
         "	Autosize, !- Design Water Flow Rate { m3 / s }",
         "	Autosize, !- Design Air Flow Rate { m3 / s }",
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
         "	Autosize, !- Maximum Water Flow Rate { m3 / s }",
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
         "	ConstantFanVariableFlow, !- Capacity Control Method",
         "	0.5, !- Maximum Supply Air Flow Rate { m3 / s }",
         "	0.3, !- Low Speed Supply Air Flow Ratio",
         "	0.6, !- Medium Speed Supply Air Flow Ratio",
         "	0.1, !- Maximum Outdoor Air Flow Rate { m3 / s }",
         "	FanAndCoilAvailSched, !- Outdoor Air Schedule Name",
         "	Zone1FanCoilAirInletNode, !- Air Inlet Node Name",
         "	Zone1FanCoilAirOutletNode, !- Air Outlet Node Name",
         "	OutdoorAir:Mixer, !- Outdoor Air Mixer Object Type",
         "	Zone1FanCoilOAMixer, !- Outdoor Air Mixer Name",
         "	Fan:OnOff, !- Supply Air Fan Object Type",
         "	Zone1FanCoilFan, !- Supply Air Fan Name",
         "	Coil:Cooling:Water, !- Cooling Coil Object Type",
         "	Zone1FanCoilCoolingCoil, !- Cooling Coil Name",
         "	0.0002, !- Maximum Cold Water Flow Rate { m3 / s }",
         "	0.0, !- Minimum Cold Water Flow Rate { m3 / s }",
         "	0.001, !- Cooling Convergence Tolerance",
         "	Coil:Heating:Water, !- Heating Coil Object Type",
         "	Zone1FanCoilHeatingCoil, !- Heating Coil Name",
         "	0.0002, !- Maximum Hot Water Flow Rate { m3 / s }",
         "	0.0, !- Minimum Hot Water Flow Rate { m3 / s }",
         "	0.001; !- Heating Convergence Tolerance",

     });

    ASSERT_TRUE(process_idf(idf_objects));

    GetZoneData(state, ErrorsFound);
    EXPECT_EQ("EAST ZONE", Zone(1).Name);

    GetZoneEquipmentData1(state);
    ProcessScheduleInput(state);
    ScheduleInputProcessed = true;
    GetFanInput(state);
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, Fan(1).FanType_Num);

    GetFanCoilUnits(state);
    EXPECT_EQ("CONSTANTFANVARIABLEFLOW", FanCoil(1).CapCtrlMeth);
    EXPECT_EQ("OUTDOORAIR:MIXER", FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", FanCoil(1).HCoilType);

    TotNumLoops = 2;
    PlantLoop.allocate(TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;

    // cooling load only
    HotWaterMassFlowRate = 0.0;
    ColdWaterMassFlowRate = 0.14;

    Node(OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    Node(OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    Node(OAMixer(1).RetNode).Temp = 24.0;
    Node(OAMixer(1).RetNode).Enthalpy = 36000;
    Node(OAMixer(1).RetNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).RetNode).Temp, Node(OAMixer(1).RetNode).Enthalpy);

    Node(OAMixer(1).InletNode).Temp = 30.0;
    Node(OAMixer(1).InletNode).Enthalpy = 53000;
    Node(OAMixer(1).InletNode).HumRat = PsyWFnTdbH(Node(OAMixer(1).InletNode).Temp, Node(OAMixer(1).InletNode).Enthalpy);

    Node(FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    Node(FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    FanCoil(1).OutAirMassFlow = 0.0;
    FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    FanCoil(1).MaxCoolCoilFluidFlow = 0.14;
    FanCoil(1).MaxHeatCoilFluidFlow = 0.14;

    Node(FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMax = 0.14;
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMax = 0.14;
    Node(FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;
    Node(FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;

    Fan(1).InletAirMassFlowRate = AirMassFlow;
    Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    Node(Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    state.dataWaterCoils->WaterCoil(2).UACoilTotal = 470.0;
    state.dataWaterCoils->WaterCoil(2).UACoilExternal = 611.0;
    state.dataWaterCoils->WaterCoil(2).UACoilInternal = 2010.0;
    state.dataWaterCoils->WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    Node(state.dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(state.dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    Node(state.dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    Node(state.dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    state.dataWaterCoils->WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    Node(state.dataWaterCoils->WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(state.dataWaterCoils->WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    Node(state.dataWaterCoils->WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    Node(state.dataWaterCoils->WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    Node(state.dataWaterCoils->WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    Node(state.dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    Node(state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    Node(state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    state.dataWaterCoils->WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= TotNumLoops; ++l) {
        auto &loop(PlantLoop(l));
        loop.LoopSide.allocate(2);
        auto &loopside(PlantLoop(l).LoopSide(1));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(PlantLoop(l).LoopSide(1).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    DataHeatBalFanSys::TempControlType.allocate(1);
    DataHeatBalFanSys::TempControlType(1) = 4;

    state.dataWaterCoils->WaterCoil(2).WaterLoopNum = 1;
    state.dataWaterCoils->WaterCoil(2).WaterLoopSide = 1;
    state.dataWaterCoils->WaterCoil(2).WaterLoopBranchNum = 1;
    state.dataWaterCoils->WaterCoil(2).WaterLoopCompNum = 1;

    state.dataWaterCoils->WaterCoil(1).WaterLoopNum = 2;
    state.dataWaterCoils->WaterCoil(1).WaterLoopSide = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopBranchNum = 1;
    state.dataWaterCoils->WaterCoil(1).WaterLoopCompNum = 1;

    PlantLoop(2).Name = "ChilledWaterLoop";
    PlantLoop(2).FluidName = "ChilledWater";
    PlantLoop(2).FluidIndex = 1;
    PlantLoop(2).FluidName = "WATER";
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(2).Name;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state.dataWaterCoils->WaterCoil_Cooling;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(2).WaterInletNodeNum;
    PlantLoop(2).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state.dataWaterCoils->WaterCoil(2).WaterOutletNodeNum;
    PlantLoop(2).LoopSide(1).FlowLock = 0;

    PlantLoop(1).Name = "HotWaterLoop";
    PlantLoop(1).FluidName = "HotWater";
    PlantLoop(1).FluidIndex = 1;
    PlantLoop(1).FluidName = "WATER";
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).Name = state.dataWaterCoils->WaterCoil(1).Name;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).TypeOf_Num = state.dataWaterCoils->WaterCoil_SimpleHeating;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumIn = state.dataWaterCoils->WaterCoil(1).WaterInletNodeNum;
    PlantLoop(1).LoopSide(1).Branch(1).Comp(1).NodeNumOut = state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;
    PlantLoop(1).LoopSide(1).FlowLock = 0;

    FanCoil(1).CoolCoilLoopNum = 2;
    FanCoil(1).HeatCoilLoopNum = 1;
    FanCoil(1).CoolCoilLoopSide = 1;
    FanCoil(1).HeatCoilLoopSide = 1;
    FanCoil(1).HeatCoilFluidOutletNodeNum = state.dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;
    FanCoil(1).CoolCoilFluidOutletNodeNum = state.dataWaterCoils->WaterCoil(2).WaterOutletNodeNum;
    FanCoil(1).CoolCoilBranchNum = 1;
    FanCoil(1).CoolCoilCompNum = 1;
    FanCoil(1).HeatCoilBranchNum = 1;
    FanCoil(1).HeatCoilCompNum = 1;

    HeatingLoad = false;
    CoolingLoad = true;
    ZoneSysEnergyDemand.allocate(1);
    ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -4000.00;
    ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -8000.0;
    FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QZnReq = -4000.0;

    DataGlobals::DoingSizing = true;

    CurZoneEqNum = 1;
    ZoneSizingRunDone = true;
    DataSizing::ZoneEqSizing.allocate(CurZoneEqNum);
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).SizingMethod.allocate(25);
    DataSizing::ZoneEqSizing(CurZoneEqNum).SizingMethod(25) = 0;
    ZoneEqFanCoil = true;
    ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent = true;

    // User-specified air volume flow rate from the ZoneHVAC:FourPipeFanCoil object
    DataSizing::ZoneEqSizing(CurZoneEqNum).AirVolFlow = 0.5;
    // User-specified water flow rate from the ZoneHVAC:FourPipeFanCoil object
    DataSizing::ZoneEqSizing(CurZoneEqNum).MaxCWVolFlow = 0.0002;
    // User-specified water flow rate from the ZoneHVAC:FourPipeFanCoil object
    DataSizing::ZoneEqSizing(CurZoneEqNum).MaxHWVolFlow = 0.0002;

    // Initial design air volume flow rate based on the design conditions
    state.dataWaterCoils->WaterCoil(2).DesAirVolFlowRate = DataSizing::AutoSize;
    // Initial design water flow rate based on the design conditions
    state.dataWaterCoils->WaterCoil(2).MaxWaterVolFlowRate = DataSizing::AutoSize;

    // normal cooling simulation for constant fan variable flow fan coil
    Sim4PipeFanCoil(state, FanCoilNum, ZoneNum, ControlledZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);

    // Expect final design air volume flow rate to equal the user-specified air volume flow rate from the ZoneHVAC:FourPipeFanCoil object
    EXPECT_EQ(state.dataWaterCoils->WaterCoil(2).DesAirVolFlowRate, 0.5);
    // Expect final design water flow rate to equal the user-specified water flow rate from the ZoneHVAC:FourPipeFanCoil object
    EXPECT_EQ(state.dataWaterCoils->WaterCoil(2).MaxWaterVolFlowRate, 0.0002);


}
