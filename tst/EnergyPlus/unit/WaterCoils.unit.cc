// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FanCoilUnits.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
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
    //    static void SetUpTestCase()
    //    {
    //        EnergyPlusFixture::SetUpTestCase(); // Sets up the base fixture
    //    }
    static void TearDownTestCase()
    {
    }

    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up individual test cases.

        state->dataSize->CurZoneEqNum = 0;
        state->dataSize->CurSysNum = 0;
        state->dataSize->CurOASysNum = 0;
        state->dataWaterCoils->NumWaterCoils = 1;
        state->dataWaterCoils->WaterCoil.allocate(state->dataWaterCoils->NumWaterCoils);
        state->dataWaterCoils->WaterCoilNumericFields.allocate(state->dataWaterCoils->NumWaterCoils);
        state->dataWaterCoils->WaterCoilNumericFields(state->dataWaterCoils->NumWaterCoils).FieldNames.allocate(17); // max N fields for water coil
        state->dataPlnt->TotNumLoops = 1;
        state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);
        state->dataSize->PlantSizData.allocate(1);
        state->dataSize->ZoneEqSizing.allocate(1);
        state->dataSize->UnitarySysEqSizing.allocate(1);
        state->dataSize->OASysEqSizing.allocate(1);
        state->dataSize->SysSizInput.allocate(1);
        state->dataSize->ZoneSizingInput.allocate(1);
        state->dataSize->SysSizPeakDDNum.allocate(1);
        state->dataSize->SysSizPeakDDNum(1).TimeStepAtSensCoolPk.allocate(1);
        state->dataSize->SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.allocate(1);
        state->dataSize->SysSizPeakDDNum(1).TimeStepAtTotCoolPk.allocate(1);
        state->dataSize->SysSizPeakDDNum(1).SensCoolPeakDD = 1;
        state->dataSize->SysSizPeakDDNum(1).CoolFlowPeakDD = 1;
        state->dataSize->SysSizPeakDDNum(1).TotCoolPeakDD = 1;
        state->dataSize->FinalSysSizing.allocate(1);
        state->dataSize->CalcSysSizing.allocate(1);
        state->dataSize->FinalZoneSizing.allocate(1);
        state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
        state->dataAirLoop->AirLoopControlInfo.allocate(1);
        InitializePsychRoutines(*state);
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

TEST_F(WaterCoilsTest, WaterCoolingCoilSizing)
{
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {

        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "WaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    state->dataSize->FinalSysSizing(1).CoolSupTemp = 10.0;
    state->dataSize->FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 0.00159;
    state->dataSize->FinalSysSizing(1).HeatSupTemp = 25.0;
    state->dataSize->FinalSysSizing(1).HeatOutTemp = 5.0;
    state->dataSize->FinalSysSizing(1).HeatRetTemp = 20.0;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Cooling Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterCooling;
    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(3) = "Maximum Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->SysSizInput(1).CoolCapControl = DataSizing::CapacityControl::VAV;
    state->dataSize->PlantSizData(1).ExitTemp = 5.7;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;
    state->dataSize->FinalSysSizing(1).MassFlowAtCoolPeak = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;
    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00159, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    // Check that all Data* variables have been reset
    EXPECT_EQ(0, state->dataSize->DataPltSizCoolNum);
    EXPECT_EQ(0, state->dataSize->DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletWaterTemp);

    // set second cooling coil to size at a different air flow, adjust sizing data, and autosize input data
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 0.00259;
    state->dataSize->FinalSysSizing(1).MassFlowAtCoolPeak = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;

    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Cooling Coil 2";
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(*state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00259, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    EXPECT_EQ(0, state->dataSize->DataPltSizCoolNum);
    EXPECT_EQ(0, state->dataSize->DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletWaterTemp);

    // size heating coil
    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 0.00359;
    state->dataSize->FinalSysSizing(1).MassFlowAtCoolPeak = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;
    state->dataAirLoop->AirLoopControlInfo(1).UnitarySys = true;

    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Heating Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(*state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00359, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    EXPECT_EQ(0, state->dataSize->DataPltSizCoolNum);
    EXPECT_EQ(0, state->dataSize->DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletWaterTemp);

    // size 2nd heating coil
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 0.00459;
    state->dataSize->FinalSysSizing(1).MassFlowAtCoolPeak = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;

    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Water Heating Coil 2";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(*state, CoilNum);

    EXPECT_DOUBLE_EQ(0.00459, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);
    EXPECT_EQ(0, state->dataSize->DataPltSizCoolNum);
    EXPECT_EQ(0, state->dataSize->DataWaterLoopNum);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataConstantUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFractionUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataAirFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataWaterFlowUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataCapacityUsedForSizing);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirTemp);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesOutletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletAirHumRat);
    EXPECT_DOUBLE_EQ(0.0, state->dataSize->DataDesInletWaterTemp);

    // size zone water heating coil
    state->dataSize->CurZoneEqNum = 1;
    state->dataSize->CurSysNum = 0;
    state->dataSize->PlantSizData(1).ExitTemp = 60.0;
    state->dataSize->NumZoneSizingInput = 1;
    state->dataSize->ZoneSizingRunDone = true;
    state->dataSize->ZoneSizingInput(1).ZoneNum = state->dataSize->CurZoneEqNum;
    state->dataSize->ZoneEqSizing(1).SizingMethod.allocate(25);
    state->dataSize->ZoneEqSizing(1).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;
    state->dataSize->FinalZoneSizing(1).ZoneTempAtHeatPeak = 20.0;
    state->dataSize->FinalZoneSizing(1).OutTempAtHeatPeak = -20.0;
    state->dataSize->FinalZoneSizing(1).DesHeatCoilInTemp = -20.0; // simulates zone heating air flow rate <= zone OA flow rate
    state->dataSize->FinalZoneSizing(1).DesHeatCoilInHumRat = 0.005;
    state->dataSize->FinalZoneSizing(1).HeatDesTemp = 30.0;
    state->dataSize->FinalZoneSizing(1).HeatDesHumRat = 0.005;
    state->dataSize->ZoneEqSizing(1).OAVolFlow = 0.01;
    state->dataSize->ZoneEqSizing(1).HeatingAirFlow = true;
    state->dataSize->ZoneEqSizing(1).HeatingAirVolFlow = 0.1;
    state->dataSize->FinalZoneSizing(1).DesHeatMassFlow = state->dataEnvrn->StdRhoAir * state->dataSize->ZoneEqSizing(1).HeatingAirVolFlow;
    state->dataWaterCoils->MySizeFlag.allocate(1);
    state->dataWaterCoils->MySizeFlag(CoilNum) = true;
    state->dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state->dataWaterCoils->MyUAAndFlowCalcFlag(CoilNum) = true;

    state->dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    SizeWaterCoil(*state, CoilNum);
    EXPECT_NEAR(state->dataWaterCoils->WaterCoil(CoilNum).InletAirTemp, 16.0, 0.0001); // a mixture of zone air (20 C) and 10% OA (-20 C) = 16 C
    EXPECT_NEAR(state->dataWaterCoils->WaterCoil(CoilNum).DesTotWaterCoilLoad, 1709.8638, 0.0001);
    EXPECT_NEAR(state->dataWaterCoils->WaterCoil(CoilNum).UACoil, 51.2456, 0.0001);
    EXPECT_NEAR(state->dataWaterCoils->WaterCoil(CoilNum).OutletAirTemp, 30.1302, 0.0001); // reasonable delta T above inlet air temp
}

TEST_F(WaterCoilsTest, TdbFnHRhPbTest)
{

    // using IP PsyCalc
    //   http://linricsoftw.web701.discountasp.net/webpsycalc.aspx

    EXPECT_NEAR(25.0, TdbFnHRhPb(*state, 45170., 0.40, 101312.), 0.05);
    EXPECT_NEAR(20.0, TdbFnHRhPb(*state, 34760., 0.40, 101312.), 0.05);
    EXPECT_NEAR(25.0, TdbFnHRhPb(*state, 50290., 0.50, 101312.), 0.05);
    EXPECT_NEAR(20.0, TdbFnHRhPb(*state, 38490., 0.50, 101312.), 0.05);
}

TEST_F(WaterCoilsTest, CoilHeatingWaterUASizing)
{
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "HotWaterLoop";
    state->dataSize->PlantSizData(1).ExitTemp = 60.0;
    state->dataSize->PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {

        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).HeatSupTemp = 40.0;
    state->dataSize->FinalSysSizing(1).HeatOutTemp = 5.0;
    state->dataSize->FinalSysSizing(1).HeatRetTemp = 20.0;
    state->dataSize->FinalSysSizing(1).HeatOAOption = DataSizing::OAControl::AllOA;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::HeatingSimple;
    state->dataWaterCoils->WaterCoil(CoilNum).SchedPtr = ScheduleManager::ScheduleAlwaysOn;

    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state->dataWaterCoils->UAandFlow;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    state->dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state->dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state->dataWaterCoils->MySizeFlag.allocate(1);
    state->dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state->dataEnvrn->StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (10.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);

    // check coil UA-value sizing
    EXPECT_NEAR(1435.01, state->dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01);

    // test single zone VAV reheat coil sizing
    state->dataSize->CurZoneEqNum = 1;
    state->dataSize->CurTermUnitSizingNum = 1;
    state->dataSize->CurSysNum = 0;
    state->dataSize->TermUnitSizing.allocate(1);
    state->dataSize->TermUnitFinalZoneSizing.allocate(1);
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).AirVolFlow =
        state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate / 3.0; // DesAirVolFlowRate = 1.0
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).MaxHWVolFlow =
        state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate / 3.0;
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).MinFlowFrac = 0.5;
    state->dataSize->TermUnitSingDuct = true;

    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state->dataWaterCoils->UAandFlow;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state->dataSize->SysSizingRunDone = false;
    state->dataSize->ZoneSizingRunDone = true;
    state->dataSize->NumZoneSizingInput = 1;
    state->dataSize->ZoneSizingInput.allocate(1);
    state->dataSize->ZoneSizingInput(1).ZoneNum = 1;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod.allocate(20);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod(DataHVACGlobals::HeatingAirflowSizing) =
        DataHVACGlobals::HeatingAirflowSizing;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingAirVolFlow = 0.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingAirVolFlow = 1.0;
    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesCoolVolFlow = 0.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatVolFlow = 1.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatCoilInTempTU = 10.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).ZoneTempAtHeatPeak = 21.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatCoilInHumRatTU = 0.006;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.008;
    state->dataSize->TermUnitFinalZoneSizing(state->dataSize->CurTermUnitSizingNum)
        .copyFromZoneSizing(state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum));

    state->dataWaterCoils->MySizeFlag(1) = true;
    // run water coil sizing
    SizeWaterCoil(*state, CoilNum);

    // check coil UA-value sizing
    EXPECT_NEAR(577.686, state->dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01); // smaller UA than result above at 1435.00
}

TEST_F(WaterCoilsTest, CoilHeatingWaterLowAirFlowUASizing)
{
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "HotWaterLoop";
    state->dataSize->PlantSizData(1).ExitTemp = 60.0;
    state->dataSize->PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {

        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).HeatSupTemp = 40.0;
    state->dataSize->FinalSysSizing(1).HeatOutTemp = 5.0;
    state->dataSize->FinalSysSizing(1).HeatRetTemp = 20.0;
    state->dataSize->FinalSysSizing(1).HeatOAOption = DataSizing::OAControl::AllOA;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::HeatingSimple;
    state->dataWaterCoils->WaterCoil(CoilNum).SchedPtr = ScheduleManager::ScheduleAlwaysOn;

    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state->dataWaterCoils->UAandFlow;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    state->dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state->dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state->dataWaterCoils->MySizeFlag.allocate(1);
    state->dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state->dataEnvrn->StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (10.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);

    // check coil UA-value sizing
    EXPECT_NEAR(1435.01, state->dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01);

    // test single zone VAV reheat coil sizing
    state->dataSize->CurZoneEqNum = 1;
    state->dataSize->CurSysNum = 0;
    state->dataSize->CurTermUnitSizingNum = 1;
    state->dataSize->TermUnitSizing.allocate(1);
    state->dataSize->TermUnitFinalZoneSizing.allocate(1);
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).AirVolFlow =
        state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate / 1500.0; // DesAirVolFlowRate = 1.0 so TU air flow = 0.00067 (lower than 0.001)
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).MaxHWVolFlow =
        state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate / 1500.0;
    state->dataSize->TermUnitSizing(state->dataSize->CurTermUnitSizingNum).MinFlowFrac = 0.5;
    state->dataSize->TermUnitSingDuct = true;

    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state->dataWaterCoils->UAandFlow;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state->dataSize->SysSizingRunDone = false;
    state->dataSize->ZoneSizingRunDone = true;
    state->dataSize->NumZoneSizingInput = 1;
    state->dataSize->ZoneSizingInput.allocate(1);
    state->dataSize->ZoneSizingInput(1).ZoneNum = 1;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod.allocate(20);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod(DataHVACGlobals::HeatingAirflowSizing) =
        DataHVACGlobals::HeatingAirflowSizing;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingAirVolFlow = 0.0;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).HeatingAirVolFlow = 1.0;
    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesCoolVolFlow = 0.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatVolFlow = 0.00095;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatCoilInTempTU = 10.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).ZoneTempAtHeatPeak = 21.0;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatCoilInHumRatTU = 0.006;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).ZoneHumRatAtHeatPeak = 0.008;
    state->dataSize->TermUnitFinalZoneSizing(state->dataSize->CurTermUnitSizingNum)
        .copyFromZoneSizing(state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum));

    state->dataWaterCoils->MySizeFlag(1) = true;
    // run water coil sizing
    SizeWaterCoil(*state, CoilNum);

    // water flow rate should be non-zero, and air flow rate being so small will get set to 0 during sizing
    EXPECT_GT(state->dataWaterCoils->WaterCoil(CoilNum).InletWaterMassFlowRate, 0.0);
    EXPECT_EQ(0.0, state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate);
    // check coil UA-value sizing
    EXPECT_NEAR(1.0, state->dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.0001); // TU air flow is too low to size, set to 0, so UA is set to 1.0
}

TEST_F(WaterCoilsTest, CoilHeatingWaterUASizingLowHwaterInletTemp)
{
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "HotWaterLoop";
    state->dataSize->PlantSizData(1).ExitTemp = 40.0; // low heating coil inlet water temp
    state->dataSize->PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {

        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).HeatSupTemp = 40.0;
    state->dataSize->FinalSysSizing(1).HeatOutTemp = 5.0;
    state->dataSize->FinalSysSizing(1).HeatRetTemp = 20.0;
    state->dataSize->FinalSysSizing(1).HeatOAOption = DataSizing::OAControl::AllOA;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::HeatingSimple;
    state->dataWaterCoils->WaterCoil(CoilNum).SchedPtr = ScheduleManager::ScheduleAlwaysOn;

    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state->dataWaterCoils->UAandFlow;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;

    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    state->dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state->dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state->dataWaterCoils->MySizeFlag.allocate(1);
    state->dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state->dataEnvrn->StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (10.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);

    // check coil UA-value sizing for low design loop exit temp
    EXPECT_NEAR(2479.27, state->dataWaterCoils->WaterCoil(CoilNum).UACoil, 0.01);

    Real64 DesCoilInletWaterTempUsed = 0.0;
    Real64 DataFanOpMode = ContFanCycCoil;
    Real64 UAMax = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate;

    // check if coil design inlet water temperature is increased above the plant loop exit temp
    EstimateCoilInletWaterTemp(*state, CoilNum, DataFanOpMode, 1.0, UAMax, DesCoilInletWaterTempUsed);
    EXPECT_GT(DesCoilInletWaterTempUsed, state->dataSize->PlantSizData(1).ExitTemp);
    EXPECT_NEAR(48.73, DesCoilInletWaterTempUsed, 0.01);
}

TEST_F(WaterCoilsTest, CoilCoolingWaterSimpleSizing)
{
    InitializePsychRoutines(*state);
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);
    ShowMessage(*state, "Begin Test: state->dataWaterCoils->WaterCoilsTest, CoilCoolingWaterSimpleSizing");

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {

        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "WaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    state->dataSize->FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    state->dataSize->FinalSysSizing(1).CoolSupTemp = 10.0;
    state->dataSize->FinalSysSizing(1).CoolSupHumRat = 0.0085;
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).MassFlowAtCoolPeak = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Simple Water Cooling Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterCooling;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::CoolingSimple;

    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state->dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = 6.67;
    state->dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(1) = "Design Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->SysSizInput(1).CoolCapControl = DataSizing::CapacityControl::VAV;
    state->dataSize->PlantSizData(1).ExitTemp = 5.7;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;

    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    // run water coil sizing
    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 DesCoilCoolingLoad = 0.0;
    Real64 CoilInEnth = 0.0;
    Real64 CoilOutEnth = 0.0;

    CoilInEnth = PsyHFnTdbW(20.0, 0.01);
    CoilOutEnth = PsyHFnTdbW(10.0, 0.0085);
    DesCoilCoolingLoad = state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state->dataEnvrn->StdRhoAir * (CoilInEnth - CoilOutEnth);

    // check cooling coil design load
    EXPECT_DOUBLE_EQ(DesCoilCoolingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::CWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::CWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate /
                       (state->dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp * Cp * rho);

    // check cooling coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}

TEST_F(WaterCoilsTest, CoilCoolingWaterDetailedSizing)
{
    InitializePsychRoutines(*state);
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);
    ShowMessage(*state, "Begin Test: state->dataWaterCoils->WaterCoilsTest, CoilCoolingWaterDetailedSizing");

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {

        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "WaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    state->dataSize->FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    state->dataSize->FinalSysSizing(1).CoolSupTemp = 10.0;
    state->dataSize->FinalSysSizing(1).CoolSupHumRat = 0.0085;
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).MassFlowAtCoolPeak = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Detailed Water Cooling Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterCooling;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::CoolingDetailed;

    state->dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea = 6.23816;
    state->dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea = 6.20007018;
    state->dataWaterCoils->WaterCoil(CoilNum).FinSurfArea = 101.7158224;
    state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea = 0.810606367;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilDepth = 0.165097968;
    state->dataWaterCoils->WaterCoil(CoilNum).FinDiam = 0.43507152;
    state->dataWaterCoils->WaterCoil(CoilNum).FinThickness = 0.001499982;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam = 0.014449958;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam = 0.015879775;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeThermConductivity = 385.764854;
    state->dataWaterCoils->WaterCoil(CoilNum).FinThermConductivity = 203.882537;
    state->dataWaterCoils->WaterCoil(CoilNum).FinSpacing = 0.001814292;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing = 0.02589977;
    state->dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows = 6;
    state->dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow = 16;
    state->dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = 6.67;
    state->dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;

    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(1) = "Design Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->SysSizInput(1).CoolCapControl = DataSizing::CapacityControl::VAV;
    state->dataSize->PlantSizData(1).ExitTemp = 5.7;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;
    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    // run water coil sizing
    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 DesCoilCoolingLoad = 0.0;
    Real64 CoilInEnth = 0.0;
    Real64 CoilOutEnth = 0.0;

    CoilInEnth = PsyHFnTdbW(state->dataSize->FinalSysSizing(1).MixTempAtCoolPeak, state->dataSize->FinalSysSizing(1).MixHumRatAtCoolPeak);
    CoilOutEnth = PsyHFnTdbW(state->dataSize->FinalSysSizing(1).CoolSupTemp, state->dataSize->FinalSysSizing(1).CoolSupHumRat);
    DesCoilCoolingLoad = state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state->dataEnvrn->StdRhoAir * (CoilInEnth - CoilOutEnth);
    // check cooling coil design load
    EXPECT_DOUBLE_EQ(DesCoilCoolingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::CWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::CWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate / (6.67 * Cp * rho);
    // check cooling coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}

TEST_F(WaterCoilsTest, CoilCoolingWaterDetailed_WarningMath)
{
    InitializePsychRoutines(*state);
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "WaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).MixTempAtCoolPeak = 20.0;
    state->dataSize->FinalSysSizing(1).MixHumRatAtCoolPeak = 0.01;
    state->dataSize->FinalSysSizing(1).CoolSupTemp = 10.0;
    state->dataSize->FinalSysSizing(1).CoolSupHumRat = 0.0085;
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).MassFlowAtCoolPeak = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Detailed Water Cooling Coil";

    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterDetailedFlatCooling;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::CoolingDetailed;

    state->dataWaterCoils->WaterCoil(CoilNum).TubeOutsideSurfArea = 6.23816;
    state->dataWaterCoils->WaterCoil(CoilNum).TotTubeInsideArea = 6.20007018;
    state->dataWaterCoils->WaterCoil(CoilNum).FinSurfArea = 101.7158224;
    state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea = 0.810606367;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilDepth = 0.165097968;
    state->dataWaterCoils->WaterCoil(CoilNum).FinDiam = 0.43507152;
    state->dataWaterCoils->WaterCoil(CoilNum).FinThickness = 0.001499982;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeInsideDiam = 0.014449958;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeOutsideDiam = 0.015879775;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeThermConductivity = 385.764854;
    state->dataWaterCoils->WaterCoil(CoilNum).FinThermConductivity = 203.882537;
    state->dataWaterCoils->WaterCoil(CoilNum).FinSpacing = 0.001814292;
    state->dataWaterCoils->WaterCoil(CoilNum).TubeDepthSpacing = 0.02589977;
    state->dataWaterCoils->WaterCoil(CoilNum).NumOfTubeRows = 6;
    state->dataWaterCoils->WaterCoil(CoilNum).NumOfTubesPerRow = 16;
    state->dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = 6.67;
    state->dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;

    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(1) = "Design Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterOutletNodeNum = 2;
    state->dataWaterCoils->WaterCoil(CoilNum).AirInletNodeNum = 3;
    state->dataWaterCoils->WaterCoil(CoilNum).AirOutletNodeNum = 4;
    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.branchNum = 1;
    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.compNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Name = state->dataWaterCoils->WaterCoil(1).Name;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::CoilWaterDetailedFlatCooling;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(1).WaterInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumOut =
        state->dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->SysSizInput(1).CoolCapControl = DataSizing::CapacityControl::VAV;
    state->dataSize->PlantSizData(1).ExitTemp = 5.7;
    state->dataSize->PlantSizData(1).DeltaT = 5.0;
    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    OutputReportPredefined::SetPredefinedTables(*state);

    // run water coil sizing
    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 DesCoilCoolingLoad = 0.0;
    Real64 CoilInEnth = 0.0;
    Real64 CoilOutEnth = 0.0;

    CoilInEnth = PsyHFnTdbW(state->dataSize->FinalSysSizing(1).MixTempAtCoolPeak, state->dataSize->FinalSysSizing(1).MixHumRatAtCoolPeak);
    CoilOutEnth = PsyHFnTdbW(state->dataSize->FinalSysSizing(1).CoolSupTemp, state->dataSize->FinalSysSizing(1).CoolSupHumRat);
    DesCoilCoolingLoad = state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state->dataEnvrn->StdRhoAir * (CoilInEnth - CoilOutEnth);
    // check cooling coil design load
    EXPECT_DOUBLE_EQ(DesCoilCoolingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::CWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::CWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterCoolingCoilRate / (6.67 * Cp * rho);
    // check cooling coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);

    state->dataLoopNodes->Node.allocate(10);

    InitWaterCoil(*state, 1, false);

    // Force getting the warning
    Real64 PartLoadRatio = 1.0;
    Real64 TempAirIn = state->dataWaterCoils->WaterCoil(CoilNum).InletAirTemp;
    Real64 InletAirHumRat = state->dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
    Real64 TempWaterIn = state->dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp;
    Real64 AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, TempAirIn, InletAirHumRat, "RoutineName");
    Real64 MinAirMassFlow = 5.0 * state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity;
    state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate = 1.1 * MinAirMassFlow;
    Real64 AirMassFlow = state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;

    EXPECT_EQ(0.81060636699999999, state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea);
    std::string expected_error = delimited_string({
        "   ** Warning ** Coil:Cooling:Water:DetailedGeometry in Coil =Test Detailed Water Cooling Coil",
        "   **   ~~~   ** Air Flow Rate Velocity has greatly exceeded upper design guidelines of ~2.5 m/s",
        format("   **   ~~~   ** Air Mass Flow Rate[kg/s]={:.6T}", state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate),
        format("   **   ~~~   ** Air Face Velocity[m/s]={:.6T}",
               AirMassFlow / (state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity)),
        format("   **   ~~~   ** Approximate Mass Flow Rate limit for Face Area[kg/s]={:.6T}",
               2.5 * state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity),
        "   **   ~~~   ** Coil:Cooling:Water:DetailedGeometry could be resized/autosized to handle capacity",
    });

    CalcDetailFlatFinCoolingCoil(*state, CoilNum, 2, 2, 1);

    compare_err_stream(expected_error, true);

    AirMassFlow = state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;
    TempAirIn = state->dataWaterCoils->WaterCoil(CoilNum).InletAirTemp;
    InletAirHumRat = state->dataWaterCoils->WaterCoil(CoilNum).InletAirHumRat;
    TempWaterIn = state->dataWaterCoils->WaterCoil(CoilNum).InletWaterTemp;
    AirDensity = Psychrometrics::PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, TempAirIn, InletAirHumRat, "RoutineName");
    MinAirMassFlow = 44.7 * state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity;
    state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate = 1.1 * MinAirMassFlow;
    AirMassFlow = state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate / PartLoadRatio;

    std::string expected_fatal_error = delimited_string({
        "   ** Severe  ** Coil:Cooling:Water:DetailedGeometry in Coil =Test Detailed Water Cooling Coil",
        "   **   ~~~   ** Air Flow Rate Velocity is > 100MPH (44.7m/s) and simulation cannot continue",
        format("   **   ~~~   ** Air Mass Flow Rate[kg/s]={:.6T}", state->dataWaterCoils->WaterCoil(CoilNum).InletAirMassFlowRate),
        format("   **   ~~~   ** Air Face Velocity[m/s]={:.6T}",
               AirMassFlow / (state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity)),
        format("   **   ~~~   ** Approximate Mass Flow Rate limit for Face Area[kg/s]={:.6T}",
               44.7 * state->dataWaterCoils->WaterCoil(CoilNum).MinAirFlowArea * AirDensity),
        "   **  Fatal  ** Coil:Cooling:Water:DetailedGeometry needs to be resized/autosized to handle capacity",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=1",
        "   ..... Last severe error=Coil:Cooling:Water:DetailedGeometry in Coil =Test Detailed Water Cooling Coil",
    });

    EXPECT_ANY_THROW(CalcDetailFlatFinCoolingCoil(*state, CoilNum, 2, 2, 1));

    compare_err_stream(expected_fatal_error, true);
}

TEST_F(WaterCoilsTest, CoilHeatingWaterSimpleSizing)
{
    InitializePsychRoutines(*state);
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);
    ShowMessage(*state, "Begin Test: state->dataWaterCoils->WaterCoilsTest, CoilHeatingWaterSimpleSizing");

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "WaterLoop";

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "WaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).HeatSupTemp = 40.0;
    state->dataSize->FinalSysSizing(1).HeatOutTemp = 5.0;
    state->dataSize->FinalSysSizing(1).HeatRetTemp = 20.0;
    state->dataSize->FinalSysSizing(1).HeatOAOption = DataSizing::OAControl::AllOA;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Test Simple Water Heating Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::HeatingSimple;
    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;

    state->dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp = 11.0;
    state->dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp = true;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;

    state->dataSize->PlantSizData(1).ExitTemp = 60.0;
    state->dataSize->PlantSizData(1).DeltaT = 10.0;

    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    // run water coil sizing
    createCoilSelectionReportObj(*state);
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd = 0.0;
    Real64 DesMassFlow = 0.0;
    Real64 DesCoilHeatingLoad = 0.0;

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state->dataSize->FinalSysSizing(1).DesMainVolFlow * state->dataEnvrn->StdRhoAir;
    DesCoilHeatingLoad = CpAirStd * DesMassFlow * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp = 0;
    Real64 rho = 0;
    Real64 DesWaterFlowRate = 0;

    Cp = GetSpecificHeatGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(
        *state, state->dataPlnt->PlantLoop(1).FluidName, Constant::HWInitConvTemp, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate / (11.0 * Cp * rho);

    // check heating coil design water flow rate
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}
TEST_F(WaterCoilsTest, HotWaterHeatingCoilAutoSizeTempTest)
{
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->OutBaroPress, 20.0, 0.0);

    // set up sizing flags
    state->dataSize->SysSizingRunDone = true;

    // set up plant sizing
    state->dataSize->NumPltSizInput = 1;
    state->dataSize->PlantSizData(1).PlantLoopName = "HotWaterLoop";
    state->dataSize->PlantSizData(1).ExitTemp = 60.0; // hot water coil inlet water temp
    state->dataSize->PlantSizData(1).DeltaT = 10.0;

    // set up plant loop
    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {
        auto &loopside(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }
    state->dataPlnt->PlantLoop(1).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "FluidWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;

    // set up sizing data
    state->dataSize->FinalSysSizing(1).DesMainVolFlow = 1.00;
    state->dataSize->FinalSysSizing(1).HeatSupTemp = 40.0;
    state->dataSize->FinalSysSizing(1).HeatOutTemp = 5.0;
    state->dataSize->FinalSysSizing(1).HeatRetTemp = 20.0;
    state->dataSize->FinalSysSizing(1).HeatOAOption = DataSizing::OAControl::AllOA;

    // set up water coil
    int CoilNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).Name = "Water Heating Coil";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilModel = WaterCoils::CoilModel::HeatingSimple;
    state->dataWaterCoils->WaterCoil(CoilNum).WaterCoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataWaterCoils->WaterCoil(CoilNum).SchedPtr = ScheduleManager::ScheduleAlwaysOn;

    state->dataWaterCoils->WaterCoil(CoilNum).RequestingAutoSize = true;
    state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).UACoil = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).CoilPerfInpMeth = state->dataWaterCoils->UAandFlow;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletWaterTemp = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesInletAirHumRat = AutoSize;
    state->dataWaterCoils->WaterCoil(CoilNum).DesOutletAirHumRat = AutoSize;

    state->dataWaterCoils->WaterCoilNumericFields(CoilNum).FieldNames(2) = "Maximum Water Flow Rate";
    state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum = 1;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(CoilNum).WaterInletNodeNum;

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->CurOASysNum = 0;

    state->dataSize->DataWaterLoopNum = 1;
    state->dataFluidProps->NumOfGlycols = 1;

    state->dataWaterCoils->MyUAAndFlowCalcFlag.allocate(1);
    state->dataWaterCoils->MyUAAndFlowCalcFlag(1) = true;

    state->dataWaterCoils->MySizeFlag.allocate(1);
    state->dataWaterCoils->MySizeFlag(1) = true;

    // run water coil sizing
    SizeWaterCoil(*state, CoilNum);
    EXPECT_DOUBLE_EQ(1.0, state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate);

    Real64 CpAirStd(0.0);
    Real64 DesMassFlow(0.0);
    Real64 DesCoilHeatingLoad(0.0);

    CpAirStd = PsyCpAirFnW(0.0);
    DesMassFlow = state->dataWaterCoils->WaterCoil(CoilNum).DesAirVolFlowRate * state->dataEnvrn->StdRhoAir;
    DesCoilHeatingLoad = DesMassFlow * CpAirStd * (40.0 - 5.0);

    // check heating coil design load
    EXPECT_DOUBLE_EQ(DesCoilHeatingLoad, state->dataWaterCoils->WaterCoil(CoilNum).DesWaterHeatingCoilRate);

    Real64 Cp(0.0);
    Real64 rho(0.0);
    Real64 DesWaterFlowRate(0.0);

    // now size heating coil hot water flow rate at 60.0C
    Cp = GetSpecificHeatGlycol(*state, state->dataPlnt->PlantLoop(1).FluidName, 60.0, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    rho = GetDensityGlycol(*state, state->dataPlnt->PlantLoop(1).FluidName, 60.0, state->dataPlnt->PlantLoop(1).FluidIndex, "Unit Test");
    DesWaterFlowRate = DesCoilHeatingLoad / (state->dataSize->PlantSizData(1).DeltaT * Cp * rho);

    // check heating coil design water flow rate calculated here and sizing results are identical
    EXPECT_DOUBLE_EQ(DesWaterFlowRate, state->dataWaterCoils->WaterCoil(CoilNum).MaxWaterVolFlowRate);
}

TEST_F(WaterCoilsTest, FanCoilCoolingWaterFlowTest)
{
    state->dataSize->PlantSizData.allocate(2);
    state->dataSize->NumPltSizInput = 2;

    state->dataSize->PlantSizData(2).PlantLoopName = "ChilledWaterLoop";
    state->dataSize->PlantSizData(2).ExitTemp = 7.22; // chilled water coil inlet water temp
    state->dataSize->PlantSizData(2).DeltaT = 6.67;
    state->dataSize->PlantSizData(1).PlantLoopName = "HotWaterLoop";
    state->dataSize->PlantSizData(1).ExitTemp = 60; // hot water coil inlet water temp
    state->dataSize->PlantSizData(1).DeltaT = 12;
    int FanCoilNum(1);
    int ZoneNum(1);
    bool FirstHVACIteration(true);
    bool ErrorsFound(false);
    Real64 QZnReq(0.0);
    Real64 HotWaterMassFlowRate(0.0);
    Real64 ColdWaterMassFlowRate(0.0);
    Real64 QUnitOut(0.0);
    Real64 AirMassFlow(0.0);
    Real64 MaxAirMassFlow(0.0);
    Real64 LatOutputProvided(0.0);

    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.20;
    state->dataWaterCoils->GetWaterCoilsInputFlag = true;
    state->dataGlobalNames->NumCoils = 0;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    InitializePsychRoutines(*state);

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

    GetZoneData(*state, ErrorsFound);
    EXPECT_EQ("EAST ZONE", state->dataHeatBal->Zone(1).Name);

    GetZoneEquipmentData(*state);
    ProcessScheduleInput(*state);
    state->dataScheduleMgr->ScheduleInputProcessed = true;
    GetFanInput(*state);
    EXPECT_EQ(DataHVACGlobals::FanType_SimpleOnOff, state->dataFans->Fan(1).FanType_Num);

    GetFanCoilUnits(*state);
    EXPECT_TRUE(compare_enums(CCM::ConsFanVarFlow, state->dataFanCoilUnits->FanCoil(1).CapCtrlMeth_Num));
    EXPECT_EQ("OUTDOORAIR:MIXER", state->dataFanCoilUnits->FanCoil(1).OAMixType);
    EXPECT_EQ("FAN:ONOFF", state->dataFanCoilUnits->FanCoil(1).FanType);
    EXPECT_EQ("COIL:COOLING:WATER", state->dataFanCoilUnits->FanCoil(1).CCoilType);
    EXPECT_EQ("COIL:HEATING:WATER", state->dataFanCoilUnits->FanCoil(1).HCoilType);

    state->dataPlnt->TotNumLoops = 2;
    state->dataPlnt->PlantLoop.allocate(state->dataPlnt->TotNumLoops);

    AirMassFlow = 0.60;
    MaxAirMassFlow = 0.60;

    // cooling load only
    HotWaterMassFlowRate = 0.0;
    ColdWaterMassFlowRate = 0.14;

    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).RetNode).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).RetNode).MassFlowRateMax = MaxAirMassFlow;

    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).RetNode).Temp = 24.0;
    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).RetNode).Enthalpy = 36000;
    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).RetNode).HumRat =
        PsyWFnTdbH(*state,
                   state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).RetNode).Temp,
                   state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).RetNode).Enthalpy);

    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).InletNode).Temp = 30.0;
    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).InletNode).Enthalpy = 53000;
    state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).InletNode).HumRat =
        PsyWFnTdbH(*state,
                   state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).InletNode).Temp,
                   state->dataLoopNodes->Node(state->dataMixedAir->OAMixer(1).InletNode).Enthalpy);

    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).AirInNode).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).AirInNode).MassFlowRateMin = AirMassFlow;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).AirInNode).MassFlowRateMinAvail = AirMassFlow;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).AirInNode).MassFlowRateMax = MaxAirMassFlow;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).AirInNode).MassFlowRateMaxAvail = MaxAirMassFlow;

    state->dataFanCoilUnits->FanCoil(1).OutAirMassFlow = 0.0;
    state->dataFanCoilUnits->FanCoil(1).MaxAirMassFlow = MaxAirMassFlow;
    state->dataFanCoilUnits->FanCoil(1).MaxCoolCoilFluidFlow = 0.14;
    state->dataFanCoilUnits->FanCoil(1).MaxHeatCoilFluidFlow = 0.14;

    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).OutsideAirNode).MassFlowRateMax = 0.0;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMax = 0.14;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMax = 0.14;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).CoolCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;
    state->dataLoopNodes->Node(state->dataFanCoilUnits->FanCoil(1).HeatCoilFluidInletNode).MassFlowRateMaxAvail = 0.14;

    state->dataFans->Fan(1).InletAirMassFlowRate = AirMassFlow;
    state->dataFans->Fan(1).MaxAirMassFlowRate = MaxAirMassFlow;

    state->dataLoopNodes->Node(state->dataFans->Fan(1).InletNodeNum).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(state->dataFans->Fan(1).InletNodeNum).MassFlowRateMin = AirMassFlow;
    state->dataLoopNodes->Node(state->dataFans->Fan(1).InletNodeNum).MassFlowRateMax = AirMassFlow;
    state->dataLoopNodes->Node(state->dataFans->Fan(1).InletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    state->dataWaterCoils->WaterCoil(2).UACoilTotal = 470.0;
    state->dataWaterCoils->WaterCoil(2).UACoilExternal = 611.0;
    state->dataWaterCoils->WaterCoil(2).UACoilInternal = 2010.0;
    state->dataWaterCoils->WaterCoil(2).TotCoilOutsideSurfArea = 50.0;

    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRateMin = AirMassFlow;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRateMax = AirMassFlow;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    state->dataWaterCoils->WaterCoil(2).InletWaterMassFlowRate = ColdWaterMassFlowRate;
    state->dataWaterCoils->WaterCoil(2).MaxWaterMassFlowRate = ColdWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).WaterInletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).WaterInletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).WaterInletNodeNum).Temp = 6.0;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).WaterOutletNodeNum).MassFlowRate = ColdWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(2).WaterOutletNodeNum).MassFlowRateMaxAvail = ColdWaterMassFlowRate;

    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRate = AirMassFlow;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(1).AirInletNodeNum).MassFlowRateMaxAvail = AirMassFlow;

    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(1).WaterInletNodeNum).Temp = 60.0;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(1).WaterInletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRate = HotWaterMassFlowRate;
    state->dataLoopNodes->Node(state->dataWaterCoils->WaterCoil(1).WaterOutletNodeNum).MassFlowRateMaxAvail = HotWaterMassFlowRate;
    state->dataWaterCoils->WaterCoil(1).InletWaterMassFlowRate = HotWaterMassFlowRate;
    state->dataWaterCoils->WaterCoil(1).MaxWaterMassFlowRate = HotWaterMassFlowRate;

    for (int l = 1; l <= state->dataPlnt->TotNumLoops; ++l) {

        auto &loopside(state->dataPlnt->PlantLoop(l).LoopSide(DataPlant::LoopSideLocation::Demand));
        loopside.TotalBranches = 1;
        loopside.Branch.allocate(1);
        auto &loopsidebranch(state->dataPlnt->PlantLoop(l).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1));
        loopsidebranch.TotalComponents = 1;
        loopsidebranch.Comp.allocate(1);
    }

    state->dataHeatBalFanSys->TempControlType.allocate(1);
    state->dataHeatBalFanSys->TempControlType(1) = DataHVACGlobals::ThermostatType::DualSetPointWithDeadBand;

    state->dataWaterCoils->WaterCoil(2).WaterPlantLoc.loopNum = 1;
    state->dataWaterCoils->WaterCoil(2).WaterPlantLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
    state->dataWaterCoils->WaterCoil(2).WaterPlantLoc.branchNum = 1;
    state->dataWaterCoils->WaterCoil(2).WaterPlantLoc.compNum = 1;

    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.loopNum = 2;
    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.branchNum = 1;
    state->dataWaterCoils->WaterCoil(1).WaterPlantLoc.compNum = 1;

    state->dataPlnt->PlantLoop(2).Name = "ChilledWaterLoop";
    state->dataPlnt->PlantLoop(2).FluidName = "ChilledWater";
    state->dataPlnt->PlantLoop(2).FluidIndex = 1;
    state->dataPlnt->PlantLoop(2).FluidName = "WATER";
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Name = state->dataWaterCoils->WaterCoil(2).Name;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::CoilWaterCooling;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(2).WaterInletNodeNum;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumOut =
        state->dataWaterCoils->WaterCoil(2).WaterOutletNodeNum;
    state->dataPlnt->PlantLoop(2).LoopSide(DataPlant::LoopSideLocation::Demand).FlowLock = DataPlant::FlowLock::Unlocked;

    state->dataPlnt->PlantLoop(1).Name = "HotWaterLoop";
    state->dataPlnt->PlantLoop(1).FluidName = "HotWater";
    state->dataPlnt->PlantLoop(1).FluidIndex = 1;
    state->dataPlnt->PlantLoop(1).FluidName = "WATER";
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Name = state->dataWaterCoils->WaterCoil(1).Name;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).Type =
        DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumIn =
        state->dataWaterCoils->WaterCoil(1).WaterInletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).Branch(1).Comp(1).NodeNumOut =
        state->dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;
    state->dataPlnt->PlantLoop(1).LoopSide(DataPlant::LoopSideLocation::Demand).FlowLock = DataPlant::FlowLock::Unlocked;

    state->dataFanCoilUnits->FanCoil(1).CoolCoilPlantLoc.loopNum = 2;
    state->dataFanCoilUnits->FanCoil(1).HeatCoilPlantLoc.loopNum = 1;
    state->dataFanCoilUnits->FanCoil(1).CoolCoilPlantLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
    state->dataFanCoilUnits->FanCoil(1).HeatCoilPlantLoc.loopSideNum = DataPlant::LoopSideLocation::Demand;
    state->dataFanCoilUnits->FanCoil(1).HeatCoilFluidOutletNodeNum = state->dataWaterCoils->WaterCoil(1).WaterOutletNodeNum;
    state->dataFanCoilUnits->FanCoil(1).CoolCoilFluidOutletNodeNum = state->dataWaterCoils->WaterCoil(2).WaterOutletNodeNum;
    state->dataFanCoilUnits->FanCoil(1).CoolCoilPlantLoc.branchNum = 1;
    state->dataFanCoilUnits->FanCoil(1).CoolCoilPlantLoc.compNum = 1;
    state->dataFanCoilUnits->FanCoil(1).HeatCoilPlantLoc.branchNum = 1;
    state->dataFanCoilUnits->FanCoil(1).HeatCoilPlantLoc.compNum = 1;

    state->dataFanCoilUnits->HeatingLoad = false;
    state->dataFanCoilUnits->CoolingLoad = true;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand.allocate(1);
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputReqToCoolSP = -4000.00;
    state->dataZoneEnergyDemand->ZoneSysEnergyDemand(1).RemainingOutputReqToHeatSP = -8000.0;
    state->dataFanCoilUnits->FanCoil(1).SpeedFanSel = 2;
    QUnitOut = 0.0;
    QZnReq = -4000.0;

    state->dataGlobal->DoingSizing = true;

    state->dataSize->CurZoneEqNum = 1;
    state->dataSize->ZoneSizingRunDone = true;
    state->dataSize->ZoneEqSizing.allocate(state->dataSize->CurZoneEqNum);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod.allocate(25);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod(25) = 0;
    state->dataSize->ZoneEqFanCoil = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).DesignSizeFromParent = true;

    // User-specified air volume flow rate from the ZoneHVAC:FourPipeFanCoil object
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).AirVolFlow = 0.5;
    // User-specified water flow rate from the ZoneHVAC:FourPipeFanCoil object
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).MaxCWVolFlow = 0.0002;
    // User-specified water flow rate from the ZoneHVAC:FourPipeFanCoil object
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).MaxHWVolFlow = 0.0002;

    // Initial design air volume flow rate based on the design conditions
    state->dataWaterCoils->WaterCoil(2).DesAirVolFlowRate = DataSizing::AutoSize;
    // Initial design water flow rate based on the design conditions
    state->dataWaterCoils->WaterCoil(2).MaxWaterVolFlowRate = DataSizing::AutoSize;

    // normal cooling simulation for constant fan variable flow fan coil
    Sim4PipeFanCoil(*state, FanCoilNum, ZoneNum, FirstHVACIteration, QUnitOut, LatOutputProvided);

    // Expect final design air volume flow rate to equal the user-specified air volume flow rate from the ZoneHVAC:FourPipeFanCoil object
    EXPECT_EQ(state->dataWaterCoils->WaterCoil(2).DesAirVolFlowRate, 0.5);
    // Expect final design water flow rate to equal the user-specified water flow rate from the ZoneHVAC:FourPipeFanCoil object
    EXPECT_EQ(state->dataWaterCoils->WaterCoil(2).MaxWaterVolFlowRate, 0.0002);
}
