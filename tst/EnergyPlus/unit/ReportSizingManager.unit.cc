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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/SQLiteFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ReportSizingManager;

TEST_F(EnergyPlusFixture, ReportSizingManager_GetCoilDesFlowT)
{
    // setup global allocation
    DataSizing::SysSizInput.allocate(1);
    DataSizing::SysSizPeakDDNum.allocate(1);
    DataSizing::FinalSysSizing.allocate(1);
    DataSizing::CalcSysSizing.allocate(1);
    DataSizing::CalcSysSizing(1).SumZoneCoolLoadSeq.allocate(1);
    DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq.allocate(1);
    DataSizing::SysSizPeakDDNum(1).TimeStepAtSensCoolPk.allocate(1);
    DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.allocate(1);
    DataSizing::SysSizPeakDDNum(1).TimeStepAtTotCoolPk.allocate(1);

    // one-time global initialization
    int const DesignDayForPeak = 1;
    DataSizing::SysSizPeakDDNum(1).SensCoolPeakDD = DesignDayForPeak;
    DataSizing::SysSizPeakDDNum(1).CoolFlowPeakDD = DesignDayForPeak;
    DataSizing::SysSizPeakDDNum(1).TotCoolPeakDD = DesignDayForPeak;
    DataSizing::SysSizPeakDDNum(1).TimeStepAtSensCoolPk(DesignDayForPeak) = 1;
    DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk(DesignDayForPeak) = 1;
    DataSizing::SysSizPeakDDNum(1).TimeStepAtTotCoolPk(DesignDayForPeak) = 1;
    DataSizing::FinalSysSizing(1).CoolSupTemp = 10;
    DataSizing::FinalSysSizing(1).MassFlowAtCoolPeak = 2.0;
    DataSizing::FinalSysSizing(1).DesCoolVolFlow = 0.15;
    DataSizing::DataAirFlowUsedForSizing = 0.2;
    DataEnvironment::StdRhoAir = 1000;
    DataSizing::CalcSysSizing(1).SumZoneCoolLoadSeq(1) = 1250000;

    // one-time argument initialization
    int const sysNum = 1;
    Real64 const CpAir = 4179;

    // argument return values
    Real64 designFlowValue;
    Real64 designExitTemp;
    Real64 designExitHumRat;

    DataSizing::SysSizInput(1).CoolingPeakLoadType = DataSizing::TotalCoolingLoad;
    DataSizing::FinalSysSizing(1).CoolingPeakLoadType = DataSizing::TotalCoolingLoad;

    // Single path for VAV
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VAV;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(0.002, designFlowValue);

    // Single path for OnOff
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::OnOff;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(0.2, designFlowValue);

    // Two paths for VT:
    // CoolSupTemp > calculated value
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VT;
    DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 10;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);
    // CoolSupTemp < calculated value
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VT;
    DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 15;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_NEAR(13.00590, designExitTemp, 0.0001);
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);

    // Two paths for bypass:
    // MixTemp > DesExitTemp
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::Bypass;
    DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 13;
    DataSizing::CalcSysSizing(1).MixTempAtCoolPeak = 15;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(10, designExitTemp);
    EXPECT_NEAR(0.119823, designFlowValue, 0.0001);
    // MixTemp < DesExitTemp
    DataSizing::CalcSysSizing(1).MixTempAtCoolPeak = 5;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(10, designExitTemp);
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);

    // Oh and the sensible cases
    DataSizing::SysSizInput(1).CoolingPeakLoadType = DataSizing::SensibleCoolingLoad;
    DataSizing::FinalSysSizing(1).CoolingPeakLoadType = DataSizing::SensibleCoolingLoad;
    // Repeat a VT case
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VT;
    DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 10;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).DesCoolVolFlow, designFlowValue);
    // And a bypass case
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::Bypass;
    DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq(1) = 13;
    DataSizing::CalcSysSizing(1).MixTempAtCoolPeak = 15;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(10, designExitTemp);
    EXPECT_NEAR(0.119823, designFlowValue, 0.0001);
}
TEST_F(EnergyPlusFixture, ReportSizingManager_GetCoilDesFlowT_NoPeak)
{
    // setup global allocation
    DataSizing::SysSizInput.allocate(1);
    DataSizing::SysSizPeakDDNum.allocate(1);
    DataSizing::FinalSysSizing.allocate(1);
    DataSizing::CalcSysSizing.allocate(1);
    DataSizing::CalcSysSizing(1).SumZoneCoolLoadSeq.allocate(1);
    DataSizing::CalcSysSizing(1).CoolZoneAvgTempSeq.allocate(1);
    DataSizing::SysSizPeakDDNum(1).TimeStepAtSensCoolPk.allocate(1);
    DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.allocate(1);
    DataSizing::SysSizPeakDDNum(1).TimeStepAtTotCoolPk.allocate(1);

    // one-time global initialization
    int const DesignDayForPeak = 0;
    DataSizing::SysSizPeakDDNum(1).SensCoolPeakDD = DesignDayForPeak;
    DataSizing::SysSizPeakDDNum(1).CoolFlowPeakDD = DesignDayForPeak;
    DataSizing::SysSizPeakDDNum(1).TotCoolPeakDD = DesignDayForPeak;
    DataSizing::FinalSysSizing(1).CoolSupTemp = 10;
    DataSizing::FinalSysSizing(1).MassFlowAtCoolPeak = 2.0;
    DataSizing::FinalSysSizing(1).DesCoolVolFlow = 0.15;
    DataSizing::DataAirFlowUsedForSizing = 0.2;
    DataEnvironment::StdRhoAir = 1000;
    DataSizing::CalcSysSizing(1).SumZoneCoolLoadSeq(1) = 1250000;

    // one-time argument initialization
    int const sysNum = 1;
    Real64 const CpAir = 4179;

    // argument return values
    Real64 designFlowValue;
    Real64 designExitTemp;
    Real64 designExitHumRat;

    DataSizing::SysSizInput(1).CoolingPeakLoadType = DataSizing::TotalCoolingLoad;
    DataSizing::FinalSysSizing(1).CoolingPeakLoadType = DataSizing::TotalCoolingLoad;

    // Single path for VAV
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VAV;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(0.002, designFlowValue);

    // Single path for OnOff
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::OnOff;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    EXPECT_FALSE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(0.2, designFlowValue);

    // VT
    // CoolSupTemp > calculated value
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::VT;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    // Expect warning and same result as VAV
    EXPECT_TRUE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(0.002, designFlowValue);

    // Bypass
    DataSizing::SysSizInput(1).CoolCapControl = DataSizing::Bypass;
    ReportSizingManager::GetCoilDesFlowT(sysNum, CpAir, designFlowValue, designExitTemp, designExitHumRat);
    // Expect warning and same result as VAV
    EXPECT_TRUE(has_err_output(true));
    EXPECT_DOUBLE_EQ(DataSizing::FinalSysSizing(1).CoolSupTemp, designExitTemp);
    EXPECT_DOUBLE_EQ(0.002, designFlowValue);
}
TEST_F(EnergyPlusFixture, ReportSizingManager_RequestSizingSystem)
{

    std::string CompName;       // component name
    std::string CompType;       // component type
    std::string SizingString;   // input field sizing description
    int SizingType;             // integer type of sizing requested
    Real64 SizingResult;        // autosized value of coil input field
    bool PrintWarning;          // true when sizing information is reported in the eio file
    std::string CallingRoutine; // calling routine

    DataConstantUsedForSizing = 1.0;
    DataFractionUsedForSizing = 1.0;
    DataTotCapCurveIndex = 0;
    DataDesOutletAirTemp = 0.0;

    CurZoneEqNum = 0;
    CurOASysNum = 0;
    CurSysNum = 1;
    FinalSysSizing.allocate(1);
    FinalSysSizing(CurSysNum).CoolSupTemp = 12.0;
    FinalSysSizing(CurSysNum).CoolSupHumRat = 0.0085;
    FinalSysSizing(CurSysNum).MixTempAtCoolPeak = 28.0;
    FinalSysSizing(CurSysNum).MixHumRatAtCoolPeak = 0.0075;
    FinalSysSizing(CurSysNum).DesCoolVolFlow = 1.00;
    FinalSysSizing(CurSysNum).DesOutAirVolFlow = 0.2;

    PrimaryAirSystem.allocate(1);
    PrimaryAirSystem(CurSysNum).NumOACoolCoils = 0;
    PrimaryAirSystem(CurSysNum).SupFanNum = 0;
    PrimaryAirSystem(CurSysNum).RetFanNum = 0;

    SysSizingRunDone = true;
    SysSizInput.allocate(1);
    SysSizInput(1).AirLoopNum = CurSysNum;
    DataSizing::NumSysSizInput = 1;

    StdBaroPress = 101325.0;
    InitializePsychRoutines();

    DataFlowUsedForSizing = FinalSysSizing(CurSysNum).DesCoolVolFlow;
    // Need this to prevent crash in RequestSizing
    UnitarySysEqSizing.allocate(1);
    OASysEqSizing.allocate(1);

    CompType = "COIL:COOLING:DX:SINGLESPEED";
    CompName = "Single Speed DX Cooling Coil";
    SizingType = DataHVACGlobals::CoolingCapacitySizing;
    SizingString = "Nominal Capacity";
    SizingResult = DataSizing::AutoSize;
    PrintWarning = true;
    CallingRoutine = "RequestSizing";
    DataIsDXCoil = true;

    // dx cooling coil capacity sizing
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);
    EXPECT_NEAR(18882.0, SizingResult, 0.1);

    // confirm that sizing data is saved for use by parent object
    EXPECT_NEAR(DataSizing::DataCoilSizingAirInTemp, 28.0, 0.0000001);
    EXPECT_NEAR(DataSizing::DataCoilSizingAirInHumRat, 0.0075, 0.0000001);
    EXPECT_NEAR(DataSizing::DataCoilSizingAirOutTemp,12.0, 0.0000001);
    EXPECT_NEAR(DataSizing::DataCoilSizingAirOutHumRat, 0.0075, 0.0000001);
    EXPECT_NEAR(DataSizing::DataCoilSizingFanCoolLoad, 0.0, 0.0000001);
    EXPECT_NEAR(DataSizing::DataCoilSizingCapFT, 1.0, 0.0000001);

    CompType = "COIL:COOLING:WATER";
    CompName = "Chilled Water Cooling Coil";
    SizingResult = DataSizing::AutoSize;
    DataEnvironment::StdRhoAir = 1.18;
    DataIsDXCoil = false;

    // chilled water cooling coil capacity sizing
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);
    EXPECT_NEAR(19234.6, SizingResult, 0.1);
}

TEST_F(EnergyPlusFixture, ReportSizingManager_RequestSizingSystemWithFans)
{

    // Add three fans to this model - one Fan:ConstantVolume, and three Fan:SystemModel in order to make the SupFanIndex=2
    std::string const idf_objects = delimited_string({

        "  Fan:SystemModel,",
        "    Test Fan 1 ,                   !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TestFanAirInletNode,         !- Air Inlet Node Name",
        "    TestFanOutletNode,           !- Air Outlet Node Name",
        "    1.0 ,                        !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    50.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",
        "  ",
        "  Fan:SystemModel,",
        "    Test Fan 2 ,                   !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TestFan2AirInletNode,         !- Air Inlet Node Name",
        "    TestFan2OutletNode,           !- Air Outlet Node Name",
        "    1.0 ,                        !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    100.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",
        "  Fan:SystemModel,",
        "    Test Fan 3 ,                   !- Name",
        "    ,                            !- Availability Schedule Name",
        "    TestFan3AirInletNode,         !- Air Inlet Node Name",
        "    TestFan3OutletNode,           !- Air Outlet Node Name",
        "    1.0 ,                        !- Design Maximum Air Flow Rate",
        "    Discrete ,                   !- Speed Control Method",
        "    0.0,                         !- Electric Power Minimum Flow Rate Fraction",
        "    200.0,                       !- Design Pressure Rise",
        "    0.9 ,                        !- Motor Efficiency",
        "    1.0 ,                        !- Motor In Air Stream Fraction",
        "    AUTOSIZE,                    !- Design Electric Power Consumption",
        "    TotalEfficiencyAndPressure,  !- Design Power Sizing Method",
        "    ,                            !- Electric Power Per Unit Flow Rate",
        "    ,                            !- Electric Power Per Unit Flow Rate Per Unit Pressure",
        "    0.50;                        !- Fan Total Efficiency",
        "  Fan:ConstantVolume,",
        "    Test Fan 4,            !- Name",
        "    ,    !- Availability Schedule Name",
        "    0.5,                     !- Fan Total Efficiency",
        "    25.0,                   !- Pressure Rise {Pa}",
        "    1.0,                  !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    TestFan4AirInletNode,         !- Air Inlet Node Name",
        "    TestFan4OutletNode;           !- Air Outlet Node Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    std::string fanName = "TEST FAN 1";
    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(fanName)); // call constructor
    DataSizing::CurZoneEqNum = 0;
    DataSizing::CurSysNum = 0;
    DataSizing::CurOASysNum = 0;
    DataEnvironment::StdRhoAir = 1.2;
    HVACFan::fanObjs[0]->simulate(state, _, _, _, _);                         // triggers sizing call
    Real64 locFanSizeVdot = HVACFan::fanObjs[0]->designAirVolFlowRate; // get function
    Real64 locDesignHeatGain1 = HVACFan::fanObjs[0]->getFanDesignHeatGain(state, locFanSizeVdot);
    EXPECT_NEAR(locDesignHeatGain1, 100.0, 0.1);

    fanName = "TEST FAN 2";
    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(fanName)); // call constructor
    HVACFan::fanObjs[1]->simulate(state, _, _, _, _);                      // triggers sizing call
    locFanSizeVdot = HVACFan::fanObjs[1]->designAirVolFlowRate;     // get function
    Real64 locDesignHeatGain2 = HVACFan::fanObjs[1]->getFanDesignHeatGain(state, locFanSizeVdot);
    EXPECT_NEAR(locDesignHeatGain2, 200.0, 0.1);

    fanName = "TEST FAN 3";
    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(fanName)); // call constructor
    DataEnvironment::StdRhoAir = 1.2;
    HVACFan::fanObjs[2]->simulate(state, _, _, _, _);                  // triggers sizing call
    locFanSizeVdot = HVACFan::fanObjs[2]->designAirVolFlowRate; // get function
    Real64 locDesignHeatGain3 = HVACFan::fanObjs[2]->getFanDesignHeatGain(state, locFanSizeVdot);
    EXPECT_NEAR(locDesignHeatGain3, 400.0, 0.1);

    GetFanInput(state.fans);
    Real64 locDesignHeatGain4 = FanDesHeatGain(state, 1, locFanSizeVdot);
    EXPECT_NEAR(locDesignHeatGain4, 50.0, 0.1);

    std::string CompName;       // component name
    std::string CompType;       // component type
    std::string SizingString;   // input field sizing description
    int SizingType;             // integer type of sizing requested
    Real64 SizingResult;        // autosized value of coil input field
    bool PrintWarning;          // true when sizing information is reported in the eio file
    std::string CallingRoutine; // calling routine

    DataConstantUsedForSizing = 1.0;
    DataFractionUsedForSizing = 1.0;
    DataTotCapCurveIndex = 0;
    DataDesOutletAirTemp = 0.0;

    CurZoneEqNum = 0;
    CurOASysNum = 0;
    CurSysNum = 1;
    FinalSysSizing.allocate(1);
    FinalSysSizing(CurSysNum).CoolSupTemp = 12.0;
    FinalSysSizing(CurSysNum).CoolSupHumRat = 0.0085;
    FinalSysSizing(CurSysNum).MixTempAtCoolPeak = 28.0;
    FinalSysSizing(CurSysNum).MixHumRatAtCoolPeak = 0.0075;
    FinalSysSizing(CurSysNum).DesCoolVolFlow = 1.00;
    FinalSysSizing(CurSysNum).DesOutAirVolFlow = 0.2;

    PrimaryAirSystem.allocate(1);
    PrimaryAirSystem(CurSysNum).NumOACoolCoils = 0;
    PrimaryAirSystem(CurSysNum).SupFanNum = 0;
    PrimaryAirSystem(CurSysNum).RetFanNum = 0;
    PrimaryAirSystem(CurSysNum).supFanModelTypeEnum = DataAirSystems::fanModelTypeNotYetSet;

    SysSizingRunDone = true;
    SysSizInput.allocate(1);
    SysSizInput(1).AirLoopNum = CurSysNum;
    DataSizing::NumSysSizInput = 1;

    StdBaroPress = 101325.0;
    InitializePsychRoutines();

    DataFlowUsedForSizing = FinalSysSizing(CurSysNum).DesCoolVolFlow;
    // Need this to prevent crash in RequestSizing
    UnitarySysEqSizing.allocate(1);
    OASysEqSizing.allocate(1);

    // Without fan heat
    CompType = "COIL:COOLING:DX:SINGLESPEED";
    CompName = "Single Speed DX Cooling Coil";
    SizingType = DataHVACGlobals::CoolingCapacitySizing;
    SizingString = "Nominal Capacity";
    SizingResult = DataSizing::AutoSize;
    PrintWarning = true;
    CallingRoutine = "RequestSizing";
    DataIsDXCoil = true;

    // dx cooling coil capacity sizing
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);
    EXPECT_NEAR(18882.0, SizingResult, 0.1);
    Real64 dxCoilSizeNoFan = SizingResult;

    // With Test Fan 4 fan heat
    PrimaryAirSystem(CurSysNum).SupFanNum = 1;
    PrimaryAirSystem(CurSysNum).RetFanNum = 0;
    PrimaryAirSystem(CurSysNum).supFanModelTypeEnum = DataAirSystems::structArrayLegacyFanModels;
    CompType = "COIL:COOLING:DX:SINGLESPEED";
    CompName = "Single Speed DX Cooling Coil";
    SizingType = DataHVACGlobals::CoolingCapacitySizing;
    SizingString = "Nominal Capacity";
    SizingResult = DataSizing::AutoSize;
    PrintWarning = true;
    CallingRoutine = "RequestSizing";
    DataIsDXCoil = true;
    // Expect coil size to increase by fan heat for fan 4
    Real64 expectedDXCoilSize = dxCoilSizeNoFan + locDesignHeatGain4;

    // dx cooling coil capacity sizing
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);
    EXPECT_NEAR(expectedDXCoilSize, SizingResult, 0.1);

    // With Test Fan 3 fan heat - this fails before the #6126 fix in ReportSizingManager
    PrimaryAirSystem(CurSysNum).SupFanNum = 2;
    PrimaryAirSystem(CurSysNum).supFanVecIndex = 2;
    PrimaryAirSystem(CurSysNum).RetFanNum = 0;
    PrimaryAirSystem(CurSysNum).supFanModelTypeEnum = DataAirSystems::objectVectorOOFanSystemModel;
    CompType = "COIL:COOLING:DX:SINGLESPEED";
    CompName = "Single Speed DX Cooling Coil";
    SizingType = DataHVACGlobals::CoolingCapacitySizing;
    SizingString = "Nominal Capacity";
    SizingResult = DataSizing::AutoSize;
    PrintWarning = true;
    CallingRoutine = "RequestSizing";
    DataIsDXCoil = true;
    // Expect coil size to increase by fan heat for fan 3
    expectedDXCoilSize = dxCoilSizeNoFan + locDesignHeatGain3;

    // dx cooling coil capacity sizing
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);
    EXPECT_NEAR(expectedDXCoilSize, SizingResult, 0.1);
}

TEST_F(EnergyPlusFixture, ReportSizingManager_RequestSizingZone)
{
    int const ZoneNum = 1;
    std::string CompName;       // component name
    std::string CompType;       // component type
    std::string SizingString;   // input field sizing description
    int SizingType;             // integerized type of sizing requested
    Real64 SizingResult;        // autosized value of coil input field
    bool PrintWarning;          // true when sizing information is reported in the eio file
    std::string CallingRoutine; // calling routine

    DataConstantUsedForSizing = 1.0;
    DataFractionUsedForSizing = 1.0;
    DataTotCapCurveIndex = 0;
    DataDesOutletAirTemp = 0.0;

    CurZoneEqNum = 1;
    CurOASysNum = 0;
    CurSysNum = 0;
    FinalZoneSizing.allocate(1);
    FinalZoneSizing(CurZoneEqNum).CoolDesTemp = 12.0;
    FinalZoneSizing(CurZoneEqNum).CoolDesHumRat = 0.0085;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInTemp = 28.0;
    FinalZoneSizing(CurZoneEqNum).DesCoolCoilInHumRat = 0.0075;
    FinalZoneSizing(CurZoneEqNum).DesCoolOAFlowFrac = 0.2;
    FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.30;

    ZoneSizingRunDone = true;
    StdBaroPress = 101325.0;
    InitializePsychRoutines();

    // Need this to prevent crash in RequestSizing
    ZoneEqSizing.allocate(1);
    ZoneSizingInput.allocate(1);
    ZoneSizingInput(1).ZoneNum = ZoneNum;
    DataSizing::NumZoneSizingInput = 1;
    ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent = false;
    DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;

    CompType = "COIL:COOLING:DX:SINGLESPEED";
    CompName = "Single Speed DX Cooling Coil";
    SizingType = DataHVACGlobals::CoolingCapacitySizing;
    SizingString = "Nominal Capacity";
    SizingResult = DataSizing::AutoSize;
    PrintWarning = true;
    CallingRoutine = "RequestSizing";
    DataIsDXCoil = true;

    // dx cooling coil capacity sizing
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);
    EXPECT_NEAR(5664.6, SizingResult, 0.1);

    CompType = "COIL:COOLING:WATER";
    CompName = "Chilled Water Cooling Coil";
    SizingResult = DataSizing::AutoSize;
    DataEnvironment::StdRhoAir = 1.18;
    FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow * StdRhoAir;
    DataIsDXCoil = false;

    // chilled water cooling coil capacity sizing
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);
    EXPECT_NEAR(5770.4, SizingResult, 0.1);
}

TEST_F(SQLiteFixture, ReportSizingManager_SQLiteRecordReportSizingOutputTest)
{
    // issue #6112
    std::string CompName;
    std::string CompType;
    std::string VarDesc;
    std::string UsrDesc;
    Real64 VarValue;
    Real64 UsrValue;

    // input values
    CompType = "BOILER:HOTWATER";
    CompName = "RESIDENTIAL BOILER ELECTRIC";
    VarDesc = "Design Size Nominal Capacity [W]";
    VarValue = 105977.98934;
    UsrDesc = "User-Specified Nominal Capacity [W]";
    UsrValue = 26352.97405;
    // boiler hot water autosizing and userspecified nominal capacity reporting to SQLite output
    ReportSizingManager::ReportSizingOutput(CompType, CompName, VarDesc, VarValue, UsrDesc, UsrValue);
    // get the sqlite output
    // query the sqLite
    auto result = queryResult("SELECT * FROM ComponentSizes;", "ComponentSizes");
    EnergyPlus::sqlite->sqliteCommit();
    // check that there are two sizing result records
    ASSERT_EQ(2ul, result.size());
    std::vector<std::string> testResult0{"1", "BOILER:HOTWATER", "RESIDENTIAL BOILER ELECTRIC", "Design Size Nominal Capacity", "105977.98934", "W"};
    std::vector<std::string> testResult1{
        "2", "BOILER:HOTWATER", "RESIDENTIAL BOILER ELECTRIC", "User-Specified Nominal Capacity", "26352.97405", "W"};
    EXPECT_EQ(testResult0, result[0]);
    EXPECT_EQ(testResult1, result[1]);
}

TEST_F(EnergyPlusFixture, setOAFracForZoneEqSizing_Test)
{
    DataSizing::ZoneEqSizing.allocate(1);
    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).OAVolFlow = 0.34;
    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).ATMixerVolFlow = 0.0;
    DataEnvironment::StdRhoAir = 1.23;

    Real64 oaFrac = 0.0;
    Real64 DesMassFlow = 0.685;
    Real64 massFlowRate = DataEnvironment::StdRhoAir * DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).OAVolFlow;
    Real64 oaFrac_Test = massFlowRate / DesMassFlow;

    ZoneEqSizingData &zoneEqSizing = DataSizing::ZoneEqSizing(1);

    // ATMixer flow rate = 0 so oaFrac depends on ZoneEqSizing.OAVolFlow
    oaFrac = setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing);
    EXPECT_EQ(oaFrac, oaFrac_Test);

    zoneEqSizing.ATMixerVolFlow = 0.11;

    oaFrac = 0.0;
    massFlowRate = DataEnvironment::StdRhoAir * zoneEqSizing.ATMixerVolFlow;
    oaFrac_Test = massFlowRate / DesMassFlow;

    // ATMixer flow rate > 0 so oaFrac depends on ZoneEqSizing.ATMixerVolFlow
    oaFrac = setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing);
    EXPECT_EQ(oaFrac, oaFrac_Test);

    DesMassFlow = 0.0;
    oaFrac = 1.0;
    oaFrac_Test = 0.0;
    zoneEqSizing.OAVolFlow = 1.0;
    zoneEqSizing.ATMixerVolFlow = 1.0;
    // DesMassFlow = 0 so oaFrac = 0 regardless of OAVolFlow or ATMixerVolFlow
    oaFrac = setOAFracForZoneEqSizing(DesMassFlow, zoneEqSizing);
    EXPECT_EQ(oaFrac, oaFrac_Test);
}

TEST_F(EnergyPlusFixture, setZoneCoilInletConditions)
{
    DataSizing::ZoneEqSizing.allocate(1);
    DataSizing::CurZoneEqNum = 1;
    DataSizing::ZoneEqSizingData &zoneEqSizing = ZoneEqSizing(CurZoneEqNum);
    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::ZoneSizingData &finalZoneSizing = DataSizing::FinalZoneSizing(CurZoneEqNum);

    zoneEqSizing.OAVolFlow = 0.34;
    zoneEqSizing.ATMixerVolFlow = 0.0;
    DataEnvironment::StdRhoAir = 1.23;

    Real64 DesMassFlow = 0.685;
    Real64 massFlowRate = DataEnvironment::StdRhoAir * zoneEqSizing.OAVolFlow;
    Real64 oaFrac = massFlowRate / DesMassFlow;

    // Test heating mode coil inlet temperature
    zoneEqSizing.ATMixerHeatPriDryBulb = 22.0;
    finalZoneSizing.ZoneRetTempAtHeatPeak = 25.0;
    finalZoneSizing.ZoneTempAtHeatPeak = 20.0;
    finalZoneSizing.OutTempAtHeatPeak = 10.0;

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition = zone condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 0.0;
    Real64 zoneCond = finalZoneSizing.ZoneTempAtHeatPeak;
    Real64 calcCoilInletCond = zoneCond;
    Real64 coilInletCond = setHeatCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate > 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition based on mixed return and ATMixer condition
    zoneEqSizing.ATMixerVolFlow = 1.0;
    zoneEqSizing.OAVolFlow = 0.0;
    zoneCond = finalZoneSizing.ZoneRetTempAtHeatPeak;
    Real64 oaCond = zoneEqSizing.ATMixerHeatPriDryBulb;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setHeatCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow > 0 so coilInlet condition based on mixed return and OA condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 1.0;
    zoneCond = finalZoneSizing.ZoneTempAtHeatPeak;
    oaCond = finalZoneSizing.OutTempAtHeatPeak;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setHeatCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // Test heating mode coil inlet humidity ratio
    zoneEqSizing.ATMixerHeatPriDryBulb = 0.0;
    finalZoneSizing.ZoneRetTempAtHeatPeak = 0.0;
    finalZoneSizing.ZoneTempAtHeatPeak = 0.0;
    finalZoneSizing.OutTempAtHeatPeak = 0.0;

    zoneEqSizing.ATMixerHeatPriHumRat = 0.008;
    finalZoneSizing.ZoneHumRatAtHeatPeak = 0.01;
    finalZoneSizing.OutHumRatAtHeatPeak = 0.003;

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition = zone condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 0.0;
    zoneCond = finalZoneSizing.ZoneHumRatAtHeatPeak;
    calcCoilInletCond = zoneCond;
    coilInletCond = setHeatCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate > 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition based on mixed return and ATMixer condition
    zoneEqSizing.ATMixerVolFlow = 1.0;
    zoneEqSizing.OAVolFlow = 0.0;
    zoneCond = finalZoneSizing.ZoneHumRatAtHeatPeak;
    oaCond = zoneEqSizing.ATMixerHeatPriHumRat;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setHeatCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow > 0 so coilInlet condition based on mixed return and OA condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 1.0;
    zoneCond = finalZoneSizing.ZoneHumRatAtHeatPeak;
    oaCond = finalZoneSizing.OutHumRatAtHeatPeak;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setHeatCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // Test cooling mode coil inlet temperature
    zoneEqSizing.ATMixerHeatPriHumRat = 0.0;
    finalZoneSizing.ZoneHumRatAtHeatPeak = 0.0;
    finalZoneSizing.OutHumRatAtHeatPeak = 0.0;

    zoneEqSizing.ATMixerCoolPriDryBulb = 22.0;
    finalZoneSizing.ZoneRetTempAtCoolPeak = 25.0;
    finalZoneSizing.ZoneTempAtCoolPeak = 20.0;
    finalZoneSizing.OutTempAtCoolPeak = 10.0;

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition = zone condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 0.0;
    zoneCond = finalZoneSizing.ZoneTempAtCoolPeak;
    calcCoilInletCond = zoneCond;
    coilInletCond = setCoolCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate > 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition based on mixed return and ATMixer condition
    zoneEqSizing.ATMixerVolFlow = 1.0;
    zoneEqSizing.OAVolFlow = 0.0;
    zoneCond = finalZoneSizing.ZoneRetTempAtCoolPeak;
    oaCond = zoneEqSizing.ATMixerCoolPriDryBulb;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setCoolCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow > 0 so coilInlet condition based on mixed return and OA condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 1.0;
    zoneCond = finalZoneSizing.ZoneTempAtCoolPeak;
    oaCond = finalZoneSizing.OutTempAtCoolPeak;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setCoolCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // Test cooling mode coil inlet humidity ratio
    zoneEqSizing.ATMixerCoolPriDryBulb = 0.0;
    finalZoneSizing.ZoneRetTempAtCoolPeak = 0.0;
    finalZoneSizing.ZoneTempAtCoolPeak = 0.0;
    finalZoneSizing.OutTempAtCoolPeak = 0.0;

    zoneEqSizing.ATMixerCoolPriHumRat = 0.008;
    finalZoneSizing.ZoneHumRatAtCoolPeak = 0.01;
    finalZoneSizing.OutHumRatAtCoolPeak = 0.003;

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition = zone condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 0.0;
    zoneCond = finalZoneSizing.ZoneHumRatAtCoolPeak;
    calcCoilInletCond = zoneCond;
    coilInletCond = setCoolCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate > 0 and ZoneEqSizing.OAVolFlow = 0 so coilInlet condition based on mixed return and ATMixer condition
    zoneEqSizing.ATMixerVolFlow = 1.0;
    zoneEqSizing.OAVolFlow = 0.0;
    zoneCond = finalZoneSizing.ZoneHumRatAtCoolPeak;
    oaCond = zoneEqSizing.ATMixerCoolPriHumRat;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setCoolCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    // ATMixer flow rate = 0 and ZoneEqSizing.OAVolFlow > 0 so coilInlet condition based on mixed return and OA condition
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 1.0;
    zoneCond = finalZoneSizing.ZoneHumRatAtCoolPeak;
    oaCond = finalZoneSizing.OutHumRatAtCoolPeak;
    calcCoilInletCond = (oaFrac * oaCond) + ((1.0 - oaFrac) * zoneCond);
    coilInletCond = setCoolCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, calcCoilInletCond);

    zoneEqSizing.ATMixerCoolPriHumRat = 0.0;
    finalZoneSizing.ZoneHumRatAtCoolPeak = 0.0;
    finalZoneSizing.OutHumRatAtCoolPeak = 0.0;

    // all ZoneEqSizing and FinalZoneSizing temp and humrat variables have been zero'd out now, all results should = 0
    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 0.0;
    coilInletCond = setHeatCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setHeatCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setCoolCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setCoolCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);

    zoneEqSizing.ATMixerVolFlow = 1.0;
    zoneEqSizing.OAVolFlow = 0.0;
    coilInletCond = setHeatCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setHeatCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setCoolCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setCoolCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);

    zoneEqSizing.ATMixerVolFlow = 0.0;
    zoneEqSizing.OAVolFlow = 1.0;
    coilInletCond = setHeatCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setHeatCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setCoolCoilInletTempForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
    coilInletCond = setCoolCoilInletHumRatForZoneEqSizing(oaFrac, zoneEqSizing, finalZoneSizing);
    EXPECT_EQ(coilInletCond, 0.0);
}

// This tests checks that the Design Day + Peak Time is filled up for Fans
// https://github.com/NREL/EnergyPlus/issues/6899
TEST_F(EnergyPlusFixture, ReportSizingManager_FanPeak)
{

    // This is needed to compute time of Peak as a string
    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 15;

    // Setup the predefined tables, because that's where the info is written.
    EnergyPlus::OutputReportPredefined::SetPredefinedTables();

    // If you wanted to check SQL, you also need this:
    // We enable the report we care about, making sure it's the right one
    // EXPECT_EQ("EquipmentSummary", OutputReportPredefined::reportName(5).name);
    // OutputReportPredefined::reportName(5).show = true;

    int const ZoneNum = 1;
    std::string CompName;       // component name
    std::string CompType;       // component type
    std::string SizingString;   // input field sizing description
    int SizingType;             // integerized type of sizing requested
    Real64 SizingResult;        // autosized value of coil input field
    bool PrintWarning;          // true when sizing information is reported in the eio file
    std::string CallingRoutine; // calling routine

    CompType = "Fan:ConstantVolume";
    CompName = "My Fan";
    SizingType = DataHVACGlobals::SystemAirflowSizing;
    SizingString = "Nominal Capacity";
    SizingResult = DataSizing::AutoSize;
    PrintWarning = true;
    CallingRoutine = "Size Fan: ";
    DataSizing::DataIsDXCoil = false;

    DataSizing::CurZoneEqNum = 1;
    DataSizing::CurOASysNum = 0;
    DataSizing::CurSysNum = 0;
    DataSizing::FinalZoneSizing.allocate(1);
    DataSizing::FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow = 0.30;
    DataSizing::FinalZoneSizing(CurZoneEqNum).CoolDDNum = 1;
    // Time of peak, should equal to 18:00:00
    DataSizing::FinalZoneSizing(CurZoneEqNum).TimeStepNumAtCoolMax = 72;

    // Fake a design day
    WeatherManager::DesDayInput.allocate(1);
    std::string DDTitle = "CHICAGO ANN CLG 1% CONDNS DB=>MWB";
    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Title = DDTitle;
    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).Month = 7;
    WeatherManager::DesDayInput(FinalZoneSizing(CurZoneEqNum).CoolDDNum).DayOfMonth = 15;
    // Also need to set this, it's used to check if DDNum <= TotDesDays
    DataEnvironment::TotDesDays = 1;

    DataSizing::ZoneSizingRunDone = true;

    // Need this to prevent crash in RequestSizing
    ZoneEqSizing.allocate(1);
    ZoneSizingInput.allocate(1);
    ZoneSizingInput(1).ZoneNum = ZoneNum;
    DataSizing::NumZoneSizingInput = 1;
    ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent = false;
    ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(DataHVACGlobals::NumOfSizingTypes);
    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingType) = DataSizing::SupplyAirFlowRate;

    // Now, we're ready to call the function
    ReportSizingManager::RequestSizing(state, CompType, CompName, SizingType, SizingString, SizingResult, PrintWarning, CallingRoutine);

    // Check that the Design Day/Time is filled
    EXPECT_EQ(DDTitle, OutputReportPredefined::RetrievePreDefTableEntry(OutputReportPredefined::pdchFanDesDay, CompName));
    EXPECT_EQ("7/15 18:00:00", OutputReportPredefined::RetrievePreDefTableEntry(OutputReportPredefined::pdchFanPkTime, CompName));

    // Bonus test for #6949
    EXPECT_EQ("End Use Subcategory", OutputReportPredefined::columnTag(OutputReportPredefined::pdchFanEndUse).heading);
}
TEST_F(EnergyPlusFixture, ReportSizingManager_SupplyAirTempLessThanZoneTStatTest)
{
    // GitHub issue 7039
    std::string const idf_objects = delimited_string({

        "  Timestep,1;",

        "  Building,",
        "    Simple One Zone,         !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.04,                    !- Loads Convergence Tolerance Value",
        "    0.004,                   !- Temperature Convergence Tolerance Value {deltaC}",
        "    MinimalShadowing,        !- Solar Distribution",
        "    30,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  SimulationControl,",
        "    Yes,                     !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    No,                      !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "    Site:Location,",
        "      Phoenix Sky Harbor Intl Ap_AZ_USA,  !- Name",
        "      33.45,                   !- Latitude {deg}",
        "      -111.98,                 !- Longitude {deg}",
        "      -7,                      !- Time Zone {hr}",
        "      337;                     !- Elevation {m}",

        "    Site:GroundTemperature:BuildingSurface,20.83,20.81,20.88,20.96,21.03,23.32,23.68,23.74,23.75,21.42,21.09,20.9;",

        "    SizingPeriod:DesignDay,",
        "      Phoenix Sky Harbor Intl Ap Ann Clg .4% Condns DB=>MWB,  !- Name",
        "      7,                       !- Month",
        "      21,                      !- Day of Month",
        "      SummerDesignDay,         !- Day Type",
        "      43.4,                    !- Maximum Dry-Bulb Temperature {C}",
        "      12,                      !- Daily Dry-Bulb Temperature Range {deltaC}",
        "      DefaultMultipliers,      !- Dry-Bulb Temperature Range Modifier Type",
        "      ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "      Wetbulb,                 !- Humidity Condition Type",
        "      21.1,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "      ,                        !- Humidity Condition Day Schedule Name",
        "      ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "      ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "      ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "      97342,                   !- Barometric Pressure {Pa}",
        "      4.1,                     !- Wind Speed {m/s}",
        "      260,                     !- Wind Direction {deg}",
        "      No,                      !- Rain Indicator",
        "      No,                      !- Snow Indicator",
        "      No,                      !- Daylight Saving Time Indicator",
        "      ASHRAETau,               !- Solar Model Indicator",
        "      ,                        !- Beam Solar Day Schedule Name",
        "      ,                        !- Diffuse Solar Day Schedule Name",
        "      0.588,                   !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "      1.653;                   !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "  ",
        "    SizingPeriod:DesignDay,",
        "      Phoenix Sky Harbor Intl Ap Ann Htg 99.6% Condns DB,  !- Name",
        "      12,                      !- Month",
        "      21,                      !- Day of Month",
        "      WinterDesignDay,         !- Day Type",
        "      3.7,                     !- Maximum Dry-Bulb Temperature {C}",
        "      0,                       !- Daily Dry-Bulb Temperature Range {deltaC}",
        "      DefaultMultipliers,      !- Dry-Bulb Temperature Range Modifier Type",
        "      ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "      Wetbulb,                 !- Humidity Condition Type",
        "      3.7,                     !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "      ,                        !- Humidity Condition Day Schedule Name",
        "      ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "      ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "      ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "      97342,                   !- Barometric Pressure {Pa}",
        "      1.7,                     !- Wind Speed {m/s}",
        "      100,                     !- Wind Direction {deg}",
        "      No,                      !- Rain Indicator",
        "      No,                      !- Snow Indicator",
        "      No,                      !- Daylight Saving Time Indicator",
        "      ASHRAEClearSky,          !- Solar Model Indicator",
        "      ,                        !- Beam Solar Day Schedule Name",
        "      ,                        !- Diffuse Solar Day Schedule Name",
        "      ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "      ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "      0;                       !- Sky Clearness",

        "  Material:NoMass,",
        "    R13LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    2.290965,                !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    R31LAYER,                !- Name",
        "    Rough,                   !- Roughness",
        "    5.456,                   !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    C5 - 4 IN HW CONCRETE,   !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Construction,",
        "    R13WALL,                 !- Name",
        "    R13LAYER;                !- Outside Layer",

        "  Construction,",
        "    FLOOR,                   !- Name",
        "    C5 - 4 IN HW CONCRETE;   !- Outside Layer",

        "  Construction,",
        "    ROOF31,                  !- Name",
        "    R31LAYER;                !- Outside Layer",

        "  Zone,",
        "    ZONE ONE,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall001,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,            !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,                   !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,            !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;     !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,     !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,     !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,     !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,            !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;     !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,     !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,            !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,                   !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;            !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,   !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,   !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,   !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;   !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    ZONE ONE,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572, !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572, !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572, !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572; !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Zn001:Wall001:Win001,    !- Name",
        "    Window,                  !- Surface Type",
        "    SimpleWindowConstruct,   !- Construction Name",
        "    Zn001:Wall001,           !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.5000000,               !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0.548000,0,2.5000,       !- X,Y,Z ==> Vertex 1 {m}",
        "    0.548000,0,0.5000,       !- X,Y,Z ==> Vertex 2 {m}",
        "    5.548000,0,0.5000,       !- X,Y,Z ==> Vertex 3 {m}",
        "    5.548000,0,2.5000;       !- X,Y,Z ==> Vertex 4 {m}",

        "    Construction,",
        "      SimpleWindowConstruct,   !- Name",
        "      SimpleWindowTest;        !- Outside Layer",

        "    WindowMaterial:SimpleGlazingSystem,",
        "      SimpleWindowTest,        !- Name",
        "      0.600,                   !- U-Factor {W/m2-K}",
        "      0.700,                   !- Solar Heat Gain Coefficient",
        "      0.700;                   !- Visible Transmittance",

        "    Sizing:Zone,",
        "      ZONE ONE,                !- Zone or ZoneList Name",
        "      SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "      12.0,                    !- Zone Cooling Design Supply Air Temperature {C}",
        "      ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "      SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "      12.0,                    !- Zone Heating Design Supply Air Temperature {C}",
        "      ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "      0.0075,                  !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "      0.004,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "      SZ DSOA Zone One,        !- Design Specification Outdoor Air Object Name",
        "      0.0,                     !- Zone Heating Sizing Factor",
        "      0.0,                     !- Zone Cooling Sizing Factor",
        "      DesignDay,               !- Cooling Design Air Flow Method",
        "      0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "      ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "      ,                        !- Cooling Minimum Air Flow {m3/s}",
        "      ,                        !- Cooling Minimum Air Flow Fraction",
        "      DesignDay,               !- Heating Design Air Flow Method",
        "      0,                       !- Heating Design Air Flow Rate {m3/s}",
        "      ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "      ,                        !- Heating Maximum Air Flow {m3/s}",
        "      ;                        !- Heating Maximum Air Flow Fraction",

        "    DesignSpecification:OutdoorAir,",
        "      SZ DSOA Zone One,        !- Name",
        "      Sum,                     !- Outdoor Air Method",
        "      0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
        "      0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "      0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "    ZoneControl:Thermostat,",
        "      Zone Thermostat,         !- Name",
        "      ZONE ONE,                !- Zone or ZoneList Name",
        "      Zone Control Type Sched, !- Control Type Schedule Name",
        "      ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "      Temperature Setpoints;   !- Control 1 Name",

        "    Schedule:Compact,",
        "      Zone Control Type Sched, !- Name",
        "      Control Type,            !- Schedule Type Limits Name",
        "      Through: 12/31,          !- Field 1",
        "      For: AllDays,            !- Field 2",
        "      Until: 24:00,4;          !- Field 3",

        "    ThermostatSetpoint:DualSetpoint,",
        "      Temperature Setpoints,   !- Name",
        "      Heating Setpoints,       !- Heating Setpoint Temperature Schedule Name",
        "      Cooling Setpoints;       !- Cooling Setpoint Temperature Schedule Name",

        "    Schedule:Compact,",
        "      Heating Setpoints,       !- Name",
        "      Temperature,             !- Schedule Type Limits Name",
        "      Through: 12/31,          !- Field 1",
        "      For: AllDays,            !- Field 2",
        "      Until: 24:00,21.0;       !- Field 3",

        "    Schedule:Compact,",
        "      Cooling Setpoints,       !- Name",
        "      Temperature,             !- Schedule Type Limits Name",
        "      Through: 12/31,          !- Field 1",
        "      For: AllDays,            !- Field 2",
        "      Until: 24:00,24.0;       !- Field 3",

        "    ScheduleTypeLimits,",
        "      Control Type,            !- Name",
        "      0,                       !- Lower Limit Value",
        "      4,                       !- Upper Limit Value",
        "      DISCRETE;                !- Numeric Type",

        "    ScheduleTypeLimits,",
        "      Temperature,             !- Name",
        "      -60,                     !- Lower Limit Value",
        "      200,                     !- Upper Limit Value",
        "      CONTINUOUS,              !- Numeric Type",
        "      Temperature;             !- Unit Type",

        "    ZoneHVAC:EquipmentConnections,",
        "      ZONE ONE,                  !- Zone Name",
        "      ZONE 1 EQUIPMENT,        !- Zone Conditioning Equipment List Name",
        "      ZONE 1 INLETS,           !- Zone Air Inlet Node or NodeList Name",
        "      ,                        !- Zone Air Exhaust Node or NodeList Name",
        "      ZONE 1 NODE,             !- Zone Air Node Name",
        "      ZONE 1 OUTLET;           !- Zone Return Air Node or NodeList Name",

        "    ZoneHVAC:EquipmentList,",
        "      ZONE 1 EQUIPMENT,        !- Name",
        "      SequentialLoad,          !- Load Distribution Scheme",
        "      ZoneHVAC:IdealLoadsAirSystem,  !- Zone Equipment 1 Object Type",
        "      ZONE 1 Ideal Loads,      !- Zone Equipment 1 Name",
        "      1,                       !- Zone Equipment 1 Cooling Sequence",
        "      1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "      ,                        !- Zone Equipment 1 Sequential Cooling Load Fraction",
        "      ;                        !- Zone Equipment 1 Sequential Heating Load Fraction",

        "    ZoneHVAC:IdealLoadsAirSystem,",
        "      ZONE 1 Ideal Loads,      !- Name",
        "      ,                        !- Availability Schedule Name",
        "      ZONE 1 INLETS,           !- Zone Supply Air Node Name",
        "      ,                        !- Zone Exhaust Air Node Name",
        "      ,                        !- System Inlet Air Node Name",
        "      50,                      !- Maximum Heating Supply Air Temperature {C}",
        "      13,                      !- Minimum Cooling Supply Air Temperature {C}",
        "      0.015,                   !- Maximum Heating Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "      0.009,                   !- Minimum Cooling Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "      NoLimit,                 !- Heating Limit",
        "      autosize,                !- Maximum Heating Air Flow Rate {m3/s}",
        "      ,                        !- Maximum Sensible Heating Capacity {W}",
        "      NoLimit,                 !- Cooling Limit",
        "      autosize,                !- Maximum Cooling Air Flow Rate {m3/s}",
        "      ,                        !- Maximum Total Cooling Capacity {W}",
        "      ,                        !- Heating Availability Schedule Name",
        "      ,                        !- Cooling Availability Schedule Name",
        "      ConstantSupplyHumidityRatio,  !- Dehumidification Control Type",
        "      ,                        !- Cooling Sensible Heat Ratio {dimensionless}",
        "      ConstantSupplyHumidityRatio,  !- Humidification Control Type",
        "      ,                        !- Design Specification Outdoor Air Object Name",
        "      ,                        !- Outdoor Air Inlet Node Name",
        "      ,                        !- Demand Controlled Ventilation Type",
        "      ,                        !- Outdoor Air Economizer Type",
        "      ,                        !- Heat Recovery Type",
        "      ,                        !- Sensible Heat Recovery Effectiveness {dimensionless}",
        "      ;                        !- Latent Heat Recovery Effectiveness {dimensionless}",

        "    NodeList,",
        "      ZONE 1 INLETS,           !- Name",
        "      ZONE 1 INLET;            !- Node 1 Name",

        "  People,",
        "      OpenOffice People,       !- Name",
        "      ZONE ONE,                !- Zone or ZoneList Name",
        "      BLDG_OCC_SCH,            !- Number of People Schedule Name",
        "      People/Area,             !- Number of People Calculation Method",
        "      ,                        !- Number of People",
        "      0.010,                    !- People per Zone Floor Area {person/m2}",
        "      ,                        !- Zone Floor Area per Person {m2/person}",
        "      0.3,                     !- Fraction Radiant",
        "      ,                        !- Sensible Heat Fraction",
        "      ACTIVITY_SCH;            !- Activity Level Schedule Name",

        "  Lights,",
        "      OfficeLights,            !- Name",
        "      ZONE ONE,                !- Zone or ZoneList Name",
        "      BLDG_LIGHT_SCH,          !- Schedule Name",
        "      Watts/Area,              !- Design Level Calculation Method",
        "      ,                        !- Lighting Level {W}",
        "      8.0,                    !- Watts per Zone Floor Area {W/m2}",
        "      ,                        !- Watts per Person {W/person}",
        "      ,                        !- Return Air Fraction",
        "      ,                        !- Fraction Radiant",
        "      ,                        !- Fraction Visible",
        "      ;                        !- Fraction Replaceable",

        "  ElectricEquipment,",
        "      ElectricEquipment,       !- Name",
        "      ZONE ONE,                !- Zone or ZoneList Name",
        "      BLDG_EQUIP_SCH,          !- Schedule Name",
        "      Watts/Area,              !- Design Level Calculation Method",
        "      ,                        !- Design Level {W}",
        "      8.0,                    !- Watts per Zone Floor Area {W/m2}",
        "      ,                        !- Watts per Person {W/person}",
        "      ,                        !- Fraction Latent",
        "      ,                        !- Fraction Radiant",
        "      ;                        !- Fraction Lost",

        "  Schedule:Compact,",
        "      BLDG_EQUIP_SCH,          !- Name",
        "      Fraction,                !- Schedule Type Limits Name",
        "      Through: 12/31,          !- Field 1",
        "      For: AllDays,            !- Field 2",
        "      Until: 24:00,            !- Field 3",
        "      0.7;                     !- Field 4",

        "  Schedule:Compact,",
        "      BLDG_LIGHT_SCH,          !- Name",
        "      Fraction,                !- Schedule Type Limits Name",
        "      Through: 12/31,          !- Field 1",
        "      For: AllDays,            !- Field 2",
        "      Until: 24:00,            !- Field 3",
        "      0.5;                     !- Field 4",

        "  Schedule:Compact,",
        "      BLDG_OCC_SCH,            !- Name",
        "      Fraction,                !- Schedule Type Limits Name",
        "      Through: 12/31,          !- Field 1",
        "      For: AllDays,            !- Field 2",
        "      Until: 24:00,            !- Field 3",
        "      0.5;                     !- Field 4",

        "  Schedule:Compact,",
        "      ACTIVITY_SCH,            !- Name",
        "      Any Number,              !- Schedule Type Limits Name",
        "      Through: 12/31,          !- Field 1",
        "      For: AllDays,            !- Field 2",
        "      Until: 24:00,            !- Field 3",
        "      120.;                    !- Field 4",

        "    ScheduleTypeLimits,",
        "      Any Number;              !- Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::ManageSimulation(state);

    int CtrlZoneNum(1);
    // design peak load conditons and design supply air temperature
    EXPECT_EQ(DataSizing::CalcFinalZoneSizing(CtrlZoneNum).HeatTstatTemp, 21.0); // expects specified value
    EXPECT_EQ(DataSizing::CalcFinalZoneSizing(CtrlZoneNum).HeatDesTemp, 12.0);   // less than zone air Temp
    EXPECT_EQ(DataSizing::CalcFinalZoneSizing(CtrlZoneNum).HeatDesDay, "PHOENIX SKY HARBOR INTL AP ANN HTG 99.6% CONDNS DB");
    // actual zone air temperature at peak load
    EXPECT_NEAR(DataSizing::CalcFinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak, 17.08, 0.01);
    EXPECT_NEAR(DataSizing::FinalZoneSizing(CtrlZoneNum).ZoneTempAtHeatPeak, 17.08, 0.01);
    // Check heating design flow rates, expected to be zero due the above conditions
    EXPECT_EQ(DataSizing::CalcFinalZoneSizing(CtrlZoneNum).DesHeatVolFlow, 0.0);  // expects zero
    EXPECT_EQ(DataSizing::CalcFinalZoneSizing(CtrlZoneNum).DesHeatMassFlow, 0.0); // expects zero
    EXPECT_EQ(DataSizing::FinalZoneSizing(CtrlZoneNum).DesHeatVolFlow, 0.0);      // expects zero
    EXPECT_EQ(DataSizing::FinalZoneSizing(CtrlZoneNum).DesHeatMassFlow, 0.0);     // expects zero
    // expects non-zero peak heating load
    EXPECT_NEAR(DataSizing::CalcFinalZoneSizing(CtrlZoneNum).DesHeatLoad, 6911.42, 0.01);
    EXPECT_NEAR(DataSizing::FinalZoneSizing(CtrlZoneNum).DesHeatLoad, 6911.42, 0.01);
}
