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

#include <exception>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DisplacementVentMgr.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, InternalHeatGains_OtherEquipment_CheckFuelType)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone1;",

        "ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

        "Schedule:Constant,Schedule1,,1.0;",

        "OtherEquipment,",
        "  OtherEq1,",
        "  ,",
        "  Zone1,",
        "  Schedule1,",
        "  EquipmentLevel,",
        "  100.0,,,",
        "  0.1,",
        "  0.2,",
        "  0.05;",

        "OtherEquipment,",
        "  OtherEq2,",
        "  Propane,",
        "  Zone1,",
        "  Schedule1,",
        "  EquipmentLevel,",
        "  100.0,,,",
        "  0.1,",
        "  0.2,",
        "  0.05;",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(outputFiles()); // read schedules

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());

    ASSERT_EQ(DataHeatBalance::ZoneOtherEq.size(), 2u);

    for (unsigned long i = 1; i <= DataHeatBalance::ZoneOtherEq.size(); ++i) {
        const DataHeatBalance::ZoneEquipData &equip = DataHeatBalance::ZoneOtherEq(i);
        if (equip.Name == "OTHEREQ1") {
            ASSERT_EQ(equip.OtherEquipFuelType, ExteriorEnergyUse::ExteriorFuelUsage::Unknown);
        } else if (equip.Name == "OTHEREQ2") {
            ASSERT_EQ(equip.OtherEquipFuelType, ExteriorEnergyUse::ExteriorFuelUsage::PropaneUse);
        }
    }
}

TEST_F(EnergyPlusFixture, InternalHeatGains_OtherEquipment_NegativeDesignLevel)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone1;",

        "ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

        "Schedule:Constant,Schedule1,,1.0;",

        "OtherEquipment,",
        "  OtherEq1,",
        "  FuelOilNo1,",
        "  Zone1,",
        "  Schedule1,",
        "  EquipmentLevel,",
        "  -100.0,,,",
        "  0.1,",
        "  0.2,",
        "  0.05;",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(outputFiles()); // read schedules

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    ASSERT_THROW(InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles()), std::runtime_error);

    std::string const error_string = delimited_string(
        {"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
         "   ** Severe  ** GetInternalHeatGains: OtherEquipment=\"OTHEREQ1\", Design Level is not allowed to be negative",
         "   **   ~~~   ** ... when a fuel type of FuelOil#1 is specified.",
         "   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
         "   ...Summary of Errors that led to program termination:", "   ..... Reference severe error count=1",
         "   ..... Last severe error=GetInternalHeatGains: OtherEquipment=\"OTHEREQ1\", Design Level is not allowed to be negative"});

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, InternalHeatGains_OtherEquipment_BadFuelType)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone1;",

        "ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

        "Schedule:Constant,Schedule1,,1.0;",

        "OtherEquipment,",
        "  OtherEq1,",
        "  Water,",
        "  Zone1,",
        "  Schedule1,",
        "  EquipmentLevel,",
        "  100.0,,,",
        "  0.1,",
        "  0.2,",
        "  0.05;",

    });

    ASSERT_FALSE(process_idf(idf_objects, false)); // add false to supress error assertions
    EXPECT_TRUE(has_err_output(false));

    std::string error_string = delimited_string(
        { "   ** Severe  ** <root>[OtherEquipment][OtherEq1][fuel_type] - \"Water\" - Failed to match against any enum values." });
    EXPECT_TRUE(compare_err_stream(error_string, true));

    bool ErrorsFound(false);

    DataGlobals::NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(outputFiles()); // read schedules

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    ASSERT_THROW(InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles()), std::runtime_error);

    error_string = delimited_string(
        {"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
         "   ** Severe  ** GetInternalHeatGains: OtherEquipment: invalid Fuel Type entered=WATER for Name=OTHEREQ1",
         "   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
         "   ...Summary of Errors that led to program termination:", "   ..... Reference severe error count=2",
         "   ..... Last severe error=GetInternalHeatGains: OtherEquipment: invalid Fuel Type entered=WATER for Name=OTHEREQ1"});

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, InternalHeatGains_AllowBlankFieldsForAdaptiveComfortModel)
{
    // Adaptive comfort model fatal for irrelevant blank fields  #5948

    std::string const idf_objects = delimited_string({
        "ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

        "  Schedule:Compact,",
        "    HOUSE OCCUPANCY,    !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "  Schedule:Compact,",
        "    Activity Sch,    !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "Zone,LIVING ZONE;",

        "People,",
        "LIVING ZONE People, !- Name",
        "LIVING ZONE, !- Zone or ZoneList Name",
        "HOUSE OCCUPANCY, !- Number of People Schedule Name",
        "people, !- Number of People Calculation Method",
        "3.000000, !- Number of People",
        ", !- People per Zone Floor Area{ person / m2 }",
        ", !- Zone Floor Area per Person{ m2 / person }",
        "0.3000000, !- Fraction Radiant",
        ", !- Sensible Heat Fraction",
        "Activity Sch, !- Activity Level Schedule Name",
        "3.82E-8, !- Carbon Dioxide Generation Rate{ m3 / s - W }",
        ", !- Enable ASHRAE 55 Comfort Warnings",
        "zoneaveraged, !- Mean Radiant Temperature Calculation Type",
        ", !- Surface Name / Angle Factor List Name",
        ", !- Work Efficiency Schedule Name",
        ", !- Clothing Insulation Calculation Method",
        ", !- Clothing Insulation Calculation Method Schedule Name",
        ", !- Clothing Insulation Schedule Name",
        ", !- Air Velocity Schedule Name",
        "AdaptiveASH55;                  !- Thermal Comfort Model 1 Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound1(false);

    ScheduleManager::ProcessScheduleInput(outputFiles()); // read schedules
    HeatBalanceManager::GetZoneData(ErrorsFound1);
    ASSERT_FALSE(ErrorsFound1);

    ScheduleManager::ScheduleInputProcessed = true;
    ScheduleManager::Schedule(1).Used = true;

    ScheduleManager::Schedule(1).CurrentValue = 1.0;
    ScheduleManager::Schedule(1).MinValue = 1.0;
    ScheduleManager::Schedule(1).MaxValue = 1.0;
    ScheduleManager::Schedule(1).MaxMinSet = true;
    ScheduleManager::Schedule(2).Used = true;

    ScheduleManager::Schedule(2).CurrentValue = 131.8;
    ScheduleManager::Schedule(2).MinValue = 131.8;
    ScheduleManager::Schedule(2).MaxValue = 131.8;
    ScheduleManager::Schedule(2).MaxMinSet = true;
    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());

    EXPECT_FALSE(InternalHeatGains::ErrorsFound);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_ElectricEquipITE_BeginEnvironmentReset)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone1;",

        "ElectricEquipment:ITE:AirCooled,",
        "  Data Center Servers,     !- Name",
        "  Zone1,                   !- Zone Name",
        "  ,",
        "  Watts/Unit,              !- Design Power Input Calculation Method",
        "  500,                     !- Watts per Unit {W}",
        "  100,                     !- Number of Units",
        "  ,                        !- Watts per Zone Floor Area {W/m2}",
        "  ,  !- Design Power Input Schedule Name",
        "  ,  !- CPU Loading  Schedule Name",
        "  Data Center Servers Power fLoadTemp,  !- CPU Power Input Function of Loading and Air Temperature Curve Name",
        "  0.4,                     !- Design Fan Power Input Fraction",
        "  0.0001,                  !- Design Fan Air Flow Rate per Power Input {m3/s-W}",
        "  Data Center Servers Airflow fLoadTemp,  !- Air Flow Function of Loading and Air Temperature Curve Name",
        "  ECM FanPower fFlow,      !- Fan Power Input Function of Flow Curve Name",
        "  15,                      !- Design Entering Air Temperature {C}",
        "  A3,                      !- Environmental Class",
        "  AdjustedSupply,          !- Air Inlet Connection Type",
        "  ,                        !- Air Inlet Room Air Model Node Name",
        "  ,                        !- Air Outlet Room Air Model Node Name",
        "  Main Zone Inlet Node,    !- Supply Air Node Name",
        "  0.1,                     !- Design Recirculation Fraction",
        "  Data Center Recirculation fLoadTemp,  !- Recirculation Function of Loading and Supply Temperature Curve Name",
        "  0.9,                     !- Design Electric Power Supply Efficiency",
        "  UPS Efficiency fPLR,     !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
        "  1,                       !- Fraction of Electric Power Supply Losses to Zone",
        "  ITE-CPU,                 !- CPU End-Use Subcategory",
        "  ITE-Fans,                !- Fan End-Use Subcategory",
        "  ITE-UPS;                 !- Electric Power Supply End-Use Subcategory",

        "Curve:Quadratic,",
        "  ECM FanPower fFlow,      !- Name",
        "  0.0,                     !- Coefficient1 Constant",
        "  1.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  99.0;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  UPS Efficiency fPLR,     !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  99.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Data Center Servers Power fLoadTemp,  !- Name",
        "  -1.0,                    !- Coefficient1 Constant",
        "  1.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.06667,                 !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "  Data Center Servers Airflow fLoadTemp,  !- Name",
        "  -1.4,                    !- Coefficient1 Constant",
        "  0.9,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.1,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "  Data Center Recirculation fLoadTemp,  !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);

    DataHeatBalFanSys::MAT(1) = 24.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());
    InternalHeatGains::CalcZoneITEq();
    Real64 InitialPower = DataHeatBalance::ZoneITEq(1).CPUPower + DataHeatBalance::ZoneITEq(1).FanPower + DataHeatBalance::ZoneITEq(1).UPSPower;

    DataLoopNode::Node(1).Temp = 45.0;
    InternalHeatGains::CalcZoneITEq();
    Real64 NewPower = DataHeatBalance::ZoneITEq(1).CPUPower + DataHeatBalance::ZoneITEq(1).FanPower + DataHeatBalance::ZoneITEq(1).UPSPower;
    ASSERT_NE(InitialPower, NewPower);
    HVACManager::ResetNodeData();

    InternalHeatGains::CalcZoneITEq();
    NewPower = DataHeatBalance::ZoneITEq(1).CPUPower + DataHeatBalance::ZoneITEq(1).FanPower + DataHeatBalance::ZoneITEq(1).UPSPower;
    ASSERT_EQ(InitialPower, NewPower);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_CheckZoneComponentLoadSubtotals)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone1;",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);
    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());

    // Set up a simple convective gain for each gain type
    int zoneNum = 1;
    int numGainTypes = DataHeatBalance::NumZoneIntGainDeviceTypes;
    Array1D<Real64> convGains;
    convGains.allocate(numGainTypes);
    convGains = 0.0;
    Real64 totConvGains = 0.0;
    Real64 expectedTotConvGains = 0.0;

    for (int gainType = 1; gainType <= numGainTypes; ++gainType) {
        convGains(gainType) = 100 * gainType;
        expectedTotConvGains += convGains(gainType);
        SetupZoneInternalGain(zoneNum, DataHeatBalance::ccZoneIntGainDeviceTypes(gainType), "Gain", gainType, &convGains(gainType));
    }

    InternalHeatGains::UpdateInternalGainValues();

    // Check total of all convective gains
    InternalHeatGains::SumAllInternalConvectionGains(zoneNum, totConvGains);
    EXPECT_EQ(totConvGains, expectedTotConvGains);

    // Check subtotals used in zone component loads
    DataEnvironment::TotDesDays = 1;
    DataEnvironment::TotRunDesPersDays = 0;
    DataSizing::CurOverallSimDay = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::NumOfTimeStepInHour = 10;
    DataGlobals::TimeStep = 1;
    OutputReportTabular::AllocateLoadComponentArrays();
    int timeStepInDay = (DataGlobals::HourOfDay - 1) * DataGlobals::NumOfTimeStepInHour + DataGlobals::TimeStep;

    DataGlobals::CompLoadReportIsReq = true;
    DataGlobals::isPulseZoneSizing = false;
    InternalHeatGains::GatherComponentLoadsIntGain();
    totConvGains = OutputReportTabular::peopleInstantSeq(DataSizing::CurOverallSimDay, timeStepInDay, zoneNum) +
                   OutputReportTabular::lightInstantSeq(DataSizing::CurOverallSimDay, timeStepInDay, zoneNum) +
                   OutputReportTabular::equipInstantSeq(DataSizing::CurOverallSimDay, timeStepInDay, zoneNum) +
                   OutputReportTabular::refrigInstantSeq(DataSizing::CurOverallSimDay, timeStepInDay, zoneNum) +
                   OutputReportTabular::waterUseInstantSeq(DataSizing::CurOverallSimDay, timeStepInDay, zoneNum) +
                   OutputReportTabular::hvacLossInstantSeq(DataSizing::CurOverallSimDay, timeStepInDay, zoneNum) +
                   OutputReportTabular::powerGenInstantSeq(DataSizing::CurOverallSimDay, timeStepInDay, zoneNum);

    // Legitimate gain types excluded from this total
    expectedTotConvGains -= convGains(DataHeatBalance::IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide); // this is only used for CO2
    expectedTotConvGains -=
        convGains(DataHeatBalance::IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam); // this is only used for generic contaminants
    expectedTotConvGains -=
        convGains(DataHeatBalance::IntGainTypeOf_DaylightingDeviceTubular); // this is included in Fenestration Conduction - Sensible Instant

    // ** NOTE: If this unit test fails, the likely cause is that a new internal gain type was added, but it was not added to one of the subtotal
    // types in InternalHeatGains::GatherComponentLoadsIntGain() this also means that the new type may be missing from other places that collect
    // internal gains by subgroups, such as the room air models and output reporting for zone-level gains search for IntGainTypeOf_Lights for places
    // where these types of subtotals occur and add the new type as appropriate
    EXPECT_EQ(totConvGains, expectedTotConvGains);

    // cleanup
    convGains.deallocate();
}

TEST_F(EnergyPlusFixture, InternalHeatGains_ElectricEquipITE_ApproachTemperatures)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone1;",

        "ElectricEquipment:ITE:AirCooled,",
        "  Data Center Servers,     !- Name",
        "  Zone1,                   !- Zone Name",
        "  FlowControlWithApproachTemperatures,    !- Calculation Method",
        "  Watts/Unit,              !- Design Power Input Calculation Method",
        "  500,                     !- Watts per Unit {W}",
        "  100,                     !- Number of Units",
        "  ,                        !- Watts per Zone Floor Area {W/m2}",
        "  ,  !- Design Power Input Schedule Name",
        "  ,  !- CPU Loading  Schedule Name",
        "  Data Center Servers Power fLoadTemp,  !- CPU Power Input Function of Loading and Air Temperature Curve Name",
        "  0.4,                     !- Design Fan Power Input Fraction",
        "  0.0001,                  !- Design Fan Air Flow Rate per Power Input {m3/s-W}",
        "  Data Center Servers Airflow fLoadTemp,  !- Air Flow Function of Loading and Air Temperature Curve Name",
        "  ECM FanPower fFlow,      !- Fan Power Input Function of Flow Curve Name",
        "  15,                      !- Design Entering Air Temperature {C}",
        "  A3,                      !- Environmental Class",
        "  AdjustedSupply,          !- Air Inlet Connection Type",
        "  ,                        !- Air Inlet Room Air Model Node Name",
        "  ,                        !- Air Outlet Room Air Model Node Name",
        "  Main Zone Inlet Node,    !- Supply Air Node Name",
        "  0.1,                     !- Design Recirculation Fraction",
        "  Data Center Recirculation fLoadTemp,  !- Recirculation Function of Loading and Supply Temperature Curve Name",
        "  0.9,                     !- Design Electric Power Supply Efficiency",
        "  UPS Efficiency fPLR,     !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
        "  1,                       !- Fraction of Electric Power Supply Losses to Zone",
        "  ITE-CPU,                 !- CPU End-Use Subcategory",
        "  ITE-Fans,                !- Fan End-Use Subcategory",
        "  ITE-UPS,                 !- Electric Power Supply End-Use Subcategory",
        "  2,                       !- Supply Approach Temperature",
        "  ,                        !- Supply Approach Temperature Schedule",
        "  -2,                      !- Return Approach Temperature",
        "  ;                        !- Return Approach Temperature Schedule",

        "Curve:Quadratic,",
        "  ECM FanPower fFlow,      !- Name",
        "  0.0,                     !- Coefficient1 Constant",
        "  1.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  99.0;                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  UPS Efficiency fPLR,     !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  99.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Data Center Servers Power fLoadTemp,  !- Name",
        "  -1.0,                    !- Coefficient1 Constant",
        "  1.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.06667,                 !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "  Data Center Servers Airflow fLoadTemp,  !- Name",
        "  -1.4,                    !- Coefficient1 Constant",
        "  0.9,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.1,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "  Data Center Recirculation fLoadTemp,  !- Name",
        "  1.0,                     !- Coefficient1 Constant",
        "  0.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
    DataHeatBalance::ZnRpt.allocate(1);
    DataZoneEquipment::ZoneEquipConfig.allocate(1);

    DataHeatBalFanSys::MAT(1) = 24.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());

    DataLoopNode::Node(1).Temp = 45.0;
    InternalHeatGains::CalcZoneITEq();
    ASSERT_DOUBLE_EQ(DataHeatBalance::ZoneITEq(1).AirOutletDryBulbT + DataHeatBalance::ZoneITEq(1).ReturnApproachTemp,
                     DataHeatBalance::Zone(1).AdjustedReturnTempByITE);
    ASSERT_DOUBLE_EQ(DataLoopNode::Node(1).Temp + DataHeatBalance::ZoneITEq(1).SupplyApproachTemp, DataHeatBalance::ZoneITEq(1).AirInletDryBulbT);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_ElectricEquipITE_DefaultCurves)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone1;",

        "ElectricEquipment:ITE:AirCooled,",
        "  Data Center Servers,     !- Name",
        "  Zone1,                   !- Zone Name",
        "  ,                        !- Air Flow Calculation Method",
        "  Watts/Unit,              !- Design Power Input Calculation Method",
        "  500,                     !- Watts per Unit {W}",
        "  100,                     !- Number of Units",
        "  ,                        !- Watts per Zone Floor Area {W/m2}",
        "  ,                        !- Design Power Input Schedule Name",
        "  ,                        !- CPU Loading  Schedule Name",
        "  Data Center Servers Power fLoadTemp,        !- CPU Power Input Function of Loading and Air Temperature Curve Name",
        "  0.4,                     !- Design Fan Power Input Fraction",
        "  0.0001,                  !- Design Fan Air Flow Rate per Power Input {m3/s-W}",
        "  Data Center Servers Airflow fLoadTemp,      !- Air Flow Function of Loading and Air Temperature Curve Name",
        "  ECM FanPower fFlow,      !- Fan Power Input Function of Flow Curve Name",
        "  15,                      !- Design Entering Air Temperature {C}",
        "  A3,                      !- Environmental Class",
        "  AdjustedSupply,          !- Air Inlet Connection Type",
        "  ,                        !- Air Inlet Room Air Model Node Name",
        "  ,                        !- Air Outlet Room Air Model Node Name",
        "  Main Zone Inlet Node,    !- Supply Air Node Name",
        "  0.1,                     !- Design Recirculation Fraction",
        // This one should be assumed to always 1
        "  ,                        !- Recirculation Function of Loading and Supply Temperature Curve Name",
        "  0.9,                     !- Design Electric Power Supply Efficiency",
        // This one should be assumed to always 1
        "  ,                        !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
        "  1,                       !- Fraction of Electric Power Supply Losses to Zone",
        "  ITE-CPU,                 !- CPU End-Use Subcategory",
        "  ITE-Fans,                !- Fan End-Use Subcategory",
        "  ITE-UPS;                 !- Electric Power Supply End-Use Subcategory",

        "Curve:Quadratic,",
        "  ECM FanPower fFlow,      !- Name",
        "  0.0,                     !- Coefficient1 Constant",
        "  1.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  99.0;                    !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Data Center Servers Power fLoadTemp,  !- Name",
        "  -1.0,                    !- Coefficient1 Constant",
        "  1.0,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.06667,                 !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Biquadratic,",
        "  Data Center Servers Airflow fLoadTemp,  !- Name",
        "  -1.4,                    !- Coefficient1 Constant",
        "  0.9,                     !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.1,                     !- Coefficient4 y",
        "  0.0,                     !- Coefficient5 y**2",
        "  0.0,                     !- Coefficient6 x*y",
        "  0.0,                     !- Minimum Value of x",
        "  1.5,                     !- Maximum Value of x",
        "  -10,                     !- Minimum Value of y",
        "  99.0,                    !- Maximum Value of y",
        "  0.0,                     !- Minimum Curve Output",
        "  99.0,                    !- Maximum Curve Output",
        "  Dimensionless,           !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type"
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);

    DataHeatBalFanSys::MAT(1) = 24.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());
    InternalHeatGains::CalcZoneITEq();

    // If Electric Power Supply Efficiency Function of Part Load Ratio Curve Name is blank => always 1, so UPSPower is calculated as such
    Real64 DefaultUPSPower = (DataHeatBalance::ZoneITEq(1).CPUPower + DataHeatBalance::ZoneITEq(1).FanPower) *
                             max((1.0 - DataHeatBalance::ZoneITEq(1).DesignUPSEfficiency), 0.0);

    ASSERT_EQ(DefaultUPSPower, DataHeatBalance::ZoneITEq(1).UPSPower);

}

TEST_F(EnergyPlusFixture, InternalHeatGains_CheckThermalComfortSchedules)
{

    bool WorkEffSchPresent; // true equals blank, false equals not blank
    bool CloInsSchPresent;  // true equals blank, false equals not blank
    bool AirVelSchPresent;  // true equals blank, false equals not blank
    bool FunctionCallResult;
    bool ExpectedResult;

    //Test 1: everything blank--should result in false result
    WorkEffSchPresent = true;
    CloInsSchPresent = true;
    AirVelSchPresent = true;
    ExpectedResult = false;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    //Additional Tests: test various combinations where at least one flag is not blank (false)--should result in a true result
    WorkEffSchPresent = false;
    CloInsSchPresent = true;
    AirVelSchPresent = true;
    ExpectedResult = true;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    WorkEffSchPresent = true;
    CloInsSchPresent = false;
    AirVelSchPresent = true;
    ExpectedResult = true;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    WorkEffSchPresent = true;
    CloInsSchPresent = true;
    AirVelSchPresent = false;
    ExpectedResult = true;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    WorkEffSchPresent = false;
    CloInsSchPresent = false;
    AirVelSchPresent = true;
    ExpectedResult = true;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    WorkEffSchPresent = false;
    CloInsSchPresent = true;
    AirVelSchPresent = false;
    ExpectedResult = true;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    WorkEffSchPresent = true;
    CloInsSchPresent = false;
    AirVelSchPresent = false;
    ExpectedResult = true;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    WorkEffSchPresent = false;
    CloInsSchPresent = false;
    AirVelSchPresent = false;
    ExpectedResult = true;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

}
TEST_F(EnergyPlusFixture, InternalHeatGains_ZnRpt_Outputs)
{

    std::string const idf_objects = delimited_string({
        "Zone,Zone 1;",

        "ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

        "Schedule:Constant,Schedule1,,1.0;",

        "  People,",
        "    Zone 1 People,           !- Name",
        "    Zone 1,                  !- Zone or ZoneList Name",
        "    Schedule1,               !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    3.000000,                !- Number of People",
        "    ,                        !- People per Zone Floor Area{ person / m2 }",
        "    ,                        !- Zone Floor Area per Person{ m2 / person }",
        "    0.3000000,               !- Fraction Radiant",
        "    0.5,                     !- Sensible Heat Fraction",
        "    Schedule1,               !- Activity Level Schedule Name",
        "    3.82E-8;                 !- Carbon Dioxide Generation Rate{ m3 / s - W }",

        "  Lights,",
        "    Zone 1 Lights,           !- Name",
        "    Zone 1,                  !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    100.0,                   !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Return Air Fraction",
        "    0.7000,                  !- Fraction Radiant",
        "    0.2000,                  !- Fraction Visible",
        "    1.0000,                  !- Fraction Replaceable",
        "    General,                 !- End-Use Subcategory",
        "    No;                      !- Return Air Fraction Calculated from Plenum Temperature",

        "  ElectricEquipment,",
        "    Zone 1 Electric Equipment,  !- Name",
        "    Zone 1,                  !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    150.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000;                  !- Fraction Lost",

        "  GasEquipment,",
        "    Zone 1 Gas Equipment,  !- Name",
        "    Zone 1,                  !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    200.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000,                  !- Fraction Lost",
        "    1.0E-7;                  !- Carbon Dioxide Generation Rate {m3/s-W}",

        "  HotWaterEquipment,",
        "    Zone 1 Hot Water Equipment,  !- Name",
        "    Zone 1,                  !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    250.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000;                  !- Fraction Lost",

        "  SteamEquipment,",
        "    Zone 1 Steam Equipment,  !- Name",
        "    Zone 1,                  !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    300.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000;                  !- Fraction Lost",

        "  OtherEquipment,",
        "    Zone 1 Other Equipment,  !- Name",
        "    OtherFuel1,              !- Fuel Type",
        "    Zone 1,                  !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    350.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000,                  !- Fraction Lost",
        "    2.0E-7;                  !- Carbon Dioxide Generation Rate {m3/s-W}",

        "  ZoneBaseboard:OutdoorTemperatureControlled,",
        "    Zone 1 BBHeat,           !- Name",
        "    Zone 1,                  !- Zone Name",
        "    Schedule1,               !- Schedule Name",
        "    1500,                    !- Capacity at Low Temperature {W}",
        "    0,                       !- Low Temperature {C}",
        "    500,                     !- Capacity at High Temperature {W}",
        "    10,                      !- High Temperature {C}",
        "    0.5,                     !- Fraction Radiant",
        "    Baseboard Heat;          !- End - Use Subcategory",

        "  ZoneContaminantSourceAndSink:CarbonDioxide,",
        "    CO2people,               !- Name",
        "    Zone 1,                  !- Zone Name",
        "    0.0001125,               !- Design Generation Rate{ m3 / s }",
        "    Schedule1;               !- Schedule Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    DataGlobals::NumOfTimeStepInHour = 1;                 // must initialize this to get schedules initialized
    DataGlobals::MinutesPerTimeStep = 60;                 // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(outputFiles()); // read schedules
    DataEnvironment::DayOfYear_Schedule = 1;
    DataEnvironment::DayOfMonth = 1;
    DataEnvironment::DayOfWeek = 1;
    DataGlobals::HourOfDay = 1;
    DataGlobals::TimeStep = 1;
    ScheduleManager::UpdateScheduleValues();

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    HeatBalanceManager::AllocateHeatBalArrays();

    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());

    EXPECT_EQ(DataHeatBalance::TotPeople, 1);
    EXPECT_EQ(DataHeatBalance::TotLights, 1);
    EXPECT_EQ(DataHeatBalance::TotElecEquip, 1);
    EXPECT_EQ(DataHeatBalance::TotGasEquip, 1);
    EXPECT_EQ(DataHeatBalance::TotHWEquip, 1);
    EXPECT_EQ(DataHeatBalance::TotStmEquip, 1);
    EXPECT_EQ(DataHeatBalance::TotOthEquip, 1);
    EXPECT_EQ(DataHeatBalance::TotBBHeat, 1);

    EnergyPlus::createFacilityElectricPowerServiceObject(); // Needs to happen before InitInternalHeatGains

    // First time should be all good, because ZnRpt values intialize to zero
    InternalHeatGains::InitInternalHeatGains(state);

    EXPECT_EQ(DataHeatBalance::ZnRpt(1).LtsPower, 100.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).ElecPower, 150.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).GasPower, 200.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).HWPower, 250.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).SteamPower, 300.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).BaseHeatPower, 1500.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).CO2Rate, 0.0001125);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).ITEqSHI, 0);

    // Second time should should give the same answers, because everything should reset before accumulating
    InternalHeatGains::InitInternalHeatGains(state);

    EXPECT_EQ(DataHeatBalance::ZnRpt(1).LtsPower, 100.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).ElecPower, 150.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).GasPower, 200.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).HWPower, 250.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).SteamPower, 300.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).BaseHeatPower, 1500.0);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).CO2Rate, 0.0001125);
    EXPECT_EQ(DataHeatBalance::ZnRpt(1).ITEqSHI, 0);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_FlowFromSystemMethod)
{
    std::string const idf_objects = delimited_string({
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

        "  Site:GroundTemperature:BuildingSurface,18.89,18.92,19.02,19.12,19.21,19.23,19.07,19.32,19.09,19.21,19.13,18.96;",

        "  Zone,",
        "    Main Zone,               !- Name",
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
        "    Main Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall002,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    Main Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall003,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    Main Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    15.24000,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,15.24000,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Wall004,           !- Name",
        "    Wall,                    !- Surface Type",
        "    R13WALL,                 !- Construction Name",
        "    Main Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,15.24000,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,15.24000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Flr001,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR,                   !- Construction Name",
        "    Main Zone,               !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Zn001:Flr001,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.000000,                !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    15.24000,0.000000,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.000000,15.24000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Zn001:Roof001,           !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF31,                  !- Construction Name",
        "    Main Zone,               !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.000000,15.24000,4.572,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.000000,0.000000,4.572,  !- X,Y,Z ==> Vertex 2 {m}",
        "    15.24000,0.000000,4.572,  !- X,Y,Z ==> Vertex 3 {m}",
        "    15.24000,15.24000,4.572;  !- X,Y,Z ==> Vertex 4 {m}",

        "  ScheduleTypeLimits,",
        "    Any Number;              !- Name",

        "  Schedule:Compact,",
        "    System Availability Schedule,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    THROUGH: 12/31,          !- Field 1",
        "    FOR: AllDays,            !- Field 2",
        "    UNTIL: 24:00,1;          !- Field 3",

        "  AirLoopHVAC,",
        "    CRAC system,             !- Name",
        "    ,                        !- Controller List Name",
        "    CRAC 1 Availability List,!- Availability Manager List Name",
        "    8.5,                     !- Design Supply Air Flow Rate {m3/s}",
        "    Air Loop Branches,       !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    Supply Inlet Node,       !- Supply Side Inlet Node Name",
        "    Zone Equipment Outlet Node,  !- Demand Side Outlet Node Name",
        "    Zone Equipment Inlet Node,  !- Demand Side Inlet Node Names",
        "    Supply Outlet Node;      !- Supply Side Outlet Node Names",

        "  AvailabilityManagerAssignmentList,",
        "    CRAC 1 Availability List,!- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    CRAC 1 Avail;            !- Availability Manager 1 Name",

        "  AvailabilityManager:Scheduled,",
        "    CRAC 1 Avail,            !- Name",
        "    System Availability Schedule;  !- Schedule Name",

        "  BranchList,",
        "    Air Loop Branches,       !- Name",
        "    Air Loop Main Branch;    !- Branch 1 Name",

        "  Branch,",
        "    Air Loop Main Branch,    !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    CoilSystem:Cooling:DX,   !- Component 1 Object Type",
        "    DX Cooling Coil System 1,!- Component 1 Name",
        "    Supply Inlet Node,       !- Component 1 Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 1 Outlet Node Name",
        "    Fan:VariableVolume,      !- Component 2 Object Type",
        "    EC Plug Fan 1,           !- Component 2 Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 2 Inlet Node Name",
        "    Supply Outlet Node;      !- Component 2 Outlet Node Name",

        "  CoilSystem:Cooling:DX,",
        "    DX Cooling Coil System 1,!- Name",
        "    System Availability Schedule,  !- Availability Schedule Name",
        "    Supply Inlet Node,       !- DX Cooling Coil System Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- DX Cooling Coil System Outlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- DX Cooling Coil System Sensor Node Name",
        "    Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type",
        "    Main Cooling Coil 1;     !- Cooling Coil Name",

        "  Coil:Cooling:DX:SingleSpeed,",
        "    Main Cooling Coil 1,     !- Name",
        "    System Availability Schedule,  !- Availability Schedule Name",
        "    148300,                  !- Gross Rated Total Cooling Capacity {W}",
        "    0.83,                    !- Gross Rated Sensible Heat Ratio",
        "    4.5669,                  !- Gross Rated Cooling COP {W/W}",
        "    8.5,                     !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    Supply Inlet Node,       !- Air Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Air Outlet Node Name",
        "    Cool Cap Mod func of Temperature,  !- Total Cooling Capacity Function of Temperature Curve Name",
        "    HPACCoolCapFFF,          !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    Liebert Econophase EIR Func T,  !- Energy Input Ratio Function of Temperature Curve Name",
        "    HPACCOOLEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    HPACCOOLPLFFPLR,         !- Part Load Fraction Correlation Curve Name",
        "    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    ,                        !- Nominal Time for Condensate Removal to Begin {s}",
        "    ,                        !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "    ,                        !- Maximum Cycling Rate {cycles/hr}",
        "    ,                        !- Latent Capacity Time Constant {s}",
        "    ,                        !- Condenser Air Inlet Node Name",
        "    ,                        !- Condenser Type",
        "    ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
        "    ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
        "    ,                        !- Evaporative Condenser Pump Rated Power Consumption {W}",
        "    ,                        !- Crankcase Heater Capacity {W}",
        "    ,                        !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "    ,                        !- Supply Water Storage Tank Name",
        "    ,                        !- Condensate Collection Water Storage Tank Name",
        "    ,                        !- Basin Heater Capacity {W/K}",
        "    ,                        !- Basin Heater Setpoint Temperature {C}",
        "    ,                        !- Basin Heater Operating Schedule Name",
        "    ,                        !- Sensible Heat Ratio Function of Temperature Curve Name",
        "    ,                        !- Sensible Heat Ratio Function of Flow Fraction Curve Name",
        "    Yes;                     !- Report ASHRAE Standard 127 Performance Ratings",

        "  Table:IndependentVariable,",
        "    Cool Cap Mod func of Temperature_IndependentVariable1,  !- Name",
        "    Linear,                  !- Interpolation Method",
        "    Constant,                !- Extrapolation Method",
        "    13.0,                    !- Minimum Value",
        "    23.89,                   !- Maximum Value",
        "    ,                        !- Normalization Reference Value",
        "    Temperature,             !- Unit Type",
        "    ,                        !- External File Name",
        "    ,                        !- External File Column Number",
        "    ,                        !- External File Starting Row Number",
        "    13.00000,                !- Value 1",
        "    17.00000,                !- Value 2",
        "    19.44440,                !- <none>",
        "    21.00000,                !- <none>",
        "    23.90000;                !- <none>",

        "  Table:IndependentVariable,",
        "    Cool Cap Mod func of Temperature_IndependentVariable2,  !- Name",
        "    Linear,                  !- Interpolation Method",
        "    Constant,                !- Extrapolation Method",
        "    -10.0,                   !- Minimum Value",
        "    46.0,                    !- Maximum Value",
        "    ,                        !- Normalization Reference Value",
        "    Temperature,             !- Unit Type",
        "    ,                        !- External File Name",
        "    ,                        !- External File Column Number",
        "    ,                        !- External File Starting Row Number",
        "    -10.00000,               !- Value 1",
        "    15.00000,                !- Value 2",
        "    18.00000,                !- <none>",
        "    24.00000,                !- <none>",
        "    30.00000,                !- <none>",
        "    35.00000,                !- <none>",
        "    38.00000,                !- <none>",
        "    46.00000;                !- <none>",

        "  Table:IndependentVariableList,",
        "    Cool Cap Mod func of Temperature_IndependentVariableList,  !- Name",
        "    Cool Cap Mod func of Temperature_IndependentVariable1,  !- Independent Variable 1 Name",
        "    Cool Cap Mod func of Temperature_IndependentVariable2;  !- Independent Variable 2 Name",

        "  Table:Lookup,",
        "    Cool Cap Mod func of Temperature,  !- Name",
        "    Cool Cap Mod func of Temperature_IndependentVariableList,  !- Independent Variable List Name",
        "    ,                        !- Normalization Method",
        "    ,                        !- Normalization Divisor",
        "    ,                        !- Minimum Output",
        "    ,                        !- Maximum Output",
        "    Dimensionless,           !- Output Unit Type",
        "    ,                        !- External File Name",
        "    ,                        !- External File Column Number",
        "    ,                        !- External File Starting Row Number",
        "    1.00,                    !- Output Value 1",
        "    1.00,                    !- Output Value 2",
        "    1.00,                    !- <none>",
        "    0.924738271,             !- <none>",
        "    0.883909339,             !- <none>",
        "    0.835522309,             !- <none>",
        "    0.800222635,             !- <none>",
        "    0.683109499,             !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    0.976933863,             !- <none>",
        "    0.937696593,             !- <none>",
        "    0.907886775,             !- <none>",
        "    0.805413255,             !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    0.9718,                  !- <none>",
        "    0.8782,                  !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.0385,                  !- <none>",
        "    1.0142,                  !- <none>",
        "    0.9264,                  !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.00,                    !- <none>",
        "    1.110828252,             !- <none>",
        "    1.090488436,             !- <none>",
        "    1.013268253;             !- <none>",

        "  Curve:Quadratic,",
        "    HPACCoolCapFFF,          !- Name",
        "    0.8,                     !- Coefficient1 Constant",
        "    0.2,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Biquadratic,",
        "    Liebert Econophase quadratic fit,  !- Name",
        "    0.1416159,               !- Coefficient1 Constant",
        "    0.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.013828452,             !- Coefficient4 y",
        "    0.00023872,              !- Coefficient5 y**2",
        "    0.0,                     !- Coefficient6 x*y",
        "    12.77778,                !- Minimum Value of x",
        "    23.88889,                !- Maximum Value of x",
        "    -10,                     !- Minimum Value of y",
        "    46.11111,                !- Maximum Value of y",
        "    0.04,                    !- Minimum Curve Output",
        "    1.4,                     !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Table:IndependentVariable,",
        "    Liebert Econophase EIR Func T_IndependentVariable1,  !- Name",
        "    Linear,                  !- Interpolation Method",
        "    Constant,                !- Extrapolation Method",
        "    12.7,                    !- Minimum Value",
        "    23.8,                    !- Maximum Value",
        "    ,                        !- Normalization Reference Value",
        "    Temperature,             !- Unit Type",
        "    ,                        !- External File Name",
        "    ,                        !- External File Column Number",
        "    ,                        !- External File Starting Row Number",
        "    12.70000,                !- Value 1",
        "    23.80000;                !- Value 2",

        "  Table:IndependentVariable,",
        "    Liebert Econophase EIR Func T_IndependentVariable2,  !- Name",
        "    Linear,                  !- Interpolation Method",
        "    Constant,                !- Extrapolation Method",
        "    -50,                     !- Minimum Value",
        "    50,                      !- Maximum Value",
        "    ,                        !- Normalization Reference Value",
        "    Temperature,             !- Unit Type",
        "    ,                        !- External File Name",
        "    ,                        !- External File Column Number",
        "    ,                        !- External File Starting Row Number",
        "    -50.00000,               !- Value 1",
        "    -4.00000,                !- Value 2",
        "    -1.22220,                !- <none>",
        "    1.55550,                 !- <none>",
        "    4.33330,                 !- <none>",
        "    7.11110,                 !- <none>",
        "    9.88880,                 !- <none>",
        "    12.66670,                !- <none>",
        "    15.44440,                !- <none>",
        "    18.22200,                !- <none>",
        "    21.00000,                !- <none>",
        "    23.77778,                !- <none>",
        "    26.55556,                !- <none>",
        "    29.33333,                !- <none>",
        "    32.11111,                !- <none>",
        "    34.88889,                !- Extended Field",
        "    50.00000;                !- Extended Field",

        "  Table:IndependentVariableList,",
        "    Liebert Econophase EIR Func T_IndependentVariableList,  !- Name",
        "    Liebert Econophase EIR Func T_IndependentVariable1,  !- Independent Variable 1 Name",
        "    Liebert Econophase EIR Func T_IndependentVariable2;  !- Independent Variable 2 Name",

        "  Table:Lookup,",
        "    Liebert Econophase EIR Func T,  !- Name",
        "    Liebert Econophase EIR Func T_IndependentVariableList,  !- Independent Variable List Name",
        "    ,                        !- Normalization Method",
        "    ,                        !- Normalization Divisor",
        "    0.03,                    !- Minimum Output",
        "    1.5,                     !- Maximum Output",
        "    Dimensionless,           !- Output Unit Type",
        "    ,                        !- External File Name",
        "    ,                        !- External File Column Number",
        "    ,                        !- External File Starting Row Number",
        "    0.042,                   !- Output Value 1",
        "    0.042,                   !- Output Value 2",
        "    0.084,                   !- <none>",
        "    0.084,                   !- <none>",
        "    0.2269,                  !- <none>",
        "    0.2395,                  !- <none>",
        "    0.311,                   !- <none>",
        "    0.3697,                  !- <none>",
        "    0.4454,                  !- <none>",
        "    0.5462,                  !- <none>",
        "    0.6723,                  !- <none>",
        "    0.7227,                  !- <none>",
        "    0.7773,                  !- <none>",
        "    0.8193,                  !- <none>",
        "    0.895,                   !- <none>",
        "    1.0,                     !- <none>",
        "    1.5,                     !- <none>",
        "    0.042,                   !- <none>",
        "    0.042,                   !- <none>",
        "    0.084,                   !- <none>",
        "    0.084,                   !- <none>",
        "    0.2269,                  !- <none>",
        "    0.2395,                  !- <none>",
        "    0.311,                   !- <none>",
        "    0.3697,                  !- <none>",
        "    0.4454,                  !- <none>",
        "    0.5462,                  !- <none>",
        "    0.6723,                  !- <none>",
        "    0.7227,                  !- <none>",
        "    0.7773,                  !- <none>",
        "    0.8193,                  !- <none>",
        "    0.895,                   !- <none>",
        "    1.0,                     !- <none>",
        "    1.5;                     !- <none>",

        "  Curve:Quadratic,",
        "    HPACCOOLEIRFFF,          !- Name",
        "    1.156,                   !- Coefficient1 Constant",
        "    -0.1816,                 !- Coefficient2 x",
        "    0.0256,                  !- Coefficient3 x**2",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACCOOLPLFFPLR,         !- Name",
        "    0.85,                    !- Coefficient1 Constant",
        "    0.15,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  SetpointManager:Warmest,",
        "    Supply air control,      !- Name",
        "    Temperature,             !- Control Variable",
        "    CRAC system,             !- HVAC Air Loop Name",
        "    10.0,                    !- Minimum Setpoint Temperature {C}",
        "    25.0,                    !- Maximum Setpoint Temperature {C}",
        "    MAximumTemperature,      !- Strategy",
        "    Supply Outlet Node;      !- Setpoint Node or NodeList Name",

        "  SetpointManager:MixedAir,",
        "    Coil Exit Temp Manager 1,!- Name",
        "    Temperature,             !- Control Variable",
        "    Supply Outlet Node,      !- Reference Setpoint Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Fan Inlet Node Name",
        "    Supply Outlet Node,      !- Fan Outlet Node Name",
        "    Main Cooling Coil 1 Outlet Node;  !- Setpoint Node or NodeList Name",

        "  Fan:VariableVolume,",
        "    EC Plug Fan 1,           !- Name",
        "    System Availability Schedule,  !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    486.0,                   !- Pressure Rise {Pa}",
        "    8.5,                     !- Maximum Flow Rate {m3/s}",
        "    Fraction,                !- Fan Power Minimum Flow Rate Input Method",
        "    0.05,                    !- Fan Power Minimum Flow Fraction",
        "    ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    0.06990146,              !- Fan Power Coefficient 1",
        "    1.39500612,              !- Fan Power Coefficient 2",
        "    -3.35487336,             !- Fan Power Coefficient 3",
        "    2.89232315,              !- Fan Power Coefficient 4",
        "    0.000,                   !- Fan Power Coefficient 5",
        "    Main Cooling Coil 1 Outlet Node,  !- Air Inlet Node Name",
        "    Supply Outlet Node;      !- Air Outlet Node Name",

        "  AirLoopHVAC:SupplyPath,",
        "    Zone Supply Air Path,    !- Name",
        "    Zone Equipment Inlet Node,  !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    Zone Supply Air Splitter;!- Component 1 Name",

        "  AirLoopHVAC:ZoneSplitter,",
        "    Zone Supply Air Splitter,!- Name",
        "    Zone Equipment Inlet Node,  !- Inlet Node Name",
        "    Main Zone ATU In Node;   !- Outlet 1 Node Name",

        "  AirLoopHVAC:ReturnPath,",
        "    Zone Return Air Path,    !- Name",
        "    Zone Equipment Outlet Node,  !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    Zone Return Air Mixer;   !- Component 1 Name",

        "  AirLoopHVAC:ZoneMixer,",
        "    Zone Return Air Mixer,   !- Name",
        "    Zone Equipment Outlet Node,  !- Outlet Node Name",
        "    Main Zone Outlet Node;   !- Inlet 1 Node Name",

        "  ZoneHVAC:EquipmentConnections,",
        "    Main Zone,               !- Zone Name",
        "    Main Zone Equipment,     !- Zone Conditioning Equipment List Name",
        "    Main Zone Inlet Node,    !- Zone Air Inlet Node or NodeList Name",
        "    ,                        !- Zone Air Exhaust Node or NodeList Name",
        "    Main Zone Node,          !- Zone Air Node Name",
        "    Main Zone Outlet Node;   !- Zone Return Air Node or NodeList Name",

        "  ZoneHVAC:EquipmentList,",
        "    Main Zone Equipment,     !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    Main Zone ATU,           !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    2,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
        "    ,                        !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",
        "    ZoneHVAC:Baseboard:Convective:Electric,  !- Zone Equipment 2 Object Type",
        "    Main Zone Baseboard,     !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    1,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "    ,                        !- Zone Equipment 2 Sequential Cooling Fraction Schedule Name",
        "    ;                        !- Zone Equipment 2 Sequential Heating Fraction Schedule Name",

        "  ZoneHVAC:AirDistributionUnit,",
        "    Main Zone ATU,           !- Name",
        "    Main Zone Inlet Node,    !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "    Main Zone VAV Air;       !- Air Terminal Name",

        "  AirTerminal:SingleDuct:VAV:NoReheat,",
        "    Main Zone VAV Air,       !- Name",
        "    System Availability Schedule,  !- Availability Schedule Name",
        "    Main Zone Inlet Node,    !- Air Outlet Node Name",
        "    Main Zone ATU In Node,   !- Air Inlet Node Name",
        "    8.5,                     !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    0.05;                    !- Constant Minimum Air Flow Fraction",

        "  ZoneHVAC:Baseboard:Convective:Electric,",
        "    Main Zone Baseboard,     !- Name",
        "    System Availability Schedule,  !- Availability Schedule Name",
        "    HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "    8000,                    !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Heating Design Capacity",
        "    0.97;                    !- Efficiency",

        "  ZoneControl:Thermostat,",
        "    Main Zone Thermostat,    !- Name",
        "    Main Zone,               !- Zone or ZoneList Name",
        "    Zone Control Type Sched, !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    DualSetPoint;            !- Control 1 Name",

        "  Schedule:Compact,",
        "    Zone Control Type Sched, !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,4;          !- Field 3",

        "  ThermostatSetpoint:DualSetpoint,",
        "    DualSetPoint,            !- Name",
        "    Heating Setpoint Schedule,  !- Heating Setpoint Temperature Schedule Name",
        "    Cooling Return Air Setpoint Schedule;  !- Cooling Setpoint Temperature Schedule Name",

        "  Schedule:Compact,",
        "    Heating Setpoint Schedule,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,15.0;       !- Field 3",

        "  Schedule:Compact,",
        "    Supply Air Setpoint Schedule,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,10.0;       !- Field 3",

        "  Schedule:Compact,",
        "    Cooling Return Air Setpoint Schedule,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,29.3;       !- Field 3",

        "  ElectricEquipment:ITE:AirCooled,",
        "    Data Center Servers,     !- Name",
        "    Main Zone,               !- Zone Name",
        "    FlowFromSystem,          !- Air Flow Calculation Method",
        "    Watts/Unit,              !- Design Power Input Calculation Method",
        "    500,                     !- Watts per Unit {W}",
        "    100,                     !- Number of Units",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    Data Center Operation Schedule,  !- Design Power Input Schedule Name",
        "    Data Center CPU Loading Schedule,  !- CPU Loading  Schedule Name",
        "    Data Center Servers Power fLoadTemp,  !- CPU Power Input Function of Loading and Air Temperature Curve Name",
        "    0.4,                     !- Design Fan Power Input Fraction",
        "    0.0001,                  !- Design Fan Air Flow Rate per Power Input {m3/s-W}",
        "    Data Center Servers Airflow fLoadTemp,  !- Air Flow Function of Loading and Air Temperature Curve Name",
        "    ECM FanPower fFlow,      !- Fan Power Input Function of Flow Curve Name",
        "    15,                      !- Design Entering Air Temperature {C}",
        "    A3,                      !- Environmental Class",
        "    AdjustedSupply,          !- Air Inlet Connection Type",
        "    ,                        !- Air Inlet Room Air Model Node Name",
        "    ,                        !- Air Outlet Room Air Model Node Name",
        "    Main Zone Inlet Node,    !- Supply Air Node Name",
        "    0.1,                     !- Design Recirculation Fraction",
        "    Data Center Recirculation fLoadTemp,  !- Recirculation Function of Loading and Supply Temperature Curve Name",
        "    0.9,                     !- Design Electric Power Supply Efficiency",
        "    UPS Efficiency fPLR,     !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
        "    1,                       !- Fraction of Electric Power Supply Losses to Zone",
        "    ITE-CPU,                 !- CPU End-Use Subcategory",
        "    ITE-Fans,                !- Fan End-Use Subcategory",
        "    ITE-UPS;                 !- Electric Power Supply End-Use Subcategory",

        "  Curve:Quadratic,",
        "    ECM FanPower fFlow,      !- Name",
        "    0.0,                     !- Coefficient1 Constant",
        "    1.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    99.0;                    !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    UPS Efficiency fPLR,     !- Name",
        "    1.0,                     !- Coefficient1 Constant",
        "    0.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    99.0;                    !- Maximum Value of x",

        "  Curve:Biquadratic,",
        "    Data Center Servers Power fLoadTemp,  !- Name",
        "    -1.0,                    !- Coefficient1 Constant",
        "    1.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.06667,                 !- Coefficient4 y",
        "    0.0,                     !- Coefficient5 y**2",
        "    0.0,                     !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    -10,                     !- Minimum Value of y",
        "    99.0,                    !- Maximum Value of y",
        "    0.0,                     !- Minimum Curve Output",
        "    99.0,                    !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    Data Center Servers Airflow fLoadTemp,  !- Name",
        "    -1.4,                    !- Coefficient1 Constant",
        "    0.9,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.1,                     !- Coefficient4 y",
        "    0.0,                     !- Coefficient5 y**2",
        "    0.0,                     !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    -10,                     !- Minimum Value of y",
        "    99.0,                    !- Maximum Value of y",
        "    0.0,                     !- Minimum Curve Output",
        "    99.0,                    !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Biquadratic,",
        "    Data Center Recirculation fLoadTemp,  !- Name",
        "    1.0,                     !- Coefficient1 Constant",
        "    0.0,                     !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 y",
        "    0.0,                     !- Coefficient5 y**2",
        "    0.0,                     !- Coefficient6 x*y",
        "    0.0,                     !- Minimum Value of x",
        "    1.5,                     !- Maximum Value of x",
        "    -10,                     !- Minimum Value of y",
        "    99.0,                    !- Maximum Value of y",
        "    0.0,                     !- Minimum Curve Output",
        "    99.0,                    !- Maximum Curve Output",
        "    Dimensionless,           !- Input Unit Type for X",
        "    Temperature,             !- Input Unit Type for Y",
        "    Dimensionless;           !- Output Unit Type",

        "  Schedule:Constant,Data Center Operation Schedule,Any Number,1.0;",

        "  Schedule:Compact,",
        "    Data Center CPU Loading Schedule,  !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 1/31,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0,        !- Field 3",
        "    Through: 2/29,           !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    Until: 24:00,0.50,       !- Field 7",
        "    Through: 3/31,           !- Field 9",
        "    For: AllDays,            !- Field 10",
        "    Until: 24:00,0.75,       !- Field 11",
        "    Through: 4/30,           !- Field 13",
        "    For: AllDays,            !- Field 14",
        "    Until: 24:00,1.0,        !- Field 15",
        "    Through: 5/31,           !- Field 17",
        "    For: AllDays,            !- Field 18",
        "    Until: 24:00,0.25,       !- Field 19",
        "    Through: 6/30,           !- Field 21",
        "    For: AllDays,            !- Field 22",
        "    Until: 24:00,0.50,       !- Field 23",
        "    Through: 7/31,           !- Field 25",
        "    For: AllDays,            !- Field 26",
        "    Until: 24:00,0.1,        !- Field 27",
        "    Through: 8/31,           !- Field 29",
        "    For: AllDays,            !- Field 30",
        "    Until: 24:00,1.0,        !- Field 31",
        "    Through: 9/30,           !- Field 33",
        "    For: AllDays,            !- Field 34",
        "    Until: 24:00,0.25,       !- Field 35",
        "    Through: 10/31,          !- Field 37",
        "    For: AllDays,            !- Field 38",
        "    Until: 24:00,0.50,       !- Field 39",
        "    Through: 11/30,          !- Field 41",
        "    For: AllDays,            !- Field 42",
        "    Until: 24:00,0.75,       !- Field 43",
        "    Through: 12/31,          !- Field 45",
        "    For: AllDays,            !- Field 46",
        "    Until: 24:00,1.00;       !- Field 47",

        "  EnergyManagementSystem:Sensor,",
        "    Qdot_DXCoil_Sens,        !- Name",
        "    Main Cooling Coil 1,     !- Output:Variable or Output:Meter Index Key Name",
        "    Cooling Coil Sensible Cooling Rate;  !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:Sensor,",
        "    Power_DX_Coil_Elec,      !- Name",
        "    Main Cooling Coil 1,     !- Output:Variable or Output:Meter Index Key Name",
        "    Cooling Coil Electric Power;  !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:Sensor,",
        "    Power_SupplyFan_Elec,    !- Name",
        "    EC Plug Fan 1,           !- Output:Variable or Output:Meter Index Key Name",
        "    Fan Electric Power;      !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:Sensor,",
        "    DeltaT_SupplyFan,        !- Name",
        "    EC Plug Fan 1,           !- Output:Variable or Output:Meter Index Key Name",
        "    Fan Rise in Air Temperature;  !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:Sensor,",
        "    Mdot_SupplyFan,          !- Name",
        "    Supply Outlet Node,      !- Output:Variable or Output:Meter Index Key Name",
        "    System Node Mass Flow Rate;  !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:Sensor,",
        "    Tout_SupplyFan,          !- Name",
        "    Supply Outlet Node,      !- Output:Variable or Output:Meter Index Key Name",
        "    System Node Temperature; !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:Sensor,",
        "    Wout_SupplyFan,          !- Name",
        "    Supply Outlet Node,      !- Output:Variable or Output:Meter Index Key Name",
        "    System Node Humidity Ratio;  !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:OutputVariable,",
        "    CRAC total system Power, !- Name",
        "    PowerCRAC,               !- EMS Variable Name",
        "    Averaged,                !- Type of Data in Variable",
        "    SystemTimeStep,          !- Update Frequency",
        "    ,                        !- EMS Program or Subroutine Name",
        "    W;                       !- Units",

        "  EnergyManagementSystem:OutputVariable,",
        "    CRAC Net Sensible Capacity,  !- Name",
        "    QdotNetSens,             !- EMS Variable Name",
        "    Averaged,                !- Type of Data in Variable",
        "    SystemTimeStep,          !- Update Frequency",
        "    ,                        !- EMS Program or Subroutine Name",
        "    W;                       !- Units",

        "  EnergyManagementSystem:OutputVariable,",
        "    CRAC SCOP,               !- Name",
        "    SCOP,                    !- EMS Variable Name",
        "    Averaged,                !- Type of Data in Variable",
        "    SystemTimeStep,          !- Update Frequency",
        "    ,                        !- EMS Program or Subroutine Name",
        "    ;                        !- Units",

        "  EnergyManagementSystem:GlobalVariable,",
        "    SCOP,                    !- Erl Variable 1 Name",
        "    QdotNetSens,             !- Erl Variable 2 Name",
        "    PowerCRAC;               !- Erl Variable 3 Name",

        "  EnergyManagementSystem:ProgramCallingManager,",
        "    Calculate Sensible Coefficient of Performance,  !- Name",
        "    EndOfZoneTimestepBeforeZoneReporting,  !- EnergyPlus Model Calling Point",
        "    CalcSCOP,                !- Program Name 1",
        "    CalcPUE;                 !- Program Name 2",

        "  EnergyManagementSystem:Program,",
        "    CalcSCOP,                !- Name",
        "    set cpair = @CpAirFnW Wout_SupplyFan,  !- Program Line 1",
        "    set FanSensHeat = (Mdot_SupplyFan * cpair ) * DeltaT_SupplyFan,  !- Program Line 2",
        "    set numerator = Qdot_DXCoil_Sens - FanSensHeat,  !- <none>",
        "    set denominator = Power_DX_Coil_Elec + Power_SupplyFan_Elec,  !- <none>",
        "    Set SCOP = numerator / denominator,  !- <none>",
        "    Set QdotNetSens = numerator,  !- <none>",
        "    Set PowerCRAC = denominator;  !- <none>",

        "  Output:EnergyManagementSystem,",
        "    Verbose,                 !- Actuator Availability Dictionary Reporting",
        "    Verbose,                 !- Internal Variable Availability Dictionary Reporting",
        "    Verbose;                 !- EMS Runtime Language Debug Output Level",

        "  EnergyManagementSystem:Sensor,",
        "    IT_Equip_power,          !- Name",
        "    Whole Building,          !- Output:Variable or Output:Meter Index Key Name",
        "    Facility Total Building Electric Demand Power;  !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:Sensor,",
        "    whole_building_power,    !- Name",
        "    Whole Building,          !- Output:Variable or Output:Meter Index Key Name",
        "    Facility Total Electric Demand Power;  !- Output:Variable or Output:Meter Name",

        "  EnergyManagementSystem:OutputVariable,",
        "    PUE,                     !- Name",
        "    PUE,                     !- EMS Variable Name",
        "    Averaged,                !- Type of Data in Variable",
        "    SystemTimeStep,          !- Update Frequency",
        "    ,                        !- EMS Program or Subroutine Name",
        "    ;                        !- Units",

        "  EnergyManagementSystem:GlobalVariable,",
        "    PUE;                     !- Erl Variable 1 Name",

        "  EnergyManagementSystem:Program,",
        "    CalcPUE,                 !- Name",
        "    IF IT_Equip_power > 0.0, !- Program Line 1",
        "    set PUE = whole_building_power / IT_Equip_power,  !- Program Line 2",
        "    ELSE,                    !- <none>",
        "    set PUE = 0.0,           !- <none>",
        "    ENDIF;                   !- <none>",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);

    HeatBalanceManager::GetZoneData(ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);

    DataHeatBalFanSys::MAT(1) = 24.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(state, outputFiles());
    DataZoneEquipment::GetZoneEquipmentData1(state);
    InternalHeatGains::CalcZoneITEq();
}