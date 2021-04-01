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

#include <exception>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
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
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/ScheduleManager.hh>

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

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    InternalHeatGains::GetInternalHeatGainsInput(*state);

    ASSERT_EQ(state->dataHeatBal->ZoneOtherEq.size(), 2u);

    for (unsigned long i = 1; i <= state->dataHeatBal->ZoneOtherEq.size(); ++i) {
        const DataHeatBalance::ZoneEquipData &equip = state->dataHeatBal->ZoneOtherEq(i);
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

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    ASSERT_THROW(InternalHeatGains::GetInternalHeatGainsInput(*state), std::runtime_error);

    std::string const error_string = delimited_string(
        {"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
         "   ** Severe  ** GetInternalHeatGains: OtherEquipment=\"OTHEREQ1\", Design Level is not allowed to be negative",
         "   **   ~~~   ** ... when a fuel type of FuelOilNo1 is specified.",
         "   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
         "   ...Summary of Errors that led to program termination:",
         "   ..... Reference severe error count=1",
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

    std::string error_string =
        delimited_string({"   ** Severe  ** <root>[OtherEquipment][OtherEq1][fuel_type] - \"Water\" - Failed to match against any enum values."});
    EXPECT_TRUE(compare_err_stream(error_string, true));

    bool ErrorsFound(false);

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    ASSERT_THROW(InternalHeatGains::GetInternalHeatGainsInput(*state), std::runtime_error);

    error_string = delimited_string(
        {"   ** Warning ** ProcessScheduleInput: Schedule:Constant=\"SCHEDULE1\", Blank Schedule Type Limits Name input -- will not be validated.",
         "   ** Severe  ** GetInternalHeatGains: OtherEquipment: invalid Fuel Type entered=WATER for Name=OTHEREQ1",
         "   **  Fatal  ** GetInternalHeatGains: Errors found in Getting Internal Gains Input, Program Stopped",
         "   ...Summary of Errors that led to program termination:",
         "   ..... Reference severe error count=2",
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

    ScheduleManager::ProcessScheduleInput(*state); // read schedules
    HeatBalanceManager::GetZoneData(*state, ErrorsFound1);
    ASSERT_FALSE(ErrorsFound1);

    state->dataScheduleMgr->ScheduleInputProcessed = true;
    state->dataScheduleMgr->Schedule(1).Used = true;

    state->dataScheduleMgr->Schedule(1).CurrentValue = 1.0;
    state->dataScheduleMgr->Schedule(1).MinValue = 1.0;
    state->dataScheduleMgr->Schedule(1).MaxValue = 1.0;
    state->dataScheduleMgr->Schedule(1).MaxMinSet = true;
    state->dataScheduleMgr->Schedule(2).Used = true;

    state->dataScheduleMgr->Schedule(2).CurrentValue = 131.8;
    state->dataScheduleMgr->Schedule(2).MinValue = 131.8;
    state->dataScheduleMgr->Schedule(2).MaxValue = 131.8;
    state->dataScheduleMgr->Schedule(2).MaxMinSet = true;
    InternalHeatGains::GetInternalHeatGainsInput(*state);

    EXPECT_FALSE(state->dataInternalHeatGains->ErrorsFound);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_ElectricEquipITE_BeginEnvironmentReset)
{

    std::string const idf_objects = delimited_string({
        "Zone,Main Zone;",

        "ZoneHVAC:EquipmentConnections,",
        "  Main Zone,                   !- Zone Name",
        "  Main Zone Equipment,         !- Zone Conditioning Equipment List Name",
        "  Main Zone Inlet Node,        !- Zone Air Inlet Node or NodeList Name",
        "  ,                            !- Zone Air Exhaust Node or NodeList Name",
        "  Main Zone Node,              !- Zone Air Node Name",
        "  Main Zone Outlet Node;       !- Zone Return Air Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        "  Main Zone Equipment,     !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "  Main Zone ATU,           !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  2,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
        "  ,                        !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",
        "  ZoneHVAC:Baseboard:Convective:Electric,  !- Zone Equipment 2 Object Type",
        "  Main Zone Baseboard,     !- Zone Equipment 2 Name",
        "  2,                       !- Zone Equipment 2 Cooling Sequence",
        "  1,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 2 Sequential Cooling Fraction Schedule Name",
        "  ;                        !- Zone Equipment 2 Sequential Heating Fraction Schedule Name",

        "ZoneHVAC:AirDistributionUnit,",
        "  Main Zone ATU,               !- Name",
        "  Main Zone Inlet Node,        !- Air Distribution Unit Outlet Node Name",
        "  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "  Main Zone VAV Air;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "  Main Zone VAV Air,           !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  Main Zone Inlet Node,    !- Air Outlet Node Name",
        "  Main Zone ATU In Node,   !- Air Inlet Node Name",
        "  8.5,                     !- Maximum Air Flow Rate {m3/s}",
        "  Constant,                !- Zone Minimum Air Flow Input Method",
        "  0.05;                    !- Constant Minimum Air Flow Fraction",

        "ZoneHVAC:Baseboard:Convective:Electric,",
        "  Main Zone Baseboard,     !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "  8000,                    !- Heating Design Capacity {W}",
        "  ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "  ,                        !- Fraction of Autosized Heating Design Capacity",
        "  0.97;                    !- Efficiency",

        "ElectricEquipment:ITE:AirCooled,",
        "  Data Center Servers,     !- Name",
        "  Main Zone,               !- Zone Name",
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
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(*state);
    InternalHeatGains::CalcZoneITEq(*state);
    Real64 InitialPower =
        state->dataHeatBal->ZoneITEq(1).CPUPower + state->dataHeatBal->ZoneITEq(1).FanPower + state->dataHeatBal->ZoneITEq(1).UPSPower;

    state->dataLoopNodes->Node(1).Temp = 45.0;
    InternalHeatGains::CalcZoneITEq(*state);
    Real64 NewPower = state->dataHeatBal->ZoneITEq(1).CPUPower + state->dataHeatBal->ZoneITEq(1).FanPower + state->dataHeatBal->ZoneITEq(1).UPSPower;
    ASSERT_NE(InitialPower, NewPower);
    HVACManager::ResetNodeData(*state);

    InternalHeatGains::CalcZoneITEq(*state);
    NewPower = state->dataHeatBal->ZoneITEq(1).CPUPower + state->dataHeatBal->ZoneITEq(1).FanPower + state->dataHeatBal->ZoneITEq(1).UPSPower;
    ASSERT_EQ(InitialPower, NewPower);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_CheckZoneComponentLoadSubtotals)
{

    std::string const idf_objects = delimited_string({
        "Zone,Main Zone;",

        "ZoneHVAC:EquipmentConnections,",
        "  Main Zone,                   !- Zone Name",
        "  Main Zone Equipment,         !- Zone Conditioning Equipment List Name",
        "  Main Zone Inlet Node,        !- Zone Air Inlet Node or NodeList Name",
        "  ,                            !- Zone Air Exhaust Node or NodeList Name",
        "  Main Zone Node,              !- Zone Air Node Name",
        "  Main Zone Outlet Node;       !- Zone Return Air Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        "  Main Zone Equipment,     !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "  Main Zone ATU,           !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  2,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
        "  ,                        !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",
        "  ZoneHVAC:Baseboard:Convective:Electric,  !- Zone Equipment 2 Object Type",
        "  Main Zone Baseboard,     !- Zone Equipment 2 Name",
        "  2,                       !- Zone Equipment 2 Cooling Sequence",
        "  1,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 2 Sequential Cooling Fraction Schedule Name",
        "  ;                        !- Zone Equipment 2 Sequential Heating Fraction Schedule Name",

        "ZoneHVAC:AirDistributionUnit,",
        "  Main Zone ATU,               !- Name",
        "  Main Zone Inlet Node,        !- Air Distribution Unit Outlet Node Name",
        "  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "  Main Zone VAV Air;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "  Main Zone VAV Air,           !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  Main Zone Inlet Node,    !- Air Outlet Node Name",
        "  Main Zone ATU In Node,   !- Air Inlet Node Name",
        "  8.5,                     !- Maximum Air Flow Rate {m3/s}",
        "  Constant,                !- Zone Minimum Air Flow Input Method",
        "  0.05;                    !- Constant Minimum Air Flow Fraction",

        "ZoneHVAC:Baseboard:Convective:Electric,",
        "  Main Zone Baseboard,     !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "  8000,                    !- Heating Design Capacity {W}",
        "  ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "  ,                        !- Fraction of Autosized Heating Design Capacity",
        "  0.97;                    !- Efficiency",

        "ElectricEquipment:ITE:AirCooled,",
        "  Data Center Servers,     !- Name",
        "  Main Zone,               !- Zone Name",
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
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    InternalHeatGains::GetInternalHeatGainsInput(*state);

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
        SetupZoneInternalGain(*state, zoneNum, DataHeatBalance::ccZoneIntGainDeviceTypes(gainType), "Gain", gainType, &convGains(gainType));
    }

    InternalHeatGains::UpdateInternalGainValues(*state);

    // Check total of all convective gains
    InternalHeatGains::SumAllInternalConvectionGains(*state, zoneNum, totConvGains);
    EXPECT_EQ(totConvGains, expectedTotConvGains);

    // Check subtotals used in zone component loads
    state->dataEnvrn->TotDesDays = 1;
    state->dataEnvrn->TotRunDesPersDays = 0;
    state->dataSize->CurOverallSimDay = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->NumOfTimeStepInHour = 10;
    state->dataGlobal->TimeStep = 1;
    OutputReportTabular::AllocateLoadComponentArrays(*state);
    int timeStepInDay = (state->dataGlobal->HourOfDay - 1) * state->dataGlobal->NumOfTimeStepInHour + state->dataGlobal->TimeStep;

    state->dataGlobal->CompLoadReportIsReq = true;
    state->dataGlobal->isPulseZoneSizing = false;
    InternalHeatGains::GatherComponentLoadsIntGain(*state);
    totConvGains = state->dataOutRptTab->peopleInstantSeq(state->dataSize->CurOverallSimDay, timeStepInDay, zoneNum) +
                   state->dataOutRptTab->lightInstantSeq(state->dataSize->CurOverallSimDay, timeStepInDay, zoneNum) +
                   state->dataOutRptTab->equipInstantSeq(state->dataSize->CurOverallSimDay, timeStepInDay, zoneNum) +
                   state->dataOutRptTab->refrigInstantSeq(state->dataSize->CurOverallSimDay, timeStepInDay, zoneNum) +
                   state->dataOutRptTab->waterUseInstantSeq(state->dataSize->CurOverallSimDay, timeStepInDay, zoneNum) +
                   state->dataOutRptTab->hvacLossInstantSeq(state->dataSize->CurOverallSimDay, timeStepInDay, zoneNum) +
                   state->dataOutRptTab->powerGenInstantSeq(state->dataSize->CurOverallSimDay, timeStepInDay, zoneNum);

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
        "Zone,Main Zone;",

        "ZoneHVAC:EquipmentConnections,",
        "  Main Zone,                   !- Zone Name",
        "  Main Zone Equipment,         !- Zone Conditioning Equipment List Name",
        "  Main Zone Inlet Node,        !- Zone Air Inlet Node or NodeList Name",
        "  ,                            !- Zone Air Exhaust Node or NodeList Name",
        "  Main Zone Node,              !- Zone Air Node Name",
        "  Main Zone Outlet Node;       !- Zone Return Air Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        "  Main Zone Equipment,     !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "  Main Zone ATU,           !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  2,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
        "  ,                        !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",
        "  ZoneHVAC:Baseboard:Convective:Electric,  !- Zone Equipment 2 Object Type",
        "  Main Zone Baseboard,     !- Zone Equipment 2 Name",
        "  2,                       !- Zone Equipment 2 Cooling Sequence",
        "  1,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 2 Sequential Cooling Fraction Schedule Name",
        "  ;                        !- Zone Equipment 2 Sequential Heating Fraction Schedule Name",

        "ZoneHVAC:AirDistributionUnit,",
        "  Main Zone ATU,               !- Name",
        "  Main Zone Inlet Node,        !- Air Distribution Unit Outlet Node Name",
        "  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "  Main Zone VAV Air;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "  Main Zone VAV Air,           !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  Main Zone Inlet Node,    !- Air Outlet Node Name",
        "  Main Zone ATU In Node,   !- Air Inlet Node Name",
        "  8.5,                     !- Maximum Air Flow Rate {m3/s}",
        "  Constant,                !- Zone Minimum Air Flow Input Method",
        "  0.05;                    !- Constant Minimum Air Flow Fraction",

        "ZoneHVAC:Baseboard:Convective:Electric,",
        "  Main Zone Baseboard,     !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "  8000,                    !- Heating Design Capacity {W}",
        "  ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "  ,                        !- Fraction of Autosized Heating Design Capacity",
        "  0.97;                    !- Efficiency",

        "ElectricEquipment:ITE:AirCooled,",
        "  Data Center Servers,     !- Name",
        "  Main Zone,               !- Zone Name",
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
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBal->ZnRpt.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(*state);

    state->dataLoopNodes->Node(1).Temp = 45.0;
    InternalHeatGains::CalcZoneITEq(*state);
    ASSERT_DOUBLE_EQ(state->dataHeatBal->ZoneITEq(1).AirOutletDryBulbT + state->dataHeatBal->ZoneITEq(1).ReturnApproachTemp,
                     state->dataHeatBal->Zone(1).AdjustedReturnTempByITE);
    ASSERT_DOUBLE_EQ(state->dataLoopNodes->Node(1).Temp + state->dataHeatBal->ZoneITEq(1).SupplyApproachTemp,
                     state->dataHeatBal->ZoneITEq(1).AirInletDryBulbT);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_ElectricEquipITE_DefaultCurves)
{

    std::string const idf_objects =
        delimited_string({"Zone,Main Zone;",

                          "ZoneHVAC:EquipmentConnections,",
                          "  Main Zone,                   !- Zone Name",
                          "  Main Zone Equipment,         !- Zone Conditioning Equipment List Name",
                          "  Main Zone Inlet Node,        !- Zone Air Inlet Node or NodeList Name",
                          "  ,                            !- Zone Air Exhaust Node or NodeList Name",
                          "  Main Zone Node,              !- Zone Air Node Name",
                          "  Main Zone Outlet Node;       !- Zone Return Air Node or NodeList Name",

                          "ZoneHVAC:EquipmentList,",
                          "  Main Zone Equipment,     !- Name",
                          "  SequentialLoad,          !- Load Distribution Scheme",
                          "  ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
                          "  Main Zone ATU,           !- Zone Equipment 1 Name",
                          "  1,                       !- Zone Equipment 1 Cooling Sequence",
                          "  2,                       !- Zone Equipment 1 Heating or No-Load Sequence",
                          "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
                          "  ,                        !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",
                          "  ZoneHVAC:Baseboard:Convective:Electric,  !- Zone Equipment 2 Object Type",
                          "  Main Zone Baseboard,     !- Zone Equipment 2 Name",
                          "  2,                       !- Zone Equipment 2 Cooling Sequence",
                          "  1,                       !- Zone Equipment 2 Heating or No-Load Sequence",
                          "  ,                        !- Zone Equipment 2 Sequential Cooling Fraction Schedule Name",
                          "  ;                        !- Zone Equipment 2 Sequential Heating Fraction Schedule Name",

                          "ZoneHVAC:AirDistributionUnit,",
                          "  Main Zone ATU,               !- Name",
                          "  Main Zone Inlet Node,        !- Air Distribution Unit Outlet Node Name",
                          "  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
                          "  Main Zone VAV Air;           !- Air Terminal Name",

                          "AirTerminal:SingleDuct:VAV:NoReheat,",
                          "  Main Zone VAV Air,           !- Name",
                          "  System Availability Schedule,  !- Availability Schedule Name",
                          "  Main Zone Inlet Node,    !- Air Outlet Node Name",
                          "  Main Zone ATU In Node,   !- Air Inlet Node Name",
                          "  8.5,                     !- Maximum Air Flow Rate {m3/s}",
                          "  Constant,                !- Zone Minimum Air Flow Input Method",
                          "  0.05;                    !- Constant Minimum Air Flow Fraction",

                          "ZoneHVAC:Baseboard:Convective:Electric,",
                          "  Main Zone Baseboard,     !- Name",
                          "  System Availability Schedule,  !- Availability Schedule Name",
                          "  HeatingDesignCapacity,   !- Heating Design Capacity Method",
                          "  8000,                    !- Heating Design Capacity {W}",
                          "  ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
                          "  ,                        !- Fraction of Autosized Heating Design Capacity",
                          "  0.97;                    !- Efficiency",

                          "ElectricEquipment:ITE:AirCooled,",
                          "  Data Center Servers,     !- Name",
                          "  Main Zone,               !- Zone Name",
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
                          "  Dimensionless;           !- Output Unit Type"});

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(*state);
    InternalHeatGains::CalcZoneITEq(*state);

    // If Electric Power Supply Efficiency Function of Part Load Ratio Curve Name is blank => always 1, so UPSPower is calculated as such
    Real64 DefaultUPSPower = (state->dataHeatBal->ZoneITEq(1).CPUPower + state->dataHeatBal->ZoneITEq(1).FanPower) *
                             max((1.0 - state->dataHeatBal->ZoneITEq(1).DesignUPSEfficiency), 0.0);

    ASSERT_EQ(DefaultUPSPower, state->dataHeatBal->ZoneITEq(1).UPSPower);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_CheckThermalComfortSchedules)
{

    bool WorkEffSchPresent; // true equals blank, false equals not blank
    bool CloInsSchPresent;  // true equals blank, false equals not blank
    bool AirVelSchPresent;  // true equals blank, false equals not blank
    bool FunctionCallResult;
    bool ExpectedResult;

    // Test 1: everything blank--should result in false result
    WorkEffSchPresent = true;
    CloInsSchPresent = true;
    AirVelSchPresent = true;
    ExpectedResult = false;
    FunctionCallResult = EnergyPlus::InternalHeatGains::CheckThermalComfortSchedules(WorkEffSchPresent, CloInsSchPresent, AirVelSchPresent);
    EXPECT_EQ(ExpectedResult, FunctionCallResult);

    // Additional Tests: test various combinations where at least one flag is not blank (false)--should result in a true result
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
        "Zone,Main Zone;",

        "ZoneHVAC:EquipmentConnections,",
        "  Main Zone,                   !- Zone Name",
        "  Main Zone Equipment,         !- Zone Conditioning Equipment List Name",
        "  Main Zone Inlet Node,        !- Zone Air Inlet Node or NodeList Name",
        "  ,                            !- Zone Air Exhaust Node or NodeList Name",
        "  Main Zone Node,              !- Zone Air Node Name",
        "  Main Zone Outlet Node;       !- Zone Return Air Node or NodeList Name",

        "ZoneHVAC:EquipmentList,",
        "  Main Zone Equipment,     !- Name",
        "  SequentialLoad,          !- Load Distribution Scheme",
        "  ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "  Main Zone ATU,           !- Zone Equipment 1 Name",
        "  1,                       !- Zone Equipment 1 Cooling Sequence",
        "  2,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 1 Sequential Cooling Fraction Schedule Name",
        "  ,                        !- Zone Equipment 1 Sequential Heating Fraction Schedule Name",
        "  ZoneHVAC:Baseboard:Convective:Electric,  !- Zone Equipment 2 Object Type",
        "  Main Zone Baseboard,     !- Zone Equipment 2 Name",
        "  2,                       !- Zone Equipment 2 Cooling Sequence",
        "  1,                       !- Zone Equipment 2 Heating or No-Load Sequence",
        "  ,                        !- Zone Equipment 2 Sequential Cooling Fraction Schedule Name",
        "  ;                        !- Zone Equipment 2 Sequential Heating Fraction Schedule Name",

        "ZoneHVAC:AirDistributionUnit,",
        "  Main Zone ATU,               !- Name",
        "  Main Zone Inlet Node,        !- Air Distribution Unit Outlet Node Name",
        "  AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "  Main Zone VAV Air;           !- Air Terminal Name",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "  Main Zone VAV Air,           !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  Main Zone Inlet Node,    !- Air Outlet Node Name",
        "  Main Zone ATU In Node,   !- Air Inlet Node Name",
        "  8.5,                     !- Maximum Air Flow Rate {m3/s}",
        "  Constant,                !- Zone Minimum Air Flow Input Method",
        "  0.05;                    !- Constant Minimum Air Flow Fraction",

        "ZoneHVAC:Baseboard:Convective:Electric,",
        "  Main Zone Baseboard,     !- Name",
        "  System Availability Schedule,  !- Availability Schedule Name",
        "  HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "  8000,                    !- Heating Design Capacity {W}",
        "  ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "  ,                        !- Fraction of Autosized Heating Design Capacity",
        "  0.97;                    !- Efficiency",

        "ScheduleTypeLimits,SchType1,0.0,1.0,Continuous,Dimensionless;",

        "Schedule:Constant,Schedule1,,1.0;",

        "  People,",
        "    Main Zone People,        !- Name",
        "    Main Zone,               !- Zone or ZoneList Name",
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
        "    Main Zone Lights,        !- Name",
        "    Main Zone,               !- Zone or ZoneList Name",
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
        "    Main Zone Electric Equipment,  !- Name",
        "    Main Zone,               !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    150.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000;                  !- Fraction Lost",

        "  GasEquipment,",
        "    Main Zone Gas Equipment,  !- Name",
        "    Main Zone,               !- Zone or ZoneList Name",
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
        "    Main Zone Hot Water Equipment,  !- Name",
        "    Main Zone,               !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    250.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000;                  !- Fraction Lost",

        "  SteamEquipment,",
        "    Main Zone Steam Equipment,  !- Name",
        "    Main Zone,               !- Zone or ZoneList Name",
        "    Schedule1,               !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    300.0,                   !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0.0000,                  !- Fraction Latent",
        "    0.5000,                  !- Fraction Radiant",
        "    0.0000;                  !- Fraction Lost",

        "  OtherEquipment,",
        "    Main Zone Other Equipment,  !- Name",
        "    OtherFuel1,              !- Fuel Type",
        "    Main Zone,               !- Zone or ZoneList Name",
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
        "    Main Zone BBHeat,           !- Name",
        "    Main Zone,                  !- Zone Name",
        "    Schedule1,               !- Schedule Name",
        "    1500,                    !- Capacity at Low Temperature {W}",
        "    0,                       !- Low Temperature {C}",
        "    500,                     !- Capacity at High Temperature {W}",
        "    10,                      !- High Temperature {C}",
        "    0.5,                     !- Fraction Radiant",
        "    Baseboard Heat;          !- End - Use Subcategory",

        "  ZoneContaminantSourceAndSink:CarbonDioxide,",
        "    CO2people,               !- Name",
        "    Main Zone,               !- Zone Name",
        "    0.0001125,               !- Design Generation Rate{ m3 / s }",
        "    Schedule1;               !- Schedule Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    bool ErrorsFound(false);

    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules
    state->dataEnvrn->DayOfYear_Schedule = 1;
    state->dataEnvrn->DayOfMonth = 1;
    state->dataEnvrn->DayOfWeek = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;
    ScheduleManager::UpdateScheduleValues(*state);

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    HeatBalanceManager::AllocateHeatBalArrays(*state);

    InternalHeatGains::GetInternalHeatGainsInput(*state);

    EXPECT_EQ(state->dataHeatBal->TotPeople, 1);
    EXPECT_EQ(state->dataHeatBal->TotLights, 1);
    EXPECT_EQ(state->dataHeatBal->TotElecEquip, 1);
    EXPECT_EQ(state->dataHeatBal->TotGasEquip, 1);
    EXPECT_EQ(state->dataHeatBal->TotHWEquip, 1);
    EXPECT_EQ(state->dataHeatBal->TotStmEquip, 1);
    EXPECT_EQ(state->dataHeatBal->TotOthEquip, 1);
    EXPECT_EQ(state->dataHeatBal->TotBBHeat, 1);

    EnergyPlus::createFacilityElectricPowerServiceObject(*state); // Needs to happen before InitInternalHeatGains

    // First time should be all good, because ZnRpt values initialize to zero
    InternalHeatGains::InitInternalHeatGains(*state);

    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsPower, 100.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecPower, 150.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).GasPower, 200.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).HWPower, 250.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).SteamPower, 300.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).BaseHeatPower, 1500.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).CO2Rate, 0.0001125);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ITEqSHI, 0);

    // Second time should should give the same answers, because everything should reset before accumulating
    InternalHeatGains::InitInternalHeatGains(*state);

    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).LtsPower, 100.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ElecPower, 150.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).GasPower, 200.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).HWPower, 250.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).SteamPower, 300.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).BaseHeatPower, 1500.0);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).CO2Rate, 0.0001125);
    EXPECT_EQ(state->dataHeatBal->ZnRpt(1).ITEqSHI, 0);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_AdjustedSupplyGoodInletNode)
{
    std::string const idf_objects = delimited_string({
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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(*state);
    ASSERT_FALSE(ErrorsFound);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_AdjustedSupplyBadInletNode)
{
    std::string const idf_objects = delimited_string({
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
        "    Inlet Node Not Found,    !- Supply Air Node Name",
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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    EXPECT_ANY_THROW(InternalHeatGains::GetInternalHeatGainsInput(*state));
}

TEST_F(EnergyPlusFixture, InternalHeatGains_FlowControlWithApproachTemperaturesGoodInletNode)
{
    std::string const idf_objects = delimited_string({
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

        "  ElectricEquipment:ITE:AirCooled,",
        "    Data Center Servers,     !- Name",
        "    Main Zone,               !- Zone Name",
        "    FlowControlWithApproachTemperatures,  !- Air Flow Calculation Method",
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
        "    ,                        !- Air Inlet Connection Type",
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
        "    ITE-UPS,                 !- Electric Power Supply End-Use Subcategory",
        "    2,                       !- Supply Temperature Difference {deltaC}",
        "    ,                        !- Supply Temperature Difference Schedule",
        "    -1,                      !- Return Temperature Difference {deltaC}",
        "    ;                        !- Return Temperature Difference Schedule",

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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(*state);
    ASSERT_FALSE(ErrorsFound);
}

TEST_F(EnergyPlusFixture, InternalHeatGains_FlowControlWithApproachTemperaturesBadInletNode)
{
    std::string const idf_objects = delimited_string({
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

        "  ElectricEquipment:ITE:AirCooled,",
        "    Data Center Servers,     !- Name",
        "    Main Zone,               !- Zone Name",
        "    FlowControlWithApproachTemperatures,  !- Air Flow Calculation Method",
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
        "    RoomAirModel,            !- Air Inlet Connection Type",
        "    ,                        !- Air Inlet Room Air Model Node Name",
        "    ,                        !- Air Outlet Room Air Model Node Name",
        "    Inlet Node Not Found,    !- Supply Air Node Name",
        "    0.1,                     !- Design Recirculation Fraction",
        "    Data Center Recirculation fLoadTemp,  !- Recirculation Function of Loading and Supply Temperature Curve Name",
        "    0.9,                     !- Design Electric Power Supply Efficiency",
        "    UPS Efficiency fPLR,     !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
        "    1,                       !- Fraction of Electric Power Supply Losses to Zone",
        "    ITE-CPU,                 !- CPU End-Use Subcategory",
        "    ITE-Fans,                !- Fan End-Use Subcategory",
        "    ITE-UPS,                 !- Electric Power Supply End-Use Subcategory",
        "    2,                       !- Supply Temperature Difference {deltaC}",
        "    ,                        !- Supply Temperature Difference Schedule",
        "    -1,                      !- Return Temperature Difference {deltaC}",
        "    ;                        !- Return Temperature Difference Schedule",

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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    ASSERT_ANY_THROW(InternalHeatGains::GetInternalHeatGainsInput(*state));
}

TEST_F(EnergyPlusFixture, InternalHeatGains_WarnMissingInletNode)
{
    std::string const idf_objects = delimited_string({
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
        "    RoomAirModel,            !- Air Inlet Connection Type",
        "    ,                        !- Air Inlet Room Air Model Node Name",
        "    ,                        !- Air Outlet Room Air Model Node Name",
        "    Inlet Node Not Found,    !- Supply Air Node Name",
        "    0.1,                     !- Design Recirculation Fraction",
        "    Data Center Recirculation fLoadTemp,  !- Recirculation Function of Loading and Supply Temperature Curve Name",
        "    0.9,                     !- Design Electric Power Supply Efficiency",
        "    UPS Efficiency fPLR,     !- Electric Power Supply Efficiency Function of Part Load Ratio Curve Name",
        "    1,                       !- Fraction of Electric Power Supply Losses to Zone",
        "    ITE-CPU,                 !- CPU End-Use Subcategory",
        "    ITE-Fans,                !- Fan End-Use Subcategory",
        "    ITE-UPS,                 !- Electric Power Supply End-Use Subcategory",
        "    2,                       !- Supply Temperature Difference {deltaC}",
        "    ,                        !- Supply Temperature Difference Schedule",
        "    -1,                      !- Return Temperature Difference {deltaC}",
        "    ;                        !- Return Temperature Difference Schedule",

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
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;

    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);
    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.008;

    InternalHeatGains::GetInternalHeatGainsInput(*state);
    ASSERT_FALSE(ErrorsFound);
}
