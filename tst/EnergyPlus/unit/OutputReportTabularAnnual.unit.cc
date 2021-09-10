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

// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/SQLiteFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportData.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/OutputReportTabularAnnual.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::OutputReportTabularAnnual;

TEST_F(EnergyPlusFixture, OutputReportTabularAnnual_GetInput)
{
    std::string const idf_objects = delimited_string({
        "Output:Table:Annual,",
        "Space Gains Annual Report, !- Name",
        "Filter1, !- Filter",
        "Schedule2, !- Schedule Name",
        "Zone People Total Heating Energy, !- Variable or Meter 1 Name",
        "SumOrAverage, !- Aggregation Type for Variable or Meter 1",
        "4, !- field Digits After Decimal 1",
        "Zone Lights Total Heating Energy, !- Variable or Meter 2 Name",
        "hoursNonZero, !- Aggregation Type for Variable or Meter 2",
        ", !- field Digits After Decimal 2",
        "Zone Electric Equipment Total Heating Energy; !- Variable or Meter 3 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->DoWeathSim = true;

    GetInputTabularAnnual(*state);

    EXPECT_EQ(state->dataOutputReportTabularAnnual->annualTables.size(), 1u);

    std::vector<AnnualTable>::iterator firstTable = state->dataOutputReportTabularAnnual->annualTables.begin();

    std::vector<std::string> tableParams = firstTable->inspectTable();

    EXPECT_EQ(tableParams[0], "SPACE GAINS ANNUAL REPORT"); // m_name
    EXPECT_EQ(tableParams[1], "FILTER1");                   //  m_filter
    EXPECT_EQ(tableParams[2], "SCHEDULE2");                 //  m_scheduleName

    std::vector<std::string> fieldSetParams = firstTable->inspectTableFieldSets(0);
    EXPECT_EQ(fieldSetParams[0], "ZONE PEOPLE TOTAL HEATING ENERGY");
    EXPECT_EQ(fieldSetParams[3], "4"); // m_showDigits
    EXPECT_EQ(fieldSetParams[8], "0"); // m_aggregate - 0 is sumOrAvg

    fieldSetParams = firstTable->inspectTableFieldSets(1);
    EXPECT_EQ(fieldSetParams[3], "2"); // m_showDigits (2 is the default if no value provided)
    EXPECT_EQ(fieldSetParams[8], "3"); // m_aggregate - 3 is hoursNonZero

    fieldSetParams = firstTable->inspectTableFieldSets(2);
    EXPECT_EQ(fieldSetParams[8], "0"); // m_aggregate - 0 is sumOrAvg is default if not included in idf input object
}

TEST_F(EnergyPlusFixture, OutputReportTabularAnnual_SetupGathering)
{
    std::string const idf_objects = delimited_string({
        "Output:Table:Annual,",
        "Space Gains Annual Report, !- Name",
        ", !- Filter",
        ", !- Schedule Name",
        "Exterior Lights Electric Energy, !- Variable or Meter 1 Name",
        "SumOrAverage, !- Aggregation Type for Variable or Meter 1",
        "4, !- field Digits After Decimal 1",
        "Exterior Lights Electric Power, !- Variable or Meter 2 Name",
        "hoursNonZero, !- Aggregation Type for Variable or Meter 2",
        ", !- field Digits After Decimal 2",
        "Zone Electric Equipment Total Heating Energy; !- Variable or Meter 3 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    Real64 extLitPow;
    Real64 extLitUse;

    SetupOutputVariable(*state,
                        "Exterior Lights Electric Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite3",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Power",
                        OutputProcessor::Unit::W,
                        extLitPow,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        "Lite1");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Power",
                        OutputProcessor::Unit::W,
                        extLitPow,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        "Lite2");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Power",
                        OutputProcessor::Unit::W,
                        extLitPow,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        "Lite3");

    state->dataGlobal->DoWeathSim = true;

    GetInputTabularAnnual(*state); // this also calls setupGathering
    EXPECT_EQ(state->dataOutputReportTabularAnnual->annualTables.size(), 1u);

    std::vector<AnnualTable>::iterator firstTable = state->dataOutputReportTabularAnnual->annualTables.begin();
    std::vector<std::string> fieldSetParams = firstTable->inspectTableFieldSets(0);

    EXPECT_EQ(fieldSetParams[0], "EXTERIOR LIGHTS ELECTRIC ENERGY");
    EXPECT_EQ(fieldSetParams[2], "J"); // m_varUnits
    EXPECT_EQ(fieldSetParams[4], "2"); // m_typeOfVar
    EXPECT_EQ(fieldSetParams[5], "3"); // m_keyCount
    EXPECT_EQ(fieldSetParams[6], "2"); // m_varAvgSum
    EXPECT_EQ(fieldSetParams[7], "1"); // m_varStepType
}

TEST_F(EnergyPlusFixture, OutputReportTabularAnnual_GatherResults)
{
    std::string const idf_objects = delimited_string({
        "Output:Table:Annual,",
        "Space Gains Annual Report, !- Name",
        ", !- Filter",
        ", !- Schedule Name",
        "Exterior Lights Electric Energy, !- Variable or Meter 1 Name",
        "SumOrAverage, !- Aggregation Type for Variable or Meter 1",
        "4, !- field Digits After Decimal 1",
        "Exterior Lights Electric Power, !- Variable or Meter 2 Name",
        "Maximum, !- Aggregation Type for Variable or Meter 2",
        ", !- field Digits After Decimal 2",
        "Zone Electric Equipment Total Heating Energy; !- Variable or Meter 3 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    Real64 extLitPow;
    Real64 extLitUse;

    SetupOutputVariable(*state,
                        "Exterior Lights Electric Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite1",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite2",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Energy",
                        OutputProcessor::Unit::J,
                        extLitUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite3",
                        _,
                        "Electricity",
                        "Exterior Lights",
                        "General");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Power",
                        OutputProcessor::Unit::W,
                        extLitPow,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        "Lite1");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Power",
                        OutputProcessor::Unit::W,
                        extLitPow,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        "Lite2");
    SetupOutputVariable(*state,
                        "Exterior Lights Electric Power",
                        OutputProcessor::Unit::W,
                        extLitPow,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        "Lite3");

    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 0.25;

    GetInputTabularAnnual(*state);
    EXPECT_EQ(state->dataOutputReportTabularAnnual->annualTables.size(), 1u);

    extLitPow = 2.01;
    extLitUse = 1.01;

    // UpdateDataandReport( 1 ); not sure if this is needed
    GatherAnnualResultsForTimeStep(*state, OutputProcessor::TimeStepType::TimeStepZone);

    // STOPPPED HERE. NOT SEEING THE POWER VARIABLE SHOWING UP

    std::vector<AnnualTable>::iterator firstTable = state->dataOutputReportTabularAnnual->annualTables.begin();
    std::vector<std::string> fieldSetParams = firstTable->inspectTableFieldSets(0);
}

TEST_F(EnergyPlusFixture, OutputReportTabularAnnual_GatherResults_MinMaxHrsShown)
{
    state->dataGlobal->TimeStepZone = 1.0;
    state->dataHVACGlobal->TimeStepSys = 1.0;

    state->dataOutputProcessor->NumEnergyMeters = 2;
    state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
    state->dataOutputProcessor->EnergyMeters(1).Name = "HEATING:MYTH:VARIABLE";
    state->dataOutputProcessor->EnergyMeters(2).Name = "ELECTRICITY:MYTH";

    std::vector<AnnualTable> annualTables;
    annualTables.push_back(AnnualTable(*state, "PEAK ELECTRICTY ANNUAL MYTH REPORT", "", ""));
    annualTables.back().addFieldSet("HEATING:MYTH:VARIABLE", AnnualFieldSet::AggregationKind::hoursPositive, 2);
    annualTables.back().addFieldSet("ELECTRICITY:MYTH", AnnualFieldSet::AggregationKind::maximumDuringHoursShown, 2);
    annualTables.back().setupGathering(*state);

    state->dataOutputProcessor->EnergyMeters(1).CurTSValue = -10.;
    state->dataOutputProcessor->EnergyMeters(2).CurTSValue = 50.;
    annualTables.back().gatherForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);

    std::vector<std::string> fieldSetParams = annualTables.back().inspectTableFieldSets(0);
    EXPECT_EQ(fieldSetParams[0], "HEATING:MYTH:VARIABLE"); // m_colHead
    EXPECT_EQ(fieldSetParams[13], "0.000000");             // m_cell[0].result

    fieldSetParams = annualTables.back().inspectTableFieldSets(1);
    EXPECT_EQ(fieldSetParams[0], "ELECTRICITY:MYTH");                  // m_colHead
    EXPECT_EQ(fieldSetParams[13].std::string::substr(0, 6), "-99000"); // m_cell[0].result

    state->dataOutputProcessor->EnergyMeters(1).CurTSValue = 15.;
    state->dataOutputProcessor->EnergyMeters(2).CurTSValue = 55.;
    annualTables.back().gatherForTimestep(*state, OutputProcessor::TimeStepType::TimeStepZone);

    fieldSetParams = annualTables.back().inspectTableFieldSets(0);
    EXPECT_EQ(fieldSetParams[0], "HEATING:MYTH:VARIABLE"); // m_colHead
    EXPECT_EQ(fieldSetParams[13], "1.000000");             // m_cell[0].result

    fieldSetParams = annualTables.back().inspectTableFieldSets(1);
    EXPECT_EQ(fieldSetParams[0], "ELECTRICITY:MYTH");                  // m_colHead
    EXPECT_EQ(fieldSetParams[13].std::string::substr(0, 6), "0.0152"); // m_cell[0].result
}

TEST_F(EnergyPlusFixture, OutputReportTabularAnnual_columnHeadersToTitleCase)
{
    std::string const idf_objects = delimited_string({
        "Output:Table:Annual,",
        "Test Report, !- Name",
        ", !- Filter",
        ", !- Schedule Name",
        "OnPeakTime, !- Variable or Meter 1 Name",
        "HoursNonZero, !- Aggregation Type for Variable or Meter 1",
        "0, !- field Digits After Decimal 1",
        "Electricity:Facility, !- Variable or Meter 2 Name",
        "SumOrAverageDuringHoursShown, !- Aggregation Type for Variable or Meter 2",
        ", !- field Digits After Decimal 2",
        "Misc Facility Electric Energy, !- Variable or Meter 3 Name",
        "SumOrAverage, !- Aggregation Type for Variable or Meter 3",
        "0; !- field Digits After Decimal 3",
        "",
        "Schedule:Compact,",
        "    OnPeakTime,              !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Weekdays SummerDesignDay,  !- Field 2",
        "    Until: 12:00, 0.0,       !- Field 4",
        "    Until: 20:00, 1.0,       !- Field 6",
        "    Until: 24:00, 0.0,       !- Field 8",
        "    For: AllOtherDays,       !- Field 9",
        "    Until: 24:00, 0.0;       !- Field 11",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    Real64 facilUse;
    SetupOutputVariable(*state,
                        "Misc Facility Electric Energy",
                        OutputProcessor::Unit::J,
                        facilUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite1",
                        _,
                        "Electricity",
                        "Facility",
                        "General"); // create an electric meter

    state->dataOutputProcessor->NumEnergyMeters = 2;
    state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
    state->dataOutputProcessor->EnergyMeters(1).Name = "Electricity:Facility"; //"ELECTRICITY:FACILITY";
    state->dataOutputProcessor->EnergyMeters(2).Name = "ELECTRICITY:LIGHTING";

    state->dataGlobal->DoWeathSim = true;

    OutputReportTabularAnnual::GetInputTabularAnnual(*state);

    EXPECT_EQ(state->dataOutputReportTabularAnnual->annualTables.size(), 1u);

    std::vector<AnnualTable>::iterator firstTable = state->dataOutputReportTabularAnnual->annualTables.begin();

    firstTable->columnHeadersToTitleCase(*state);

    std::vector<std::string> fieldSetParams = firstTable->inspectTableFieldSets(0);
    EXPECT_EQ(fieldSetParams[0], "ONPEAKTIME"); // m_colHead
    EXPECT_EQ(fieldSetParams[4], "4");          // m_typeOfVar = OutputProcessor::VarType_Schedule

    fieldSetParams = firstTable->inspectTableFieldSets(1);
    EXPECT_EQ(fieldSetParams[0], "Electricity:Facility"); // m_colHead
    EXPECT_EQ(fieldSetParams[4], "3");                    // m_typeOfVar = OutputProcessor::VarType_Meter

    fieldSetParams = firstTable->inspectTableFieldSets(2);
    EXPECT_EQ(fieldSetParams[0], "Misc Facility Electric Energy"); // m_colHead
    EXPECT_EQ(fieldSetParams[4], "2");                             // m_typeOfVar = OutputProcessor::VarType_Real
}

TEST_F(EnergyPlusFixture, OutputReportTabularAnnual_invalidAggregationOrder)
{
    std::string const idf_objects = delimited_string({
        "Output:Table:Annual,",
        "Test Report, !- Name",
        ", !- Filter",
        ", !- Schedule Name",
        "Electricity:Facility, !- Variable or Meter 2 Name",
        "SumOrAverageDuringHoursShown, !- Aggregation Type for Variable or Meter 2",
        ", !- field Digits After Decimal 2",
        "Misc Facility Electric Energy, !- Variable or Meter 3 Name",
        "SumOrAverage, !- Aggregation Type for Variable or Meter 3",
        "0; !- field Digits After Decimal 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    Real64 facilUse;
    SetupOutputVariable(*state,
                        "Misc Facility Electric Energy",
                        OutputProcessor::Unit::J,
                        facilUse,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Summed,
                        "Lite1",
                        _,
                        "Electricity",
                        "Facility",
                        "General"); // create an electric meter

    state->dataOutputProcessor->NumEnergyMeters = 2;
    state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
    state->dataOutputProcessor->EnergyMeters(1).Name = "Electricity:Facility"; //"ELECTRICITY:FACILITY";
    state->dataOutputProcessor->EnergyMeters(2).Name = "ELECTRICITY:LIGHTING";

    state->dataGlobal->DoWeathSim = true;

    OutputReportTabularAnnual::GetInputTabularAnnual(*state);

    EXPECT_EQ(state->dataOutputReportTabularAnnual->annualTables.size(), 1u);

    std::vector<AnnualTable>::iterator firstTable = state->dataOutputReportTabularAnnual->annualTables.begin();

    EXPECT_TRUE(firstTable->invalidAggregationOrder(*state));
}

TEST_F(SQLiteFixture, OutputReportTabularAnnual_CurlyBraces)
{
    // Test for #8921

    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    std::string const idf_objects = delimited_string({
        "Output:Table:Annual,",
        "  ANNUAL EXAMPLE,                         !- Name",
        "  ,                                       !- Filter",
        "  ,                                       !- Schedule Name",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 1",
        "  SumOrAverage,                           !- Aggregation Type for Variable or Meter 1",
        "  2,                                      !- Digits After Decimal 1",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 2",
        "  Maximum,                                !- Aggregation Type for Variable or Meter 2",
        "  2,                                      !- Digits After Decimal 2",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 3",
        "  Minimum,                                !- Aggregation Type for Variable or Meter 3",
        "  2,                                      !- Digits After Decimal 3",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 4",
        "  ValueWhenMaximumOrMinimum,              !- Aggregation Type for Variable or Meter 4",
        "  2,                                      !- Digits After Decimal 4",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 5",
        "  HoursNonZero,                           !- Aggregation Type for Variable or Meter 5",
        "  2,                                      !- Digits After Decimal 5",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 6",
        "  HoursZero,                              !- Aggregation Type for Variable or Meter 6",
        "  2,                                      !- Digits After Decimal 6",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 7",
        "  HoursPositive,                          !- Aggregation Type for Variable or Meter 7",
        "  2,                                      !- Digits After Decimal 7",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 8",
        "  HoursNonPositive,                       !- Aggregation Type for Variable or Meter 8",
        "  2,                                      !- Digits After Decimal 8",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 9",
        "  HoursNegative,                          !- Aggregation Type for Variable or Meter 9",
        "  2,                                      !- Digits After Decimal 9",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 10",
        "  HoursNonNegative,                       !- Aggregation Type for Variable or Meter 10",
        "  2,                                      !- Digits After Decimal 10",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 11",
        "  HourInTenBinsMinToMax,                  !- Aggregation Type for Variable or Meter 11",
        "  2,                                      !- Digits After Decimal 11",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 12",
        "  HourInTenBinsZeroToMax,                 !- Aggregation Type for Variable or Meter 12",
        "  2,                                      !- Digits After Decimal 12",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 13",
        "  HourInTenBinsMinToZero,                 !- Aggregation Type for Variable or Meter 13",
        "  2,                                      !- Digits After Decimal 13",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 14",
        "  SumOrAverageDuringHoursShown,           !- Aggregation Type for Variable or Meter 14",
        "  2,                                      !- Digits After Decimal 14",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 15",
        "  MaximumDuringHoursShown,                !- Aggregation Type for Variable or Meter 15",
        "  2,                                      !- Digits After Decimal 15",
        "  Electricity:Facility,                   !- Variable or Meter or EMS Variable or Field Name 16",
        "  MinimumDuringHoursShown,                !- Aggregation Type for Variable or Meter 16",
        "  2;                                      !- Digits After Decimal 16",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataOutputProcessor->NumEnergyMeters = 1;
    state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
    state->dataOutputProcessor->EnergyMeters(1).Name = "Electricity:Facility";

    state->dataGlobal->DoWeathSim = true;
    state->dataGlobal->TimeStepZone = 0.25;
    state->dataGlobal->TimeStepZoneSec = state->dataGlobal->TimeStepZone * 60.0;

    OutputReportTabularAnnual::GetInputTabularAnnual(*state);
    EXPECT_EQ(state->dataOutputReportTabularAnnual->annualTables.size(), 1u);

    OutputReportTabularAnnual::WriteAnnualTables(*state);

    auto columnHeaders = queryResult(
        R"(SELECT DISTINCT(ColumnName) FROM TabularDataWithStrings
             WHERE ReportName LIKE "ANNUAL EXAMPLE%")",
        "TabularDataWithStrings");
    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    // 17 agg types for the same variable requested above + the {TIMESTAMP} ones (but distinct, so counts as 1)
    // + the BIN A TO BIN J ones
    EXPECT_EQ(36, columnHeaders.size());

    auto missingBracesHeaders = queryResult(
        R"(SELECT DISTINCT(ColumnName) FROM TabularDataWithStrings
             WHERE ReportName LIKE "ANNUAL EXAMPLE%"
             AND ColumnName LIKE "%{%" AND ColumnName NOT LIKE "%}%")",
        "TabularDataWithStrings");
    state->dataSQLiteProcedures->sqlite->sqliteCommit();

    // Should be none!
    for (auto &col : missingBracesHeaders) {
        std::string colHeader = col[0];
        EXPECT_TRUE(false) << "Missing braces in monthly table for : " << colHeader;
    }
}
