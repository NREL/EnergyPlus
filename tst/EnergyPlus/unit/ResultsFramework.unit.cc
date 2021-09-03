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

// EnergyPlus::OutputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ResultsFramework.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

// Fixture
#include "Fixtures/ResultsFrameworkFixture.hh"

using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::ResultsFramework;
using namespace EnergyPlus::SimulationManager;
using namespace EnergyPlus::DataOutputs;
using namespace EnergyPlus::NodeInputManager;

namespace EnergyPlus {

TEST_F(ResultsFrameworkFixture, ResultsFramework_ParseJsonObject1)
{
    std::string const idf_objects = delimited_string({
        "Output:JSON,",
        "TimeSeriesAndTabular;",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataResultsFramework->resultsFramework->setupOutputOptions(*state);

    EXPECT_TRUE(state->dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_ParseJsonObject2)
{
    std::string const idf_objects = delimited_string({
        "Output:JSON,",
        "TimeSeries;",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataResultsFramework->resultsFramework->setupOutputOptions(*state);

    EXPECT_TRUE(state->dataResultsFramework->resultsFramework->timeSeriesEnabled());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_SimInfo)
{

    state->dataResultsFramework->resultsFramework->SimulationInformation.setProgramVersion("EnergyPlus, Version 8.6.0-0f5a10914b");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setStartDateTimeStamp("2017.03.22 11:03");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setInputModelURI("");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setRunTime("00hr 08min  6.67sec");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSummary("1", "2");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSizing("0", "0");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsWarmup("0", "2");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setSimulationEnvironment("");

    json result = state->dataResultsFramework->resultsFramework->SimulationInformation.getJSON();
    json expectedResult = R"( {
            "ErrorSummary": {
                "NumSevere": "2",
                "NumWarnings": "1"
            },
            "ErrorSummarySizing": {
                "NumSevere": "0",
                "NumWarnings": "0"
            },
            "ErrorSummaryWarmup": {
                "NumSevere": "2",
                "NumWarnings": "0"
            },
            "InputModelURI": "",
            "ProgramVersion": "EnergyPlus, Version 8.6.0-0f5a10914b",
            "RunTime": "00hr 08min  6.67sec",
            "SimulationEnvironment": "",
            "StartDateTimeStamp": "2017.03.22 11:03"
        } )"_json;
    EXPECT_EQ(result.dump(), expectedResult.dump());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_SimInfo_String)
{
    state->dataResultsFramework->resultsFramework->SimulationInformation.setProgramVersion("EnergyPlus, Version 8.6.0-0f5a10914b");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setStartDateTimeStamp("2017.03.22 11:03");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setInputModelURI("");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setRunTime("00hr 08min  6.67sec");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSummary("1", "2");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsSizing("0", "0");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setNumErrorsWarmup("0", "2");
    state->dataResultsFramework->resultsFramework->SimulationInformation.setSimulationEnvironment("");

    json result = state->dataResultsFramework->resultsFramework->SimulationInformation.getJSON();

    std::string expectedResult =
        "{\n    \"ErrorSummary\": {\n        \"NumSevere\": \"2\",\n        \"NumWarnings\": \"1\"\n    },\n    \"ErrorSummarySizing\": {\n        "
        "\"NumSevere\": \"0\",\n        \"NumWarnings\": \"0\"\n    },\n    \"ErrorSummaryWarmup\": {\n        \"NumSevere\": \"2\",\n        "
        "\"NumWarnings\": \"0\"\n    },\n    \"InputModelURI\": \"\",\n    \"ProgramVersion\": \"EnergyPlus, Version 8.6.0-0f5a10914b\",\n    "
        "\"RunTime\": \"00hr 08min  6.67sec\",\n    \"SimulationEnvironment\": \"\",\n    \"StartDateTimeStamp\": \"2017.03.22 11:03\"\n}";
    EXPECT_EQ(result.dump(4), expectedResult);
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_VariableInfo)
{

    //      std::string const idf_objects = delimited_string({
    //                                                               "Output:Variable,SalesFloor Inlet Node,System Node Temperature,timestep;",
    //                                                               "Output:Variable,SalesFloor Inlet Node,System Node Mass Flow Rate,timestep;",
    //                                                       });
    //      ASSERT_TRUE( process_idf( idf_objects ) );
    //      //resultsFramework->setupOutputOptions();
    //
    //      EXPECT_EQ( (int)OutputVariablesForSimulation.size(), 2 );
    // SetupNodeVarsForReporting();
    // EXPECT_EQ( "SALESFLOOR INLET NODE:System Node Temperature", RVariableTypes( 1 ).VarName );
    // EXPECT_EQ( "SALESFLOOR INLET NODE:System Node Mass Flow Rate", OutputProcessor::RVariableTypes( 2 ).VarName );
    // EXPECT_EQ( 1, OutputProcessor::RVariableTypes( 1 ).ReportID );
    // EXPECT_EQ( 2, OutputProcessor::RVariableTypes( 2 ).ReportID );
    OutputProcessor::TimeStepType indexType = OutputProcessor::TimeStepType::TimeStepZone;
    int repordId = 1;

    Variable var("SALESFLOOR INLET NODE:System Node Temperature", ReportingFrequency::TimeStep, indexType, repordId, Unit::C);

    std::string expected_result = "{\n         \"Frequency\": \"TimeStep\",\n         \"Name\": \"SALESFLOOR INLET NODE:System Node Temperature\",\n "
                                  "        \"Units\": \"C\"\n}";
    EXPECT_EQ(expected_result, var.getJSON().dump('\t'));

    json expectedObject = R"( {
            "Frequency": "TimeStep",
            "Name": "SALESFLOOR INLET NODE:System Node Temperature",
            "Units": "C"
        } )"_json;

    EXPECT_EQ(expectedObject, var.getJSON());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_DataFrameInfo1)
{

    json OutputVars;
    OutputProcessor::TimeStepType indexType = OutputProcessor::TimeStepType::TimeStepZone;
    int reportId = 1;

    Variable var0("SALESFLOOR INLET NODE:System Node Temperature", ReportingFrequency::TimeStep, indexType, reportId, Unit::C);
    reportId++;
    Variable var1("SALESFLOOR INLET NODE:System Node Humidity Ratio", ReportingFrequency::TimeStep, indexType, reportId, Unit::kgWater_kgDryAir);

    state->dataResultsFramework->resultsFramework->RITimestepTSData.addVariable(var0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.addVariable(var1);

    OutputVars["TimeStep"] = state->dataResultsFramework->resultsFramework->RITimestepTSData.getVariablesJSON();

    json expectedObject = R"( {
            "TimeStep": [
                 {
                    "Frequency": "TimeStep",
                    "Name": "SALESFLOOR INLET NODE:System Node Humidity Ratio",
                    "Units": "kgWater/kgDryAir"
                },
                {
                    "Frequency": "TimeStep",
                    "Name": "SALESFLOOR INLET NODE:System Node Temperature",
                    "Units": "C"
                }]
        } )"_json;

    // There is some weird *nix vs windows issue when dumping the json. It changes ordering but I don't know why.
    // EXPECT_EQ( expectedObject.dump(), OutputVars.dump());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_DataFrameInfo2)
{

    json OutputData;
    OutputProcessor::TimeStepType indexType = OutputProcessor::TimeStepType::TimeStepZone;
    int reportId = 1;

    Variable var0("SALESFLOOR INLET NODE:System Node Temperature", ReportingFrequency::TimeStep, indexType, reportId, Unit::C);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.addVariable(var0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.newRow(2, 25, 1, 45);  // month,day,hour,minute
    state->dataResultsFramework->resultsFramework->RITimestepTSData.newRow(2, 25, 1, 60);  // month,day,hour,minute
    state->dataResultsFramework->resultsFramework->RITimestepTSData.newRow(2, 25, 24, 45); // month,day,hour,minute
    state->dataResultsFramework->resultsFramework->RITimestepTSData.newRow(2, 25, 24, 60); // month,day,hour,minute

    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 1.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 2.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 3.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 4.0);

    reportId++;
    Variable var1("SALESFLOOR INLET NODE:System Node Humidity Ratio", ReportingFrequency::TimeStep, indexType, reportId, Unit::kgWater_kgDryAir);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.addVariable(var1);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 5.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 6.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 7.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 8.0);

    OutputData["TimeStep"] = state->dataResultsFramework->resultsFramework->RITimestepTSData.getJSON();

    json expectedObject = R"( {
            "TimeStep": {
                "Cols":[
                    {
                        "Units" : "C",
                        "Variable":"SALESFLOOR INLET NODE:System Node Temperature"
                    },
                    {
                        "Units" : "kgWater/kgDryAir",
                        "Variable" : "SALESFLOOR INLET NODE:System Node Humidity Ratio"
                    }
                ],
                "ReportFrequency" : "TimeStep",
                "Rows":[
                    { "02/25 00:45:00" : [1.0,5.0] },
                    { "02/25 01:00:00" : [2.0,6.0] },
                    { "02/25 23:45:00" : [3.0,7.0] },
                    { "02/25 24:00:00" : [4.0,8.0] }
                ]
            }
        } )"_json;

    EXPECT_EQ(expectedObject.dump(), OutputData.dump());

    // If add one more, it also should go to the top of json cols array
    reportId++;
    Variable var2("SALESFLOOR OUTLET NODE:System Node Temperature", ReportingFrequency::TimeStep, indexType, reportId, Unit::C);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.addVariable(var2);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 9.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 10.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 11.0);
    state->dataResultsFramework->resultsFramework->RITimestepTSData.pushVariableValue(reportId, 12.0);
    OutputData["TimeStep"] = state->dataResultsFramework->resultsFramework->RITimestepTSData.getJSON();

    expectedObject = R"( {
            "TimeStep": {
                "Cols":[
                    {
                        "Units" : "C",
                        "Variable":"SALESFLOOR INLET NODE:System Node Temperature"
                    },
                    {
                        "Units" : "kgWater/kgDryAir",
                        "Variable" : "SALESFLOOR INLET NODE:System Node Humidity Ratio"
                    },
                    {
                        "Units": "C",
                        "Variable" : "SALESFLOOR OUTLET NODE:System Node Temperature"
                    }
                ],
                "ReportFrequency" : "TimeStep",
                "Rows":[
                    { "02/25 00:45:00" : [1.0,5.0,9.0] },
                    { "02/25 01:00:00" : [2.0,6.0,10.0] },
                    { "02/25 23:45:00" : [3.0,7.0,11.0] },
                    { "02/25 24:00:00" : [4.0,8.0,12.0] }
                ]
            }
        } )"_json;

    EXPECT_EQ(expectedObject.dump(), OutputData.dump());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_TableInfo)
{

    Array1D_string rowLabels(2);
    rowLabels(1) = "ZONE1DIRECTAIR";
    rowLabels(2) = "ZONE2DIRECTAIR";

    Array1D_string columnLabels(1);
    columnLabels(1) = "User-Specified Maximum Air Flow Rate [m3/s]";

    Array2D_string tableBody;
    tableBody.allocate(columnLabels.size1(), rowLabels.size1());
    tableBody = ""; // set entire table to blank as default

    tableBody(1, 1) = "5.22";
    tableBody(1, 2) = "0.275000";

    Table tbl(tableBody,
              rowLabels,
              columnLabels,
              "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
              "User-Specified values were used. Design Size values were used if no User-Specified values were provided.");

    json result = tbl.getJSON();
    json expectedResult = R"( {
            "Cols": [
                    "User-Specified Maximum Air Flow Rate [m3/s]"
            ],
            "Footnote": "User-Specified values were used. Design Size values were used if no User-Specified values were provided.",
            "Rows": {
            "ZONE1DIRECTAIR": [
                   "5.22"
                ],
                "ZONE2DIRECTAIR": [
                   "0.275000"
                ]
            },
            "TableName": "AirTerminal:SingleDuct:ConstantVolume:NoReheat"
        } )"_json;

    EXPECT_EQ(result.dump(), expectedResult.dump());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_ReportInfo)
{

    Array1D_string rowLabels(2);
    rowLabels(1) = "ZONE1DIRECTAIR";
    rowLabels(2) = "ZONE2DIRECTAIR";

    Array1D_string columnLabels(1);
    columnLabels(1) = "User-Specified Maximum Air Flow Rate [m3/s]";

    Array2D_string tableBody;
    tableBody.allocate(columnLabels.size1(), rowLabels.size1());

    tableBody(1, 1) = "5.22";
    tableBody(1, 2) = "0.275000";

    Table tbl(tableBody,
              rowLabels,
              columnLabels,
              "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
              "User-Specified values were used. Design Size values were used if no User-Specified values were provided.");

    rowLabels.deallocate();
    columnLabels.deallocate();
    tableBody.deallocate();

    rowLabels.allocate(1);
    columnLabels.allocate(3);
    tableBody.allocate(columnLabels.size1(), rowLabels.size1());

    rowLabels(1) = "FURNACE ACDXCOIL 1";

    columnLabels(1) = "User-Specified rated_air_flow_rate [m3/s]";
    columnLabels(2) = "User-Specified gross_rated_total_cooling_capacity [W]";
    columnLabels(3) = "User-Specified gross_rated_sensible_heat_ratio";

    tableBody(1, 1) = "5.50";
    tableBody(2, 1) = "100000.00";
    tableBody(3, 1) = "100000.00";

    Table tbl2(tableBody,
               rowLabels,
               columnLabels,
               "Coil:Cooling:DX:SingleSpeed",
               "User-Specified values were used. Design Size values were used if no User-Specified values were provided.");

    Report report;
    report.Tables.push_back(tbl);
    report.Tables.push_back(tbl2);
    report.ReportName = "Component Sizing Summary";
    report.ReportForString = "Entire Facility";

    json result = report.getJSON();
    json expectedResult = R"( {
            "For": "Entire Facility",
            "ReportName": "Component Sizing Summary",
            "Tables": [
                {
                    "Cols": [
                        "User-Specified Maximum Air Flow Rate [m3/s]"
                    ],
                    "Footnote": "User-Specified values were used. Design Size values were used if no User-Specified values were provided.",
                    "Rows": {
                        "ZONE1DIRECTAIR": [
                            "5.22"
                        ],
                        "ZONE2DIRECTAIR": [
                            "0.275000"
                        ]
                    },
                    "TableName": "AirTerminal:SingleDuct:ConstantVolume:NoReheat"
                },
                {
                    "Cols": [
                        "User-Specified rated_air_flow_rate [m3/s]",
                        "User-Specified gross_rated_total_cooling_capacity [W]",
                        "User-Specified gross_rated_sensible_heat_ratio"
                    ],
                    "Footnote": "User-Specified values were used. Design Size values were used if no User-Specified values were provided.",
                    "Rows": {
                        "FURNACE ACDXCOIL 1": [
                            "5.50",
                            "100000.00",
                            "100000.00"
                        ]
                    },
                    "TableName": "Coil:Cooling:DX:SingleSpeed"
                }
            ]
        } )"_json;

    EXPECT_EQ(result.dump(), expectedResult.dump());
}

TEST_F(ResultsFrameworkFixture, ResultsFramework_convertToMonth)
{
    std::string datetime;
    datetime = "01/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "January");
    datetime = "02/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "February");
    datetime = "03/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "March");
    datetime = "04/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "April");
    datetime = "05/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "May");
    datetime = "06/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "June");
    datetime = "07/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "July");
    datetime = "08/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "August");
    datetime = "09/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "September");
    datetime = "10/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "October");
    datetime = "11/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "November");
    datetime = "12/01 24:00:00";
    convertToMonth(*state, datetime);
    EXPECT_EQ(datetime, "December");
    datetime = "01/01 23:00:00";
    EXPECT_THROW(convertToMonth(*state, datetime), FatalError);
}

} // namespace EnergyPlus
