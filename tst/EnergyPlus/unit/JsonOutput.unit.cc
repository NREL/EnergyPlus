// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ResultsSchema.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/NodeInputManager.hh>

//Fixture
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::ResultsFramework;
using namespace EnergyPlus::SimulationManager;
using namespace EnergyPlus::DataOutputs;
using namespace EnergyPlus::NodeInputManager;

namespace EnergyPlus {


	TEST_F( EnergyPlusFixture , JsonOutput_ParseJsonObject1){
		std::string const idf_objects = delimited_string({
				                                                 "Output:JSON,",
				                                                 "TimeSeriesAndTabular;",
		                                                 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		OutputSchema->setupOutputOptions();

		EXPECT_TRUE( OutputSchema->timeSeriesAndTabularEnabled() );
	}

	TEST_F( EnergyPlusFixture , JsonOutput_ParseJsonObject2){
		std::string const idf_objects = delimited_string({
				                                                 "Output:JSON,",
				                                                 "TimeSeries;",
		                                                 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		OutputSchema->setupOutputOptions();

		EXPECT_TRUE( OutputSchema->timeSeriesEnabled() );
		//compare_json_stream( "" );
	}

	TEST_F( EnergyPlusFixture , JsonOutput_SimInfo){

		OutputSchema->SimulationInformation.setProgramVersion("EnergyPlus, Version 8.6.0-0f5a10914b");
		OutputSchema->SimulationInformation.setStartDateTimeStamp("2017.03.22 11:03");
		OutputSchema->SimulationInformation.setUUID("b307f053-6c0a-8ca8-4d76-81841cd48f20");
		OutputSchema->SimulationInformation.setInputModelURI("");
		OutputSchema->SimulationInformation.setRunTime("00hr 08min  6.67sec");
		OutputSchema->SimulationInformation.setNumErrorsSummary("1","2");
		OutputSchema->SimulationInformation.setNumErrorsSizing("0","0");
		OutputSchema->SimulationInformation.setNumErrorsWarmup("0","2");
		OutputSchema->SimulationInformation.setSimulationEnvironment("");

		json result = OutputSchema->SimulationInformation.getJSON();
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
					"StartDateTimeStamp": "2017.03.22 11:03",
					"UUID": "b307f053-6c0a-8ca8-4d76-81841cd48f20"
		} )"_json;
		EXPECT_EQ( result.dump(), expectedResult.dump() );
	}

	TEST_F( EnergyPlusFixture , JsonOutput_SimInfo_String) {
		OutputSchema->SimulationInformation.setProgramVersion("EnergyPlus, Version 8.6.0-0f5a10914b");
		OutputSchema->SimulationInformation.setStartDateTimeStamp("2017.03.22 11:03");
		OutputSchema->SimulationInformation.setUUID("b307f053-6c0a-8ca8-4d76-81841cd48f20");
		OutputSchema->SimulationInformation.setInputModelURI("");
		OutputSchema->SimulationInformation.setRunTime("00hr 08min  6.67sec");
		OutputSchema->SimulationInformation.setNumErrorsSummary("1","2");
		OutputSchema->SimulationInformation.setNumErrorsSizing("0","0");
		OutputSchema->SimulationInformation.setNumErrorsWarmup("0","2");
		OutputSchema->SimulationInformation.setSimulationEnvironment("");

		json result = OutputSchema->SimulationInformation.getJSON();

		std::string expectedResult =
				"{\n    \"ErrorSummary\": {\n        \"NumSevere\": \"2\",\n        \"NumWarnings\": \"1\"\n    },\n    \"ErrorSummarySizing\": {\n        \"NumSevere\": \"0\",\n        \"NumWarnings\": \"0\"\n    },\n    \"ErrorSummaryWarmup\": {\n        \"NumSevere\": \"2\",\n        \"NumWarnings\": \"0\"\n    },\n    \"InputModelURI\": \"\",\n    \"ProgramVersion\": \"EnergyPlus, Version 8.6.0-0f5a10914b\",\n    \"RunTime\": \"00hr 08min  6.67sec\",\n    \"SimulationEnvironment\": \"\",\n    \"StartDateTimeStamp\": \"2017.03.22 11:03\",\n    \"UUID\": \"b307f053-6c0a-8ca8-4d76-81841cd48f20\"\n}";
		EXPECT_EQ( result.dump(4), expectedResult );
	};

	TEST_F( EnergyPlusFixture , JsonOutput_VariableInfo) {


//		std::string const idf_objects = delimited_string({
//		                                                         "Output:Variable,SalesFloor Inlet Node,System Node Temperature,timestep;",
//		                                                         "Output:Variable,SalesFloor Inlet Node,System Node Mass Flow Rate,timestep;",
//		                                                 });
//		ASSERT_TRUE( process_idf( idf_objects ) );
//		//OutputSchema->setupOutputOptions();
//
//		EXPECT_EQ( (int)OutputVariablesForSimulation.size(), 2 );
		//SetupNodeVarsForReporting();
		//EXPECT_EQ( "SALESFLOOR INLET NODE:System Node Temperature", RVariableTypes( 1 ).VarName );
		//EXPECT_EQ( "SALESFLOOR INLET NODE:System Node Mass Flow Rate", OutputProcessor::RVariableTypes( 2 ).VarName );
		//EXPECT_EQ( 1, OutputProcessor::RVariableTypes( 1 ).ReportID );
		//EXPECT_EQ( 2, OutputProcessor::RVariableTypes( 2 ).ReportID );
		int indexType = 1;
		int repordId = 1;

		Variable *var = new Variable("SALESFLOOR INLET NODE:System Node Temperature", 0, indexType, repordId, "C");
		var->setUUID("b78f1461-c383-2b7c-4e6c-8ad163036bc7");

		std::string expected_result = "{\n         \"Frequency\": \"Timestep\",\n         \"Name\": \"SALESFLOOR INLET NODE:System Node Temperature\",\n         \"UUID\": \"b78f1461-c383-2b7c-4e6c-8ad163036bc7\",\n         \"Units\": \"C\"\n}";
		EXPECT_EQ(expected_result, var->getJSON().dump('\t'));

		json expectedObject = R"( {
					"Frequency": "Timestep",
					"Name": "SALESFLOOR INLET NODE:System Node Temperature",
					"UUID": "b78f1461-c383-2b7c-4e6c-8ad163036bc7",
					"Units": "C"
		} )"_json;

		EXPECT_EQ( expectedObject, var->getJSON() );
	};

	TEST_F( EnergyPlusFixture , JsonOutput_DataFrameInfo1) {

		json OutputVars;
		int indexType = 1;
		int reportId = 1;

		Variable *var0 = new Variable("SALESFLOOR INLET NODE:System Node Temperature", 0, indexType, reportId, "C");
		var0->setUUID("b78f1461-c383-2b7c-4e6c-8ad163036bc7");
		reportId++;
		Variable *var1 = new Variable("SALESFLOOR INLET NODE:System Node Humidity Ratio", 0, indexType, reportId, "kgWater/kgDryAir");
		var1->setUUID("d83f492c-bc10-9d87-4456-846a10607b79");

		OutputSchema->RITimestepTSData.addVariable(var0);
		OutputSchema->RITimestepTSData.addVariable(var1);

		OutputVars["Timestep"] = OutputSchema->RITimestepTSData.getVariablesJSON();

		json expectedObject = R"( {
				"Timestep": [
					 {
						"Frequency": "Timestep",
						"Name": "SALESFLOOR INLET NODE:System Node Humidity Ratio",
						"UUID": "d83f492c-bc10-9d87-4456-846a10607b79",
						"Units": "kgWater/kgDryAir"
					},
					{
						"Frequency": "Timestep",
						"Name": "SALESFLOOR INLET NODE:System Node Temperature",
						"UUID": "b78f1461-c383-2b7c-4e6c-8ad163036bc7",
						"Units": "C"
					}]
		} )"_json;

		EXPECT_EQ( expectedObject.dump(), OutputVars.dump());

	};

	TEST_F( EnergyPlusFixture , JsonOutput_DataFrameInfo2) {

		json OutputData;
		int indexType = 1;
		int reportId = 1;

		Variable *var0 = new Variable("SALESFLOOR INLET NODE:System Node Temperature", 0, indexType, reportId, "C");
		var0->setUUID("b78f1461-c383-2b7c-4e6c-8ad163036bc7");
		OutputSchema->RITimestepTSData.addVariable(var0);
		OutputSchema->RITimestepTSData.newRow(2,25,14,40); //month,day,hour,minute
		OutputSchema->RITimestepTSData.newRow(2,25,14,45); //month,day,hour,minute

		OutputSchema->RITimestepTSData.pushVariableValue(reportId, 1.0);
		OutputSchema->RITimestepTSData.pushVariableValue(reportId, 2.0);

		reportId++;
		Variable *var1 = new Variable("SALESFLOOR INLET NODE:System Node Humidity Ratio", 0, indexType, reportId, "kgWater/kgDryAir");
		var1->setUUID("d83f492c-bc10-9d87-4456-846a10607b79");
		OutputSchema->RITimestepTSData.addVariable(var1);
		OutputSchema->RITimestepTSData.pushVariableValue(reportId, 3.0);
		OutputSchema->RITimestepTSData.pushVariableValue(reportId, 4.0);

		OutputSchema->RITimestepTSData.setUUID("b78f1461-c383-2b7c-4e6c-8ad163036bc7");

		OutputData["Timestep"] = OutputSchema->RITimestepTSData.getJSON();

		json expectedObject = R"( {
				"Timestep": {
					"Cols":[
						{
							"UUID" : "d83f492c-bc10-9d87-4456-846a10607b79",
							"Units" : "kgWater/kgDryAir",
							"Variable" : "SALESFLOOR INLET NODE:System Node Humidity Ratio"
						},
						{
							"UUID" : "b78f1461-c383-2b7c-4e6c-8ad163036bc7",
							"Units" : "C",
							"Variable":"SALESFLOOR INLET NODE:System Node Temperature"
						}
					],
					"ReportFrequency" : "Timestep",
					"Rows":[
						{ "2/25 14:40:00" : [3,1] },
						{ "2/25 14:45:00" : [4,2] }
					],
					"UUID":"b78f1461-c383-2b7c-4e6c-8ad163036bc7"
				}
		} )"_json;

		EXPECT_EQ( expectedObject.dump(), OutputData.dump());

		// If add one more, it also should go to the top of json cols array
		reportId++;
		Variable *var2 = new Variable("SALESFLOOR OUTLET NODE:System Node Temperature", 0, indexType, reportId, "C");
		var2->setUUID("fcbad2f4-7342-17a4-429e-8f9de01bb10f");
		OutputSchema->RITimestepTSData.addVariable(var2);
		OutputSchema->RITimestepTSData.pushVariableValue(reportId, 5.0);
		OutputSchema->RITimestepTSData.pushVariableValue(reportId, 6.0);
		OutputData["Timestep"] = OutputSchema->RITimestepTSData.getJSON();

		expectedObject = R"( {
				"Timestep": {
					"Cols":[
						{
			                "UUID": "fcbad2f4-7342-17a4-429e-8f9de01bb10f",
			                "Units": "C",
							"Variable" : "SALESFLOOR OUTLET NODE:System Node Temperature"
			            },
						{
							"UUID" : "d83f492c-bc10-9d87-4456-846a10607b79",
							"Units" : "kgWater/kgDryAir",
							"Variable" : "SALESFLOOR INLET NODE:System Node Humidity Ratio"
						},
						{
							"UUID" : "b78f1461-c383-2b7c-4e6c-8ad163036bc7",
							"Units" : "C",
							"Variable":"SALESFLOOR INLET NODE:System Node Temperature"
						}
					],
					"ReportFrequency" : "Timestep",
					"Rows":[
						{ "2/25 14:40:00" : [5,3,1] },
						{ "2/25 14:45:00" : [6,4,2] }
					],
					"UUID":"b78f1461-c383-2b7c-4e6c-8ad163036bc7"
				}
		} )"_json;

		EXPECT_EQ( expectedObject.dump(), OutputData.dump());
	};
}


