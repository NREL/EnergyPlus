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

//Fixture
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::ResultsFramework;

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

	TEST_F( EnergyPlusFixture , JsonOutput_Variable) {
		OutputSchema->
	};

}
