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

//Fixture
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::ResultsFramework;
using namespace EnergyPlus::SimulationManager;

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
//		std::string const idf_objects = delimited_string({
//				                                                 "Version,8.6;",
//				                                                 "SimulationControl,",
//				                                                 "No,                      !- Do Zone Sizing Calculation",
//				                                                 "No,                      !- Do System Sizing Calculation",
//				                                                 "No,                      !- Do Plant Sizing Calculation",
//				                                                 "No,                      !- Run Simulation for Sizing Periods",
//				                                                 "Yes;                     !- Run Simulation for Weather File Run Periods",
//				                                                 "Building,",
//				                                                 "None,                    !- Name",
//				                                                 "0,                       !- North Axis {deg}",
//				                                                 " Suburbs,                 !- Terrain",
//				                                                 "0.04,                    !- Loads Convergence Tolerance Value",
//				                                                 "0.4,                     !- Temperature Convergence Tolerance Value {deltaC}",
//				                                                 "MinimalShadowing,        !- Solar Distribution",
//				                                                 "50,                      !- Maximum Number of Warmup Days",
//				                                                 "6;                       !- Minimum Number of Warmup Days",
//
//				                                                 "SurfaceConvectionAlgorithm:Inside,TARP;",
//
//				                                                 "Refrigeration:Case,",
//				                                                 "FishDisplayCase,         !- Name",
//				                                                 "CaseCleanedOnMonday,     !- Availability Schedule Name",
//				                                                 "SalesFloor,              !- Zone Name",
//				                                                 "23.88,                   !- Rated Ambient Temperature {C}",
//				                                                 "55.0,                    !- Rated Ambient Relative Humidity {percent}",
//				                                                 "288.4,                   !- Rated Total Cooling Capacity per Unit Length {W/m}",
//				                                                 "0.1,                     !- Rated Latent Heat Ratio",
//				                                                 "0.85,                    !- Rated Runtime Fraction",
//				                                                 "15.0,                    !- Case Length {m}",
//				                                                 "1.1,                     !- Case Operating Temperature {C}",
//				                                                 "RelativeHumidityMethod,  !- Latent Case Credit Curve Type",
//				                                                 "RHCubic_LatentEnergyMult,!- Latent Case Credit Curve Name",
//				                                                 "0.0,                     !- Standard Case Fan Power per Unit Length {W/m}",
//				                                                 "0.0,                     !- Operating Case Fan Power per Unit Length {W/m}",
//				                                                 "41.56,                   !- Standard Case Lighting Power per Unit Length {W/m}",
//				                                                 ",                        !- Installed Case Lighting Power per Unit Length {W/m}",
//				                                                 "CaseLightingSched,       !- Case Lighting Schedule Name",
//				                                                 "0.9,                     !- Fraction of Lighting Energy to Case",
//				                                                 "0.0,                     !- Case Anti-Sweat Heater Power per Unit Length {W/m}",
//				                                                 ",                        !- Minimum Anti-Sweat Heater Power per Unit Length {W/m}",
//				                                                 "None,                    !- Anti-Sweat Heater Control Type",
//				                                                 "0.0,                     !- Humidity at Zero Anti-Sweat Heater Energy {percent}",
//				                                                 "0.0,                     !- Case Height {m}",
//				                                                 "0.2,                     !- Fraction of Anti-Sweat Heater Energy to Case",
//				                                                 "0.0,                     !- Case Defrost Power per Unit Length {W/m}",
//				                                                 "OffCycle,                !- Case Defrost Type",
//				                                                 "CaseDefrostSched,        !- Case Defrost Schedule Name",
//				                                                 "CaseDripDownSched,       !- Case Defrost Drip-Down Schedule Name",
//				                                                 ",                        !- Defrost Energy Correction Curve Type",
//				                                                 ",                        !- Defrost Energy Correction Curve Name",
//				                                                 "0.05,                    !- Under Case HVAC Return Air Fraction",
//				                                                 "CaseStockingSched;       !- Refrigerated Case Restocking Schedule Name",
//
//				                                                 "Schedule:Compact,",
//				                                                 "CaseCleanedOnMonday,     !- Name",
//				                                                 "AnyNumber,               !- Schedule Type Limits Name",
//				                                                 "Through: 12/31,          !- Field 1",
//				                                                 "For: Monday,             !- Field 2",
//				                                                 "Until: 5:00,1.0,         !- Field 3",
//				                                                 "Until: 7:00,0.0,         !- Field 5",
//				                                                 "Until: 24:00,1.0,        !- Field 7",
//				                                                 "For: AllOtherDays,       !- Field 9",
//				                                                 "Until: 24:00,1.0;        !- Field 10",
//
//				                                                 "Zone,",
//				                                                 "SalesFloor,              !- Name",
//				                                                 "0,                       !- Direction of Relative North {deg}",
//				                                                 "0,                       !- X Origin {m}",
//				                                                 "0,                       !- Y Origin {m}",
//				                                                 "0,                       !- Z Origin {m}",
//				                                                 "1,                       !- Type",
//				                                                 "1,                       !- Multiplier",
//				                                                 "4.266992,                !- Ceiling Height {m}",
//				                                                 "autocalculate,           !- Volume {m3}",
//				                                                 "autocalculate,           !- Floor Area {m2}",
//				                                                 "TARP;                    !- Zone Inside Convection Algori",
//
//				                                                 "Curve:Cubic,",
//				                                                 "RHCubic_LatentEnergyMult,!- Name",
//				                                                 "-0.4641,                 !- Coefficient1 Constant",
//				                                                 "0.0268,                  !- Coefficient2 x",
//				                                                 "0.0,                     !- Coefficient3 x**2",
//				                                                 "0.0,                     !- Coefficient4 x**3",
//				                                                 "17.32,                   !- Minimum Value of x",
//				                                                 "80.0;                    !- Maximum Value o",
//
//				                                                 "Schedule:Compact,",
//				                                                 "CaseLightingSched,       !- Name",
//				                                                 "ON/OFF,                  !- Schedule Type Limits Name",
//				                                                 "Through: 12/31,          !- Field 1",
//				                                                 "For: AllDays,            !- Field 2",
//				                                                 "Until: 6:00,0.0,         !- Field 3",
//				                                                 "Until: 21:00,1.0,        !- Field 5",
//				                                                 "Until: 24:00,0.0;        !- Fie",
//
//				                                                 "Schedule:Compact,",
//				                                                 "CaseDefrostSched,        !- Name",
//				                                                 "ON/OFF,                  !- Schedule Type Limits Name",
//				                                                 "Through: 12/31,          !- Field 1",
//				                                                 "For:AllDays,             !- Field 2",
//				                                                 "Interpolate:Yes,         !- Field 3",
//				                                                 "Until: 4:00,0,           !- Field 4",
//				                                                 "Until: 4:17,1,           !- Field 6",
//				                                                 "Until: 14:00,0,          !- Field 8",
//				                                                 "Until: 14:17,1,          !- Field 10",
//				                                                 "Until: 24:00,0;          !- Field 12",
//				                                                 "Schedule:Compact,",
//				                                                 "CaseDripDownSched,       !- Name",
//				                                                 "ON/OFF,                  !- Schedule Type Limits Name",
//				                                                 "Through: 12/31,          !- Field 1",
//				                                                 "For:AllDays,             !- Field 2",
//				                                                 "Interpolate:Yes,         !- Field 3",
//				                                                 "Until: 4:00,0,           !- Field 4",
//				                                                 "Until: 4:22,1,           !- Field 6",
//				                                                 "Until: 14:00,0,          !- Field 8",
//				                                                 "Until: 14:22,1,          !- Field 10",
//				                                                 "Until: 24:00,0;          !- Field 12",
//				                                                 "Schedule:Compact,",
//				                                                 "CaseStockingSched,       !- Name",
//				                                                 "AnyNumber,               !- Schedule Type Limits Name",
//				                                                 "Through: 12/31,          !- Field 1",
//				                                                 "For: AllDays,            !- Field 2",
//				                                                 "Until: 13:00,0.0,        !- Field 3",
//				                                                 "Until: 14:00,50.0,       !- Field 5",
//				                                                 "Until: 15:00,35.0,       !- Field 7",
//				                                                 "Until: 24:00,0.0;        !- Field 9",
//
//				                                                 "ScheduleTypeLimits,",
//				                                                 "On/Off,                  !- Name",
//				                                                 "0,                       !- Lower Limit Value",
//				                                                 "1,                       !- Upper Limit Value",
//				                                                 "DISCRETE;                !- Numeric ",
//
//				                                                 "Output:VariableDictionary,Regular;",
//				                                                 "Output:Surfaces:List,detailswithvertices;",
//				                                                 "Output:Surfaces:Drawing,DXF;",
//				                                                 "Output:Schedules,timestep;",
//				                                                 "Output:Constructions,Constructions;",
//				                                                 "Output:Table:SummaryReports,",
//				                                                 "AllSummary;              !- Report 1 Name",
//				                                                 "Output:Table:Monthly,",
//				                                                 "Building Loads - Cooling,!- Name",
//				                                                 "2,                       !- Digits After Decimal",
//				                                                 "Zone Air System Sensible Cooling Energy,  !- Variable or Meter 1 Name",
//				                                                 "SumOrAverage,            !- Aggregation Type for Variable or Meter 1",
//				                                                 "Zone Air System Sensible Cooling Rate,  !- Variable or Meter 2 Name",
//				                                                 "Maximum,                 !- Aggregation Type for Variable or Meter 2",
//				                                                 "Site Outdoor Air Drybulb Temperature,  !- Variable or Meter 3 Name",
//				                                                 "ValueWhenMaximumOrMinimum,  !- Aggregation Type for Variable or Meter 3",
//				                                                 "Site Outdoor Air Wetbulb Temperature,  !- Variable or Meter 4 Name",
//				                                                 "ValueWhenMaximumOrMinimum,  !- Aggregation Type for Variable or Meter 4",
//				                                                 "Zone Total Internal Latent Gain Energy,  !- Variable or Meter 5 Name",
//				                                                 "SumOrAverage,            !- Aggregation Type for Variable or Meter 5",
//				                                                 "Zone Total Internal Latent Gain Energy,  !- Variable or Meter 6 Name",
//				                                                 "Maximum,                 !- Aggregation Type for Variable or Meter 6",
//				                                                 "Site Outdoor Air Drybulb Temperature,  !- Variable or Meter 7 Name",
//				                                                 "ValueWhenMaximumOrMinimum,  !- Aggregation Type for Variable or Meter 7",
//				                                                 "Site Outdoor Air Wetbulb Temperature,  !- Variable or Meter 8 Name",
//				                                                 "ValueWhenMaximumOrMinimum;  !- Aggregation Type for Variable or Meter 8",
//				                                                 "Output:Table:Monthly,",
//				                                                 "Building Loads - Heating,!- Name",
//				                                                 "2,                       !- Digits After Decimal",
//				                                                 "Zone Air System Sensible Heating Energy,  !- Variable or Meter 1 Name",
//				                                                 "SumOrAverage,            !- Aggregation Type for Variable or Meter 1",
//				                                                 "Zone Air System Sensible Heating Rate,  !- Variable or Meter 2 Name",
//				                                                 "Maximum,                 !- Aggregation Type for Variable or Meter 2",
//				                                                 "Site Outdoor Air Drybulb Temperature,  !- Variable or Meter 3 Name",
//				                                                 "eWhenMaximumOrMinimum;  !- Aggregation Type for Variable or Meter 3",
//				                                                 "Output:Variable,Outside Air Inlet Node,System Node Mass Flow Rate,timestep;",
//				                                                 "Output:Variable,Outside Air Inlet Node,System Node Temperature,timestep;",
//				                                                 "Output:Variable,Relief Air Outlet Node,System Node Mass Flow Rate,timestep;",
//				                                                 "Output:Variable,Relief Air Outlet Node,System Node Temperature,timestep;",
//				                                                 "Output:Variable,Outdoor Air Mixer Inlet Node,System Node Mass Flow Rate,timestep;",
//				                                                 "Output:Variable,Outdoor Air Mixer Inlet Node,System Node Temperature,timestep;",
//				                                                 "Output:Variable,Mixed Air Node,System Node Mass Flow Rate,timestep;",
//				                                                 "Output:Variable,Mixed Air Node,System Node Temperature,timestep;",
//				                                                 "Output:Variable,Heating Coil Air Inlet Node,System Node Temperature,timestep;",
//				                                                 "Output:Variable,DX Cooling Coil Air Inlet Node,System Node Temperature,timestep;",
//				                                                 "Output:Variable,Air Loop Outlet Node,System Node Temperature,timestep;",
//				                                                 "Output:Variable,*,Unitary System Compressor Part Load Ratio,timestep;",
//				                                                 "Output:Variable,*,Zone Air System Sensible Heating Energy,timestep;",
//				                                                 "Output:Variable,*,Refrigeration Case Evaporator Sensible Cooling Rate,timestep;",
//				                                                 "Output:Variable,*,Refrigeration Case Evaporator Latent Cooling Rate,timestep;",
//				                                                 "Output:Variable,*,Zone Air System Sensible Cooling Energy,timeste",
//
//				                                                 "Refrigeration:CaseAndWalkInList,",
//				                                                 " MedTempCaseList,         !- Name",
//				                                                 "FishDisplayCase,         !- Case or WalkIn 1 Name",
//
//				                                                 "Output:JSON,",
//				                                                 "TimeSeriesAndTabular;",
//
//		                                                 });
//
//		ASSERT_TRUE( process_idf( idf_objects ) );
//		OutputSchema->setupOutputOptions();
//
//		ManageSimulation();
//
//		compare_json_stream( "" );
	};

}
