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
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/ResultsSchema.hh>

#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/OutAirNodeManager.hh>

using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::SizingManager;

namespace EnergyPlus {


	TEST_F( EnergyPlusFixture , Output1 ){
		std::string const idf_objects = delimited_string({
				                                                 "Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				                                                 "Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				                                                 "Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				                                                 "Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				                                                 "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
				                                                 "Output:Meter,Electricity:Facility,timestep;",
				                                                 "Output:Meter,Electricity:Facility,hourly;",
				                                                 "Output:Meter,Electricity:Facility,daily;",
				                                                 "Output:Meter,Electricity:Facility,monthly;",
				                                                 "Output:Meter,Electricity:Facility,runperiod;",
		                                                 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		if (ResultsFramework::OutputSchema->HRMeters.rDataFrameEnabled())
			ResultsFramework::OutputSchema->HRMeters.setRDataFrameEnabled(false);
		if (ResultsFramework::OutputSchema->DYMeters.rDataFrameEnabled())
			ResultsFramework::OutputSchema->DYMeters.setRDataFrameEnabled(false);
		if (ResultsFramework::OutputSchema->DYMeters.rVariablesScanned())
			ResultsFramework::OutputSchema->DYMeters.setRVariablesScanned(false);
		if (ResultsFramework::OutputSchema->MNMeters.rVariablesScanned())
			ResultsFramework::OutputSchema->MNMeters.setRVariablesScanned(false);
		if (ResultsFramework::OutputSchema->SMMeters.rVariablesScanned())
			ResultsFramework::OutputSchema->SMMeters.setRVariablesScanned(false);

		DataGlobals::DayOfSim = 365;
		DataGlobals::DayOfSimChr = "365";
		DataEnvironment::Month = 12;
		DataEnvironment::DayOfMonth = 31;
		DataEnvironment::DSTIndicator = 0;
		DataEnvironment::DayOfWeek = 3;
		DataEnvironment::HolidayIndex = 0;
		DataGlobals::HourOfDay = 24;
		DataGlobals::NumOfDayInEnvrn = 365;
		DataGlobals::MinutesPerTimeStep = 10;

		if ( DataGlobals::TimeStep == DataGlobals::NumOfTimeStepInHour ) {
			DataGlobals::EndHourFlag = true;
			if ( DataGlobals::HourOfDay == 24 ) {
				DataGlobals::EndDayFlag = true;
				if ( ( ! DataGlobals::WarmupFlag ) && ( DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn ) ) {
					DataGlobals::EndEnvrnFlag = true;
				}
			}
		}

		if ( DataEnvironment::DayOfMonth == WeatherManager::EndDayOfMonth( DataEnvironment::Month ) ) {
			DataEnvironment::EndMonthFlag = true;
		}

		TimeValue.allocate( 2 );

		auto timeStep = 1.0 / 6;

		SetupTimePointers( "Zone", timeStep );
		SetupTimePointers( "HVAC", timeStep );

		TimeValue( 1 ).CurMinute = 50;
		TimeValue( 2 ).CurMinute = 50;

		EnergyPlus::sqlite = std::move( sqlite_test );
		GetReportVariableInput();
		SetupOutputVariable( "Site Outdoor Air Drybulb Temperature [C]", DataEnvironment::OutDryBulbTemp, "Zone", "Average", "Environment" );
		Real64 light_consumption = 999;
		SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE1-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE1-1", 1, 1 );
		SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE2-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE2-1", 1, 1 );
		SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE3-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE3-1", 1, 1 );
		SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE4-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE4-1", 1, 1 );
		SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE5-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE5-1", 1, 1 );
		Real64 zone_infil_total_loss = 999;
		SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE1-1" );
		SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE2-1" );
		SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE3-1" );
		SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE4-1" );
		SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE5-1" );

		UpdateMeterReporting();

		UpdateDataandReport( DataGlobals::ZoneTSReporting );

		sqlite_test = std::move( EnergyPlus::sqlite );

		auto timeResults = queryResult("SELECT * FROM Time;", "Time");

		std::vector< std::vector<std::string> > timeData({
				                                                 {"1", "12", "31", "24", "0", "0", "10", "-1", "365", "Tuesday", "0", "0"},
				                                                 {"2", "12", "31", "24", "0", "0", "60", "1", "365", "Tuesday", "0", "0"},
				                                                 {"3", "12", "31", "24", "0", "0", "1440", "2", "365", "Tuesday", "0", "0"},
				                                                 {"4", "12", "31", "24", "0", "", "44640", "3", "365", "", "0", "0"},
				                                                 {"5", "", "", "", "", "", "525600", "4", "365", "", "0", "0"},
		                                                 });

		EXPECT_EQ( timeData, timeResults );

		auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

		std::vector< std::vector<std::string> > reportDataDictionary({
				                                                             { "1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C" },
				                                                             { "2", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C" },
				                                                             { "3", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C" },
				                                                             { "4", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C" },
				                                                             { "5", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C" },
				                                                             { "7", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Zone Timestep", "", "J" },
				                                                             { "8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Hourly", "", "J" },
				                                                             { "9", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Daily", "", "J" },
				                                                             { "10", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Monthly", "", "J" },
				                                                             { "11", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Run Period", "", "J" },
		                                                             });

		EXPECT_EQ( reportDataDictionary, reportDataDictionaryResults );

		auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
		auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

		std::vector< std::vector<std::string> > reportData({
				                                                   { "1", "1", "1", "0.0" },
				                                                   { "2", "1", "7", "4995.0" },
				                                                   { "3", "2", "2", "0.0" },
				                                                   { "4", "2", "8", "4995.0" },
				                                                   { "5", "3", "3", "0.0" },
				                                                   { "6", "3", "9", "4995.0" },
				                                                   { "7", "4", "4", "0.0" },
				                                                   { "8", "4", "10", "4995.0" },
				                                                   { "9", "5", "5", "0.0" },
				                                                   { "10", "5", "11", "4995.0" },
		                                                   });

		std::vector< std::vector<std::string> > reportExtendedData({
				                                                           { "1", "5", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0" },
				                                                           { "2", "6", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0" },
				                                                           { "3", "7", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0" },
				                                                           { "4", "8", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0" },
				                                                           { "5", "9", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0" },
				                                                           { "6", "10", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0" }
		                                                           });

		EXPECT_EQ( reportData, reportDataResults );
		EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

		compare_eso_stream( delimited_string( {
				                                      "1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
				                                      "2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
				                                      "3,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
				                                      "4,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				                                      "5,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				                                      "7,1,Electricity:Facility [J] !TimeStep",
				                                      "8,1,Electricity:Facility [J] !Hourly",
				                                      "9,7,Electricity:Facility [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]",
				                                      "10,9,Electricity:Facility [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				                                      "11,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				                                      ",365,12,31, 0,24,50.00,60.00,Tuesday",
				                                      "1,0.0",
				                                      "7,4995.0",
				                                      ",365,12,31, 0,24, 0.00,60.00,Tuesday",
				                                      "2,0.0",
				                                      "8,4995.0",
				                                      ",365,12,31, 0,Tuesday",
				                                      "3,0.0,0.0,24,60,0.0,24,60",
				                                      "9,4995.0,4995.0,24,60,4995.0,24,60",
				                                      ",365,12",
				                                      "4,0.0,0.0,31,24,60,0.0,31,24,60",
				                                      "10,4995.0,4995.0,31,24,60,4995.0,31,24,60",
				                                      ",365",
				                                      "5,0.0,0.0,12,31,24,60,0.0,12,31,24,60",
				                                      "11,4995.0,4995.0,12,31,24,60,4995.0,12,31,24,60",
		                                      } ) );

		compare_mtr_stream( delimited_string( {
				                                      "7,1,Electricity:Facility [J] !TimeStep",
				                                      "8,1,Electricity:Facility [J] !Hourly",
				                                      "9,7,Electricity:Facility [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]",
				                                      "10,9,Electricity:Facility [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				                                      "11,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				                                      ",365,12,31, 0,24,50.00,60.00,Tuesday",
				                                      "7,4995.0",
				                                      ",365,12,31, 0,24, 0.00,60.00,Tuesday",
				                                      "8,4995.0",
				                                      ",365,12,31, 0,Tuesday",
				                                      "9,4995.0,4995.0,24,60,4995.0,24,60",
				                                      ",365,12",
				                                      "10,4995.0,4995.0,31,24,60,4995.0,31,24,60",
				                                      ",365",
				                                      "11,4995.0,4995.0,12,31,24,60,4995.0,12,31,24,60",
		                                      } ) );

		compare_json_stream( delimited_string ({
				                                       "{",
				                                       "}"
		                                       }) );
	}

}
