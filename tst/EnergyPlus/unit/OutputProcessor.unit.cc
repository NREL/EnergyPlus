// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::OutputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/SQLiteFixture.hh"
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/PurchasedAirManager.hh>

#include <map>

using namespace EnergyPlus::PurchasedAirManager;
using namespace EnergyPlus::WeatherManager;
using namespace EnergyPlus::OutputProcessor;

namespace EnergyPlus {

	namespace OutputProcessor {

		TEST_F( SQLiteFixture, OutputProcessor_TestGetMeteredVariables )
		{
			int const NumVariables = 2;
			Array1D_int VarIndexes( NumVariables ); // Variable Numbers
			Array1D_int VarTypes( NumVariables ); // Variable Types (1=integer, 2=real, 3=meter)
			Array1D_int IndexTypes( NumVariables ); // Variable Index Types (1=Zone,2=HVAC)
			Array1D_string UnitsStrings( NumVariables ); // UnitsStrings for each variable
			Array1D_int ResourceTypes( NumVariables ); // ResourceTypes for each variable
			Array1D_string EndUses( NumVariables ); // EndUses for each variable
			Array1D_string Groups( NumVariables ); // Groups for each variable
			Array1D_string Names( NumVariables ); // Variable Names for each variable
			Reference< RealVariables > RVar;

			std::string TypeOfComp = "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW";
			std::string NameOfComp = "FC-5-1B";

			int NumFound;

			GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

			EXPECT_EQ( 0, NumFound );

			NumOfRVariable = 2;
			RVariableTypes.allocate( NumOfRVariable );
			NameOfComp = "OUTSIDELIGHTS";
			RVar.allocate();

			RVar().MeterArrayPtr = 1;
			RVariableTypes( 1 ).KeyNameOnlyUC = NameOfComp;
			RVariableTypes( 1 ).VarPtr = RVar;
			VarMeterArrays.allocate( 1 );

			VarMeterArrays( 1 ).NumOnMeters = 1;
			VarMeterArrays( 1 ).OnMeters( 1 ) = 1;

			EnergyMeters.allocate( 10 );
			EnergyMeters( 1 ).ResourceType = NameOfComp;

			GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
			EXPECT_EQ( 1 , NumFound );

		}

		TEST_F( SQLiteFixture, OutputProcessor_reportTSMeters_PrintESOTimeStamp )
		{
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
			sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

			NumEnergyMeters = 2;
			EnergyMeters.allocate( NumEnergyMeters );
			EnergyMeters( 1 ).CurTSValue = 999.9;
			EnergyMeters( 1 ).TSValue = 999.9;
			EnergyMeters( 1 ).RptTS = true;
			EnergyMeters( 1 ).RptAccTS = false;
			EnergyMeters( 1 ).RptTSFO = false;
			EnergyMeters( 1 ).RptAccTSFO = false;
			EnergyMeters( 1 ).TSRptNum = 1;
			EnergyMeters( 1 ).TSRptNumChr = "1";
			EnergyMeters( 1 ).TSAccRptNum = 1;
			EnergyMeters( 1 ).SMValue = 999.9;

			EnergyMeters( 2 ).CurTSValue = 9999.9;
			EnergyMeters( 2 ).TSValue = 9999.9;
			EnergyMeters( 2 ).RptTS = true;
			EnergyMeters( 2 ).RptAccTS = false;
			EnergyMeters( 2 ).RptTSFO = false;
			EnergyMeters( 2 ).RptAccTSFO = false;
			EnergyMeters( 2 ).TSRptNum = 2;
			EnergyMeters( 2 ).TSRptNumChr = "2";
			EnergyMeters( 2 ).TSAccRptNum = 2;
			EnergyMeters( 2 ).SMValue = 9999.9;

			TimeStepStampReportNbr = 1;
			TimeStepStampReportChr = "1";
			DataGlobals::DayOfSim = 1;
			DataGlobals::DayOfSimChr = "1";
			DataGlobals::HourOfDay = 1;
			DataEnvironment::Month = 12;
			DataEnvironment::DayOfMonth = 21;
			DataEnvironment::DSTIndicator = 0;
			DataEnvironment::DayOfWeek = 2;
			DataEnvironment::HolidayIndex = 3;
			int EndMinute = 10;
			int StartMinute = 0;
			bool PrintESOTimeStamp = true;

			functionUsingSQLite( std::bind( ReportTSMeters, StartMinute, EndMinute, PrintESOTimeStamp, true ) );

			auto result = queryResult("SELECT * FROM Time;", "Time");

			ASSERT_EQ(1ul, result.size());

			std::vector<std::string> testResult0 {"1", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
			EXPECT_EQ( testResult0, result[0] );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "2", "9999.9"}
			});

			std::vector< std::vector<std::string> > reportExtendedData({});

			EXPECT_EQ( reportData, reportDataResults );
			ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

		}

		TEST_F( SQLiteFixture, OutputProcessor_reportTSMeters )
		{
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
			sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

			NumEnergyMeters = 2;
			EnergyMeters.allocate( NumEnergyMeters );
			EnergyMeters( 1 ).CurTSValue = 999.9;
			EnergyMeters( 1 ).TSValue = 999.9;
			EnergyMeters( 1 ).RptTS = true;
			EnergyMeters( 1 ).RptAccTS = false;
			EnergyMeters( 1 ).RptTSFO = false;
			EnergyMeters( 1 ).RptAccTSFO = false;
			EnergyMeters( 1 ).TSRptNum = 1;
			EnergyMeters( 1 ).TSRptNumChr = "1";
			EnergyMeters( 1 ).TSAccRptNum = 1;
			EnergyMeters( 1 ).SMValue = 999.9;

			EnergyMeters( 2 ).CurTSValue = 9999.9;
			EnergyMeters( 2 ).TSValue = 9999.9;
			EnergyMeters( 2 ).RptTS = true;
			EnergyMeters( 2 ).RptAccTS = false;
			EnergyMeters( 2 ).RptTSFO = false;
			EnergyMeters( 2 ).RptAccTSFO = false;
			EnergyMeters( 2 ).TSRptNum = 2;
			EnergyMeters( 2 ).TSRptNumChr = "2";
			EnergyMeters( 2 ).TSAccRptNum = 2;
			EnergyMeters( 2 ).SMValue = 9999.9;

			TimeStepStampReportNbr = 1;
			TimeStepStampReportChr = "1";
			DataGlobals::DayOfSim = 1;
			DataGlobals::DayOfSimChr = "1";
			DataGlobals::HourOfDay = 1;
			DataEnvironment::Month = 12;
			DataEnvironment::DayOfMonth = 21;
			DataEnvironment::DSTIndicator = 0;
			DataEnvironment::DayOfWeek = 2;
			DataEnvironment::HolidayIndex = 3;
			int EndMinute = 10;
			int StartMinute = 0;
			bool PrintESOTimeStamp = false;

			functionUsingSQLite( std::bind( ReportTSMeters, StartMinute, EndMinute, PrintESOTimeStamp, true ) );

			auto result = queryResult("SELECT * FROM Time;", "Time");

			ASSERT_EQ(1ul, result.size());

			std::vector<std::string> testResult0 {"1", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
			EXPECT_EQ( testResult0, result[0] );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9", "2,9999.9" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "2", "9999.9"}
			});

			std::vector< std::vector<std::string> > reportExtendedData({});

			EXPECT_EQ( reportData, reportDataResults );
			ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

		}

		TEST_F( SQLiteFixture, OutputProcessor_reportHRMeters )
		{
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
			sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

			NumEnergyMeters = 2;
			EnergyMeters.allocate( NumEnergyMeters );
			EnergyMeters( 1 ).RptHR = true;
			EnergyMeters( 1 ).RptHRFO = true;
			EnergyMeters( 1 ).RptAccHR = false;
			EnergyMeters( 1 ).RptAccHRFO = false;
			EnergyMeters( 1 ).HRRptNum = 1;
			EnergyMeters( 1 ).HRRptNumChr = "1";
			EnergyMeters( 1 ).HRValue = 999.9;
			EnergyMeters( 1 ).HRAccRptNum = 1;
			EnergyMeters( 1 ).SMValue = 999.9;

			EnergyMeters( 2 ).RptHR = true;
			EnergyMeters( 2 ).RptHRFO = true;
			EnergyMeters( 2 ).RptAccHR = false;
			EnergyMeters( 2 ).RptAccHRFO = false;
			EnergyMeters( 2 ).HRRptNum = 2;
			EnergyMeters( 2 ).HRRptNumChr = "2";
			EnergyMeters( 2 ).HRValue = 9999.9;
			EnergyMeters( 2 ).HRAccRptNum = 2;
			EnergyMeters( 2 ).SMValue = 9999.9;

			TimeStepStampReportNbr = 1;
			TimeStepStampReportChr = "1";
			DataGlobals::DayOfSim = 1;
			DataGlobals::DayOfSimChr = "1";
			DataGlobals::HourOfDay = 1;
			DataEnvironment::Month = 12;
			DataEnvironment::DayOfMonth = 21;
			DataEnvironment::DSTIndicator = 0;
			DataEnvironment::DayOfWeek = 2;
			DataEnvironment::HolidayIndex = 3;

			functionUsingSQLite( std::bind( ReportHRMeters, true ) );

			auto result = queryResult("SELECT * FROM Time;", "Time");

			ASSERT_EQ(1ul, result.size());

			std::vector<std::string> testResult0 {"1", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", ""};
			EXPECT_EQ( testResult0, result[0] );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay", "1,999.9", "2,9999.9" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "2", "9999.9"}
			});

			std::vector< std::vector<std::string> > reportExtendedData({});

			EXPECT_EQ( reportData, reportDataResults );
			ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

		}

		TEST_F( SQLiteFixture, OutputProcessor_reportDYMeters )
		{
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
			sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

			NumEnergyMeters = 2;
			EnergyMeters.allocate( NumEnergyMeters );
			EnergyMeters( 1 ).RptDY = true;
			EnergyMeters( 1 ).RptDYFO = true;
			EnergyMeters( 1 ).RptAccDY = false;
			EnergyMeters( 1 ).RptAccDYFO = false;
			EnergyMeters( 1 ).DYRptNum = 1;
			EnergyMeters( 1 ).DYRptNumChr = "1";
			EnergyMeters( 1 ).DYValue = 999.9;
			EnergyMeters( 1 ).DYAccRptNum = 1;
			EnergyMeters( 1 ).SMValue = 999.9;
			EnergyMeters( 1 ).DYMaxVal = 4283136.2524843821;
			EnergyMeters( 1 ).DYMaxValDate = 12210160;
			EnergyMeters( 1 ).DYMinVal = 4283136.2516839253;
			EnergyMeters( 1 ).DYMinValDate = 12210110;

			EnergyMeters( 2 ).RptDY = true;
			EnergyMeters( 2 ).RptDYFO = true;
			EnergyMeters( 2 ).RptAccDY = false;
			EnergyMeters( 2 ).RptAccDYFO = false;
			EnergyMeters( 2 ).DYRptNum = 2;
			EnergyMeters( 2 ).DYRptNumChr = "2";
			EnergyMeters( 2 ).DYValue = 9999.9;
			EnergyMeters( 2 ).DYAccRptNum = 2;
			EnergyMeters( 2 ).SMValue = 9999.9;
			EnergyMeters( 2 ).DYMaxVal = 4283136.2524843821;
			EnergyMeters( 2 ).DYMaxValDate = 12210160;
			EnergyMeters( 2 ).DYMinVal = 4283136.2516839253;
			EnergyMeters( 2 ).DYMinValDate = 12210110;

			DailyStampReportNbr = 1;
			DailyStampReportChr = "1";
			DataGlobals::DayOfSim = 1;
			DataGlobals::DayOfSimChr = "1";
			DataGlobals::HourOfDay = 1;
			DataEnvironment::Month = 12;
			DataEnvironment::DayOfMonth = 21;
			DataEnvironment::DSTIndicator = 0;
			DataEnvironment::DayOfWeek = 2;
			DataEnvironment::HolidayIndex = 3;

			functionUsingSQLite( std::bind( ReportDYMeters, true ) );

			auto result = queryResult("SELECT * FROM Time;", "Time");

			ASSERT_EQ(1ul, result.size());

			std::vector<std::string> testResult0 {"1", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", ""};
			EXPECT_EQ( testResult0, result[0] );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0,WinterDesignDay", "1,999.9,4283136.25168393, 1,10,4283136.25248438, 1,60", "2,9999.9,4283136.25168393, 1,10,4283136.25248438, 1,60" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "2", "9999.9"}
			});

			std::vector< std::vector<std::string> > reportExtendedData(
			{
				{"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"},
				{"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"}
			});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_reportMNMeters )
		{
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
			sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

			NumEnergyMeters = 2;
			EnergyMeters.allocate( NumEnergyMeters );
			EnergyMeters( 1 ).RptMN = true;
			EnergyMeters( 1 ).RptMNFO = true;
			EnergyMeters( 1 ).RptAccMN = false;
			EnergyMeters( 1 ).RptAccMNFO = false;
			EnergyMeters( 1 ).MNRptNum = 1;
			EnergyMeters( 1 ).MNRptNumChr = "1";
			EnergyMeters( 1 ).MNValue = 999.9;
			EnergyMeters( 1 ).MNAccRptNum = 1;
			EnergyMeters( 1 ).SMValue = 999.9;
			EnergyMeters( 1 ).MNMaxVal = 4283136.2524843821;
			EnergyMeters( 1 ).MNMaxValDate = 12210160;
			EnergyMeters( 1 ).MNMinVal = 4283136.2516839253;
			EnergyMeters( 1 ).MNMinValDate = 12210110;

			EnergyMeters( 2 ).RptMN = true;
			EnergyMeters( 2 ).RptMNFO = true;
			EnergyMeters( 2 ).RptAccMN = false;
			EnergyMeters( 2 ).RptAccMNFO = false;
			EnergyMeters( 2 ).MNRptNum = 2;
			EnergyMeters( 2 ).MNRptNumChr = "2";
			EnergyMeters( 2 ).MNValue = 9999.9;
			EnergyMeters( 2 ).MNAccRptNum = 2;
			EnergyMeters( 2 ).SMValue = 9999.9;
			EnergyMeters( 2 ).MNMaxVal = 4283136.2524843821;
			EnergyMeters( 2 ).MNMaxValDate = 12210160;
			EnergyMeters( 2 ).MNMinVal = 4283136.2516839253;
			EnergyMeters( 2 ).MNMinValDate = 12210110;

			MonthlyStampReportNbr = 1;
			MonthlyStampReportChr = "1";
			DataGlobals::DayOfSim = 1;
			DataGlobals::DayOfSimChr = "1";
			DataGlobals::HourOfDay = 1;
			DataEnvironment::Month = 12;
			DataEnvironment::DayOfMonth = 21;
			DataEnvironment::DSTIndicator = 0;
			DataEnvironment::DayOfWeek = 2;
			DataEnvironment::HolidayIndex = 3;

			functionUsingSQLite( std::bind( ReportMNMeters, true ) );

			auto result = queryResult("SELECT * FROM Time;", "Time");

			ASSERT_EQ(1ul, result.size());

			std::vector<std::string> testResult0 {"1", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", ""};
			EXPECT_EQ( testResult0, result[0] );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12", "1,999.9,4283136.25168393,21, 1,10,4283136.25248438,21, 1,60", "2,9999.9,4283136.25168393,21, 1,10,4283136.25248438,21, 1,60" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "2", "9999.9"}
			});

			std::vector< std::vector<std::string> > reportExtendedData(
			{
				{"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"},
				{"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"}
			});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_reportSMMeters )
		{
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
			sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

			NumEnergyMeters = 2;
			EnergyMeters.allocate( NumEnergyMeters );
			EnergyMeters( 1 ).RptSM = true;
			EnergyMeters( 1 ).RptSMFO = true;
			EnergyMeters( 1 ).RptAccSM = false;
			EnergyMeters( 1 ).RptAccSMFO = false;
			EnergyMeters( 1 ).SMRptNum = 1;
			EnergyMeters( 1 ).SMRptNumChr = "1";
			EnergyMeters( 1 ).SMValue = 999.9;
			EnergyMeters( 1 ).SMAccRptNum = 1;
			EnergyMeters( 1 ).SMValue = 999.9;
			EnergyMeters( 1 ).SMMaxVal = 4283136.2524843821;
			EnergyMeters( 1 ).SMMaxValDate = 12210160;
			EnergyMeters( 1 ).SMMinVal = 4283136.2516839253;
			EnergyMeters( 1 ).SMMinValDate = 12210110;

			EnergyMeters( 2 ).RptSM = true;
			EnergyMeters( 2 ).RptSMFO = true;
			EnergyMeters( 2 ).RptAccSM = false;
			EnergyMeters( 2 ).RptAccSMFO = false;
			EnergyMeters( 2 ).SMRptNum = 2;
			EnergyMeters( 2 ).SMRptNumChr = "2";
			EnergyMeters( 2 ).SMValue = 9999.9;
			EnergyMeters( 2 ).SMAccRptNum = 2;
			EnergyMeters( 2 ).SMValue = 9999.9;
			EnergyMeters( 2 ).SMMaxVal = 4283136.2524843821;
			EnergyMeters( 2 ).SMMaxValDate = 12210160;
			EnergyMeters( 2 ).SMMinVal = 4283136.2516839253;
			EnergyMeters( 2 ).SMMinValDate = 12210110;

			RunPeriodStampReportNbr = 1;
			RunPeriodStampReportChr = "1";
			DataGlobals::DayOfSim = 1;
			DataGlobals::DayOfSimChr = "1";
			DataGlobals::HourOfDay = 1;
			DataEnvironment::Month = 12;
			DataEnvironment::DayOfMonth = 21;
			DataEnvironment::DSTIndicator = 0;
			DataEnvironment::DayOfWeek = 2;
			DataEnvironment::HolidayIndex = 3;

			functionUsingSQLite( std::bind( ReportSMMeters, true ) );

			auto result = queryResult("SELECT * FROM Time;", "Time");

			ASSERT_EQ(1ul, result.size());

			std::vector<std::string> testResult0 {"1", "", "", "", "", "", "1440", "4", "1", "", "0", ""};
			EXPECT_EQ( testResult0, result[0] );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1", "1,999.9,4283136.25168393,12,21, 1,10,4283136.25248438,12,21, 1,60", "2,9999.9,4283136.25168393,12,21, 1,10,4283136.25248438,12,21, 1,60" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "2", "9999.9"}
			});

			std::vector< std::vector<std::string> > reportExtendedData(
			{
				{"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"},
				{"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"}
			});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeTimeStampFormatData )
		{
			int TimeStepStampReportNbr = 1;
			std::string TimeStepStampReportChr = "1";

			int DailyStampReportNbr = 1;
			std::string DailyStampReportChr = "1";

			int MonthlyStampReportNbr = 1;
			std::string MonthlyStampReportChr = "1";

			int RunPeriodStampReportNbr = 1;
			std::string RunPeriodStampReportChr = "1";

			int DayOfSim = 1;
			std::string DayOfSimChr = "1";
			bool PrintTimeStamp = true;
			int Month = 12;
			int DayOfMonth = 21;
			int HourOfDay = 1;
			int EndMinute = 10;
			int StartMinute = 0;
			int DSTIndicator = 0;
			int CurDayType = 10;

			// TSMeter
			functionUsingSQLite( std::bind( WriteTimeStampFormatData, DataGlobals::mtr_stream, ReportTimeStep, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim,
				DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, EndMinute, StartMinute, DSTIndicator, DayTypes( CurDayType ) ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay" } ) ) );

			// TSMeter
			functionUsingSQLite( std::bind( WriteTimeStampFormatData, DataGlobals::mtr_stream, ReportEach, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim,
				DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, EndMinute, StartMinute, DSTIndicator, DayTypes( CurDayType ) ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay" } ) ) );

			// HRMeter
			functionUsingSQLite( std::bind( WriteTimeStampFormatData, DataGlobals::mtr_stream, ReportHourly, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim,
				DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, _, _, DSTIndicator, DayTypes( CurDayType ) ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay" } ) ) );

			// DYMeter
			functionUsingSQLite( std::bind( WriteTimeStampFormatData, DataGlobals::mtr_stream, ReportDaily, DailyStampReportNbr, DailyStampReportChr, DayOfSim, DayOfSimChr,
				PrintTimeStamp, Month, DayOfMonth, _, _, _, DSTIndicator, DayTypes( CurDayType ) ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12,21, 0,WinterDesignDay" } ) ) );

			// MNMeter
			functionUsingSQLite( std::bind( WriteTimeStampFormatData, DataGlobals::mtr_stream, ReportMonthly, MonthlyStampReportNbr, MonthlyStampReportChr, DayOfSim, DayOfSimChr, PrintTimeStamp, Month, _, _, _, _, _, _ ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,12" } ) ) );

			// SMMeter
			functionUsingSQLite( std::bind( WriteTimeStampFormatData, DataGlobals::mtr_stream, ReportSim, RunPeriodStampReportNbr, RunPeriodStampReportChr, DayOfSim, DayOfSimChr, PrintTimeStamp, _, _, _, _, _, _, _ ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1" } ) ) );

			// Bad input
			functionUsingSQLite( std::bind( WriteTimeStampFormatData, DataGlobals::mtr_stream, 999, RunPeriodStampReportNbr, RunPeriodStampReportChr, DayOfSim, DayOfSimChr, PrintTimeStamp, _, _, _, _, _, _, _ ) );

			EXPECT_EQ("SQLite3 message, Illegal reportingInterval passed to WriteTimeStampFormatData: 999\n", ss->str());
			ss->str(std::string());

			auto timeResults = queryResult("SELECT * FROM Time;", "Time");

			std::vector< std::vector<std::string> > timeData(
			{
				{"1", "12", "21", "0", "10", "0", "10", "0", "1", "WinterDesignDay", "0", "0"},
				{"2", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"},
				{"3", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", "0"},
				{"4", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", "0"},
				{"5", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", "0"},
				{"6", "", "", "", "", "", "1440", "4", "1", "", "0", "0"}
			});

			EXPECT_EQ( timeData, timeResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeReportMeterData )
		{
			DataGlobals::MinutesPerTimeStep = 10;

			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 999.9, ReportTimeStep, 0.0, 0, 0.0, 0, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,999.9" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 999.9, ReportEach, 0.0, 0, 0.0, 0, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,999.9" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportHourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportTimeStep, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportEach, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportHourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 616771620.98702729, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportMeterData, 1, "1", 0, ReportTimeStep, 0.0, 0, 0.0, 0, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,0.0" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.0" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "1", "999.9"},
				{"3", "1", "1", "616771620.987027"},
				{"4", "1", "1", "616771620.987027"},
				{"5", "1", "1", "616771620.987027"},
				{"6", "1", "1", "616771620.987027"},
				{"7", "1", "1", "616771620.987027"},
				{"8", "1", "1", "616771620.987027"},
				{"9", "1", "1", "616771620.987027"},
				{"10", "1", "1", "616771620.987027"},
				{"11", "1", "1", "616771620.987027"},
				{"12", "1", "1", "616771620.987027"},
				{"13", "1", "1", "0.0"}
			});

			std::vector< std::vector<std::string> > reportExtendedData(
			{
				{"1","3","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
				{"2","4","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
				{"3","5","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
				{"4","6","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
				{"5","9","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
				{"6","10","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
				{"7","11","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
				{"8","12","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"}
			});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeReportRealData )
		{
			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 999.9, 2, 1, ReportTimeStep, 0.0, 0, 0.0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 999.9, 2, 1, ReportEach, 0.0, 0, 0.0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 999.9, 2, 1, ReportHourly, 0.0, 0, 0.0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 2, 1, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 2, 1, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 2, 1, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 1, 10, ReportTimeStep, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 1, 10, ReportEach, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 1, 10, ReportHourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 1, 10, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 1, 10, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 616771620.98702729, 1, 10, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportRealData, 1, "1", 0, 2, 1, ReportTimeStep, 0.0, 0, 0.0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.0" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "1", "999.9"},
				{"3", "1", "1", "999.9"},
				{"4", "1", "1", "616771620.987027"},
				{"5", "1", "1", "616771620.987027"},
				{"6", "1", "1", "616771620.987027"},
				{"7", "1", "1", "61677162.0987027"},
				{"8", "1", "1", "61677162.0987027"},
				{"9", "1", "1", "61677162.0987027"},
				{"10", "1", "1", "61677162.0987027"},
				{"11", "1", "1", "61677162.0987027"},
				{"12", "1", "1", "61677162.0987027"},
				{"13", "1", "1", "0.0"}
			});

			std::vector< std::vector<std::string> > reportExtendedData(
			{
				{"1","4","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
				{"2","5","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
				{"3","6","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
				{"4","10","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
				{"5","11","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
				{"6","12","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"}
			});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeReportIntegerData )
		{
			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 999.9, 2, 1, ReportTimeStep, 0, 0, 0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 999.9, 2, 1, ReportEach, 0, 0, 0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 999.9, 2, 1, ReportHourly, 0, 0, 0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 2, 1, ReportDaily, 4283136, 12210110, 4283196, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136, 1,10,4283196,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 2, 1, ReportMonthly, 4283136, 12210110, 4283196, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136,21, 1,10,4283196,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 2, 1, ReportSim, 4283136, 12210110, 4283196, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027,4283136,12,21, 1,10,4283196,12,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 1, 10, ReportTimeStep, 0, 0, 0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 1, 10, ReportEach, 0, 0, 0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 1, 10, ReportHourly, 0, 0, 0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 1, 10, ReportDaily, 4283136, 12210110, 4283196, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027,4283136, 1,10,4283196,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 1, 10, ReportMonthly, 4283136, 12210110, 4283196, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027,4283136,21, 1,10,4283196,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 616771620.98702729, 1, 10, ReportSim, 4283136, 12210110, 4283196, 12212460 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,61677162.0987027,4283136,12,21, 1,10,4283196,12,21,24,60" } ) ) );

			functionUsingSQLite( std::bind( WriteReportIntegerData, 1, "1", 0, 2, 1, ReportTimeStep, 0, 0, 0, 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.0" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.9"},
				{"2", "1", "1", "999.9"},
				{"3", "1", "1", "999.9"},
				{"4", "1", "1", "616771620.987027"},
				{"5", "1", "1", "616771620.987027"},
				{"6", "1", "1", "616771620.987027"},
				{"7", "1", "1", "61677162.0987027"},
				{"8", "1", "1", "61677162.0987027"},
				{"9", "1", "1", "61677162.0987027"},
				{"10", "1", "1", "61677162.0987027"},
				{"11", "1", "1", "61677162.0987027"},
				{"12", "1", "1", "61677162.0987027"},
				{"13", "1", "1", "0.0"}
			});

			std::vector< std::vector<std::string> > reportExtendedData(
			{
				{"1","4","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
				{"2","5","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
				{"3","6","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
				{"4","10","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
				{"5","11","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
				{"6","12","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"}
			});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeIntegerData )
		{
			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", 999, _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", 0, _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", -999, _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-999" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", _, 999.9 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", _, 0.0) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.0" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", _, -999.9 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", 999, 999.9 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,999.9" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", 0, 0.0) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.0" } ) ) );

			functionUsingSQLite( std::bind( WriteIntegerData, 1, "1", -999, -999.9 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-999.9" } ) ) );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{"1", "1", "1", "999.0"},
				{"2", "1", "1", "0.0"},
				{"3", "1", "1", "-999.0"},
				{"4", "1", "1", "999.9"},
				{"5", "1", "1", "0.0"},
				{"6", "1", "1", "-999.9"},
				{"7", "1", "1", "999.9"},
				{"8", "1", "1", "0.0"},
				{"9", "1", "1", "-999.9"}
			});

			std::vector< std::vector<std::string> > reportExtendedData({});

			EXPECT_EQ( reportData, reportDataResults );
			ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

		}

		TEST_F( SQLiteFixture, OutputProcessor_getStandardMeterResourceType )
		{
			std::map< std::string, std::string > const resource_map = {
				{ "ELECTRICITY", "Electricity" },
				{ "ELECTRIC", "Electricity" },
				{ "ELEC", "Electricity" },
				{ "GAS", "Gas" },
				{ "NATURALGAS", "Gas" },
				{ "NATURAL GAS", "Gas" },
				{ "GASOLINE", "Gasoline" },
				{ "DIESEL", "Diesel" },
				{ "COAL", "Coal" },
				{ "FUEL OIL #1", "FuelOil#1" },
				{ "FUELOIL#1", "FuelOil#1" },
				{ "FUEL OIL", "FuelOil#1" },
				{ "DISTILLATE OIL", "FuelOil#1" },
				{ "FUEL OIL #2", "FuelOil#2" },
				{ "FUELOIL#2", "FuelOil#2" },
				{ "RESIDUAL OIL", "FuelOil#2" },
				{ "PROPANE", "Propane" },
				{ "LPG", "Propane" },
				{ "PROPANEGAS", "Propane" },
				{ "PROPANE GAS", "Propane" },
				{ "WATER", "Water" },
				{ "H2O", "Water" },
				{ "ONSITEWATER", "OnSiteWater" },
				{ "WATERPRODUCED", "OnSiteWater" },
				{ "ONSITE WATER", "OnSiteWater" },
				{ "MAINSWATER", "MainsWater" },
				{ "WATERSUPPLY", "MainsWater" },
				{ "RAINWATER", "RainWater" },
				{ "PRECIPITATION", "RainWater" },
				{ "WELLWATER", "WellWater" },
				{ "GROUNDWATER", "WellWater" },
				{ "CONDENSATE", "Condensate" },
				{ "ENERGYTRANSFER", "EnergyTransfer" },
				{ "ENERGYXFER", "EnergyTransfer" },
				{ "XFER", "EnergyTransfer" },
				{ "STEAM", "Steam" },
				{ "DISTRICTCOOLING", "DistrictCooling" },
				{ "DISTRICTHEATING", "DistrictHeating" },
				{ "ELECTRICITYPRODUCED", "ElectricityProduced" },
				{ "ELECTRICITYPURCHASED", "ElectricityPurchased" },
				{ "ELECTRICITYSURPLUSSOLD", "ElectricitySurplusSold" },
				{ "ELECTRICITYNET", "ElectricityNet" },
				{ "SOLARWATER", "SolarWater" },
				{ "SOLARAIR", "SolarAir" },
				{ "SO2", "SO2" },
				{ "NOX", "NOx" },
				{ "N2O", "N2O" },
				{ "PM", "PM" },
				{ "PM2.5", "PM2.5" },
				{ "PM10", "PM10" },
				{ "CO", "CO" },
				{ "CO2", "CO2" },
				{ "CH4", "CH4" },
				{ "NH3", "NH3" },
				{ "NMVOC", "NMVOC" },
				{ "HG", "Hg" },
				{ "PB", "Pb" },
				{ "NUCLEAR HIGH", "Nuclear High" },
				{ "NUCLEAR LOW", "Nuclear Low" },
				{ "WATERENVIRONMENTALFACTORS", "WaterEnvironmentalFactors" },
				{ "CARBON EQUIVALENT", "Carbon Equivalent" },
				{ "SOURCE", "Source" },
				{ "PLANTLOOPHEATINGDEMAND", "PlantLoopHeatingDemand" },
				{ "PLANTLOOPCOOLINGDEMAND", "PlantLoopCoolingDemand" },
				{ "GENERIC", "Generic" },
				{ "OTHERFUEL1", "OtherFuel1" },
				{ "OTHERFUEL2", "OtherFuel2" }
			};

			std::string out_resource_type;
			bool error_found = false;

			for( auto const & meterType : resource_map ) {
				GetStandardMeterResourceType( out_resource_type, meterType.first, error_found );
				EXPECT_EQ( meterType.second, out_resource_type );
				EXPECT_FALSE( error_found );
			}

			sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

			auto const meterType = "BAD INPUT";
			out_resource_type = "BAD INPUT";

			EnergyPlus::sqlite = std::move( sqlite_test );
			GetStandardMeterResourceType( out_resource_type, meterType, error_found );
			sqlite_test = std::move( EnergyPlus::sqlite );

			EXPECT_EQ( meterType, out_resource_type );
			EXPECT_TRUE( error_found );

			auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

			ASSERT_EQ(1ul, errorData.size());
			std::vector<std::string> errorData0 {"1", "1", "1", "GetStandardMeterResourceType: Illegal OutResourceType (for Meters) Entered=BAD INPUT", "1"};
			EXPECT_EQ(errorData0, errorData[0]);

		}

		TEST_F( SQLiteFixture, OutputProcessor_determineIndexGroupKeyFromMeterName )
		{
			std::map< std::string, int > const resource_map = {
				{ "Electricity:Facility", 100 },
				{ "Gas:Facility", 101 },
				{ "DistricHeating:Facility", 102 },
				{ "DistricCooling:Facility", 103 },
				{ "ElectricityNet:Facility", 104 },
				{ "Electricity:Building", 201 },
				{ "Gas:Building", 202 },
				{ "Electricity:HVAC", 301 },
				{ "InteriorLights:Electricity", 401 },
				{ "InteriorLights:Electricity:Zone", 501 },
				{ "BAD INPUT", -11 }
			};

			for( auto const & indexGroup : resource_map ) {
				EXPECT_EQ( indexGroup.second, DetermineIndexGroupKeyFromMeterName( indexGroup.first ) ) << "where meterName is " << indexGroup.first;
			}
		}

		TEST_F( SQLiteFixture, OutputProcessor_validateIndexType )
		{
			std::map< std::string, int > const resource_map = {
				{ "ZONE", 1 },
				{ "HEATBALANCE", 1 },
				{ "HEAT BALANCE", 1 },
				{ "HVAC", 2 },
				{ "SYSTEM", 2 },
				{ "PLANT", 2 }
			};

			auto const calledFrom = "UnitTest";

			for( auto const & indexGroup : resource_map ) {
				EXPECT_EQ( indexGroup.second, ValidateIndexType( indexGroup.first, calledFrom ) ) << "where indexTypeKey is " << indexGroup.first;
			}

		}

		TEST_F( SQLiteFixture, OutputProcessor_DeathTest_validateIndexType )
		{
			auto const calledFrom = "UnitTest";
			EXPECT_ANY_THROW( ValidateIndexType( "BAD INPUT", calledFrom ) );

		}

		TEST_F( SQLiteFixture, OutputProcessor_standardIndexTypeKey )
		{
			EXPECT_EQ( "Zone", StandardIndexTypeKey( 1 ) );
			EXPECT_EQ( "HVAC", StandardIndexTypeKey( 2 ) );
			EXPECT_EQ( "UNKW", StandardIndexTypeKey( 0 ) );
			EXPECT_EQ( "UNKW", StandardIndexTypeKey( -1 ) );
			EXPECT_EQ( "UNKW", StandardIndexTypeKey( 3 ) );

		}

		TEST_F( SQLiteFixture, OutputProcessor_validateVariableType )
		{
			std::map< std::string, int > const resource_map = {
				{ "STATE", 1 },
				{ "AVERAGE", 1 },
				{ "AVERAGED", 1 },
				{ "NON STATE", 2 },
				{ "NONSTATE", 2 },
				{ "SUM", 2 },
				{ "SUMMED", 2 }
			};

			for( auto const & variableType : resource_map ) {
				EXPECT_EQ( variableType.second, ValidateVariableType( variableType.first ) ) << "where variableTypeKey is " << variableType.first;
			}

			sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

			std::string const variableTypeKey = "BAD INPUT";

			EnergyPlus::sqlite = std::move( sqlite_test );
			auto index = ValidateVariableType( variableTypeKey );
			sqlite_test = std::move( EnergyPlus::sqlite );
			// See note in SQLiteFixture.hh, can maybe move to this in the future.
			// auto index = functionUsingSQLite( std::bind( ValidateVariableType, variableTypeKey ) );
			// auto index = functionUsingSQLite<int>( std::bind( ValidateVariableType, variableTypeKey ) );

			EXPECT_EQ( 0, index );

			auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

			ASSERT_EQ(1ul, errorData.size());
			std::vector<std::string> errorData0 {"1", "1", "1", "Invalid variable type requested=BAD INPUT", "1"};
			EXPECT_EQ(errorData0, errorData[0]);

		}

		TEST_F( SQLiteFixture, OutputProcessor_standardVariableTypeKey )
		{
			EXPECT_EQ( "Average", StandardVariableTypeKey( 1 ) );
			EXPECT_EQ( "Sum", StandardVariableTypeKey( 2 ) );
			EXPECT_EQ( "Unknown", StandardVariableTypeKey( 0 ) );
			EXPECT_EQ( "Unknown", StandardVariableTypeKey( -1 ) );
			EXPECT_EQ( "Unknown", StandardVariableTypeKey( 3 ) );

		}

		TEST_F( SQLiteFixture, OutputProcessor_determineMeterIPUnits )
		{
			int ipUnits = -999999;
			bool errorFound = false;

			DetermineMeterIPUnits( ipUnits, "ELEC", "J", errorFound );
			EXPECT_EQ( RT_IPUnits_Electricity, ipUnits );
			EXPECT_FALSE( errorFound );

			DetermineMeterIPUnits( ipUnits, "GAS", "J", errorFound );
			EXPECT_EQ( RT_IPUnits_Gas, ipUnits );
			EXPECT_FALSE( errorFound );

			DetermineMeterIPUnits( ipUnits, "COOL", "J", errorFound );
			EXPECT_EQ( RT_IPUnits_Cooling, ipUnits );
			EXPECT_FALSE( errorFound );

			DetermineMeterIPUnits( ipUnits, "WATER", "m3", errorFound );
			EXPECT_EQ( RT_IPUnits_Water, ipUnits );
			EXPECT_FALSE( errorFound );

			DetermineMeterIPUnits( ipUnits, "OTHER", "m3", errorFound );
			EXPECT_EQ( RT_IPUnits_OtherM3, ipUnits );
			EXPECT_FALSE( errorFound );

			DetermineMeterIPUnits( ipUnits, "OTHER", "kg", errorFound );
			EXPECT_EQ( RT_IPUnits_OtherKG, ipUnits );
			EXPECT_FALSE( errorFound );

			DetermineMeterIPUnits( ipUnits, "OTHER", "L", errorFound );
			EXPECT_EQ( RT_IPUnits_OtherL, ipUnits );
			EXPECT_FALSE( errorFound );

			sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

			EnergyPlus::sqlite = std::move( sqlite_test );

			ipUnits = -999999;
			DetermineMeterIPUnits( ipUnits, "UNKONWN", "badUnits", errorFound );
			EXPECT_EQ( RT_IPUnits_OtherJ, ipUnits );
			EXPECT_TRUE( errorFound );

			ipUnits = -999999;
			DetermineMeterIPUnits( ipUnits, "ELEC", "kWh", errorFound );
			EXPECT_EQ( RT_IPUnits_Electricity, ipUnits );
			EXPECT_TRUE( errorFound );

			sqlite_test = std::move( EnergyPlus::sqlite );

			auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

			ASSERT_EQ(2ul, errorData.size());
			std::vector<std::string> errorData0 {"1", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[badUnits].", "1"};
			std::vector<std::string> errorData1 {"2", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[kWh].", "1"};
			EXPECT_EQ(errorData0, errorData[0]);
			EXPECT_EQ(errorData1, errorData[1]);

		}

		TEST_F( SQLiteFixture, OutputProcessor_dateToStringWithMonth )
		{
			EXPECT_EQ( "01-JAN-00:01", DateToStringWithMonth( 1010101 ) );
			EXPECT_EQ( "01-JAN-00:00", DateToStringWithMonth( 1010100 ) );
			EXPECT_EQ( "01-FEB-01:00", DateToStringWithMonth( 2010160 ) );
			EXPECT_EQ( "20-MAR-01:59", DateToStringWithMonth( 3200259 ) );
			EXPECT_EQ( "13-APR-23:59", DateToStringWithMonth( 4132459 ) );
			EXPECT_EQ( "15-MAY-20:30", DateToStringWithMonth( 5152130 ) );
			EXPECT_EQ( "19-JUN-12:10", DateToStringWithMonth( 6191310 ) );
			EXPECT_EQ( "25-JUL-19:40", DateToStringWithMonth( 7252040 ) );
			EXPECT_EQ( "05-AUG-06:22", DateToStringWithMonth( 8050722 ) );
			EXPECT_EQ( "03-SEP-09:50", DateToStringWithMonth( 9031050 ) );
			EXPECT_EQ( "27-OCT-03:31", DateToStringWithMonth( 10270431 ) );
			EXPECT_EQ( "08-NOV-22:28", DateToStringWithMonth( 11082328 ) );
			EXPECT_EQ( "21-DEC-00:10", DateToStringWithMonth( 12210110 ) );

			EXPECT_EQ( "-", DateToStringWithMonth( 0 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( -9999 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( -12210110 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 13082328 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 10100 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 1000101 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 990101 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 1010001 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 1009901 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 1010099 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 1320100 ) );
			EXPECT_EQ( "-", DateToStringWithMonth( 1012500 ) );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeMeterDictionaryItem )
		{
			InitializeOutput();

			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportTimeStep, 1, 1, -999, "indexGroup", "1", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,1,meterName [meterUnits] !TimeStep" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1,meterName [meterUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportTimeStep, 2, 2, -999, "indexGroup", "2", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "2,1,meterName [meterUnits] !TimeStep" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "2,1,meterName [meterUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportTimeStep, 1, 3, -999, "indexGroup", "3", "meterName", "meterUnits", true, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "3,1,Cumulative meterName [meterUnits] !TimeStep" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "3,1,Cumulative meterName [meterUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportTimeStep, 1, 4, -999, "indexGroup", "4", "meterName", "meterUnits", false, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "4,1,meterName [meterUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportTimeStep, 1, 5, -999, "indexGroup", "5", "meterName", "meterUnits", true, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "5,1,Cumulative meterName [meterUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportEach, 1, 6, -999, "indexGroup", "6", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "6,1,meterName [meterUnits] !Each Call" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "6,1,meterName [meterUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportEach, 2, 7, -999, "indexGroup", "7", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "7,1,meterName [meterUnits] !Each Call" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "7,1,meterName [meterUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportEach, 1, 8, -999, "indexGroup", "8", "meterName", "meterUnits", true, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "8,1,Cumulative meterName [meterUnits] !Each Call" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "8,1,Cumulative meterName [meterUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportEach, 1, 9, -999, "indexGroup", "9", "meterName", "meterUnits", false, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "9,1,meterName [meterUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportEach, 1, 10, -999, "indexGroup", "10", "meterName", "meterUnits", true, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "10,1,Cumulative meterName [meterUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportHourly, 1, 11, -999, "indexGroup", "11", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "11,1,meterName [meterUnits] !Hourly" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "11,1,meterName [meterUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportHourly, 2, 12, -999, "indexGroup", "12", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "12,1,meterName [meterUnits] !Hourly" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "12,1,meterName [meterUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportHourly, 1, 13, -999, "indexGroup", "13", "meterName", "meterUnits", true, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "13,1,Cumulative meterName [meterUnits] !Hourly" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "13,1,Cumulative meterName [meterUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportHourly, 1, 14, -999, "indexGroup", "14", "meterName", "meterUnits", false, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "14,1,meterName [meterUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportHourly, 1, 15, -999, "indexGroup", "15", "meterName", "meterUnits", true, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "15,1,Cumulative meterName [meterUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportDaily, 1, 16, -999, "indexGroup", "16", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "16,7,meterName [meterUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "16,7,meterName [meterUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportDaily, 2, 17, -999, "indexGroup", "17", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "17,7,meterName [meterUnits] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "17,7,meterName [meterUnits] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportDaily, 1, 18, -999, "indexGroup", "18", "meterName", "meterUnits", true, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "18,1,Cumulative meterName [meterUnits] !Daily " } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "18,1,Cumulative meterName [meterUnits] !Daily " } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportDaily, 1, 19, -999, "indexGroup", "19", "meterName", "meterUnits", false, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "19,7,meterName [meterUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportDaily, 1, 20, -999, "indexGroup", "20", "meterName", "meterUnits", true, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "20,1,Cumulative meterName [meterUnits] !Daily " } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportMonthly, 1, 21, -999, "indexGroup", "21", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "21,9,meterName [meterUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "21,9,meterName [meterUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportMonthly, 2, 22, -999, "indexGroup", "22", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "22,9,meterName [meterUnits] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "22,9,meterName [meterUnits] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportMonthly, 1, 23, -999, "indexGroup", "23", "meterName", "meterUnits", true, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "23,1,Cumulative meterName [meterUnits] !Monthly " } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "23,1,Cumulative meterName [meterUnits] !Monthly " } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportMonthly, 1, 24, -999, "indexGroup", "24", "meterName", "meterUnits", false, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "24,9,meterName [meterUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportMonthly, 1, 25, -999, "indexGroup", "25", "meterName", "meterUnits", true, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "25,1,Cumulative meterName [meterUnits] !Monthly " } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportSim, 1, 26, -999, "indexGroup", "26", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "26,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "26,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportSim, 2, 27, -999, "indexGroup", "27", "meterName", "meterUnits", false, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "27,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "27,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportSim, 1, 28, -999, "indexGroup", "28", "meterName", "meterUnits", true, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "28,1,Cumulative meterName [meterUnits] !RunPeriod " } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "28,1,Cumulative meterName [meterUnits] !RunPeriod " } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportSim, 1, 29, -999, "indexGroup", "29", "meterName", "meterUnits", false, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "29,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteMeterDictionaryItem, ReportSim, 1, 30, -999, "indexGroup", "30", "meterName", "meterUnits", true, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "30,1,Cumulative meterName [meterUnits] !RunPeriod " } ) ) );

			auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

			std::vector< std::vector<std::string> > reportDataDictionary(
			{
				{"1", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"},
				{"2", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"},
				{"3", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "meterUnits"},
				{"4", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"},
				{"5", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "meterUnits"},
				{"6", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"},
				{"7", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"},
				{"8", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "meterUnits"},
				{"9", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"},
				{"10", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "meterUnits"},
				{"11", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"},
				{"12", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"},
				{"13", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", "meterUnits"},
				{"14", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"},
				{"15", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", "meterUnits"},
				{"16", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"},
				{"17", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"},
				{"18", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "meterUnits"},
				{"19", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"},
				{"20", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "meterUnits"},
				{"21", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"},
				{"22", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"},
				{"23", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "meterUnits"},
				{"24", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"},
				{"25", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "meterUnits"},
				{"26", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"},
				{"27", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"},
				{"28", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "meterUnits"},
				{"29", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"},
				{"30", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "meterUnits"}
			});
			EXPECT_EQ( reportDataDictionary, reportDataDictionaryResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeReportVariableDictionaryItem )
		{
			InitializeOutput();

			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportTimeStep, 1, 1, -999, "indexGroup", "1", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1,keyedValue,variableName [variableUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportTimeStep, 2, 2, -999, "indexGroup", "2", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "2,1,keyedValue,variableName [variableUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportTimeStep, 1, 3, -999, "indexGroup", "3", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "3,1,keyedValue,variableName [variableUnits] !TimeStep,scheduleName" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportTimeStep, 1, 4, -999, "indexGroup", "4", "keyedValue", "variableName", 2, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "4,1,keyedValue,variableName [variableUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportTimeStep, 1, 5, -999, "indexGroup", "5", "keyedValue", "variableName", 3, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "5,1,keyedValue,variableName [variableUnits] !TimeStep" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportEach, 1, 6, -999, "indexGroup", "6", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "6,1,keyedValue,variableName [variableUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportEach, 2, 7, -999, "indexGroup", "7", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "7,1,keyedValue,variableName [variableUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportEach, 1, 8, -999, "indexGroup", "8", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "8,1,keyedValue,variableName [variableUnits] !Each Call,scheduleName" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportEach, 1, 9, -999, "indexGroup", "9", "keyedValue", "variableName", 2, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "9,1,keyedValue,variableName [variableUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportEach, 1, 10, -999, "indexGroup", "10", "keyedValue", "variableName", 3, "variableUnits", _ ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "10,1,keyedValue,variableName [variableUnits] !Each Call" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportHourly, 1, 11, -999, "indexGroup", "11", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingHourlyVariables );
			TrackingHourlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "11,1,keyedValue,variableName [variableUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportHourly, 2, 12, -999, "indexGroup", "12", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingHourlyVariables );
			TrackingHourlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "12,1,keyedValue,variableName [variableUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportHourly, 1, 13, -999, "indexGroup", "13", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" ) );
			EXPECT_TRUE( TrackingHourlyVariables );
			TrackingHourlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "13,1,keyedValue,variableName [variableUnits] !Hourly,scheduleName" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportHourly, 1, 14, -999, "indexGroup", "14", "keyedValue", "variableName", 2, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingHourlyVariables );
			TrackingHourlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "14,1,keyedValue,variableName [variableUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportHourly, 1, 15, -999, "indexGroup", "15", "keyedValue", "variableName", 3, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingHourlyVariables );
			TrackingHourlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "15,1,keyedValue,variableName [variableUnits] !Hourly" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportDaily, 1, 16, -999, "indexGroup", "16", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingDailyVariables );
			TrackingDailyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "16,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportDaily, 2, 17, -999, "indexGroup", "17", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingDailyVariables );
			TrackingDailyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "17,7,keyedValue,variableName [variableUnits] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportDaily, 1, 18, -999, "indexGroup", "18", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" ) );
			EXPECT_TRUE( TrackingDailyVariables );
			TrackingDailyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "18,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute],scheduleName" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportDaily, 1, 19, -999, "indexGroup", "19", "keyedValue", "variableName", 2, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingDailyVariables );
			TrackingDailyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "19,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportDaily, 1, 20, -999, "indexGroup", "20", "keyedValue", "variableName", 3, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingDailyVariables );
			TrackingDailyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "20,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportMonthly, 1, 21, -999, "indexGroup", "21", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingMonthlyVariables );
			TrackingMonthlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "21,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportMonthly, 2, 22, -999, "indexGroup", "22", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingMonthlyVariables );
			TrackingMonthlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "22,9,keyedValue,variableName [variableUnits] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportMonthly, 1, 23, -999, "indexGroup", "23", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" ) );
			EXPECT_TRUE( TrackingMonthlyVariables );
			TrackingMonthlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "23,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute],scheduleName" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportMonthly, 1, 24, -999, "indexGroup", "24", "keyedValue", "variableName", 2, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingMonthlyVariables );
			TrackingMonthlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "24,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportMonthly, 1, 25, -999, "indexGroup", "25", "keyedValue", "variableName", 3, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingMonthlyVariables );
			TrackingMonthlyVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "25,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportSim, 1, 26, -999, "indexGroup", "26", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingRunPeriodVariables );
			TrackingRunPeriodVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "26,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportSim, 2, 27, -999, "indexGroup", "27", "keyedValue", "variableName", 1, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingRunPeriodVariables );
			TrackingRunPeriodVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "27,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportSim, 1, 28, -999, "indexGroup", "28", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" ) );
			EXPECT_TRUE( TrackingRunPeriodVariables );
			TrackingRunPeriodVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "28,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute],scheduleName" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportSim, 1, 29, -999, "indexGroup", "29", "keyedValue", "variableName", 2, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingRunPeriodVariables );
			TrackingRunPeriodVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "29,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );

			functionUsingSQLite( std::bind( WriteReportVariableDictionaryItem, ReportSim, 1, 30, -999, "indexGroup", "30", "keyedValue", "variableName", 3, "variableUnits", _ ) );
			EXPECT_TRUE( TrackingRunPeriodVariables );
			TrackingRunPeriodVariables = false;
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "30,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ) ) );

			auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

			std::vector< std::vector<std::string> > reportDataDictionary(
			{
				{"1", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
				{"2", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
				{"3", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "scheduleName", "variableUnits"},
				{"4", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
				{"5", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
				{"6", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
				{"7", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
				{"8", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "scheduleName", "variableUnits"},
				{"9", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
				{"10", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
				{"11", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
				{"12", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
				{"13", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "scheduleName", "variableUnits"},
				{"14", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
				{"15", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
				{"16", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "", "variableUnits"},
				{"17", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "", "variableUnits"},
				{"18", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "scheduleName", "variableUnits"},
				{"19", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Daily", "", "variableUnits"},
				{"20", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Daily", "", "variableUnits"},
				{"21", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
				{"22", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
				{"23", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "scheduleName", "variableUnits"},
				{"24", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
				{"25", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
				{"26", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "", "variableUnits"},
				{"27", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "", "variableUnits"},
				{"28", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "scheduleName", "variableUnits"},
				{"29", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Run Period", "", "variableUnits"},
				{"30", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Run Period", "", "variableUnits"}
			});
			EXPECT_EQ( reportDataDictionary, reportDataDictionaryResults );

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeCumulativeReportMeterData )
		{
			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

			functionUsingSQLite( std::bind( WriteCumulativeReportMeterData, 1, "1", 616771620.98702729, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027" } ) ) );

			functionUsingSQLite( std::bind( WriteCumulativeReportMeterData, 1, "1", 616771620.98702729, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,616771620.987027" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,616771620.987027" } ) ) );

			functionUsingSQLite( std::bind( WriteCumulativeReportMeterData, 1, "1", 0, true ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,0.0" } ) ) );

			functionUsingSQLite( std::bind( WriteCumulativeReportMeterData, 1, "1", 0, false ) );
			EXPECT_TRUE( compare_mtr_stream( delimited_string( { "1,0.0" } ) ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.0" } ) ) );

			auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			ASSERT_EQ(4ul, reportData.size());
			std::vector<std::string> reportData0 {"1", "1", "1", "616771620.987027"};
			std::vector<std::string> reportData1 {"2", "1", "1", "616771620.987027"};
			std::vector<std::string> reportData2 {"3", "1", "1", "0.0"};
			std::vector<std::string> reportData3 {"4", "1", "1", "0.0"};
			EXPECT_EQ(reportData0, reportData[0]);
			EXPECT_EQ(reportData1, reportData[1]);
			EXPECT_EQ(reportData2, reportData[2]);
			EXPECT_EQ(reportData3, reportData[3]);

			ASSERT_EQ(0ul, reportExtendedData.size());

		}

		TEST_F( SQLiteFixture, OutputProcessor_writeRealData )
		{
			sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
			sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.0" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 0.1 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.1" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", -0.1 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-0.1" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-2 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-002" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-02" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-3 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-003" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-03" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-4 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-004" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-04" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-5 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-005" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-05" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-6 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-006" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-06" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-7 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-007" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-07" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-8 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-008" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-08" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-9 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-009" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-09" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-10 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-010" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-10" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-11 ) );
			// this seems to always be low... not 1.0e-11
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,9.999999999999999E-012" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,9.999999999999999E-12" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-12 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-012" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-12" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-13 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-013" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-13" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-14 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-014" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-14" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-15 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-015" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-15" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-16 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-016" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-16" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", -1.0e-16 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-1.000000000000000E-016" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-1.000000000000000E-16" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e-19 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-019" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E-19" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 0.5 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,0.5" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 10.0 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,10." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e2 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,100." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e3 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e4 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,10000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e5 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,100000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e6 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e7 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,10000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e8 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,100000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e9 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1000000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e10 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,10000000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e11 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,100000000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e12 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1000000000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e13 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,10000000000000." } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e14 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,100000000000000" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e15 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1000000000000000" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e16 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,10000000000000000" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e17 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E+017" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E+17" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", -1.0e16 ) );
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-10000000000000000" } ) ) );

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", -1.0e17 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-1.000000000000000E+017" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,-1.000000000000000E+17" } ) ) );
		#endif

			functionUsingSQLite( std::bind( WriteRealData, 1, "1", 1.0e25 ) );
		#if defined( _WIN32 ) && _MSC_VER < 1900
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E+025" } ) ) );
		#else
			EXPECT_TRUE( compare_eso_stream( delimited_string( { "1,1.000000000000000E+25" } ) ) );
		#endif

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData(
			{
				{ "1", "1", "1", "0.0" },
				{ "2", "1", "1", "0.1" },
				{ "3", "1", "1", "-0.1" },
				{ "4", "1", "1", "0.01" },
				{ "5", "1", "1", "0.001" },
				{ "6", "1", "1", "0.0001" },
				{ "7", "1", "1", "1.0e-05" },
				{ "8", "1", "1", "1.0e-06" },
				{ "9", "1", "1", "1.0e-07" },
				{ "10", "1", "1", "1.0e-08" },
				{ "11", "1", "1", "1.0e-09" },
				{ "12", "1", "1", "1.0e-10" },
				{ "13", "1", "1", "1.0e-11" },
				{ "14", "1", "1", "1.0e-12" },
				{ "15", "1", "1", "1.0e-13" },
				{ "16", "1", "1", "1.0e-14" },
				{ "17", "1", "1", "1.0e-15" },
				{ "18", "1", "1", "1.0e-16" },
				{ "19", "1", "1", "-1.0e-16" },
				{ "20", "1", "1", "1.0e-19" },
				{ "21", "1", "1", "0.5" },
				{ "22", "1", "1", "1.0" },
				{ "23", "1", "1", "10.0" },
				{ "24", "1", "1", "100.0" },
				{ "25", "1", "1", "1000.0" },
				{ "26", "1", "1", "10000.0" },
				{ "27", "1", "1", "100000.0" },
				{ "28", "1", "1", "1000000.0" },
				{ "29", "1", "1", "10000000.0" },
				{ "30", "1", "1", "100000000.0" },
				{ "31", "1", "1", "1000000000.0" },
				{ "32", "1", "1", "10000000000.0" },
				{ "33", "1", "1", "100000000000.0" },
				{ "34", "1", "1", "1000000000000.0" },
				{ "35", "1", "1", "10000000000000.0" },
				{ "36", "1", "1", "100000000000000.0" },
				{ "37", "1", "1", "1.0e+15" },
				{ "38", "1", "1", "1.0e+16" },
				{ "39", "1", "1", "1.0e+17" },
				{ "40", "1", "1", "-1.0e+16" },
				{ "41", "1", "1", "-1.0e+17" },
				{ "42", "1", "1", "1.0e+25" }
			});

			std::vector< std::vector<std::string> > reportExtendedData({});

			EXPECT_EQ( reportData, reportDataResults );
			ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

		}

		TEST_F( SQLiteFixture, OutputProcessor_addMeter )
		{
			auto const name( "testMeter" );
			auto const units( "J" );
			auto const resourceType( "ELEC" );
			auto const endUse( "testEndUse" );
			auto const endUseSub( "testEndUseSub" );
			auto const group( "testGroup" );

			EXPECT_EQ( 0, NumEnergyMeters );
			EXPECT_EQ( 0ul, EnergyMeters.size() );

			AddMeter( name, units, resourceType, endUse, endUseSub, group );

			ASSERT_EQ( 1, NumEnergyMeters );
			ASSERT_EQ( 1ul, EnergyMeters.size() );

			EXPECT_EQ( name, EnergyMeters( 1 ).Name );
			EXPECT_EQ( resourceType, EnergyMeters( 1 ).ResourceType );
			EXPECT_EQ( endUse, EnergyMeters( 1 ).EndUse );
			EXPECT_EQ( endUseSub, EnergyMeters( 1 ).EndUseSub );
			EXPECT_EQ( group, EnergyMeters( 1 ).Group );
			EXPECT_EQ( units, EnergyMeters( 1 ).Units );
			EXPECT_EQ( 1, EnergyMeters( 1 ).TSRptNum );
			EXPECT_EQ( 2, EnergyMeters( 1 ).HRRptNum );
			EXPECT_EQ( 3, EnergyMeters( 1 ).DYRptNum );
			EXPECT_EQ( 4, EnergyMeters( 1 ).MNRptNum );
			EXPECT_EQ( 5, EnergyMeters( 1 ).SMRptNum );
			EXPECT_EQ( 6, EnergyMeters( 1 ).TSAccRptNum );
			EXPECT_EQ( 7, EnergyMeters( 1 ).HRAccRptNum );
			EXPECT_EQ( 8, EnergyMeters( 1 ).DYAccRptNum );
			EXPECT_EQ( 9, EnergyMeters( 1 ).MNAccRptNum );
			EXPECT_EQ( 10, EnergyMeters( 1 ).SMAccRptNum );

			EXPECT_EQ( 1, NumEnergyMeters );
			EXPECT_EQ( 1ul, EnergyMeters.size() );

			sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

			auto const name2( "testMeter2" );
			auto const units2( "kWh" );
			auto const resourceType2( "OTHER" );
			auto const endUse2( "testEndUse2" );
			auto const endUseSub2( "testEndUseSub2" );
			auto const group2( "testGroup2" );
			functionUsingSQLite( std::bind(AddMeter, name2, units2, resourceType2, endUse2, endUseSub2, group2 ) );

			auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

			ASSERT_EQ(1ul, errorData.size());
			std::vector<std::string> errorData0 {"1", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[kWh].  ..on Meter=\"testMeter2\".  ..requests for IP units from this meter will be ignored.", "1"};
			EXPECT_EQ(errorData0, errorData[0]);

			ASSERT_EQ( 2, NumEnergyMeters );
			ASSERT_EQ( 2ul, EnergyMeters.size() );

		}

		TEST_F( SQLiteFixture, OutputProcessor_validateNStandardizeMeterTitles )
		{
			std::vector< std::vector< std::string > > input_map = {
				{ "J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "SYSTEM" },
				{ "J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "PLANT" },
				{ "J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "BUILDING", "zoneName" },
				{ "J", "ELEC", "INTERIORLIGHTS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "EXTERIOR LIGHTS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "EXTERIORLIGHTS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HTG", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATPRODUCED", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COOLING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "CLG", "endUseSub", "HVAC" },
				{ "J", "ELEC", "DOMESTICHOTWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "DHW", "endUseSub", "HVAC" },
				{ "J", "ELEC", "DOMESTIC HOT WATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COGEN", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COGENERATION", "endUseSub", "HVAC" },
				{ "J", "ELEC", "INTERIOREQUIPMENT", "endUseSub", "HVAC" },
				{ "J", "ELEC", "INTERIOR EQUIPMENT", "endUseSub", "HVAC" },
				{ "J", "ELEC", "EXTERIOREQUIPMENT", "endUseSub", "HVAC" },
				{ "J", "ELEC", "EXTERIOR EQUIPMENT", "endUseSub", "HVAC" },
				{ "J", "ELEC", "EXT EQ", "endUseSub", "HVAC" },
				{ "J", "ELEC", "EXTERIOREQ", "endUseSub", "HVAC" },
				{ "J", "ELEC", "EXTERIOR:WATEREQUIPMENT", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASEDHOTWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "DISTRICTHOTWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASED HEATING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASEDCOLDWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "DISTRICTCHILLEDWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASEDCHILLEDWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASED COLD WATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASED COOLING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "FANS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "FAN", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATINGCOILS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATINGCOIL", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATING COILS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATING COIL", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COOLINGCOILS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COOLINGCOIL", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COOLING COILS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COOLING COIL", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PUMPS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PUMP", "endUseSub", "HVAC" },
				{ "J", "ELEC", "FREECOOLING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "FREE COOLING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "LOOPTOLOOP", "endUseSub", "HVAC" },
				{ "J", "ELEC", "CHILLERS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "CHILLER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "BOILERS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "BOILER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "BASEBOARD", "endUseSub", "HVAC" },
				{ "J", "ELEC", "BASEBOARDS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATREJECTION", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEAT REJECTION", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HUMIDIFIER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HUMIDIFIERS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATRECOVERY", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEAT RECOVERY", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PHOTOVOLTAICS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PV", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PHOTOVOLTAIC", "endUseSub", "HVAC" },
				{ "J", "ELEC", "WINDTURBINES", "endUseSub", "HVAC" },
				{ "J", "ELEC", "WT", "endUseSub", "HVAC" },
				{ "J", "ELEC", "WINDTURBINE", "endUseSub", "HVAC" },
				{ "J", "ELEC", "ELECTRICSTORAGE", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEAT RECOVERY FOR COOLING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATRECOVERYFORCOOLING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATRECOVERYCOOLING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEAT RECOVERY FOR HEATING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATRECOVERYFORHEATING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "HEATRECOVERYHEATING", "endUseSub", "HVAC" },
				{ "J", "ELEC", "ELECTRICEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASEDELECTRICEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "SOLDELECTRICEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "NATURALGASEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "FUELOIL#1EMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "FUELOIL#2EMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COALEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "GASOLINEEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PROPANEEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "DIESELEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "OTHERFUEL1EMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "OTHERFUEL2EMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "CARBONEQUIVALENTEMISSIONS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "REFRIGERATION", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COLDSTORAGECHARGE", "endUseSub", "HVAC" },
				{ "J", "ELEC", "COLDSTORAGEDISCHARGE", "endUseSub", "HVAC" },
				{ "J", "ELEC", "WATERSYSTEMS", "endUseSub", "HVAC" },
				{ "J", "ELEC", "WATERSYSTEM", "endUseSub", "HVAC" },
				// { "J", "ELEC", "Water System", "endUseSub", "HVAC" },  // This one fails because Water System isn't a proper choice (needs to be upper cased in code...)
				{ "J", "ELEC", "RAINWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "CONDENSATE", "endUseSub", "HVAC" },
				{ "J", "ELEC", "WELLWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "MAINSWATER", "endUseSub", "HVAC" },
				{ "J", "ELEC", "PURCHASEDWATER", "endUseSub", "HVAC" }
			};

			std::vector< std::string > const result_map = {
				"Electricity:Facility",
				"Electricity:HVAC",
				"InteriorLights:Electricity",
				"endUseSub:InteriorLights:Electricity",
				"Electricity:Plant",
				"Electricity:Building",
				"Electricity:Zone:zoneName",
				"InteriorLights:Electricity:Zone:zoneName",
				"ExteriorLights:Electricity",
				"endUseSub:ExteriorLights:Electricity",
				"Heating:Electricity",
				"endUseSub:Heating:Electricity",
				"HeatProduced:Electricity",
				"endUseSub:HeatProduced:Electricity",
				"Cooling:Electricity",
				"endUseSub:Cooling:Electricity",
				"WaterSystems:Electricity",
				"endUseSub:WaterSystems:Electricity",
				"Cogeneration:Electricity",
				"endUseSub:Cogeneration:Electricity",
				"InteriorEquipment:Electricity",
				"endUseSub:InteriorEquipment:Electricity",
				"ExteriorEquipment:Electricity",
				"endUseSub:ExteriorEquipment:Electricity",
				"DistrictHotWater:Electricity",
				"endUseSub:DistrictHotWater:Electricity",
				"DistrictChilledWater:Electricity",
				"endUseSub:DistrictChilledWater:Electricity",
				"Fans:Electricity",
				"endUseSub:Fans:Electricity",
				"HeatingCoils:Electricity",
				"endUseSub:HeatingCoils:Electricity",
				"CoolingCoils:Electricity",
				"endUseSub:CoolingCoils:Electricity",
				"Pumps:Electricity",
				"endUseSub:Pumps:Electricity",
				"Freecooling:Electricity",
				"endUseSub:Freecooling:Electricity",
				"LoopToLoop:Electricity",
				"endUseSub:LoopToLoop:Electricity",
				"Chillers:Electricity",
				"endUseSub:Chillers:Electricity",
				"Boilers:Electricity",
				"endUseSub:Boilers:Electricity",
				"Baseboard:Electricity",
				"endUseSub:Baseboard:Electricity",
				"HeatRejection:Electricity",
				"endUseSub:HeatRejection:Electricity",
				"Humidifier:Electricity",
				"endUseSub:Humidifier:Electricity",
				"HeatRecovery:Electricity",
				"endUseSub:HeatRecovery:Electricity",
				"Photovoltaic:Electricity",
				"endUseSub:Photovoltaic:Electricity",
				"WindTurbine:Electricity",
				"endUseSub:WindTurbine:Electricity",
				"ElectricStorage:Electricity",
				"endUseSub:ElectricStorage:Electricity",
				"HeatRecoveryForCooling:Electricity",
				"endUseSub:HeatRecoveryForCooling:Electricity",
				"HeatRecoveryForHeating:Electricity",
				"endUseSub:HeatRecoveryForHeating:Electricity",
				"ElectricEmissions:Electricity",
				"endUseSub:ElectricEmissions:Electricity",
				"PurchasedElectricEmissions:Electricity",
				"endUseSub:PurchasedElectricEmissions:Electricity",
				"SoldElectricEmissions:Electricity",
				"endUseSub:SoldElectricEmissions:Electricity",
				"NaturalGasEmissions:Electricity",
				"endUseSub:NaturalGasEmissions:Electricity",
				"FuelOil#1Emissions:Electricity",
				"endUseSub:FuelOil#1Emissions:Electricity",
				"FuelOil#2Emissions:Electricity",
				"endUseSub:FuelOil#2Emissions:Electricity",
				"CoalEmissions:Electricity",
				"endUseSub:CoalEmissions:Electricity",
				"GasolineEmissions:Electricity",
				"endUseSub:GasolineEmissions:Electricity",
				"PropaneEmissions:Electricity",
				"endUseSub:PropaneEmissions:Electricity",
				"DieselEmissions:Electricity",
				"endUseSub:DieselEmissions:Electricity",
				"OtherFuel1Emissions:Electricity",
				"endUseSub:OtherFuel1Emissions:Electricity",
				"OtherFuel2Emissions:Electricity",
				"endUseSub:OtherFuel2Emissions:Electricity",
				"CarbonEquivalentEmissions:Electricity",
				"endUseSub:CarbonEquivalentEmissions:Electricity",
				"Refrigeration:Electricity",
				"endUseSub:Refrigeration:Electricity",
				"ColdStorageCharge:Electricity",
				"endUseSub:ColdStorageCharge:Electricity",
				"ColdStorageDischarge:Electricity",
				"endUseSub:ColdStorageDischarge:Electricity",
				"Rainwater:Electricity",
				"endUseSub:Rainwater:Electricity",
				"Condensate:Electricity",
				"endUseSub:Condensate:Electricity",
				"Wellwater:Electricity",
				"endUseSub:Wellwater:Electricity",
				"MainsWater:Electricity",
				"endUseSub:MainsWater:Electricity"
			};

			bool errorFound = false;
			for ( auto & meter : input_map ) {
				errorFound = false;
				if ( meter.size() == 5 ) {
					ValidateNStandardizeMeterTitles( meter[ 0 ], meter[ 1 ], meter[ 2 ], meter[ 3 ], meter[ 4 ], errorFound );
				} else if ( meter.size() == 6 ) {
					ValidateNStandardizeMeterTitles( meter[ 0 ], meter[ 1 ], meter[ 2 ], meter[ 3 ], meter[ 4 ], errorFound, meter[ 5 ] );
				}
				EXPECT_FALSE( errorFound );
			}

			ASSERT_EQ( 102, NumEnergyMeters );
			ASSERT_EQ( 102ul, EnergyMeters.size() );

			for (int i = 0; i < NumEnergyMeters; ++i)
			{
				EXPECT_EQ( result_map[ i ], EnergyMeters( i + 1 ).Name );
			}

			sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

			EnergyPlus::sqlite = std::move( sqlite_test );

			std::string units = "J";
			std::string resourceType = "ELEC";
			std::string endUse = "INTERIOR LIGHTS";
			std::string endUseSub = "endUseSub";
			std::string group = "BAD INPUT";
			errorFound = false;

			ValidateNStandardizeMeterTitles( units, resourceType, endUse, endUseSub, group, errorFound );
			EXPECT_TRUE( errorFound );

			units = "J";
			resourceType = "ELEC";
			endUse = "BAD INPUT";
			endUseSub = "endUseSub";
			group = "HVAC";
			errorFound = false;

			ValidateNStandardizeMeterTitles( units, resourceType, endUse, endUseSub, group, errorFound );
			EXPECT_TRUE( errorFound );

			sqlite_test = std::move( EnergyPlus::sqlite );

			auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

			ASSERT_EQ(2ul, errorData.size());
			std::vector<std::string> errorData0 {"1", "1", "1", "Illegal Group (for Meters) Entered=BAD INPUT", "1"};
			std::vector<std::string> errorData1 {"2", "1", "1", "Illegal EndUse (for Meters) Entered=BAD INPUT", "1"};
			EXPECT_EQ(errorData0, errorData[0]);
			EXPECT_EQ(errorData1, errorData[1]);

		}

		TEST_F( SQLiteFixture, OutputProcessor_setupTimePointers )
		{
			TimeValue.allocate( 2 );

			auto timeStep = 1.0;

			SetupTimePointers( "Zone", timeStep );

			EXPECT_DOUBLE_EQ( timeStep, TimeValue( 1 ).TimeStep );
			EXPECT_DOUBLE_EQ( 0.0, TimeValue( 1 ).CurMinute );

			timeStep = 2.0;

			SetupTimePointers( "HVAC", timeStep );

			EXPECT_DOUBLE_EQ( timeStep, TimeValue( 2 ).TimeStep );
			EXPECT_DOUBLE_EQ( 0.0, TimeValue( 2 ).CurMinute );

		}

		TEST_F( SQLiteFixture, OutputProcessor_getVariableUnitsString )
		{
			EXPECT_EQ( "C", GetVariableUnitsString( "Site Outdoor Air Drybulb Temperature [C]" ) );
			EXPECT_EQ( "%", GetVariableUnitsString( "Site Outdoor Air Relative Humidity [%]" ) );
			EXPECT_EQ( "kgWater/kgDryAir", GetVariableUnitsString( "Site Outdoor Air Humidity Ratio [kgWater/kgDryAir]" ) );
			EXPECT_EQ( "", GetVariableUnitsString( "Site Daylighting Model Sky Clearness []" ) );
			EXPECT_EQ( "", GetVariableUnitsString( "Site Outdoor Air Drybulb Temperature" ) );
			EXPECT_EQ( "0123456789012345", GetVariableUnitsString( "Site Outdoor Air Drybulb Temperature [0123456789012345]" ) );

		}

		TEST_F( SQLiteFixture, OutputProcessor_DeathTest_getVariableUnitsString )
		{
			EXPECT_ANY_THROW( GetVariableUnitsString( "Site Outdoor Air Drybulb Temperature [C" ) );
			EXPECT_ANY_THROW( GetVariableUnitsString( "Site Outdoor Air Drybulb Temperature ]C[" ) );
			EXPECT_ANY_THROW( GetVariableUnitsString( "Site Outdoor Air Drybulb Temperature [01234567890123456]" ) );

		}

		TEST_F( SQLiteFixture, OutputProcessor_getReportVariableInput )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			GetReportVariableInput();

			NumOfReqVariables = InputProcessor::GetNumObjectsFound( "Output:Variable" );

			EXPECT_EQ( 5, NumOfReqVariables );

			EXPECT_EQ( "", ReqRepVars( 1 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 1 ).VarName );
			EXPECT_EQ( ReportTimeStep, ReqRepVars( 1 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 1 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 1 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 1 ).Used );

			EXPECT_EQ( "", ReqRepVars( 2 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 2 ).VarName );
			EXPECT_EQ( ReportHourly, ReqRepVars( 2 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 2 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 2 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 2 ).Used );

			EXPECT_EQ( "", ReqRepVars( 3 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 3 ).VarName );
			EXPECT_EQ( ReportDaily, ReqRepVars( 3 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 3 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 3 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 3 ).Used );

			EXPECT_EQ( "", ReqRepVars( 4 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 4 ).VarName );
			EXPECT_EQ( ReportMonthly, ReqRepVars( 4 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 4 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 4 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 4 ).Used );

			EXPECT_EQ( "", ReqRepVars( 5 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 5 ).VarName );
			EXPECT_EQ( ReportSim, ReqRepVars( 5 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 5 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 5 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 5 ).Used );

		}

		TEST_F( SQLiteFixture, OutputProcessor_buildKeyVarList )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			GetReportVariableInput();

			auto const keyed_value = "ENVIRONMENT";
			auto const var_name = "SITE OUTDOOR AIR DRYBULB TEMPERATURE";

			BuildKeyVarList( keyed_value, var_name, 1, 6 );

			EXPECT_EQ( 0, NumExtraVars );
			EXPECT_EQ( 6, NumOfReqVariables );

			EXPECT_EQ( "", ReqRepVars( 1 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 1 ).VarName );
			EXPECT_EQ( ReportTimeStep, ReqRepVars( 1 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 1 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 1 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 1 ).Used );

			EXPECT_EQ( "", ReqRepVars( 2 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 2 ).VarName );
			EXPECT_EQ( ReportHourly, ReqRepVars( 2 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 2 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 2 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 2 ).Used );

			EXPECT_EQ( "", ReqRepVars( 3 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 3 ).VarName );
			EXPECT_EQ( ReportDaily, ReqRepVars( 3 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 3 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 3 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 3 ).Used );

			EXPECT_EQ( "", ReqRepVars( 4 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 4 ).VarName );
			EXPECT_EQ( ReportMonthly, ReqRepVars( 4 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 4 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 4 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 4 ).Used );

			EXPECT_EQ( "", ReqRepVars( 5 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 5 ).VarName );
			EXPECT_EQ( ReportSim, ReqRepVars( 5 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 5 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 5 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 5 ).Used );
		}

		TEST_F( SQLiteFixture, OutputProcessor_addBlankKeys )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			InitializeOutput();

			GetReportVariableInput();

			auto const var_name = "Site Outdoor Air Drybulb Temperature";

			AddBlankKeys( var_name, 1, 5 );

			EXPECT_EQ( 5, NumExtraVars );
			EXPECT_EQ( 1, ReportList( 1 ) );
			EXPECT_EQ( 2, ReportList( 2 ) );
			EXPECT_EQ( 3, ReportList( 3 ) );
			EXPECT_EQ( 4, ReportList( 4 ) );
			EXPECT_EQ( 5, ReportList( 5 ) );
			EXPECT_EQ( 5, NumOfReqVariables );

			EXPECT_EQ( "", ReqRepVars( 1 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 1 ).VarName );
			EXPECT_EQ( ReportTimeStep, ReqRepVars( 1 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 1 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 1 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 1 ).Used );

			EXPECT_EQ( "", ReqRepVars( 2 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 2 ).VarName );
			EXPECT_EQ( ReportHourly, ReqRepVars( 2 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 2 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 2 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 2 ).Used );

			EXPECT_EQ( "", ReqRepVars( 3 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 3 ).VarName );
			EXPECT_EQ( ReportDaily, ReqRepVars( 3 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 3 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 3 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 3 ).Used );

			EXPECT_EQ( "", ReqRepVars( 4 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 4 ).VarName );
			EXPECT_EQ( ReportMonthly, ReqRepVars( 4 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 4 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 4 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 4 ).Used );

			EXPECT_EQ( "", ReqRepVars( 5 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 5 ).VarName );
			EXPECT_EQ( ReportSim, ReqRepVars( 5 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 5 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 5 ).SchedName );
			EXPECT_FALSE( ReqRepVars( 5 ).Used );

		}

		TEST_F( SQLiteFixture, OutputProcessor_determineFrequency )
		{
			auto const valid_options = std::map< std::string, int >({
				{ "Detailed", -1 },
				{ "Timestep", 0 },
				{ "Hourly", 1 },
				{ "Daily", 2 },
				{ "Monthly", 3 },
				{ "RunPeriod", 4 },
				{ "Environment", 4 },
				{ "Annual", 4 },
				{ "Bad Input", 1 }
			});

			int report_freq = -2;

			for ( auto const option : valid_options ) {
				DetermineFrequency( option.first, report_freq );
				EXPECT_EQ( option.second, report_freq );
			}

		}

		TEST_F( SQLiteFixture, OutputProcessor_addToOutputVariableList )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Wetbulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Humidity Ratio,hourly;",
				"Output:Variable,*,Site Outdoor Air Relative Humidity,hourly;",
				"Output:Variable,*,Zone Mean Air Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			AddToOutputVariableList( "Site Outdoor Air Drybulb Temperature", 1, 1, 2, "C" );
			AddToOutputVariableList( "Site Outdoor Air Wetbulb Temperature", 1, 1, 2, "C" );
			AddToOutputVariableList( "Site Outdoor Air Humidity Ratio", 1, 1, 2, "kgWater/kgDryAir" );
			AddToOutputVariableList( "Site Outdoor Air Relative Humidity", 1, 1, 2, "%" );

			EXPECT_EQ( 1, DDVariableTypes( 1 ).IndexType );
			EXPECT_EQ( 1, DDVariableTypes( 1 ).StoreType );
			EXPECT_EQ( 2, DDVariableTypes( 1 ).VariableType );
			EXPECT_EQ( 0, DDVariableTypes( 1 ).Next );
			EXPECT_FALSE( DDVariableTypes( 1 ).ReportedOnDDFile );
			EXPECT_EQ( "Site Outdoor Air Drybulb Temperature", DDVariableTypes( 1 ).VarNameOnly );
			EXPECT_EQ( "C", DDVariableTypes( 1 ).UnitsString );

			EXPECT_EQ( 1, DDVariableTypes( 2 ).IndexType );
			EXPECT_EQ( 1, DDVariableTypes( 2 ).StoreType );
			EXPECT_EQ( 2, DDVariableTypes( 2 ).VariableType );
			EXPECT_EQ( 0, DDVariableTypes( 2 ).Next );
			EXPECT_FALSE( DDVariableTypes( 2 ).ReportedOnDDFile );
			EXPECT_EQ( "Site Outdoor Air Wetbulb Temperature", DDVariableTypes( 2 ).VarNameOnly );
			EXPECT_EQ( "C", DDVariableTypes( 2 ).UnitsString );

			EXPECT_EQ( 1, DDVariableTypes( 3 ).IndexType );
			EXPECT_EQ( 1, DDVariableTypes( 3 ).StoreType );
			EXPECT_EQ( 2, DDVariableTypes( 3 ).VariableType );
			EXPECT_EQ( 0, DDVariableTypes( 3 ).Next );
			EXPECT_FALSE( DDVariableTypes( 3 ).ReportedOnDDFile );
			EXPECT_EQ( "Site Outdoor Air Humidity Ratio", DDVariableTypes( 3 ).VarNameOnly );
			EXPECT_EQ( "kgWater/kgDryAir", DDVariableTypes( 3 ).UnitsString );

			EXPECT_EQ( 1, DDVariableTypes( 4 ).IndexType );
			EXPECT_EQ( 1, DDVariableTypes( 4 ).StoreType );
			EXPECT_EQ( 2, DDVariableTypes( 4 ).VariableType );
			EXPECT_EQ( 0, DDVariableTypes( 4 ).Next );
			EXPECT_FALSE( DDVariableTypes( 4 ).ReportedOnDDFile );
			EXPECT_EQ( "Site Outdoor Air Relative Humidity", DDVariableTypes( 4 ).VarNameOnly );
			EXPECT_EQ( "%", DDVariableTypes( 4 ).UnitsString );

		}

		TEST_F( SQLiteFixture, OutputProcessor_setupOutputVariable )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			EnergyPlus::sqlite = std::move( sqlite_test );
			GetReportVariableInput();
			SetupOutputVariable( "Site Outdoor Air Drybulb Temperature [C]", DataEnvironment::OutDryBulbTemp, "Zone", "Average", "Environment" );
			sqlite_test = std::move( EnergyPlus::sqlite );

			auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

			std::vector< std::vector<std::string> > reportDataDictionary(
			{
				{ "1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C" }
			});
			EXPECT_EQ( reportDataDictionary, reportDataDictionaryResults );

			EXPECT_EQ( 1, NumExtraVars );

			EXPECT_EQ( "", ReqRepVars( 1 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 1 ).VarName );
			EXPECT_EQ( ReportSim, ReqRepVars( 1 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 1 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 1 ).SchedName );
			EXPECT_EQ( true, ReqRepVars( 1 ).Used );

			EXPECT_EQ( 1, DDVariableTypes( 1 ).IndexType );
			EXPECT_EQ( 1, DDVariableTypes( 1 ).StoreType );
			EXPECT_EQ( 2, DDVariableTypes( 1 ).VariableType );
			EXPECT_EQ( 0, DDVariableTypes( 1 ).Next );
			EXPECT_FALSE( DDVariableTypes( 1 ).ReportedOnDDFile );
			EXPECT_EQ( "Site Outdoor Air Drybulb Temperature", DDVariableTypes( 1 ).VarNameOnly );

		}

		TEST_F( SQLiteFixture, OutputProcessor_checkReportVariable )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			auto const keyed_value = "Environment";
			auto const var_name = "Site Outdoor Air Drybulb Temperature";

			InitializeOutput();

			GetReportVariableInput();
			CheckReportVariable( keyed_value, var_name );

			EXPECT_EQ( 5, NumOfReqVariables );

			EXPECT_EQ( "", ReqRepVars( 1 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 1 ).VarName );
			EXPECT_EQ( ReportTimeStep, ReqRepVars( 1 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 1 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 1 ).SchedName );
			EXPECT_EQ( true, ReqRepVars( 1 ).Used );

			EXPECT_EQ( "", ReqRepVars( 2 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 2 ).VarName );
			EXPECT_EQ( ReportHourly, ReqRepVars( 2 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 2 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 2 ).SchedName );
			EXPECT_EQ( true, ReqRepVars( 2 ).Used );

			EXPECT_EQ( "", ReqRepVars( 3 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 3 ).VarName );
			EXPECT_EQ( ReportDaily, ReqRepVars( 3 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 3 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 3 ).SchedName );
			EXPECT_EQ( true, ReqRepVars( 3 ).Used );

			EXPECT_EQ( "", ReqRepVars( 4 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 4 ).VarName );
			EXPECT_EQ( ReportMonthly, ReqRepVars( 4 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 4 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 4 ).SchedName );
			EXPECT_EQ( true, ReqRepVars( 4 ).Used );

			EXPECT_EQ( "", ReqRepVars( 5 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars( 5 ).VarName );
			EXPECT_EQ( ReportSim, ReqRepVars( 5 ).ReportFreq );
			EXPECT_EQ( 0, ReqRepVars( 5 ).SchedPtr );
			EXPECT_EQ( "", ReqRepVars( 5 ).SchedName );
			EXPECT_EQ( true, ReqRepVars( 5 ).Used );
		}

		TEST_F( SQLiteFixture, OutputProcessor_getCustomMeterInput )
		{
			std::string const idf_objects = delimited_string({
				"  Output:Meter:MeterFileOnly,MyGeneralLights,monthly;",
				"  Output:Meter:MeterFileOnly,MyGeneralLights,runperiod;",
				"  Output:Meter:MeterFileOnly,MyBuildingOther,monthly;",
				"  Output:Meter:MeterFileOnly,MyBuildingOther,runperiod;",
				"  Output:Meter:MeterFileOnly,Building Infiltration Heat Loss,monthly;",
				"  Output:Meter:MeterFileOnly,Building Infiltration Heat Loss,runperiod;",
				"  Meter:Custom,",
				"    MyGeneralLights,         !- Name",
				"    Electricity,             !- Fuel Type",
				"    SPACE1-1 Lights 1,       !- Key Name 1",
				"    Lights Electric Energy,  !- Output Variable or Meter Name 1",
				"    SPACE2-1 Lights 1,       !- Key Name 2",
				"    Lights Electric Energy,  !- Output Variable or Meter Name 2",
				"    SPACE3-1 Lights 1,       !- Key Name 3",
				"    Lights Electric Energy,  !- Output Variable or Meter Name 3",
				"    SPACE4-1 Lights 1,       !- Key Name 4",
				"    Lights Electric Energy,  !- Output Variable or Meter Name 4",
				"    SPACE5-1 Lights 1,       !- Key Name 5",
				"    Lights Electric Energy;  !- Output Variable or Meter Name 5",
				"  Meter:Custom,",
				"    Building Infiltration Heat Loss,  !- Name",
				"    Generic,                 !- Fuel Type",
				"    *,                       !- Key Name 1",
				"    Zone Infiltration Total Heat Loss Energy;  !- Output Variable or Meter Name 1",
				"  Meter:CustomDecrement,",
				"    MyBuildingOther,         !- Name",
				"    Electricity,             !- Fuel Type",
				"    Electricity:Building,    !- Source Meter Name",
				"    ,                        !- Key Name 1",
				"    MyGeneralLights;         !- Output Variable or Meter Name 1",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			Real64 light_consumption = 0;
			SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE1-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE1-1", 1, 1 );
			SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE2-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE2-1", 1, 1 );
			SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE3-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE3-1", 1, 1 );
			SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE4-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE4-1", 1, 1 );
			SetupOutputVariable( "Lights Electric Energy [J]", light_consumption, "Zone", "Sum", "SPACE5-1 LIGHTS 1", _, "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE5-1", 1, 1 );
			Real64 zone_infil_total_loss = 0;
			SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE1-1" );
			SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE2-1" );
			SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE3-1" );
			SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE4-1" );
			SetupOutputVariable( "Zone Infiltration Total Heat Loss Energy [J]", zone_infil_total_loss, "System", "Sum", "SPACE5-1" );

			bool errors_found = false;

			GetCustomMeterInput( errors_found );

			ASSERT_FALSE( errors_found );

			ASSERT_EQ( 17, NumEnergyMeters );

			auto const meters_result = std::map< int, std::tuple < int, std::string, std::string, std::string, std::string, std::string, std::string > >({
				{ 1, std::make_tuple( 0, "Electricity:Facility", "Electricity", "", "", "", "J" ) },
				{ 2, std::make_tuple( 0, "Electricity:Building", "Electricity", "", "", "Building", "J" ) },
				{ 3, std::make_tuple( 0, "Electricity:Zone:SPACE1-1", "Electricity", "", "", "Zone", "J" ) },
				{ 4, std::make_tuple( 0, "InteriorLights:Electricity", "Electricity", "InteriorLights", "", "", "J" ) },
				{ 5, std::make_tuple( 0, "InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "", "Zone", "J" ) },
				{ 6, std::make_tuple( 0, "GeneralLights:InteriorLights:Electricity", "Electricity", "InteriorLights", "GeneralLights", "", "J" ) },
				{ 7, std::make_tuple( 0, "Electricity:Zone:SPACE2-1", "Electricity", "", "", "Zone", "J" ) },
				{ 8, std::make_tuple( 0, "InteriorLights:Electricity:Zone:SPACE2-1", "Electricity", "InteriorLights", "", "Zone", "J" ) },
				{ 9, std::make_tuple( 0, "Electricity:Zone:SPACE3-1", "Electricity", "", "", "Zone", "J" ) },
				{ 10, std::make_tuple( 0, "InteriorLights:Electricity:Zone:SPACE3-1", "Electricity", "InteriorLights", "", "Zone", "J" ) },
				{ 11, std::make_tuple( 0, "Electricity:Zone:SPACE4-1", "Electricity", "", "", "Zone", "J" ) },
				{ 12, std::make_tuple( 0, "InteriorLights:Electricity:Zone:SPACE4-1", "Electricity", "InteriorLights", "", "Zone", "J" ) },
				{ 13, std::make_tuple( 0, "Electricity:Zone:SPACE5-1", "Electricity", "", "", "Zone", "J" ) },
				{ 14, std::make_tuple( 0, "InteriorLights:Electricity:Zone:SPACE5-1", "Electricity", "InteriorLights", "", "Zone", "J" ) },
				{ 15, std::make_tuple( 1, "MYGENERALLIGHTS", "Electricity", "", "", "", "J" ) },
				{ 16, std::make_tuple( 1, "BUILDING INFILTRATION HEAT LOSS", "Generic", "", "", "", "J" ) },
				{ 17, std::make_tuple( 2, "MYBUILDINGOTHER", "Electricity", "", "", "", "J" ) },
			});

			for ( auto const result : meters_result ) {
				EXPECT_EQ( std::get<0>( result.second ), EnergyMeters( result.first ).TypeOfMeter );
				EXPECT_EQ( std::get<1>( result.second ), EnergyMeters( result.first ).Name );
				EXPECT_EQ( std::get<2>( result.second ), EnergyMeters( result.first ).ResourceType );
				EXPECT_EQ( std::get<3>( result.second ), EnergyMeters( result.first ).EndUse );
				EXPECT_EQ( std::get<4>( result.second ), EnergyMeters( result.first ).EndUseSub );
				EXPECT_EQ( std::get<5>( result.second ), EnergyMeters( result.first ).Group );
				EXPECT_EQ( std::get<6>( result.second ), EnergyMeters( result.first ).Units );
			}
		}

		TEST_F( SQLiteFixture, OutputProcessor_attachMeters )
		{
			std::string const idf_objects = delimited_string({
				"Output:Meter,Electricity:Facility,timestep;",
				"Output:Meter,Electricity:Facility,hourly;",
				"Output:Meter,Electricity:Facility,daily;",
				"Output:Meter,Electricity:Facility,monthly;",
				"Output:Meter,Electricity:Facility,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			InitializeOutput();

			int meter_array_ptr = -1;
			bool errors_found = false;

			std::string resourceType( "Electricity" );
			std::string endUse( "InteriorLights" );
			std::string endUseSub( "GeneralLights" );
			std::string group( "Building" );
			std::string const zoneName( "SPACE1-1" );

			AttachMeters( "J", resourceType, endUse, endUseSub, group, zoneName, 1, meter_array_ptr, errors_found );

			EXPECT_FALSE( errors_found );
			EXPECT_EQ( 1, meter_array_ptr );

			ASSERT_EQ( 6, NumEnergyMeters );

			auto const meters_result = std::map< int, std::tuple < int, std::string, std::string, std::string, std::string, std::string, std::string > >({
				{ 1, std::make_tuple( 0, "Electricity:Facility", "Electricity", "", "", "", "J" ) },
				{ 2, std::make_tuple( 0, "Electricity:Building", "Electricity", "", "", "Building", "J" ) },
				{ 3, std::make_tuple( 0, "Electricity:Zone:SPACE1-1", "Electricity", "", "", "Zone", "J" ) },
				{ 4, std::make_tuple( 0, "InteriorLights:Electricity", "Electricity", "InteriorLights", "", "", "J" ) },
				{ 5, std::make_tuple( 0, "InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "", "Zone", "J" ) },
				{ 6, std::make_tuple( 0, "GeneralLights:InteriorLights:Electricity", "Electricity", "InteriorLights", "GeneralLights", "", "J" ) },
			});

			for ( auto const result : meters_result ) {
				EXPECT_EQ( std::get<0>( result.second ), EnergyMeters( result.first ).TypeOfMeter );
				EXPECT_EQ( std::get<1>( result.second ), EnergyMeters( result.first ).Name );
				EXPECT_EQ( std::get<2>( result.second ), EnergyMeters( result.first ).ResourceType );
				EXPECT_EQ( std::get<3>( result.second ), EnergyMeters( result.first ).EndUse );
				EXPECT_EQ( std::get<4>( result.second ), EnergyMeters( result.first ).EndUseSub );
				EXPECT_EQ( std::get<5>( result.second ), EnergyMeters( result.first ).Group );
				EXPECT_EQ( std::get<6>( result.second ), EnergyMeters( result.first ).Units );
			}
		}

		TEST_F( SQLiteFixture, OutputProcessor_updateDataandReport_ZoneTSReporting )
		{
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

			ASSERT_FALSE( process_idf( idf_objects ) );

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
				"7,4995.",
				",365,12,31, 0,24, 0.00,60.00,Tuesday",
				"2,0.0",
				"8,4995.",
				",365,12,31, 0,Tuesday",
				"3,0.0,0.0,24,60,0.0,24,60",
				"9,4995.,4995.,24,60,4995.,24,60",
				",365,12",
				"4,0.0,0.0,31,24,60,0.0,31,24,60",
				"10,4995.,4995.,31,24,60,4995.,31,24,60",
				",365",
				"5,0.0,0.0,12,31,24,60,0.0,12,31,24,60",
				"11,4995.,4995.,12,31,24,60,4995.,12,31,24,60",
			} ) );

			compare_mtr_stream( delimited_string( {
				"7,1,Electricity:Facility [J] !TimeStep",
				"8,1,Electricity:Facility [J] !Hourly",
				"9,7,Electricity:Facility [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]",
				"10,9,Electricity:Facility [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				"11,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				",365,12,31, 0,24,50.00,60.00,Tuesday",
				"7,4995.",
				",365,12,31, 0,24, 0.00,60.00,Tuesday",
				"8,4995.",
				",365,12,31, 0,Tuesday",
				"9,4995.,4995.,24,60,4995.,24,60",
				",365,12",
				"10,4995.,4995.,31,24,60,4995.,31,24,60",
				",365",
				"11,4995.,4995.,12,31,24,60,4995.,12,31,24,60",
			} ) );
		}

		TEST_F( SQLiteFixture, OutputProcessor_updateDataandReport_ZoneTSReporting_with_detailed )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,detailed;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
				"Output:Variable,*,Boiler Gas Rate,detailed;",
				"Output:Variable,*,Boiler Heating Rate,detailed;",
				"Output:Meter,Electricity:Facility,detailed;",
				"Output:Meter,Electricity:Facility,timestep;",
				"Output:Meter,Electricity:Facility,hourly;",
				"Output:Meter,Electricity:Facility,daily;",
				"Output:Meter,Electricity:Facility,monthly;",
				"Output:Meter,Electricity:Facility,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

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
			Real64 fuel_used = 999;
			Real64 boiler_load = 999;
			SetupOutputVariable( "Boiler Heating Rate [W]", boiler_load, "System", "Average", "Boiler1" );
			SetupOutputVariable( "Boiler Gas Rate [W]", fuel_used, "System", "Average", "Boiler1" );

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
				{ "1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "HVAC System Timestep", "", "C" },
				{ "2", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C" },
				{ "3", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C" },
				{ "4", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C" },
				{ "5", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C" },
				{ "6", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C" },
				{ "8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "HVAC System Timestep", "", "J" },
				{ "9", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Hourly", "", "J" },
				{ "10", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Daily", "", "J" },
				{ "11", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Monthly", "", "J" },
				{ "12", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Run Period", "", "J" },
				{ "152", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Heating Rate", "HVAC System Timestep", "", "W" },
				{ "153", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Gas Rate", "HVAC System Timestep", "", "W" },
			});

			EXPECT_EQ( reportDataDictionary, reportDataDictionaryResults );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData({
				{ "1", "1", "1", "0.0" },
				{ "2", "1", "2", "0.0" },
				{ "3", "1", "8", "4995.0" },
				{ "4", "2", "3", "0.0" },
				{ "5", "2", "9", "4995.0" },
				{ "6", "3", "4", "0.0" },
				{ "7", "3", "10", "4995.0" },
				{ "8", "4", "5", "0.0" },
				{ "9", "4", "11", "4995.0" },
				{ "10", "5", "6", "0.0" },
				{ "11", "5", "12", "4995.0" },
			});

			std::vector< std::vector<std::string> > reportExtendedData({
				{ "1", "6", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0" },
				{ "2", "7", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0" },
				{ "3", "8", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0" },
				{ "4", "9", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0" },
				{ "5", "10", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0" },
				{ "6", "11", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0" },
			});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

			compare_eso_stream( delimited_string( {
				"1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Each Call",
				"2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
				"3,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
				"4,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
				"5,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				"6,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				"152,1,Boiler1,Boiler Heating Rate [W] !Each Call",
				"153,1,Boiler1,Boiler Gas Rate [W] !Each Call",
				"8,1,Electricity:Facility [J] !Each Call",
				"9,1,Electricity:Facility [J] !Hourly",
				"10,7,Electricity:Facility [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]",
				"11,9,Electricity:Facility [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				"12,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				",365,12,31, 0,24,50.00,60.00,Tuesday",
				"1,0.0",
				"2,0.0",
				"8,4995.",
				",365,12,31, 0,24, 0.00,60.00,Tuesday",
				"3,0.0",
				"9,4995.",
				",365,12,31, 0,Tuesday",
				"4,0.0,0.0,24,60,0.0,24,60",
				"10,4995.,4995.,24,60,4995.,24,60",
				",365,12",
				"5,0.0,0.0,31,24,60,0.0,31,24,60",
				"11,4995.,4995.,31,24,60,4995.,31,24,60",
				",365",
				"6,0.0,0.0,12,31,24,60,0.0,12,31,24,60",
				"12,4995.,4995.,12,31,24,60,4995.,12,31,24,60",
			} ) );

			compare_mtr_stream( delimited_string( {
				"8,1,Electricity:Facility [J] !Each Call",
				"9,1,Electricity:Facility [J] !Hourly",
				"10,7,Electricity:Facility [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]",
				"11,9,Electricity:Facility [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				"12,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				",365,12,31, 0,24,50.00,60.00,Tuesday",
				"8,4995.",
				",365,12,31, 0,24, 0.00,60.00,Tuesday",
				"9,4995.",
				",365,12,31, 0,Tuesday",
				"10,4995.,4995.,24,60,4995.,24,60",
				",365,12",
				"11,4995.,4995.,31,24,60,4995.,31,24,60",
				",365",
				"12,4995.,4995.,12,31,24,60,4995.,12,31,24,60",
			} ) );

		}

		TEST_F( SQLiteFixture, OutputProcessor_updateDataandReport_HVACTSReporting )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,detailed;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
				"Output:Variable,*,Boiler Gas Rate,detailed;",
				"Output:Variable,*,Boiler Heating Rate,detailed;",
				"Output:Meter,Electricity:Facility,detailed;",
				"Output:Meter,Electricity:Facility,timestep;",
				"Output:Meter,Electricity:Facility,hourly;",
				"Output:Meter,Electricity:Facility,daily;",
				"Output:Meter,Electricity:Facility,monthly;",
				"Output:Meter,Electricity:Facility,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

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
			Real64 fuel_used = 999;
			Real64 boiler_load = 999;
			SetupOutputVariable( "Boiler Heating Rate [W]", boiler_load, "System", "Average", "Boiler1" );
			SetupOutputVariable( "Boiler Gas Rate [W]", fuel_used, "System", "Average", "Boiler1" );

			UpdateMeterReporting();

			UpdateDataandReport( DataGlobals::HVACTSReporting );

			sqlite_test = std::move( EnergyPlus::sqlite );

			auto timeResults = queryResult("SELECT * FROM Time;", "Time");

			std::vector< std::vector<std::string> > timeData({
				{ "1", "12", "31", "24", "0", "0", "10", "-1", "365", "Tuesday", "0", "0" },
			});

			EXPECT_EQ( timeData, timeResults );

			auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

			std::vector< std::vector<std::string> > reportDataDictionary({
				{ "1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "HVAC System Timestep", "", "C" },
				{ "2", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C" },
				{ "3", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C" },
				{ "4", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C" },
				{ "5", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C" },
				{ "6", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C" },
				{ "8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "HVAC System Timestep", "", "J" },
				{ "9", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Hourly", "", "J" },
				{ "10", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Daily", "", "J" },
				{ "11", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Monthly", "", "J" },
				{ "12", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Run Period", "", "J" },
				{ "152", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Heating Rate", "HVAC System Timestep", "", "W" },
				{ "153", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Gas Rate", "HVAC System Timestep", "", "W" },
			});

			EXPECT_EQ( reportDataDictionary, reportDataDictionaryResults );

			auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
			auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

			std::vector< std::vector<std::string> > reportData({
				{ "1", "1", "152", "999.0" },
				{ "2", "1", "153", "999.0" },
			});

			std::vector< std::vector<std::string> > reportExtendedData({});

			EXPECT_EQ( reportData, reportDataResults );
			EXPECT_EQ( reportExtendedData, reportExtendedDataResults );

			compare_eso_stream( delimited_string( {
				"1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Each Call",
				"2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
				"3,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
				"4,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
				"5,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				"6,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				"152,1,Boiler1,Boiler Heating Rate [W] !Each Call",
				"153,1,Boiler1,Boiler Gas Rate [W] !Each Call",
				"8,1,Electricity:Facility [J] !Each Call",
				"9,1,Electricity:Facility [J] !Hourly",
				"10,7,Electricity:Facility [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]",
				"11,9,Electricity:Facility [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				"12,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				",365,12,31, 0,24,50.00,60.00,Tuesday",
				"152,999.",
				"153,999.",
			} ) );

			compare_mtr_stream( delimited_string( {
				"8,1,Electricity:Facility [J] !Each Call",
				"9,1,Electricity:Facility [J] !Hourly",
				"10,7,Electricity:Facility [J] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]",
				"11,9,Electricity:Facility [J] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
				"12,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
			} ) );

		}

		TEST_F( EnergyPlusFixture, OutputProcessor_ResetAccumulationWhenWarmupComplete )
		{
			std::string const idf_objects = delimited_string( {
				"Version,8.3;",
				"Output:Variable,*,Zone Ideal Loads Supply Air Total Heating Energy,detailed;",
				"Output:Meter:MeterFileOnly,DistrictHeating:HVAC,detailed;",
				"Output:Variable,*,Zone Ideal Loads Supply Air Total Heating Energy,runperiod;",
				"Output:Meter:MeterFileOnly,DistrictHeating:HVAC,r;",
			} );

			ASSERT_FALSE( process_idf( idf_objects ) );

			// Setup so that UpdateDataandReport can be called.
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
					if ( ( !DataGlobals::WarmupFlag ) && ( DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn ) ) {
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

			TimeValue( 1 ).CurMinute = 10;
			TimeValue( 2 ).CurMinute = 10;

			DataGlobals::WarmupFlag = true;

			ReportOutputFileHeaders();

			GetReportVariableInput();
			Array1D< ZonePurchasedAir > PurchAir; // Used to specify purchased air parameters
			PurchAir.allocate( 1 );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Total Heating Energy [J]", PurchAir( 1 ).TotHeatEnergy, "System", "Sum", PurchAir( 1 ).Name, _, "DISTRICTHEATING", "Heating", _, "System" );

			PurchAir( 1 ).TotHeatEnergy = 1.1;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			PurchAir( 1 ).TotHeatEnergy = 1.3;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			PurchAir( 1 ).TotHeatEnergy = 1.5;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			PurchAir( 1 ).TotHeatEnergy = 1.7;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			PurchAir( 1 ).TotHeatEnergy = 1.9;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			PurchAir( 1 ).TotHeatEnergy = 2.2;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			DataGlobals::WarmupFlag = false;

			PurchAir( 1 ).TotHeatEnergy = 2.4;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::ZoneTSReporting ); //zone timestep


			compare_eso_stream( delimited_string( {
				"6,1,,Zone Ideal Loads Supply Air Total Heating Energy [J] !Each Call",
				"37,11,,Zone Ideal Loads Supply Air Total Heating Energy [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
				"2,365,12,31, 0,24,10.00,20.00,Tuesday",
				"6,1.1",
				"2,365,12,31, 0,24,20.00,30.00,Tuesday",
				"6,1.3",
				"2,365,12,31, 0,24,30.00,40.00,Tuesday",
				"6,1.5",
				"2,365,12,31, 0,24,40.00,50.00,Tuesday",
				"6,1.7",
				"2,365,12,31, 0,24,50.00,60.00,Tuesday",
				"6,1.9",
				"2,365,12,31, 0,24,60.00,70.00,Tuesday",
				"6,2.2",
				"5,365",
				"37,9.7,1.1,12,31,24,20,2.2,12,31,24,70",
			} ) );


			ResetAccumulationWhenWarmupComplete();

			PurchAir( 1 ).TotHeatEnergy = 100.0;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			PurchAir( 1 ).TotHeatEnergy = 200.0;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::HVACTSReporting );

			PurchAir( 1 ).TotHeatEnergy = 300.0;
			UpdateMeterReporting();
			UpdateDataandReport( DataGlobals::ZoneTSReporting ); //zone timestep

			compare_eso_stream( delimited_string( {
				"2,365,12,31, 0,24, 0.00,10.00,Tuesday",
				"6,100.",
				"2,365,12,31, 0,24,10.00,20.00,Tuesday",
				"6,200.",
				"5,365",
				"37,300.,100.,12,31,24,10,200.,12,31,24,20",
			} ) );

		}

	}

}
