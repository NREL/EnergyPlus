// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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
#include "Fixtures/SQLiteFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/WeatherManager.hh>

#include <map>

using namespace EnergyPlus::PurchasedAirManager;
using namespace EnergyPlus::WeatherManager;
using namespace EnergyPlus::OutputProcessor;

namespace EnergyPlus {

namespace OutputProcessor {

    TEST_F(SQLiteFixture, OutputProcessor_TestGetMeteredVariables)
    {
        int const NumVariables = 2;
        Array1D_int VarIndexes(NumVariables);                     // Variable Numbers
        Array1D_int VarTypes(NumVariables);                       // Variable Types (1=integer, 2=real, 3=meter)
        Array1D_int IndexTypes(NumVariables);                     // Variable Index Types (1=Zone,2=HVAC)
        Array1D<OutputProcessor::Unit> unitsForVar(NumVariables); // units from enum for each variable
        Array1D_int ResourceTypes(NumVariables);                  // ResourceTypes for each variable
        Array1D_string EndUses(NumVariables);                     // EndUses for each variable
        Array1D_string Groups(NumVariables);                      // Groups for each variable
        Array1D_string Names(NumVariables);                       // Variable Names for each variable
        Reference<RealVariables> RVar;

        std::string TypeOfComp = "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW";
        std::string NameOfComp = "FC-5-1B";

        int NumFound;

        GetMeteredVariables(TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);

        EXPECT_EQ(0, NumFound);

        NumOfRVariable = 2;
        RVariableTypes.allocate(NumOfRVariable);
        NameOfComp = "OUTSIDELIGHTS";
        RVar.allocate();

        RVar().MeterArrayPtr = 1;
        RVariableTypes(1).KeyNameOnlyUC = NameOfComp;
        RVariableTypes(1).VarPtr = RVar;
        VarMeterArrays.allocate(1);

        VarMeterArrays(1).NumOnMeters = 1;
        VarMeterArrays(1).OnMeters(1) = 1;

        EnergyMeters.allocate(10);
        EnergyMeters(1).ResourceType = NameOfComp;

        GetMeteredVariables(TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);
        EXPECT_EQ(1, NumFound);
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportTSMeters_PrintESOTimeStamp)
    {
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _);

        NumEnergyMeters = 2;
        EnergyMeters.allocate(NumEnergyMeters);
        EnergyMeters(1).CurTSValue = 999.9;
        EnergyMeters(1).TSValue = 999.9;
        EnergyMeters(1).RptTS = true;
        EnergyMeters(1).RptAccTS = false;
        EnergyMeters(1).RptTSFO = false;
        EnergyMeters(1).RptAccTSFO = false;
        EnergyMeters(1).TSRptNum = 1;
        EnergyMeters(1).TSRptNumChr = "1";
        EnergyMeters(1).TSAccRptNum = 1;
        EnergyMeters(1).SMValue = 999.9;

        EnergyMeters(2).CurTSValue = 9999.9;
        EnergyMeters(2).TSValue = 9999.9;
        EnergyMeters(2).RptTS = true;
        EnergyMeters(2).RptAccTS = false;
        EnergyMeters(2).RptTSFO = false;
        EnergyMeters(2).RptAccTSFO = false;
        EnergyMeters(2).TSRptNum = 2;
        EnergyMeters(2).TSRptNumChr = "2";
        EnergyMeters(2).TSAccRptNum = 2;
        EnergyMeters(2).SMValue = 9999.9;

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

        ReportTSMeters(StartMinute, EndMinute, PrintESOTimeStamp, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportTSMeters)
    {
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _);

        NumEnergyMeters = 2;
        EnergyMeters.allocate(NumEnergyMeters);
        EnergyMeters(1).CurTSValue = 999.9;
        EnergyMeters(1).TSValue = 999.9;
        EnergyMeters(1).RptTS = true;
        EnergyMeters(1).RptAccTS = false;
        EnergyMeters(1).RptTSFO = false;
        EnergyMeters(1).RptAccTSFO = false;
        EnergyMeters(1).TSRptNum = 1;
        EnergyMeters(1).TSRptNumChr = "1";
        EnergyMeters(1).TSAccRptNum = 1;
        EnergyMeters(1).SMValue = 999.9;

        EnergyMeters(2).CurTSValue = 9999.9;
        EnergyMeters(2).TSValue = 9999.9;
        EnergyMeters(2).RptTS = true;
        EnergyMeters(2).RptAccTS = false;
        EnergyMeters(2).RptTSFO = false;
        EnergyMeters(2).RptAccTSFO = false;
        EnergyMeters(2).TSRptNum = 2;
        EnergyMeters(2).TSRptNumChr = "2";
        EnergyMeters(2).TSAccRptNum = 2;
        EnergyMeters(2).SMValue = 9999.9;

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

        ReportTSMeters(StartMinute, EndMinute, PrintESOTimeStamp, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9", "2,9999.9"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportHRMeters)
    {
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _);

        NumEnergyMeters = 2;
        EnergyMeters.allocate(NumEnergyMeters);
        EnergyMeters(1).RptHR = true;
        EnergyMeters(1).RptHRFO = true;
        EnergyMeters(1).RptAccHR = false;
        EnergyMeters(1).RptAccHRFO = false;
        EnergyMeters(1).HRRptNum = 1;
        EnergyMeters(1).HRRptNumChr = "1";
        EnergyMeters(1).HRValue = 999.9;
        EnergyMeters(1).HRAccRptNum = 1;
        EnergyMeters(1).SMValue = 999.9;

        EnergyMeters(2).RptHR = true;
        EnergyMeters(2).RptHRFO = true;
        EnergyMeters(2).RptAccHR = false;
        EnergyMeters(2).RptAccHRFO = false;
        EnergyMeters(2).HRRptNum = 2;
        EnergyMeters(2).HRRptNumChr = "2";
        EnergyMeters(2).HRValue = 9999.9;
        EnergyMeters(2).HRAccRptNum = 2;
        EnergyMeters(2).SMValue = 9999.9;

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

        ReportHRMeters(true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay", "1,999.9", "2,9999.9"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportDYMeters)
    {
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _);

        NumEnergyMeters = 2;
        EnergyMeters.allocate(NumEnergyMeters);
        EnergyMeters(1).RptDY = true;
        EnergyMeters(1).RptDYFO = true;
        EnergyMeters(1).RptAccDY = false;
        EnergyMeters(1).RptAccDYFO = false;
        EnergyMeters(1).DYRptNum = 1;
        EnergyMeters(1).DYRptNumChr = "1";
        EnergyMeters(1).DYValue = 999.9;
        EnergyMeters(1).DYAccRptNum = 1;
        EnergyMeters(1).SMValue = 999.9;
        EnergyMeters(1).DYMaxVal = 4283136.2524843821;
        EnergyMeters(1).DYMaxValDate = 12210160;
        EnergyMeters(1).DYMinVal = 4283136.2516839253;
        EnergyMeters(1).DYMinValDate = 12210110;

        EnergyMeters(2).RptDY = true;
        EnergyMeters(2).RptDYFO = true;
        EnergyMeters(2).RptAccDY = false;
        EnergyMeters(2).RptAccDYFO = false;
        EnergyMeters(2).DYRptNum = 2;
        EnergyMeters(2).DYRptNumChr = "2";
        EnergyMeters(2).DYValue = 9999.9;
        EnergyMeters(2).DYAccRptNum = 2;
        EnergyMeters(2).SMValue = 9999.9;
        EnergyMeters(2).DYMaxVal = 4283136.2524843821;
        EnergyMeters(2).DYMaxValDate = 12210160;
        EnergyMeters(2).DYMinVal = 4283136.2516839253;
        EnergyMeters(2).DYMinValDate = 12210110;

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

        ReportDYMeters(true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0,WinterDesignDay", "1,999.9,4283136.251683925, 1,10,4283136.252484382, 1,60",
                                                         "2,9999.9,4283136.251683925, 1,10,4283136.252484382, 1,60"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "1", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"},
             {"2", "2", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportMNMeters)
    {
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _);

        NumEnergyMeters = 2;
        EnergyMeters.allocate(NumEnergyMeters);
        EnergyMeters(1).RptMN = true;
        EnergyMeters(1).RptMNFO = true;
        EnergyMeters(1).RptAccMN = false;
        EnergyMeters(1).RptAccMNFO = false;
        EnergyMeters(1).MNRptNum = 1;
        EnergyMeters(1).MNRptNumChr = "1";
        EnergyMeters(1).MNValue = 999.9;
        EnergyMeters(1).MNAccRptNum = 1;
        EnergyMeters(1).SMValue = 999.9;
        EnergyMeters(1).MNMaxVal = 4283136.2524843821;
        EnergyMeters(1).MNMaxValDate = 12210160;
        EnergyMeters(1).MNMinVal = 4283136.2516839253;
        EnergyMeters(1).MNMinValDate = 12210110;

        EnergyMeters(2).RptMN = true;
        EnergyMeters(2).RptMNFO = true;
        EnergyMeters(2).RptAccMN = false;
        EnergyMeters(2).RptAccMNFO = false;
        EnergyMeters(2).MNRptNum = 2;
        EnergyMeters(2).MNRptNumChr = "2";
        EnergyMeters(2).MNValue = 9999.9;
        EnergyMeters(2).MNAccRptNum = 2;
        EnergyMeters(2).SMValue = 9999.9;
        EnergyMeters(2).MNMaxVal = 4283136.2524843821;
        EnergyMeters(2).MNMaxValDate = 12210160;
        EnergyMeters(2).MNMinVal = 4283136.2516839253;
        EnergyMeters(2).MNMinValDate = 12210110;

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

        ReportMNMeters(true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12", "1,999.9,4283136.251683925,21, 1,10,4283136.252484382,21, 1,60",
                                                         "2,9999.9,4283136.251683925,21, 1,10,4283136.252484382,21, 1,60"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "1", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"},
             {"2", "2", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportSMMeters)
    {
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _);

        NumEnergyMeters = 2;
        EnergyMeters.allocate(NumEnergyMeters);
        EnergyMeters(1).RptSM = true;
        EnergyMeters(1).RptSMFO = true;
        EnergyMeters(1).RptAccSM = false;
        EnergyMeters(1).RptAccSMFO = false;
        EnergyMeters(1).SMRptNum = 1;
        EnergyMeters(1).SMRptNumChr = "1";
        EnergyMeters(1).SMValue = 999.9;
        EnergyMeters(1).SMAccRptNum = 1;
        EnergyMeters(1).SMValue = 999.9;
        EnergyMeters(1).SMMaxVal = 4283136.2524843821;
        EnergyMeters(1).SMMaxValDate = 12210160;
        EnergyMeters(1).SMMinVal = 4283136.2516839253;
        EnergyMeters(1).SMMinValDate = 12210110;

        EnergyMeters(2).RptSM = true;
        EnergyMeters(2).RptSMFO = true;
        EnergyMeters(2).RptAccSM = false;
        EnergyMeters(2).RptAccSMFO = false;
        EnergyMeters(2).SMRptNum = 2;
        EnergyMeters(2).SMRptNumChr = "2";
        EnergyMeters(2).SMValue = 9999.9;
        EnergyMeters(2).SMAccRptNum = 2;
        EnergyMeters(2).SMValue = 9999.9;
        EnergyMeters(2).SMMaxVal = 4283136.2524843821;
        EnergyMeters(2).SMMaxValDate = 12210160;
        EnergyMeters(2).SMMinVal = 4283136.2516839253;
        EnergyMeters(2).SMMinValDate = 12210110;

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

        ReportSMMeters(true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "", "", "", "", "", "", "1440", "4", "1", "", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1", "1,999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60",
                                                         "2,9999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "1", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"},
             {"2", "2", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportYRMeters)
    {
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _);

        NumEnergyMeters = 2;
        EnergyMeters.allocate(NumEnergyMeters);
        EnergyMeters(1).RptYR = true;
        EnergyMeters(1).RptYRFO = true;
        EnergyMeters(1).RptAccYR = false;
        EnergyMeters(1).RptAccYRFO = false;
        EnergyMeters(1).YRRptNum = 1;
        EnergyMeters(1).YRRptNumChr = "1";
        EnergyMeters(1).YRValue = 999.9;
        EnergyMeters(1).YRAccRptNum = 1;
        EnergyMeters(1).YRValue = 999.9;
        EnergyMeters(1).YRMaxVal = 4283136.2524843821;
        EnergyMeters(1).YRMaxValDate = 12210160;
        EnergyMeters(1).YRMinVal = 4283136.2516839253;
        EnergyMeters(1).YRMinValDate = 12210110;

        EnergyMeters(2).RptYR = true;
        EnergyMeters(2).RptYRFO = true;
        EnergyMeters(2).RptAccYR = false;
        EnergyMeters(2).RptAccYRFO = false;
        EnergyMeters(2).YRRptNum = 2;
        EnergyMeters(2).YRRptNumChr = "2";
        EnergyMeters(2).YRValue = 9999.9;
        EnergyMeters(2).YRAccRptNum = 2;
        EnergyMeters(2).YRValue = 9999.9;
        EnergyMeters(2).YRMaxVal = 4283136.2524843821;
        EnergyMeters(2).YRMaxValDate = 12210160;
        EnergyMeters(2).YRMinVal = 4283136.2516839253;
        EnergyMeters(2).YRMinValDate = 12210110;

        YearlyStampReportNbr = 1;
        YearlyStampReportChr = "1";
        DataGlobals::DayOfSim = 1;
        DataGlobals::DayOfSimChr = "1";
        DataGlobals::HourOfDay = 1;
        DataGlobals::CalendarYear = 2017;
        DataGlobals::CalendarYearChr = "2017";
        DataEnvironment::Month = 12;
        DataEnvironment::DayOfMonth = 21;
        DataEnvironment::DSTIndicator = 0;
        DataEnvironment::DayOfWeek = 2;
        DataEnvironment::HolidayIndex = 3;

        OutputProcessor::ReportYRMeters(true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "2017", "", "", "", "", "", "", "5", "", "", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,2017", "1,999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60",
                                                         "2,9999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "1", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"},
             {"2", "2", "4283136.25248438", "12", "21", "1", "1", "0", "4283136.25168393", "12", "21", "0", "11", "10"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeTimeStampFormatData)
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
        WriteTimeStampFormatData(DataGlobals::mtr_stream, ReportingFrequency::TimeStep, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim,
                                 DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, EndMinute, StartMinute, DSTIndicator,
                                 DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay"})));

        // TSMeter
        WriteTimeStampFormatData(DataGlobals::mtr_stream, ReportingFrequency::EachCall, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim,
                                 DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, EndMinute, StartMinute, DSTIndicator,
                                 DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay"})));

        // HRMeter
        WriteTimeStampFormatData(DataGlobals::mtr_stream, ReportingFrequency::Hourly, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim,
                                 DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, _, _, DSTIndicator, DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay"})));

        // DYMeter
        WriteTimeStampFormatData(DataGlobals::mtr_stream, ReportingFrequency::Daily, DailyStampReportNbr, DailyStampReportChr, DayOfSim, DayOfSimChr,
                                 PrintTimeStamp, Month, DayOfMonth, _, _, _, DSTIndicator, DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0,WinterDesignDay"})));

        // MNMeter
        WriteTimeStampFormatData(DataGlobals::mtr_stream, ReportingFrequency::Monthly, MonthlyStampReportNbr, MonthlyStampReportChr, DayOfSim,
                                 DayOfSimChr, PrintTimeStamp, Month, _, _, _, _, _, _);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12"})));

        // SMMeter
        WriteTimeStampFormatData(DataGlobals::mtr_stream, ReportingFrequency::Simulation, RunPeriodStampReportNbr, RunPeriodStampReportChr, DayOfSim,
                                 DayOfSimChr, PrintTimeStamp, _, _, _, _, _, _, _);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1"})));

        // Bad input
        WriteTimeStampFormatData(DataGlobals::mtr_stream, static_cast<ReportingFrequency>(999), RunPeriodStampReportNbr, RunPeriodStampReportChr,
                                 DayOfSim, DayOfSimChr, PrintTimeStamp, _, _, _, _, _, _, _);

        EXPECT_EQ("SQLite3 message, Illegal reportingInterval passed to WriteTimeStampFormatData: 999\n", ss->str());
        ss->str(std::string());

        auto timeResults = queryResult("SELECT * FROM Time;", "Time");

        std::vector<std::vector<std::string>> timeData({{"1", "0", "12", "21", "0", "10", "0", "10", "0", "1", "WinterDesignDay", "0", "0"},
                                                        {"2", "0", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"},
                                                        {"3", "0", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", "0"},
                                                        {"4", "0", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", "0"},
                                                        {"5", "0", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", "0"},
                                                        {"6", "", "", "", "", "", "", "1440", "4", "1", "", "0", "0"}});

        EXPECT_EQ(timeData, timeResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeReportMeterData)
    {
        DataGlobals::MinutesPerTimeStep = 10;

        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);

        WriteReportMeterData(1, "1", 999.9, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,999.9"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportMeterData(1, "1", 999.9, ReportingFrequency::EachCall, 0.0, 0, 0.0, 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,999.9"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Hourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Daily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211779,24,60"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211779,24,60"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Monthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211779,21,24,60"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211779,21,24,60"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Simulation, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211779,12,21,24,60"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211779,12,21,24,60"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::TimeStep, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::EachCall, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Hourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Daily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211779,24,60"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Monthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211779,21,24,60"})));

        WriteReportMeterData(1, "1", 616771620.98702729, ReportingFrequency::Simulation, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460,
                             true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211779,12,21,24,60"})));

        WriteReportMeterData(1, "1", 0, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,0.0"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"},
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
                                                          {"13", "1", "1", "0.0"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "3", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"},
             {"2", "4", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"},
             {"3", "5", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"},
             {"4", "6", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"},
             {"5", "9", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"},
             {"6", "10", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"},
             {"7", "11", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"},
             {"8", "12", "4283136.25872118", "12", "21", "24", "-9", "0", "4283136.25168393", "12", "21", "0", "1", "10"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeReportRealData)
    {
        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);

        WriteReportRealData(1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportRealData(1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::EachCall, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportRealData(1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::Hourly, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Daily, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211779,24,60"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Monthly, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211779,21,24,60"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Simulation, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211779,12,21,24,60"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::TimeStep, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::EachCall, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Hourly, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Daily, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273,4283136.251683925, 1,10,4283136.2587211779,24,60"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Monthly, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273,4283136.251683925,21, 1,10,4283136.2587211779,21,24,60"})));

        WriteReportRealData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Simulation, 4283136.2516839253, 12210110,
                            4283136.2587211775, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273,4283136.251683925,12,21, 1,10,4283136.2587211779,12,21,24,60"})));

        WriteReportRealData(1, "1", 0, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"},
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
                                                          {"13", "1", "1", "0.0"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "4", "4283136.25872118", "12", "21", "24", "", "0", "4283136.25168393", "12", "21", "0", "", "10"},
             {"2", "5", "4283136.25872118", "12", "21", "24", "", "0", "4283136.25168393", "12", "21", "0", "", "10"},
             {"3", "6", "4283136.25872118", "12", "21", "24", "", "0", "4283136.25168393", "12", "21", "0", "", "10"},
             {"4", "10", "4283136.25872118", "12", "21", "24", "", "0", "4283136.25168393", "12", "21", "0", "", "10"},
             {"5", "11", "4283136.25872118", "12", "21", "24", "", "0", "4283136.25168393", "12", "21", "0", "", "10"},
             {"6", "12", "4283136.25872118", "12", "21", "24", "", "0", "4283136.25168393", "12", "21", "0", "", "10"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeReportIntegerData)
    {
        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);

        WriteReportIntegerData(1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportIntegerData(1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::EachCall, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportIntegerData(1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::Hourly, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Daily, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.987027,4283136, 1,10,4283196,24,60"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Monthly, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.987027,4283136,21, 1,10,4283196,21,24,60"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Simulation, 4283136, 12210110, 4283196,
                               12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.987027,4283136,12,21, 1,10,4283196,12,21,24,60"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::TimeStep, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::EachCall, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Hourly, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Daily, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027,4283136, 1,10,4283196,24,60"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Monthly, 4283136, 12210110, 4283196,
                               12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027,4283136,21, 1,10,4283196,21,24,60"})));

        WriteReportIntegerData(1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Simulation, 4283136, 12210110, 4283196,
                               12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027,4283136,12,21, 1,10,4283196,12,21,24,60"})));

        WriteReportIntegerData(1, "1", 0, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"},
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
                                                          {"13", "1", "1", "0.0"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "4", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"2", "5", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"3", "6", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"4", "10", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"5", "11", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"6", "12", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeNumericData_1)
    {
        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);

        WriteNumericData(1, "1", 999);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999"})));

        WriteNumericData(1, "1", 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0"})));

        WriteNumericData(1, "1", -999);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-999"})));

        WriteNumericData(1, "1", 999.9);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"})));

        WriteNumericData(1, "1", 0.0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"})));

        WriteNumericData(1, "1", -999.9);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-999.9"})));

        WriteNumericData(1, "1", 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0"})));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({
            {"1", "1", "1", "999.0"},
            {"2", "1", "1", "0.0"},
            {"3", "1", "1", "-999.0"},
            {"4", "1", "1", "999.9"},
            {"5", "1", "1", "0.0"},
            {"6", "1", "1", "-999.9"},
            {"7", "1", "1", "0.0"},
        });

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_getStandardMeterResourceType)
    {
        std::map<std::string, std::string> const resource_map = {{"ELECTRICITY", "Electricity"},
                                                                 {"ELECTRIC", "Electricity"},
                                                                 {"ELEC", "Electricity"},
                                                                 {"GAS", "Gas"},
                                                                 {"NATURALGAS", "Gas"},
                                                                 {"NATURAL GAS", "Gas"},
                                                                 {"GASOLINE", "Gasoline"},
                                                                 {"DIESEL", "Diesel"},
                                                                 {"COAL", "Coal"},
                                                                 {"FUEL OIL #1", "FuelOil#1"},
                                                                 {"FUELOIL#1", "FuelOil#1"},
                                                                 {"FUEL OIL", "FuelOil#1"},
                                                                 {"DISTILLATE OIL", "FuelOil#1"},
                                                                 {"FUEL OIL #2", "FuelOil#2"},
                                                                 {"FUELOIL#2", "FuelOil#2"},
                                                                 {"RESIDUAL OIL", "FuelOil#2"},
                                                                 {"PROPANE", "Propane"},
                                                                 {"LPG", "Propane"},
                                                                 {"PROPANEGAS", "Propane"},
                                                                 {"PROPANE GAS", "Propane"},
                                                                 {"WATER", "Water"},
                                                                 {"H2O", "Water"},
                                                                 {"ONSITEWATER", "OnSiteWater"},
                                                                 {"WATERPRODUCED", "OnSiteWater"},
                                                                 {"ONSITE WATER", "OnSiteWater"},
                                                                 {"MAINSWATER", "MainsWater"},
                                                                 {"WATERSUPPLY", "MainsWater"},
                                                                 {"RAINWATER", "RainWater"},
                                                                 {"PRECIPITATION", "RainWater"},
                                                                 {"WELLWATER", "WellWater"},
                                                                 {"GROUNDWATER", "WellWater"},
                                                                 {"CONDENSATE", "Condensate"},
                                                                 {"ENERGYTRANSFER", "EnergyTransfer"},
                                                                 {"ENERGYXFER", "EnergyTransfer"},
                                                                 {"XFER", "EnergyTransfer"},
                                                                 {"STEAM", "Steam"},
                                                                 {"DISTRICTCOOLING", "DistrictCooling"},
                                                                 {"DISTRICTHEATING", "DistrictHeating"},
                                                                 {"ELECTRICITYPRODUCED", "ElectricityProduced"},
                                                                 {"ELECTRICITYPURCHASED", "ElectricityPurchased"},
                                                                 {"ELECTRICITYSURPLUSSOLD", "ElectricitySurplusSold"},
                                                                 {"ELECTRICITYNET", "ElectricityNet"},
                                                                 {"SOLARWATER", "SolarWater"},
                                                                 {"SOLARAIR", "SolarAir"},
                                                                 {"SO2", "SO2"},
                                                                 {"NOX", "NOx"},
                                                                 {"N2O", "N2O"},
                                                                 {"PM", "PM"},
                                                                 {"PM2.5", "PM2.5"},
                                                                 {"PM10", "PM10"},
                                                                 {"CO", "CO"},
                                                                 {"CO2", "CO2"},
                                                                 {"CH4", "CH4"},
                                                                 {"NH3", "NH3"},
                                                                 {"NMVOC", "NMVOC"},
                                                                 {"HG", "Hg"},
                                                                 {"PB", "Pb"},
                                                                 {"NUCLEAR HIGH", "Nuclear High"},
                                                                 {"NUCLEAR LOW", "Nuclear Low"},
                                                                 {"WATERENVIRONMENTALFACTORS", "WaterEnvironmentalFactors"},
                                                                 {"CARBON EQUIVALENT", "Carbon Equivalent"},
                                                                 {"SOURCE", "Source"},
                                                                 {"PLANTLOOPHEATINGDEMAND", "PlantLoopHeatingDemand"},
                                                                 {"PLANTLOOPCOOLINGDEMAND", "PlantLoopCoolingDemand"},
                                                                 {"GENERIC", "Generic"},
                                                                 {"OTHERFUEL1", "OtherFuel1"},
                                                                 {"OTHERFUEL2", "OtherFuel2"}};

        std::string out_resource_type;
        bool error_found = false;

        for (auto const &meterType : resource_map) {
            GetStandardMeterResourceType(out_resource_type, meterType.first, error_found);
            EXPECT_EQ(meterType.second, out_resource_type);
            EXPECT_FALSE(error_found);
        }

        EnergyPlus::sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        auto const meterType = "BAD INPUT";
        out_resource_type = "BAD INPUT";

        GetStandardMeterResourceType(out_resource_type, meterType, error_found);

        EXPECT_EQ(meterType, out_resource_type);
        EXPECT_TRUE(error_found);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(1ul, errorData.size());
        std::vector<std::string> errorData0{"1", "1", "1", "GetStandardMeterResourceType: Illegal OutResourceType (for Meters) Entered=BAD INPUT",
                                            "1"};
        EXPECT_EQ(errorData0, errorData[0]);
    }

    TEST_F(SQLiteFixture, OutputProcessor_determineIndexGroupKeyFromMeterName)
    {
        std::map<std::string, int> const resource_map = {{"Electricity:Facility", 100},
                                                         {"Gas:Facility", 101},
                                                         {"DistricHeating:Facility", 102},
                                                         {"DistricCooling:Facility", 103},
                                                         {"ElectricityNet:Facility", 104},
                                                         {"Electricity:Building", 201},
                                                         {"Gas:Building", 202},
                                                         {"Electricity:HVAC", 301},
                                                         {"InteriorLights:Electricity", 401},
                                                         {"InteriorLights:Electricity:Zone", 501},
                                                         {"BAD INPUT", -11}};

        for (auto const &indexGroup : resource_map) {
            EXPECT_EQ(indexGroup.second, DetermineIndexGroupKeyFromMeterName(indexGroup.first)) << "where meterName is " << indexGroup.first;
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_validateIndexType)
    {
        std::map<std::string, int> const resource_map = {{"ZONE", 1}, {"HEATBALANCE", 1}, {"HEAT BALANCE", 1},
                                                         {"HVAC", 2}, {"SYSTEM", 2},      {"PLANT", 2}};

        auto const calledFrom = "UnitTest";

        for (auto const &indexGroup : resource_map) {
            EXPECT_EQ(indexGroup.second, ValidateIndexType(indexGroup.first, calledFrom)) << "where indexTypeKey is " << indexGroup.first;
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_DeathTest_validateIndexType)
    {
        auto const calledFrom = "UnitTest";
        EXPECT_ANY_THROW(ValidateIndexType("BAD INPUT", calledFrom));
    }

    TEST_F(SQLiteFixture, OutputProcessor_standardIndexTypeKey)
    {
        EXPECT_EQ("Zone", StandardIndexTypeKey(1));
        EXPECT_EQ("HVAC", StandardIndexTypeKey(2));
        EXPECT_EQ("UNKW", StandardIndexTypeKey(0));
        EXPECT_EQ("UNKW", StandardIndexTypeKey(-1));
        EXPECT_EQ("UNKW", StandardIndexTypeKey(3));
    }

    TEST_F(SQLiteFixture, OutputProcessor_validateVariableType)
    {
        std::map<std::string, StoreType> const resource_map = {
            {"STATE", StoreType::Averaged},  {"AVERAGE", StoreType::Averaged}, {"AVERAGED", StoreType::Averaged}, {"NON STATE", StoreType::Summed},
            {"NONSTATE", StoreType::Summed}, {"SUM", StoreType::Summed},       {"SUMMED", StoreType::Summed}};

        for (auto const &variableType : resource_map) {
            EXPECT_EQ(variableType.second, validateVariableType(variableType.first)) << "where variableTypeKey is " << variableType.first;
        }

        EnergyPlus::sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        std::string const variableTypeKey = "BAD INPUT";

        auto index = validateVariableType(variableTypeKey);

        EXPECT_EQ(StoreType::Averaged, index);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(1ul, errorData.size());
        std::vector<std::string> errorData0{"1", "1", "1", "Invalid variable type requested=BAD INPUT", "1"};
        EXPECT_EQ(errorData0, errorData[0]);
    }

    TEST_F(SQLiteFixture, OutputProcessor_standardVariableTypeKey)
    {
        EXPECT_EQ("Average", standardVariableTypeKey(StoreType::Averaged));
        EXPECT_EQ("Sum", standardVariableTypeKey(StoreType::Summed));
        EXPECT_EQ("Unknown", standardVariableTypeKey(static_cast<StoreType>(0)));
    }

    TEST_F(SQLiteFixture, OutputProcessor_determineMeterIPUnits)
    {
        int ipUnits = -999999;
        bool errorFound = false;

        DetermineMeterIPUnits(ipUnits, "ELEC", OutputProcessor::Unit::J, errorFound);
        EXPECT_EQ(RT_IPUnits_Electricity, ipUnits);
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(ipUnits, "GAS", OutputProcessor::Unit::J, errorFound);
        EXPECT_EQ(RT_IPUnits_Gas, ipUnits);
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(ipUnits, "COOL", OutputProcessor::Unit::J, errorFound);
        EXPECT_EQ(RT_IPUnits_Cooling, ipUnits);
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(ipUnits, "WATER", OutputProcessor::Unit::m3, errorFound);
        EXPECT_EQ(RT_IPUnits_Water, ipUnits);
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(ipUnits, "OTHER", OutputProcessor::Unit::m3, errorFound);
        EXPECT_EQ(RT_IPUnits_OtherM3, ipUnits);
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(ipUnits, "OTHER", OutputProcessor::Unit::kg, errorFound);
        EXPECT_EQ(RT_IPUnits_OtherKG, ipUnits);
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(ipUnits, "OTHER", OutputProcessor::Unit::L, errorFound);
        EXPECT_EQ(RT_IPUnits_OtherL, ipUnits);
        EXPECT_FALSE(errorFound);

        EnergyPlus::sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        ipUnits = -999999;
        DetermineMeterIPUnits(ipUnits, "UNKONWN", OutputProcessor::Unit::unknown, errorFound); // was "badunits"
        EXPECT_EQ(RT_IPUnits_OtherJ, ipUnits);
        EXPECT_TRUE(errorFound);

        ipUnits = -999999;
        DetermineMeterIPUnits(ipUnits, "ELEC", OutputProcessor::Unit::unknown, errorFound); // was "kWh"
        EXPECT_EQ(RT_IPUnits_Electricity, ipUnits);
        EXPECT_TRUE(errorFound);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(2ul, errorData.size());
        std::vector<std::string> errorData0{"1", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[unknown].",
                                            "1"};
        std::vector<std::string> errorData1{"2", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[unknown].",
                                            "1"};
        EXPECT_EQ(errorData0, errorData[0]);
        EXPECT_EQ(errorData1, errorData[1]);
    }

    TEST_F(SQLiteFixture, OutputProcessor_dateToStringWithMonth)
    {
        EXPECT_EQ("01-JAN-00:01", DateToStringWithMonth(1010101));
        EXPECT_EQ("01-JAN-00:00", DateToStringWithMonth(1010100));
        EXPECT_EQ("01-FEB-01:00", DateToStringWithMonth(2010160));
        EXPECT_EQ("20-MAR-01:59", DateToStringWithMonth(3200259));
        EXPECT_EQ("13-APR-23:59", DateToStringWithMonth(4132459));
        EXPECT_EQ("15-MAY-20:30", DateToStringWithMonth(5152130));
        EXPECT_EQ("19-JUN-12:10", DateToStringWithMonth(6191310));
        EXPECT_EQ("25-JUL-19:40", DateToStringWithMonth(7252040));
        EXPECT_EQ("05-AUG-06:22", DateToStringWithMonth(8050722));
        EXPECT_EQ("03-SEP-09:50", DateToStringWithMonth(9031050));
        EXPECT_EQ("27-OCT-03:31", DateToStringWithMonth(10270431));
        EXPECT_EQ("08-NOV-22:28", DateToStringWithMonth(11082328));
        EXPECT_EQ("21-DEC-00:10", DateToStringWithMonth(12210110));

        EXPECT_EQ("-", DateToStringWithMonth(0));
        EXPECT_EQ("-", DateToStringWithMonth(-9999));
        EXPECT_EQ("-", DateToStringWithMonth(-12210110));
        EXPECT_EQ("-", DateToStringWithMonth(13082328));
        EXPECT_EQ("-", DateToStringWithMonth(10100));
        EXPECT_EQ("-", DateToStringWithMonth(1000101));
        EXPECT_EQ("-", DateToStringWithMonth(990101));
        EXPECT_EQ("-", DateToStringWithMonth(1010001));
        EXPECT_EQ("-", DateToStringWithMonth(1009901));
        EXPECT_EQ("-", DateToStringWithMonth(1010099));
        EXPECT_EQ("-", DateToStringWithMonth(1320100));
        EXPECT_EQ("-", DateToStringWithMonth(1012500));
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeMeterDictionaryItem)
    {
        InitializeOutput();

        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);

        WriteMeterDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 1, -999, "indexGroup", "1", "meterName", OutputProcessor::Unit::J,
                                 false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,meterName [J] !TimeStep"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1,meterName [J] !TimeStep"})));

        WriteMeterDictionaryItem(ReportingFrequency::TimeStep, StoreType::Summed, 2, -999, "indexGroup", "2", "meterName", OutputProcessor::Unit::W,
                                 false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"2,1,meterName [W] !TimeStep"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"2,1,meterName [W] !TimeStep"})));

        WriteMeterDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 3, -999, "indexGroup", "3", "meterName", OutputProcessor::Unit::J,
                                 true, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"3,1,Cumulative meterName [J] !TimeStep"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"3,1,Cumulative meterName [J] !TimeStep"})));

        WriteMeterDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 4, -999, "indexGroup", "4", "meterName", OutputProcessor::Unit::W,
                                 false, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"4,1,meterName [W] !TimeStep"})));

        WriteMeterDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 5, -999, "indexGroup", "5", "meterName", OutputProcessor::Unit::W,
                                 true, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"5,1,Cumulative meterName [W] !TimeStep"})));

        WriteMeterDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 6, -999, "indexGroup", "6", "meterName", OutputProcessor::Unit::J,
                                 false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"6,1,meterName [J] !Each Call"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"6,1,meterName [J] !Each Call"})));

        WriteMeterDictionaryItem(ReportingFrequency::EachCall, StoreType::Summed, 7, -999, "indexGroup", "7", "meterName", OutputProcessor::Unit::J,
                                 false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"7,1,meterName [J] !Each Call"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"7,1,meterName [J] !Each Call"})));

        WriteMeterDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 8, -999, "indexGroup", "8", "meterName", OutputProcessor::Unit::J,
                                 true, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"8,1,Cumulative meterName [J] !Each Call"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"8,1,Cumulative meterName [J] !Each Call"})));

        WriteMeterDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 9, -999, "indexGroup", "9", "meterName", OutputProcessor::Unit::J,
                                 false, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"9,1,meterName [J] !Each Call"})));

        WriteMeterDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 10, -999, "indexGroup", "10", "meterName",
                                 OutputProcessor::Unit::J, true, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"10,1,Cumulative meterName [J] !Each Call"})));

        WriteMeterDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 11, -999, "indexGroup", "11", "meterName", OutputProcessor::Unit::J,
                                 false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"11,1,meterName [J] !Hourly"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"11,1,meterName [J] !Hourly"})));

        WriteMeterDictionaryItem(ReportingFrequency::Hourly, StoreType::Summed, 12, -999, "indexGroup", "12", "meterName",
                                 OutputProcessor::Unit::None, false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"12,1,meterName [] !Hourly"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"12,1,meterName [] !Hourly"})));

        WriteMeterDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 13, -999, "indexGroup", "13", "meterName",
                                 OutputProcessor::Unit::None, true, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"13,1,Cumulative meterName [] !Hourly"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"13,1,Cumulative meterName [] !Hourly"})));

        WriteMeterDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 14, -999, "indexGroup", "14", "meterName",
                                 OutputProcessor::Unit::None, false, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"14,1,meterName [] !Hourly"})));

        WriteMeterDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 15, -999, "indexGroup", "15", "meterName",
                                 OutputProcessor::Unit::None, true, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"15,1,Cumulative meterName [] !Hourly"})));

        WriteMeterDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 16, -999, "indexGroup", "16", "meterName",
                                 OutputProcessor::Unit::None, false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"16,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"16,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Daily, StoreType::Summed, 17, -999, "indexGroup", "17", "meterName", OutputProcessor::Unit::None,
                                 false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"17,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"17,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 18, -999, "indexGroup", "18", "meterName",
                                 OutputProcessor::Unit::deltaC, true, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"18,1,Cumulative meterName [deltaC] !Daily "})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"18,1,Cumulative meterName [deltaC] !Daily "})));

        WriteMeterDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 19, -999, "indexGroup", "19", "meterName",
                                 OutputProcessor::Unit::deltaC, false, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"19,7,meterName [deltaC] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 20, -999, "indexGroup", "20", "meterName",
                                 OutputProcessor::Unit::deltaC, true, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"20,1,Cumulative meterName [deltaC] !Daily "})));

        WriteMeterDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 21, -999, "indexGroup", "21", "meterName",
                                 OutputProcessor::Unit::deltaC, false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"21,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"21,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Monthly, StoreType::Summed, 22, -999, "indexGroup", "22", "meterName",
                                 OutputProcessor::Unit::deltaC, false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"22,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"22,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 23, -999, "indexGroup", "23", "meterName",
                                 OutputProcessor::Unit::deltaC, true, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"23,1,Cumulative meterName [deltaC] !Monthly "})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"23,1,Cumulative meterName [deltaC] !Monthly "})));

        WriteMeterDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 24, -999, "indexGroup", "24", "meterName",
                                 OutputProcessor::Unit::deltaC, false, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"24,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 25, -999, "indexGroup", "25", "meterName",
                                 OutputProcessor::Unit::deltaC, true, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"25,1,Cumulative meterName [deltaC] !Monthly "})));

        WriteMeterDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 26, -999, "indexGroup", "26", "meterName",
                                 OutputProcessor::Unit::deltaC, false, false);
        EXPECT_TRUE(compare_mtr_stream(
            delimited_string({"26,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"26,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Simulation, StoreType::Summed, 27, -999, "indexGroup", "27", "meterName",
                                 OutputProcessor::Unit::deltaC, false, false);
        EXPECT_TRUE(compare_mtr_stream(
            delimited_string({"27,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"27,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 28, -999, "indexGroup", "28", "meterName",
                                 OutputProcessor::Unit::deltaC, true, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"28,1,Cumulative meterName [deltaC] !RunPeriod "})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"28,1,Cumulative meterName [deltaC] !RunPeriod "})));

        WriteMeterDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 29, -999, "indexGroup", "29", "meterName",
                                 OutputProcessor::Unit::deltaC, false, true);
        EXPECT_TRUE(compare_mtr_stream(
            delimited_string({"29,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));

        WriteMeterDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 30, -999, "indexGroup", "30", "meterName",
                                 OutputProcessor::Unit::deltaC, true, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"30,1,Cumulative meterName [deltaC] !RunPeriod "})));

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary(
            {{"1", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "J"},
             {"2", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "W"},
             {"3", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "J"},
             {"4", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "W"},
             {"5", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "W"},
             {"6", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "J"},
             {"7", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "J"},
             {"8", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "J"},
             {"9", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "J"},
             {"10", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "J"},
             {"11", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "J"},
             {"12", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", ""},
             {"13", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", ""},
             {"14", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", ""},
             {"15", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", ""},
             {"16", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", ""},
             {"17", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Daily", "", ""},
             {"18", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "deltaC"},
             {"19", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "deltaC"},
             {"20", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "deltaC"},
             {"21", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "deltaC"},
             {"22", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "deltaC"},
             {"23", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "deltaC"},
             {"24", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "deltaC"},
             {"25", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "deltaC"},
             {"26", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "deltaC"},
             {"27", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "deltaC"},
             {"28", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "deltaC"},
             {"29", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "deltaC"},
             {"30", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "deltaC"}});
        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeReportVariableDictionaryItem)
    {
        InitializeOutput();

        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);

        WriteReportVariableDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 1, -999, "indexGroup", "1", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1,keyedValue,variableName [m3/s] !TimeStep"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::TimeStep, StoreType::Summed, 2, -999, "indexGroup", "2", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"2,1,keyedValue,variableName [m3/s] !TimeStep"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 3, -999, "indexGroup", "3", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, "scheduleName");
        EXPECT_TRUE(compare_eso_stream(delimited_string({"3,1,keyedValue,variableName [m3/s] !TimeStep,scheduleName"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 4, -999, "indexGroup", "4", "keyedValue", "variableName",
                                          2, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"4,1,keyedValue,variableName [m3/s] !TimeStep"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::TimeStep, StoreType::Averaged, 5, -999, "indexGroup", "5", "keyedValue", "variableName",
                                          3, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"5,1,keyedValue,variableName [m3/s] !TimeStep"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 6, -999, "indexGroup", "6", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"6,1,keyedValue,variableName [m3/s] !Each Call"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::EachCall, StoreType::Summed, 7, -999, "indexGroup", "7", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"7,1,keyedValue,variableName [m3/s] !Each Call"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 8, -999, "indexGroup", "8", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, "scheduleName");
        EXPECT_TRUE(compare_eso_stream(delimited_string({"8,1,keyedValue,variableName [m3/s] !Each Call,scheduleName"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 9, -999, "indexGroup", "9", "keyedValue", "variableName",
                                          2, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"9,1,keyedValue,variableName [m3/s] !Each Call"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::EachCall, StoreType::Averaged, 10, -999, "indexGroup", "10", "keyedValue",
                                          "variableName", 3, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"10,1,keyedValue,variableName [m3/s] !Each Call"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 11, -999, "indexGroup", "11", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingHourlyVariables);
        TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"11,1,keyedValue,variableName [m3/s] !Hourly"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Hourly, StoreType::Summed, 12, -999, "indexGroup", "12", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingHourlyVariables);
        TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"12,1,keyedValue,variableName [m3/s] !Hourly"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 13, -999, "indexGroup", "13", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, "scheduleName");
        EXPECT_TRUE(TrackingHourlyVariables);
        TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"13,1,keyedValue,variableName [m3/s] !Hourly,scheduleName"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 14, -999, "indexGroup", "14", "keyedValue", "variableName",
                                          2, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingHourlyVariables);
        TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"14,1,keyedValue,variableName [m3/s] !Hourly"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Hourly, StoreType::Averaged, 15, -999, "indexGroup", "15", "keyedValue", "variableName",
                                          3, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingHourlyVariables);
        TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"15,1,keyedValue,variableName [m3/s] !Hourly"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 16, -999, "indexGroup", "16", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingDailyVariables);
        TrackingDailyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"16,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Daily, StoreType::Summed, 17, -999, "indexGroup", "17", "keyedValue", "variableName", 1,
                                          OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingDailyVariables);
        TrackingDailyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"17,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 18, -999, "indexGroup", "18", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, "scheduleName");
        EXPECT_TRUE(TrackingDailyVariables);
        TrackingDailyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"18,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute],scheduleName"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 19, -999, "indexGroup", "19", "keyedValue", "variableName",
                                          2, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingDailyVariables);
        TrackingDailyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"19,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Daily, StoreType::Averaged, 20, -999, "indexGroup", "20", "keyedValue", "variableName",
                                          3, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingDailyVariables);
        TrackingDailyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"20,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 21, -999, "indexGroup", "21", "keyedValue",
                                          "variableName", 1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingMonthlyVariables);
        TrackingMonthlyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"21,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Monthly, StoreType::Summed, 22, -999, "indexGroup", "22", "keyedValue", "variableName",
                                          1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingMonthlyVariables);
        TrackingMonthlyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"22,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 23, -999, "indexGroup", "23", "keyedValue",
                                          "variableName", 1, OutputProcessor::Unit::m3_s, _, "scheduleName");
        EXPECT_TRUE(TrackingMonthlyVariables);
        TrackingMonthlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"23,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute],scheduleName"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 24, -999, "indexGroup", "24", "keyedValue",
                                          "variableName", 2, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingMonthlyVariables);
        TrackingMonthlyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"24,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Monthly, StoreType::Averaged, 25, -999, "indexGroup", "25", "keyedValue",
                                          "variableName", 3, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingMonthlyVariables);
        TrackingMonthlyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"25,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 26, -999, "indexGroup", "26", "keyedValue",
                                          "variableName", 1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingRunPeriodVariables);
        TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"26,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Simulation, StoreType::Summed, 27, -999, "indexGroup", "27", "keyedValue",
                                          "variableName", 1, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingRunPeriodVariables);
        TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"27,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 28, -999, "indexGroup", "28", "keyedValue",
                                          "variableName", 1, OutputProcessor::Unit::m3_s, _, "scheduleName");
        EXPECT_TRUE(TrackingRunPeriodVariables);
        TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string(
            {"28,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute],scheduleName"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 29, -999, "indexGroup", "29", "keyedValue",
                                          "variableName", 2, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingRunPeriodVariables);
        TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"29,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));

        WriteReportVariableDictionaryItem(ReportingFrequency::Simulation, StoreType::Averaged, 30, -999, "indexGroup", "30", "keyedValue",
                                          "variableName", 3, OutputProcessor::Unit::m3_s, _, _);
        EXPECT_TRUE(TrackingRunPeriodVariables);
        TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"30,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"})));

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary(
            {{"1", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"2", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"3", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "scheduleName", "m3/s"},
             {"4", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"5", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"6", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"7", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"8", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "scheduleName", "m3/s"},
             {"9", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"10", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"11", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"12", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"13", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "scheduleName", "m3/s"},
             {"14", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"15", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"16", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"17", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"18", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "scheduleName", "m3/s"},
             {"19", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"20", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"21", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"22", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"23", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "scheduleName", "m3/s"},
             {"24", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"25", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"26", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "", "m3/s"},
             {"27", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "", "m3/s"},
             {"28", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "scheduleName", "m3/s"},
             {"29", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Run Period", "", "m3/s"},
             {"30", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Run Period", "", "m3/s"}});
        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeCumulativeReportMeterData)
    {
        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);

        WriteCumulativeReportMeterData(1, "1", 616771620.98702729, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"})));

        WriteCumulativeReportMeterData(1, "1", 616771620.98702729, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273"})));

        WriteCumulativeReportMeterData(1, "1", 0, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,0.0"})));

        WriteCumulativeReportMeterData(1, "1", 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,0.0"})));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"})));

        auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        ASSERT_EQ(4ul, reportData.size());
        std::vector<std::string> reportData0{"1", "1", "1", "616771620.987027"};
        std::vector<std::string> reportData1{"2", "1", "1", "616771620.987027"};
        std::vector<std::string> reportData2{"3", "1", "1", "0.0"};
        std::vector<std::string> reportData3{"4", "1", "1", "0.0"};
        EXPECT_EQ(reportData0, reportData[0]);
        EXPECT_EQ(reportData1, reportData[1]);
        EXPECT_EQ(reportData2, reportData[2]);
        EXPECT_EQ(reportData3, reportData[3]);

        ASSERT_EQ(0ul, reportExtendedData.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeNumericData_2)
    {
        EnergyPlus::sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        EnergyPlus::sqlite->createSQLiteReportDictionaryRecord(1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false,
                                                               _);

        WriteNumericData(1, "1", 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0"})));

        WriteNumericData(1, "1", 0.1);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.1"})));

        WriteNumericData(1, "1", -0.1);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-0.1"})));

        WriteNumericData(1, "1", 1.0e-2);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.01"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.01"})));
#endif

        WriteNumericData(1, "1", 1.0e-3);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.001"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.001"})));
#endif

        WriteNumericData(1, "1", 1.0e-4);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0001"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0001"})));
#endif

        WriteNumericData(1, "1", 1.0e-5);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.00001"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.00001"})));
#endif

        WriteNumericData(1, "1", 1.0e-6);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.000001"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.000001"})));
#endif

        WriteNumericData(1, "1", 1.0e-7);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-7"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-7"})));
#endif

        WriteNumericData(1, "1", 1.0e-8);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-8"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-8"})));
#endif

        WriteNumericData(1, "1", 1.0e-9);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-9"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-9"})));
#endif

        WriteNumericData(1, "1", 1.0e-10);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-10"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-10"})));
#endif

        WriteNumericData(1, "1", 1.0e-11);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-11"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-11"})));
#endif

        WriteNumericData(1, "1", 1.0e-12);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-12"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-12"})));
#endif

        WriteNumericData(1, "1", 1.0e-13);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-13"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-13"})));
#endif

        WriteNumericData(1, "1", 1.0e-14);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-14"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-14"})));
#endif

        WriteNumericData(1, "1", 1.0e-15);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-15"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-15"})));
#endif

        WriteNumericData(1, "1", 1.0e-16);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-16"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-16"})));
#endif

        WriteNumericData(1, "1", -1.0e-16);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-1e-16"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-1e-16"})));
#endif

        WriteNumericData(1, "1", 1.0e-19);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-19"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-19"})));
#endif

        WriteNumericData(1, "1", 0.5);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.5"})));

        WriteNumericData(1, "1", 1.0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1.0"})));

        WriteNumericData(1, "1", 10.0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10.0"})));

        WriteNumericData(1, "1", 1.0e2);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100.0"})));

        WriteNumericData(1, "1", 1.0e3);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000.0"})));

        WriteNumericData(1, "1", 1.0e4);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000.0"})));

        WriteNumericData(1, "1", 1.0e5);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000.0"})));

        WriteNumericData(1, "1", 1.0e6);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000.0"})));

        WriteNumericData(1, "1", 1.0e7);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000.0"})));

        WriteNumericData(1, "1", 1.0e8);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000.0"})));

        WriteNumericData(1, "1", 1.0e9);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000000.0"})));

        WriteNumericData(1, "1", 1.0e10);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000000.0"})));

        WriteNumericData(1, "1", 1.0e11);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000.0"})));

        WriteNumericData(1, "1", 1.0e12);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000000000.0"})));

        WriteNumericData(1, "1", 1.0e13);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000000000.0"})));

        WriteNumericData(1, "1", 1.0e14);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000000.0"})));

        WriteNumericData(1, "1", 1.0e15);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000000000000.0"})));

        WriteNumericData(1, "1", 1.0e16);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000000000000.0"})));

        WriteNumericData(1, "1", 1.0e17);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000000000.0"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000000000.0"})));
#endif

        WriteNumericData(1, "1", -1.0e16);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-10000000000000000.0"})));

        WriteNumericData(1, "1", -1.0e17);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-100000000000000000.0"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-100000000000000000.0"})));
#endif

        WriteNumericData(1, "1", 1.0e25);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e25"})));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e25"})));
#endif

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "0.0"},
                                                          {"2", "1", "1", "0.1"},
                                                          {"3", "1", "1", "-0.1"},
                                                          {"4", "1", "1", "0.01"},
                                                          {"5", "1", "1", "0.001"},
                                                          {"6", "1", "1", "0.0001"},
                                                          {"7", "1", "1", "1.0e-05"},
                                                          {"8", "1", "1", "1.0e-06"},
                                                          {"9", "1", "1", "1.0e-07"},
                                                          {"10", "1", "1", "1.0e-08"},
                                                          {"11", "1", "1", "1.0e-09"},
                                                          {"12", "1", "1", "1.0e-10"},
                                                          {"13", "1", "1", "1.0e-11"},
                                                          {"14", "1", "1", "1.0e-12"},
                                                          {"15", "1", "1", "1.0e-13"},
                                                          {"16", "1", "1", "1.0e-14"},
                                                          {"17", "1", "1", "1.0e-15"},
                                                          {"18", "1", "1", "1.0e-16"},
                                                          {"19", "1", "1", "-1.0e-16"},
                                                          {"20", "1", "1", "1.0e-19"},
                                                          {"21", "1", "1", "0.5"},
                                                          {"22", "1", "1", "1.0"},
                                                          {"23", "1", "1", "10.0"},
                                                          {"24", "1", "1", "100.0"},
                                                          {"25", "1", "1", "1000.0"},
                                                          {"26", "1", "1", "10000.0"},
                                                          {"27", "1", "1", "100000.0"},
                                                          {"28", "1", "1", "1000000.0"},
                                                          {"29", "1", "1", "10000000.0"},
                                                          {"30", "1", "1", "100000000.0"},
                                                          {"31", "1", "1", "1000000000.0"},
                                                          {"32", "1", "1", "10000000000.0"},
                                                          {"33", "1", "1", "100000000000.0"},
                                                          {"34", "1", "1", "1000000000000.0"},
                                                          {"35", "1", "1", "10000000000000.0"},
                                                          {"36", "1", "1", "100000000000000.0"},
                                                          {"37", "1", "1", "1.0e+15"},
                                                          {"38", "1", "1", "1.0e+16"},
                                                          {"39", "1", "1", "1.0e+17"},
                                                          {"40", "1", "1", "-1.0e+16"},
                                                          {"41", "1", "1", "-1.0e+17"},
                                                          {"42", "1", "1", "1.0e+25"}});

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_addMeter)
    {
        auto const name("testMeter");
        OutputProcessor::Unit const units(OutputProcessor::Unit::J);
        auto const resourceType("ELEC");
        auto const endUse("testEndUse");
        auto const endUseSub("testEndUseSub");
        auto const group("testGroup");

        EXPECT_EQ(0, NumEnergyMeters);
        EXPECT_EQ(0ul, EnergyMeters.size());

        AddMeter(name, units, resourceType, endUse, endUseSub, group);

        ASSERT_EQ(1, NumEnergyMeters);
        ASSERT_EQ(1ul, EnergyMeters.size());

        EXPECT_EQ(name, EnergyMeters(1).Name);
        EXPECT_EQ(resourceType, EnergyMeters(1).ResourceType);
        EXPECT_EQ(endUse, EnergyMeters(1).EndUse);
        EXPECT_EQ(endUseSub, EnergyMeters(1).EndUseSub);
        EXPECT_EQ(group, EnergyMeters(1).Group);
        EXPECT_EQ(units, EnergyMeters(1).Units);
        EXPECT_EQ(1, EnergyMeters(1).TSRptNum);
        EXPECT_EQ(2, EnergyMeters(1).HRRptNum);
        EXPECT_EQ(3, EnergyMeters(1).DYRptNum);
        EXPECT_EQ(4, EnergyMeters(1).MNRptNum);
        EXPECT_EQ(5, EnergyMeters(1).YRRptNum);
        EXPECT_EQ(6, EnergyMeters(1).SMRptNum);
        EXPECT_EQ(7, EnergyMeters(1).TSAccRptNum);
        EXPECT_EQ(8, EnergyMeters(1).HRAccRptNum);
        EXPECT_EQ(9, EnergyMeters(1).DYAccRptNum);
        EXPECT_EQ(10, EnergyMeters(1).MNAccRptNum);
        EXPECT_EQ(11, EnergyMeters(1).YRAccRptNum);
        EXPECT_EQ(12, EnergyMeters(1).SMAccRptNum);

        EXPECT_EQ(1, NumEnergyMeters);
        EXPECT_EQ(1ul, EnergyMeters.size());

        EnergyPlus::sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        auto const name2("testMeter2");
        OutputProcessor::Unit const units2(OutputProcessor::Unit::unknown); // was "kwh"
        auto const resourceType2("OTHER");
        auto const endUse2("testEndUse2");
        auto const endUseSub2("testEndUseSub2");
        auto const group2("testGroup2");
        AddMeter(name2, units2, resourceType2, endUse2, endUseSub2, group2);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(1ul, errorData.size());
        std::vector<std::string> errorData0{"1", "1", "0",
                                            "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[unknown].  ..on "
                                            "Meter=\"testMeter2\".  ..requests for IP units from this meter will be ignored.",
                                            "1"};
        EXPECT_EQ(errorData0, errorData[0]);

        ASSERT_EQ(2, NumEnergyMeters);
        ASSERT_EQ(2ul, EnergyMeters.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_validateNStandardizeMeterTitles)
    {
        std::vector<std::vector<std::string>> input_map = {{"J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "SYSTEM"},
                                                           {"J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "PLANT"},
                                                           {"J", "ELEC", "INTERIOR LIGHTS", "endUseSub", "BUILDING", "zoneName"},
                                                           {"J", "ELEC", "INTERIORLIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "EXTERIOR LIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "EXTERIORLIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HTG", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATPRODUCED", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "CLG", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "DOMESTICHOTWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "DHW", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "DOMESTIC HOT WATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COGEN", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COGENERATION", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "INTERIOREQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "INTERIOR EQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "EXTERIOREQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "EXTERIOR EQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "EXT EQ", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "EXTERIOREQ", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "EXTERIOR:WATEREQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASEDHOTWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "DISTRICTHOTWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASED HEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASEDCOLDWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "DISTRICTCHILLEDWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASEDCHILLEDWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASED COLD WATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASED COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "FANS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "FAN", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATINGCOILS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATINGCOIL", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATING COILS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATING COIL", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COOLINGCOILS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COOLINGCOIL", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COOLING COILS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COOLING COIL", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PUMPS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PUMP", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "FREECOOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "FREE COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "LOOPTOLOOP", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "CHILLERS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "CHILLER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "BOILERS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "BOILER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "BASEBOARD", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "BASEBOARDS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATREJECTION", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEAT REJECTION", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HUMIDIFIER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HUMIDIFIERS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATRECOVERY", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEAT RECOVERY", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PHOTOVOLTAICS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PV", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PHOTOVOLTAIC", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "WINDTURBINES", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "WT", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "WINDTURBINE", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "ELECTRICSTORAGE", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEAT RECOVERY FOR COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATRECOVERYFORCOOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATRECOVERYCOOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEAT RECOVERY FOR HEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATRECOVERYFORHEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "HEATRECOVERYHEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "ELECTRICEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASEDELECTRICEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "SOLDELECTRICEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "NATURALGASEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "FUELOIL#1EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "FUELOIL#2EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COALEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "GASOLINEEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PROPANEEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "DIESELEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "OTHERFUEL1EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "OTHERFUEL2EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "CARBONEQUIVALENTEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "REFRIGERATION", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COLDSTORAGECHARGE", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "COLDSTORAGEDISCHARGE", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "WATERSYSTEMS", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "WATERSYSTEM", "endUseSub", "HVAC"},
                                                           // { "J", "ELEC", "Water System", "endUseSub", "HVAC" },  // This one fails because Water
                                                           // System isn't a proper choice (needs to be upper cased in code...)
                                                           {"J", "ELEC", "RAINWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "CONDENSATE", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "WELLWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "MAINSWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELEC", "PURCHASEDWATER", "endUseSub", "HVAC"}};

        std::vector<std::string> const result_map = {"Electricity:Facility",
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
                                                     "endUseSub:MainsWater:Electricity"};

        bool errorFound = false;
        for (auto &meter : input_map) {
            errorFound = false;
            if (meter.size() == 5) {
                ValidateNStandardizeMeterTitles(OutputProcessor::Unit::J, meter[1], meter[2], meter[3], meter[4],
                                                errorFound); // the first argument was  meter[ 0 ]
            } else if (meter.size() == 6) {
                ValidateNStandardizeMeterTitles(OutputProcessor::Unit::J, meter[1], meter[2], meter[3], meter[4], errorFound,
                                                meter[5]); // the first argument was  meter[ 0 ]
            }
            EXPECT_FALSE(errorFound);
        }

        ASSERT_EQ(102, NumEnergyMeters);
        ASSERT_EQ(102ul, EnergyMeters.size());

        for (int i = 0; i < NumEnergyMeters; ++i) {
            EXPECT_EQ(result_map[i], EnergyMeters(i + 1).Name);
        }

        EnergyPlus::sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        OutputProcessor::Unit units = OutputProcessor::Unit::J;
        std::string resourceType = "ELEC";
        std::string endUse = "INTERIOR LIGHTS";
        std::string endUseSub = "endUseSub";
        std::string group = "BAD INPUT";
        errorFound = false;

        ValidateNStandardizeMeterTitles(units, resourceType, endUse, endUseSub, group, errorFound);
        EXPECT_TRUE(errorFound);

        units = OutputProcessor::Unit::J;
        resourceType = "ELEC";
        endUse = "BAD INPUT";
        endUseSub = "endUseSub";
        group = "HVAC";
        errorFound = false;

        ValidateNStandardizeMeterTitles(units, resourceType, endUse, endUseSub, group, errorFound);
        EXPECT_TRUE(errorFound);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(2ul, errorData.size());
        std::vector<std::string> errorData0{"1", "1", "1", "Illegal Group (for Meters) Entered=BAD INPUT", "1"};
        std::vector<std::string> errorData1{"2", "1", "1", "Illegal EndUse (for Meters) Entered=BAD INPUT", "1"};
        EXPECT_EQ(errorData0, errorData[0]);
        EXPECT_EQ(errorData1, errorData[1]);
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupTimePointers)
    {
        TimeValue.allocate(2);

        auto timeStep = 1.0;

        SetupTimePointers("Zone", timeStep);

        EXPECT_DOUBLE_EQ(timeStep, TimeValue(1).TimeStep);
        EXPECT_DOUBLE_EQ(0.0, TimeValue(1).CurMinute);

        timeStep = 2.0;

        SetupTimePointers("HVAC", timeStep);

        EXPECT_DOUBLE_EQ(timeStep, TimeValue(2).TimeStep);
        EXPECT_DOUBLE_EQ(0.0, TimeValue(2).CurMinute);
    }

    TEST_F(SQLiteFixture, OutputProcessor_getReportVariableInput)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();

        NumOfReqVariables = inputProcessor->getNumObjectsFound("Output:Variable");

        EXPECT_EQ(5, NumOfReqVariables);

        EXPECT_EQ("", ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(1).VarName);
        EXPECT_EQ(ReportingFrequency::TimeStep, ReqRepVars(1).frequency);
        EXPECT_EQ(0, ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", ReqRepVars(1).SchedName);
        EXPECT_FALSE(ReqRepVars(1).Used);

        EXPECT_EQ("", ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(2).VarName);
        EXPECT_EQ(ReportingFrequency::Hourly, ReqRepVars(2).frequency);
        EXPECT_EQ(0, ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", ReqRepVars(2).SchedName);
        EXPECT_FALSE(ReqRepVars(2).Used);

        EXPECT_EQ("", ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(3).VarName);
        EXPECT_EQ(ReportingFrequency::Daily, ReqRepVars(3).frequency);
        EXPECT_EQ(0, ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", ReqRepVars(3).SchedName);
        EXPECT_FALSE(ReqRepVars(3).Used);

        EXPECT_EQ("", ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(4).VarName);
        EXPECT_EQ(ReportingFrequency::Monthly, ReqRepVars(4).frequency);
        EXPECT_EQ(0, ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", ReqRepVars(4).SchedName);
        EXPECT_FALSE(ReqRepVars(4).Used);

        EXPECT_EQ("", ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(5).VarName);
        EXPECT_EQ(ReportingFrequency::Simulation, ReqRepVars(5).frequency);
        EXPECT_EQ(0, ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", ReqRepVars(5).SchedName);
        EXPECT_FALSE(ReqRepVars(5).Used);
    }

    TEST_F(SQLiteFixture, OutputProcessor_buildKeyVarList)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();

        auto const keyed_value = "ENVIRONMENT";
        auto const var_name = "SITE OUTDOOR AIR DRYBULB TEMPERATURE";

        BuildKeyVarList(keyed_value, var_name, 1, 6);

        EXPECT_EQ(0, NumExtraVars);
        EXPECT_EQ(6, NumOfReqVariables);

        EXPECT_EQ("", ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(1).VarName);
        EXPECT_EQ(ReportingFrequency::TimeStep, ReqRepVars(1).frequency);
        EXPECT_EQ(0, ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", ReqRepVars(1).SchedName);
        EXPECT_FALSE(ReqRepVars(1).Used);

        EXPECT_EQ("", ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(2).VarName);
        EXPECT_EQ(ReportingFrequency::Hourly, ReqRepVars(2).frequency);
        EXPECT_EQ(0, ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", ReqRepVars(2).SchedName);
        EXPECT_FALSE(ReqRepVars(2).Used);

        EXPECT_EQ("", ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(3).VarName);
        EXPECT_EQ(ReportingFrequency::Daily, ReqRepVars(3).frequency);
        EXPECT_EQ(0, ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", ReqRepVars(3).SchedName);
        EXPECT_FALSE(ReqRepVars(3).Used);

        EXPECT_EQ("", ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(4).VarName);
        EXPECT_EQ(ReportingFrequency::Monthly, ReqRepVars(4).frequency);
        EXPECT_EQ(0, ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", ReqRepVars(4).SchedName);
        EXPECT_FALSE(ReqRepVars(4).Used);

        EXPECT_EQ("", ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(5).VarName);
        EXPECT_EQ(ReportingFrequency::Simulation, ReqRepVars(5).frequency);
        EXPECT_EQ(0, ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", ReqRepVars(5).SchedName);
        EXPECT_FALSE(ReqRepVars(5).Used);
    }

    TEST_F(SQLiteFixture, OutputProcessor_buildKeyVarListWithKey)
    {
        std::string const idf_objects = delimited_string({
            "  Output:Table:Monthly,",
            "    Test Report,  !- Name",
            "    2,                       !- Digits After Decimal",
            "    Zone Total Internal Latent Gain Rate,  !- Variable or Meter 1 Name",
            "    SumOrAverage;            !- Aggregation Type for Variable or Meter 1",
            "",
            "Output:Variable,Living,Zone Total Internal Latent Gain Rate,Hourly;",
            "Output:Variable,Attic,Zone Total Internal Latent Gain Rate,Hourly;",
            "",
            "Output:Variable,Living,Zone Total Internal Sensible Gain Rate,Hourly;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        inputProcessor->preScanReportingVariables();
        InitializeOutput();

        Real64 ilgrGarage;
        Real64 ilgrLiving;
        Real64 ilgrAttic;

        SetupOutputVariable("Zone Total Internal Latent Gain Rate", OutputProcessor::Unit::J, ilgrGarage, "Zone", "Sum", "Garage");
        SetupOutputVariable("Zone Total Internal Latent Gain Rate", OutputProcessor::Unit::J, ilgrLiving, "Zone", "Sum", "Living");
        SetupOutputVariable("Zone Total Internal Latent Gain Rate", OutputProcessor::Unit::J, ilgrAttic, "Zone", "Sum", "Attic");

        Real64 isgrGarage;
        Real64 isgrLiving;
        Real64 isgrAttic;

        SetupOutputVariable("Zone Total Internal Sensible Gain Rate", OutputProcessor::Unit::J, isgrGarage, "Zone", "Sum", "Garage");
        SetupOutputVariable("Zone Total Internal Sensible Gain Rate", OutputProcessor::Unit::J, isgrLiving, "Zone", "Sum", "Living");
        SetupOutputVariable("Zone Total Internal Sensible Gain Rate", OutputProcessor::Unit::J, isgrAttic, "Zone", "Sum", "Attic");

        DataGlobals::DoWeathSim = true;
        DataGlobals::TimeStepZone = 0.25;

        OutputReportTabular::GetInputTabularMonthly();
        EXPECT_EQ(OutputReportTabular::MonthlyInputCount, 1);
        OutputReportTabular::InitializeTabularMonthly();

        GetReportVariableInput();

        NumExtraVars = 0;
        BuildKeyVarList("LIVING", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 3);
        EXPECT_EQ(1, NumExtraVars);

        NumExtraVars = 0;
        BuildKeyVarList("GARAGE", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 3);
        EXPECT_EQ(0, NumExtraVars);

        NumExtraVars = 0;
        BuildKeyVarList("ATTIC", "ZONE TOTAL INTERNAL SENSIBLE GAIN RATE", 1, 3);
        EXPECT_EQ(0, NumExtraVars);
    }

    TEST_F(SQLiteFixture, OutputProcessor_buildKeyVarListWithRegexKey)
    {
        std::string const idf_objects = delimited_string({
            "  Output:Table:Monthly,",
            "    Test Report,  !- Name",
            "    2,                       !- Digits After Decimal",
            "    Zone Total Internal Latent Gain Rate,  !- Variable or Meter 1 Name",
            "    SumOrAverage;            !- Aggregation Type for Variable or Meter 1",
            "",
            "Output:Variable,Liv.*,Zone Total Internal Latent Gain Rate,Hourly;",
            "",
            "Output:Variable,Living,Zone Total Internal Sensible Gain Rate,Hourly;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        inputProcessor->preScanReportingVariables();
        InitializeOutput();

        Real64 ilgrGarage;
        Real64 ilgrLiving1;
        Real64 ilgrLiving2;

        SetupOutputVariable("Zone Total Internal Latent Gain Rate", OutputProcessor::Unit::J, ilgrGarage, "Zone", "Sum", "Garage");
        SetupOutputVariable("Zone Total Internal Latent Gain Rate", OutputProcessor::Unit::J, ilgrLiving1, "Zone", "Sum", "Living1");
        SetupOutputVariable("Zone Total Internal Latent Gain Rate", OutputProcessor::Unit::J, ilgrLiving2, "Zone", "Sum", "Living2");

        Real64 isgrGarage;
        Real64 isgrLiving;
        Real64 isgrAttic;

        SetupOutputVariable("Zone Total Internal Sensible Gain Rate", OutputProcessor::Unit::J, isgrGarage, "Zone", "Sum", "Garage");
        SetupOutputVariable("Zone Total Internal Sensible Gain Rate", OutputProcessor::Unit::J, isgrLiving, "Zone", "Sum", "Living1");
        SetupOutputVariable("Zone Total Internal Sensible Gain Rate", OutputProcessor::Unit::J, isgrAttic, "Zone", "Sum", "Living2");

        DataGlobals::DoWeathSim = true;
        DataGlobals::TimeStepZone = 0.25;

        OutputReportTabular::GetInputTabularMonthly();
        EXPECT_EQ(OutputReportTabular::MonthlyInputCount, 1);
        OutputReportTabular::InitializeTabularMonthly();

        GetReportVariableInput();

        NumExtraVars = 0;
        BuildKeyVarList("LIVING1", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 2);
        EXPECT_EQ(1, NumExtraVars);

        NumExtraVars = 0;
        BuildKeyVarList("GARAGE", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 2);
        EXPECT_EQ(0, NumExtraVars);
    }

    TEST_F(SQLiteFixture, OutputProcessor_addBlankKeys)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        InitializeOutput();

        GetReportVariableInput();

        auto const var_name = "Site Outdoor Air Drybulb Temperature";

        AddBlankKeys(var_name, 1, 5);

        EXPECT_EQ(5, NumExtraVars);
        EXPECT_EQ(1, ReportList(1));
        EXPECT_EQ(2, ReportList(2));
        EXPECT_EQ(3, ReportList(3));
        EXPECT_EQ(4, ReportList(4));
        EXPECT_EQ(5, ReportList(5));
        EXPECT_EQ(5, NumOfReqVariables);

        EXPECT_EQ("", ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(1).VarName);
        EXPECT_EQ(ReportingFrequency::TimeStep, ReqRepVars(1).frequency);
        EXPECT_EQ(0, ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", ReqRepVars(1).SchedName);
        EXPECT_FALSE(ReqRepVars(1).Used);

        EXPECT_EQ("", ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(2).VarName);
        EXPECT_EQ(ReportingFrequency::Hourly, ReqRepVars(2).frequency);
        EXPECT_EQ(0, ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", ReqRepVars(2).SchedName);
        EXPECT_FALSE(ReqRepVars(2).Used);

        EXPECT_EQ("", ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(3).VarName);
        EXPECT_EQ(ReportingFrequency::Daily, ReqRepVars(3).frequency);
        EXPECT_EQ(0, ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", ReqRepVars(3).SchedName);
        EXPECT_FALSE(ReqRepVars(3).Used);

        EXPECT_EQ("", ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(4).VarName);
        EXPECT_EQ(ReportingFrequency::Monthly, ReqRepVars(4).frequency);
        EXPECT_EQ(0, ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", ReqRepVars(4).SchedName);
        EXPECT_FALSE(ReqRepVars(4).Used);

        EXPECT_EQ("", ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(5).VarName);
        EXPECT_EQ(ReportingFrequency::Simulation, ReqRepVars(5).frequency);
        EXPECT_EQ(0, ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", ReqRepVars(5).SchedName);
        EXPECT_FALSE(ReqRepVars(5).Used);
    }

    TEST_F(SQLiteFixture, OutputProcessor_determineFrequency)
    {
        auto const valid_options = std::map<std::string, ReportingFrequency>({{"Detailed", ReportingFrequency::EachCall},
                                                                              {"Timestep", ReportingFrequency::TimeStep},
                                                                              {"Hourly", ReportingFrequency::Hourly},
                                                                              {"Daily", ReportingFrequency::Daily},
                                                                              {"Monthly", ReportingFrequency::Monthly},
                                                                              {"RunPeriod", ReportingFrequency::Simulation},
                                                                              {"Environment", ReportingFrequency::Simulation},
                                                                              {"Annual", ReportingFrequency::Yearly},
                                                                              {"Bad Input", ReportingFrequency::Hourly}});

        ReportingFrequency report_freq = ReportingFrequency::EachCall;

        for (auto const option : valid_options) {
            report_freq = determineFrequency(option.first);
            EXPECT_EQ(option.second, report_freq);
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_addToOutputVariableList)
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

        ASSERT_TRUE(process_idf(idf_objects));

        AddToOutputVariableList("Site Outdoor Air Drybulb Temperature", 1, StoreType::Averaged, 2, OutputProcessor::Unit::C);
        AddToOutputVariableList("Site Outdoor Air Wetbulb Temperature", 1, StoreType::Averaged, 2, OutputProcessor::Unit::C);
        AddToOutputVariableList("Site Outdoor Air Humidity Ratio", 1, StoreType::Averaged, 2, OutputProcessor::Unit::kgWater_kgDryAir);
        AddToOutputVariableList("Site Outdoor Air Relative Humidity", 1, StoreType::Averaged, 2, OutputProcessor::Unit::Perc);

        EXPECT_EQ(1, DDVariableTypes(1).IndexType);
        EXPECT_EQ(StoreType::Averaged, DDVariableTypes(1).storeType);
        EXPECT_EQ(2, DDVariableTypes(1).VariableType);
        EXPECT_EQ(0, DDVariableTypes(1).Next);
        EXPECT_FALSE(DDVariableTypes(1).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Drybulb Temperature", DDVariableTypes(1).VarNameOnly);
        EXPECT_EQ(OutputProcessor::Unit::C, DDVariableTypes(1).units);

        EXPECT_EQ(1, DDVariableTypes(2).IndexType);
        EXPECT_EQ(StoreType::Averaged, DDVariableTypes(2).storeType);
        EXPECT_EQ(2, DDVariableTypes(2).VariableType);
        EXPECT_EQ(0, DDVariableTypes(2).Next);
        EXPECT_FALSE(DDVariableTypes(2).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Wetbulb Temperature", DDVariableTypes(2).VarNameOnly);
        EXPECT_EQ(OutputProcessor::Unit::C, DDVariableTypes(2).units);

        EXPECT_EQ(1, DDVariableTypes(3).IndexType);
        EXPECT_EQ(StoreType::Averaged, DDVariableTypes(3).storeType);
        EXPECT_EQ(2, DDVariableTypes(3).VariableType);
        EXPECT_EQ(0, DDVariableTypes(3).Next);
        EXPECT_FALSE(DDVariableTypes(3).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Humidity Ratio", DDVariableTypes(3).VarNameOnly);
        EXPECT_EQ(OutputProcessor::Unit::kgWater_kgDryAir, DDVariableTypes(3).units);

        EXPECT_EQ(1, DDVariableTypes(4).IndexType);
        EXPECT_EQ(StoreType::Averaged, DDVariableTypes(4).storeType);
        EXPECT_EQ(2, DDVariableTypes(4).VariableType);
        EXPECT_EQ(0, DDVariableTypes(4).Next);
        EXPECT_FALSE(DDVariableTypes(4).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Relative Humidity", DDVariableTypes(4).VarNameOnly);
        EXPECT_EQ(OutputProcessor::Unit::Perc, DDVariableTypes(4).units);
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();
        SetupOutputVariable("Site Outdoor Air Drybulb Temperature", OutputProcessor::Unit::C, DataEnvironment::OutDryBulbTemp, "Zone", "Average",
                            "Environment");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary(
            {{"1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"}});
        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        EXPECT_EQ(1, NumExtraVars);

        EXPECT_EQ("", ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(1).VarName);
        EXPECT_EQ(ReportingFrequency::Simulation, ReqRepVars(1).frequency);
        EXPECT_EQ(0, ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", ReqRepVars(1).SchedName);
        EXPECT_EQ(true, ReqRepVars(1).Used);

        EXPECT_EQ(1, DDVariableTypes(1).IndexType);
        EXPECT_EQ(StoreType::Averaged, DDVariableTypes(1).storeType);
        EXPECT_EQ(2, DDVariableTypes(1).VariableType);
        EXPECT_EQ(0, DDVariableTypes(1).Next);
        EXPECT_FALSE(DDVariableTypes(1).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Drybulb Temperature", DDVariableTypes(1).VarNameOnly);
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_star)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,*,Boiler Gas Rate,runperiod;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();
        Real64 fuel_used = 999;
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler1");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler2");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler3");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Gas Rate", "Run Period", "", "W"},
            {"2", "0", "Avg", "System", "Zone", "Boiler2", "Boiler Gas Rate", "Run Period", "", "W"},
            {"3", "0", "Avg", "System", "Zone", "Boiler3", "Boiler Gas Rate", "Run Period", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(
            delimited_string({"1,11,Boiler1,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "2,11,Boiler2,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "3,11,Boiler3,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,Boiler[13],Boiler Gas Rate,runperiod;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();
        Real64 fuel_used = 999;
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler1");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler2");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler3");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Gas Rate", "Run Period", "", "W"},
            {"2", "0", "Avg", "System", "Zone", "Boiler3", "Boiler Gas Rate", "Run Period", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(
            delimited_string({"1,11,Boiler1,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "2,11,Boiler3,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex_2)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,Boiler.*,Boiler Gas Rate,runperiod;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();
        Real64 fuel_used = 999;
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler1");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler2");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler3");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Gas Rate", "Run Period", "", "W"},
            {"2", "0", "Avg", "System", "Zone", "Boiler2", "Boiler Gas Rate", "Run Period", "", "W"},
            {"3", "0", "Avg", "System", "Zone", "Boiler3", "Boiler Gas Rate", "Run Period", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(
            delimited_string({"1,11,Boiler1,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "2,11,Boiler2,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "3,11,Boiler3,Boiler Gas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex_3)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,Zn003:Wall.*,AFN Linkage Node 1 to Node 2 Volume Flow Rate,timestep;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();
        Real64 vol_flow = 999;
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "Zn003:Wall001");
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "Zn003:Wall002");
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "Zn003:Wall002:Win001");
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "Zn003:Wall003");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "Zone", "Zn003:Wall001", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"2", "0", "Avg", "System", "Zone", "Zn003:Wall002", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"3", "0", "Avg", "System", "Zone", "Zn003:Wall002:Win001", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"4", "0", "Avg", "System", "Zone", "Zn003:Wall003", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(delimited_string({
            "1,1,Zn003:Wall001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            "2,1,Zn003:Wall002,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            "3,1,Zn003:Wall002:Win001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            "4,1,Zn003:Wall003,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
        }));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex_4)
    {
        // case-insensitive comparison
        std::string const idf_objects =
            delimited_string({"Output:Variable,(?i)Zn003:Wall.*,AFN Linkage Node 1 to Node 2 Volume Flow Rate,timestep;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput();
        Real64 vol_flow = 999;
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "ZN003:WALL001");
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "ZN003:WALL002");
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "ZN003:WALL002:WIN001");
        SetupOutputVariable("AFN Linkage Node 1 to Node 2 Volume Flow Rate", OutputProcessor::Unit::m3_s, vol_flow, "System", "Average",
                            "ZN003:WALL003");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "Zone", "ZN003:WALL001", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"2", "0", "Avg", "System", "Zone", "ZN003:WALL002", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"3", "0", "Avg", "System", "Zone", "ZN003:WALL002:WIN001", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"4", "0", "Avg", "System", "Zone", "ZN003:WALL003", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(delimited_string({
            "1,1,ZN003:WALL001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            "2,1,ZN003:WALL002,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            "3,1,ZN003:WALL002:WIN001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            "4,1,ZN003:WALL003,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
        }));
    }

    TEST_F(SQLiteFixture, OutputProcessor_checkReportVariable)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,hourly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,daily;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,monthly;",
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        auto const keyed_value = "Environment";
        auto const var_name = "Site Outdoor Air Drybulb Temperature";

        InitializeOutput();

        GetReportVariableInput();
        CheckReportVariable(keyed_value, var_name);

        EXPECT_EQ(5, NumOfReqVariables);

        EXPECT_EQ("", ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(1).VarName);
        EXPECT_EQ(ReportingFrequency::TimeStep, ReqRepVars(1).frequency);
        EXPECT_EQ(0, ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", ReqRepVars(1).SchedName);
        EXPECT_EQ(true, ReqRepVars(1).Used);

        EXPECT_EQ("", ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(2).VarName);
        EXPECT_EQ(ReportingFrequency::Hourly, ReqRepVars(2).frequency);
        EXPECT_EQ(0, ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", ReqRepVars(2).SchedName);
        EXPECT_EQ(true, ReqRepVars(2).Used);

        EXPECT_EQ("", ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(3).VarName);
        EXPECT_EQ(ReportingFrequency::Daily, ReqRepVars(3).frequency);
        EXPECT_EQ(0, ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", ReqRepVars(3).SchedName);
        EXPECT_EQ(true, ReqRepVars(3).Used);

        EXPECT_EQ("", ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(4).VarName);
        EXPECT_EQ(ReportingFrequency::Monthly, ReqRepVars(4).frequency);
        EXPECT_EQ(0, ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", ReqRepVars(4).SchedName);
        EXPECT_EQ(true, ReqRepVars(4).Used);

        EXPECT_EQ("", ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", ReqRepVars(5).VarName);
        EXPECT_EQ(ReportingFrequency::Simulation, ReqRepVars(5).frequency);
        EXPECT_EQ(0, ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", ReqRepVars(5).SchedName);
        EXPECT_EQ(true, ReqRepVars(5).Used);
    }

    TEST_F(SQLiteFixture, OutputProcessor_getCustomMeterInput)
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

        ASSERT_TRUE(process_idf(idf_objects));

        Real64 light_consumption = 0;
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE1-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE1-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE2-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE2-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE3-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE3-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE4-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE4-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE5-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE5-1", 1, 1);
        Real64 zone_infil_total_loss = 0;
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE1-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE2-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE3-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE4-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE5-1");

        bool errors_found = false;

        GetCustomMeterInput(errors_found);

        ASSERT_FALSE(errors_found);

        ASSERT_EQ(17, NumEnergyMeters);

        auto const meters_result = std::map<int, std::tuple<int, std::string, std::string, std::string, std::string, std::string, std::string>>({
            {1, std::make_tuple(0, "Electricity:Facility", "Electricity", "", "", "", "J")},
            {2, std::make_tuple(0, "Electricity:Building", "Electricity", "", "", "Building", "J")},
            {3, std::make_tuple(0, "Electricity:Zone:SPACE1-1", "Electricity", "", "", "Zone", "J")},
            {4, std::make_tuple(0, "InteriorLights:Electricity", "Electricity", "InteriorLights", "", "", "J")},
            {5, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "", "Zone", "J")},
            {6, std::make_tuple(0, "GeneralLights:InteriorLights:Electricity", "Electricity", "InteriorLights", "GeneralLights", "", "J")},
            {7, std::make_tuple(0, "Electricity:Zone:SPACE2-1", "Electricity", "", "", "Zone", "J")},
            {8, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE2-1", "Electricity", "InteriorLights", "", "Zone", "J")},
            {9, std::make_tuple(0, "Electricity:Zone:SPACE3-1", "Electricity", "", "", "Zone", "J")},
            {10, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE3-1", "Electricity", "InteriorLights", "", "Zone", "J")},
            {11, std::make_tuple(0, "Electricity:Zone:SPACE4-1", "Electricity", "", "", "Zone", "J")},
            {12, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE4-1", "Electricity", "InteriorLights", "", "Zone", "J")},
            {13, std::make_tuple(0, "Electricity:Zone:SPACE5-1", "Electricity", "", "", "Zone", "J")},
            {14, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE5-1", "Electricity", "InteriorLights", "", "Zone", "J")},
            {15, std::make_tuple(1, "MYGENERALLIGHTS", "Electricity", "", "", "", "J")},
            {16, std::make_tuple(1, "BUILDING INFILTRATION HEAT LOSS", "Generic", "", "", "", "J")},
            {17, std::make_tuple(2, "MYBUILDINGOTHER", "Electricity", "", "", "", "J")},
        });

        for (auto const result : meters_result) {
            EXPECT_EQ(std::get<0>(result.second), EnergyMeters(result.first).TypeOfMeter);
            EXPECT_EQ(std::get<1>(result.second), EnergyMeters(result.first).Name);
            EXPECT_EQ(std::get<2>(result.second), EnergyMeters(result.first).ResourceType);
            EXPECT_EQ(std::get<3>(result.second), EnergyMeters(result.first).EndUse);
            EXPECT_EQ(std::get<4>(result.second), EnergyMeters(result.first).EndUseSub);
            EXPECT_EQ(std::get<5>(result.second), EnergyMeters(result.first).Group);
            EXPECT_EQ(std::get<6>(result.second), unitEnumToString(EnergyMeters(result.first).Units));
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_attachMeters)
    {
        std::string const idf_objects = delimited_string({
            "Output:Meter,Electricity:Facility,timestep;",
            "Output:Meter,Electricity:Facility,hourly;",
            "Output:Meter,Electricity:Facility,daily;",
            "Output:Meter,Electricity:Facility,monthly;",
            "Output:Meter,Electricity:Facility,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        InitializeOutput();

        int meter_array_ptr = -1;
        bool errors_found = false;

        std::string resourceType("Electricity");
        std::string endUse("InteriorLights");
        std::string endUseSub("GeneralLights");
        std::string group("Building");
        std::string const zoneName("SPACE1-1");

        AttachMeters(OutputProcessor::Unit::J, resourceType, endUse, endUseSub, group, zoneName, 1, meter_array_ptr, errors_found);

        EXPECT_FALSE(errors_found);
        EXPECT_EQ(1, meter_array_ptr);

        ASSERT_EQ(6, NumEnergyMeters);

        auto const meters_result = std::map<int, std::tuple<int, std::string, std::string, std::string, std::string, std::string, std::string>>({
            {1, std::make_tuple(0, "Electricity:Facility", "Electricity", "", "", "", "J")},
            {2, std::make_tuple(0, "Electricity:Building", "Electricity", "", "", "Building", "J")},
            {3, std::make_tuple(0, "Electricity:Zone:SPACE1-1", "Electricity", "", "", "Zone", "J")},
            {4, std::make_tuple(0, "InteriorLights:Electricity", "Electricity", "InteriorLights", "", "", "J")},
            {5, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "", "Zone", "J")},
            {6, std::make_tuple(0, "GeneralLights:InteriorLights:Electricity", "Electricity", "InteriorLights", "GeneralLights", "", "J")},
        });

        for (auto const result : meters_result) {
            EXPECT_EQ(std::get<0>(result.second), EnergyMeters(result.first).TypeOfMeter);
            EXPECT_EQ(std::get<1>(result.second), EnergyMeters(result.first).Name);
            EXPECT_EQ(std::get<2>(result.second), EnergyMeters(result.first).ResourceType);
            EXPECT_EQ(std::get<3>(result.second), EnergyMeters(result.first).EndUse);
            EXPECT_EQ(std::get<4>(result.second), EnergyMeters(result.first).EndUseSub);
            EXPECT_EQ(std::get<5>(result.second), EnergyMeters(result.first).Group);
            EXPECT_EQ(std::get<6>(result.second), unitEnumToString(EnergyMeters(result.first).Units));
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_updateDataandReport_ZoneTSReporting)
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

        ASSERT_TRUE(process_idf(idf_objects));

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

        if (DataGlobals::TimeStep == DataGlobals::NumOfTimeStepInHour) {
            DataGlobals::EndHourFlag = true;
            if (DataGlobals::HourOfDay == 24) {
                DataGlobals::EndDayFlag = true;
                if ((!DataGlobals::WarmupFlag) && (DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn)) {
                    DataGlobals::EndEnvrnFlag = true;
                }
            }
        }

        if (DataEnvironment::DayOfMonth == WeatherManager::EndDayOfMonth(DataEnvironment::Month)) {
            DataEnvironment::EndMonthFlag = true;
        }

        TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers("Zone", timeStep);
        SetupTimePointers("HVAC", timeStep);

        TimeValue(1).CurMinute = 50;
        TimeValue(2).CurMinute = 50;

        GetReportVariableInput();
        SetupOutputVariable("Site Outdoor Air Drybulb Temperature", OutputProcessor::Unit::C, DataEnvironment::OutDryBulbTemp, "Zone", "Average",
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE1-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE1-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE2-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE2-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE3-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE3-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE4-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE4-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE5-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE5-1", 1, 1);
        Real64 zone_infil_total_loss = 999;
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE1-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE2-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE3-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE4-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE5-1");

        UpdateMeterReporting();

        UpdateDataandReport(DataGlobals::ZoneTSReporting);

        auto timeResults = queryResult("SELECT * FROM Time;", "Time");

        std::vector<std::vector<std::string>> timeData({
            {"1", "0", "12", "31", "24", "0", "0", "10", "-1", "365", "Tuesday", "0", "0"},
            {"2", "0", "12", "31", "24", "0", "0", "60", "1", "365", "Tuesday", "0", "0"},
            {"3", "0", "12", "31", "24", "0", "0", "1440", "2", "365", "Tuesday", "0", "0"},
            {"4", "0", "12", "31", "24", "0", "", "44640", "3", "365", "", "0", "0"},
            {"5", "", "", "", "", "", "", "525600", "4", "365", "", "0", "0"},
        });

        EXPECT_EQ(timeData, timeResults);

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C"},
            {"2", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"},
            {"3", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C"},
            {"4", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C"},
            {"5", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"},
            {"7", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Zone Timestep", "", "J"},
            {"8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Hourly", "", "J"},
            {"9", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Daily", "", "J"},
            {"10", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Monthly", "", "J"},
            {"12", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Run Period", "", "J"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({
            {"1", "1", "1", "0.0"},
            {"2", "1", "7", "4995.0"},
            {"3", "2", "2", "0.0"},
            {"4", "2", "8", "4995.0"},
            {"5", "3", "3", "0.0"},
            {"6", "3", "9", "4995.0"},
            {"7", "4", "4", "0.0"},
            {"8", "4", "10", "4995.0"},
            {"9", "5", "5", "0.0"},
            {"10", "5", "12", "4995.0"},
        });

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "5", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0"},
             {"2", "6", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0"},
             {"3", "7", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0"},
             {"4", "8", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0"},
             {"5", "9", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0"},
             {"6", "10", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);

        compare_eso_stream(delimited_string({
            "1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
            "2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
            "3,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "4,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "5,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            "7,1,Electricity:Facility [J] !TimeStep",
            "8,1,Electricity:Facility [J] !Hourly",
            "9,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "10,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "12,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
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
            "12,4995.0,4995.0,12,31,24,60,4995.0,12,31,24,60",
        }));

        compare_mtr_stream(delimited_string({
            "7,1,Electricity:Facility [J] !TimeStep",
            "8,1,Electricity:Facility [J] !Hourly",
            "9,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "10,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "12,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            ",365,12,31, 0,24,50.00,60.00,Tuesday",
            "7,4995.0",
            ",365,12,31, 0,24, 0.00,60.00,Tuesday",
            "8,4995.0",
            ",365,12,31, 0,Tuesday",
            "9,4995.0,4995.0,24,60,4995.0,24,60",
            ",365,12",
            "10,4995.0,4995.0,31,24,60,4995.0,31,24,60",
            ",365",
            "12,4995.0,4995.0,12,31,24,60,4995.0,12,31,24,60",
        }));
    }

    TEST_F(SQLiteFixture, OutputProcessor_updateDataandReport_ZoneTSReporting_with_detailed)
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

        ASSERT_TRUE(process_idf(idf_objects));

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

        if (DataGlobals::TimeStep == DataGlobals::NumOfTimeStepInHour) {
            DataGlobals::EndHourFlag = true;
            if (DataGlobals::HourOfDay == 24) {
                DataGlobals::EndDayFlag = true;
                if ((!DataGlobals::WarmupFlag) && (DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn)) {
                    DataGlobals::EndEnvrnFlag = true;
                }
            }
        }

        if (DataEnvironment::DayOfMonth == WeatherManager::EndDayOfMonth(DataEnvironment::Month)) {
            DataEnvironment::EndMonthFlag = true;
        }

        TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers("Zone", timeStep);
        SetupTimePointers("HVAC", timeStep);

        TimeValue(1).CurMinute = 50;
        TimeValue(2).CurMinute = 50;

        GetReportVariableInput();
        SetupOutputVariable("Site Outdoor Air Drybulb Temperature", OutputProcessor::Unit::C, DataEnvironment::OutDryBulbTemp, "Zone", "Average",
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE1-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE1-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE2-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE2-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE3-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE3-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE4-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE4-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE5-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE5-1", 1, 1);
        Real64 zone_infil_total_loss = 999;
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE1-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE2-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE3-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE4-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE5-1");
        Real64 fuel_used = 999;
        Real64 boiler_load = 999;
        SetupOutputVariable("Boiler Heating Rate", OutputProcessor::Unit::W, boiler_load, "System", "Average", "Boiler1");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler1");

        UpdateMeterReporting();

        UpdateDataandReport(DataGlobals::ZoneTSReporting);

        auto timeResults = queryResult("SELECT * FROM Time;", "Time");

        std::vector<std::vector<std::string>> timeData({
            {"1", "0", "12", "31", "24", "0", "0", "10", "-1", "365", "Tuesday", "0", "0"},
            {"2", "0", "12", "31", "24", "0", "0", "60", "1", "365", "Tuesday", "0", "0"},
            {"3", "0", "12", "31", "24", "0", "0", "1440", "2", "365", "Tuesday", "0", "0"},
            {"4", "0", "12", "31", "24", "0", "", "44640", "3", "365", "", "0", "0"},
            {"5", "", "", "", "", "", "", "525600", "4", "365", "", "0", "0"},
        });

        EXPECT_EQ(timeData, timeResults);

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "HVAC System Timestep", "", "C"},
            {"2", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C"},
            {"3", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"},
            {"4", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C"},
            {"5", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C"},
            {"6", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"},
            {"8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "HVAC System Timestep", "", "J"},
            {"9", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Hourly", "", "J"},
            {"10", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Daily", "", "J"},
            {"11", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Monthly", "", "J"},
            {"13", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Run Period", "", "J"},
            {"180", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Heating Rate", "HVAC System Timestep", "", "W"},
            {"181", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Gas Rate", "HVAC System Timestep", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({
            {"1", "1", "1", "0.0"},
            {"2", "1", "2", "0.0"},
            {"3", "1", "8", "4995.0"},
            {"4", "2", "3", "0.0"},
            {"5", "2", "9", "4995.0"},
            {"6", "3", "4", "0.0"},
            {"7", "3", "10", "4995.0"},
            {"8", "4", "5", "0.0"},
            {"9", "4", "11", "4995.0"},
            {"10", "5", "6", "0.0"},
            {"11", "5", "13", "4995.0"},
        });

        std::vector<std::vector<std::string>> reportExtendedData({
            {"1", "6", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0"},
            {"2", "7", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0"},
            {"3", "8", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0"},
            {"4", "9", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0"},
            {"5", "10", "0.0", "12", "31", "24", "", "0", "0.0", "12", "31", "24", "", "0"},
            {"6", "11", "4995.0", "12", "31", "24", "-9", "0", "4995.0", "12", "31", "24", "-9", "0"},
        });

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);

        compare_eso_stream(delimited_string({
            "1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Each Call",
            "2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
            "3,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
            "4,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "5,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "6,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            "180,1,Boiler1,Boiler Heating Rate [W] !Each Call",
            "181,1,Boiler1,Boiler Gas Rate [W] !Each Call",
            "8,1,Electricity:Facility [J] !Each Call",
            "9,1,Electricity:Facility [J] !Hourly",
            "10,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "11,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "13,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            ",365,12,31, 0,24,50.00,60.00,Tuesday",
            "1,0.0",
            "2,0.0",
            "8,4995.0",
            ",365,12,31, 0,24, 0.00,60.00,Tuesday",
            "3,0.0",
            "9,4995.0",
            ",365,12,31, 0,Tuesday",
            "4,0.0,0.0,24,60,0.0,24,60",
            "10,4995.0,4995.0,24,60,4995.0,24,60",
            ",365,12",
            "5,0.0,0.0,31,24,60,0.0,31,24,60",
            "11,4995.0,4995.0,31,24,60,4995.0,31,24,60",
            ",365",
            "6,0.0,0.0,12,31,24,60,0.0,12,31,24,60",
            "13,4995.0,4995.0,12,31,24,60,4995.0,12,31,24,60",
        }));

        compare_mtr_stream(delimited_string({
            "8,1,Electricity:Facility [J] !Each Call",
            "9,1,Electricity:Facility [J] !Hourly",
            "10,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "11,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "13,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            ",365,12,31, 0,24,50.00,60.00,Tuesday",
            "8,4995.0",
            ",365,12,31, 0,24, 0.00,60.00,Tuesday",
            "9,4995.0",
            ",365,12,31, 0,Tuesday",
            "10,4995.0,4995.0,24,60,4995.0,24,60",
            ",365,12",
            "11,4995.0,4995.0,31,24,60,4995.0,31,24,60",
            ",365",
            "13,4995.0,4995.0,12,31,24,60,4995.0,12,31,24,60",
        }));
    }

    TEST_F(SQLiteFixture, OutputProcessor_updateDataandReport_HVACTSReporting)
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

        ASSERT_TRUE(process_idf(idf_objects));

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

        if (DataGlobals::TimeStep == DataGlobals::NumOfTimeStepInHour) {
            DataGlobals::EndHourFlag = true;
            if (DataGlobals::HourOfDay == 24) {
                DataGlobals::EndDayFlag = true;
                if ((!DataGlobals::WarmupFlag) && (DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn)) {
                    DataGlobals::EndEnvrnFlag = true;
                }
            }
        }

        if (DataEnvironment::DayOfMonth == WeatherManager::EndDayOfMonth(DataEnvironment::Month)) {
            DataEnvironment::EndMonthFlag = true;
        }

        TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers("Zone", timeStep);
        SetupTimePointers("HVAC", timeStep);

        TimeValue(1).CurMinute = 50;
        TimeValue(2).CurMinute = 50;

        GetReportVariableInput();
        SetupOutputVariable("Site Outdoor Air Drybulb Temperature", OutputProcessor::Unit::C, DataEnvironment::OutDryBulbTemp, "Zone", "Average",
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE1-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE1-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE2-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE2-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE3-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE3-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE4-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE4-1", 1, 1);
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE5-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE5-1", 1, 1);
        Real64 zone_infil_total_loss = 999;
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE1-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE2-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE3-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE4-1");
        SetupOutputVariable("Zone Infiltration Total Heat Loss Energy", OutputProcessor::Unit::J, zone_infil_total_loss, "System", "Sum", "SPACE5-1");
        Real64 fuel_used = 999;
        Real64 boiler_load = 999;
        SetupOutputVariable("Boiler Heating Rate", OutputProcessor::Unit::W, boiler_load, "System", "Average", "Boiler1");
        SetupOutputVariable("Boiler Gas Rate", OutputProcessor::Unit::W, fuel_used, "System", "Average", "Boiler1");

        UpdateMeterReporting();

        UpdateDataandReport(DataGlobals::HVACTSReporting);

        auto timeResults = queryResult("SELECT * FROM Time;", "Time");

        std::vector<std::vector<std::string>> timeData({
            {"1", "0", "12", "31", "24", "0", "0", "10", "-1", "365", "Tuesday", "0", "0"},
        });

        EXPECT_EQ(timeData, timeResults);

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "HVAC System Timestep", "", "C"},
            {"2", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C"},
            {"3", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"},
            {"4", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C"},
            {"5", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C"},
            {"6", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"},
            {"8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "HVAC System Timestep", "", "J"},
            {"9", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Hourly", "", "J"},
            {"10", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Daily", "", "J"},
            {"11", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Monthly", "", "J"},
            {"13", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Electricity:Facility", "Run Period", "", "J"},
            {"180", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Heating Rate", "HVAC System Timestep", "", "W"},
            {"181", "0", "Avg", "System", "Zone", "Boiler1", "Boiler Gas Rate", "HVAC System Timestep", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({
            {"1", "1", "180", "999.0"},
            {"2", "1", "181", "999.0"},
        });

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);

        compare_eso_stream(delimited_string({
            "1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Each Call",
            "2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
            "3,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
            "4,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "5,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "6,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            "180,1,Boiler1,Boiler Heating Rate [W] !Each Call",
            "181,1,Boiler1,Boiler Gas Rate [W] !Each Call",
            "8,1,Electricity:Facility [J] !Each Call",
            "9,1,Electricity:Facility [J] !Hourly",
            "10,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "11,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "13,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            ",365,12,31, 0,24,50.00,60.00,Tuesday",
            "180,999.0",
            "181,999.0",
        }));

        compare_mtr_stream(delimited_string({
            "8,1,Electricity:Facility [J] !Each Call",
            "9,1,Electricity:Facility [J] !Hourly",
            "10,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
            "11,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
            "13,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
        }));
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_ResetAccumulationWhenWarmupComplete)
    {
        std::string const idf_objects = delimited_string({
            "Version,8.3;",
            "Output:Variable,*,Zone Ideal Loads Supply Air Total Heating Energy,detailed;",
            "Output:Meter:MeterFileOnly,DistrictHeating:HVAC,detailed;",
            "Output:Variable,*,Zone Ideal Loads Supply Air Total Heating Energy,runperiod;",
            "Output:Meter:MeterFileOnly,DistrictHeating:HVAC,hourly;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

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

        if (DataGlobals::TimeStep == DataGlobals::NumOfTimeStepInHour) {
            DataGlobals::EndHourFlag = true;
            if (DataGlobals::HourOfDay == 24) {
                DataGlobals::EndDayFlag = true;
                if ((!DataGlobals::WarmupFlag) && (DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn)) {
                    DataGlobals::EndEnvrnFlag = true;
                }
            }
        }

        if (DataEnvironment::DayOfMonth == WeatherManager::EndDayOfMonth(DataEnvironment::Month)) {
            DataEnvironment::EndMonthFlag = true;
        }
        TimeValue.allocate(2);
        auto timeStep = 1.0 / 6;
        SetupTimePointers("Zone", timeStep);
        SetupTimePointers("HVAC", timeStep);

        TimeValue(1).CurMinute = 10;
        TimeValue(2).CurMinute = 10;

        DataGlobals::WarmupFlag = true;

        ReportOutputFileHeaders();

        GetReportVariableInput();
        Array1D<ZonePurchasedAir> PurchAir; // Used to specify purchased air parameters
        PurchAir.allocate(1);
        SetupOutputVariable("Zone Ideal Loads Supply Air Total Heating Energy", OutputProcessor::Unit::J, PurchAir(1).TotHeatEnergy, "System", "Sum",
                            PurchAir(1).Name, _, "DISTRICTHEATING", "Heating", _, "System");

        PurchAir(1).TotHeatEnergy = 1.1;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        PurchAir(1).TotHeatEnergy = 1.3;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        PurchAir(1).TotHeatEnergy = 1.5;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        PurchAir(1).TotHeatEnergy = 1.7;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        PurchAir(1).TotHeatEnergy = 1.9;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        PurchAir(1).TotHeatEnergy = 2.2;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        DataGlobals::WarmupFlag = false;

        PurchAir(1).TotHeatEnergy = 2.4;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::ZoneTSReporting); // zone timestep

        compare_eso_stream(delimited_string({
            "7,1,,Zone Ideal Loads Supply Air Total Heating Energy [J] !Each Call",
            "44,11,,Zone Ideal Loads Supply Air Total Heating Energy [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            "2,365,12,31, 0,24,10.00,20.00,Tuesday",
            "7,1.1",
            "2,365,12,31, 0,24,20.00,30.00,Tuesday",
            "7,1.3",
            "2,365,12,31, 0,24,30.00,40.00,Tuesday",
            "7,1.5",
            "2,365,12,31, 0,24,40.00,50.00,Tuesday",
            "7,1.7",
            "2,365,12,31, 0,24,50.00,60.00,Tuesday",
            "7,1.9",
            "2,365,12,31, 0,24,60.00,70.00,Tuesday",
            "7,2.2",
            "5,365",
            "44,9.7,1.1,12,31,24,20,2.2,12,31,24,70",
        }));

        ResetAccumulationWhenWarmupComplete();

        PurchAir(1).TotHeatEnergy = 100.0;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        PurchAir(1).TotHeatEnergy = 200.0;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::HVACTSReporting);

        PurchAir(1).TotHeatEnergy = 300.0;
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::ZoneTSReporting); // zone timestep

        compare_eso_stream(delimited_string({
            "2,365,12,31, 0,24, 0.00,10.00,Tuesday",
            "7,100.0",
            "2,365,12,31, 0,24,10.00,20.00,Tuesday",
            "7,200.0",
            "5,365",
            "44,300.0,100.0,12,31,24,10,200.0,12,31,24,20",
        }));
    }
    TEST_F(EnergyPlusFixture, OutputProcessor_GenOutputVariablesAuditReport)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
            "Output:Variable,*,Boiler Gas Rate,detailed;",
            "Output:Variable,*,Boiler Heating Rate,detailed;",
            "Output:Meter,Electricity:Facility,timestep;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

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

        if (DataGlobals::TimeStep == DataGlobals::NumOfTimeStepInHour) {
            DataGlobals::EndHourFlag = true;
            if (DataGlobals::HourOfDay == 24) {
                DataGlobals::EndDayFlag = true;
                if ((!DataGlobals::WarmupFlag) && (DataGlobals::DayOfSim == DataGlobals::NumOfDayInEnvrn)) {
                    DataGlobals::EndEnvrnFlag = true;
                }
            }
        }

        if (DataEnvironment::DayOfMonth == WeatherManager::EndDayOfMonth(DataEnvironment::Month)) {
            DataEnvironment::EndMonthFlag = true;
        }

        TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers("Zone", timeStep);
        SetupTimePointers("HVAC", timeStep);

        TimeValue(1).CurMinute = 50;
        TimeValue(2).CurMinute = 50;

        GetReportVariableInput();
        SetupOutputVariable("Site Outdoor Air Drybulb Temperature", OutputProcessor::Unit::C, DataEnvironment::OutDryBulbTemp, "Zone", "Average",
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable("Lights Electric Energy", OutputProcessor::Unit::J, light_consumption, "Zone", "Sum", "SPACE1-1 LIGHTS 1", _,
                            "Electricity", "InteriorLights", "GeneralLights", "Building", "SPACE1-1", 1, 1);
        UpdateMeterReporting();
        UpdateDataandReport(DataGlobals::ZoneTSReporting);

        GenOutputVariablesAuditReport();

        std::string errMsg = delimited_string({
            "   ** Warning ** The following Report Variables were requested but not generated -- check.rdd file",
            "   **   ~~~   ** Either the IDF did not contain these elements, the variable name is misspelled,",
            "   **   ~~~   ** or the requested variable is an advanced output which requires Output : Diagnostics, DisplayAdvancedReportVariables;",
            "   ************* Key=*, VarName=BOILER GAS RATE, Frequency=Detailed",
            "   ************* Key=*, VarName=BOILER HEATING RATE, Frequency=Detailed",
        });

        compare_err_stream(errMsg);
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_DuplicateMeterCustom)
    {
        std::string const idf_objects = delimited_string(
            {"Version,8.6;", "Meter:Custom,", "CustomMeter1,               !- Name", "Generic,                    !- Fuel Type",
             ",                           !- Key Name 1", "DistrictHeating:Facility;   !- Variable or Meter 1 Name", "Meter:Custom,",
             "CustomMeter2,               !- Name", "Generic,                    !- Fuel Type", ",                           !- Key Name 1",
             "CustomMeter1;               !- Variable or Meter 1 Name", "Output:Meter,CustomMeter1,Hourly;", "Output:Meter,CustomMeter2,Hourly;"});

        ASSERT_TRUE(process_idf(idf_objects));

        bool errors_found = false;

        GetCustomMeterInput(errors_found);

        EXPECT_FALSE(errors_found);

        std::string errMsg = delimited_string(
            {"   ** Warning ** Meter:Custom=\"CUSTOMMETER1\", invalid Output Variable or Meter Name=\"DISTRICTHEATING:FACILITY\".",
             "   **   ~~~   ** ...will not be shown with the Meter results.", "   ** Warning ** Meter:Custom=\"CUSTOMMETER1\", no items assigned ",
             "   **   ~~~   ** ...will not be shown with the Meter results. This may be caused by a Meter:Custom be assigned to another "
             "Meter:Custom.",
             "   ** Warning ** Meter:Custom=\"CUSTOMMETER2\", contains a reference to another Meter:Custom in field: Output Variable or Meter "
             "Name=\"CUSTOMMETER1\"."});

        compare_err_stream(errMsg);
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_unitStringToEnum)
    {

        EXPECT_EQ(OutputProcessor::Unit::J, unitStringToEnum("J"));
        EXPECT_EQ(OutputProcessor::Unit::J, unitStringToEnum("j"));

        EXPECT_EQ(OutputProcessor::Unit::kgWater_kgDryAir, unitStringToEnum("kgWater/kgDryAir"));
        EXPECT_EQ(OutputProcessor::Unit::kgWater_s, unitStringToEnum("kgWater/s"));

        EXPECT_EQ(OutputProcessor::Unit::unknown, unitStringToEnum("junk"));
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_unitEnumToString)
    {

        EXPECT_EQ("J", unitEnumToString(OutputProcessor::Unit::J));

        EXPECT_EQ("kgWater/kgDryAir", unitEnumToString(OutputProcessor::Unit::kgWater_kgDryAir));
        EXPECT_EQ("kgWater/s", unitEnumToString(OutputProcessor::Unit::kgWater_s));

        EXPECT_EQ("unknown", unitEnumToString(OutputProcessor::Unit::unknown));
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_unitEnumToStringBrackets)
    {

        EXPECT_EQ(" [J]", unitEnumToStringBrackets(OutputProcessor::Unit::J));

        EXPECT_EQ(" [kgWater/kgDryAir]", unitEnumToStringBrackets(OutputProcessor::Unit::kgWater_kgDryAir));
        EXPECT_EQ(" [kgWater/s]", unitEnumToStringBrackets(OutputProcessor::Unit::kgWater_s));

        EXPECT_EQ(" [unknown]", unitEnumToStringBrackets(OutputProcessor::Unit::unknown));
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_unitStringFromDDitem)
    {

        AddToOutputVariableList("energy variable 1", 1, StoreType::Averaged, 1, OutputProcessor::Unit::J);
        AddToOutputVariableList("energy variable 2", 1, StoreType::Averaged, 1, OutputProcessor::Unit::J);
        AddToOutputVariableList("energy variable 3", 1, StoreType::Averaged, 1, OutputProcessor::Unit::J);

        AddToOutputVariableList("humidity ratio variable 1", 1, StoreType::Averaged, 1, OutputProcessor::Unit::kgWater_kgDryAir);
        AddToOutputVariableList("humidity ratio variable 2", 1, StoreType::Averaged, 1, OutputProcessor::Unit::kgWater_kgDryAir);

        AddToOutputVariableList("flow variable 1", 1, StoreType::Averaged, 1, OutputProcessor::Unit::kgWater_s);
        AddToOutputVariableList("flow variable 2", 1, StoreType::Averaged, 1, OutputProcessor::Unit::kgWater_s);

        AddToOutputVariableList("user defined EMS variable 1", 1, StoreType::Averaged, 1, OutputProcessor::Unit::customEMS, "ergs/century");
        AddToOutputVariableList("user defined EMS variable 2", 1, StoreType::Averaged, 1, OutputProcessor::Unit::customEMS, "swamps/county");

        EXPECT_EQ(" [J]", unitStringFromDDitem(3));

        EXPECT_EQ(" [kgWater/kgDryAir]", unitStringFromDDitem(4));

        EXPECT_EQ(" [kgWater/s]", unitStringFromDDitem(6));

        EXPECT_EQ(" [ergs/century]", unitStringFromDDitem(8));

        EXPECT_EQ(" [swamps/county]", unitStringFromDDitem(9));
    }

} // namespace OutputProcessor

} // namespace EnergyPlus
