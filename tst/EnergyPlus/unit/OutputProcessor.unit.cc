// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <map>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/SQLiteFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/WeatherManager.hh>

using namespace EnergyPlus::PurchasedAirManager;
using namespace EnergyPlus::WeatherManager;
using namespace EnergyPlus::OutputProcessor;

namespace EnergyPlus {

namespace OutputProcessor {

    TEST_F(SQLiteFixture, OutputProcessor_TestGetMeteredVariables)
    {
        int constexpr NumVariables = 2;
        Array1D_int VarIndexes(NumVariables);                            // Variable Numbers
        Array1D<OutputProcessor::VariableType> VarTypes(NumVariables);   // Variable Types (1=integer, 2=real, 3=meter)
        Array1D<OutputProcessor::TimeStepType> IndexTypes(NumVariables); // Variable Index Types (1=Zone,2=HVAC)
        Array1D<OutputProcessor::Unit> unitsForVar(NumVariables);        // units from enum for each variable
        std::map<int, DataGlobalConstants::ResourceType> ResourceTypes;  // ResourceTypes for each variable
        Array1D_string EndUses(NumVariables);                            // EndUses for each variable
        Array1D_string Groups(NumVariables);                             // Groups for each variable
        Array1D_string Names(NumVariables);                              // Variable Names for each variable
        Reference<RealVariables> RVar;

        std::string TypeOfComp = "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW";
        std::string NameOfComp = "FC-5-1B";

        int NumFound;

        for (int varN = 1; varN <= NumVariables; ++varN) {
            ResourceTypes.insert(std::pair<int, DataGlobalConstants::ResourceType>(varN, DataGlobalConstants::ResourceType::None));
        }

        GetMeteredVariables(
            *state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);

        EXPECT_EQ(0, NumFound);

        state->dataOutputProcessor->NumOfRVariable = 2;
        state->dataOutputProcessor->RVariableTypes.allocate(state->dataOutputProcessor->NumOfRVariable);
        NameOfComp = "OUTSIDELIGHTS";
        RVar.allocate();

        RVar().MeterArrayPtr = 1;
        state->dataOutputProcessor->RVariableTypes(1).KeyNameOnlyUC = NameOfComp;
        state->dataOutputProcessor->RVariableTypes(1).VarPtr = RVar;
        state->dataOutputProcessor->VarMeterArrays.allocate(1);

        state->dataOutputProcessor->VarMeterArrays(1).NumOnMeters = 1;
        state->dataOutputProcessor->VarMeterArrays(1).OnMeters(1) = 1;

        state->dataOutputProcessor->EnergyMeters.allocate(10);
        state->dataOutputProcessor->EnergyMeters(1).ResourceType = NameOfComp;

        GetMeteredVariables(
            *state, TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, unitsForVar, ResourceTypes, EndUses, Groups, Names, NumFound);
        EXPECT_EQ(1, NumFound);
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportTSMeters_PrintESOTimeStamp)
    {
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true);

        state->dataOutputProcessor->NumEnergyMeters = 2;
        state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
        state->dataOutputProcessor->EnergyMeters(1).CurTSValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).TSValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).RptTS = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccTS = false;
        state->dataOutputProcessor->EnergyMeters(1).RptTSFO = false;
        state->dataOutputProcessor->EnergyMeters(1).RptAccTSFO = false;
        state->dataOutputProcessor->EnergyMeters(1).TSRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).TSRptNumChr = "1";
        state->dataOutputProcessor->EnergyMeters(1).TSAccRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).SMValue = 999.9;

        state->dataOutputProcessor->EnergyMeters(2).CurTSValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).TSValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).RptTS = true;
        state->dataOutputProcessor->EnergyMeters(2).RptAccTS = false;
        state->dataOutputProcessor->EnergyMeters(2).RptTSFO = false;
        state->dataOutputProcessor->EnergyMeters(2).RptAccTSFO = false;
        state->dataOutputProcessor->EnergyMeters(2).TSRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).TSRptNumChr = "2";
        state->dataOutputProcessor->EnergyMeters(2).TSAccRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).SMValue = 9999.9;

        state->dataOutputProcessor->TimeStepStampReportNbr = 1;
        state->dataOutputProcessor->TimeStepStampReportChr = "1";
        state->dataGlobal->DayOfSim = 1;
        state->dataGlobal->DayOfSimChr = "1";
        state->dataGlobal->HourOfDay = 1;
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 21;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 2;
        state->dataEnvrn->HolidayIndex = 3;
        int EndMinute = 10;
        int StartMinute = 0;
        bool PrintESOTimeStamp = true;

        ReportTSMeters(*state, StartMinute, EndMinute, PrintESOTimeStamp, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9"}, "\n")));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportTSMeters)
    {
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true);

        state->dataOutputProcessor->NumEnergyMeters = 2;
        state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
        state->dataOutputProcessor->EnergyMeters(1).CurTSValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).TSValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).RptTS = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccTS = false;
        state->dataOutputProcessor->EnergyMeters(1).RptTSFO = false;
        state->dataOutputProcessor->EnergyMeters(1).RptAccTSFO = false;
        state->dataOutputProcessor->EnergyMeters(1).TSRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).TSRptNumChr = "1";
        state->dataOutputProcessor->EnergyMeters(1).TSAccRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).SMValue = 999.9;

        state->dataOutputProcessor->EnergyMeters(2).CurTSValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).TSValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).RptTS = true;
        state->dataOutputProcessor->EnergyMeters(2).RptAccTS = false;
        state->dataOutputProcessor->EnergyMeters(2).RptTSFO = false;
        state->dataOutputProcessor->EnergyMeters(2).RptAccTSFO = false;
        state->dataOutputProcessor->EnergyMeters(2).TSRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).TSRptNumChr = "2";
        state->dataOutputProcessor->EnergyMeters(2).TSAccRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).SMValue = 9999.9;

        state->dataOutputProcessor->TimeStepStampReportNbr = 1;
        state->dataOutputProcessor->TimeStepStampReportChr = "1";
        state->dataGlobal->DayOfSim = 1;
        state->dataGlobal->DayOfSimChr = "1";
        state->dataGlobal->HourOfDay = 1;
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 21;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 2;
        state->dataEnvrn->HolidayIndex = 3;
        int EndMinute = 10;
        int StartMinute = 0;
        bool PrintESOTimeStamp = false;

        ReportTSMeters(*state, StartMinute, EndMinute, PrintESOTimeStamp, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9", "2,9999.9"}, "\n")));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportHRMeters)
    {
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true);

        state->dataOutputProcessor->NumEnergyMeters = 2;
        state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
        state->dataOutputProcessor->EnergyMeters(1).RptHR = true;
        state->dataOutputProcessor->EnergyMeters(1).RptHRFO = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccHR = false;
        state->dataOutputProcessor->EnergyMeters(1).RptAccHRFO = false;
        state->dataOutputProcessor->EnergyMeters(1).HRRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).HRRptNumChr = "1";
        state->dataOutputProcessor->EnergyMeters(1).HRValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).HRAccRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).SMValue = 999.9;

        state->dataOutputProcessor->EnergyMeters(2).RptHR = true;
        state->dataOutputProcessor->EnergyMeters(2).RptHRFO = true;
        state->dataOutputProcessor->EnergyMeters(2).RptAccHR = false;
        state->dataOutputProcessor->EnergyMeters(2).RptAccHRFO = false;
        state->dataOutputProcessor->EnergyMeters(2).HRRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).HRRptNumChr = "2";
        state->dataOutputProcessor->EnergyMeters(2).HRValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).HRAccRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).SMValue = 9999.9;

        state->dataOutputProcessor->TimeStepStampReportNbr = 1;
        state->dataOutputProcessor->TimeStepStampReportChr = "1";
        state->dataGlobal->DayOfSim = 1;
        state->dataGlobal->DayOfSimChr = "1";
        state->dataGlobal->HourOfDay = 1;
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 21;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 2;
        state->dataEnvrn->HolidayIndex = 3;

        ReportHRMeters(*state, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay", "1,999.9", "2,9999.9"}, "\n")));

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({{"1", "1", "1", "999.9"}, {"2", "1", "2", "9999.9"}});

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        ASSERT_EQ(reportExtendedData.size(), reportExtendedDataResults.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_reportDYMeters)
    {
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true);

        state->dataOutputProcessor->NumEnergyMeters = 2;
        state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
        state->dataOutputProcessor->EnergyMeters(1).RptDY = true;
        state->dataOutputProcessor->EnergyMeters(1).RptDYFO = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccDY = false;
        state->dataOutputProcessor->EnergyMeters(1).RptAccDYFO = false;
        state->dataOutputProcessor->EnergyMeters(1).DYRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).DYRptNumChr = "1";
        state->dataOutputProcessor->EnergyMeters(1).DYValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).DYAccRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).SMValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).DYMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(1).DYMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(1).DYMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(1).DYMinValDate = 12210110;

        state->dataOutputProcessor->EnergyMeters(2).RptDY = true;
        state->dataOutputProcessor->EnergyMeters(2).RptDYFO = true;
        state->dataOutputProcessor->EnergyMeters(2).RptAccDY = false;
        state->dataOutputProcessor->EnergyMeters(2).RptAccDYFO = false;
        state->dataOutputProcessor->EnergyMeters(2).DYRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).DYRptNumChr = "2";
        state->dataOutputProcessor->EnergyMeters(2).DYValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).DYAccRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).SMValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).DYMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(2).DYMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(2).DYMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(2).DYMinValDate = 12210110;

        state->dataOutputProcessor->DailyStampReportNbr = 1;
        state->dataOutputProcessor->DailyStampReportChr = "1";
        state->dataGlobal->DayOfSim = 1;
        state->dataGlobal->DayOfSimChr = "1";
        state->dataGlobal->HourOfDay = 1;
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 21;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 2;
        state->dataEnvrn->HolidayIndex = 3;

        ReportDYMeters(*state, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0,WinterDesignDay",
                                                         "1,999.9,4283136.251683925, 1,10,4283136.252484382, 1,60",
                                                         "2,9999.9,4283136.251683925, 1,10,4283136.252484382, 1,60"},
                                                        "\n")));

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
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true);

        state->dataOutputProcessor->NumEnergyMeters = 2;
        state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
        state->dataOutputProcessor->EnergyMeters(1).RptMN = true;
        state->dataOutputProcessor->EnergyMeters(1).RptMNFO = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccMN = false;
        state->dataOutputProcessor->EnergyMeters(1).RptAccMNFO = false;
        state->dataOutputProcessor->EnergyMeters(1).MNRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).MNRptNumChr = "1";
        state->dataOutputProcessor->EnergyMeters(1).MNValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).MNAccRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).SMValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).MNMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(1).MNMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(1).MNMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(1).MNMinValDate = 12210110;

        state->dataOutputProcessor->EnergyMeters(2).RptMN = true;
        state->dataOutputProcessor->EnergyMeters(2).RptMNFO = true;
        state->dataOutputProcessor->EnergyMeters(2).RptAccMN = false;
        state->dataOutputProcessor->EnergyMeters(2).RptAccMNFO = false;
        state->dataOutputProcessor->EnergyMeters(2).MNRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).MNRptNumChr = "2";
        state->dataOutputProcessor->EnergyMeters(2).MNValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).MNAccRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).SMValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).MNMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(2).MNMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(2).MNMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(2).MNMinValDate = 12210110;

        state->dataOutputProcessor->MonthlyStampReportNbr = 1;
        state->dataOutputProcessor->MonthlyStampReportChr = "1";
        state->dataGlobal->DayOfSim = 1;
        state->dataGlobal->DayOfSimChr = "1";
        state->dataGlobal->HourOfDay = 1;
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 21;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 2;
        state->dataEnvrn->HolidayIndex = 3;

        ReportMNMeters(*state, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "0", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12",
                                                         "1,999.9,4283136.251683925,21, 1,10,4283136.252484382,21, 1,60",
                                                         "2,9999.9,4283136.251683925,21, 1,10,4283136.252484382,21, 1,60"},
                                                        "\n")));

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
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true);

        state->dataOutputProcessor->NumEnergyMeters = 2;
        state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
        state->dataOutputProcessor->EnergyMeters(1).RptSM = true;
        state->dataOutputProcessor->EnergyMeters(1).RptSMFO = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccSM = false;
        state->dataOutputProcessor->EnergyMeters(1).RptAccSMFO = false;
        state->dataOutputProcessor->EnergyMeters(1).SMRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).SMRptNumChr = "1";
        state->dataOutputProcessor->EnergyMeters(1).SMValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).SMAccRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).SMValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).SMMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(1).SMMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(1).SMMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(1).SMMinValDate = 12210110;

        state->dataOutputProcessor->EnergyMeters(2).RptSM = true;
        state->dataOutputProcessor->EnergyMeters(2).RptSMFO = true;
        state->dataOutputProcessor->EnergyMeters(2).RptAccSM = false;
        state->dataOutputProcessor->EnergyMeters(2).RptAccSMFO = false;
        state->dataOutputProcessor->EnergyMeters(2).SMRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).SMRptNumChr = "2";
        state->dataOutputProcessor->EnergyMeters(2).SMValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).SMAccRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).SMValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).SMMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(2).SMMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(2).SMMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(2).SMMinValDate = 12210110;

        state->dataOutputProcessor->RunPeriodStampReportNbr = 1;
        state->dataOutputProcessor->RunPeriodStampReportChr = "1";
        state->dataGlobal->DayOfSim = 1;
        state->dataGlobal->DayOfSimChr = "1";
        state->dataGlobal->HourOfDay = 1;
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 21;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 2;
        state->dataEnvrn->HolidayIndex = 3;

        ReportSMMeters(*state, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "", "", "", "", "", "", "1440", "4", "1", "", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1",
                                                         "1,999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60",
                                                         "2,9999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60"},
                                                        "\n")));

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
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true);

        state->dataOutputProcessor->NumEnergyMeters = 2;
        state->dataOutputProcessor->EnergyMeters.allocate(state->dataOutputProcessor->NumEnergyMeters);
        state->dataOutputProcessor->EnergyMeters(1).RptYR = true;
        state->dataOutputProcessor->EnergyMeters(1).RptYRFO = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccYR = false;
        state->dataOutputProcessor->EnergyMeters(1).RptAccYRFO = false;
        state->dataOutputProcessor->EnergyMeters(1).YRRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).YRRptNumChr = "1";
        state->dataOutputProcessor->EnergyMeters(1).YRValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).YRAccRptNum = 1;
        state->dataOutputProcessor->EnergyMeters(1).YRValue = 999.9;
        state->dataOutputProcessor->EnergyMeters(1).YRMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(1).YRMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(1).YRMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(1).YRMinValDate = 12210110;

        state->dataOutputProcessor->EnergyMeters(2).RptYR = true;
        state->dataOutputProcessor->EnergyMeters(2).RptYRFO = true;
        state->dataOutputProcessor->EnergyMeters(2).RptAccYR = false;
        state->dataOutputProcessor->EnergyMeters(2).RptAccYRFO = false;
        state->dataOutputProcessor->EnergyMeters(2).YRRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).YRRptNumChr = "2";
        state->dataOutputProcessor->EnergyMeters(2).YRValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).YRAccRptNum = 2;
        state->dataOutputProcessor->EnergyMeters(2).YRValue = 9999.9;
        state->dataOutputProcessor->EnergyMeters(2).YRMaxVal = 4283136.2524843821;
        state->dataOutputProcessor->EnergyMeters(2).YRMaxValDate = 12210160;
        state->dataOutputProcessor->EnergyMeters(2).YRMinVal = 4283136.2516839253;
        state->dataOutputProcessor->EnergyMeters(2).YRMinValDate = 12210110;

        state->dataOutputProcessor->YearlyStampReportNbr = 1;
        state->dataOutputProcessor->YearlyStampReportChr = "1";
        state->dataGlobal->DayOfSim = 1;
        state->dataGlobal->DayOfSimChr = "1";
        state->dataGlobal->HourOfDay = 1;
        state->dataGlobal->CalendarYear = 2017;
        state->dataGlobal->CalendarYearChr = "2017";
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 21;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 2;
        state->dataEnvrn->HolidayIndex = 3;

        OutputProcessor::ReportYRMeters(*state, true);

        auto result = queryResult("SELECT * FROM Time;", "Time");

        ASSERT_EQ(1ul, result.size());

        std::vector<std::string> testResult0{"1", "2017", "", "", "", "", "", "", "5", "", "", "0", ""};
        EXPECT_EQ(testResult0, result[0]);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,2017",
                                                         "1,999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60",
                                                         "2,9999.9,4283136.251683925,12,21, 1,10,4283136.252484382,12,21, 1,60"},
                                                        "\n")));

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
        state->dataOutputProcessor->TimeStepStampReportNbr = 1;
        state->dataOutputProcessor->TimeStepStampReportChr = "1";

        int DailyStampReportNbr = 1;
        std::string DailyStampReportChr = "1";

        int MonthlyStampReportNbr = 1;
        std::string MonthlyStampReportChr = "1";

        int RunPeriodStampReportNbr = 1;
        std::string RunPeriodStampReportChr = "1";

        state->dataGlobal->DayOfSim = 1;
        std::string DayOfSimChr = "1";
        bool PrintTimeStamp = true;
        int Month = 12;
        int DayOfMonth = 21;
        state->dataGlobal->HourOfDay = 1;
        int EndMinute = 10;
        int StartMinute = 0;
        int DSTIndicator = 0;
        int CurDayType = 10;

        // TSMeter
        WriteTimeStampFormatData(*state,
                                 state->files.mtr,
                                 ReportingFrequency::TimeStep,
                                 state->dataOutputProcessor->TimeStepStampReportNbr,
                                 state->dataOutputProcessor->TimeStepStampReportChr,
                                 DayOfSimChr,
                                 PrintTimeStamp,
                                 Month,
                                 DayOfMonth,
                                 state->dataGlobal->HourOfDay,
                                 EndMinute,
                                 StartMinute,
                                 DSTIndicator,
                                 DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay"}, "\n")));

        // TSMeter
        WriteTimeStampFormatData(*state,
                                 state->files.mtr,
                                 ReportingFrequency::EachCall,
                                 state->dataOutputProcessor->TimeStepStampReportNbr,
                                 state->dataOutputProcessor->TimeStepStampReportChr,
                                 DayOfSimChr,
                                 PrintTimeStamp,
                                 Month,
                                 DayOfMonth,
                                 state->dataGlobal->HourOfDay,
                                 EndMinute,
                                 StartMinute,
                                 DSTIndicator,
                                 DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay"}, "\n")));

        // HRMeter
        WriteTimeStampFormatData(*state,
                                 state->files.mtr,
                                 ReportingFrequency::Hourly,
                                 state->dataOutputProcessor->TimeStepStampReportNbr,
                                 state->dataOutputProcessor->TimeStepStampReportChr,
                                 DayOfSimChr,
                                 PrintTimeStamp,
                                 Month,
                                 DayOfMonth,
                                 state->dataGlobal->HourOfDay,
                                 _,
                                 _,
                                 DSTIndicator,
                                 DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay"}, "\n")));

        // DYMeter
        WriteTimeStampFormatData(*state,
                                 state->files.mtr,
                                 ReportingFrequency::Daily,
                                 DailyStampReportNbr,
                                 DailyStampReportChr,
                                 DayOfSimChr,
                                 PrintTimeStamp,
                                 Month,
                                 DayOfMonth,
                                 _,
                                 _,
                                 _,
                                 DSTIndicator,
                                 DayTypes(CurDayType));
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12,21, 0,WinterDesignDay"}, "\n")));

        // MNMeter
        WriteTimeStampFormatData(*state,
                                 state->files.mtr,
                                 ReportingFrequency::Monthly,
                                 MonthlyStampReportNbr,
                                 MonthlyStampReportChr,
                                 DayOfSimChr,
                                 PrintTimeStamp,
                                 Month,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,12"}, "\n")));

        // SMMeter
        WriteTimeStampFormatData(*state,
                                 state->files.mtr,
                                 ReportingFrequency::Simulation,
                                 RunPeriodStampReportNbr,
                                 RunPeriodStampReportChr,
                                 DayOfSimChr,
                                 PrintTimeStamp,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1"}, "\n")));

        // Bad input
        WriteTimeStampFormatData(*state,
                                 state->files.mtr,
                                 static_cast<ReportingFrequency>(999),
                                 RunPeriodStampReportNbr,
                                 RunPeriodStampReportChr,
                                 DayOfSimChr,
                                 PrintTimeStamp,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _);

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
        state->dataGlobal->MinutesPerTimeStep = 10;

        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);

        WriteReportMeterData(*state, 1, "1", 999.9, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,999.9"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportMeterData(*state, 1, "1", 999.9, ReportingFrequency::EachCall, 0.0, 0, 0.0, 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,999.9"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Hourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Daily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211775,24,60"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211775,24,60"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Monthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211775,21,24,60"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211775,21,24,60"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Simulation, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211775,12,21,24,60"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211775,12,21,24,60"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::TimeStep, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::EachCall, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Hourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Daily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211775,24,60"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Monthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211775,21,24,60"}, "\n")));

        WriteReportMeterData(
            *state, 1, "1", 616771620.98702729, ReportingFrequency::Simulation, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211775,12,21,24,60"}, "\n")));

        WriteReportMeterData(*state, 1, "1", 0, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,0.0"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"}, "\n")));

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
        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);

        WriteReportRealData(*state, 1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportRealData(*state, 1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::EachCall, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportRealData(*state, 1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::Hourly, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Summed,
                            1,
                            ReportingFrequency::Daily,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925, 1,10,4283136.2587211775,24,60"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Summed,
                            1,
                            ReportingFrequency::Monthly,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,21, 1,10,4283136.2587211775,21,24,60"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Summed,
                            1,
                            ReportingFrequency::Simulation,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273,4283136.251683925,12,21, 1,10,4283136.2587211775,12,21,24,60"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Averaged,
                            10,
                            ReportingFrequency::TimeStep,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Averaged,
                            10,
                            ReportingFrequency::EachCall,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Averaged,
                            10,
                            ReportingFrequency::Hourly,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Averaged,
                            10,
                            ReportingFrequency::Daily,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273,4283136.251683925, 1,10,4283136.2587211775,24,60"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Averaged,
                            10,
                            ReportingFrequency::Monthly,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273,4283136.251683925,21, 1,10,4283136.2587211775,21,24,60"}, "\n")));

        WriteReportRealData(*state,
                            1,
                            "1",
                            616771620.98702729,
                            StoreType::Averaged,
                            10,
                            ReportingFrequency::Simulation,
                            4283136.2516839253,
                            12210110,
                            4283136.2587211775,
                            12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.09870273,4283136.251683925,12,21, 1,10,4283136.2587211775,12,21,24,60"}, "\n")));

        WriteReportRealData(*state, 1, "1", 0, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0.0, 0, 0.0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"}, "\n")));

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
        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);

        WriteReportIntegerData(*state, 1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportIntegerData(*state, 1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::EachCall, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportIntegerData(*state, 1, "1", 999.9, StoreType::Summed, 1, ReportingFrequency::Hourly, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteReportIntegerData(
            *state, 1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Daily, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.987027,4283136, 1,10,4283196,24,60"}, "\n")));

        WriteReportIntegerData(
            *state, 1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Monthly, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.987027,4283136,21, 1,10,4283196,21,24,60"}, "\n")));

        WriteReportIntegerData(
            *state, 1, "1", 616771620.98702729, StoreType::Summed, 1, ReportingFrequency::Simulation, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.987027,4283136,12,21, 1,10,4283196,12,21,24,60"}, "\n")));

        WriteReportIntegerData(*state, 1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::TimeStep, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027"}, "\n")));

        WriteReportIntegerData(*state, 1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::EachCall, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027"}, "\n")));

        WriteReportIntegerData(*state, 1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Hourly, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027"}, "\n")));

        WriteReportIntegerData(
            *state, 1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Daily, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027,4283136, 1,10,4283196,24,60"}, "\n")));

        WriteReportIntegerData(
            *state, 1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Monthly, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027,4283136,21, 1,10,4283196,21,24,60"}, "\n")));

        WriteReportIntegerData(
            *state, 1, "1", 616771620.98702729, StoreType::Averaged, 10, ReportingFrequency::Simulation, 4283136, 12210110, 4283196, 12212460);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,61677162.0987027,4283136,12,21, 1,10,4283196,12,21,24,60"}, "\n")));

        WriteReportIntegerData(*state, 1, "1", 0, StoreType::Summed, 1, ReportingFrequency::TimeStep, 0, 0, 0, 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"}, "\n")));

        WriteReportIntegerData(*state, 1, "1", 25.75, StoreType::Averaged, 720, ReportingFrequency::Monthly, 0, 4010115, 1, 4011560);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.3E-01,0, 1, 1,15,1, 1,15,60"}, "\n")));

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
                                                          {"13", "1", "1", "0.0"},
                                                          {"14", "1", "1", "0.0357638888888889"}});

        std::vector<std::vector<std::string>> reportExtendedData(
            {{"1", "4", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"2", "5", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"3", "6", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"4", "10", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"5", "11", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"6", "12", "4283196.0", "12", "21", "24", "", "0", "4283136.0", "12", "21", "0", "", "10"},
             {"7", "14", "1.0", "4", "1", "15", "", "0", "0.0", "4", "1", "0", "", "15"}});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeNumericData_1)
    {
        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);

        WriteNumericData(*state, 1, "1", 999);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999"}, "\n")));

        WriteNumericData(*state, 1, "1", 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0"}, "\n")));

        WriteNumericData(*state, 1, "1", -999);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-999"}, "\n")));

        WriteNumericData(*state, 1, "1", 999.9);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,999.9"}, "\n")));

        WriteNumericData(*state, 1, "1", 0.0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"}, "\n")));

        WriteNumericData(*state, 1, "1", -999.9);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-999.9"}, "\n")));

        WriteNumericData(*state, 1, "1", 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0"}, "\n")));

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
                                                                 {"NATURALGAS", "NaturalGas"},
                                                                 {"GASOLINE", "Gasoline"},
                                                                 {"DIESEL", "Diesel"},
                                                                 {"COAL", "Coal"},
                                                                 {"FUELOILNO1", "FuelOilNo1"},
                                                                 {"FUELOILNO2", "FuelOilNo2"},
                                                                 {"PROPANE", "Propane"},
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
            GetStandardMeterResourceType(*state, out_resource_type, meterType.first, error_found);
            EXPECT_EQ(meterType.second, out_resource_type);
            EXPECT_FALSE(error_found);
        }

        state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        auto const meterType = "BAD INPUT";
        out_resource_type = "BAD INPUT";

        GetStandardMeterResourceType(*state, out_resource_type, meterType, error_found);

        EXPECT_EQ(meterType, out_resource_type);
        EXPECT_TRUE(error_found);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(1ul, errorData.size());
        std::vector<std::string> errorData0{
            "1", "1", "1", "GetStandardMeterResourceType: Illegal OutResourceType (for Meters) Entered=BAD INPUT", "1"};
        EXPECT_EQ(errorData0, errorData[0]);
    }

    TEST_F(SQLiteFixture, OutputProcessor_determineIndexGroupKeyFromMeterName)
    {
        std::map<std::string, int> const resource_map = {{"Electricity:Facility", 100},
                                                         {"NaturalGas:Facility", 101},
                                                         {"DistricHeating:Facility", 102},
                                                         {"DistricCooling:Facility", 103},
                                                         {"ElectricityNet:Facility", 104},
                                                         {"Electricity:Building", 201},
                                                         {"NaturalGas:Building", 202},
                                                         {"Electricity:HVAC", 301},
                                                         {"InteriorLights:Electricity", 401},
                                                         {"InteriorLights:Electricity:Zone", 501},
                                                         {"BAD INPUT", -11}};

        for (auto const &indexGroup : resource_map) {
            EXPECT_EQ(indexGroup.second, DetermineIndexGroupKeyFromMeterName(*state, indexGroup.first)) << "where meterName is " << indexGroup.first;
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_validateTimeStepType)
    {
        std::map<OutputProcessor::SOVTimeStepType, OutputProcessor::TimeStepType> const resource_map = {
            // Zone
            {OutputProcessor::SOVTimeStepType::Zone, OutputProcessor::TimeStepType::Zone},
            // System
            {OutputProcessor::SOVTimeStepType::HVAC, OutputProcessor::TimeStepType::System},
            {OutputProcessor::SOVTimeStepType::System, OutputProcessor::TimeStepType::System},
            {OutputProcessor::SOVTimeStepType::Plant, OutputProcessor::TimeStepType::System}};

        for (auto const &indexGroup : resource_map) {
            EXPECT_TRUE(compare_enums(indexGroup.second, ValidateTimeStepType(*state, indexGroup.first)));
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_standardIndexTypeKey)
    {
        EXPECT_EQ("Zone", StandardTimeStepTypeKey(OutputProcessor::TimeStepType::Zone));
        EXPECT_EQ("HVAC", StandardTimeStepTypeKey(OutputProcessor::TimeStepType::System));

        // It's no longer possible to pass something that isn't part of the enum, that's kind of the point of using an enum!
        // EXPECT_EQ("UNKW", StandardTimeStepTypeKey(0));
        // EXPECT_EQ("UNKW", StandardTimeStepTypeKey(-1));
        // EXPECT_EQ("UNKW", StandardTimeStepTypeKey(3));
    }

    TEST_F(SQLiteFixture, OutputProcessor_validateVariableType)
    {
        std::map<OutputProcessor::SOVStoreType, StoreType> const resource_map = {{OutputProcessor::SOVStoreType::State, StoreType::Averaged},
                                                                                 {OutputProcessor::SOVStoreType::Average, StoreType::Averaged},
                                                                                 {OutputProcessor::SOVStoreType::NonState, StoreType::Summed},
                                                                                 {OutputProcessor::SOVStoreType::Summed, StoreType::Summed}};

        for (auto const &variableType : resource_map) {
            EXPECT_TRUE(compare_enums(variableType.second, validateVariableType(*state, variableType.first)));
        }
    }

    TEST_F(SQLiteFixture, OutputProcessor_standardVariableTypeKey)
    {
        EXPECT_EQ("Average", standardVariableTypeKey(StoreType::Averaged));
        EXPECT_EQ("Sum", standardVariableTypeKey(StoreType::Summed));
        EXPECT_EQ("Unknown", standardVariableTypeKey(static_cast<StoreType>(0)));
    }

    TEST_F(SQLiteFixture, OutputProcessor_determineMeterIPUnits)
    {
        OutputProcessor::RT_IPUnits ipUnits = OutputProcessor::RT_IPUnits::Invalid;
        bool errorFound = false;

        DetermineMeterIPUnits(*state, ipUnits, "ELEC", OutputProcessor::Unit::J, errorFound);
        EXPECT_TRUE(compare_enums(RT_IPUnits::Electricity, ipUnits));
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(*state, ipUnits, "GAS", OutputProcessor::Unit::J, errorFound);
        EXPECT_TRUE(compare_enums(RT_IPUnits::Gas, ipUnits));
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(*state, ipUnits, "COOL", OutputProcessor::Unit::J, errorFound);
        EXPECT_TRUE(compare_enums(RT_IPUnits::Cooling, ipUnits));
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(*state, ipUnits, "WATER", OutputProcessor::Unit::m3, errorFound);
        EXPECT_TRUE(compare_enums(RT_IPUnits::Water, ipUnits));
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(*state, ipUnits, "OTHER", OutputProcessor::Unit::m3, errorFound);
        EXPECT_TRUE(compare_enums(RT_IPUnits::OtherM3, ipUnits));
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(*state, ipUnits, "OTHER", OutputProcessor::Unit::kg, errorFound);
        EXPECT_TRUE(compare_enums(RT_IPUnits::OtherKG, ipUnits));
        EXPECT_FALSE(errorFound);

        DetermineMeterIPUnits(*state, ipUnits, "OTHER", OutputProcessor::Unit::L, errorFound);
        EXPECT_TRUE(compare_enums(RT_IPUnits::OtherL, ipUnits));
        EXPECT_FALSE(errorFound);

        state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        ipUnits = OutputProcessor::RT_IPUnits::Invalid;
        DetermineMeterIPUnits(*state, ipUnits, "UNKONWN", OutputProcessor::Unit::unknown, errorFound); // was "badunits"
        EXPECT_TRUE(compare_enums(RT_IPUnits::OtherJ, ipUnits));
        EXPECT_TRUE(errorFound);

        ipUnits = OutputProcessor::RT_IPUnits::Invalid;
        DetermineMeterIPUnits(*state, ipUnits, "ELEC", OutputProcessor::Unit::unknown, errorFound); // was "kWh"
        EXPECT_TRUE(compare_enums(RT_IPUnits::Electricity, ipUnits));
        EXPECT_TRUE(errorFound);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(2ul, errorData.size());
        std::vector<std::string> errorData0{
            "1", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[unknown].", "1"};
        std::vector<std::string> errorData1{
            "2", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[unknown].", "1"};
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
        InitializeOutput(*state);

        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::TimeStep,
                                 StoreType::Averaged,
                                 1,
                                 -999,
                                 "indexGroup",
                                 "1",
                                 "meterName",
                                 OutputProcessor::Unit::J,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,1,meterName [J] !TimeStep"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1,meterName [J] !TimeStep"}, "\n")));

        WriteMeterDictionaryItem(
            *state, ReportingFrequency::TimeStep, StoreType::Summed, 2, -999, "indexGroup", "2", "meterName", OutputProcessor::Unit::W, false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"2,1,meterName [W] !TimeStep"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"2,1,meterName [W] !TimeStep"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::TimeStep,
                                 StoreType::Averaged,
                                 3,
                                 -999,
                                 "indexGroup",
                                 "3",
                                 "meterName",
                                 OutputProcessor::Unit::J,
                                 true,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"3,1,Cumulative meterName [J] !TimeStep"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"3,1,Cumulative meterName [J] !TimeStep"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::TimeStep,
                                 StoreType::Averaged,
                                 4,
                                 -999,
                                 "indexGroup",
                                 "4",
                                 "meterName",
                                 OutputProcessor::Unit::W,
                                 false,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"4,1,meterName [W] !TimeStep"}, "\n")));

        WriteMeterDictionaryItem(
            *state, ReportingFrequency::TimeStep, StoreType::Averaged, 5, -999, "indexGroup", "5", "meterName", OutputProcessor::Unit::W, true, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"5,1,Cumulative meterName [W] !TimeStep"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::EachCall,
                                 StoreType::Averaged,
                                 6,
                                 -999,
                                 "indexGroup",
                                 "6",
                                 "meterName",
                                 OutputProcessor::Unit::J,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"6,1,meterName [J] !Each Call"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"6,1,meterName [J] !Each Call"}, "\n")));

        WriteMeterDictionaryItem(
            *state, ReportingFrequency::EachCall, StoreType::Summed, 7, -999, "indexGroup", "7", "meterName", OutputProcessor::Unit::J, false, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"7,1,meterName [J] !Each Call"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"7,1,meterName [J] !Each Call"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::EachCall,
                                 StoreType::Averaged,
                                 8,
                                 -999,
                                 "indexGroup",
                                 "8",
                                 "meterName",
                                 OutputProcessor::Unit::J,
                                 true,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"8,1,Cumulative meterName [J] !Each Call"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"8,1,Cumulative meterName [J] !Each Call"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::EachCall,
                                 StoreType::Averaged,
                                 9,
                                 -999,
                                 "indexGroup",
                                 "9",
                                 "meterName",
                                 OutputProcessor::Unit::J,
                                 false,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"9,1,meterName [J] !Each Call"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::EachCall,
                                 StoreType::Averaged,
                                 10,
                                 -999,
                                 "indexGroup",
                                 "10",
                                 "meterName",
                                 OutputProcessor::Unit::J,
                                 true,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"10,1,Cumulative meterName [J] !Each Call"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Hourly,
                                 StoreType::Averaged,
                                 11,
                                 -999,
                                 "indexGroup",
                                 "11",
                                 "meterName",
                                 OutputProcessor::Unit::J,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"11,1,meterName [J] !Hourly"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"11,1,meterName [J] !Hourly"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Hourly,
                                 StoreType::Summed,
                                 12,
                                 -999,
                                 "indexGroup",
                                 "12",
                                 "meterName",
                                 OutputProcessor::Unit::None,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"12,1,meterName [] !Hourly"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"12,1,meterName [] !Hourly"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Hourly,
                                 StoreType::Averaged,
                                 13,
                                 -999,
                                 "indexGroup",
                                 "13",
                                 "meterName",
                                 OutputProcessor::Unit::None,
                                 true,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"13,1,Cumulative meterName [] !Hourly"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"13,1,Cumulative meterName [] !Hourly"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Hourly,
                                 StoreType::Averaged,
                                 14,
                                 -999,
                                 "indexGroup",
                                 "14",
                                 "meterName",
                                 OutputProcessor::Unit::None,
                                 false,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"14,1,meterName [] !Hourly"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Hourly,
                                 StoreType::Averaged,
                                 15,
                                 -999,
                                 "indexGroup",
                                 "15",
                                 "meterName",
                                 OutputProcessor::Unit::None,
                                 true,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"15,1,Cumulative meterName [] !Hourly"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Daily,
                                 StoreType::Averaged,
                                 16,
                                 -999,
                                 "indexGroup",
                                 "16",
                                 "meterName",
                                 OutputProcessor::Unit::None,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"16,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"16,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Daily,
                                 StoreType::Summed,
                                 17,
                                 -999,
                                 "indexGroup",
                                 "17",
                                 "meterName",
                                 OutputProcessor::Unit::None,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"17,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"17,7,meterName [] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Daily,
                                 StoreType::Averaged,
                                 18,
                                 -999,
                                 "indexGroup",
                                 "18",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 true,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"18,1,Cumulative meterName [deltaC] !Daily "}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"18,1,Cumulative meterName [deltaC] !Daily "}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Daily,
                                 StoreType::Averaged,
                                 19,
                                 -999,
                                 "indexGroup",
                                 "19",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 false,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"19,7,meterName [deltaC] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Daily,
                                 StoreType::Averaged,
                                 20,
                                 -999,
                                 "indexGroup",
                                 "20",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 true,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"20,1,Cumulative meterName [deltaC] !Daily "}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Monthly,
                                 StoreType::Averaged,
                                 21,
                                 -999,
                                 "indexGroup",
                                 "21",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"21,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"21,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Monthly,
                                 StoreType::Summed,
                                 22,
                                 -999,
                                 "indexGroup",
                                 "22",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"22,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"22,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Monthly,
                                 StoreType::Averaged,
                                 23,
                                 -999,
                                 "indexGroup",
                                 "23",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 true,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"23,1,Cumulative meterName [deltaC] !Monthly "}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"23,1,Cumulative meterName [deltaC] !Monthly "}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Monthly,
                                 StoreType::Averaged,
                                 24,
                                 -999,
                                 "indexGroup",
                                 "24",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 false,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"24,9,meterName [deltaC] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Monthly,
                                 StoreType::Averaged,
                                 25,
                                 -999,
                                 "indexGroup",
                                 "25",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 true,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"25,1,Cumulative meterName [deltaC] !Monthly "}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Simulation,
                                 StoreType::Averaged,
                                 26,
                                 -999,
                                 "indexGroup",
                                 "26",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(
            delimited_string({"26,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"26,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Simulation,
                                 StoreType::Summed,
                                 27,
                                 -999,
                                 "indexGroup",
                                 "27",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 false,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(
            delimited_string({"27,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"27,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Simulation,
                                 StoreType::Averaged,
                                 28,
                                 -999,
                                 "indexGroup",
                                 "28",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 true,
                                 false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"28,1,Cumulative meterName [deltaC] !RunPeriod "}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"28,1,Cumulative meterName [deltaC] !RunPeriod "}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Simulation,
                                 StoreType::Averaged,
                                 29,
                                 -999,
                                 "indexGroup",
                                 "29",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 false,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(
            delimited_string({"29,11,meterName [deltaC] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));

        WriteMeterDictionaryItem(*state,
                                 ReportingFrequency::Simulation,
                                 StoreType::Averaged,
                                 30,
                                 -999,
                                 "indexGroup",
                                 "30",
                                 "meterName",
                                 OutputProcessor::Unit::deltaC,
                                 true,
                                 true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"30,1,Cumulative meterName [deltaC] !RunPeriod "}, "\n")));

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        // TODO: Double check my determination of correct behavior
        // Before it was internally calling createSQLiteReportDictionaryRecord with indexType = 1 which in SQLiteProcedures means "HVAC System"
        // but in OutputProcessor domain it means Zone...
        // According to the I/O ref guide: Output:Meter is supposed to have the "Zone Timestep interval" for its resolution
        std::string timeStepTypeString = "Zone";

        std::vector<std::vector<std::string>> reportDataDictionary(
            {{"1", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Zone Timestep", "", "J"},
             {"2", "1", "Sum", "indexGroup", timeStepTypeString, "", "meterName", "Zone Timestep", "", "W"},
             {"3", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Zone Timestep", "", "J"},
             {"4", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Zone Timestep", "", "W"},
             {"5", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Zone Timestep", "", "W"},
             {"6", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "HVAC System Timestep", "", "J"},
             {"7", "1", "Sum", "indexGroup", timeStepTypeString, "", "meterName", "HVAC System Timestep", "", "J"},
             {"8", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "HVAC System Timestep", "", "J"},
             {"9", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "HVAC System Timestep", "", "J"},
             {"10", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "HVAC System Timestep", "", "J"},
             {"11", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Hourly", "", "J"},
             {"12", "1", "Sum", "indexGroup", timeStepTypeString, "", "meterName", "Hourly", "", ""},
             {"13", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Hourly", "", ""},
             {"14", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Hourly", "", ""},
             {"15", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Hourly", "", ""},
             {"16", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Daily", "", ""},
             {"17", "1", "Sum", "indexGroup", timeStepTypeString, "", "meterName", "Daily", "", ""},
             {"18", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Daily", "", "deltaC"},
             {"19", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Daily", "", "deltaC"},
             {"20", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Daily", "", "deltaC"},
             {"21", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Monthly", "", "deltaC"},
             {"22", "1", "Sum", "indexGroup", timeStepTypeString, "", "meterName", "Monthly", "", "deltaC"},
             {"23", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Monthly", "", "deltaC"},
             {"24", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Monthly", "", "deltaC"},
             {"25", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Monthly", "", "deltaC"},
             {"26", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Run Period", "", "deltaC"},
             {"27", "1", "Sum", "indexGroup", timeStepTypeString, "", "meterName", "Run Period", "", "deltaC"},
             {"28", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Run Period", "", "deltaC"},
             {"29", "1", "Avg", "indexGroup", timeStepTypeString, "", "meterName", "Run Period", "", "deltaC"},
             {"30", "1", "Avg", "indexGroup", timeStepTypeString, "Cumulative ", "meterName", "Run Period", "", "deltaC"}});
        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeReportVariableDictionaryItem)
    {
        InitializeOutput(*state);

        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);

        // Store expected results
        std::vector<std::vector<std::string>> expectedReportDataDictionary;

        std::string timeStepZoneString = "Zone";
        std::string timeStepSystemString = "HVAC System";

        // For now I don't accept anything else than TimeStepZone or TimeStepSystem, but to make it easier if we need to change that later
        // and to preserve the original test (passing int=3 before should have defaulted to Zone...)
        OutputProcessor::TimeStepType aThirdTimeStepType = OutputProcessor::TimeStepType::Zone;
        std::string aThirdTimeStepString = timeStepZoneString;

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::TimeStep,
                                          StoreType::Averaged,
                                          1,
                                          -999,
                                          "indexGroup",
                                          "1",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1,keyedValue,variableName [m3/s] !TimeStep"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::TimeStep,
                                          StoreType::Summed,
                                          2,
                                          -999,
                                          "indexGroup",
                                          "2",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"2,1,keyedValue,variableName [m3/s] !TimeStep"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::TimeStep,
                                          StoreType::Averaged,
                                          3,
                                          -999,
                                          "indexGroup",
                                          "3",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          "scheduleName");
        EXPECT_TRUE(compare_eso_stream(delimited_string({"3,1,keyedValue,variableName [m3/s] !TimeStep,scheduleName"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::TimeStep,
                                          StoreType::Averaged,
                                          4,
                                          -999,
                                          "indexGroup",
                                          "4",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::System,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"4,1,keyedValue,variableName [m3/s] !TimeStep"}, "\n")));

        // Hum, can no longer pass Something else than what's in the enum...
        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::TimeStep,
                                          StoreType::Averaged,
                                          5,
                                          -999,
                                          "indexGroup",
                                          "5",
                                          "keyedValue",
                                          "variableName",
                                          aThirdTimeStepType,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"5,1,keyedValue,variableName [m3/s] !TimeStep"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::EachCall,
                                          StoreType::Averaged,
                                          6,
                                          -999,
                                          "indexGroup",
                                          "6",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"6,1,keyedValue,variableName [m3/s] !Each Call"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::EachCall,
                                          StoreType::Summed,
                                          7,
                                          -999,
                                          "indexGroup",
                                          "7",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"7,1,keyedValue,variableName [m3/s] !Each Call"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::EachCall,
                                          StoreType::Averaged,
                                          8,
                                          -999,
                                          "indexGroup",
                                          "8",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          "scheduleName");
        EXPECT_TRUE(compare_eso_stream(delimited_string({"8,1,keyedValue,variableName [m3/s] !Each Call,scheduleName"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::EachCall,
                                          StoreType::Averaged,
                                          9,
                                          -999,
                                          "indexGroup",
                                          "9",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::System,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"9,1,keyedValue,variableName [m3/s] !Each Call"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::EachCall,
                                          StoreType::Averaged,
                                          10,
                                          -999,
                                          "indexGroup",
                                          "10",
                                          "keyedValue",
                                          "variableName",
                                          aThirdTimeStepType,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(compare_eso_stream(delimited_string({"10,1,keyedValue,variableName [m3/s] !Each Call"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Hourly,
                                          StoreType::Averaged,
                                          11,
                                          -999,
                                          "indexGroup",
                                          "11",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingHourlyVariables);
        state->dataOutputProcessor->TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"11,1,keyedValue,variableName [m3/s] !Hourly"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Hourly,
                                          StoreType::Summed,
                                          12,
                                          -999,
                                          "indexGroup",
                                          "12",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingHourlyVariables);
        state->dataOutputProcessor->TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"12,1,keyedValue,variableName [m3/s] !Hourly"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Hourly,
                                          StoreType::Averaged,
                                          13,
                                          -999,
                                          "indexGroup",
                                          "13",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          "scheduleName");
        EXPECT_TRUE(state->dataOutputProcessor->TrackingHourlyVariables);
        state->dataOutputProcessor->TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"13,1,keyedValue,variableName [m3/s] !Hourly,scheduleName"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Hourly,
                                          StoreType::Averaged,
                                          14,
                                          -999,
                                          "indexGroup",
                                          "14",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::System,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingHourlyVariables);
        state->dataOutputProcessor->TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"14,1,keyedValue,variableName [m3/s] !Hourly"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Hourly,
                                          StoreType::Averaged,
                                          15,
                                          -999,
                                          "indexGroup",
                                          "15",
                                          "keyedValue",
                                          "variableName",
                                          aThirdTimeStepType,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingHourlyVariables);
        state->dataOutputProcessor->TrackingHourlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string({"15,1,keyedValue,variableName [m3/s] !Hourly"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Daily,
                                          StoreType::Averaged,
                                          16,
                                          -999,
                                          "indexGroup",
                                          "16",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingDailyVariables);
        state->dataOutputProcessor->TrackingDailyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"16,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Daily,
                                          StoreType::Summed,
                                          17,
                                          -999,
                                          "indexGroup",
                                          "17",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingDailyVariables);
        state->dataOutputProcessor->TrackingDailyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"17,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Daily,
                                          StoreType::Averaged,
                                          18,
                                          -999,
                                          "indexGroup",
                                          "18",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          "scheduleName");
        EXPECT_TRUE(state->dataOutputProcessor->TrackingDailyVariables);
        state->dataOutputProcessor->TrackingDailyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"18,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute],scheduleName"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Daily,
                                          StoreType::Averaged,
                                          19,
                                          -999,
                                          "indexGroup",
                                          "19",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::System,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingDailyVariables);
        state->dataOutputProcessor->TrackingDailyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"19,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Daily,
                                          StoreType::Averaged,
                                          20,
                                          -999,
                                          "indexGroup",
                                          "20",
                                          "keyedValue",
                                          "variableName",
                                          aThirdTimeStepType,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingDailyVariables);
        state->dataOutputProcessor->TrackingDailyVariables = false;
        EXPECT_TRUE(
            compare_eso_stream(delimited_string({"20,7,keyedValue,variableName [m3/s] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Monthly,
                                          StoreType::Averaged,
                                          21,
                                          -999,
                                          "indexGroup",
                                          "21",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingMonthlyVariables);
        state->dataOutputProcessor->TrackingMonthlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"21,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Monthly,
                                          StoreType::Summed,
                                          22,
                                          -999,
                                          "indexGroup",
                                          "22",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingMonthlyVariables);
        state->dataOutputProcessor->TrackingMonthlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"22,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Monthly,
                                          StoreType::Averaged,
                                          23,
                                          -999,
                                          "indexGroup",
                                          "23",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          "scheduleName");
        EXPECT_TRUE(state->dataOutputProcessor->TrackingMonthlyVariables);
        state->dataOutputProcessor->TrackingMonthlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"23,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute],scheduleName"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Monthly,
                                          StoreType::Averaged,
                                          24,
                                          -999,
                                          "indexGroup",
                                          "24",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::System,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingMonthlyVariables);
        state->dataOutputProcessor->TrackingMonthlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"24,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Monthly,
                                          StoreType::Averaged,
                                          25,
                                          -999,
                                          "indexGroup",
                                          "25",
                                          "keyedValue",
                                          "variableName",
                                          aThirdTimeStepType,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingMonthlyVariables);
        state->dataOutputProcessor->TrackingMonthlyVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"25,9,keyedValue,variableName [m3/s] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Simulation,
                                          StoreType::Averaged,
                                          26,
                                          -999,
                                          "indexGroup",
                                          "26",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingRunPeriodVariables);
        state->dataOutputProcessor->TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"26,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Simulation,
                                          StoreType::Summed,
                                          27,
                                          -999,
                                          "indexGroup",
                                          "27",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingRunPeriodVariables);
        state->dataOutputProcessor->TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"27,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Simulation,
                                          StoreType::Averaged,
                                          28,
                                          -999,
                                          "indexGroup",
                                          "28",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::Zone,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          "scheduleName");
        EXPECT_TRUE(state->dataOutputProcessor->TrackingRunPeriodVariables);
        state->dataOutputProcessor->TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(delimited_string(
            {"28,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute],scheduleName"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Simulation,
                                          StoreType::Averaged,
                                          29,
                                          -999,
                                          "indexGroup",
                                          "29",
                                          "keyedValue",
                                          "variableName",
                                          OutputProcessor::TimeStepType::System,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingRunPeriodVariables);
        state->dataOutputProcessor->TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"29,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));

        WriteReportVariableDictionaryItem(*state,
                                          ReportingFrequency::Simulation,
                                          StoreType::Averaged,
                                          30,
                                          -999,
                                          "indexGroup",
                                          "30",
                                          "keyedValue",
                                          "variableName",
                                          aThirdTimeStepType,
                                          OutputProcessor::Unit::m3_s,
                                          _,
                                          {});
        EXPECT_TRUE(state->dataOutputProcessor->TrackingRunPeriodVariables);
        state->dataOutputProcessor->TrackingRunPeriodVariables = false;
        EXPECT_TRUE(compare_eso_stream(
            delimited_string({"30,11,keyedValue,variableName [m3/s] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"}, "\n")));

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary(
            {{"1", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"2", "0", "Sum", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"3", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Zone Timestep", "scheduleName", "m3/s"},
             {"4", "0", "Avg", "indexGroup", timeStepSystemString, "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"5", "0", "Avg", "indexGroup", aThirdTimeStepString, "keyedValue", "variableName", "Zone Timestep", "", "m3/s"},
             {"6", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"7", "0", "Sum", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"8", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "HVAC System Timestep", "scheduleName", "m3/s"},
             {"9", "0", "Avg", "indexGroup", timeStepSystemString, "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"10", "0", "Avg", "indexGroup", aThirdTimeStepString, "keyedValue", "variableName", "HVAC System Timestep", "", "m3/s"},
             {"11", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"12", "0", "Sum", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"13", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Hourly", "scheduleName", "m3/s"},
             {"14", "0", "Avg", "indexGroup", timeStepSystemString, "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"15", "0", "Avg", "indexGroup", aThirdTimeStepString, "keyedValue", "variableName", "Hourly", "", "m3/s"},
             {"16", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"17", "0", "Sum", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"18", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Daily", "scheduleName", "m3/s"},
             {"19", "0", "Avg", "indexGroup", timeStepSystemString, "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"20", "0", "Avg", "indexGroup", aThirdTimeStepString, "keyedValue", "variableName", "Daily", "", "m3/s"},
             {"21", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"22", "0", "Sum", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"23", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Monthly", "scheduleName", "m3/s"},
             {"24", "0", "Avg", "indexGroup", timeStepSystemString, "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"25", "0", "Avg", "indexGroup", aThirdTimeStepString, "keyedValue", "variableName", "Monthly", "", "m3/s"},
             {"26", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Run Period", "", "m3/s"},
             {"27", "0", "Sum", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Run Period", "", "m3/s"},
             {"28", "0", "Avg", "indexGroup", timeStepZoneString, "keyedValue", "variableName", "Run Period", "scheduleName", "m3/s"},
             {"29", "0", "Avg", "indexGroup", timeStepSystemString, "keyedValue", "variableName", "Run Period", "", "m3/s"},
             {"30", "0", "Avg", "indexGroup", aThirdTimeStepString, "keyedValue", "variableName", "Run Period", "", "m3/s"}});
        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);
    }

    TEST_F(SQLiteFixture, OutputProcessor_writeCumulativeReportMeterData)
    {
        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);

        WriteCumulativeReportMeterData(*state, 1, "1", 616771620.98702729, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"}, "\n")));

        WriteCumulativeReportMeterData(*state, 1, "1", 616771620.98702729, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,616771620.9870273"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,616771620.9870273"}, "\n")));

        WriteCumulativeReportMeterData(*state, 1, "1", 0, true);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,0.0"}, "\n")));

        WriteCumulativeReportMeterData(*state, 1, "1", 0, false);
        EXPECT_TRUE(compare_mtr_stream(delimited_string({"1,0.0"}, "\n")));
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0"}, "\n")));

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
        state->dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord(4, 1, 1, 0, 2017);
        state->dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(
            1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false);

        WriteNumericData(*state, 1, "1", 0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0"}, "\n")));

        WriteNumericData(*state, 1, "1", 0.1);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.1"}, "\n")));

        WriteNumericData(*state, 1, "1", -0.1);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-0.1"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e-2);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.01"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.01"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-3);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.001"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.001"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-4);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0001"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.0001"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-5);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.00001"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.00001"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-6);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.000001"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.000001"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-7);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-7"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-7"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-8);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-8"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-8"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-9);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-9"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-9"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-10);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-10"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-10"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-11);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-11"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-11"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-12);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-12"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-12"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-13);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-13"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-13"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-14);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-14"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-14"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-15);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-15"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-15"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-16);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-16"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-16"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", -1.0e-16);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-1e-16"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-1e-16"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e-19);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-19"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e-19"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 0.5);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,0.5"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 10.0);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e2);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e3);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e4);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e5);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e6);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e7);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e8);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e9);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e10);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e11);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e12);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e13);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e14);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e15);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1000000000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e16);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,10000000000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", 1.0e17);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000000000.0"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,100000000000000000.0"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", -1.0e16);
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-10000000000000000.0"}, "\n")));

        WriteNumericData(*state, 1, "1", -1.0e17);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-100000000000000000.0"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,-100000000000000000.0"}, "\n")));
#endif

        WriteNumericData(*state, 1, "1", 1.0e25);
#if defined(_WIN32) && _MSC_VER < 1900
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e25"}, "\n")));
#else
        EXPECT_TRUE(compare_eso_stream(delimited_string({"1,1e25"}, "\n")));
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

        EXPECT_EQ(0, state->dataOutputProcessor->NumEnergyMeters);
        EXPECT_EQ(0ul, state->dataOutputProcessor->EnergyMeters.size());

        AddMeter(*state, name, units, resourceType, endUse, endUseSub, group);

        ASSERT_EQ(1, state->dataOutputProcessor->NumEnergyMeters);
        ASSERT_EQ(1ul, state->dataOutputProcessor->EnergyMeters.size());

        EXPECT_EQ(name, state->dataOutputProcessor->EnergyMeters(1).Name);
        EXPECT_EQ(resourceType, state->dataOutputProcessor->EnergyMeters(1).ResourceType);
        EXPECT_EQ(endUse, state->dataOutputProcessor->EnergyMeters(1).EndUse);
        EXPECT_EQ(endUseSub, state->dataOutputProcessor->EnergyMeters(1).EndUseSub);
        EXPECT_EQ(group, state->dataOutputProcessor->EnergyMeters(1).Group);
        EXPECT_TRUE(compare_enums(units, state->dataOutputProcessor->EnergyMeters(1).Units));
        EXPECT_EQ(1, state->dataOutputProcessor->EnergyMeters(1).TSRptNum);
        EXPECT_EQ(2, state->dataOutputProcessor->EnergyMeters(1).HRRptNum);
        EXPECT_EQ(3, state->dataOutputProcessor->EnergyMeters(1).DYRptNum);
        EXPECT_EQ(4, state->dataOutputProcessor->EnergyMeters(1).MNRptNum);
        EXPECT_EQ(5, state->dataOutputProcessor->EnergyMeters(1).YRRptNum);
        EXPECT_EQ(6, state->dataOutputProcessor->EnergyMeters(1).SMRptNum);
        EXPECT_EQ(7, state->dataOutputProcessor->EnergyMeters(1).TSAccRptNum);
        EXPECT_EQ(8, state->dataOutputProcessor->EnergyMeters(1).HRAccRptNum);
        EXPECT_EQ(9, state->dataOutputProcessor->EnergyMeters(1).DYAccRptNum);
        EXPECT_EQ(10, state->dataOutputProcessor->EnergyMeters(1).MNAccRptNum);
        EXPECT_EQ(11, state->dataOutputProcessor->EnergyMeters(1).YRAccRptNum);
        EXPECT_EQ(12, state->dataOutputProcessor->EnergyMeters(1).SMAccRptNum);

        EXPECT_EQ(1, state->dataOutputProcessor->NumEnergyMeters);
        EXPECT_EQ(1ul, state->dataOutputProcessor->EnergyMeters.size());

        state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        auto const name2("testMeter2");
        OutputProcessor::Unit const units2(OutputProcessor::Unit::unknown); // was "kwh"
        auto const resourceType2("OTHER");
        auto const endUse2("testEndUse2");
        auto const endUseSub2("testEndUseSub2");
        auto const group2("testGroup2");
        AddMeter(*state, name2, units2, resourceType2, endUse2, endUseSub2, group2);

        auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

        ASSERT_EQ(1ul, errorData.size());
        std::vector<std::string> errorData0{"1",
                                            "1",
                                            "0",
                                            "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[unknown].  ..on "
                                            "Meter=\"testMeter2\".  ..requests for IP units from this meter will be ignored.",
                                            "1"};
        EXPECT_EQ(errorData0, errorData[0]);

        ASSERT_EQ(2, state->dataOutputProcessor->NumEnergyMeters);
        ASSERT_EQ(2ul, state->dataOutputProcessor->EnergyMeters.size());
    }

    TEST_F(SQLiteFixture, OutputProcessor_validateNStandardizeMeterTitles)
    {
        std::vector<std::vector<std::string>> input_map = {{"J", "ELECTRICITY", "INTERIOR LIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "INTERIOR LIGHTS", "endUseSub", "SYSTEM"},
                                                           {"J", "ELECTRICITY", "INTERIOR LIGHTS", "endUseSub", "PLANT"},
                                                           {"J", "ELECTRICITY", "INTERIOR LIGHTS", "endUseSub", "BUILDING", "zoneName"},
                                                           {"J", "ELECTRICITY", "INTERIORLIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "EXTERIOR LIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "EXTERIORLIGHTS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HTG", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATPRODUCED", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "CLG", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "DOMESTICHOTWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "DHW", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "DOMESTIC HOT WATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COGEN", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COGENERATION", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "INTERIOREQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "INTERIOR EQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "EXTERIOREQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "EXTERIOR EQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "EXT EQ", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "EXTERIOREQ", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "EXTERIOR:WATEREQUIPMENT", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASEDHOTWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "DISTRICTHOTWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASED HEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASEDCOLDWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "DISTRICTCHILLEDWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASEDCHILLEDWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASED COLD WATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASED COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "FANS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "FAN", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATINGCOILS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATINGCOIL", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATING COILS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATING COIL", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COOLINGCOILS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COOLINGCOIL", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COOLING COILS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COOLING COIL", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PUMPS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PUMP", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "FREECOOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "FREE COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "LOOPTOLOOP", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "CHILLERS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "CHILLER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "BOILERS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "BOILER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "BASEBOARD", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "BASEBOARDS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATREJECTION", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEAT REJECTION", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HUMIDIFIER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HUMIDIFIERS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATRECOVERY", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEAT RECOVERY", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PHOTOVOLTAICS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PV", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PHOTOVOLTAIC", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "WINDTURBINES", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "WT", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "WINDTURBINE", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "ELECTRICSTORAGE", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEAT RECOVERY FOR COOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATRECOVERYFORCOOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATRECOVERYCOOLING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEAT RECOVERY FOR HEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATRECOVERYFORHEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "HEATRECOVERYHEATING", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "ELECTRICITYEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASEDELECTRICITYEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "SOLDELECTRICITYEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "NATURALGASEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "FUELOILNO1EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "FUELOILNO2EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COALEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "GASOLINEEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PROPANEEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "DIESELEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "OTHERFUEL1EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "OTHERFUEL2EMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "CARBONEQUIVALENTEMISSIONS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "REFRIGERATION", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COLDSTORAGECHARGE", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "COLDSTORAGEDISCHARGE", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "WATERSYSTEMS", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "WATERSYSTEM", "endUseSub", "HVAC"},
                                                           // { "J", "ELECTRICITY", "Water System", "endUseSub", "HVAC" },  // This one fails because
                                                           // Water System isn't a proper choice (needs to be upper cased in code...)
                                                           {"J", "ELECTRICITY", "RAINWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "CONDENSATE", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "WELLWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "MAINSWATER", "endUseSub", "HVAC"},
                                                           {"J", "ELECTRICITY", "PURCHASEDWATER", "endUseSub", "HVAC"}};

        std::vector<std::string> const result_map = {"Electricity:Facility",
                                                     "Electricity:HVAC",
                                                     "InteriorLights:Electricity",
                                                     "endUseSub:InteriorLights:Electricity",
                                                     "Electricity:Plant",
                                                     "Electricity:Building",
                                                     "Electricity:Zone:zoneName",
                                                     "InteriorLights:Electricity:Zone:zoneName",
                                                     "endUseSub:InteriorLights:Electricity:Zone:zoneName",
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
                                                     "ElectricityEmissions:Electricity",
                                                     "endUseSub:ElectricityEmissions:Electricity",
                                                     "PurchasedElectricityEmissions:Electricity",
                                                     "endUseSub:PurchasedElectricityEmissions:Electricity",
                                                     "SoldElectricityEmissions:Electricity",
                                                     "endUseSub:SoldElectricityEmissions:Electricity",
                                                     "NaturalGasEmissions:Electricity",
                                                     "endUseSub:NaturalGasEmissions:Electricity",
                                                     "FuelOilNo1Emissions:Electricity",
                                                     "endUseSub:FuelOilNo1Emissions:Electricity",
                                                     "FuelOilNo2Emissions:Electricity",
                                                     "endUseSub:FuelOilNo2Emissions:Electricity",
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
                ValidateNStandardizeMeterTitles(*state,
                                                OutputProcessor::Unit::J,
                                                meter[1],
                                                meter[2],
                                                meter[3],
                                                meter[4],
                                                errorFound,
                                                "",
                                                ""); // the first argument was  meter[ 0 ]
            } else if (meter.size() == 6) {
                ValidateNStandardizeMeterTitles(*state,
                                                OutputProcessor::Unit::J,
                                                meter[1],
                                                meter[2],
                                                meter[3],
                                                meter[4],
                                                errorFound,
                                                meter[5],
                                                ""); // the first argument was  meter[ 0 ]
            }
            EXPECT_FALSE(errorFound);
        }

        ASSERT_EQ(103, state->dataOutputProcessor->NumEnergyMeters);
        ASSERT_EQ(103ul, state->dataOutputProcessor->EnergyMeters.size());

        for (int i = 0; i < state->dataOutputProcessor->NumEnergyMeters; ++i) {
            EXPECT_EQ(result_map[i], state->dataOutputProcessor->EnergyMeters(i + 1).Name);
        }

        state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

        OutputProcessor::Unit units = OutputProcessor::Unit::J;
        std::string resourceType = "ELECTRICITY";
        std::string endUse = "INTERIOR LIGHTS";
        std::string endUseSub = "endUseSub";
        std::string group = "BAD INPUT";
        errorFound = false;

        ValidateNStandardizeMeterTitles(*state, units, resourceType, endUse, endUseSub, group, errorFound, "", "");
        EXPECT_TRUE(errorFound);

        units = OutputProcessor::Unit::J;
        resourceType = "ELECTRICITY";
        endUse = "BAD INPUT";
        endUseSub = "endUseSub";
        group = "HVAC";
        errorFound = false;

        ValidateNStandardizeMeterTitles(*state, units, resourceType, endUse, endUseSub, group, errorFound, "", "");
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
        // OutputProcessor::TimeValue.allocate(2);

        auto timeStep = 1.0;

        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::Zone, timeStep);

        EXPECT_DOUBLE_EQ(timeStep, *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).TimeStep);
        EXPECT_DOUBLE_EQ(0.0, state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).CurMinute);

        timeStep = 2.0;

        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::HVAC, timeStep);

        EXPECT_DOUBLE_EQ(timeStep, *state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).TimeStep);
        EXPECT_DOUBLE_EQ(0.0, state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).CurMinute);
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

        GetReportVariableInput(*state);

        state->dataOutputProcessor->NumOfReqVariables = state->dataInputProcessing->inputProcessor->getNumObjectsFound(*state, "Output:Variable");

        EXPECT_EQ(5, state->dataOutputProcessor->NumOfReqVariables);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(1).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::TimeStep, state->dataOutputProcessor->ReqRepVars(1).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(1).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(2).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Hourly, state->dataOutputProcessor->ReqRepVars(2).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(2).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(3).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Daily, state->dataOutputProcessor->ReqRepVars(3).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(3).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(4).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Monthly, state->dataOutputProcessor->ReqRepVars(4).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(4).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(5).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Simulation, state->dataOutputProcessor->ReqRepVars(5).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(5).Used);
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

        GetReportVariableInput(*state);

        auto const keyed_value = "ENVIRONMENT";
        auto const var_name = "SITE OUTDOOR AIR DRYBULB TEMPERATURE";

        BuildKeyVarList(*state, keyed_value, var_name, 1, 6);

        EXPECT_EQ(0, state->dataOutputProcessor->NumExtraVars);
        EXPECT_EQ(6, state->dataOutputProcessor->NumOfReqVariables);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(1).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::TimeStep, state->dataOutputProcessor->ReqRepVars(1).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(1).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(2).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Hourly, state->dataOutputProcessor->ReqRepVars(2).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(2).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(3).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Daily, state->dataOutputProcessor->ReqRepVars(3).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(3).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(4).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Monthly, state->dataOutputProcessor->ReqRepVars(4).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(4).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(5).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Simulation, state->dataOutputProcessor->ReqRepVars(5).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(5).Used);
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

        state->dataInputProcessing->inputProcessor->preScanReportingVariables(*state);
        InitializeOutput(*state);

        Real64 ilgrGarage;
        Real64 ilgrLiving;
        Real64 ilgrAttic;

        SetupOutputVariable(*state,
                            "Zone Total Internal Latent Gain Rate",
                            OutputProcessor::Unit::J,
                            ilgrGarage,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Garage");
        SetupOutputVariable(*state,
                            "Zone Total Internal Latent Gain Rate",
                            OutputProcessor::Unit::J,
                            ilgrLiving,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Living");
        SetupOutputVariable(*state,
                            "Zone Total Internal Latent Gain Rate",
                            OutputProcessor::Unit::J,
                            ilgrAttic,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Attic");

        Real64 isgrGarage;
        Real64 isgrLiving;
        Real64 isgrAttic;

        SetupOutputVariable(*state,
                            "Zone Total Internal Sensible Gain Rate",
                            OutputProcessor::Unit::J,
                            isgrGarage,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Garage");
        SetupOutputVariable(*state,
                            "Zone Total Internal Sensible Gain Rate",
                            OutputProcessor::Unit::J,
                            isgrLiving,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Living");
        SetupOutputVariable(*state,
                            "Zone Total Internal Sensible Gain Rate",
                            OutputProcessor::Unit::J,
                            isgrAttic,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Attic");

        state->dataGlobal->DoWeathSim = true;
        state->dataGlobal->TimeStepZone = 0.25;

        OutputReportTabular::GetInputTabularMonthly(*state);
        EXPECT_EQ(state->dataOutRptTab->MonthlyInputCount, 1);
        OutputReportTabular::InitializeTabularMonthly(*state);

        GetReportVariableInput(*state);

        state->dataOutputProcessor->NumExtraVars = 0;
        BuildKeyVarList(*state, "LIVING", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 3);
        EXPECT_EQ(1, state->dataOutputProcessor->NumExtraVars);

        state->dataOutputProcessor->NumExtraVars = 0;
        BuildKeyVarList(*state, "GARAGE", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 3);
        EXPECT_EQ(0, state->dataOutputProcessor->NumExtraVars);

        state->dataOutputProcessor->NumExtraVars = 0;
        BuildKeyVarList(*state, "ATTIC", "ZONE TOTAL INTERNAL SENSIBLE GAIN RATE", 1, 3);
        EXPECT_EQ(0, state->dataOutputProcessor->NumExtraVars);
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

        state->dataInputProcessing->inputProcessor->preScanReportingVariables(*state);
        InitializeOutput(*state);

        Real64 ilgrGarage;
        Real64 ilgrLiving1;
        Real64 ilgrLiving2;

        SetupOutputVariable(*state,
                            "Zone Total Internal Latent Gain Rate",
                            OutputProcessor::Unit::J,
                            ilgrGarage,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Garage");
        SetupOutputVariable(*state,
                            "Zone Total Internal Latent Gain Rate",
                            OutputProcessor::Unit::J,
                            ilgrLiving1,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Living1");
        SetupOutputVariable(*state,
                            "Zone Total Internal Latent Gain Rate",
                            OutputProcessor::Unit::J,
                            ilgrLiving2,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Living2");

        Real64 isgrGarage;
        Real64 isgrLiving;
        Real64 isgrAttic;

        SetupOutputVariable(*state,
                            "Zone Total Internal Sensible Gain Rate",
                            OutputProcessor::Unit::J,
                            isgrGarage,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Garage");
        SetupOutputVariable(*state,
                            "Zone Total Internal Sensible Gain Rate",
                            OutputProcessor::Unit::J,
                            isgrLiving,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Living1");
        SetupOutputVariable(*state,
                            "Zone Total Internal Sensible Gain Rate",
                            OutputProcessor::Unit::J,
                            isgrAttic,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "Living2");

        state->dataGlobal->DoWeathSim = true;
        state->dataGlobal->TimeStepZone = 0.25;

        OutputReportTabular::GetInputTabularMonthly(*state);
        EXPECT_EQ(state->dataOutRptTab->MonthlyInputCount, 1);
        OutputReportTabular::InitializeTabularMonthly(*state);

        GetReportVariableInput(*state);

        state->dataOutputProcessor->NumExtraVars = 0;
        BuildKeyVarList(*state, "LIVING1", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 2);
        EXPECT_EQ(1, state->dataOutputProcessor->NumExtraVars);

        state->dataOutputProcessor->NumExtraVars = 0;
        BuildKeyVarList(*state, "GARAGE", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 1, 2);
        EXPECT_EQ(0, state->dataOutputProcessor->NumExtraVars);
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

        InitializeOutput(*state);

        GetReportVariableInput(*state);

        auto const var_name = "Site Outdoor Air Drybulb Temperature";

        AddBlankKeys(*state, var_name, 1, 5);

        EXPECT_EQ(5, state->dataOutputProcessor->NumExtraVars);
        EXPECT_EQ(1, state->dataOutputProcessor->ReportList(1));
        EXPECT_EQ(2, state->dataOutputProcessor->ReportList(2));
        EXPECT_EQ(3, state->dataOutputProcessor->ReportList(3));
        EXPECT_EQ(4, state->dataOutputProcessor->ReportList(4));
        EXPECT_EQ(5, state->dataOutputProcessor->ReportList(5));
        EXPECT_EQ(5, state->dataOutputProcessor->NumOfReqVariables);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(1).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::TimeStep, state->dataOutputProcessor->ReqRepVars(1).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(1).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(2).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Hourly, state->dataOutputProcessor->ReqRepVars(2).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(2).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(3).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Daily, state->dataOutputProcessor->ReqRepVars(3).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(3).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(4).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Monthly, state->dataOutputProcessor->ReqRepVars(4).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(4).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(5).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Simulation, state->dataOutputProcessor->ReqRepVars(5).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).SchedName);
        EXPECT_FALSE(state->dataOutputProcessor->ReqRepVars(5).Used);
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

        for (auto const &option : valid_options) {
            report_freq = determineFrequency(*state, option.first);
            EXPECT_TRUE(compare_enums(option.second, report_freq));
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

        AddToOutputVariableList(*state,
                                "Site Outdoor Air Drybulb Temperature",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Real,
                                OutputProcessor::Unit::C);
        AddToOutputVariableList(*state,
                                "Site Outdoor Air Wetbulb Temperature",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Real,
                                OutputProcessor::Unit::C);
        AddToOutputVariableList(*state,
                                "Site Outdoor Air Humidity Ratio",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Real,
                                OutputProcessor::Unit::kgWater_kgDryAir);
        AddToOutputVariableList(*state,
                                "Site Outdoor Air Relative Humidity",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Real,
                                OutputProcessor::Unit::Perc);

        EXPECT_TRUE(compare_enums(OutputProcessor::TimeStepType::Zone, state->dataOutputProcessor->DDVariableTypes(1).timeStepType));
        EXPECT_TRUE(compare_enums(StoreType::Averaged, state->dataOutputProcessor->DDVariableTypes(1).storeType));
        EXPECT_TRUE(compare_enums(VariableType::Real, state->dataOutputProcessor->DDVariableTypes(1).variableType));
        EXPECT_EQ(0, state->dataOutputProcessor->DDVariableTypes(1).Next);
        EXPECT_FALSE(state->dataOutputProcessor->DDVariableTypes(1).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Drybulb Temperature", state->dataOutputProcessor->DDVariableTypes(1).VarNameOnly);
        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::C, state->dataOutputProcessor->DDVariableTypes(1).units));

        EXPECT_TRUE(compare_enums(OutputProcessor::TimeStepType::Zone, state->dataOutputProcessor->DDVariableTypes(2).timeStepType));
        EXPECT_TRUE(compare_enums(StoreType::Averaged, state->dataOutputProcessor->DDVariableTypes(2).storeType));
        EXPECT_TRUE(compare_enums(VariableType::Real, state->dataOutputProcessor->DDVariableTypes(2).variableType));
        EXPECT_EQ(0, state->dataOutputProcessor->DDVariableTypes(2).Next);
        EXPECT_FALSE(state->dataOutputProcessor->DDVariableTypes(2).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Wetbulb Temperature", state->dataOutputProcessor->DDVariableTypes(2).VarNameOnly);
        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::C, state->dataOutputProcessor->DDVariableTypes(2).units));

        EXPECT_TRUE(compare_enums(OutputProcessor::TimeStepType::Zone, state->dataOutputProcessor->DDVariableTypes(3).timeStepType));
        EXPECT_TRUE(compare_enums(StoreType::Averaged, state->dataOutputProcessor->DDVariableTypes(3).storeType));
        EXPECT_TRUE(compare_enums(VariableType::Real, state->dataOutputProcessor->DDVariableTypes(3).variableType));
        EXPECT_EQ(0, state->dataOutputProcessor->DDVariableTypes(3).Next);
        EXPECT_FALSE(state->dataOutputProcessor->DDVariableTypes(3).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Humidity Ratio", state->dataOutputProcessor->DDVariableTypes(3).VarNameOnly);
        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::kgWater_kgDryAir, state->dataOutputProcessor->DDVariableTypes(3).units));

        EXPECT_TRUE(compare_enums(OutputProcessor::TimeStepType::Zone, state->dataOutputProcessor->DDVariableTypes(4).timeStepType));
        EXPECT_TRUE(compare_enums(StoreType::Averaged, state->dataOutputProcessor->DDVariableTypes(4).storeType));
        EXPECT_TRUE(compare_enums(VariableType::Real, state->dataOutputProcessor->DDVariableTypes(4).variableType));
        EXPECT_EQ(0, state->dataOutputProcessor->DDVariableTypes(4).Next);
        EXPECT_FALSE(state->dataOutputProcessor->DDVariableTypes(4).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Relative Humidity", state->dataOutputProcessor->DDVariableTypes(4).VarNameOnly);
        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::Perc, state->dataOutputProcessor->DDVariableTypes(4).units));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput(*state);
        SetupOutputVariable(*state,
                            "Site Outdoor Air Drybulb Temperature",
                            OutputProcessor::Unit::C,
                            state->dataEnvrn->OutDryBulbTemp,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            "Environment");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary(
            {{"1", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"}});
        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        EXPECT_EQ(1, state->dataOutputProcessor->NumExtraVars);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(1).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Simulation, state->dataOutputProcessor->ReqRepVars(1).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).SchedName);
        EXPECT_EQ(true, state->dataOutputProcessor->ReqRepVars(1).Used);

        EXPECT_TRUE(compare_enums(OutputProcessor::TimeStepType::Zone, state->dataOutputProcessor->DDVariableTypes(1).timeStepType));
        EXPECT_TRUE(compare_enums(StoreType::Averaged, state->dataOutputProcessor->DDVariableTypes(1).storeType));
        EXPECT_TRUE(compare_enums(VariableType::Real, state->dataOutputProcessor->DDVariableTypes(1).variableType));
        EXPECT_EQ(0, state->dataOutputProcessor->DDVariableTypes(1).Next);
        EXPECT_FALSE(state->dataOutputProcessor->DDVariableTypes(1).ReportedOnDDFile);
        EXPECT_EQ("Site Outdoor Air Drybulb Temperature", state->dataOutputProcessor->DDVariableTypes(1).VarNameOnly);
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_setupOutputVariable_endUseSubKey)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Chiller Electricity Energy,runperiod;",
            "Output:Variable,*,Lights Electricity Energy,runperiod;",
            "Output:Variable,*,Environmental Impact Fuel Oil No 2 CO2 Emissions Mass,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput(*state);

        Real64 cooling_consumption = 0.;
        SetupOutputVariable(*state,
                            "Chiller Electricity Energy",
                            OutputProcessor::Unit::J,
                            cooling_consumption,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "Cool-1",
                            _,
                            "ELECTRICITY",
                            "Cooling",
                            _, // EndUseSubKey
                            "Plant");

        Real64 light_consumption = 0.;
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "RailroadCrossing", // EndUseSubKey
                            "Building",
                            "SPACE1-1",
                            1,
                            1);

        Real64 fuel_oil_co2 = 0.;
        SetupOutputVariable(*state,
                            "Environmental Impact Fuel Oil No 2 CO2 Emissions Mass",
                            OutputProcessor::Unit::kg,
                            fuel_oil_co2,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "Site",
                            _,
                            "CO2",
                            "FuelOilNo2Emissions",
                            _, // EndUseSubKey
                            "");

        int found;

        // Cooling
        // testing an ABUPS end use with no sub end use specified
        EXPECT_EQ(1, state->dataOutputProcessor->EndUseCategory(2).NumSubcategories);
        EXPECT_EQ("General", state->dataOutputProcessor->EndUseCategory(2).SubcategoryName(1));

        found = UtilityRoutines::FindItem("Cooling:Electricity", state->dataOutputProcessor->EnergyMeters);
        EXPECT_NE(0, found);
        EXPECT_EQ("Electricity", state->dataOutputProcessor->EnergyMeters(found).ResourceType);
        EXPECT_EQ("Cooling", state->dataOutputProcessor->EnergyMeters(found).EndUse);
        EXPECT_EQ("", state->dataOutputProcessor->EnergyMeters(found).EndUseSub);

        found = UtilityRoutines::FindItem("General:Cooling:Electricity", state->dataOutputProcessor->EnergyMeters);
        EXPECT_NE(0, found);
        EXPECT_EQ("Electricity", state->dataOutputProcessor->EnergyMeters(found).ResourceType);
        EXPECT_EQ("Cooling", state->dataOutputProcessor->EnergyMeters(found).EndUse);
        EXPECT_EQ("General", state->dataOutputProcessor->EnergyMeters(found).EndUseSub);

        // lighting
        // testing an ABUPS end use with a sub end use specified
        EXPECT_EQ(1, state->dataOutputProcessor->EndUseCategory(3).NumSubcategories); // lighting end use
        EXPECT_EQ("RailroadCrossing", state->dataOutputProcessor->EndUseCategory(3).SubcategoryName(1));

        found = UtilityRoutines::FindItem("InteriorLights:Electricity", state->dataOutputProcessor->EnergyMeters);
        EXPECT_NE(0, found);
        EXPECT_EQ("Electricity", state->dataOutputProcessor->EnergyMeters(found).ResourceType);
        EXPECT_EQ("InteriorLights", state->dataOutputProcessor->EnergyMeters(found).EndUse);
        EXPECT_EQ("", state->dataOutputProcessor->EnergyMeters(found).EndUseSub);

        found = UtilityRoutines::FindItem("General:InteriorLights:Electricity", state->dataOutputProcessor->EnergyMeters);
        EXPECT_EQ(0, found); // should not find this

        found = UtilityRoutines::FindItem("RailroadCrossing:InteriorLights:Electricity", state->dataOutputProcessor->EnergyMeters);
        EXPECT_NE(0, found);
        EXPECT_EQ("Electricity", state->dataOutputProcessor->EnergyMeters(found).ResourceType);
        EXPECT_EQ("InteriorLights", state->dataOutputProcessor->EnergyMeters(found).EndUse);
        EXPECT_EQ("RailroadCrossing", state->dataOutputProcessor->EnergyMeters(found).EndUseSub);

        // fuel oil CO2 emissions
        // testing a non-ABUPS end use with no sub end use specified
        found = UtilityRoutines::FindItem("FuelOilNo2Emissions:CO2", state->dataOutputProcessor->EnergyMeters);
        EXPECT_NE(0, found);
        EXPECT_EQ("CO2", state->dataOutputProcessor->EnergyMeters(found).ResourceType);
        EXPECT_EQ("FuelOilNo2Emissions", state->dataOutputProcessor->EnergyMeters(found).EndUse);
        EXPECT_EQ("", state->dataOutputProcessor->EnergyMeters(found).EndUseSub);
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_star)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,*,Boiler NaturalGas Rate,runperiod;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput(*state);
        Real64 fuel_used = 999;
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler1");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler2");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler3");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "HVAC System", "Boiler1", "Boiler NaturalGas Rate", "Run Period", "", "W"},
            {"2", "0", "Avg", "System", "HVAC System", "Boiler2", "Boiler NaturalGas Rate", "Run Period", "", "W"},
            {"3", "0", "Avg", "System", "HVAC System", "Boiler3", "Boiler NaturalGas Rate", "Run Period", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(
            delimited_string({"1,11,Boiler1,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "2,11,Boiler2,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "3,11,Boiler3,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"},
                             "\n"));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,Boiler[13],Boiler NaturalGas Rate,runperiod;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput(*state);
        Real64 fuel_used = 999;
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler1");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler2");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler3");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "HVAC System", "Boiler1", "Boiler NaturalGas Rate", "Run Period", "", "W"},
            {"2", "0", "Avg", "System", "HVAC System", "Boiler3", "Boiler NaturalGas Rate", "Run Period", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(
            delimited_string({"1,11,Boiler1,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "2,11,Boiler3,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"},
                             "\n"));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex_2)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,Boiler.*,Boiler NaturalGas Rate,runperiod;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput(*state);
        Real64 fuel_used = 999;
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler1");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler2");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler3");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "HVAC System", "Boiler1", "Boiler NaturalGas Rate", "Run Period", "", "W"},
            {"2", "0", "Avg", "System", "HVAC System", "Boiler2", "Boiler NaturalGas Rate", "Run Period", "", "W"},
            {"3", "0", "Avg", "System", "HVAC System", "Boiler3", "Boiler NaturalGas Rate", "Run Period", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(
            delimited_string({"1,11,Boiler1,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "2,11,Boiler2,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                              "3,11,Boiler3,Boiler NaturalGas Rate [W] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]"},
                             "\n"));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex_3)
    {
        std::string const idf_objects = delimited_string({"Output:Variable,Zn003:Wall.*,AFN Linkage Node 1 to Node 2 Volume Flow Rate,timestep;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput(*state);
        Real64 vol_flow = 999;
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Zn003:Wall001");
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Zn003:Wall002");
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Zn003:Wall002:Win001");
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Zn003:Wall003");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "HVAC System", "Zn003:Wall001", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"2", "0", "Avg", "System", "HVAC System", "Zn003:Wall002", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"3",
             "0",
             "Avg",
             "System",
             "HVAC System",
             "Zn003:Wall002:Win001",
             "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
             "Zone Timestep",
             "",
             "m3/s"},
            {"4", "0", "Avg", "System", "HVAC System", "Zn003:Wall003", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(delimited_string(
            {
                "1,1,Zn003:Wall001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
                "2,1,Zn003:Wall002,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
                "3,1,Zn003:Wall002:Win001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
                "4,1,Zn003:Wall003,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            },
            "\n"));
    }

    TEST_F(SQLiteFixture, OutputProcessor_setupOutputVariable_regex_4)
    {
        // case-insensitive comparison
        std::string const idf_objects =
            delimited_string({"Output:Variable,(?i)Zn003:Wall.*,AFN Linkage Node 1 to Node 2 Volume Flow Rate,timestep;"});

        ASSERT_TRUE(process_idf(idf_objects));

        GetReportVariableInput(*state);
        Real64 vol_flow = 999;
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "ZN003:WALL001");
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "ZN003:WALL002");
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "ZN003:WALL002:WIN001");
        SetupOutputVariable(*state,
                            "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            vol_flow,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "ZN003:WALL003");

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "System", "HVAC System", "ZN003:WALL001", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"2", "0", "Avg", "System", "HVAC System", "ZN003:WALL002", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
            {"3",
             "0",
             "Avg",
             "System",
             "HVAC System",
             "ZN003:WALL002:WIN001",
             "AFN Linkage Node 1 to Node 2 Volume Flow Rate",
             "Zone Timestep",
             "",
             "m3/s"},
            {"4", "0", "Avg", "System", "HVAC System", "ZN003:WALL003", "AFN Linkage Node 1 to Node 2 Volume Flow Rate", "Zone Timestep", "", "m3/s"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        compare_eso_stream(delimited_string(
            {
                "1,1,ZN003:WALL001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
                "2,1,ZN003:WALL002,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
                "3,1,ZN003:WALL002:WIN001,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
                "4,1,ZN003:WALL003,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s] !TimeStep",
            },
            "\n"));
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

        InitializeOutput(*state);

        GetReportVariableInput(*state);
        CheckReportVariable(*state, keyed_value, var_name);

        EXPECT_EQ(5, state->dataOutputProcessor->NumOfReqVariables);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(1).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::TimeStep, state->dataOutputProcessor->ReqRepVars(1).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(1).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(1).SchedName);
        EXPECT_EQ(true, state->dataOutputProcessor->ReqRepVars(1).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(2).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Hourly, state->dataOutputProcessor->ReqRepVars(2).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(2).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(2).SchedName);
        EXPECT_EQ(true, state->dataOutputProcessor->ReqRepVars(2).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(3).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Daily, state->dataOutputProcessor->ReqRepVars(3).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(3).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(3).SchedName);
        EXPECT_EQ(true, state->dataOutputProcessor->ReqRepVars(3).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(4).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Monthly, state->dataOutputProcessor->ReqRepVars(4).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(4).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(4).SchedName);
        EXPECT_EQ(true, state->dataOutputProcessor->ReqRepVars(4).Used);

        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).Key);
        EXPECT_EQ("SITE OUTDOOR AIR DRYBULB TEMPERATURE", state->dataOutputProcessor->ReqRepVars(5).VarName);
        EXPECT_TRUE(compare_enums(ReportingFrequency::Simulation, state->dataOutputProcessor->ReqRepVars(5).frequency));
        EXPECT_EQ(0, state->dataOutputProcessor->ReqRepVars(5).SchedPtr);
        EXPECT_EQ("", state->dataOutputProcessor->ReqRepVars(5).SchedName);
        EXPECT_EQ(true, state->dataOutputProcessor->ReqRepVars(5).Used);
    }

    TEST_F(SQLiteFixture, OutputProcessor_getMeters_WildCard)
    {
        // Test for #9150
        std::string const idf_objects = delimited_string({"Output:Meter:MeterFileOnly,InteriorLights:Electricity:Zone:*,Monthly;"});

        ASSERT_TRUE(process_idf(idf_objects));

        Real64 light_consumption = 0;
        for (int i = 1; i <= 5; ++i) {
            SetupOutputVariable(*state,
                                "Lights Electricity Energy",
                                OutputProcessor::Unit::J,
                                light_consumption,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Summed,
                                "SPACE" + std::to_string(i) + "LIGHTS",
                                _,
                                "Electricity",
                                "InteriorLights",
                                "GeneralLights",
                                "Building",
                                "SPACE" + std::to_string(i),
                                1,
                                1);
        }

        UpdateMeterReporting(*state);

        compare_mtr_stream(
            delimited_string({"53,9,InteriorLights:Electricity:Zone:SPACE1 [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                              "102,9,InteriorLights:Electricity:Zone:SPACE2 [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                              "139,9,InteriorLights:Electricity:Zone:SPACE3 [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                              "176,9,InteriorLights:Electricity:Zone:SPACE4 [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                              "213,9,InteriorLights:Electricity:Zone:SPACE5 [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]"},
                             "\n"));
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
            "    Lights Electricity Energy,  !- Output Variable or Meter Name 1",
            "    SPACE2-1 Lights 1,       !- Key Name 2",
            "    Lights Electricity Energy,  !- Output Variable or Meter Name 2",
            "    SPACE3-1 Lights 1,       !- Key Name 3",
            "    Lights Electricity Energy,  !- Output Variable or Meter Name 3",
            "    SPACE4-1 Lights 1,       !- Key Name 4",
            "    Lights Electricity Energy,  !- Output Variable or Meter Name 4",
            "    SPACE5-1 Lights 1,       !- Key Name 5",
            "    Lights Electricity Energy;  !- Output Variable or Meter Name 5",
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
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE1-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE2-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE3-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE4-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE5-1",
                            1,
                            1);
        Real64 zone_infil_total_loss = 0;
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1");

        bool errors_found = false;

        GetCustomMeterInput(*state, errors_found);

        ASSERT_FALSE(errors_found);

        ASSERT_EQ(22, state->dataOutputProcessor->NumEnergyMeters);

        auto const meters_result =
            std::map<int,
                     std::tuple<int, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view>>({
                {1, std::make_tuple(0, "Electricity:Facility", "Electricity", "", "", "", "J")},
                {2, std::make_tuple(0, "Electricity:Building", "Electricity", "", "", "Building", "J")},
                {3, std::make_tuple(0, "Electricity:Zone:SPACE1-1", "Electricity", "", "", "Zone", "J")},
                {4, std::make_tuple(0, "InteriorLights:Electricity", "Electricity", "InteriorLights", "", "", "J")},
                {5, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "", "Zone", "J")},
                {6, std::make_tuple(0, "GeneralLights:InteriorLights:Electricity", "Electricity", "InteriorLights", "GeneralLights", "", "J")},
                {7,
                 std::make_tuple(
                     0, "GeneralLights:InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "GeneralLights", "Zone", "J")},
                {8, std::make_tuple(0, "Electricity:Zone:SPACE2-1", "Electricity", "", "", "Zone", "J")},
                {9, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE2-1", "Electricity", "InteriorLights", "", "Zone", "J")},
                {10,
                 std::make_tuple(
                     0, "GeneralLights:InteriorLights:Electricity:Zone:SPACE2-1", "Electricity", "InteriorLights", "GeneralLights", "Zone", "J")},
                {11, std::make_tuple(0, "Electricity:Zone:SPACE3-1", "Electricity", "", "", "Zone", "J")},
                {12, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE3-1", "Electricity", "InteriorLights", "", "Zone", "J")},
                {13,
                 std::make_tuple(
                     0, "GeneralLights:InteriorLights:Electricity:Zone:SPACE3-1", "Electricity", "InteriorLights", "GeneralLights", "Zone", "J")},
                {14, std::make_tuple(0, "Electricity:Zone:SPACE4-1", "Electricity", "", "", "Zone", "J")},
                {15, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE4-1", "Electricity", "InteriorLights", "", "Zone", "J")},
                {16,
                 std::make_tuple(
                     0, "GeneralLights:InteriorLights:Electricity:Zone:SPACE4-1", "Electricity", "InteriorLights", "GeneralLights", "Zone", "J")},
                {17, std::make_tuple(0, "Electricity:Zone:SPACE5-1", "Electricity", "", "", "Zone", "J")},
                {18, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE5-1", "Electricity", "InteriorLights", "", "Zone", "J")},
                {19,
                 std::make_tuple(
                     0, "GeneralLights:InteriorLights:Electricity:Zone:SPACE5-1", "Electricity", "InteriorLights", "GeneralLights", "Zone", "J")},
                {20, std::make_tuple(1, "MYGENERALLIGHTS", "Electricity", "", "", "", "J")},
                {21, std::make_tuple(1, "BUILDING INFILTRATION HEAT LOSS", "Generic", "", "", "", "J")},
                {22, std::make_tuple(2, "MYBUILDINGOTHER", "Electricity", "", "", "", "J")},
            });

        for (auto const &result : meters_result) {
            EXPECT_EQ(std::get<0>(result.second), static_cast<int>(state->dataOutputProcessor->EnergyMeters(result.first).TypeOfMeter));
            EXPECT_EQ(std::get<1>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).Name);
            EXPECT_EQ(std::get<2>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).ResourceType);
            EXPECT_EQ(std::get<3>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).EndUse);
            EXPECT_EQ(std::get<4>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).EndUseSub);
            EXPECT_EQ(std::get<5>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).Group);
            EXPECT_EQ(std::get<6>(result.second), unitEnumToString(state->dataOutputProcessor->EnergyMeters(result.first).Units));
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

        InitializeOutput(*state);

        int meter_array_ptr = -1;
        bool errors_found = false;

        std::string resourceType("Electricity");
        std::string endUse("InteriorLights");
        std::string endUseSub("GeneralLights");
        std::string group("Building");
        std::string const zoneName("SPACE1-1");
        std::string const spaceType("OFFICE");

        AttachMeters(*state, OutputProcessor::Unit::J, resourceType, endUse, endUseSub, group, zoneName, spaceType, 1, meter_array_ptr, errors_found);

        EXPECT_FALSE(errors_found);
        EXPECT_EQ(1, meter_array_ptr);

        ASSERT_EQ(10, state->dataOutputProcessor->NumEnergyMeters);

        auto const meters_result = std::map<
            int,
            std::tuple<int, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view, std::string_view>>({
            {1, std::make_tuple(0, "Electricity:Facility", "Electricity", "", "", "", "J")},
            {2, std::make_tuple(0, "Electricity:Building", "Electricity", "", "", "Building", "J")},
            {3, std::make_tuple(0, "Electricity:Zone:SPACE1-1", "Electricity", "", "", "Zone", "J")},
            {4, std::make_tuple(0, "Electricity:SpaceType:OFFICE", "Electricity", "", "", "SpaceType", "J")},
            {5, std::make_tuple(0, "InteriorLights:Electricity", "Electricity", "InteriorLights", "", "", "J")},
            {6, std::make_tuple(0, "InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "", "Zone", "J")},
            {7, std::make_tuple(0, "InteriorLights:Electricity:SpaceType:OFFICE", "Electricity", "InteriorLights", "", "SpaceType", "J")},
            {8, std::make_tuple(0, "GeneralLights:InteriorLights:Electricity", "Electricity", "InteriorLights", "GeneralLights", "", "J")},
            {9,
             std::make_tuple(
                 0, "GeneralLights:InteriorLights:Electricity:Zone:SPACE1-1", "Electricity", "InteriorLights", "GeneralLights", "Zone", "J")},
            {10,
             std::make_tuple(
                 0, "GeneralLights:InteriorLights:Electricity:SpaceType:OFFICE", "Electricity", "InteriorLights", "GeneralLights", "SpaceType", "J")},
        });

        for (auto const &result : meters_result) {
            EXPECT_EQ(std::get<0>(result.second), static_cast<int>(state->dataOutputProcessor->EnergyMeters(result.first).TypeOfMeter));
            EXPECT_EQ(std::get<1>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).Name);
            EXPECT_EQ(std::get<2>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).ResourceType);
            EXPECT_EQ(std::get<3>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).EndUse);
            EXPECT_EQ(std::get<4>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).EndUseSub);
            EXPECT_EQ(std::get<5>(result.second), state->dataOutputProcessor->EnergyMeters(result.first).Group);
            EXPECT_EQ(std::get<6>(result.second), unitEnumToString(state->dataOutputProcessor->EnergyMeters(result.first).Units));
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

        state->dataGlobal->DayOfSim = 365;
        state->dataGlobal->DayOfSimChr = "365";
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 31;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 3;
        state->dataEnvrn->HolidayIndex = 0;
        state->dataGlobal->HourOfDay = 24;
        state->dataGlobal->NumOfDayInEnvrn = 365;
        state->dataGlobal->MinutesPerTimeStep = 10;

        if (state->dataGlobal->TimeStep == state->dataGlobal->NumOfTimeStepInHour) {
            state->dataGlobal->EndHourFlag = true;
            if (state->dataGlobal->HourOfDay == 24) {
                state->dataGlobal->EndDayFlag = true;
                if ((!state->dataGlobal->WarmupFlag) && (state->dataGlobal->DayOfSim == state->dataGlobal->NumOfDayInEnvrn)) {
                    state->dataGlobal->EndEnvrnFlag = true;
                }
            }
        }

        if (state->dataEnvrn->DayOfMonth == state->dataWeatherManager->EndDayOfMonth(state->dataEnvrn->Month)) {
            state->dataEnvrn->EndMonthFlag = true;
        }

        // OutputProcessor::TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::Zone, timeStep);
        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::HVAC, timeStep);

        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).CurMinute = 50;
        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).CurMinute = 50;

        GetReportVariableInput(*state);
        SetupOutputVariable(*state,
                            "Site Outdoor Air Drybulb Temperature",
                            OutputProcessor::Unit::C,
                            state->dataEnvrn->OutDryBulbTemp,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE1-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE2-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE3-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE4-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE5-1",
                            1,
                            1);
        Real64 zone_infil_total_loss = 999;
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1");

        UpdateMeterReporting(*state);

        UpdateDataandReport(*state, OutputProcessor::TimeStepType::Zone);

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
            {"1", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C"},
            {"2", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"},
            {"3", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C"},
            {"4", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C"},
            {"5", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"},
            {"7", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Zone Timestep", "", "J"},
            {"8", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Hourly", "", "J"},
            {"9", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Daily", "", "J"},
            {"10", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Monthly", "", "J"},
            {"12", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Run Period", "", "J"},
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

        compare_eso_stream(delimited_string(
            {
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
            },
            "\n"));

        compare_mtr_stream(delimited_string(
            {
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
            },
            "\n"));
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
            "Output:Variable,*,Boiler NaturalGas Rate,detailed;",
            "Output:Variable,*,Boiler Heating Rate,detailed;",
            "Output:Meter,Electricity:Facility,detailed;",
            "Output:Meter,Electricity:Facility,timestep;",
            "Output:Meter,Electricity:Facility,hourly;",
            "Output:Meter,Electricity:Facility,daily;",
            "Output:Meter,Electricity:Facility,monthly;",
            "Output:Meter,Electricity:Facility,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        state->dataGlobal->DayOfSim = 365;
        state->dataGlobal->DayOfSimChr = "365";
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 31;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 3;
        state->dataEnvrn->HolidayIndex = 0;
        state->dataGlobal->HourOfDay = 24;
        state->dataGlobal->NumOfDayInEnvrn = 365;
        state->dataGlobal->MinutesPerTimeStep = 10;

        if (state->dataGlobal->TimeStep == state->dataGlobal->NumOfTimeStepInHour) {
            state->dataGlobal->EndHourFlag = true;
            if (state->dataGlobal->HourOfDay == 24) {
                state->dataGlobal->EndDayFlag = true;
                if ((!state->dataGlobal->WarmupFlag) && (state->dataGlobal->DayOfSim == state->dataGlobal->NumOfDayInEnvrn)) {
                    state->dataGlobal->EndEnvrnFlag = true;
                }
            }
        }

        if (state->dataEnvrn->DayOfMonth == state->dataWeatherManager->EndDayOfMonth(state->dataEnvrn->Month)) {
            state->dataEnvrn->EndMonthFlag = true;
        }

        // OutputProcessor::TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::Zone, timeStep);
        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::HVAC, timeStep);

        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).CurMinute = 50;
        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).CurMinute = 50;

        GetReportVariableInput(*state);
        SetupOutputVariable(*state,
                            "Site Outdoor Air Drybulb Temperature",
                            OutputProcessor::Unit::C,
                            state->dataEnvrn->OutDryBulbTemp,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE1-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE2-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE3-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE4-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE5-1",
                            1,
                            1);
        Real64 zone_infil_total_loss = 999;
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1");
        Real64 fuel_used = 999;
        Real64 boiler_load = 999;
        SetupOutputVariable(*state,
                            "Boiler Heating Rate",
                            OutputProcessor::Unit::W,
                            boiler_load,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler1");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler1");

        UpdateMeterReporting(*state);

        UpdateDataandReport(*state, OutputProcessor::TimeStepType::Zone);

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
            {"1", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "HVAC System Timestep", "", "C"},
            {"2", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C"},
            {"3", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"},
            {"4", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C"},
            {"5", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C"},
            {"6", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"},
            {"8", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "HVAC System Timestep", "", "J"},
            {"9", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Hourly", "", "J"},
            {"10", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Daily", "", "J"},
            {"11", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Monthly", "", "J"},
            {"13", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Run Period", "", "J"},
            {"240", "0", "Avg", "System", "HVAC System", "Boiler1", "Boiler Heating Rate", "HVAC System Timestep", "", "W"},
            {"241", "0", "Avg", "System", "HVAC System", "Boiler1", "Boiler NaturalGas Rate", "HVAC System Timestep", "", "W"},
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

        compare_eso_stream(delimited_string(
            {
                "1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Each Call",
                "2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
                "3,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
                "4,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
                "5,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                "6,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                "240,1,Boiler1,Boiler Heating Rate [W] !Each Call",
                "241,1,Boiler1,Boiler NaturalGas Rate [W] !Each Call",
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
            },
            "\n"));

        compare_mtr_stream(delimited_string(
            {
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
            },
            "\n"));
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
            "Output:Variable,*,Boiler NaturalGas Rate,detailed;",
            "Output:Variable,*,Boiler Heating Rate,detailed;",
            "Output:Meter,Electricity:Facility,detailed;",
            "Output:Meter,Electricity:Facility,timestep;",
            "Output:Meter,Electricity:Facility,hourly;",
            "Output:Meter,Electricity:Facility,daily;",
            "Output:Meter,Electricity:Facility,monthly;",
            "Output:Meter,Electricity:Facility,runperiod;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        state->dataGlobal->DayOfSim = 365;
        state->dataGlobal->DayOfSimChr = "365";
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 31;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 3;
        state->dataEnvrn->HolidayIndex = 0;
        state->dataGlobal->HourOfDay = 24;
        state->dataGlobal->NumOfDayInEnvrn = 365;
        state->dataGlobal->MinutesPerTimeStep = 10;

        if (state->dataGlobal->TimeStep == state->dataGlobal->NumOfTimeStepInHour) {
            state->dataGlobal->EndHourFlag = true;
            if (state->dataGlobal->HourOfDay == 24) {
                state->dataGlobal->EndDayFlag = true;
                if ((!state->dataGlobal->WarmupFlag) && (state->dataGlobal->DayOfSim == state->dataGlobal->NumOfDayInEnvrn)) {
                    state->dataGlobal->EndEnvrnFlag = true;
                }
            }
        }

        if (state->dataEnvrn->DayOfMonth == state->dataWeatherManager->EndDayOfMonth(state->dataEnvrn->Month)) {
            state->dataEnvrn->EndMonthFlag = true;
        }

        // OutputProcessor::TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::Zone, timeStep);
        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::HVAC, timeStep);

        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).CurMinute = 50;
        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).CurMinute = 50;

        GetReportVariableInput(*state);
        SetupOutputVariable(*state,
                            "Site Outdoor Air Drybulb Temperature",
                            OutputProcessor::Unit::C,
                            state->dataEnvrn->OutDryBulbTemp,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE1-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE2-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE3-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE4-1",
                            1,
                            1);
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE5-1",
                            1,
                            1);
        Real64 zone_infil_total_loss = 999;
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE2-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE3-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE4-1");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            zone_infil_total_loss,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE5-1");
        Real64 fuel_used = 999;
        Real64 boiler_load = 999;
        SetupOutputVariable(*state,
                            "Boiler Heating Rate",
                            OutputProcessor::Unit::W,
                            boiler_load,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler1");
        SetupOutputVariable(*state,
                            "Boiler NaturalGas Rate",
                            OutputProcessor::Unit::W,
                            fuel_used,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            "Boiler1");

        UpdateMeterReporting(*state);

        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        auto timeResults = queryResult("SELECT * FROM Time;", "Time");

        std::vector<std::vector<std::string>> timeData({
            {"1", "0", "12", "31", "24", "0", "0", "10", "-1", "365", "Tuesday", "0", "0"},
        });

        EXPECT_EQ(timeData, timeResults);

        auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

        std::vector<std::vector<std::string>> reportDataDictionary({
            {"1", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "HVAC System Timestep", "", "C"},
            {"2", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Zone Timestep", "", "C"},
            {"3", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"},
            {"4", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Daily", "", "C"},
            {"5", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Monthly", "", "C"},
            {"6", "0", "Avg", "Zone", "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", "Run Period", "", "C"},
            {"8", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "HVAC System Timestep", "", "J"},
            {"9", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Hourly", "", "J"},
            {"10", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Daily", "", "J"},
            {"11", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Monthly", "", "J"},
            {"13", "1", "Sum", "Facility:Electricity", "Zone", "", "Electricity:Facility", "Run Period", "", "J"},
            {"240", "0", "Avg", "System", "HVAC System", "Boiler1", "Boiler Heating Rate", "HVAC System Timestep", "", "W"},
            {"241", "0", "Avg", "System", "HVAC System", "Boiler1", "Boiler NaturalGas Rate", "HVAC System Timestep", "", "W"},
        });

        EXPECT_EQ(reportDataDictionary, reportDataDictionaryResults);

        auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
        auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

        std::vector<std::vector<std::string>> reportData({
            {"1", "1", "240", "999.0"},
            {"2", "1", "241", "999.0"},
        });

        std::vector<std::vector<std::string>> reportExtendedData({});

        EXPECT_EQ(reportData, reportDataResults);
        EXPECT_EQ(reportExtendedData, reportExtendedDataResults);

        compare_eso_stream(delimited_string(
            {
                "1,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Each Call",
                "2,1,Environment,Site Outdoor Air Drybulb Temperature [C] !TimeStep",
                "3,1,Environment,Site Outdoor Air Drybulb Temperature [C] !Hourly",
                "4,7,Environment,Site Outdoor Air Drybulb Temperature [C] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
                "5,9,Environment,Site Outdoor Air Drybulb Temperature [C] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                "6,11,Environment,Site Outdoor Air Drybulb Temperature [C] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                "240,1,Boiler1,Boiler Heating Rate [W] !Each Call",
                "241,1,Boiler1,Boiler NaturalGas Rate [W] !Each Call",
                "8,1,Electricity:Facility [J] !Each Call",
                "9,1,Electricity:Facility [J] !Hourly",
                "10,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
                "11,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                "13,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
                ",365,12,31, 0,24,50.00,60.00,Tuesday",
                "240,999.0",
                "241,999.0",
            },
            "\n"));

        compare_mtr_stream(delimited_string(
            {
                "8,1,Electricity:Facility [J] !Each Call",
                "9,1,Electricity:Facility [J] !Hourly",
                "10,7,Electricity:Facility [J] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]",
                "11,9,Electricity:Facility [J] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]",
                "13,11,Electricity:Facility [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
            },
            "\n"));
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_ResetAccumulationWhenWarmupComplete)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Zone Ideal Loads Supply Air Total Heating Energy,detailed;",
            "Output:Meter:MeterFileOnly,DistrictHeating:HVAC,detailed;",
            "Output:Variable,*,Zone Ideal Loads Supply Air Total Heating Energy,runperiod;",
            "Output:Meter:MeterFileOnly,DistrictHeating:HVAC,hourly;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        // Setup so that UpdateDataandReport can be called.
        state->dataGlobal->DayOfSim = 365;
        state->dataGlobal->DayOfSimChr = "365";
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 31;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 3;
        state->dataEnvrn->HolidayIndex = 0;
        state->dataGlobal->HourOfDay = 24;
        state->dataGlobal->NumOfDayInEnvrn = 365;
        state->dataGlobal->MinutesPerTimeStep = 10;

        if (state->dataGlobal->TimeStep == state->dataGlobal->NumOfTimeStepInHour) {
            state->dataGlobal->EndHourFlag = true;
            if (state->dataGlobal->HourOfDay == 24) {
                state->dataGlobal->EndDayFlag = true;
                if ((!state->dataGlobal->WarmupFlag) && (state->dataGlobal->DayOfSim == state->dataGlobal->NumOfDayInEnvrn)) {
                    state->dataGlobal->EndEnvrnFlag = true;
                }
            }
        }

        if (state->dataEnvrn->DayOfMonth == state->dataWeatherManager->EndDayOfMonth(state->dataEnvrn->Month)) {
            state->dataEnvrn->EndMonthFlag = true;
        }
        // OutputProcessor::TimeValue.allocate(2);
        auto timeStep = 1.0 / 6;
        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::Zone, timeStep);
        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::HVAC, timeStep);

        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).CurMinute = 10;
        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).CurMinute = 10;

        state->dataGlobal->WarmupFlag = true;

        ReportOutputFileHeaders(*state);

        GetReportVariableInput(*state);
        Array1D<ZonePurchasedAir> PurchAir; // Used to specify purchased air parameters
        PurchAir.allocate(1);
        SetupOutputVariable(*state,
                            "Zone Ideal Loads Supply Air Total Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(1).TotHeatEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            PurchAir(1).Name,
                            _,
                            "DISTRICTHEATING",
                            "Heating",
                            _,
                            "System");

        PurchAir(1).TotHeatEnergy = 1.1;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        PurchAir(1).TotHeatEnergy = 1.3;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        PurchAir(1).TotHeatEnergy = 1.5;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        PurchAir(1).TotHeatEnergy = 1.7;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        PurchAir(1).TotHeatEnergy = 1.9;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        PurchAir(1).TotHeatEnergy = 2.2;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        state->dataGlobal->WarmupFlag = false;

        PurchAir(1).TotHeatEnergy = 2.4;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::Zone); // zone timestep

        compare_eso_stream(delimited_string(
            {
                "1,5,Environment Title[],Latitude[deg],Longitude[deg],Time Zone[],Elevation[m]",
                "2,8,Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType",
                "3,5,Cumulative Day of Simulation[],Month[],Day of Month[],DST Indicator[1=yes 0=no],DayType  ! When Daily Report Variables "
                "Requested",
                "4,2,Cumulative Days of Simulation[],Month[]  ! When Monthly Report Variables Requested",
                "5,1,Cumulative Days of Simulation[] ! When Run Period Report Variables Requested",
                "6,1,Calendar Year of Simulation[] ! When Annual Report Variables Requested",
                "7,1,,Zone Ideal Loads Supply Air Total Heating Energy [J] !Each Call",
                "56,11,,Zone Ideal Loads Supply Air Total Heating Energy [J] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]",
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
                "56,9.7,1.1,12,31,24,20,2.2,12,31,24,70",
            },
            "\n"));

        ResetAccumulationWhenWarmupComplete(*state);

        PurchAir(1).TotHeatEnergy = 100.0;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        PurchAir(1).TotHeatEnergy = 200.0;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::System);

        PurchAir(1).TotHeatEnergy = 300.0;
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::Zone); // zone timestep

        compare_eso_stream(delimited_string(
            {
                "2,365,12,31, 0,24, 0.00,10.00,Tuesday",
                "7,100.0",
                "2,365,12,31, 0,24,10.00,20.00,Tuesday",
                "7,200.0",
                "5,365",
                "56,300.0,100.0,12,31,24,10,200.0,12,31,24,20",
            },
            "\n"));
    }
    TEST_F(EnergyPlusFixture, OutputProcessor_GenOutputVariablesAuditReport)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
            "Output:Variable,*,Boiler NaturalGas Rate,detailed;",
            "Output:Variable,*,Boiler Heating Rate,detailed;",
            "Output:Meter,Electricity:Facility,timestep;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        state->dataGlobal->DayOfSim = 365;
        state->dataGlobal->DayOfSimChr = "365";
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 31;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 3;
        state->dataEnvrn->HolidayIndex = 0;
        state->dataGlobal->HourOfDay = 24;
        state->dataGlobal->NumOfDayInEnvrn = 365;
        state->dataGlobal->MinutesPerTimeStep = 10;

        if (state->dataGlobal->TimeStep == state->dataGlobal->NumOfTimeStepInHour) {
            state->dataGlobal->EndHourFlag = true;
            if (state->dataGlobal->HourOfDay == 24) {
                state->dataGlobal->EndDayFlag = true;
                if ((!state->dataGlobal->WarmupFlag) && (state->dataGlobal->DayOfSim == state->dataGlobal->NumOfDayInEnvrn)) {
                    state->dataGlobal->EndEnvrnFlag = true;
                }
            }
        }

        if (state->dataEnvrn->DayOfMonth == state->dataWeatherManager->EndDayOfMonth(state->dataEnvrn->Month)) {
            state->dataEnvrn->EndMonthFlag = true;
        }

        // OutputProcessor::TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::Zone, timeStep);
        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::HVAC, timeStep);

        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).CurMinute = 50;
        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).CurMinute = 50;

        GetReportVariableInput(*state);
        SetupOutputVariable(*state,
                            "Site Outdoor Air Drybulb Temperature",
                            OutputProcessor::Unit::C,
                            state->dataEnvrn->OutDryBulbTemp,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE1-1",
                            1,
                            1);
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::Zone);

        GenOutputVariablesAuditReport(*state);

        std::string errMsg = delimited_string({
            "   ** Warning ** The following Report Variables were requested but not generated -- check.rdd file",
            "   **   ~~~   ** Either the IDF did not contain these elements, the variable name is misspelled,",
            "   **   ~~~   ** or the requested variable is an advanced output which requires Output : Diagnostics, DisplayAdvancedReportVariables;",
            "   ************* Key=*, VarName=BOILER NATURALGAS RATE, Frequency=Detailed",
            "   ************* Key=*, VarName=BOILER HEATING RATE, Frequency=Detailed",
        });

        compare_err_stream(errMsg);
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_fullOutputVariableKeyComparisonWithRegex)
    {
        std::string const idf_objects = delimited_string({
            "Output:Variable,Air Loop 1|AirSupply InletNode,System Node Setpoint Temperature,Hourly;",
            "Output:Variable,Air Loop 1|AirSupply InletNode,System Node Temperature,Hourly;",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        state->dataGlobal->DayOfSim = 365;
        state->dataGlobal->DayOfSimChr = "365";
        state->dataEnvrn->Month = 12;
        state->dataEnvrn->DayOfMonth = 31;
        state->dataEnvrn->DSTIndicator = 0;
        state->dataEnvrn->DayOfWeek = 3;
        state->dataEnvrn->HolidayIndex = 0;
        state->dataGlobal->HourOfDay = 24;
        state->dataGlobal->NumOfDayInEnvrn = 365;
        state->dataGlobal->MinutesPerTimeStep = 10;

        if (state->dataGlobal->TimeStep == state->dataGlobal->NumOfTimeStepInHour) {
            state->dataGlobal->EndHourFlag = true;
            if (state->dataGlobal->HourOfDay == 24) {
                state->dataGlobal->EndDayFlag = true;
                if ((!state->dataGlobal->WarmupFlag) && (state->dataGlobal->DayOfSim == state->dataGlobal->NumOfDayInEnvrn)) {
                    state->dataGlobal->EndEnvrnFlag = true;
                }
            }
        }

        if (state->dataEnvrn->DayOfMonth == state->dataWeatherManager->EndDayOfMonth(state->dataEnvrn->Month)) {
            state->dataEnvrn->EndMonthFlag = true;
        }

        // OutputProcessor::TimeValue.allocate(2);

        auto timeStep = 1.0 / 6;

        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::Zone, timeStep);
        SetupTimePointers(*state, OutputProcessor::SOVTimeStepType::HVAC, timeStep);

        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::Zone).CurMinute = 50;
        state->dataOutputProcessor->TimeValue.at(OutputProcessor::TimeStepType::System).CurMinute = 50;

        OutputReportTabular::GetInputTabularMonthly(*state);
        OutputReportTabular::InitializeTabularMonthly(*state);

        GetReportVariableInput(*state);
        SetupOutputVariable(*state,
                            "Site Outdoor Air Drybulb Temperature",
                            OutputProcessor::Unit::C,
                            state->dataEnvrn->OutDryBulbTemp,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            "Environment");
        Real64 light_consumption = 999;
        SetupOutputVariable(*state,
                            "Lights Electricity Energy",
                            OutputProcessor::Unit::J,
                            light_consumption,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "SPACE1-1 LIGHTS 1",
                            _,
                            "Electricity",
                            "InteriorLights",
                            "GeneralLights",
                            "Building",
                            "SPACE1-1",
                            1,
                            1);
        UpdateMeterReporting(*state);
        UpdateDataandReport(*state, OutputProcessor::TimeStepType::Zone);

        state->dataOutputProcessor->NumExtraVars = 0;
        BuildKeyVarList(*state, "Air Loop 1|AirSupply InletNode", "SYSTEM NODE SETPOINT TEMPERATURE", 1, 2);
        EXPECT_EQ(1, state->dataOutputProcessor->NumExtraVars);

        state->dataOutputProcessor->NumExtraVars = 0;
        BuildKeyVarList(*state, "Air Loop 1|AirSupply InletNode", "SYSTEM NODE TEMPERATURE", 1, 2);
        EXPECT_EQ(1, state->dataOutputProcessor->NumExtraVars);

        GenOutputVariablesAuditReport(*state);

        compare_err_stream("");
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_MeterCustomSystemEnergy)
    {
        std::string const idf_objects =
            delimited_string({"Meter:Custom,",
                              "Meter Surface Average Face Conduction Heat Transfer Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Surface Average Face Conduction Heat Transfer Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Surface Window Heat Loss Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Surface Window Heat Loss Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Zone Windows Total Heat Gain Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Windows Total Heat Gain Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Surface Window Heat Gain Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Surface Window Heat Gain Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Zone Ventilation Heat Gain,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Ventilation Total Heat Gain Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Zone Ventilation Heat Loss,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Ventilation Total Heat Loss Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Zone Infiltration Heat Gain,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Infiltration Total Heat Gain Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Zone Infiltration Heat Loss,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Infiltration Total Heat Loss Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Internal Loads Heating Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Electric Equipment Total Heating Energy,  !- Output Variable or Meter Name 1",
                              "* ,                       !- Key Name 2",
                              "Zone Lights Total Heating Energy,  !- Output Variable or Meter Name 2",
                              "* ,                       !- Key Name 3",
                              "People Total Heating Energy;  !- Output Variable or Meter Name 3",

                              "Meter:Custom,",
                              "Meter Zone Electric Equipment Total Heating Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Electric Equipment Total Heating Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Zone Mechanical Ventilation XYZ,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Mechanical Ventilation Cooling Load Decrease Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Zone Mechanical Ventilation 123,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Zone Mechanical Ventilation No Load Heat Removal Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Air System Total Heating Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Air System Total Heating Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Air System Total Cooling Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Air System Total Cooling Energy;  !- Output Variable or Meter Name 1",

                              "Meter:Custom,",
                              "Meter Air System Hot Water Energy,  !- Name",
                              "Generic,                 !- Fuel Type",
                              "*,                       !- Key Name 1",
                              "Air System Hot Water Energy;  !- Output Variable or Meter Name 1",

                              "Output:Meter, Meter Surface Average Face Conduction Heat Transfer Energy, Timestep;",

                              "Output:Meter, Meter Surface Window Heat Loss Energy, Timestep;",

                              "Output:Meter, Meter Zone Windows Total Heat Gain Energy, Timestep;",

                              "Output:Meter, Meter Surface Window Heat Gain Energy, Timestep;",

                              "Output:Meter, Meter Zone Ventilation Heat Gain, Timestep;",

                              "Output:Meter, Meter Zone Ventilation Heat Loss, Timestep;",

                              "Output:Meter, Meter Zone Infiltration Heat Gain, Timestep;",

                              "Output:Meter, Meter Zone Infiltration Heat Loss, Timestep;",

                              "Output:Meter, Meter Internal Loads Heating Energy, Timestep;",

                              "Output:Meter, Meter Zone Electric Equipment Total Heating Energy, Timestep;",

                              "Output:Meter, Meter Zone Mechanical Ventilation XYZ, Timestep;",

                              "Output:Meter, Meter Zone Mechanical Ventilation 123, Timestep;",

                              "Output:Meter, Meter Air System Total Heating Energy, Timestep;",

                              "Output:Meter, Meter Air System Total Cooling Energy, Timestep;",

                              "Output:Meter, Meter Air System Hot Water Energy, Timestep;"});

        ASSERT_TRUE(process_idf(idf_objects));
        bool errors_found = false;
        Real64 transferredenergy = 0;
        state->dataGlobal->NumOfZones = 1;
        state->dataHVACGlobal->NumPrimaryAirSys = 1;
        state->dataAirSystemsData->PrimaryAirSystems.allocate(state->dataHVACGlobal->NumPrimaryAirSys);
        state->dataAirSystemsData->PrimaryAirSystems(1).Name = "Air Loop 1";
        state->dataZoneEquip->ZoneEquipConfig.allocate(state->dataGlobal->NumOfZones);
        state->dataZoneEquip->ZoneEquipConfig(state->dataGlobal->NumOfZones).IsControlled = true;
        SetupOutputVariable(*state,
                            "Surface Average Face Conduction Heat Transfer Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Surface Window Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Zone Windows Total Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Surface Window Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Zone Ventilation Total Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Zone Ventilation Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Gain Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Zone Infiltration Total Heat Loss Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Zone Electric Equipment Total Heating Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "Zone Lights Total Heating Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SetupOutputVariable(*state,
                            "People Total Heating Energy",
                            OutputProcessor::Unit::J,
                            transferredenergy,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Summed,
                            "*");
        SystemReports::AllocateAndSetUpVentReports(*state);
        SystemReports::AllocateAndSetUpVentReports(*state);
        GetCustomMeterInput(*state, errors_found);
        EXPECT_FALSE(errors_found);
        EXPECT_EQ(15, state->dataOutputProcessor->NumEnergyMeters);
        EXPECT_EQ(state->dataOutputProcessor->EnergyMeters(1).Name, "METER SURFACE AVERAGE FACE CONDUCTION HEAT TRANSFER ENERGY");
        EXPECT_EQ(state->dataOutputProcessor->EnergyMeters(12).Name, "METER ZONE MECHANICAL VENTILATION 123");
        EXPECT_EQ(state->dataOutputProcessor->EnergyMeters(15).Name, "METER AIR SYSTEM HOT WATER ENERGY");
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_DuplicateMeterCustom)
    {
        std::string const idf_objects = delimited_string({"Meter:Custom,",
                                                          "CustomMeter1,               !- Name",
                                                          "Generic,                    !- Fuel Type",
                                                          ",                           !- Key Name 1",
                                                          "DistrictHeating:Facility;   !- Variable or Meter 1 Name",
                                                          "Meter:Custom,",
                                                          "CustomMeter2,               !- Name",
                                                          "Generic,                    !- Fuel Type",
                                                          ",                           !- Key Name 1",
                                                          "CustomMeter1;               !- Variable or Meter 1 Name",
                                                          "Output:Meter,CustomMeter1,Hourly;",
                                                          "Output:Meter,CustomMeter2,Hourly;"});

        ASSERT_TRUE(process_idf(idf_objects));

        bool errors_found = false;

        GetCustomMeterInput(*state, errors_found);

        EXPECT_FALSE(errors_found);

        std::string errMsg = delimited_string(
            {"   ** Warning ** Meter:Custom=\"CUSTOMMETER1\", invalid Output Variable or Meter Name=\"DISTRICTHEATING:FACILITY\".",
             "   **   ~~~   ** ...will not be shown with the Meter results.",
             "   ** Warning ** Meter:Custom=\"CUSTOMMETER1\", no items assigned ",
             "   **   ~~~   ** ...will not be shown with the Meter results. This may be caused by a Meter:Custom be assigned to another "
             "Meter:Custom.",
             "   ** Warning ** Meter:Custom=\"CUSTOMMETER2\", contains a reference to another Meter:Custom in field: Output Variable or Meter "
             "Name=\"CUSTOMMETER1\"."});
        compare_err_stream(errMsg);
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_unitStringToEnum)
    {

        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::J, unitStringToEnum("J")));
        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::J, unitStringToEnum("j")));

        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::kgWater_kgDryAir, unitStringToEnum("kgWater/kgDryAir")));
        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::kgWater_s, unitStringToEnum("kgWater/s")));

        EXPECT_TRUE(compare_enums(OutputProcessor::Unit::unknown, unitStringToEnum("junk")));
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

        AddToOutputVariableList(
            *state, "energy variable 1", OutputProcessor::TimeStepType::Zone, StoreType::Averaged, VariableType::Integer, OutputProcessor::Unit::J);
        AddToOutputVariableList(
            *state, "energy variable 2", OutputProcessor::TimeStepType::Zone, StoreType::Averaged, VariableType::Integer, OutputProcessor::Unit::J);
        AddToOutputVariableList(
            *state, "energy variable 3", OutputProcessor::TimeStepType::Zone, StoreType::Averaged, VariableType::Integer, OutputProcessor::Unit::J);

        AddToOutputVariableList(*state,
                                "humidity ratio variable 1",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Integer,
                                OutputProcessor::Unit::kgWater_kgDryAir);
        AddToOutputVariableList(*state,
                                "humidity ratio variable 2",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Integer,
                                OutputProcessor::Unit::kgWater_kgDryAir);

        AddToOutputVariableList(*state,
                                "flow variable 1",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Integer,
                                OutputProcessor::Unit::kgWater_s);
        AddToOutputVariableList(*state,
                                "flow variable 2",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Integer,
                                OutputProcessor::Unit::kgWater_s);

        AddToOutputVariableList(*state,
                                "user defined EMS variable 1",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Integer,
                                OutputProcessor::Unit::customEMS,
                                "ergs/century");
        AddToOutputVariableList(*state,
                                "user defined EMS variable 2",
                                OutputProcessor::TimeStepType::Zone,
                                StoreType::Averaged,
                                VariableType::Integer,
                                OutputProcessor::Unit::customEMS,
                                "swamps/county");

        EXPECT_EQ(" [J]", unitStringFromDDitem(*state, 3));

        EXPECT_EQ(" [kgWater/kgDryAir]", unitStringFromDDitem(*state, 4));

        EXPECT_EQ(" [kgWater/s]", unitStringFromDDitem(*state, 6));

        EXPECT_EQ(" [ergs/century]", unitStringFromDDitem(*state, 8));

        EXPECT_EQ(" [swamps/county]", unitStringFromDDitem(*state, 9));
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_GetCustomMeterInput)
    {
        std::string const idf_objects = delimited_string({
            "Meter:Custom,",
            "FuelOilNo1CustomMeter,      !- Name",
            "FuelOilNo1,                 !- Fuel Type",
            ",                           !- Key Name 1",
            "FuelOilNo1:Facility;        !- Variable or Meter 1 Name",

            "Meter:Custom,",
            "FuelOilNo2CustomMeter,      !- Name",
            "FuelOilNo2,                 !- Fuel Type",
            ",                           !- Key Name 1",
            "FuelOilNo2:Facility;        !- Variable or Meter 1 Name",

            "Meter:Custom,",
            "NaturalGasCustomMeter,      !- Name",
            "NaturalGas,                 !- Fuel Type",
            ",                           !- Key Name 1",
            "NaturalGas:Facility;        !- Variable or Meter 1 Name",
        });

        ASSERT_TRUE(process_idf(idf_objects));

        bool errors_found = false;

        GetCustomMeterInput(*state, errors_found);

        EXPECT_FALSE(errors_found);
    }

    TEST_F(EnergyPlusFixture, OutputProcessor_FilePathInSetInitialMeterReportingAndOutputNames)
    {
        state->dataOutputProcessor->EnergyMeters.allocate(1);
        state->dataOutputProcessor->EnergyMeters(1).Name = "Foo";
        state->dataOutputProcessor->EnergyMeters(1).RptTS = true;
        state->dataOutputProcessor->EnergyMeters(1).RptAccTS = true;

        // by default, the eso fs::path instance will just be a filename, so this will definitely pass

        // first call it as a non-cumulative variable
        SetInitialMeterReportingAndOutputNames(*state, 1, true, ReportingFrequency::EachCall, false);
        std::string errMsg = delimited_string({"   ** Warning ** Output:Meter:MeterFileOnly requested for \"Foo\" (TimeStep), already on "
                                               "\"Output:Meter\". Will report to both eplusout.eso and eplusout.mtr"});
        compare_err_stream(errMsg);

        // then with a cumulative variable
        SetInitialMeterReportingAndOutputNames(*state, 1, true, ReportingFrequency::EachCall, true);
        errMsg = delimited_string({"   ** Warning ** Output:Meter:MeterFileOnly requested for \"Cumulative Foo\" (TimeStep), already on "
                                   "\"Output:Meter\". Will report to both eplusout.eso and eplusout.mtr"});
        compare_err_stream(errMsg);

        // but then if you run EnergyPlus with `-d` to set an output directory, the variable will become a fully qualified path to the file
        // to test this we need to set up a proper path for the current platform, luckily this seems to suffic
        state->files.eso.filePath = fs::path("foo") / "bar.eso";
        state->files.mtr.filePath = fs::path("foo") / "bar.mtr";

        // first call it as a non-cumulative variable
        SetInitialMeterReportingAndOutputNames(*state, 1, true, ReportingFrequency::EachCall, false);
        errMsg = delimited_string({"   ** Warning ** Output:Meter:MeterFileOnly requested for \"Foo\" (TimeStep), already on \"Output:Meter\". Will "
                                   "report to both bar.eso and bar.mtr"});
        compare_err_stream(errMsg);

        // then with a cumulative variable
        SetInitialMeterReportingAndOutputNames(*state, 1, true, ReportingFrequency::EachCall, true);
        errMsg = delimited_string({"   ** Warning ** Output:Meter:MeterFileOnly requested for \"Cumulative Foo\" (TimeStep), already on "
                                   "\"Output:Meter\". Will report to both bar.eso and bar.mtr"});
        compare_err_stream(errMsg);
    }
} // namespace OutputProcessor

} // namespace EnergyPlus
