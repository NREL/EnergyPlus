// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// Unit tests for data transfer API methods
#include <utility>
#include <vector>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/api/datatransfer.h>

#include "../Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

class DataExchangeAPIUnitTestFixture : public EnergyPlusFixture
{
    // create a plugin manager instance
    // TODO: Note that this requires the Python package to be built next to the E+ unit test binary
    //       Right now, this is built inside the E+ src/EnergyPlus/CMakeLists.txt file, it should probably be a separate project
    //       so that both E+ and the unit test binary can depend on it.
    EnergyPlus::PluginManagement::PluginManager pluginManager;

    struct DummyRealVariable
    {
        std::string varName;
        std::string varKey;
        Real64 value = 0.0;
        bool meterType = false;
        DummyRealVariable(std::string _varName, std::string _varKey, Real64 _value, bool _meterType)
            : varName(std::move(_varName)), varKey(std::move(_varKey)), value(_value), meterType(_meterType)
        {
        }
    };
    std::vector<DummyRealVariable> realVariablePlaceholders;
    struct DummyIntVariable
    {
        std::string varName;
        std::string varKey;
        int value = 0;
        DummyIntVariable(std::string _varName, std::string _varKey, Real64 _value)
            : varName(std::move(_varName)), varKey(std::move(_varKey)), value(_value)
        {
        }
    };
    std::vector<DummyIntVariable> intVariablePlaceholders;
    struct actuator
    {
        Real64 val;
        bool flag;
    };
    std::vector<actuator> actuatorPlaceholders;
    std::vector<Real64> internalVarPlaceholders;

    void SetUp() override
    {
        EnergyPlusFixture::SetUp();
        Real64 timeStep = 1.0;
        OutputProcessor::SetupTimePointers("Zone", timeStep);
        OutputProcessor::SetupTimePointers("HVAC", timeStep);
        *OutputProcessor::TimeValue.at(OutputProcessor::TimeStepType::TimeStepZone).TimeStep = 60;
        *OutputProcessor::TimeValue.at(OutputProcessor::TimeStepType::TimeStepSystem).TimeStep = 60;
    }

    void TearDown() override
    {
        this->realVariablePlaceholders.clear();
        this->actuatorPlaceholders.clear();
        EnergyPlusFixture::TearDown();
    }

public:
    void preRequestRealVariable(std::string const &varName, std::string const &key, Real64 initialValue = 0.0, bool meterType = false)
    {
        this->realVariablePlaceholders.emplace_back(varName, key, initialValue, meterType);
        requestVariable(varName.c_str(), key.c_str());
    }

    void preRequestIntegerVariable(std::string const &varName, std::string const &key, int initialValue = 0)
    {
        this->intVariablePlaceholders.emplace_back(varName, key, initialValue);
        requestVariable(varName.c_str(), key.c_str());
    }

    void setupVariablesOnceAllAreRequested()
    {
        inputProcessor->preScanReportingVariables();
        for (auto &val : this->realVariablePlaceholders) {
            if (val.meterType) {
                SetupOutputVariable(
                    val.varName, OutputProcessor::Unit::kg_s, val.value, "Zone", "Sum", val.varKey, _, "ELECTRICITY", "HEATING", _, "System");
            } else {
                SetupOutputVariable(val.varName, OutputProcessor::Unit::kg_s, val.value, "Zone", "Average", val.varKey);
            }
        }
        for (auto &val : this->intVariablePlaceholders) {
            SetupOutputVariable(val.varName, OutputProcessor::Unit::kg_s, val.value, "Zone", "Average", val.varKey);
        }
    }

    constexpr static auto dummyActuatorType = "Chiller:Electric";
    constexpr static auto dummyActuatorVar = "Max Flow Rate";
    constexpr static auto dummyActuatorInstance = "Chiller 1";
    void addActuator(std::string const &objType = DataExchangeAPIUnitTestFixture::dummyActuatorType,
                     std::string const &controlType = DataExchangeAPIUnitTestFixture::dummyActuatorVar,
                     std::string const &objKey = DataExchangeAPIUnitTestFixture::dummyActuatorInstance)
    {
        this->actuatorPlaceholders.emplace_back();
        int lastActuatorIndex = (int)this->actuatorPlaceholders.size() - 1;
        SetupEMSActuator(objType,
                         objKey,
                         controlType,
                         "kg/s",
                         this->actuatorPlaceholders[lastActuatorIndex].flag,
                         this->actuatorPlaceholders[lastActuatorIndex].val);
    }

    void addInternalVariable(std::string const &varType, std::string const &varKey)
    {
        this->internalVarPlaceholders.emplace_back();
        SetupEMSInternalVariable(varType, varKey, "kg/s", this->internalVarPlaceholders.back());
        this->internalVarPlaceholders.clear();
    }

    void addPluginGlobal(std::string const &varName)
    {
        this->pluginManager.addGlobalVariable(varName);
    }

    void addTrendWithNewGlobal(std::string const &newGlobalVarName, std::string const &trendName, int numTrendValues)
    {
        this->pluginManager.addGlobalVariable(newGlobalVarName);
        int i = EnergyPlus::PluginManagement::PluginManager::getGlobalVariableHandle(newGlobalVarName, true);
        EnergyPlus::PluginManagement::trends.emplace_back(trendName, numTrendValues, i);
    }

    static void simulateTimeStepAndReport()
    {
        UpdateMeterReporting(OutputFiles::getSingleton());
        UpdateDataandReport(OutputProcessor::TimeStepType::TimeStepZone);
    }
};

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestListAllDataInCSV)
{

    std::string const idf_objects = delimited_string({"Version, 9.3.0;"});
    ASSERT_TRUE(process_idf(idf_objects, false)); // this had to be here or I was getting a strange segfault during a JSON string dtor

    // first off, the function should return, even if there isn't anything meaningful in it (it will have headers)
    std::string csvDataEmpty = listAllAPIDataCSV();

    // then as we add stuff, and make sure it appears in the output
    this->preRequestRealVariable("Boiler Heat Transfer", "Boiler 1");
    this->setupVariablesOnceAllAreRequested();
    this->addActuator();
    this->addInternalVariable("Floor Area", "Zone 1");
    this->addPluginGlobal("PluginGlobalVarName");
    this->addTrendWithNewGlobal("NewGlobalVarHere", "Trend 1", 3);
    std::string csvData = listAllAPIDataCSV();
    std::size_t foundAddedBoiler = csvData.find("BOILER 1") != std::string::npos; // Note output variables only keep UC, so we should check UC here
    std::size_t foundAddedActuator = csvData.find(DataExchangeAPIUnitTestFixture::dummyActuatorType) != std::string::npos;
    std::size_t foundAddedIV = csvData.find("Zone 1") != std::string::npos;
    std::size_t foundAddedGlobal =
        csvData.find("PLUGINGLOBALVARNAME") != std::string::npos; // Note globals are kept in upper case internally, check UC here
    std::size_t foundAddedTrend = csvData.find("Trend 1") != std::string::npos;
    EXPECT_TRUE(foundAddedBoiler);
    EXPECT_TRUE(foundAddedActuator);
    EXPECT_TRUE(foundAddedIV);
    EXPECT_TRUE(foundAddedGlobal);
    EXPECT_TRUE(foundAddedTrend);
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestApiDataFullyReady)
{
    // basically, the data should not be ready at the beginning of a unit test -- ever, so just check that for now
    EXPECT_EQ(1, apiDataFullyReady()); // 1 is false
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestGetVariableHandlesRealTypes)
{
    this->preRequestRealVariable("Chiller Heat Transfer", "Chiller 1");
    this->preRequestRealVariable("Zone Mean Temperature", "Zone 1");
    this->setupVariablesOnceAllAreRequested();
    int hChillerHT = getVariableHandle("Chiller Heat Transfer", "Chiller 1");
    int hZoneTemp = getVariableHandle("Zone Mean Temperature", "Zone 1");
    EXPECT_GT(hChillerHT, -1);
    EXPECT_GT(hZoneTemp, -1);
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestGetVariableHandlesIntegerTypes)
{
    this->preRequestIntegerVariable("Chiller Operating Mode", "Chiller 1");
    this->preRequestIntegerVariable("Chiller Operating Mode", "Chiller 2");
    this->setupVariablesOnceAllAreRequested();
    int hChillerMode1 = getVariableHandle("Chiller Operating Mode", "Chiller 1");
    int hChillerMode2 = getVariableHandle("Chiller Operating Mode", "Chiller 2");
    EXPECT_GT(hChillerMode1, -1);
    EXPECT_GT(hChillerMode2, -1);
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestGetVariableHandlesMixedTypes)
{
    // In order to access them, you must request them to make them available, then look them up...add a couple floating point ones and an integer one
    this->preRequestRealVariable("Chiller Heat Transfer", "Chiller 1");
    this->preRequestRealVariable("Zone Mean Temperature", "Zone 1");
    this->preRequestIntegerVariable("Chiller Operating Mode", "Chiller 1");
    this->setupVariablesOnceAllAreRequested();
    // Then try to get their handles
    int hChillerHT = getVariableHandle("Chiller Heat Transfer", "Chiller 1");
    int hZoneTemp = getVariableHandle("Zone Mean Temperature", "Zone 1");
    int hChillerMode = getVariableHandle("Chiller Operating Mode", "Chiller 1");
    EXPECT_GT(hChillerHT, -1);
    EXPECT_GT(hZoneTemp, -1);
    EXPECT_GT(hChillerMode, -1);
    // now try to get handles to variables that doesn't exist
    int hChiller2HT = getVariableHandle("Chiller Heat Transfer", "Chiller 2");
    int hZone2Temp = getVariableHandle("Zone Mean Radiant Temperature", "Zone 1");
    EXPECT_EQ(-1, hChiller2HT);
    EXPECT_EQ(-1, hZone2Temp);
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestGetVariableValuesRealTypes)
{
    this->preRequestRealVariable("Chiller Heat Transfer", "Chiller 1", 3.14);
    this->preRequestRealVariable("Zone Mean Temperature", "Zone 1", 2.718);
    this->setupVariablesOnceAllAreRequested();
    int hChillerHT = getVariableHandle("Chiller Heat Transfer", "Chiller 1");
    int hZoneTemp = getVariableHandle("Zone Mean Temperature", "Zone 1");

    // pretend like E+ ran a time step
    DataExchangeAPIUnitTestFixture::simulateTimeStepAndReport();

    // get the values for valid handles
    Real64 curHeatTransfer = getVariableValue(hChillerHT);
    Real64 curZoneTemp = getVariableValue(hZoneTemp);
    EXPECT_NEAR(3.14, curHeatTransfer, 0.0001);
    EXPECT_NEAR(2.718, curZoneTemp, 0.0001);
    // invalid handles will respond differently based on whether E+ is in API or Plugin mode

    // in API mode, the function will throw an exception (for now)
    DataGlobals::eplusRunningViaAPI = true;
    EXPECT_THROW(getVariableValue(-1), std::runtime_error);
    EXPECT_THROW(getVariableValue(3), std::runtime_error);

    // in Plugin mode, there is a flag that should be set to true
    DataGlobals::eplusRunningViaAPI = false;
    PluginManagement::shouldIssueFatalAfterPluginCompletes = false;
    // first the thing should just pass
    getVariableValue(-1);
    // but the flag should be set
    EXPECT_TRUE(PluginManagement::shouldIssueFatalAfterPluginCompletes);
    PluginManagement::shouldIssueFatalAfterPluginCompletes = false;
    getVariableValue(3);
    EXPECT_TRUE(PluginManagement::shouldIssueFatalAfterPluginCompletes);
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestGetMeterHandles)
{
    this->preRequestRealVariable("Chiller Electric Energy", "Chiller 1", 3.14, true);
    this->setupVariablesOnceAllAreRequested();
    // Then try to get the meter handle
    int hFacilityElectricity = getMeterHandle("Electricity:Facility");
    EXPECT_GT(hFacilityElectricity, -1);
    // now try to get handles to meters that doesn't exist
    int hDummyMeter = getMeterHandle("EnergySomething");
    EXPECT_EQ(-1, hDummyMeter);
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestGetMeterValues)
{
    this->preRequestRealVariable("Chiller Electric Energy", "Chiller 1", 3.14, true);
    this->setupVariablesOnceAllAreRequested();
    int hFacilityElectricity = getMeterHandle("Electricity:Facility");
    EXPECT_GT(hFacilityElectricity, -1);
    // pretend like E+ ran a time step
    DataExchangeAPIUnitTestFixture::simulateTimeStepAndReport();
    // get the value for a valid meter
    Real64 curFacilityElectricity = getMeterValue(hFacilityElectricity);
    EXPECT_NEAR(3.14, curFacilityElectricity, 0.001);
    // TODO: Figure out how to get accrued meter value and test that here
}

TEST_F(DataExchangeAPIUnitTestFixture, DataTransfer_TestActuators)
{
    this->addActuator();
    // Then try to get the actuator handle
    int hActuator = getActuatorHandle(DataExchangeAPIUnitTestFixture::dummyActuatorType,
                                      DataExchangeAPIUnitTestFixture::dummyActuatorVar,
                                      DataExchangeAPIUnitTestFixture::dummyActuatorInstance);
    EXPECT_GT(hActuator, -1);
    // now try to get handles to invalid actuators
    {
        int hActuatorBad =
            getActuatorHandle(DataExchangeAPIUnitTestFixture::dummyActuatorType, DataExchangeAPIUnitTestFixture::dummyActuatorVar, "InvalidInstance");
        EXPECT_EQ(hActuatorBad, -1);
    }
    {
        int hActuatorBad =
            getActuatorHandle(DataExchangeAPIUnitTestFixture::dummyActuatorType, "InvalidVar", DataExchangeAPIUnitTestFixture::dummyActuatorInstance);
        EXPECT_EQ(hActuatorBad, -1);
    }
    {
        int hActuatorBad =
            getActuatorHandle("InvalidType", DataExchangeAPIUnitTestFixture::dummyActuatorVar, DataExchangeAPIUnitTestFixture::dummyActuatorInstance);
        EXPECT_EQ(hActuatorBad, -1);
    }
}
