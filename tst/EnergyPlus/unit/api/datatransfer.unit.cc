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
#include <vector>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/api/datatransfer.h>

#include "../Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

class DataExchangeUnitTestFixture : public EnergyPlusFixture {
    // create a plugin manager instance
    // TODO: Note that this requires the Python package to be built next to the E+ unit test binary
    //       Right now, this is built inside the E+ src/EnergyPlus/CMakeLists.txt file, it should probably be a separate project
    //       so that both E+ and the unit test binary can depend on it.
    EnergyPlus::PluginManagement::PluginManager pluginManager;

    std::vector<Real64> variablePlaceholders;
    struct actuator {
        Real64 val;
        bool flag;
    };
    std::vector<actuator> actuatorPlaceholders;
    std::vector<Real64> internalVarPlaceholders;

    void TearDown() override {
        this->variablePlaceholders.clear();
        this->actuatorPlaceholders.clear();
    }

public:
    void addOutputVariable(std::string const & varName, std::string const & key) {
        this->variablePlaceholders.emplace_back();
        requestVariable(varName.c_str(), key.c_str());
        inputProcessor->preScanReportingVariables();
        SetupOutputVariable(varName, OutputProcessor::Unit::kg_s, this->variablePlaceholders.back(), "Zone", "Average", key);
    }

    void addActuator(std::string const & objType, std::string const & controlType, std::string const & objKey) {
        this->actuatorPlaceholders.emplace_back();
        int lastActuatorIndex = (int)this->actuatorPlaceholders.size() - 1;
        SetupEMSActuator(objType, objKey, controlType, "kg/s", this->actuatorPlaceholders[lastActuatorIndex].flag, this->actuatorPlaceholders[lastActuatorIndex].val);
    }

    void addInternalVariable(std::string const & varType, std::string const & varKey) {
        this->internalVarPlaceholders.emplace_back();
        SetupEMSInternalVariable(varType, varKey, "kg/s", this->internalVarPlaceholders.back());
        this->internalVarPlaceholders.clear();
    }

    void addPluginGlobal(std::string const & varName) {
        this->pluginManager.addGlobalVariable(varName);
    }

    void addTrendWithNewGlobal(std::string const & newGlobalVarName, std::string const & trendName, int numTrendValues) {
        this->pluginManager.addGlobalVariable(newGlobalVarName);
        int i = EnergyPlus::PluginManagement::PluginManager::getGlobalVariableHandle(newGlobalVarName, true);
        EnergyPlus::PluginManagement::trends.emplace_back(trendName, numTrendValues, i);
    }
};

TEST_F(DataExchangeUnitTestFixture, DataTransfer_TestListAllDataInCSV)
{

    std::string const idf_objects = delimited_string({"Version, 9.3.0;"});
    ASSERT_TRUE(process_idf(idf_objects, false));

    // first off, the function should return, even if there isn't anything meaningful in it (it will have headers)
    std::string csvDataEmpty = listAllAPIDataCSV();

    // then as we add stuff, it should appear in there incrementally
    this->addOutputVariable("Boiler Heat Transfer", "Boiler 1");
    {
        std::string csvData = listAllAPIDataCSV();
        std::size_t foundAddedBoiler = csvData.find("BOILER 1") != std::string::npos; // Note output variables only keep UC, so we should check UC here
        std::size_t foundAddedActuator = csvData.find("Chiller 1") != std::string::npos;
        std::size_t foundAddedIV = csvData.find("Zone 1") != std::string::npos;
        std::size_t foundAddedGlobal = csvData.find("PLUGINGLOBALVARNAME") != std::string::npos; // Note globals are kept in upper case internally, check UC here
        std::size_t foundAddedTrend = csvData.find("Trend 1") != std::string::npos;
        EXPECT_TRUE(foundAddedBoiler);
        EXPECT_FALSE(foundAddedActuator);
        EXPECT_FALSE(foundAddedIV);
        EXPECT_FALSE(foundAddedGlobal);
        EXPECT_FALSE(foundAddedTrend);
    }

    this->addActuator("Chiller:Electric", "Max Flow Rate", "Chiller 1");
    {
        std::string csvData = listAllAPIDataCSV();
        std::size_t foundAddedBoiler = csvData.find("BOILER 1") != std::string::npos; // Note output variables only keep UC, so we should check UC here
        std::size_t foundAddedActuator = csvData.find("Chiller 1") != std::string::npos;
        std::size_t foundAddedIV = csvData.find("Zone 1") != std::string::npos;
        std::size_t foundAddedGlobal = csvData.find("PLUGINGLOBALVARNAME") != std::string::npos; // Note globals are kept in upper case internally, check UC here
        std::size_t foundAddedTrend = csvData.find("Trend 1") != std::string::npos;
        EXPECT_TRUE(foundAddedBoiler);
        EXPECT_TRUE(foundAddedActuator);
        EXPECT_FALSE(foundAddedIV);
        EXPECT_FALSE(foundAddedGlobal);
        EXPECT_FALSE(foundAddedTrend);
    }

    this->addInternalVariable("Floor Area", "Zone 1");
    {
        std::string csvData = listAllAPIDataCSV();
        std::size_t foundAddedBoiler = csvData.find("BOILER 1") != std::string::npos; // Note output variables only keep UC, so we should check UC here
        std::size_t foundAddedActuator = csvData.find("Chiller 1") != std::string::npos;
        std::size_t foundAddedIV = csvData.find("Zone 1") != std::string::npos;
        std::size_t foundAddedGlobal = csvData.find("PLUGINGLOBALVARNAME") != std::string::npos; // Note globals are kept in upper case internally, check UC here
        std::size_t foundAddedTrend = csvData.find("Trend 1") != std::string::npos;
        EXPECT_TRUE(foundAddedBoiler);
        EXPECT_TRUE(foundAddedActuator);
        EXPECT_TRUE(foundAddedIV);
        EXPECT_FALSE(foundAddedGlobal);
        EXPECT_FALSE(foundAddedTrend);
    }

    this->addPluginGlobal("PluginGlobalVarName");
    {
        std::string csvData = listAllAPIDataCSV();
        std::size_t foundAddedBoiler = csvData.find("BOILER 1") != std::string::npos; // Note output variables only keep UC, so we should check UC here
        std::size_t foundAddedActuator = csvData.find("Chiller 1") != std::string::npos;
        std::size_t foundAddedIV = csvData.find("Zone 1") != std::string::npos;
        std::size_t foundAddedGlobal = csvData.find("PLUGINGLOBALVARNAME") != std::string::npos; // Note globals are kept in upper case internally, check UC here
        std::size_t foundAddedTrend = csvData.find("Trend 1") != std::string::npos;
        EXPECT_TRUE(foundAddedBoiler);
        EXPECT_TRUE(foundAddedActuator);
        EXPECT_TRUE(foundAddedIV);
        EXPECT_TRUE(foundAddedGlobal);
        EXPECT_FALSE(foundAddedTrend);
    }

    this->addTrendWithNewGlobal("NewGlobalVarHere", "Trend 1", 3);
    {
        std::string csvData = listAllAPIDataCSV();
        std::size_t foundAddedBoiler = csvData.find("BOILER 1") != std::string::npos; // Note output variables only keep UC, so we should check UC here
        std::size_t foundAddedActuator = csvData.find("Chiller 1") != std::string::npos;
        std::size_t foundAddedIV = csvData.find("Zone 1") != std::string::npos;
        std::size_t foundAddedGlobal = csvData.find("PLUGINGLOBALVARNAME") != std::string::npos; // Note globals are kept in upper case internally, check UC here
        std::size_t foundAddedTrend = csvData.find("Trend 1") != std::string::npos;
        EXPECT_TRUE(foundAddedBoiler);
        EXPECT_TRUE(foundAddedActuator);
        EXPECT_TRUE(foundAddedIV);
        EXPECT_TRUE(foundAddedGlobal);
        EXPECT_TRUE(foundAddedTrend);
    }

}
