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

#include "AutosizingFixture.hh"
#include <gtest/gtest.h>

#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, SystemAirFlowSizingGauntlet)
{
    // this global state is what would be set up by E+ currently
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    static std::string const routineName("SystemAirFlowSizingGauntlet");

    // create the sizer and set up the flags to specify the sizing configuration
    SystemAirFlowSizer sizer;
    Real64 inputValue = 5;
    bool errorsFound = false;
    bool printFlag = false;

    // uninitialized sizing type
    Real64 sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(errorsFound);
    EXPECT_EQ(AutoSizingResultType::ErrorType2, sizer.errorType);
    EXPECT_NEAR(0.0, sizedValue, 0.0001); // unitialized sizing types always return 0
    errorsFound = false;

    // ZONE EQUIPMENT TESTING
    DataSizing::CurZoneEqNum = 1;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);

    // Test #1 - Zone Equipment, no autosizing
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset eio stream
    has_eio_output(true);
    printFlag = true;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    std::string eiooutput =
        std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                    " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Maximum Flow Rate [m3/s], 5.00000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // reset eio stream
    has_eio_output(true);

    // now allocate sizing arrays for testing autosized field
    EnergyPlus::DataSizing::FinalZoneSizing.allocate(1);
    EnergyPlus::DataSizing::ZoneEqSizing.allocate(1);
    EnergyPlus::DataSizing::ZoneEqSizing(1).SizingMethod.allocate(35);

    DataSizing::ZoneSizingRunDone = true;
    EnergyPlus::DataSizing::FinalZoneSizing(1).DesCoolVolFlow = 1.6;
    EnergyPlus::DataSizing::FinalZoneSizing(1).DesHeatVolFlow = 1.2;
    EnergyPlus::DataSizing::FinalZoneSizing(1).CoolDDNum = 1;
    EnergyPlus::DataSizing::FinalZoneSizing(1).HeatDDNum = 2;
    EnergyPlus::DataSizing::FinalZoneSizing(1).TimeStepNumAtCoolMax = 12;
    EnergyPlus::DataSizing::FinalZoneSizing(1).TimeStepNumAtHeatMax = 6;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    state->dataEnvrn->TotDesDays = 2;
    state->dataWeatherManager->DesDayInput.allocate(2);
    state->dataWeatherManager->DesDayInput(1).Month = 7;
    state->dataWeatherManager->DesDayInput(1).DayOfMonth = 7;
    state->dataWeatherManager->DesDayInput(2).Month = 1;
    state->dataWeatherManager->DesDayInput(2).DayOfMonth = 1;
    state->dataWeatherManager->DesDayInput(1).Title = "CoolingDD";
    state->dataWeatherManager->DesDayInput(2).Title = "HeatingDD";

    // Test 2 - Zone Equipment, sizes to greater of zone cooling and heating flow
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    DataSizing::ZoneSizingInput.allocate(1);
    DataSizing::ZoneSizingInput(1).ZoneNum = 1;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.6, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0;         // reset for next test

    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Maximum Flow Rate [m3/s], 1.60000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 3 - Zone Equipment, set heating only fan
    DataSizing::ZoneHeatingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 4 - Zone Equipment, set cooling only fan
    DataSizing::ZoneHeatingOnlyFan = false;
    DataSizing::ZoneCoolingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.6, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 5 - Zone Equipment, cooling only fan, set fraction used for sizing
    DataSizing::DataFractionUsedForSizing = 0.5;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.8, sizedValue, 0.0001); // 1/2 of zone cooling air flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test
    DataSizing::DataFractionUsedForSizing = 0.0; // reset for next test

    // Test 6 - Zone Equipment, set ZoneEqSizing data
    DataSizing::ZoneEqSizing(1).SystemAirFlow = true;
    DataSizing::ZoneEqSizing(1).AirVolFlow = 1.8;
    DataSizing::ZoneEqSizing(1).SizingMethod(int(sizer.sizingType)) = DataSizing::SupplyAirFlowRate;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.8, sizedValue, 0.0001); // max of zone cooling/heating/ZoneEqSizing
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 7 - Zone Equipment, set cooling only fan
    DataSizing::ZoneEqSizing(1).SystemAirFlow = false;
    DataSizing::ZoneCoolingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.6, sizedValue, 0.0001); // zone cooling flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 8 - Zone Equipment, set heating only fan
    DataSizing::ZoneCoolingOnlyFan = false;
    DataSizing::ZoneHeatingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // zone heating flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 9 - Zone Equipment, set ZoneEqSizing cooling air flow
    DataSizing::ZoneHeatingOnlyFan = false;
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    DataSizing::ZoneEqSizing(1).CoolingAirVolFlow = 2.2;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(2.2, sizedValue, 0.0001); // ZoneEqSizing cooling flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 10 - Zone Equipment, set ZoneEqSizing heating air flow
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = true;
    DataSizing::ZoneEqSizing(1).HeatingAirVolFlow = 3.2;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3.2, sizedValue, 0.0001); // ZoneEqSizing heating flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 11 - Zone Equipment, set ZoneEqSizing cooling and heating air flow
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3.2, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = false;

    DataSizing::ZoneEqSizing(1).SizingMethod(int(sizer.sizingType)) = DataSizing::FractionOfAutosizedCoolingAirflow;

    // Test 12 - Zone Equipment, set fraction of autosized cooling flow for cooling only fan
    DataSizing::DataFracOfAutosizedCoolingAirflow = 0.4;
    DataSizing::ZoneCoolingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.64, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 13 - Zone Equipment, set fraction of autosized heating flow for heating only fan
    DataSizing::DataFracOfAutosizedHeatingAirflow = 0.4;
    DataSizing::ZoneCoolingOnlyFan = false;
    DataSizing::ZoneHeatingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.48, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 14 - Zone Equipment, cooling or heating fan not set
    DataSizing::ZoneHeatingOnlyFan = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.64, sizedValue, 0.0001); // max of FinalZoneSizing cooling/heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 15 - Zone Equipment, ZoneEqSizing cooling
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.88, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 16 - Zone Equipment, ZoneEqSizing heating
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.28, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 17 - Zone Equipment, ZoneEqSizing cooling and heating fan
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.28, sizedValue, 0.0001); // max of FinalZoneSizing cooling/heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqSizing(1).SizingMethod(int(sizer.sizingType)) = DataSizing::FractionOfAutosizedHeatingAirflow;

    // Test 18 - Zone Equipment, set fraction of autosized cooling flow for cooling only fan
    DataSizing::DataFracOfAutosizedCoolingAirflow = 0.4;
    DataSizing::ZoneCoolingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.64, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 19 - Zone Equipment, set fraction of autosized heating flow for heating only fan
    DataSizing::ZoneCoolingOnlyFan = false;
    DataSizing::ZoneHeatingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.48, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 20 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing CoolingAirFlow is set
    DataSizing::ZoneHeatingOnlyFan = false;
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.88, sizedValue, 0.0001); // max of ZoneEqSizing cooling flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 21 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing HeatingAirFlow is set
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = true;
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.28, sizedValue, 0.0001); // max of ZoneEqSizing heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 22 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing Cooling/HeatingAirFlow are set
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.28, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = false;

    // Test 23 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing Cooling/HeatingAirFlow not set
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.64, sizedValue, 0.0001); // max of FinalZoneSizing cooling/heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqSizing(1).SizingMethod(int(sizer.sizingType)) = DataSizing::FlowPerCoolingCapacity;

    // Test 24 - Zone Equipment, set fraction of autosized cooling capacity for cooling only fan
    DataSizing::DataFlowPerCoolingCapacity = 0.00005;
    DataSizing::DataAutosizedCoolingCapacity = 10000.0;
    DataSizing::DataFlowPerHeatingCapacity = 0.00006;
    DataSizing::DataAutosizedHeatingCapacity = 20000.0;
    DataSizing::ZoneCoolingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.5, sizedValue, 0.0001); // flow per cooling capacity
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 25 - Zone Equipment, set fraction of autosized heating flow for heating only fan
    DataSizing::ZoneCoolingOnlyFan = false;
    DataSizing::ZoneHeatingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // flow per heating capacity
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 26 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing CoolingAirFlow is set
    DataSizing::ZoneHeatingOnlyFan = false;
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.5, sizedValue, 0.0001); // max of ZoneEqSizing cooling capacity * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 27 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing HeatingAirFlow is set
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = true;
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // ZoneEqSizing heating capacity * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 28 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing Cooling/HeatingAirFlow are set
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // max of ZoneEqSizing cooling/heating flow * fraction
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = false;

    // Test 29 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing Cooling/HeatingAirFlow not set
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // max of autosized cooling/heating capacity * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    DataSizing::ZoneEqSizing(1).SizingMethod(int(sizer.sizingType)) = DataSizing::FlowPerHeatingCapacity;

    // Test 30 - Zone Equipment, set fraction of autosized cooling capacity for cooling only fan
    DataSizing::ZoneCoolingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.5, sizedValue, 0.0001); // flow per cooling capacity
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 31 - Zone Equipment, set fraction of autosized heating flow for heating only fan
    DataSizing::ZoneCoolingOnlyFan = false;
    DataSizing::ZoneHeatingOnlyFan = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // flow per heating capacity
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 32 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing CoolingAirFlow is set
    DataSizing::ZoneHeatingOnlyFan = false;
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.5, sizedValue, 0.0001); // max of autosized cooling capacity * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 33 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing HeatingAirFlow is set
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = true;
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // autosized heating capacity * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 34 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing Cooling/HeatingAirFlow are set
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = true;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // max of autosized cooling/heating capacity * fraction
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).CoolingAirFlow = false;
    DataSizing::ZoneEqSizing(1).HeatingAirFlow = false;

    // Test 35 - Zone Equipment, cooling or heating fan not set, ZoneEqSizing Cooling/HeatingAirFlow not set
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.2, sizedValue, 0.0001); // max of autosized cooling/heating capacity * fraction
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 36 - Zone Equipment, set design size from parent
    DataSizing::ZoneEqSizing(1).DesignSizeFromParent = true;
    DataSizing::ZoneEqSizing(1).AirVolFlow = 1.75;
    inputValue = EnergyPlus::DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.75, sizedValue, 0.0001); // parent passed size
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::ZoneEqSizing(1).DesignSizeFromParent = false;

    // Test 37 - Zone Equipment, hard size with zone sizing run
    inputValue = 1.44;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(1.44, sizedValue, 0.0001); // hard sized result
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 38 - Zone Equipment, hard size
    inputValue = 1.44;
    DataSizing::ZoneSizingRunDone = false;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(1.44, sizedValue, 0.0001); // hard sized result
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 39 - EMS override
    DataSizing::DataEMSOverrideON = true;
    DataSizing::DataEMSOverride = 1.33;
    inputValue = 1.44;
    DataSizing::ZoneSizingRunDone = false;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(1.33, sizedValue, 0.0001); // override result
    EXPECT_NEAR(1.44, sizer.originalValue, 0.0001); // original input
    sizer.autoSizedValue = 0.0; // reset for next test

                                // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // AIRLOOP EQUIPMENT TESTING - no reporting, CurDuctType not set
    // Test 40 - Airloop Equipment
    DataSizing::CurZoneEqNum = 0;
    DataSizing::NumZoneSizingInput = 0;
    // baseFlags.otherEqType = false; set in initialize function based on other flags
    EnergyPlus::DataSizing::ZoneEqSizing.deallocate();
    EnergyPlus::DataSizing::FinalZoneSizing.deallocate();

    DataSizing::CurSysNum = 1;
    DataHVACGlobals::NumPrimaryAirSys = 1;
    DataSizing::NumSysSizInput = 1;
    DataSizing::SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 5.0;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 41 - Airloop Equipment - ems override is on
    DataSizing::SysSizingRunDone = true;
    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataEnvrn->TotDesDays = 2;
    DataSizing::SysSizPeakDDNum.allocate(2);
    DataSizing::SysSizPeakDDNum(1).CoolFlowPeakDD = 1;
    DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk.allocate(2);
    DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk(1) = 12;
    DataSizing::SysSizPeakDDNum(1).TimeStepAtCoolFlowPk(2) = 6;
    DataSizing::FinalSysSizing.allocate(1);
    DataSizing::FinalSysSizing(1).HeatDDNum = 2;
    DataSizing::SysSizInput.allocate(1);
    DataSizing::SysSizInput(1).AirLoopNum = 1;

    DataSizing::FinalSysSizing(1).DesMainVolFlow = 5.0;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(1.33, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test
    DataSizing::DataEMSOverrideON = false;

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Maximum Flow Rate [m3/s], 1.33000\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 42 - Airloop Equipment - CurDuctType = Main
    DataSizing::CurDuctType = DataHVACGlobals::Main;
    EnergyPlus::DataSizing::FinalSysSizing(1).DesMainVolFlow = 5.0;
    EnergyPlus::DataSizing::FinalSysSizing(1).DesCoolVolFlow = 5.0;
    EnergyPlus::DataSizing::FinalSysSizing(1).SysAirMinFlowRat = 0.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 43 - Airloop Equipment - AirLoopSysFlag set
    DataSizing::UnitarySysEqSizing.allocate(1);
    DataSizing::UnitarySysEqSizing(1).CoolingCapacity = true;
    DataSizing::UnitarySysEqSizing(1).CoolingAirFlow = true;
    DataSizing::UnitarySysEqSizing(1).CoolingAirVolFlow = 6.0;
    DataSizing::UnitarySysEqSizing(1).HeatingAirFlow = true;
    DataSizing::UnitarySysEqSizing(1).HeatingAirVolFlow = 7.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(7.0, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 44 - Airloop Equipment - AirLoopSysFlag set, heating only
    DataSizing::UnitarySysEqSizing(1).CoolingAirFlow = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(7.0, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 45 - Airloop Equipment - AirLoopSysFlag set, cooling only
    DataSizing::UnitarySysEqSizing(1).CoolingAirFlow = true;
    DataSizing::UnitarySysEqSizing(1).HeatingAirFlow = false;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(6.0, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 46 - Outdoor Air System HX Equipment
    DataSizing::HRFlowSizingFlag = true;
    EnergyPlus::DataSizing::FinalSysSizing(1).DesHeatVolFlow = 0.0;
    EnergyPlus::DataSizing::FinalSysSizing(1).DesOutAirVolFlow = 3.0;
    EnergyPlus::DataSizing::OASysEqSizing.allocate(1);
    state->dataAirLoop->OutsideAirSys.allocate(1);
    DataSizing::CurOASysNum = 1;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(3.0, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 47 - Air Loop HX Equipment, CurDuctType = Main
    EnergyPlus::DataSizing::FinalSysSizing(1).DesOutAirVolFlow = 0.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses main flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 48 - Air Loop HX Equipment, CurDuctType = Cooling
    DataSizing::CurDuctType = DataHVACGlobals::Cooling;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses cooling flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 49 - Air Loop HX Equipment, CurDuctType = Heating
    DataSizing::CurDuctType = DataHVACGlobals::Heating;
    EnergyPlus::DataSizing::FinalSysSizing(1).DesHeatVolFlow = 8.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(8.0, sizedValue, 0.01); // uses heating flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 50 - Air Loop HX Equipment, CurDuctType = Other
    DataSizing::CurDuctType = DataHVACGlobals::Other;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses a main flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 51 - Air Loop HX Equipment, CurDuctType = Main
    DataSizing::CurOASysNum = 0;
    DataSizing::CurDuctType = DataHVACGlobals::Main;
    EnergyPlus::DataSizing::FinalSysSizing(1).DesOutAirVolFlow = 0.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses main flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 52 - Air Loop HX Equipment, CurDuctType = Cooling
    DataSizing::CurDuctType = DataHVACGlobals::Cooling;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses cooling flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 53 - Air Loop HX Equipment, CurDuctType = Heating
    DataSizing::CurDuctType = DataHVACGlobals::Heating;
    EnergyPlus::DataSizing::FinalSysSizing(1).DesHeatVolFlow = 8.0;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(8.0, sizedValue, 0.01); // uses heating flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 54 - Air Loop HX Equipment, CurDuctType = Other
    DataSizing::CurDuctType = DataHVACGlobals::Other;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses a main flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // Test 55 - Air Loop HX Equipment, CurDuctType = RAB
    DataSizing::CurDuctType = DataHVACGlobals::RAB;
    // start with an auto-sized value as the user input
    inputValue = EnergyPlus::DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType);
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(5.0, sizedValue, 0.01); // uses a main flow rate
    sizer.autoSizedValue = 0.0;         // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 56 - Air Loop HX Equipment, CurDuctType = RAB, hard-sized air flow rate
    // start with an hard-sized value as the user input
    inputValue = 2.0;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(2.0, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Maximum Flow Rate [m3/s], 5.00000\n"
                            " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Maximum Flow Rate [m3/s], 2.00000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 57 - Air Loop HX Equipment, CurDuctType = RAB, hard-sized air flow rate
    // start with an hard-sized value as the user input
    inputValue = 2.2;
    DataSizing::DataConstantUsedForSizing = 3.5;
    DataSizing::DataFractionUsedForSizing = 1.0;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_EQ(AutoSizingResultType::NoError, sizer.errorType); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(2.2, sizedValue, 0.01); // hard-sized value
    sizer.autoSizedValue = 0.0;         // reset for next test
    DataSizing::DataConstantUsedForSizing = 0.0;
    DataSizing::DataFractionUsedForSizing = 0.0;

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Maximum Flow Rate [m3/s], 3.50000\n"
        " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Maximum Flow Rate [m3/s], 2.20000\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
