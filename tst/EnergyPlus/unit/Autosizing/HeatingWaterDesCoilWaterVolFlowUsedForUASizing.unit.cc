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

#include <EnergyPlus/Autosizing/HeatingWaterDesCoilWaterVolFlowUsedForUASizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>

namespace EnergyPlus {

TEST_F(AutoSizingFixture, HeatingWaterDesCoilWaterVolFlowUsedForUASizingGauntlet)
{
    static constexpr std::string_view routineName("HeatingWaterDesCoilWaterVolFlowUsedForUASizingSizingGauntlet");

    // create the sizer and set up the flags to specify the sizing configuration
    HeatingWaterDesCoilWaterVolFlowUsedForUASizer sizer;
    Real64 inputValue = 0.0005;
    bool errorsFound = false;
    bool printFlag = false;

    // uninitialized sizing type
    Real64 sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::ErrorType2, sizer.errorType));
    EXPECT_NEAR(0.0, sizedValue, 0.0001); // unitialized sizing types always return 0
    errorsFound = false;

    // ZONE EQUIPMENT TESTING
    state->dataSize->DataPltSizHeatNum = 1;
    state->dataSize->CurZoneEqNum = 1;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);

    // Test #1 - Zone Equipment, no autosizing
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0005, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;              // reset for next test

    // reset eio stream
    has_eio_output(true);
    printFlag = true;

    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0005, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;              // reset for next test

    std::string eiooutput = std::string("! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value\n"
                                        " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Design Water Volume Flow "
                                        "Rate Used for UA Sizing, 5.00000E-004\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    state->dataSize->PlantSizData.allocate(1);
    state->dataSize->DataWaterFlowUsedForSizing = 0.0001;
    state->dataSize->ZoneSizingInput.allocate(1);
    state->dataSize->ZoneSizingInput(state->dataSize->CurZoneEqNum).ZoneNum = state->dataSize->CurZoneEqNum;
    state->dataSize->ZoneSizingRunDone = true;

    // Test 2 - Zone Equipment, Single Duct TU
    state->dataSize->TermUnitSingDuct = true;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0001, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 3 - Zone Equipment, Powered Induction TU
    state->dataSize->DataWaterFlowUsedForSizing = 0.00025;
    state->dataSize->TermUnitSingDuct = false;
    state->dataSize->TermUnitPIU = true;
    state->dataSize->CurTermUnitSizingNum = 1;
    state->dataSize->TermUnitSizing.allocate(1);
    state->dataSize->TermUnitSizing(1).ReheatLoadMult = 0.5;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.000125, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 4 - Zone Equipment, Powered Induction TU
    state->dataSize->DataWaterFlowUsedForSizing = 0.00025;
    state->dataSize->TermUnitSingDuct = false;
    state->dataSize->TermUnitPIU = true;
    state->dataSize->TermUnitSizing.allocate(1);
    state->dataSize->TermUnitSizing(1).ReheatLoadMult = 0.5;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;
    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.000125, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // reset eio stream
    has_eio_output(true);
    eiooutput = "";

    // AIRLOOP EQUIPMENT TESTING - CurDuctType not set, no reporting
    // Test 5 - Airloop Equipment
    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->NumZoneSizingInput = 0;
    state->dataSize->CurTermUnitSizingNum = 0;
    // baseFlags.otherEqType = false; set in initialize function based on other flags
    state->dataSize->ZoneEqSizing.deallocate();

    state->dataSize->CurSysNum = 1;
    state->dataHVACGlobal->NumPrimaryAirSys = 1;
    state->dataSize->NumSysSizInput = 1;
    state->dataSize->SysSizingRunDone = false;
    // start with a hard-sized value as the user input, no system sizing arrays
    inputValue = 0.0003;
    // do sizing
    sizer.wasAutoSized = false;
    printFlag = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0003, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;              // reset for next test
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // Test 6 - Airloop Equipment - CurDuctType not set
    state->dataSize->SysSizingRunDone = true;
    state->dataSize->FinalSysSizing.allocate(1);
    state->dataSize->SysSizInput.allocate(1);
    state->dataSize->SysSizInput(1).AirLoopNum = 1;
    state->dataSize->DataWaterFlowUsedForSizing = 0.0004;

    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0004, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Design Water Volume Flow "
                            "Rate Used for UA Sizing, 4.00000E-004\n");

    EXPECT_TRUE(compare_eio_stream(eiooutput, true));

    // OUTDOOR AIR SYSTEM EQUIPMENT TESTING
    // Test 7 - Outdoor Air System Equipment, no DOAS air loop
    state->dataSize->OASysEqSizing.allocate(1);
    state->dataAirLoop->OutsideAirSys.allocate(1);
    state->dataSize->CurOASysNum = 1;
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0004, sizedValue, 0.0001);
    sizer.autoSizedValue = 0.0; // reset for next test

    // Test 8 - Outdoor Air System Equipment with DOAS system
    state->dataAirLoop->OutsideAirSys(state->dataSize->CurOASysNum).AirLoopDOASNum = 0;
    state->dataAirLoopHVACDOAS->airloopDOAS.emplace_back();
    // start with an auto-sized value as the user input
    inputValue = DataSizing::AutoSize;

    // do sizing
    sizer.wasAutoSized = false;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType));
    EXPECT_TRUE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0004, sizedValue, 0.0001); // uses a mass flow rate for sizing
    sizer.autoSizedValue = 0.0;              // reset for next test

    // reset eio stream
    has_eio_output(true);

    // Test 9 - Outdoor Air System Equipment with DOAS system, hard-sized temperature
    inputValue = 0.0005;

    // do sizing
    sizer.wasAutoSized = false;
    printFlag = true;
    sizer.initializeWithinEP(*this->state, DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::Coil_HeatingWater), "MyWaterCoil", printFlag, routineName);
    sizedValue = sizer.size(*this->state, inputValue, errorsFound);
    EXPECT_TRUE(compare_enums(AutoSizingResultType::NoError, sizer.errorType)); // cumulative of previous calls
    EXPECT_FALSE(sizer.wasAutoSized);
    EXPECT_NEAR(0.0005, sizedValue, 0.0001); // hard-sized value
    sizer.autoSizedValue = 0.0;              // reset for next test

    EXPECT_FALSE(errorsFound);

    // <Component Sizing Information> header already reported above (and flag set false). Only coil sizing information reported here.
    eiooutput = std::string(" Component Sizing Information, Coil:Heating:Water, MyWaterCoil, Design Size Design Water Volume Flow "
                            "Rate Used for UA Sizing, 4.00000E-004\n"
                            " Component Sizing Information, Coil:Heating:Water, MyWaterCoil, User-Specified Design Water Volume Flow "
                            "Rate Used for UA Sizing, 5.00000E-004\n");
    EXPECT_TRUE(compare_eio_stream(eiooutput, true));
}

} // namespace EnergyPlus
