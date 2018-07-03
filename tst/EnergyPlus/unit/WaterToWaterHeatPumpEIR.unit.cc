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

#include <stdexcept>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/WaterToWaterHeatPumpEIR.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EIRWaterToWaterHeatPumps;

TEST_F(EnergyPlusFixture, TestAddFunction) {
    EIRWaterToWaterHeatPump wwhp;
    EXPECT_EQ(2, wwhp.add(1, 1));
}

TEST_F(EnergyPlusFixture, TestEIRWWHPHeatingConstruction) {
    std::string const idf_objects =
            delimited_string(
                    {"HeatPump:WaterToWater:EIR:Heating,", "  hp heating side,", "  node 1,", "  node 2,", "  node 3,",
                     "  node 4,", "0.001,", "0.001,", "1000,", "3.14,", "25.56,", "40.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(96, "HP HEATING SIDE");
    // verify the size of the vector and the processed name
    EXPECT_EQ(1u, eir_wwhp.size());
    EIRWaterToWaterHeatPump *thisWWHP = &eir_wwhp[0];
    EXPECT_EQ("HP HEATING SIDE", thisWWHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRHeating, thisWWHP->plantTypeOfNum);
    // calling the factory with an invalid name should call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRWaterToWaterHeatPump::factory(96, "hp fake name"), std::runtime_error);
    // calling the factory with an invalid type of num should also call ShowFatalError, triggering an exception
    EXPECT_THROW(EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRCooling, "HP HEATING SIDE"), std::runtime_error);
}

TEST_F(EnergyPlusFixture, TestEIRWWHPCoolingConstruction) {
    std::string const idf_objects =
            delimited_string(
                    {"HeatPump:WaterToWater:EIR:Cooling,", "  hp cooling side,", "  node 1,", "  node 2,", "  node 3,",
                     "  node 4,", "0.001,", "0.001,", "1000,", "3.14,", "25.56,", "40.0;"});
    ASSERT_TRUE(process_idf(idf_objects));
    // call the factory with a valid name to trigger reading inputs
    EIRWaterToWaterHeatPump::factory(95, "HP COOLING SIDE");
    // verify the size of the vector and the processed name
    EXPECT_EQ(1u, eir_wwhp.size());
    EIRWaterToWaterHeatPump *thisWWHP = &eir_wwhp[0];
    EXPECT_EQ("HP COOLING SIDE", thisWWHP->name);
    EXPECT_EQ(DataPlant::TypeOf_HeatPumpEIRCooling, thisWWHP->plantTypeOfNum);
    // calling the factory with an invalid name should call ShowFatalError, which will trigger a runtime exception
    EXPECT_THROW(EIRWaterToWaterHeatPump::factory(95, "hp fake name"), std::runtime_error);
    // calling the factory with an invalid type of num should also call ShowFatalError, triggering an exception
    EXPECT_THROW(EIRWaterToWaterHeatPump::factory(DataPlant::TypeOf_HeatPumpEIRHeating, "HP COOLING SIDE"), std::runtime_error);
}