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

// EnergyPlus::ZoneEquipmentManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/FluidProperties.hh>

#include <ctgmath>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::FluidProperties;

TEST_F(EnergyPlusFixture, FluidProperties_GetDensityGlycol)
{

    std::string const idf_objects = delimited_string({"FluidProperties:GlycolConcentration,",
                                                      "  GLHXFluid,       !- Name",
                                                      "  PropyleneGlycol, !- Glycol Type",
                                                      "  ,                !- User Defined Glycol Name",
                                                      "  0.3;             !- Glycol Concentration",
                                                      " "});

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    int FluidIndex = 0;

    EXPECT_NEAR(1037.89, GetDensityGlycol(*state, "GLHXFLUID", -35.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(1037.89, GetDensityGlycol(*state, "GLHXFLUID", -15.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(1034.46, GetDensityGlycol(*state, "GLHXFLUID", 5.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(1030.51, GetDensityGlycol(*state, "GLHXFLUID", 15.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(1026.06, GetDensityGlycol(*state, "GLHXFLUID", 25.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(1021.09, GetDensityGlycol(*state, "GLHXFLUID", 35.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(1015.62, GetDensityGlycol(*state, "GLHXFLUID", 45.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(1003.13, GetDensityGlycol(*state, "GLHXFLUID", 65.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(988.60, GetDensityGlycol(*state, "GLHXFLUID", 85.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(972.03, GetDensityGlycol(*state, "GLHXFLUID", 105.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(953.41, GetDensityGlycol(*state, "GLHXFLUID", 125.0, FluidIndex, "UnitTest"), 0.01);
}

TEST_F(EnergyPlusFixture, FluidProperties_GetSpecificHeatGlycol)
{

    std::string const idf_objects = delimited_string({"FluidProperties:GlycolConcentration,",
                                                      "  GLHXFluid,       !- Name",
                                                      "  PropyleneGlycol, !- Glycol Type",
                                                      "  ,                !- User Defined Glycol Name",
                                                      "  0.3;             !- Glycol Concentration",
                                                      " "});

    ASSERT_TRUE(process_idf(idf_objects));
    EXPECT_FALSE(has_err_output());

    int FluidIndex = 0;

    EXPECT_NEAR(3779, GetSpecificHeatGlycol(*state, "GLHXFLUID", -35.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(3779, GetSpecificHeatGlycol(*state, "GLHXFLUID", -15.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(3807, GetSpecificHeatGlycol(*state, "GLHXFLUID", 5.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(3834, GetSpecificHeatGlycol(*state, "GLHXFLUID", 15.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(3862, GetSpecificHeatGlycol(*state, "GLHXFLUID", 25.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(3889, GetSpecificHeatGlycol(*state, "GLHXFLUID", 35.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(3917, GetSpecificHeatGlycol(*state, "GLHXFLUID", 45.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(3972, GetSpecificHeatGlycol(*state, "GLHXFLUID", 65.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(4027, GetSpecificHeatGlycol(*state, "GLHXFLUID", 85.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(4082, GetSpecificHeatGlycol(*state, "GLHXFLUID", 105.0, FluidIndex, "UnitTest"), 0.01);
    EXPECT_NEAR(4137, GetSpecificHeatGlycol(*state, "GLHXFLUID", 125.0, FluidIndex, "UnitTest"), 0.01);
}
