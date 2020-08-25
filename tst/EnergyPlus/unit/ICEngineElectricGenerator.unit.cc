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

// EnergyPlus::ICEngineElectricGenerator Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ICEngineElectricGenerator.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::ICEngineElectricGenerator;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, ICEngineElectricGenerator_Fueltype)
{
    std::string const idf_objects = delimited_string({
        "Generator:InternalCombustionEngine,",
        "  Cat Diesel,              !- Name",
        "  50000,                   !- Rated Power Output {W}",
        "  Generator Diesel Electric Node,  !- Electric Circuit Node Name",
        "  0.15,                    !- Minimum Part Load Ratio",
        "  1.0,                     !- Maximum Part Load Ratio",
        "  0.65,                    !- Optimum Part Load Ratio",
        "  BG Shaft Power Curve,    !- Shaft Power Curve Name",
        "  BG Recovery Jacket Heat Curve,  !- Jacket Heat Recovery Curve Name",
        "  BG Recovery Lube Heat Curve,  !- Lube Heat Recovery Curve Name",
        "  BG Total Exhaust Energy Curve,  !- Total Exhaust Energy Curve Name",
        "  BG Exhaust Temperature Curve,  !- Exhaust Temperature Curve Name",
        "  0.00952329,              !- Coefficient 1 of U-Factor Times Area Curve",
        "  0.9,                     !- Coefficient 2 of U-Factor Times Area Curve",
        "  0.00000063,              !- Maximum Exhaust Flow per Unit of Power Output {(kg/s)/W}",
        "  150,                     !- Design Minimum Exhaust Temperature {C}",
        "  45500,                   !- Fuel Higher Heating Value {kJ/kg}",
        "  0.0,                     !- Design Heat Recovery Water Flow Rate {m3/s}",
        "  ,                        !- Heat Recovery Inlet Node Name",
        "  ,                        !- Heat Recovery Outlet Node Name",
        "  Diesel;                  !- Fuel Type",

        "Curve:Quadratic,",
        "  BG Shaft Power Curve,    !- Name",
        "  0.09755,                 !- Coefficient1 Constant",
        "  0.6318,                  !- Coefficient2 x",
        "  -0.4165,                 !- Coefficient3 x**2",
        "  0,                       !- Minimum Value of x",
        "  1;                       !- Maximum Value of x",

        "Curve:Quadratic,",
        "  BG Recovery Jacket Heat Curve,  !- Name",
        "  0.25,                    !- Coefficient1 Constant",
        "  0,                       !- Coefficient2 x",
        "  0,                       !- Coefficient3 x**2",
        "  0,                       !- Minimum Value of x",
        "  1;                       !- Maximum Value of x",

        "Curve:Quadratic,",
        "  BG Recovery Lube Heat Curve,  !- Name",
        "  0.15,                    !- Coefficient1 Constant",
        "  0,                       !- Coefficient2 x",
        "  0,                       !- Coefficient3 x**2",
        "  0,                       !- Minimum Value of x",
        "  1;                       !- Maximum Value of x",

        "Curve:Quadratic,",
        "  BG Total Exhaust Energy Curve,  !- Name",
        "  0.1,                     !- Coefficient1 Constant",
        "  0,                       !- Coefficient2 x",
        "  0,                       !- Coefficient3 x**2",
        "  0,                       !- Minimum Value of x",
        "  1;                       !- Maximum Value of x",

        "Curve:Quadratic,",
        "  BG Exhaust Temperature Curve,  !- Name",
        "  425,                     !- Coefficient1 Constant",
        "  0,                       !- Coefficient2 x",
        "  0,                       !- Coefficient3 x**2",
        "  0,                       !- Minimum Value of x",
        "  1;                       !- Maximum Value of x",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetICEngineGeneratorInput();

    EXPECT_EQ(ICEngineGenerator(1).FuelType, "Diesel");
}

} // namespace EnergyPlus
