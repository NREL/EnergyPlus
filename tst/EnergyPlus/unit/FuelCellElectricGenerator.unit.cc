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

// EnergyPlus::FuelCellElectricGenerator Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// C++ Headers
#include <memory>
#include <vector>
// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/ElectricPowerServiceManager.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, FuelCellTest_LowInletEnthalySover1)
{

    std::string const idf_objects = delimited_string({
        "Version,8.9;",

        "ElectricLoadCenter:Distribution,",
        "Electric Load Center Distribution 1,    !- Name",
        "Electric Load Center Distribution 1 Generators, !- Generator List Name",
        "TrackSchedule,                          !- Generator Operation Scheme Type",
        "0,                                      !- Generator Demand Limit Scheme Purchased Electric Demand Limit {W}",
        "Generator Demand,                       !- Generator Track Schedule Name Scheme Schedule Name",
        ",                                       !- Generator Track Meter Scheme Meter Name",
        "AlternatingCurrent;                     !- Electrical Buss Type",

        "ElectricLoadCenter:Generators,",
        "Electric Load Center Distribution 1 Generators, !- Name",
        "Generator Fuel Cell 1,                  !- Generator Name 1",
        "Generator:FuelCell,                     !- Generator Object Type 1",
        "3400,                                   !- Generator Rated Electric Power Output 1 {W}",
        ",                                       !- Generator Availability Schedule Name 1",
        ";                                       !- Generator Rated Thermal to Electrical Power Ratio 1",

        "Generator:FuelCell,",
        "Generator Fuel Cell 1,                  !- Name",
        "Generator Fuel Cell Power Module 1,     !- Power Module Name",
        "Generator Fuel Cell Air Supply 1,       !- Air Supply Name",
        "NATURALGAS,                             !- Fuel Supply Name",
        "Generator Fuel Cell Water Supply 1,     !- Water Supply Name",
        "Generator Fuel Cell Auxiliary Heater 1, !- Auxiliary Heater Name",
        ", !- Heat Exchanger Name",
        "Generator Fuel Cell Electrical Storage 1, !- Electrical Storage Name",
        "Generator Fuel Cell Inverter 1;         !- Inverter Name",

        "Generator:FuelCell:PowerModule,",
        "Generator Fuel Cell Power Module 1,     !- Name",
        "Normalized,                             !- Efficiency Curve Mode",
        "FCT Power Curve,                        !- Efficiency Curve Name",
        "0.354,                                  !- Nominal Efficiency",
        "3400,                                   !- Nominal Electrical Power {W}",
        "0,                                      !- Number of Stops at Start of Simulation",
        "0,                                      !- Cycling Performance Degradation Coefficient",
        "1000,                                   !- Number of Run Hours at Beginning of Simulation {hr}",
        "0,                                      !- Accumulated Run Time Degradation Coefficient",
        "100000,                                 !- Run Time Degradation Initiation Time Threshold {hr}",
        "1.4,                                    !- Power Up Transient Limit {W/s}",
        "0.2,                                    !- Power Down Transient Limit {W/s}",
        "0,                                      !- Start Up Time {s}",
        "0.2,                                    !- Start Up Fuel {kmol}",
        "0,                                      !- Start Up Electricity Consumption {J}",
        "0,                                      !- Start Up Electricity Produced {J}",
        "0,                                      !- Shut Down Time {s}",
        "0.2,                                    !- Shut Down Fuel {kmol}",
        "0,                                      !- Shut Down Electricity Consumption {J}",
        "0,                                      !- Ancillary Electricity Constant Term",
        "0,                                      !- Ancillary Electricity Linear Term",
        "ConstantRate,                           !- Skin Loss Calculation Mode",
        "Core_bottom ZN,                         !- Zone Name",
        "0.6392,                                 !- Skin Loss Radiative Fraction",
        "729,                                    !- Constant Skin Loss Rate {W}",
        "0,                                      !- Skin Loss U-Factor Times Area Term {W/K}",
        ",                                       !- Skin Loss Quadratic Curve Name",
        "0.006156,                               !- Dilution Air Flow Rate {kmol/s}",
        "2307,                                   !- Stack Heat loss to Dilution Air {W}",
        "Generator Fuel Cell Power Module 1 OA Node, !- Dilution Inlet Air Node Name",
        "Generator Fuel Cell Power Module 1 Dilution Outlet Air Node, !- Dilution Outlet Air Node Name",
        "0,                                      !- Minimum Operating Point {W}",
        "500000;                                 !- Maximum Operating Point {W}",

        "Curve:Quadratic,",
        "FCT Power Curve,                        !- Name",
        "0.642388,                               !- Coefficient1 Constant",
        "-0.0001619,                             !- Coefficient2 x",
        "2.26007e-008,                           !- Coefficient3 x**2",
        "0,                                      !- Minimum Value of x {BasedOnField A2}",
        "10000;                                  !- Maximum Value of x {BasedOnField A2}",

        "Generator:FuelCell:Inverter,",
        "Generator Fuel Cell Inverter 1,         !- Name",
        "Constant,                               !- Inverter Efficiency Calculation Mode",
        "1,                                      !- Inverter Efficiency",
        "FCT Inverter Quadratic;                 !- Efficiency Function of DC Power Curve Name",

        "Generator:FuelCell:AirSupply,",
        "Generator Fuel Cell Air Supply 1,       !- Name",
        "Generator Fuel Cell Air Supply 1 OA Node, !- Air Inlet Node Name",
        "Null Curve,                             !- Blower Power Curve Name",
        "1,                                      !- Blower Heat Loss Factor",
        "QuadraticFunctionofElectricPower,       !- Air Supply Rate Calculation Mode",
        "0,                                      !- Stoichiometric Ratio",
        "FCT Excess Air Ratio Curve,             !- Air Rate Function of Electric Power Curve Name",
        "0.00283507,                             !- Air Rate Air Temperature Coefficient",
        ",                                       !- Air Rate Function of Fuel Rate Curve Name",
        "NoRecovery,                             !- Air Intake Heat Recovery Mode",
        "UserDefinedConstituents,                !- Air Supply Constituent Mode",
        "5,                                      !- Number of UserDefined Constituents",
        "Nitrogen,                               !- Constituent Name 1",
        "0.7728,                                 !- Molar Fraction 1",
        "Oxygen,                                 !- Constituent Name 2",
        "0.2073,                                 !- Molar Fraction 2",
        "Water,                                  !- Constituent Name 3",
        "0.0104,                                 !- Molar Fraction 3",
        "Argon,                                  !- Constituent Name 4",
        "0.0092,                                 !- Molar Fraction 4",
        "CarbonDioxide,                          !- Constituent Name 5",
        "0.0003;                                 !- Molar Fraction 5",

        "Curve:Cubic,",
        "Null Curve, !- Name",
        "0,                                      !- Coefficient1 Constant",
        "0,                                      !- Coefficient2 x",
        "0,                                      !- Coefficient3 x**2",
        "0,                                      !- Coefficient4 x**3",
        "0,                                      !- Minimum Value of x {BasedOnField A2}",
        "10000000000;                            !- Maximum Value of x {BasedOnField A2}",

        "  Schedule:Compact,",
        "    ALWAYS_ON,               !- Name",
        "    On/Off,                  !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1;          !- Field 3",
    });
}
