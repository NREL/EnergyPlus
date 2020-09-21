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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "../Coils/CoilCoolingDXFixture.hh"

using namespace EnergyPlus;

TEST_F( CoilCoolingDXTest, CoilCoolingDXInput )
{
    std::string idf_objects = this->getCoilObjectString("coolingCoil", false, 2);
    EXPECT_TRUE(process_idf( idf_objects, false ));
    int coilIndex = CoilCoolingDX::factory(state, "coolingCoil");
    auto const &thisCoil(coilCoolingDXs[coilIndex]);
    EXPECT_EQ("COOLINGCOIL", thisCoil.name);
    EXPECT_EQ("PERFORMANCEOBJECTNAME", thisCoil.performance.name);
}

TEST_F( CoilCoolingDXTest, CoilCoolingDXAlternateModePerformance )
{
    std::string idf_objects = delimited_string({
        "  Coil:Cooling:DX,",
        "    Coil,",
        "    Evaporator Inlet Node,Evaporator Outlet Node,",
        "    ,,",
        "    Condenser Inlet Node,Condenser Outlet Node,",
        "    Coil Performance,",
        "    ,;",

        "  Coil:Cooling:DX:CurveFit:Performance,",
        "    Coil Performance,,,,,,,,,Electricity,Coil Mode 1,Coil Mode 2;",

        "  Coil:Cooling:DX:CurveFit:OperatingMode,",
        "    Coil Mode 1,",
        "    5000,   !- Rated Gross Total Cooling Capacity {W}",
        "    0.25,   !- Rated Evaporator Air Flow Rate {m3/s}",
        "    ,,,,,,,,",
        "    2,Coil Mode 1 Speed 1,Coil Mode 1 Speed 2;",

        "  Coil:Cooling:DX:CurveFit:Speed,",
        "    Coil Mode 1 Speed 1,     !- Name",
        "    0.50,                    !- Gross Total Cooling Capacity Fraction",
        "    0.50,                    !- Evaporator Air Flow Rate Fraction",
        "    ,                        !- Condenser Air Flow Rate Fraction",
        "    0.7,                     !- Gross Sensible Heat Ratio",
        "    3,                       !- Gross Cooling COP {W/W}",
        "    0.5,                     !- Active Fraction of Coil Face Area",
        "    ,,,,,,,,,,,;",

        "  Coil:Cooling:DX:CurveFit:Speed,",
        "    Coil Mode 1 Speed 2,     !- Name",
        "    1.0,                     !- Gross Total Cooling Capacity Fraction",
        "    1.0,                     !- Evaporator Air Flow Rate Fraction",
        "    ,                        !- Condenser Air Flow Rate Fraction",
        "    0.7,                     !- Gross Sensible Heat Ratio",
        "    3,                       !- Gross Cooling COP {W/W}",
        "    1.0,                     !- Active Fraction of Coil Face Area",
        "    ,,,,,,,,,,,;",

        "  Coil:Cooling:DX:CurveFit:OperatingMode,",
        "    Coil Mode 2,",
        "    5000,   !- Rated Gross Total Cooling Capacity {W}",
        "    0.25,   !- Rated Evaporator Air Flow Rate {m3/s}",
        "    ,,,,,,,,",
        "    2,Coil Mode 2 Speed 1,Coil Mode 2 Speed 2;",

        "  Coil:Cooling:DX:CurveFit:Speed,",
        "    Coil Mode 2 Speed 1,     !- Name",
        "    0.45,                    !- Gross Total Cooling Capacity Fraction",
        "    0.50,                    !- Evaporator Air Flow Rate Fraction",
        "    ,                        !- Condenser Air Flow Rate Fraction",
        "    0.6,                     !- Gross Sensible Heat Ratio",
        "    2.7,                     !- Gross Cooling COP {W/W}",
        "    0.5,                     !- Active Fraction of Coil Face Area",
        "    ,,,,,,,,,,,;",

        "  Coil:Cooling:DX:CurveFit:Speed,",
        "    Coil Mode 2 Speed 2,     !- Name",
        "    0.9,                     !- Gross Total Cooling Capacity Fraction",
        "    1.0,                     !- Evaporator Air Flow Rate Fraction",
        "    ,                        !- Condenser Air Flow Rate Fraction",
        "    0.6,                     !- Gross Sensible Heat Ratio",
        "    2.7,                     !- Gross Cooling COP {W/W}",
        "    1.0,                     !- Active Fraction of Coil Face Area",
        "    ,,,,,,,,,,,;"
    });
    EXPECT_TRUE(process_idf( idf_objects, false ));
    int coilIndex = CoilCoolingDX::factory(state, "Coil");
    auto &thisCoil(coilCoolingDXs[coilIndex]);

    // fix the inlet conditions
    auto &evapInletNode = DataLoopNode::Node(thisCoil.evapInletNodeIndex);
    auto &condInletNode = DataLoopNode::Node(thisCoil.condInletNodeIndex);
    evapInletNode.Temp = 28.5;
    evapInletNode.Press = 101325;
    evapInletNode.HumRat = 0.014;
    evapInletNode.Enthalpy = Psychrometrics::PsyHFnTdbW(evapInletNode.Temp, evapInletNode.HumRat);
    condInletNode.Temp = 35.0;
    condInletNode.Press = 101325;
    condInletNode.HumRat = 0.008;
    condInletNode.Enthalpy = Psychrometrics::PsyHFnTdbW(condInletNode.Temp, condInletNode.HumRat);

    // size it
    thisCoil.size(state);

    // for speed > 1 we use the mshp rated high speed flow...
    DataHVACGlobals::MSHPMassFlowRateHigh = thisCoil.performance.normalMode.speeds.back().RatedAirMassFlowRate;

    // we'll use this later
    auto &evapOutletNode = DataLoopNode::Node(thisCoil.evapOutletNodeIndex);

    // set some values to run at rated conditions and call to run normal mode speed 1
    evapInletNode.MassFlowRate = thisCoil.performance.normalMode.speeds.front().RatedAirMassFlowRate;
    int useAlternateMode = DataHVACGlobals::coilNormalMode;
    Real64 PLR = 1.0;
    int speedNum = 1;
    Real64 speedRatio = 1.0;
    int fanOpMode = 1;
    bool singleMode = false;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
//    std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    EXPECT_NEAR(2500, thisCoil.totalCoolingEnergyRate, 0.1); // expect the coil to run full out, at speed 1
    EXPECT_NEAR(19.485, evapOutletNode.Temp, 0.01);
    EXPECT_NEAR(0.0114, evapOutletNode.HumRat, 0.001);

    // alter values and run at rated conditions normal mode speed 2
    evapInletNode.MassFlowRate = thisCoil.performance.normalMode.speeds.back().RatedAirMassFlowRate;
    speedNum = 2;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
//    std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    EXPECT_NEAR(5000, thisCoil.totalCoolingEnergyRate, 0.01); // expect the coil to run full out, at speed 1
    EXPECT_NEAR(17.896, evapOutletNode.Temp, 0.01);
    EXPECT_NEAR(0.0114, evapOutletNode.HumRat, 0.001);

    // ok so now run at alternate mode, speed 1
    useAlternateMode = DataHVACGlobals::coilEnhancedMode;
    speedNum = 1;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
//    std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    EXPECT_NEAR(2250, thisCoil.totalCoolingEnergyRate, 0.01); // expect the coil to run full out, at speed 1
    EXPECT_NEAR(24.45, evapOutletNode.Temp, 0.01);
    EXPECT_NEAR(0.0126, evapOutletNode.HumRat, 0.0001);

    // ok so now run at alternate mode, speed 2
    useAlternateMode = DataHVACGlobals::coilEnhancedMode;
    speedNum = 2;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
//    std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    EXPECT_NEAR(4500, thisCoil.totalCoolingEnergyRate, 0.01); // expect the coil to run full out, at speed 1
    EXPECT_NEAR(20.39, evapOutletNode.Temp, 0.01);
    EXPECT_NEAR(0.0111, evapOutletNode.HumRat, 0.0001);
}


TEST_F( CoilCoolingDXTest, CoilCoolingDXAlternateModePerformanceHitsSaturation )
{
    std::string idf_objects = delimited_string({
                                                       "  Coil:Cooling:DX,",
                                                       "    Coil,",
                                                       "    Evaporator Inlet Node,Evaporator Outlet Node,",
                                                       "    ,,",
                                                       "    Condenser Inlet Node,Condenser Outlet Node,",
                                                       "    Coil Performance,",
                                                       "    ,;",

                                                       "  Coil:Cooling:DX:CurveFit:Performance,",
                                                       "    Coil Performance,,,,,,,,,Electricity,Coil Mode 1,Coil Mode 2;",

                                                       "  Coil:Cooling:DX:CurveFit:OperatingMode,",
                                                       "    Coil Mode 1,",
                                                       "    10000,   !- Rated Gross Total Cooling Capacity {W}",
                                                       "    0.25,   !- Rated Evaporator Air Flow Rate {m3/s}",
                                                       "    ,,,,,,,,",
                                                       "    2,Coil Mode 1 Speed 1,Coil Mode 1 Speed 2;",

                                                       "  Coil:Cooling:DX:CurveFit:Speed,",
                                                       "    Coil Mode 1 Speed 1,     !- Name",
                                                       "    0.50,                    !- Gross Total Cooling Capacity Fraction",
                                                       "    0.50,                    !- Evaporator Air Flow Rate Fraction",
                                                       "    ,                        !- Condenser Air Flow Rate Fraction",
                                                       "    0.7,                     !- Gross Sensible Heat Ratio",
                                                       "    3,                       !- Gross Cooling COP {W/W}",
                                                       "    0.5,                     !- Active Fraction of Coil Face Area",
                                                       "    ,,,,,,,,,,,;",

                                                       "  Coil:Cooling:DX:CurveFit:Speed,",
                                                       "    Coil Mode 1 Speed 2,     !- Name",
                                                       "    1.0,                     !- Gross Total Cooling Capacity Fraction",
                                                       "    1.0,                     !- Evaporator Air Flow Rate Fraction",
                                                       "    ,                        !- Condenser Air Flow Rate Fraction",
                                                       "    0.7,                     !- Gross Sensible Heat Ratio",
                                                       "    3,                       !- Gross Cooling COP {W/W}",
                                                       "    1.0,                     !- Active Fraction of Coil Face Area",
                                                       "    ,,,,,,,,,,,;",

                                                       "  Coil:Cooling:DX:CurveFit:OperatingMode,",
                                                       "    Coil Mode 2,",
                                                       "    10000,   !- Rated Gross Total Cooling Capacity {W}",
                                                       "    0.25,   !- Rated Evaporator Air Flow Rate {m3/s}",
                                                       "    ,,,,,,,,",
                                                       "    2,Coil Mode 2 Speed 1,Coil Mode 2 Speed 2;",

                                                       "  Coil:Cooling:DX:CurveFit:Speed,",
                                                       "    Coil Mode 2 Speed 1,     !- Name",
                                                       "    0.45,                    !- Gross Total Cooling Capacity Fraction",
                                                       "    0.50,                    !- Evaporator Air Flow Rate Fraction",
                                                       "    ,                        !- Condenser Air Flow Rate Fraction",
                                                       "    0.6,                     !- Gross Sensible Heat Ratio",
                                                       "    2.7,                     !- Gross Cooling COP {W/W}",
                                                       "    0.5,                     !- Active Fraction of Coil Face Area",
                                                       "    ,,,,,,,,,,,;",

                                                       "  Coil:Cooling:DX:CurveFit:Speed,",
                                                       "    Coil Mode 2 Speed 2,     !- Name",
                                                       "    0.9,                     !- Gross Total Cooling Capacity Fraction",
                                                       "    1.0,                     !- Evaporator Air Flow Rate Fraction",
                                                       "    ,                        !- Condenser Air Flow Rate Fraction",
                                                       "    0.6,                     !- Gross Sensible Heat Ratio",
                                                       "    2.7,                     !- Gross Cooling COP {W/W}",
                                                       "    1.0,                     !- Active Fraction of Coil Face Area",
                                                       "    ,,,,,,,,,,,;"
                                               });
    EXPECT_TRUE(process_idf( idf_objects, false ));
    int coilIndex = CoilCoolingDX::factory(state, "Coil");
    auto &thisCoil(coilCoolingDXs[coilIndex]);

    // fix the inlet conditions
    auto &evapInletNode = DataLoopNode::Node(thisCoil.evapInletNodeIndex);
    auto &condInletNode = DataLoopNode::Node(thisCoil.condInletNodeIndex);
    evapInletNode.Temp = 28.5;
    evapInletNode.Press = 101325;
    evapInletNode.HumRat = 0.014;
    evapInletNode.Enthalpy = Psychrometrics::PsyHFnTdbW(evapInletNode.Temp, evapInletNode.HumRat);
    condInletNode.Temp = 35.0;
    condInletNode.Press = 101325;
    condInletNode.HumRat = 0.008;
    condInletNode.Enthalpy = Psychrometrics::PsyHFnTdbW(condInletNode.Temp, condInletNode.HumRat);

    // size it
    thisCoil.size(state);

    // for speed > 1 we use the mshp rated high speed flow...
    DataHVACGlobals::MSHPMassFlowRateHigh = thisCoil.performance.normalMode.speeds.back().RatedAirMassFlowRate;

    // we'll use this later
    auto &evapOutletNode = DataLoopNode::Node(thisCoil.evapOutletNodeIndex);

    bool setExpectations = true;

    // set some values to run at rated conditions and call to run normal mode speed 1
    evapInletNode.MassFlowRate = thisCoil.performance.normalMode.speeds.front().RatedAirMassFlowRate;
    int useAlternateMode = DataHVACGlobals::coilNormalMode;
    Real64 PLR = 1.0;
    int speedNum = 1;
    Real64 speedRatio = 1.0;
    int fanOpMode = 1;
    bool singleMode = false;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
    if (!setExpectations) {
        std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    } else {
        EXPECT_NEAR(5000, thisCoil.totalCoolingEnergyRate, 0.1); // expect the coil to run full out, at speed 1
        EXPECT_NEAR(9.934, evapOutletNode.Temp, 0.01);
        EXPECT_NEAR(0.00787, evapOutletNode.HumRat, 0.0001);
    }
    // alter values and run at rated conditions normal mode speed 2
    evapInletNode.MassFlowRate = thisCoil.performance.normalMode.speeds.back().RatedAirMassFlowRate;
    speedNum = 2;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
    if (!setExpectations) {
        std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    } else {
        EXPECT_NEAR(10000, thisCoil.totalCoolingEnergyRate, 0.01); // expect the coil to run full out, at speed 1
        EXPECT_NEAR(10.247, evapOutletNode.Temp, 0.01);
        EXPECT_NEAR(0.00774, evapOutletNode.HumRat, 0.0001);
    }

    // ok so now run at alternate mode, speed 1
    useAlternateMode = DataHVACGlobals::coilEnhancedMode;
    speedNum = 1;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
    if (!setExpectations) {
        std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    } else {
        EXPECT_NEAR(4500, thisCoil.totalCoolingEnergyRate, 0.01); // expect the coil to run full out, at speed 1
        EXPECT_NEAR(20.373, evapOutletNode.Temp, 0.01);
        EXPECT_NEAR(0.0111, evapOutletNode.HumRat, 0.0001);
    }

    // ok so now run at alternate mode, speed 2
    useAlternateMode = DataHVACGlobals::coilEnhancedMode;
    speedNum = 2;
    thisCoil.simulate(state, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode, singleMode);
    if (!setExpectations) {
        std::cout << thisCoil.totalCoolingEnergyRate << ',' << evapOutletNode.Temp << ',' << evapOutletNode.HumRat << std::endl;
    } else {
        EXPECT_NEAR(9000, thisCoil.totalCoolingEnergyRate, 0.01); // expect the coil to run full out, at speed 1
        EXPECT_NEAR(12.163, evapOutletNode.Temp, 0.01);
        EXPECT_NEAR(0.00833, evapOutletNode.HumRat, 0.0001);
    }
}
