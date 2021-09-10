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

// EnergyPlus::EarthTube Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/EarthTube.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EarthTube;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataEnvironment;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, EarthTube_CalcEarthTubeHumRatTest)
{

    // AUTHOR: R. Strand, UIUC
    // DATE WRITTEN: June 2017

    // Set subroutine arguments
    int ETnum = 1;
    int ZNnum = 1;

    // Set environmental variables for all cases
    state->dataEnvrn->OutHumRat = 0.009;
    state->dataEnvrn->OutBaroPress = 101400.0;

    // Allocate and set earth tube parameters necessary to run the tests
    state->dataEarthTube->EarthTubeSys.allocate(ETnum);
    state->dataEarthTube->EarthTubeSys(ETnum).InsideAirTemp = 21.0;
    state->dataEarthTube->EarthTubeSys(ETnum).FanType = EarthTubeVentilation::Natural;
    state->dataEarthTube->EarthTubeSys(ETnum).AirTemp = 20.0;
    state->dataEarthTube->EarthTubeSys(ETnum).FanPower = 0.05;

    // Allocate and set any zone variables necessary to run the tests
    state->dataHeatBalFanSys->MCPE.allocate(ZNnum);
    state->dataHeatBalFanSys->MCPTE.allocate(ZNnum);
    state->dataHeatBalFanSys->EAMFL.allocate(ZNnum);
    state->dataHeatBalFanSys->EAMFLxHumRat.allocate(ZNnum);
    state->dataHeatBalFanSys->MCPE(ZNnum) = 0.05;
    state->dataHeatBalFanSys->EAMFL(ZNnum) = 0.05;

    // First case--no condensation so inside humidity ratio should be the same as the outdoor humidity ratio
    CalcEarthTubeHumRat(*state, ETnum, ZNnum);
    EXPECT_EQ(state->dataEarthTube->EarthTubeSys(ETnum).HumRat, state->dataEnvrn->OutHumRat);

    // Second case--condensation so inside humidity should be less than outdoor humidity ratio
    state->dataEarthTube->EarthTubeSys(ETnum).InsideAirTemp = 10.0;
    CalcEarthTubeHumRat(*state, ETnum, ZNnum);
    EXPECT_GT(state->dataEnvrn->OutHumRat, state->dataEarthTube->EarthTubeSys(ETnum).HumRat);
}

TEST_F(EnergyPlusFixture, EarthTube_CheckEarthTubesInZonesTest)
{

    // AUTHOR: R. Strand, UIUC
    // DATE WRITTEN: June 2017

    // Set subroutine arguments
    std::string ZoneName = "ZONE 1";
    std::string InputName = "ZoneEarthtube";
    bool ErrorsFound = false;

    // Allocate and set earth tube parameters necessary to run the tests
    state->dataEarthTube->TotEarthTube = 3;
    state->dataEarthTube->EarthTubeSys.allocate(state->dataEarthTube->TotEarthTube);
    state->dataEarthTube->EarthTubeSys(1).ZonePtr = 1;
    state->dataEarthTube->EarthTubeSys(2).ZonePtr = 2;
    state->dataEarthTube->EarthTubeSys(3).ZonePtr = 3;

    // First case--no conflicts, only one earth tube per zone (ErrorsFound = false)
    CheckEarthTubesInZones(*state, ZoneName, InputName, ErrorsFound);
    EXPECT_EQ(ErrorsFound, false);

    // Second case--conflict with the last earth tube and first (ErrorsFound = true)
    state->dataEarthTube->EarthTubeSys(3).ZonePtr = 1;
    CheckEarthTubesInZones(*state, ZoneName, InputName, ErrorsFound);
    EXPECT_EQ(ErrorsFound, true);

    state->dataEarthTube->EarthTubeSys.deallocate();
    state->dataEarthTube->TotEarthTube = 0;
}

} // namespace EnergyPlus
