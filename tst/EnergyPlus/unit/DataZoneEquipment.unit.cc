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

// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataZoneEquipment;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, DataZoneEquipment_TestGetSystemNodeNumberForZone)
{

    state->dataGlobal->NumOfZones = 2;
    state->dataZoneEquip->ZoneEquipConfig.allocate(state->dataGlobal->NumOfZones);

    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone1";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneNode = 1;

    state->dataZoneEquip->ZoneEquipConfig(2).ZoneName = "Zone2";
    state->dataZoneEquip->ZoneEquipConfig(2).ActualZoneNum = 2;
    state->dataZoneEquip->ZoneEquipConfig(2).ZoneNode = 2;

    state->dataZoneEquip->ZoneEquipInputsFilled = true;

    EXPECT_EQ(0, GetSystemNodeNumberForZone(*state, "NonExistingZone"));
    EXPECT_EQ(1, GetSystemNodeNumberForZone(*state, "Zone1"));

    state->dataZoneEquip->ZoneEquipConfig.deallocate();
}

TEST_F(EnergyPlusFixture, DataZoneEquipment_TestCalcDesignSpecificationOutdoorAir)
{
    // #6225

    state->dataHeatBal->Zone.allocate(1);
    state->dataSize->OARequirements.allocate(1);
    state->dataHeatBal->ZoneIntGain.allocate(1);
    state->dataHeatBal->People.allocate(1);
    state->dataScheduleMgr->Schedule.allocate(2);
    state->dataContaminantBalance->ZoneCO2GainFromPeople.allocate(1);
    state->dataContaminantBalance->ZoneAirCO2.allocate(1);
    state->dataContaminantBalance->ZoneSysContDemand.allocate(1);

    state->dataEnvrn->StdRhoAir = 1.20;

    state->dataHeatBal->Zone(1).FloorArea = 10.0;
    state->dataHeatBal->Zone(1).TotOccupants = 5.0;
    state->dataHeatBal->Zone(1).ZoneContamControllerSchedIndex = 1;
    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->TotPeople = 1;
    state->dataHeatBal->People(1).ActivityLevelPtr = 2;
    state->dataHeatBal->People(1).CO2RateFactor = 3.82e-8;
    state->dataHeatBal->People(1).NumberOfPeople = state->dataHeatBal->Zone(1).TotOccupants;

    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataContaminantBalance->OutdoorCO2 = 400.0;
    state->dataContaminantBalance->ZoneCO2GainFromPeople(1) = 3.82E-8 * 5.0;

    state->dataSize->NumOARequirements = 1;
    state->dataSize->OARequirements(1).Name = "ZONE OA";
    state->dataSize->OARequirements(1).OAFlowMethod = DataSizing::ZOAM_ProportionalControlSchOcc;
    state->dataSize->OARequirements(1).OAFlowPerPerson = 0.002;
    state->dataSize->OARequirements(1).OAFlowPerArea = 0.003;
    state->dataHeatBal->ZoneIntGain(1).NOFOCC = 0.5;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1.0;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 131.881995;

    Real64 OAVolumeFlowRate;
    // Test ZOAM_ProportionalControlSchOcc
    state->dataContaminantBalance->ZoneAirCO2(1) = 500.0;
    OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.031, OAVolumeFlowRate, 0.00001);

    state->dataContaminantBalance->ZoneAirCO2(1) = 405.0;
    OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.0308115, OAVolumeFlowRate, 0.00001);

    // Test ZOAM_ProportionalControlDesOcc
    state->dataContaminantBalance->ZoneAirCO2(1) = 500.0;
    state->dataSize->OARequirements(1).OAFlowMethod = DataSizing::ZOAM_ProportionalControlDesOcc;
    OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.0315879, OAVolumeFlowRate, 0.00001);

    // Test ZOAM_IAQP
    state->dataSize->OARequirements(1).OAFlowMethod = DataSizing::ZOAM_IAQP;
    state->dataContaminantBalance->ZoneSysContDemand(1).OutputRequiredToCO2SP = 0.2 * state->dataEnvrn->StdRhoAir;
    OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(*state, 1, 1, false, false);
    EXPECT_NEAR(0.2, OAVolumeFlowRate, 0.00001);

    // Cleanup
    state->dataHeatBal->Zone.deallocate();
    state->dataSize->OARequirements.deallocate();
    state->dataHeatBal->ZoneIntGain.deallocate();
    state->dataScheduleMgr->Schedule.deallocate();
    state->dataHeatBal->People.deallocate();
    state->dataContaminantBalance->ZoneCO2GainFromPeople.deallocate();
    state->dataContaminantBalance->ZoneAirCO2.deallocate();
    state->dataContaminantBalance->ZoneSysContDemand.deallocate();
}
