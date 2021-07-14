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

// EnergyPlus::HeatBalanceKivaManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/ConfiguredFunctions.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/HeatBalanceKivaManager.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatBalanceKiva_SetInitialBCs)
{

    // Unit test for Issue #7291 - Kiva initialization indoor temperature assumptions

    // Create Kiva foundation and set parameters
    Kiva::Foundation fnd;

    fnd.reductionStrategy = Kiva::Foundation::RS_AP;

    Kiva::Material concrete(1.95, 2400.0, 900.0);

    Kiva::Layer tempLayer;
    tempLayer.thickness = 0.10;
    tempLayer.material = concrete;

    fnd.slab.interior.emissivity = 0.8;
    fnd.slab.layers.push_back(tempLayer);

    tempLayer.thickness = 0.2;
    tempLayer.material = concrete;

    fnd.wall.layers.push_back(tempLayer);

    fnd.wall.heightAboveGrade = 0.1;
    fnd.wall.depthBelowSlab = 0.2;
    fnd.wall.interior.emissivity = 0.8;
    fnd.wall.exterior.emissivity = 0.8;
    fnd.wall.interior.absorptivity = 0.8;
    fnd.wall.exterior.absorptivity = 0.8;

    fnd.foundationDepth = 0.0;
    fnd.numericalScheme = Kiva::Foundation::NS_ADI;

    fnd.polygon.outer().push_back(Kiva::Point(-6.0, -6.0));
    fnd.polygon.outer().push_back(Kiva::Point(-6.0, 6.0));
    fnd.polygon.outer().push_back(Kiva::Point(6.0, 6.0));
    fnd.polygon.outer().push_back(Kiva::Point(6.0, -6.0));

    // Create Kiva weather data
    HeatBalanceKivaManager::KivaWeatherData kivaweather;
    kivaweather.annualAverageDrybulbTemp = 10.0;
    kivaweather.intervalsPerHour = 1;
    kivaweather.dryBulb = {10.0};
    kivaweather.windSpeed = {0.0};
    kivaweather.skyEmissivity = {0.0};

    HeatBalanceKivaManager::KivaManager km;

    std::string const idf_objects = delimited_string({
        "Zone,",
        "  Core_bottom,             !- Name",
        "  0.0000,                  !- Direction of Relative North {deg}",
        "  0.0000,                  !- X Origin {m}",
        "  0.0000,                  !- Y Origin {m}",
        "  0.0000,                  !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  ,                        !- Ceiling Height {m}",
        "  ,                        !- Volume {m3}",
        "  autocalculate,           !- Floor Area {m2}",
        "  ,                        !- Zone Inside Convection Algorithm",
        "  ,                        !- Zone Outside Convection Algorithm",
        "  Yes;                     !- Part of Total Floor Area",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_bottom Thermostat,  !- Name",
        "  Core_bottom,             !- Zone or ZoneList Name",
        "  Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "  Core_bottom DualSPSched; !- Control 1 Name",
        " ",
        "Schedule:Constant,",
        "  Dual Zone Control Type Sched,  !- Name",
        "  Control Type,            !- Schedule Type Limits Name",
        "  4;                       !- Field 1",
        " ",
        "ThermostatSetpoint:DualSetpoint,",
        "  Core_bottom DualSPSched, !- Name",
        "  HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
        "  CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",
        " ",
        "Schedule:Constant,",
        "  CLGSETP_SCH,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  24.0;                    !- Field 1",
        " ",
        "Schedule:Constant,",
        "  HTGSETP_SCH,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  20.0;                    !- Field 1",
        " ",
        "Schedule:Constant,",
        "  CLGSETP_SCH_EXTREME,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  100.0;                    !- Field 1",
        " ",
        "Schedule:Constant,",
        "  HTGSETP_SCH_EXTREME,             !- Name",
        "  Temperature,             !- Schedule Type Limits Name",
        "  -100.0;                    !- Field 1",
        " ",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    ASSERT_FALSE(ErrorsFound);

    int DualZoneNum(1);

    state->dataEnvrn->DayOfYear_Schedule = 1;      // must initialize this to get schedules initialized
    state->dataEnvrn->DayOfWeek = 1;               // must initialize this to get schedules initialized
    state->dataGlobal->HourOfDay = 1;              // must initialize this to get schedules initialized
    state->dataGlobal->TimeStep = 1;               // must initialize this to get schedules initialized
    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    ZoneTempPredictorCorrector::GetZoneAirSetPoints(*state);

    state->dataScheduleMgr->Schedule(state->dataZoneCtrls->TempControlledZone(DualZoneNum).CTSchedIndex).CurrentValue =
        DataHVACGlobals::DualSetPointWithDeadBand;

    // Test Initial Indoor Temperature input of 15C with Cooling/Heating Setpoints of 24C/20C

    Real64 zoneAssumedTemperature1 = 15.0;
    HeatBalanceKivaManager::KivaInstanceMap kv1(*state, fnd, 0, {}, 0, zoneAssumedTemperature1, 1.0, 0, &km);

    kv1.zoneControlNum = 1;
    kv1.zoneControlType = 1; // Temperature

    kv1.setInitialBoundaryConditions(*state, kivaweather, 1, 1, 1);

    Real64 expectedResult1 = kv1.instance.bcs->slabConvectiveTemp;

    EXPECT_NEAR(expectedResult1, zoneAssumedTemperature1 + DataGlobalConstants::KelvinConv, 0.001);

    // Test using default Initial Indoor Temperature with Cooling/Heating Setpoints of 24C/20C

    Real64 coolingSetpoint2 = 24.0;
    Real64 zoneAssumedTemperature2 = -9999;
    HeatBalanceKivaManager::KivaInstanceMap kv2(*state, fnd, 0, {}, 0, zoneAssumedTemperature2, 1.0, 0, &km);

    kv2.zoneControlNum = 1;
    kv2.zoneControlType = 1; // Temperature

    kv2.setInitialBoundaryConditions(*state, kivaweather, 1, 1, 1);

    Real64 expectedResult2 = kv2.instance.bcs->slabConvectiveTemp;

    EXPECT_NEAR(expectedResult2, coolingSetpoint2 + DataGlobalConstants::KelvinConv, 0.001);

    // Test using default Initial Indoor Temperature with Cooling/Heating Setpoints of 100C/-100C

    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).CoolTempSchedIndex = 4;
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).HeatTempSchedIndex = 5;

    Real64 coolingSetpoint3 = 100.0;
    Real64 zoneAssumedTemperature3 = -9999;
    HeatBalanceKivaManager::KivaInstanceMap kv3(*state, fnd, 0, {}, 0, zoneAssumedTemperature3, 1.0, 0, &km);

    kv3.zoneControlNum = 1;
    kv3.zoneControlType = 1; // Temperature

    kv3.setInitialBoundaryConditions(*state, kivaweather, 1, 1, 1);

    Real64 expectedResult3 = kv3.instance.bcs->slabConvectiveTemp;

    EXPECT_NEAR(expectedResult3, coolingSetpoint3 + DataGlobalConstants::KelvinConv, 0.001);

    // Test Initial Indoor Temperature input of 15C with Cooling/Heating Setpoints of 100C/-100C

    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).CoolTempSchedIndex = 4;
    state->dataZoneTempPredictorCorrector->SetPointDualHeatCool(1).HeatTempSchedIndex = 5;

    Real64 zoneAssumedTemperature4 = 15.0;
    HeatBalanceKivaManager::KivaInstanceMap kv4(*state, fnd, 0, {}, 0, zoneAssumedTemperature4, 1.0, 0, &km);

    kv4.zoneControlNum = 1;
    kv4.zoneControlType = 1; // Temperature

    kv4.setInitialBoundaryConditions(*state, kivaweather, 1, 1, 1);

    Real64 expectedResult4 = kv4.instance.bcs->slabConvectiveTemp;

    EXPECT_NEAR(expectedResult4, zoneAssumedTemperature4 + DataGlobalConstants::KelvinConv, 0.001);
}

TEST_F(EnergyPlusFixture, OpaqueSkyCover_InterpretWeatherMissingOpaqueSkyCover)
{

    // DERIVED TYPE DEFINITIONS:
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    state->files.inputWeatherFilePath.filePath = configured_source_directory() / "tst/EnergyPlus/unit/Resources/HeatBalanceKivaManagerOSkyTest.epw";
    state->dataWeatherManager->Missing.OpaqSkyCvr = 5;

    HeatBalanceKivaManager::KivaManager km;
    km.readWeatherData(*state);

    Real64 TDewK = 264.25;
    Real64 expected_OSky = 5;
    Real64 expected_ESky = (0.787 + 0.764 * std::log(TDewK / DataGlobalConstants::KelvinConv)) *
                           (1.0 + 0.0224 * expected_OSky - 0.0035 * pow_2(expected_OSky) + 0.00028 * pow_3(expected_OSky));
    ;

    EXPECT_NEAR(expected_ESky, km.kivaWeather.skyEmissivity[0], 0.01);
}

} // namespace EnergyPlus
